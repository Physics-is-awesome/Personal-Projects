!===============================================================================
! ns_axi_blowup.f90
!
! Axisymmetric (with swirl) incompressible Navier-Stokes solver, nu = 1, driven by
! a smooth force that makes a collapsing "spinning-top" vortex an EXACT solution.
! The vortex has the leading-order structure of the finite-time blow-up flow in
! OpenAI, "Finite Time Blowup for Navier-Stokes" (2026):
!     start at rest, smooth force switched on,
!     core radius  l_r ~ tau^(1/2),   core height l_z ~ tau^(1/2-h),   tau = 1-t
!     |u_theta|, |u_z| ~ tau^(-1/2-h), |u_r| ~ tau^(-1/2)
!
! IMPORTANT: the Gaussian profiles here are NOT the profiles of the proof, and the
! force that results is singular as tau -> 0 (see README).  The solver is run
! until tau_end > 0.  What this program demonstrates and tests is the solver and
! the scalings, not the proof.
!
! Formulation (Hou-Li variables):  u1 = u_theta/r, om1 = omega_theta/r, psi1 = psi/r^2
!   u1_t  + u_r u1_r  + u_z u1_z  - 2 u1 psi1_z = L5 u1  + F1
!   om1_t + u_r om1_r + u_z om1_z - (u1^2)_z    = L5 om1 + F2
!   -L5 psi1 = om1,  u_r = -r psi1_z,  u_z = 2 psi1 + r psi1_r,  L5 = d_rr + 3/r d_r + d_zz
!
! Numerics: cell-centred uniform grid, 2nd-order finite volume in r (flux form
! with r^3 weight -> regular on the axis), 2nd-order central differences, RK4 in
! time, banded Cholesky for the Poisson problem.  Dirichlet (=0) on r=R, z=+-Z.
!
! Extra outputs / options (all off by default, so old input files still work):
!   balance.dat   term-by-term size of the theta-momentum equation (always written)
!   tau_off, woff force removed smoothly around t = 1 - tau_off (width woff in time);
!                 with tau_end < 0 the run continues past t = 1 (no singularity then)
!   nframes ...   binary movie frames  -> frames.bin
!   ntr_r ...     tracer particles advected with the flow inside the RK4 stages
!                 -> tracers.bin
!
! Build:   python3 gen_forcing.py && gfortran -O3 -march=native forcing_gen.f90 ns_axi_blowup.f90 -o ns_axi_blowup
! Run:     ./ns_axi_blowup            (defaults)   or   ./ns_axi_blowup in.nml
!===============================================================================
program ns_axi_blowup
  use case_params
  use forcing_gen
  implicit none
  real(dp), parameter :: pi = 3.14159265358979323846d0
  integer, parameter :: maxsnap = 12

  ! ---- run parameters (namelist /input/) ----
  integer  :: nr = 120, nz = 240
  real(dp) :: rmax = 3.0d0, zmax = 3.0d0
  real(dp) :: tau_end = 0.02d0
  real(dp) :: cfl = 0.8d0
  integer  :: ndiag = 20
  ! text snapshots: list of tau values, DESCENDING, unused entries = -99
  real(dp) :: tau_snap(maxsnap) = (/ 0.5d0, 0.3d0, 0.15d0, 0.08d0, 0.04d0, 0.02d0, &
                                     -99d0, -99d0, -99d0, -99d0, -99d0, -99d0 /)
  ! force removal: f -> f*(1 - step((t - (1-tau_off))/woff)); tau_off < 0 means never
  real(dp) :: tau_off = -1.0d30, woff = 0.01d0
  ! movie frames (binary): nframes frames between frame_t0 and frame_t1, geometric in tau if frame_log
  integer  :: nframes = 0
  real(dp) :: frame_t0 = 0.40d0, frame_t1 = 0.98d0
  logical  :: frame_log = .true.
  ! tracers: ntr_r x ntr_z x ntr_th seeds released at t_release, r in (0,tr_rmax), z in (-tr_zmax,tr_zmax)
  integer  :: ntr_r = 0, ntr_z = 0, ntr_th = 0
  real(dp) :: tr_rmax = 1.2d0, tr_zmax = 1.0d0, t_release = 0.40d0
  namelist /input/ nr, nz, rmax, zmax, hh, p0, f0, t0, t1, tau_end, cfl, ndiag, tau_snap, &
                   tau_off, woff, nframes, frame_t0, frame_t1, frame_log, &
                   ntr_r, ntr_z, ntr_th, tr_rmax, tr_zmax, t_release

  ! ---- grid / operators ----
  real(dp) :: dr, dz
  real(dp), allocatable :: rc(:), zc(:), crp(:), crm(:)
  real(dp), allocatable :: ab(:,:)             ! banded Cholesky factor of Poisson matrix
  ! ---- fields ----
  real(dp), allocatable :: u1(:,:), w1(:,:), psi(:,:), ur(:,:), uz(:,:)
  real(dp), allocatable :: F1(:,:), F2(:,:)
  real(dp), allocatable :: pu(:,:), pw(:,:), pp(:,:)       ! padded (ghost) copies
  real(dp), allocatable :: ka1(:,:), ka2(:,:), kb1(:,:), kb2(:,:), kc1(:,:), kc2(:,:), kd1(:,:), kd2(:,:)
  real(dp), allocatable :: s1(:,:), s2(:,:), ue(:,:), pe(:,:), wk1(:,:)
  ! individual terms of the u1 and om1 equations (see eval_terms)
  real(dp), allocatable :: tA1(:,:), tS1(:,:), tV1(:,:), tA2(:,:), tC2(:,:), tV2(:,:)
  ! ---- tracers ----
  integer  :: ntrace = 0
  logical  :: released = .false.
  real(dp), allocatable :: pr(:), pz(:), pth(:), alive(:), qr(:), qz(:)
  real(dp), allocatable :: kvr(:,:), kvz(:,:), kvt(:,:), trbuf(:,:)
  real(dp), allocatable :: pvr(:,:), pvz(:,:), pvu(:,:)

  real(dp) :: t, dt, tend, umax_prev, lam, dtv, dta, tsnap(maxsnap), tnext, x
  real(dp), allocatable :: tfr(:)
  integer  :: istep, isn, nsn, ifr, k, unit_h, unit_b, unit_f, unit_t, ios, narg
  logical  :: hit_s, hit_f, hit
  character(len=256) :: infile

  ! ---------------------------------------------------------------- input
  narg = command_argument_count()
  if (narg >= 1) then
    call get_command_argument(1, infile)
    open(newunit=k, file=trim(infile), status='old', action='read', iostat=ios)
    if (ios /= 0) stop 'cannot open input file'
    read(k, nml=input)
    close(k)
  end if
  dr = rmax / nr
  dz = 2.0d0 * zmax / nz
  tend = 1.0d0 - tau_end
  ! the generated force/exact flow contain tau**(non-integer): never evaluate them at tau <= 0
  if (tau_off > 0.0d0 .and. tau_off <= woff + 1.0d-9) stop 'need tau_off > woff'
  if (tau_end < 1.0d-3 .and. .not. (tau_off > woff)) &
    stop 'running to tau_end < 1e-3 needs the force removed first (set tau_off > woff)'
  nsn = 0
  do k = 1, maxsnap
    if (tau_snap(k) > tau_end - 1d-12 .and. tau_snap(k) < 1.0d0 - t0) then
      nsn = nsn + 1
      tsnap(nsn) = 1.0d0 - tau_snap(k)
    end if
  end do
  ntrace = max(ntr_r, 0) * max(ntr_z, 0) * max(ntr_th, 0)
  if (nframes > 0) then
    if (frame_t0 <= t0 + 1.0d-9) stop 'frame_t0 must be > t0'
    if (ntrace > 0 .and. frame_t0 < t_release - 1.0d-9) stop 'frame_t0 must be >= t_release'
    if (frame_log .and. frame_t1 >= 1.0d0) stop 'frame_log needs frame_t1 < 1'
    allocate(tfr(nframes))
    do k = 1, nframes
      x = real(k - 1, dp) / real(max(nframes - 1, 1), dp)
      if (frame_log) then
        tfr(k) = 1.0d0 - (1.0d0 - frame_t0) * ((1.0d0 - frame_t1) / (1.0d0 - frame_t0))**x
      else
        tfr(k) = frame_t0 + (frame_t1 - frame_t0) * x
      end if
    end do
  else
    allocate(tfr(1)); tfr = 1.0d30
  end if

  allocate(rc(nr), zc(nz), crp(nr), crm(nr))
  allocate(u1(nr,nz), w1(nr,nz), psi(nr,nz), ur(nr,nz), uz(nr,nz), F1(nr,nz), F2(nr,nz))
  allocate(pu(0:nr+1,0:nz+1), pw(0:nr+1,0:nz+1), pp(0:nr+1,0:nz+1))
  allocate(ka1(nr,nz), ka2(nr,nz), kb1(nr,nz), kb2(nr,nz), kc1(nr,nz), kc2(nr,nz), kd1(nr,nz), kd2(nr,nz))
  allocate(s1(nr,nz), s2(nr,nz), ue(nr,nz), pe(nr,nz), wk1(nr,nz))
  allocate(tA1(nr,nz), tS1(nr,nz), tV1(nr,nz), tA2(nr,nz), tC2(nr,nz), tV2(nr,nz))
  if (ntrace > 0) then
    allocate(pr(ntrace), pz(ntrace), pth(ntrace), alive(ntrace), qr(ntrace), qz(ntrace))
    allocate(kvr(ntrace,4), kvz(ntrace,4), kvt(ntrace,4), trbuf(4,ntrace))
    allocate(pvr(0:nr+1,0:nz+1), pvz(0:nr+1,0:nz+1), pvu(0:nr+1,0:nz+1))
  end if
  call setup_grid()
  call build_poisson()

  write(*,'(a,2i6,a,2f8.4,a,f7.3)') ' grid nr,nz =', nr, nz, '   dr,dz =', dr, dz, '   h =', hh
  write(*,'(a,f6.3,a,f6.3,a,f8.4)') ' force on over t in [', t0, ',', t1, ']   run to tau =', tau_end
  if (tau_off > 0.0d0) write(*,'(a,f8.4,a,f8.4)') ' force removed around tau =', tau_off, '  (width', woff
  if (ntrace > 0) write(*,'(a,i6,a,f7.3)') ' tracers:', ntrace, '   released at t =', t_release
  if (nframes > 0) write(*,'(a,i6,a,2f8.4)') ' frames:', nframes, '   t range', tfr(1), tfr(nframes)
  write(*,'(a,a)') '      t        tau     max|u_th|   max|u_z|    max|u_r|    Rrms     Zrms   ', &
                   '   Energy   max|f_th|  err_u   err_psi    dt'

  open(newunit=unit_h, file='history.dat', status='replace')
  write(unit_h,'(a)') '# t tau umax_th umax_z umax_r Rrms Zrms Energy fth_max f2_max err_u err_psi dt'
  open(newunit=unit_b, file='balance.dat', status='replace')
  write(unit_b,'(a)') '# t tau | max-norms: dt_uth nonlin visc force int_sum adv str | L2 norms (same order)'
  if (nframes > 0) then
    open(newunit=unit_f, file='frames.bin', status='replace', access='stream', form='unformatted')
    write(unit_f) int(nr, 4), int(nz, 4), rmax, zmax
  end if
  if (ntrace > 0) then
    open(newunit=unit_t, file='tracers.bin', status='replace', access='stream', form='unformatted')
    write(unit_t) int(ntrace, 4)
  end if

  ! -------------------------------------------------------------- time loop
  u1 = 0.0d0;  w1 = 0.0d0
  t = t0
  umax_prev = 0.0d0
  isn = 1;  ifr = 1
  istep = 0
  lam = 2.0d0 * maxval(crp + crm) + 4.0d0 / dz**2
  dtv = cfl * 2.78d0 / lam
  call diagnostics(t, 0.0d0)
  call maybe_release()
  do while (t < tend - 1.0d-14)
    dta = dtv
    if (umax_prev > 0.0d0) dta = cfl * min(dr, dz) / umax_prev
    dt = min(dtv, dta)
    ! land exactly on the next event (snapshot, frame, tracer release)
    tnext = 1.0d30
    if (isn <= nsn)     tnext = min(tnext, tsnap(isn))
    if (ifr <= nframes) tnext = min(tnext, tfr(ifr))
    if (ntrace > 0 .and. .not. released) tnext = min(tnext, t_release)
    if (t + dt >= tnext - 1.0d-13) dt = tnext - t
    if (t + dt > tend) dt = tend - t

    call rhs(t, u1, w1, ka1, ka2)
    umax_prev = max(maxval(abs(ur)), maxval(abs(uz)))
    if (released) then
      qr = pr;  qz = pz
      call tracer_vel(u1, 1)
      qr = pr + 0.5d0*dt*kvr(:,1);  qz = pz + 0.5d0*dt*kvz(:,1)
    end if
    s1 = u1 + 0.5d0*dt*ka1;  s2 = w1 + 0.5d0*dt*ka2
    call rhs(t+0.5d0*dt, s1, s2, kb1, kb2)
    if (released) then
      call tracer_vel(s1, 2)
      qr = pr + 0.5d0*dt*kvr(:,2);  qz = pz + 0.5d0*dt*kvz(:,2)
    end if
    s1 = u1 + 0.5d0*dt*kb1;  s2 = w1 + 0.5d0*dt*kb2
    call rhs(t+0.5d0*dt, s1, s2, kc1, kc2)
    if (released) then
      call tracer_vel(s1, 3)
      qr = pr + dt*kvr(:,3);  qz = pz + dt*kvz(:,3)
    end if
    s1 = u1 + dt*kc1;        s2 = w1 + dt*kc2
    call rhs(t+dt, s1, s2, kd1, kd2)
    if (released) then
      call tracer_vel(s1, 4)
      call tracer_update(dt)
    end if
    u1 = u1 + dt/6.0d0*(ka1 + 2.0d0*kb1 + 2.0d0*kc1 + kd1)
    w1 = w1 + dt/6.0d0*(ka2 + 2.0d0*kb2 + 2.0d0*kc2 + kd2)
    t = t + dt
    istep = istep + 1
    call maybe_release()

    hit_s = (isn <= nsn)     .and. abs(t - tsnap(min(isn, maxsnap))) < 1.0d-11
    hit_f = (ifr <= nframes) .and. abs(t - tfr(min(ifr, max(nframes, 1)))) < 1.0d-11
    hit = hit_s .or. hit_f
    if (mod(istep, ndiag) == 0 .or. t >= tend - 1.0d-14 .or. hit) call diagnostics(t, dt)
    if (hit_s) then
      call write_snapshot(isn, t)
      isn = isn + 1
    end if
    if (hit_f) then
      call write_frame(t)
      if (released) call write_tracers(t)
      ifr = ifr + 1
    end if
    if (.not. (maxval(abs(u1)) < 1.0d6)) stop 'blow-up of the NUMERICAL solution: stopping'
  end do
  close(unit_h);  close(unit_b)
  if (nframes > 0) close(unit_f)
  if (ntrace > 0) close(unit_t)
  write(*,'(a,i8,a)') ' done after', istep, ' steps'

contains

  !-------------------------------------------------------------------------
  subroutine setup_grid()
    integer :: i, j
    real(dp) :: rp, rm, ri
    do i = 1, nr
      rc(i) = (i - 0.5d0) * dr
      rp = i * dr;  rm = (i - 1) * dr;  ri = rc(i)
      crp(i) = rp**3 / (ri**3 * dr**2)       ! coefficient to the east neighbour
      crm(i) = rm**3 / (ri**3 * dr**2)       ! ... to the west neighbour (0 on the axis)
    end do
    do j = 1, nz
      zc(j) = -zmax + (j - 0.5d0) * dz
    end do
  end subroutine setup_grid

  !-------------------------------------------------------------------------
  ! Poisson matrix A = -r^3 L5 (symmetric positive definite), lower band storage
  ! ab(d,k) = A(k+d,k), unknown index k=(j-1)*nr+i, half-bandwidth nr.
  subroutine build_poisson()
    integer :: i, j, k, n, bw, d, kn, jj, ii
    real(dp) :: rp3, rm3, ri3
    n = nr * nz;  bw = nr
    allocate(ab(0:bw, n))
    ab = 0.0d0
    do j = 1, nz
      do i = 1, nr
        k = (j - 1) * nr + i
        rp3 = (i * dr)**3;  rm3 = ((i - 1) * dr)**3;  ri3 = rc(i)**3
        ab(0,k) = (rp3 + rm3) / dr**2 + 2.0d0 * ri3 / dz**2
        if (i == nr) ab(0,k) = ab(0,k) + rp3 / dr**2            ! Dirichlet ghost = -f
        if (j == 1 .or. j == nz) ab(0,k) = ab(0,k) + ri3 / dz**2
        if (i < nr) ab(1,k)  = -rp3 / dr**2
        if (j < nz) ab(bw,k) = -ri3 / dz**2
      end do
    end do
    ! in-place banded Cholesky A = L L^T
    do k = 1, n
      ab(0,k) = sqrt(ab(0,k))
      kn = min(bw, n - k)
      do d = 1, kn
        ab(d,k) = ab(d,k) / ab(0,k)
      end do
      do jj = 1, kn
        do ii = jj, kn
          ab(ii-jj, k+jj) = ab(ii-jj, k+jj) - ab(ii,k) * ab(jj,k)
        end do
      end do
    end do
  end subroutine build_poisson

  subroutine poisson_solve(om, ps)
    real(dp), intent(in)  :: om(nr,nz)
    real(dp), intent(out) :: ps(nr,nz)
    real(dp), allocatable, save :: x(:)
    integer :: i, j, k, n, bw, d, kn
    real(dp) :: sacc
    n = nr * nz;  bw = nr
    if (.not. allocated(x)) allocate(x(n))
    do j = 1, nz
      do i = 1, nr
        x((j-1)*nr + i) = rc(i)**3 * om(i,j)
      end do
    end do
    do k = 1, n                                  ! L y = b
      x(k) = x(k) / ab(0,k)
      kn = min(bw, n - k)
      do d = 1, kn
        x(k+d) = x(k+d) - ab(d,k) * x(k)
      end do
    end do
    do k = n, 1, -1                              ! L^T x = y
      kn = min(bw, n - k)
      sacc = 0.0d0
      do d = 1, kn
        sacc = sacc + ab(d,k) * x(k+d)
      end do
      x(k) = (x(k) - sacc) / ab(0,k)
    end do
    do j = 1, nz
      do i = 1, nr
        ps(i,j) = x((j-1)*nr + i)
      end do
    end do
  end subroutine poisson_solve

  !-------------------------------------------------------------------------
  subroutine pad(f, g)
    ! ghost cells: even about the axis, odd (Dirichlet 0) at r=R and z=+-Z
    real(dp), intent(in)  :: f(nr,nz)
    real(dp), intent(out) :: g(0:nr+1,0:nz+1)
    g(1:nr,1:nz) = f
    g(0,1:nz)    =  f(1,:)
    g(nr+1,1:nz) = -f(nr,:)
    g(1:nr,0)    = -f(:,1)
    g(1:nr,nz+1) = -f(:,nz)
    g(0,0) = 0.0d0; g(nr+1,0) = 0.0d0; g(0,nz+1) = 0.0d0; g(nr+1,nz+1) = 0.0d0
  end subroutine pad

  !-------------------------------------------------------------------------
  real(dp) function smooth_step(xx)
    ! C-infinity step: 0 for xx<=0, 1 for xx>=1
    real(dp), intent(in) :: xx
    real(dp) :: a, b
    if (xx <= 0.0d0) then
      smooth_step = 0.0d0
    else if (xx >= 1.0d0) then
      smooth_step = 1.0d0
    else
      a = exp(-1.0d0 / xx);  b = exp(-1.0d0 / (1.0d0 - xx))
      smooth_step = a / (a + b)
    end if
  end function smooth_step

  !-------------------------------------------------------------------------
  ! force on the grid at time t_, including the optional smooth removal
  subroutine get_force(t_)
    real(dp), intent(in) :: t_
    real(dp) :: g
    g = 1.0d0 - smooth_step((t_ - (1.0d0 - tau_off)) / woff)
    if (g <= 0.0d0) then
      F1 = 0.0d0;  F2 = 0.0d0
    else
      call eval_force(nr, nz, rc, zc, t_, F1, F2)
      if (g < 1.0d0) then
        F1 = g * F1;  F2 = g * F2
      end if
    end if
  end subroutine get_force

  !-------------------------------------------------------------------------
  ! Individual terms of the evolution equations for state (u,w) at time t_.
  ! Also updates psi, the padded copies, ur, uz and the force F1, F2.
  !   u1_t  = tA1 + tS1 + tV1 + F1        tA1 = -(u_r u1_r + u_z u1_z)  tS1 = 2 u1 psi1_z   tV1 = L5 u1
  !   om1_t = tA2 + tC2 + tV2 + F2        tA2 = -(u_r w_r  + u_z w_z)   tC2 = (u1^2)_z      tV2 = L5 om1
  subroutine eval_terms(t_, u, w)
    real(dp), intent(in)  :: t_, u(nr,nz), w(nr,nz)
    integer :: i, j
    real(dp) :: psr, psz, ur_r, ur_z, wr, wz, lapu, lapw, r
    call poisson_solve(w, psi)
    call pad(psi, pp);  call pad(u, pu);  call pad(w, pw)
    call get_force(t_)
    do j = 1, nz
      do i = 1, nr
        r   = rc(i)
        psr = (pp(i+1,j) - pp(i-1,j)) / (2.0d0*dr)
        psz = (pp(i,j+1) - pp(i,j-1)) / (2.0d0*dz)
        ur(i,j) = -r * psz
        uz(i,j) = 2.0d0 * pp(i,j) + r * psr
        ur_r = (pu(i+1,j) - pu(i-1,j)) / (2.0d0*dr)
        ur_z = (pu(i,j+1) - pu(i,j-1)) / (2.0d0*dz)
        wr   = (pw(i+1,j) - pw(i-1,j)) / (2.0d0*dr)
        wz   = (pw(i,j+1) - pw(i,j-1)) / (2.0d0*dz)
        lapu = crp(i)*(pu(i+1,j) - pu(i,j)) - crm(i)*(pu(i,j) - pu(i-1,j)) &
             + (pu(i,j+1) - 2.0d0*pu(i,j) + pu(i,j-1)) / dz**2
        lapw = crp(i)*(pw(i+1,j) - pw(i,j)) - crm(i)*(pw(i,j) - pw(i-1,j)) &
             + (pw(i,j+1) - 2.0d0*pw(i,j) + pw(i,j-1)) / dz**2
        tA1(i,j) = -ur(i,j)*ur_r - uz(i,j)*ur_z
        tS1(i,j) = 2.0d0*u(i,j)*psz
        tV1(i,j) = lapu
        tA2(i,j) = -ur(i,j)*wr - uz(i,j)*wz
        tC2(i,j) = (pu(i,j+1)**2 - pu(i,j-1)**2) / (2.0d0*dz)
        tV2(i,j) = lapw
      end do
    end do
  end subroutine eval_terms

  subroutine rhs(t_, u, w, du, dw)
    real(dp), intent(in)  :: t_, u(nr,nz), w(nr,nz)
    real(dp), intent(out) :: du(nr,nz), dw(nr,nz)
    call eval_terms(t_, u, w)
    du = tA1 + tS1 + tV1 + F1
    dw = tA2 + tC2 + tV2 + F2
  end subroutine rhs

  !-------------------------------------------------------------------------
  subroutine diagnostics(t_, dt_)
    real(dp), intent(in) :: t_, dt_
    integer :: i, j
    real(dp) :: tau, r, uth, dV, e, sw_th, sr2, sw_z, sz2, umth, umz, umr, fth, f2m
    real(dp) :: nu, du, np, dp2, eu, ep
    call poisson_solve(w1, psi)
    call pad(psi, pp)
    call get_force(t_)
    tau = 1.0d0 - t_
    if (tau > 1.0d-6) then
      call eval_exact(nr, nz, rc, zc, t_, ue, pe)
    else
      ue = 0.0d0;  pe = 0.0d0
    end if
    e = 0.0d0; sw_th = 0.0d0; sr2 = 0.0d0; sw_z = 0.0d0; sz2 = 0.0d0
    umth = 0.0d0; umz = 0.0d0; umr = 0.0d0; fth = 0.0d0; f2m = 0.0d0
    nu = 0.0d0; du = 0.0d0; np = 0.0d0; dp2 = 0.0d0
    do j = 1, nz
      do i = 1, nr
        r  = rc(i)
        dV = 2.0d0 * pi * r * dr * dz
        uth = r * u1(i,j)
        ur(i,j) = -r * (pp(i,j+1) - pp(i,j-1)) / (2.0d0*dz)
        uz(i,j) = 2.0d0*pp(i,j) + r * (pp(i+1,j) - pp(i-1,j)) / (2.0d0*dr)
        e = e + 0.5d0 * (ur(i,j)**2 + uth**2 + uz(i,j)**2) * dV
        sw_th = sw_th + uth**2 * dV;   sr2 = sr2 + r**2 * uth**2 * dV
        sw_z  = sw_z  + uz(i,j)**2 * dV;  sz2 = sz2 + zc(j)**2 * uz(i,j)**2 * dV
        umth = max(umth, abs(uth)); umz = max(umz, abs(uz(i,j))); umr = max(umr, abs(ur(i,j)))
        fth = max(fth, abs(r * F1(i,j)));  f2m = max(f2m, abs(r * F2(i,j)))
        du = du + (r * (u1(i,j) - ue(i,j)))**2 * dV;  nu = nu + (r * ue(i,j))**2 * dV
        dp2 = dp2 + (psi(i,j) - pe(i,j))**2 * dV;     np = np + pe(i,j)**2 * dV
      end do
    end do
    if (sw_th <= 0.0d0) sw_th = 1.0d0
    if (sw_z  <= 0.0d0) sw_z  = 1.0d0
    if (nu <= 0.0d0) nu = 1.0d0
    if (np <= 0.0d0) np = 1.0d0
    eu = sqrt(du/nu);  ep = sqrt(dp2/np)
    if (tau <= 1.0d-6) then                       ! no exact flow to compare with
      eu = -1.0d0;  ep = -1.0d0
    end if
    write(*,'(f10.6,f10.6,3es11.3,2f9.5,es11.3,es10.2,2es9.1,es9.1)') t_, tau, umth, umz, umr, &
         sqrt(sr2/sw_th), sqrt(sz2/sw_z), e, fth, eu, ep, dt_
    write(unit_h,'(13es18.8e3)') t_, tau, umth, umz, umr, sqrt(sr2/sw_th), sqrt(sz2/sw_z), e, fth, f2m, &
         eu, ep, dt_
    flush(unit_h)
    call write_balance(t_)
  end subroutine diagnostics

  !-------------------------------------------------------------------------
  ! Size of every term of the physical theta-momentum equation
  !   d_t u_theta = NL + VISC + f_theta ,   NL = -(u.grad u)_theta ,  VISC = (Lap - 1/r^2) u_theta
  ! in terms of the Hou-Li terms: u_theta = r u1  ->  d_t u_theta = r u1_t, NL = r (tA1 + tS1), VISC = r tV1.
  ! The force is "cancelling" if |f_theta| << the individual terms.
  subroutine write_balance(t_)
    real(dp), intent(in) :: t_
    integer :: i, j, m
    real(dp) :: r, dV, T(7), mx(7), l2(7)
    call eval_terms(t_, u1, w1)
    mx = 0.0d0;  l2 = 0.0d0
    do j = 1, nz
      do i = 1, nr
        r  = rc(i)
        dV = 2.0d0 * pi * r * dr * dz
        T(1) = r * (tA1(i,j) + tS1(i,j) + tV1(i,j) + F1(i,j))     ! d_t u_theta
        T(2) = r * (tA1(i,j) + tS1(i,j))                           ! nonlinear term
        T(3) = r * tV1(i,j)                                        ! viscous term
        T(4) = r * F1(i,j)                                         ! force
        T(5) = r * (tA1(i,j) + tS1(i,j) + tV1(i,j))                ! nonlinear + viscous  (= d_t u_theta - f_theta)
        T(6) = r * tA1(i,j)                                        ! pure advection u_r d_r u1 + u_z d_z u1
        T(7) = r * tS1(i,j)                                        ! geometric coupling 2 u1 psi1_z
        do m = 1, 7
          mx(m) = max(mx(m), abs(T(m)))
          l2(m) = l2(m) + T(m)**2 * dV
        end do
      end do
    end do
    write(unit_b,'(16es18.8e3)') t_, 1.0d0 - t_, mx, sqrt(l2)
    flush(unit_b)
  end subroutine write_balance

  !-------------------------------------------------------------------------
  subroutine write_snapshot(idx, t_)
    integer, intent(in) :: idx
    real(dp), intent(in) :: t_
    integer :: i, j, u
    character(len=32) :: fname
    real(dp) :: r
    write(fname,'(a,i2.2,a)') 'snap_', idx, '.dat'
    call poisson_solve(w1, psi)
    call pad(psi, pp)
    call get_force(t_)
    open(newunit=u, file=trim(fname), status='replace')
    write(u,'(a,es18.10,a,es18.10,2i6)') '# t=', t_, '  tau=', 1.0d0 - t_, nr, nz
    write(u,'(a)') '# r z u_r u_theta u_z omega_theta f_theta'
    do j = 1, nz
      do i = 1, nr
        r = rc(i)
        write(u,'(7es18.8e3)') r, zc(j), -r*(pp(i,j+1)-pp(i,j-1))/(2.0d0*dz), r*u1(i,j), &
             2.0d0*pp(i,j) + r*(pp(i+1,j)-pp(i-1,j))/(2.0d0*dr), r*w1(i,j), r*F1(i,j)
      end do
    end do
    close(u)
  end subroutine write_snapshot

  !-------------------------------------------------------------------------
  ! binary frame: t (real64), then u_theta, u_r, u_z, omega_theta as real32 arrays (nr,nz)
  subroutine write_frame(t_)
    real(dp), intent(in) :: t_
    integer :: i, j
    real(dp) :: r
    call poisson_solve(w1, psi)
    call pad(psi, pp)
    write(unit_f) t_
    do j = 1, nz
      do i = 1, nr
        wk1(i,j) = rc(i) * u1(i,j)
      end do
    end do
    write(unit_f) real(wk1, 4)
    do j = 1, nz
      do i = 1, nr
        r = rc(i)
        wk1(i,j) = -r * (pp(i,j+1) - pp(i,j-1)) / (2.0d0*dz)
      end do
    end do
    write(unit_f) real(wk1, 4)
    do j = 1, nz
      do i = 1, nr
        r = rc(i)
        wk1(i,j) = 2.0d0*pp(i,j) + r * (pp(i+1,j) - pp(i-1,j)) / (2.0d0*dr)
      end do
    end do
    write(unit_f) real(wk1, 4)
    do j = 1, nz
      do i = 1, nr
        wk1(i,j) = rc(i) * w1(i,j)
      end do
    end do
    write(unit_f) real(wk1, 4)
  end subroutine write_frame

  !-------------------------------------------------------------------------
  ! ------------------------------ tracers --------------------------------
  subroutine maybe_release()
    integer :: a, b, c, n
    if (ntrace <= 0 .or. released) return
    if (t < t_release - 1.0d-11) return
    n = 0
    do c = 1, ntr_th
      do b = 1, ntr_z
        do a = 1, ntr_r
          n = n + 1
          pr(n)  = tr_rmax * (a - 0.5d0) / ntr_r
          pz(n)  = -tr_zmax + 2.0d0 * tr_zmax * (b - 0.5d0) / ntr_z
          pth(n) = 2.0d0 * pi * (real(c - 1, dp) / ntr_th + 0.1d0 * b)
          alive(n) = 1.0d0
        end do
      end do
    end do
    released = .true.
  end subroutine maybe_release

  ! bilinear interpolation on the padded cell-centred grid
  real(dp) function bilin(g, rr, zz)
    real(dp), intent(in) :: g(0:nr+1,0:nz+1), rr, zz
    real(dp) :: xr, yz, fx, fy
    integer :: i0, j0
    xr = rr / dr + 0.5d0
    yz = (zz + zmax) / dz + 0.5d0
    i0 = int(xr);  j0 = int(yz)
    fx = xr - i0;  fy = yz - j0
    bilin = (1.0d0-fx)*(1.0d0-fy)*g(i0,j0)   + fx*(1.0d0-fy)*g(i0+1,j0) &
          + (1.0d0-fx)*fy*g(i0,j0+1)         + fx*fy*g(i0+1,j0+1)
  end function bilin

  ! velocity (dr/dt, dz/dt, dtheta/dt = u1) at the stage positions (qr,qz); u = u1 field of this stage.
  ! ur, uz must be those of the same stage (just computed by rhs).  r may be negative = other side of the axis.
  subroutine tracer_vel(u, stage)
    real(dp), intent(in) :: u(nr,nz)
    integer, intent(in) :: stage
    integer :: n
    real(dp) :: ra, sg
    pvr(1:nr,1:nz) = ur;  pvz(1:nr,1:nz) = uz;  pvu(1:nr,1:nz) = u
    pvr(0,1:nz) = -ur(1,:);  pvz(0,1:nz) = uz(1,:);  pvu(0,1:nz) = u(1,:)      ! axis: u_r odd, others even
    pvr(nr+1,1:nz) = ur(nr,:);  pvz(nr+1,1:nz) = uz(nr,:);  pvu(nr+1,1:nz) = u(nr,:)
    pvr(:,0) = pvr(:,1);  pvz(:,0) = pvz(:,1);  pvu(:,0) = pvu(:,1)
    pvr(:,nz+1) = pvr(:,nz);  pvz(:,nz+1) = pvz(:,nz);  pvu(:,nz+1) = pvu(:,nz)
    do n = 1, ntrace
      kvr(n,stage) = 0.0d0;  kvz(n,stage) = 0.0d0;  kvt(n,stage) = 0.0d0
      if (alive(n) < 0.5d0) cycle
      ra = abs(qr(n));  sg = sign(1.0d0, qr(n))
      if (ra > rmax - 2.0d0*dr .or. abs(qz(n)) > zmax - 2.0d0*dz) then     ! left the domain: freeze
        alive(n) = 0.0d0
        cycle
      end if
      kvr(n,stage) = sg * bilin(pvr, ra, qz(n))
      kvz(n,stage) = bilin(pvz, ra, qz(n))
      kvt(n,stage) = bilin(pvu, ra, qz(n))
    end do
  end subroutine tracer_vel

  subroutine tracer_update(dt_)
    real(dp), intent(in) :: dt_
    integer :: n
    do n = 1, ntrace
      if (alive(n) < 0.5d0) cycle
      pr(n)  = pr(n)  + dt_/6.0d0*(kvr(n,1) + 2.0d0*kvr(n,2) + 2.0d0*kvr(n,3) + kvr(n,4))
      pz(n)  = pz(n)  + dt_/6.0d0*(kvz(n,1) + 2.0d0*kvz(n,2) + 2.0d0*kvz(n,3) + kvz(n,4))
      pth(n) = pth(n) + dt_/6.0d0*(kvt(n,1) + 2.0d0*kvt(n,2) + 2.0d0*kvt(n,3) + kvt(n,4))
      if (pr(n) < 0.0d0) then                       ! went through the axis
        pr(n) = -pr(n);  pth(n) = pth(n) + pi
      end if
    end do
  end subroutine tracer_update

  subroutine write_tracers(t_)
    real(dp), intent(in) :: t_
    integer :: n
    do n = 1, ntrace
      trbuf(1,n) = pr(n);  trbuf(2,n) = pz(n);  trbuf(3,n) = pth(n);  trbuf(4,n) = alive(n)
    end do
    write(unit_t) t_
    write(unit_t) trbuf
  end subroutine write_tracers

end program ns_axi_blowup
