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
! Build:   python3 gen_forcing.py && gfortran -O3 -march=native forcing_gen.f90 ns_axi_blowup.f90 -o ns_axi_blowup
! Run:     ./ns_axi_blowup [input.nml]
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
  real(dp) :: tau_snap(maxsnap) = (/ 0.5d0, 0.3d0, 0.15d0, 0.08d0, 0.04d0, 0.02d0, &
                                     -1d0, -1d0, -1d0, -1d0, -1d0, -1d0 /)
  namelist /input/ nr, nz, rmax, zmax, hh, p0, f0, t0, t1, tau_end, cfl, ndiag, tau_snap

  ! ---- grid / operators ----
  real(dp) :: dr, dz
  real(dp), allocatable :: rc(:), zc(:), crp(:), crm(:)
  real(dp), allocatable :: ab(:,:)             ! banded Cholesky factor of Poisson matrix
  ! ---- fields ----
  real(dp), allocatable :: u1(:,:), w1(:,:), psi(:,:), ur(:,:), uz(:,:)
  real(dp), allocatable :: F1(:,:), F2(:,:)
  real(dp), allocatable :: pu(:,:), pw(:,:), pp(:,:)       ! padded (ghost) copies
  real(dp), allocatable :: ka1(:,:), ka2(:,:), kb1(:,:), kb2(:,:), kc1(:,:), kc2(:,:), kd1(:,:), kd2(:,:)
  real(dp), allocatable :: s1(:,:), s2(:,:), ue(:,:), pe(:,:)

  real(dp) :: t, dt, tend, umax_prev, lam, dtv, dta, tsnap(maxsnap), tnext
  integer  :: istep, isn, nsn, k, unit_h, ios, narg
  logical  :: hit
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
  nsn = 0
  do k = 1, maxsnap
    if (tau_snap(k) > tau_end - 1d-12 .and. tau_snap(k) < 1.0d0 - t0) then
      nsn = nsn + 1
      tsnap(nsn) = 1.0d0 - tau_snap(k)
    end if
  end do

  allocate(rc(nr), zc(nz), crp(nr), crm(nr))
  allocate(u1(nr,nz), w1(nr,nz), psi(nr,nz), ur(nr,nz), uz(nr,nz), F1(nr,nz), F2(nr,nz))
  allocate(pu(0:nr+1,0:nz+1), pw(0:nr+1,0:nz+1), pp(0:nr+1,0:nz+1))
  allocate(ka1(nr,nz), ka2(nr,nz), kb1(nr,nz), kb2(nr,nz), kc1(nr,nz), kc2(nr,nz), kd1(nr,nz), kd2(nr,nz))
  allocate(s1(nr,nz), s2(nr,nz), ue(nr,nz), pe(nr,nz))
  call setup_grid()
  call build_poisson()

  write(*,'(a,2i6,a,2f8.4,a,f7.3)') ' grid nr,nz =', nr, nz, '   dr,dz =', dr, dz, '   h =', hh
  write(*,'(a,f6.3,a,f6.3,a,f7.4)') ' force on over t in [', t0, ',', t1, ']   run to tau =', tau_end
  write(*,'(a,a)') '      t        tau     max|u_th|   max|u_z|    max|u_r|    Rrms     Zrms   ', &
                   '   Energy   max|f_th|  err_u   err_psi    dt'

  open(newunit=unit_h, file='history.dat', status='replace')
  write(unit_h,'(a)') '# t tau umax_th umax_z umax_r Rrms Zrms Energy fth_max f2_max err_u err_psi dt'

  ! -------------------------------------------------------------- time loop
  u1 = 0.0d0;  w1 = 0.0d0
  t = t0
  umax_prev = 0.0d0
  isn = 1
  istep = 0
  lam = 2.0d0 * maxval(crp + crm) + 4.0d0 / dz**2
  dtv = cfl * 2.78d0 / lam
  call diagnostics(t, 0.0d0)
  do while (t < tend - 1.0d-14)
    dta = dtv
    if (umax_prev > 0.0d0) dta = cfl * min(dr, dz) / umax_prev
    dt = min(dtv, dta)
    hit = .false.
    if (isn <= nsn) then
      tnext = tsnap(isn)
      if (t + dt >= tnext - 1.0d-13) then
        dt = tnext - t;  hit = .true.
      end if
    end if
    if (t + dt > tend) dt = tend - t

    call rhs(t,            u1, w1, ka1, ka2)
    umax_prev = max(maxval(abs(ur)), maxval(abs(uz)))
    s1 = u1 + 0.5d0*dt*ka1;  s2 = w1 + 0.5d0*dt*ka2
    call rhs(t+0.5d0*dt,   s1, s2, kb1, kb2)
    s1 = u1 + 0.5d0*dt*kb1;  s2 = w1 + 0.5d0*dt*kb2
    call rhs(t+0.5d0*dt,   s1, s2, kc1, kc2)
    s1 = u1 + dt*kc1;        s2 = w1 + dt*kc2
    call rhs(t+dt,         s1, s2, kd1, kd2)
    u1 = u1 + dt/6.0d0*(ka1 + 2.0d0*kb1 + 2.0d0*kc1 + kd1)
    w1 = w1 + dt/6.0d0*(ka2 + 2.0d0*kb2 + 2.0d0*kc2 + kd2)
    t = t + dt
    istep = istep + 1

    if (mod(istep, ndiag) == 0 .or. t >= tend - 1.0d-14 .or. hit) call diagnostics(t, dt)
    if (hit) then
      call write_snapshot(isn, t)
      isn = isn + 1
    end if
    if (.not. (maxval(abs(u1)) < 1.0d6)) stop 'blow-up of the NUMERICAL solution: stopping'
  end do
  close(unit_h)
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
  subroutine rhs(t_, u, w, du, dw)
    real(dp), intent(in)  :: t_, u(nr,nz), w(nr,nz)
    real(dp), intent(out) :: du(nr,nz), dw(nr,nz)
    integer :: i, j
    real(dp) :: psr, psz, ur_r, ur_z, wr, wz, lapu, lapw, r
    call poisson_solve(w, psi)
    call pad(psi, pp);  call pad(u, pu);  call pad(w, pw)
    call eval_force(nr, nz, rc, zc, t_, F1, F2)
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
        du(i,j) = -ur(i,j)*ur_r - uz(i,j)*ur_z + 2.0d0*u(i,j)*psz + lapu + F1(i,j)
        dw(i,j) = -ur(i,j)*wr   - uz(i,j)*wz &
                + (pu(i,j+1)**2 - pu(i,j-1)**2) / (2.0d0*dz) + lapw + F2(i,j)
      end do
    end do
  end subroutine rhs

  !-------------------------------------------------------------------------
  subroutine diagnostics(t_, dt_)
    real(dp), intent(in) :: t_, dt_
    integer :: i, j
    real(dp) :: tau, r, uth, dV, e, sw_th, sr2, sw_z, sz2, umth, umz, umr, fth, f2m
    real(dp) :: nu, du, np, dp2
    call poisson_solve(w1, psi)
    call pad(psi, pp)
    call eval_force(nr, nz, rc, zc, t_, F1, F2)
    call eval_exact(nr, nz, rc, zc, t_, ue, pe)
    tau = 1.0d0 - t_
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
    write(*,'(f10.6,f10.6,3es11.3,2f9.5,es11.3,es10.2,2es9.1,es9.1)') t_, tau, umth, umz, umr, &
         sqrt(sr2/sw_th), sqrt(sz2/sw_z), e, fth, sqrt(du/nu), sqrt(dp2/np), dt_
    write(unit_h,'(13es18.8e3)') t_, tau, umth, umz, umr, sqrt(sr2/sw_th), sqrt(sz2/sw_z), e, fth, f2m, &
         sqrt(du/nu), sqrt(dp2/np), dt_
    flush(unit_h)
  end subroutine diagnostics

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
    call eval_force(nr, nz, rc, zc, t_, F1, F2)
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

end program ns_axi_blowup
