! VERSION v4 (2026-10-01): horizon values written to diagnostics.h5 / state_*.h5 via output_mod
!============================
! Vacumm kerr driver, stage 1
!============================
program main
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: NVARS, IALPHA, IPHI, IK, IBETAX, IBX, alloc_state, ICONFMET, IATILDE
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections
  use constraints_mod, only: hamiltonian_constraint, momentum_constraint
  use timestep_mod, only: rk4_step
  use excision_mod, only: extrapolate_excised, is_excised, set_excision_background, &
                          excision_characteristic_check
  use output_mod, only: output_init, output_write_snapshot, &
    output_write_diagnostics, output_finalize, OUT_NHZ
  use deriv_stencils_mod
  use horizon_mod, only: horizon_t, horizon_init_grid, horizon_init, &
    horizon_adm_from_state, find_horizon, horizon_log_header, &
    horizon_log_line, horizon_inside_excision

  implicit none

  real(dp), allocatable :: u(:,:,:), Hgrid(:,:), Mgrid(:,:,:)
  integer :: imin, imax, kmin, kmax
  real(dp) :: Hmax, Hrms, Mmax
  integer :: n, npts
  real(dp) :: dt, t
  integer :: axis_k, i, k
  real(dp) :: diag_margin
  real(dp) :: r_horizon
  real(dp) :: char_speed
  integer :: n_char_bad
  character(len=64) :: buf
  integer :: envstat
  character(len=256) :: par_file

  ! --- apparent-horizon diagnostics (opt-in: GR_HORIZON_EVERY=<steps>, 0 = off)
  type(horizon_t) :: hz
  integer :: hz_every, hz_unit, hz_ierr, hz_nl, hz_last_step
  real(dp) :: hz_vals(OUT_NHZ)   ! found, area, m_irr, J, m_H, r_min, r_max -> output_mod
  real(dp) :: hz_ref = 0.0_dp, hz_drift_tol = 0.0_dp, drift
  logical :: hz_abort_drift = .true.
  logical :: hz_eqsym
  ! Vacuum run: metric-only output
  ! Horizon switch is read BEFORE output_init so the hz_* columns can be created.
  hz_every = 0
  hz_last_step = -1
  hz_vals = 0.0_dp
  call get_command_argument(1, par_file)
  write(*,*) "Reading parameter file: ", trim(par_file)
  call init_params(par_file)
  ! Read hz_every_par only AFTER init_params has parsed the file.  Reading it
  ! before (as this used to) silently discarded the par-file `hz_every` key and
  ! used the module default (10) instead.
  hz_every = hz_every_par


  call setup_grid()
  call alloc_state(u, Nx, Nz)

  diag_margin = 1.0_dp
  call get_environment_variable("GR_DIAG_MARGIN", buf, status=envstat)
  if (envstat == 0) read(buf,*) diag_margin



  call get_environment_variable("GR_HORIZON_EVERY", buf, status=envstat)
  if (envstat == 0) read(buf,*) hz_every

  call output_init('output', has_horizon=(hz_every > 0))

  ! Horizon finder setup (needs xg/zg from setu p_grid and the output/ directory)
  if (hz_every > 0) then
    hz_eqsym = (zg(1) > -1.0e-12_dp)          ! z>=0 only  ->  equatorial symmetry
    hz_nl = 8
    if (.not. hz_eqsym) hz_nl = 10
    ! requires xg(1) = 0 or dx/2 (and zg(1) = 0 or dz/2 if hz_eqsym); the
    ! first interior point is array index (1,1) -- change the last two
    ! arguments of horizon_adm_from_state below if you have low-side ghosts.
    call horizon_init_grid(xg(1), zg(1), dx, dz, Nx, Nz, hz_eqsym)
    call horizon_init(hz, hz_nl, 0.0_dp, hz_eqsym)
    open(newunit=hz_unit, file='output/horizon.dat', status='replace', action='write')
    call horizon_log_header(hz_unit)
  end if

  print *, "=== GR BSSNOK (vacuum, Cartoon axisymmetric) - Milestone 1 ==="
  print '(A,I0,A,I0)', " Grid: Nx=", Nx, "  Nz=", Nz
  print '(A,F6.3,A,F6.3,A,F6.3)', " dx=", dx, "  dy=", dy, "  dz=", dz
  print '(A,F6.3,A,F6.3)', " BH mass M=", bh_mass, "   spin a=", bh_spin
  r_horizon = bh_mass + sqrt(max(bh_mass**2 - bh_spin**2, 0.0_dp))
  hz_drift_tol = 0.01_dp*max(r_horizon, 1.0_dp)
  call get_environment_variable("GR_HZ_ABORT", buf, status=envstat)
  if (envstat == 0) then
    block
      integer :: hz_abort_i
      read(buf,*) hz_abort_i
      hz_abort_drift = (hz_abort_i /= 0)
    end block
  end if
  print '(A,F6.3,A,F6.3,A)', " Excision radius r_excise=", r_excise, &
    "  (horizon at r_+=", r_horizon, "M; interior frozen)"
  print *

  call set_kerr_schild_id(u)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call init_deriv_stencils()

  ! Build the excision mask and fill the horizon interior BEFORE any
  ! derivative-consuming routine (the Hamiltonian diagnostic below, then
  ! every RHS evaluation in the time loop) touches u. Without this, the
  ! first calls run against an unbuilt excision mask.
  call extrapolate_excised(u)
  call excision_characteristic_check(u, char_speed, n_char_bad)
  print '(A,ES12.4,A,I0)', " Initial max inner characteristic speed=", char_speed, &
    "  incoming lapse-gauge points=", n_char_bad

  allocate(Hgrid(Nx,Nz))
  allocate(Mgrid(Nx,Nz,3))
  call hamiltonian_constraint(u, Hgrid, imin, imax, kmin, kmax)
  Hmax = 0.0_dp; Hrms = 0.0_dp; npts = 0
  do k = kmin, kmax
    do i = imin, imax
      if (sqrt(xg(i)**2 + zg(k)**2) < r_excise + diag_margin) cycle
      Hmax = max(Hmax, abs(Hgrid(i,k)))
      Hrms = Hrms + Hgrid(i,k)**2
      npts = npts + 1
    end do
  end do
  Hrms = sqrt(Hrms / real(max(npts,1), dp))
  print '(A,ES12.4,A,ES12.4)', " t=0 Hamiltonian constraint (exterior): max|H| = ", Hmax, &
    "   rms|H| = ", Hrms
  block
    integer :: ii, kk0
    real(dp) :: rr
    ii = -1; kk0 = -1
    do k = kmin, kmax
      do i = imin, imax
        if (sqrt(xg(i)**2 + zg(k)**2) < r_excise + diag_margin) cycle
        if (ii < 0) then
          ii = i; kk0 = k
        else if (abs(Hgrid(i,k)) >= abs(Hgrid(ii,kk0))) then
          ii = i; kk0 = k
        end if
      end do
    end do
    if (ii > 0) then
      rr = sqrt(xg(ii)**2 + zg(kk0)**2)
      print '(A,I0,A,I0,A,F8.4,A,ES12.4)', " worst point: i=", ii, " k=", kk0, &
        "  varpi~r=", rr, "  H=", Hgrid(ii,kk0)
    end if
    print *, " H at a few sample radii along the axis (x=0):"
    do kk0 = kmin, kmax, max(1,(kmax-kmin)/8)
      print '(A,F8.4,A,ES12.4)', "   z=", zg(kk0), "   H=", Hgrid(imin,kk0)
    end do
  end block

  ! Representative axis point comfortably outside the excision radius, used
  ! for the lapse trace below (should evolve smoothly, unlike the frozen
  ! interior).
  axis_k = 1
  do k = kmin, kmax
    if (sqrt(xg(1)**2 + zg(k)**2) >= r_excise + 1.0_dp) then
      axis_k = k
      exit
    end if
  end do
  print '(A,F10.6,A,F8.4)', " Lapse alpha(x=0, outside excision) at t=0: ", u(1,axis_k,IALPHA), &
    "  at z=", zg(axis_k)
  print *

  if (hz_every > 0) call update_horizon(0, 0.0_dp)

  ! The fastest principal gauge characteristic is sqrt(2) times the
  ! light speed.  Include that factor in the explicit RK4 CFL bound rather
  ! than relying on a coordinate-speed diagnostic after the step.
  dt = cfl * min(dx, dz) / sqrt(2.0_dp)
  t = 0.0_dp
  print '(A,F8.5)', " Time step dt = ", dt
  if (slicing_mode == 2 .and. gamma_driver_mode == 2) then
    print *, "Evolving vacuum Kerr-Schild data with frozen lapse and shift (diagnostic control)..."
  else
    if (slicing_mode == 1) then
      print *, " Slicing: maximal"
    else
      print *, " Slicing: 1+log"
    end if
    select case (gamma_driver_mode)
    case (0)
      print *, " Shift: legacy Gamma-driver (not a stationary puncture gauge)"
    case (1)
      print *, " Shift: analytic puncture-profile driver (experimental)"
    case (3)
      print *, " Shift: advective Gamma-driver (experimental)"
    case (4)
      print *, " Shift: non-advective Gamma-driver (stationarity candidate)"
    case default
      print *, " Shift: frozen Kerr-Schild control"
    end select
    print *, " Evolving vacuum Kerr data with excision..."
  end if

  ! t=0 diagnostics/snapshot, using the whole-domain Hmax/Hrms already
  ! computed above (step 0 has no exterior-only recomputation).
  call output_write_diagnostics(0, t, dt, Nx, Nz, u, Hmax, Hrms, horizon_vals=hz_vals)
  call output_write_snapshot(0, t, 'output', Nx, Nz, xg, zg, u, horizon_vals=hz_vals)

  do n = 1, nsteps
    call rk4_step(u, dt, n)
    call extrapolate_excised(u)
    call excision_characteristic_check(u, char_speed, n_char_bad)
    if (n_char_bad > 0 .and. mod(n, max(1,out_every)) == 0) then
      print '(A,I0,A,ES12.4,A,I0)', " WARNING step ", n, &
        ": inner boundary has incoming lapse-gauge modes; max speed=", char_speed, &
        " points=", n_char_bad
    end if
    t = t + dt

    ! Shift / Gamma-driver diagnostic.  Off by default (it prints every step);
    ! enable with GR_DIAG_SHIFT=1.  It is the fastest way to see whether the
    ! Gamma-driver is actually tracking its profile or decaying to zero.
    block
      logical :: diag_shift
      integer :: dstat, read_stat, diag_flag
      diag_shift = .false.
      call get_environment_variable("GR_DIAG_SHIFT", buf, status=dstat)
      if (dstat == 0) then
        read(buf,*,iostat=read_stat) diag_shift
        if (read_stat /= 0) then
          read(buf,*,iostat=read_stat) diag_flag
          if (read_stat == 0) diag_shift = (diag_flag /= 0)
        end if
      end if
      if (diag_shift) then
        block
          integer :: ii, kk0, bI, bK, GI, GK
          real(dp) :: rr, bmax, Gmax
          bmax = -1.0_dp; Gmax = -1.0_dp; bI = -1; bK = -1; GI = -1; GK = -1
          do kk0 = 1, Nz
            do ii = 1, Nx
              rr = sqrt(xg(ii)**2 + zg(kk0)**2)
              if (rr < r_excise .or. rr > r_excise + 3.0_dp*max(dx,dz)) cycle
              if (abs(u(ii,kk0,IBETAX)) > bmax) then
                bmax = abs(u(ii,kk0,IBETAX)); bI = ii; bK = kk0
              end if
              if (abs(u(ii,kk0,IBX)) > Gmax) then
                Gmax = abs(u(ii,kk0,IBX)); GI = ii; GK = kk0
              end if
            end do
          end do
          print '(A,I0,A,ES11.3,A,I0,A,I0,A,ES11.3,A,I0,A,I0)', &
            " [DIAG shift/driver] step ", n, &
            "  max|betax|=", bmax, " @(", bI, ",", bK, &
            ")  max|Gbar_x|=", Gmax, " @(", GI, ",", GK, ")"
        end block
      end if
    end block

    ! Near-boundary ring diagnostic: report at the ordinary output cadence,
    ! so a long run remains usable while still showing growth before a failure.
    if (mod(n, max(1,out_every)) == 0 .or. n == nsteps) then
    block
      integer :: ii, kk0, ai, ak, pI, pK, kI, kK
      real(dp) :: rr, ring_lo, ring_hi, amax, pmax, kmax_
      ring_lo = r_excise
      ring_hi = r_excise + 3.0_dp*max(dx,dz)
      amax = -1.0_dp; pmax = -1.0_dp; kmax_ = -1.0_dp
      ai=-1; ak=-1; pI=-1; pK=-1; kI=-1; kK=-1
      do kk0 = 1, Nz
        do ii = 1, Nx
          rr = sqrt(xg(ii)**2 + zg(kk0)**2)
          if (rr < ring_lo .or. rr > ring_hi) cycle
          if (abs(u(ii,kk0,IALPHA)) > amax .or. u(ii,kk0,IALPHA) /= u(ii,kk0,IALPHA)) then
            amax = abs(u(ii,kk0,IALPHA)); ai = ii; ak = kk0
          end if
          if (abs(u(ii,kk0,IPHI)) > pmax .or. u(ii,kk0,IPHI) /= u(ii,kk0,IPHI)) then
            pmax = abs(u(ii,kk0,IPHI)); pI = ii; pK = kk0
          end if
          if (abs(u(ii,kk0,IK)) > kmax_ .or. u(ii,kk0,IK) /= u(ii,kk0,IK)) then
            kmax_ = abs(u(ii,kk0,IK)); kI = ii; kK = kk0
          end if
        end do
      end do
      print '(A,I0,A,ES11.3,A,I0,A,I0,A,ES11.3,A,I0,A,I0,A,ES11.3,A,I0,A,I0)', &
        " [ring r in (r_excise, r_excise+3dx)] step ", n, &
        "  max|alpha|=", amax, " @(", ai, ",", ak, &
        ")  max|phi|=", pmax, " @(", pI, ",", pK, &
        ")  max|K|=", kmax_, " @(", kI, ",", kK, ")"
    end block
    end if

    ! lightweight safety net: report (but do not spam) any NaN outside excision
    block
      integer :: ii, kk0
      logical :: found
      found = .false.
      do kk0 = 1, Nz
        do ii = 1, Nx
          if (is_excised(ii,kk0)) cycle
          if (u(ii,kk0,IALPHA) /= u(ii,kk0,IALPHA)) then
            if (.not. found) then
              print '(A,I0,A,F8.4,A,F8.4)', " NaN outside excision at step ", n, &
                "  x=", xg(ii), "  z=", zg(kk0)
              found = .true.
              exit
            end if
          end if
        end do
      end do
    end block

    if (hz_every > 0) then
      if (mod(n, hz_every) == 0 .or. n == nsteps) call update_horizon(n, t)
    end if

    if (mod(n, out_every) == 0 .or. n == nsteps) then
      ! make sure the horizon values written below belong to THIS step
      if (hz_every > 0) then
        if (hz_last_step /= n) call update_horizon(n, t)
      end if
      call hamiltonian_constraint(u, Hgrid, imin, imax, kmin, kmax)
      call momentum_constraint(u, Mgrid)

      ! Stats over the exterior (evolved) region only.
      Hmax = 0.0_dp; Hrms = 0.0_dp; npts = 0
      do k = kmin, kmax
        do i = imin, imax
          if (sqrt(xg(i)**2 + zg(k)**2) < r_excise + diag_margin) cycle
          Hmax = max(Hmax, abs(Hgrid(i,k)))
          Hrms = Hrms + Hgrid(i,k)**2
          npts = npts + 1
        end do
      end do
      Hrms = sqrt(Hrms / real(max(npts,1), dp))

      ! Momentum constraint residual over the same exterior region.  Both
      ! differential constraints matter for a puncture evolution: H growing
      ! signals the gauge/inner boundary is wrong, M growing signals the
      ! characteristic fields are inconsistent.  Reported together so the
      ! failure mode is identifiable rather than guessed at.
      Mmax = 0.0_dp
      do k = kmin, kmax
        do i = imin, imax
          if (sqrt(xg(i)**2 + zg(k)**2) < r_excise + diag_margin) cycle
          Mmax = max(Mmax, sqrt(Mgrid(i,k,1)**2 + Mgrid(i,k,2)**2 + Mgrid(i,k,3)**2))
        end do
      end do

      print '(A,I5,A,F8.4,A,ES12.4,A,ES12.4,A,ES12.4,A,F10.6)', &
        " step ", n, "  t=", t, "  max|H|(ext)=", Hmax, "  rms|H|(ext)=", Hrms, &
        "  max|M|(ext)=", Mmax, "  alpha(probe)=", u(1,axis_k,IALPHA)

      call output_write_diagnostics(n, t, dt, Nx, Nz, u, Hmax, Hrms, horizon_vals=hz_vals)
      call output_write_snapshot(n, t, 'output', Nx, Nz, xg, zg, u, horizon_vals=hz_vals)

      block
        integer :: ii, kk0, bi, bk
        real(dp) :: bval, rr
        bval = 0.0_dp; bi = -1; bk = -1
        do k = kmin, kmax
          do i = imin, imax
            if (sqrt(xg(i)**2 + zg(k)**2) < r_excise + diag_margin) cycle
            if (abs(Hgrid(i,k)) > bval) then
              bval = abs(Hgrid(i,k)); bi = i; bk = k
            end if
          end do
        end do
        if (bi > 0) then
          rr = sqrt(xg(bi)**2 + zg(bk)**2)
          print '(A,F8.4,A,F8.4)', "    (worst point beyond margin at r=", rr, &
            "  vs r_excise+margin=", r_excise + diag_margin
        end if
      end block
    end if
  end do

  if (hz_every > 0) close(hz_unit)
  call output_finalize()

contains

  !---------------------------------------------------------------------------
  ! Find the apparent horizon in the current state and log/print it.  The area,
  ! J and M_H of a stationary Kerr BH are gauge-invariant, so their drift from
  ! the exact values is a direct, slicing-independent accuracy monitor.
  !---------------------------------------------------------------------------
  subroutine update_horizon(step, tnow)
    integer,  intent(in) :: step
    real(dp), intent(in) :: tnow
    real(dp), parameter :: pi_ = 3.141592653589793238462643383279502884_dp
    real(dp) :: area_ex, j_ex, rejected_hmin, rejected_hmax, reject_limit

    hz_last_step = step
    hz_vals = 0.0_dp                       ! found = 0 unless the solve succeeds below

    if (any(u(:,:,IALPHA) /= u(:,:,IALPHA))) then
      print '(A,I0,A)', " [horizon] step ", step, ": skipped (NaN in state)"
      return
    end if

    call horizon_adm_from_state(u, IPHI, IK, ICONFMET, IATILDE, 1, 1)
    call find_horizon(hz, r_horizon, hz_ierr)     ! r_horizon only seeds the 1st solve

    if (hz_ierr /= 0) then
      call horizon_log_line(hz_unit, step, tnow, hz)
      flush(hz_unit)
      print '(A,I0,A,I0,A,ES9.2)', " [horizon] step ", step, ": NOT FOUND (ierr=", &
        hz_ierr, ")  last max|Theta|=", hz%resid
      return
    end if

    ! The horizon finder interpolates the evolved metric.  A mathematically
    ! converged surface is not physical if it enters the excision/interpolation
    ! buffer, where the state is ghost-filled rather than evolved.
    reject_limit = r_excise + 0.5_dp*max(dx,dz)
    if (hz%hmin <= reject_limit) then
      rejected_hmin = hz%hmin
      rejected_hmax = hz%hmax
      hz_vals = 0.0_dp
      hz%found = .false.
      hz%area = 0.0_dp
      hz%m_irr = 0.0_dp
      hz%j_spin = 0.0_dp
      hz%m_h = 0.0_dp
      ! Keep the geometric extent in horizon.dat so the reason for rejection
      ! remains diagnosable; output_mod masks all diagnostic fields for F rows.
      hz%hmin = rejected_hmin
      hz%hmax = rejected_hmax
      call horizon_log_line(hz_unit, step, tnow, hz)
      flush(hz_unit)
      print '(A,I0,A,F9.5,A,F9.5)', " [horizon] step ", step, &
        ": rejected surface inside excision buffer (r_min=", rejected_hmin, &
        ", limit=", reject_limit, ")"
      return
    end if

    hz_vals = [1.0_dp, hz%area, hz%m_irr, hz%j_spin, hz%m_h, hz%hmin, hz%hmax]

    ! Stationarity guard.  For a single stationary vacuum black hole the found
    ! surface must stay put.  A steady inward drift means the gauge has no
    ! puncture anchoring the interior -- the lapse never vanishes at r_excise,
    ! the interior degenerates, and the run will eventually blow up near the
    ! inner boundary.  Warn as soon as the drift is unambiguous rather than
    ! letting the user find out 18000 steps later.
    if (hz_ref <= 0.0_dp) then
      hz_ref = hz%hmin
    else
      drift = hz_ref - hz%hmin
      if (drift > hz_drift_tol) then
        print '(A,I0,A,ES10.4,A,ES10.4,A,ES10.4,A,ES10.4,A)', &
          " [horizon] step ", step, ": DRIFT r_min ", hz_ref, " -> ", hz%hmin, &
          "  (d=", drift, ', rate=', drift/max(tnow,1.0e-12_dp), '/t)'
        print '(A)', "   -> stationary solution is NOT stationary: check the gauge (1+log puncture lapse/shift) first."
        if (hz_abort_drift) then
          print '(A,I0,A,I0,A)', " ABORT at step ", step, " (of ", nsteps, &
            " requested)."
          print '(A)', " The horizon is migrating into the excision region, so"
          print '(A)', " this run cannot produce valid physics; continuing only"
          print '(A)', " ends in a non-positive conformal determinant ~1.8e4"
          print '(A)', " steps later. Set GR_HZ_ABORT=0 to override."
          error stop 'horizon drift: the gauge is not stationary'
        end if
        hz_drift_tol = 2.0_dp*drift      ! escalate to the next doubling
      end if
    end if

    call horizon_log_line(hz_unit, step, tnow, hz)
    flush(hz_unit)

    area_ex = 8.0_dp*pi_*bh_mass*r_horizon          ! exact Kerr: 8 pi M r_+
    j_ex    = bh_spin*bh_mass
    print '(A,I0,A,F9.5,A,F9.5,A,ES10.2)', " [horizon] step ", step, &
      "  r in [", hz%hmin, ",", hz%hmax, "]  dA/A_Kerr=", hz%area/area_ex - 1.0_dp
    print '(A,F9.6,A,F9.6,A,F9.6)', "            M_irr=", hz%m_irr, &
      "  J=", hz%j_spin, "  M_H=", hz%m_h
    if (horizon_inside_excision(hz, r_excise)) &
      print '(A,F7.4,A,F7.4)', "            WARNING: horizon r_min=", hz%hmin, &
        " is within 1.2*r_excise =", 1.2_dp*r_excise
  end subroutine update_horizon

end program main
