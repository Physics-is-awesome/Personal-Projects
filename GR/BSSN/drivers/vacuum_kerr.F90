! VERSION v4 (2026-10-01): horizon values written to diagnostics.h5 / state_*.h5 via output_mod
!============================
! Vacumm kerr driver, stage 1
!============================
program main
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: NVARS, IALPHA, IPHI, IK, IBETAX, IBETAZ, IBX, &
                      IGXX, IGXZ, IGZZ, alloc_state, ICONFMET, IATILDE
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections, &
                              fill_conformal_connection_boundary
  use eq_kerr_bssn_data_mod, only: set_eq_kerr_trumpet_id, initialize_kerr_f_cache
  use constraints_mod, only: hamiltonian_constraint, momentum_constraint
  use timestep_mod, only: rk4_step
  use excision_mod, only: extrapolate_excised, is_excised, set_excision_background, &
                          excision_characteristic_check, kerr_radius_squared
  use output_mod, only: output_init, output_write_snapshot, &
    output_write_diagnostics, output_finalize, OUT_NHZ
  use deriv_stencils_mod
  use fo_normal_system_mod, only: fo_published_speeds, FO_NG_NGAUGE, FO_NN_NGAUGE
  use fo_curved_sat_mod, only: kerr_schild_excision_normal
  use horizon_mod, only: horizon_t, horizon_init_grid, horizon_init, &
    horizon_adm_from_state, find_horizon, horizon_log_header, &
    horizon_log_line, horizon_inside_excision, horizon_set_excision

  implicit none

  real(dp), allocatable :: u(:,:,:), Hgrid(:,:), Mgrid(:,:,:), Mterms(:,:,:,:)
  integer :: imin, imax, kmin, kmax
  real(dp) :: Hmax, Hrms, Mmax
  integer :: n, npts
  real(dp) :: dt, t
  real(dp) :: published_gauge(FO_NG_NGAUGE), published_nongauge(FO_NN_NGAUGE)
  real(dp) :: shell_alpha, shell_phi, shell_beta_n, shell_gamma_nn
  real(dp) :: shell_nx, shell_nz, shell_gnorm
  integer :: incoming_gauge, incoming_nongauge
  integer :: axis_k, i, k
  ! Constraint shells, keyed to the apparent horizon (see the note below).
  real(dp) :: near_mask_w        ! horizon-shell width, in cells
  real(dp) :: r_ah               ! apparent-horizon radius (fallback: analytic r_+)
  real(dp) :: shell_w            ! horizon-shell width in length units
  real(dp) :: junk_H, junk_R, junk_M
  real(dp) :: hor_H,  hor_R,  hor_M
  real(dp) :: alpha_min
  integer  :: junk_n, hor_n, hor_i, hor_k
  integer  :: ext_i, ext_k
  integer  :: skip_i = -1, skip_k = -1   ! sentinel: do not record a worst point
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
  ! Gauge-invariant stationarity guard.  The horizon COORDINATE position r_min
  ! is a poor stationarity measure here: under refinement r_min diverges while
  ! area / M_irr / J converge (see the resolution evidence in the Makefile).  So
  ! r_min drift is reported but does not abort; the abort keys on the
  ! invariants, which are the quantities a stationary vacuum BH must conserve.
  real(dp) :: inv_ref_mirr = 0.0_dp, inv_ref_area = 0.0_dp
  real(dp) :: dmirr = 0.0_dp, darea = 0.0_dp, inv_tol = 0.05_dp
  logical :: hz_abort_drift = .true.
  ! DIAGNOSTIC PROBE (default off).  The excision fill is
  !   u = background + one-sided-polynomial extrapolation of (u - background)
  ! and set_excision_background() is otherwise called ONCE before the time
  ! loop, so the interior stays pinned to the t=0 data while the exterior
  ! evolves.  Setting GR_REFRESH_BG=1 re-anchors the background every step so
  ! the A/B difference isolates how much the stale anchor contributes.  See
  ! baseline/INNER_BOUNDARY_FINDINGS.md.
  logical :: refresh_bg = .false.
  logical :: hz_eqsym
  logical :: write_hdf5_output
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

  ! ---------------------------------------------------------------------
  ! Constraint-diagnostic shells.
  !
  ! These used to be defined as r > r_excise + diag_margin (exterior) and
  ! r_excise < r < r_excise + w (near-mask).  Both are wrong when the mask
  ! sits inside the horizon, which it always does here (r_excise ~ 0.55 r_+):
  !
  !   * with r_excise = 1.0 and diag_margin = 1.0 the exterior test discards
  !     r < 2.0, i.e. the whole shell r_+ < r < 2.0 in which inner-boundary
  !     error can legitimately enter the physical domain;
  !   * the near-mask band starts at r_excise and therefore contains the
  !     inside-horizon region r_excise < r < r_+, where the constraint has no
  !     physical meaning.  Its maximum is inside-horizon junk (measured at
  !     r ~ 1.06 for r_excise = 1.0), not a boundary-operator error.
  !
  ! Shells are now keyed to the apparent horizon r_ah:
  !
  !   inside-horizon  [r_excise, r_ah)        reported, never gated
  !   horizon shell   [r_ah, r_ah + w)        reported, this is the gate
  !   exterior        [r_ah + w, inf)         reported, this is the claim
  !
  ! w = GR_NEAR_MASK_W cells (default 2).  Set GR_NEAR_MASK_W=0 to collapse
  ! the horizon shell into the exterior.
  ! ---------------------------------------------------------------------
  near_mask_w = 2.0_dp
  call get_environment_variable("GR_NEAR_MASK_W", buf, status=envstat)
  if (envstat == 0) read(buf,*) near_mask_w



  call get_environment_variable("GR_HORIZON_EVERY", buf, status=envstat)
  if (envstat == 0) read(buf,*) hz_every

  ! Optional text-only mode for isolated evolution diagnostics. The default
  ! remains full HDF5 output.
  write_hdf5_output = .true.
  call get_environment_variable("GR_WRITE_HDF5", buf, status=envstat)
  if (envstat == 0) write_hdf5_output = (trim(buf) /= '0')

  call output_init('output', has_horizon=(hz_every > 0))
  call write_run_info()

  ! Horizon finder setup (needs xg/zg from setu p_grid and the output/ directory)
  if (hz_every > 0) then
    hz_eqsym = (zg(1) > -1.0e-12_dp)          ! z>=0 only  ->  equatorial symmetry
    ! The grid spans both hemispheres, but the centered Kerr horizon is still
    ! reflection-symmetric. Keep both z hemispheres for interpolation while
    ! constraining the surface fit to even Legendre modes.
    hz_nl = 8
    ! requires xg(1) = 0 or dx/2 (and zg(1) = 0 or dz/2 if hz_eqsym); the
    ! first interior point is array index (1,1) -- change the last two
    ! arguments of horizon_adm_from_state below if you have low-side ghosts.
    call horizon_init_grid(xg(1), zg(1), dx, dz, Nx, Nz, hz_eqsym)
    ! The finder's 4th-order stencil must not read the frozen interior.
    ! The evolution excision and horizon mask are distinct. The horizon
    ! module extends its mask outward by 1.5 cells; offset its mask inward by
    ! two cells so the extension ends half a cell inside the true excision
    ! surface, rather than covering the physical horizon at high resolution.
    call horizon_set_excision(max(0.0_dp, r_excise-2.0_dp*max(dx,dz)), bh_spin)
    call horizon_init(hz, hz_nl, 0.0_dp, .true.)
    open(newunit=hz_unit, file='output/horizon.dat', status='replace', action='write')
    call horizon_log_header(hz_unit)
  end if

  print *, "=== GR BSSNOK (vacuum, Cartoon axisymmetric) - Milestone 1 ==="
  print '(A,I0,A,I0)', " Grid: Nx=", Nx, "  Nz=", Nz
  print '(A,F6.3,A,F6.3,A,F6.3)', " dx=", dx, "  dy=", dy, "  dz=", dz
  print '(A,F6.3,A,F6.3)', " BH mass M=", bh_mass, "   spin a=", bh_spin
  r_horizon = bh_mass + sqrt(max(bh_mass**2 - bh_spin**2, 0.0_dp))
  hz_drift_tol = 0.01_dp*max(r_horizon, 1.0_dp)
  call get_environment_variable("GR_REFRESH_BG", buf, status=envstat)
  if (envstat == 0) then
    block
      integer :: rbg_i
      read(buf,*) rbg_i
      refresh_bg = (rbg_i /= 0)
    end block
  end if
  if (refresh_bg) &
       print '(A)', " DIAGNOSTIC: excision background re-anchored every step (GR_REFRESH_BG=1)"
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

  if (initial_data_mode == 1) then
    print *, " Initial data: Dennison-Baumgarte-Montero trumpet (route C)"
    call set_eq_kerr_trumpet_id(u, bh_mass, bh_spin, bh_mass)
  else
    call set_kerr_schild_id(u)
  end if
  if (slicing_mode == 4) then
    if (initial_data_mode /= 1) error stop 'slicing_mode=4 requires DBM route-C initial data'
    call initialize_kerr_f_cache()
    print *, " Lapse gauge: experimental cached DBM position-dependent profile"
  end if
  call compute_conformal_connections(u)
  call fill_conformal_connection_boundary(u)
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
  allocate(Mterms(Nx,Nz,3,7))
  hor_i = 0; hor_k = 0; ext_i = 0; ext_k = 0
  call hamiltonian_constraint(u, Hgrid, imin, imax, kmin, kmax)
  call momentum_constraint(u, Mgrid, Mterms)

  ! Shells keyed to the horizon, not to r_excise -- see the note near
  ! near_mask_w above.  At t=0 the horizon has not been found yet, so
  ! shell_radius() falls back to the analytic r_+.
  r_ah = shell_radius()
  shell_w = near_mask_w*max(dx,dz)
  call shell_stats(r_excise, r_ah, junk_H, junk_R, junk_M, junk_n, skip_i, skip_k)
  call shell_stats(r_ah, r_ah + shell_w, hor_H, hor_R, hor_M, hor_n, hor_i, hor_k)
  call shell_stats(r_ah + shell_w, huge(1.0_dp), Hmax, Hrms, Mmax, npts, ext_i, ext_k)

  print '(A,ES12.4,A,ES12.4)', " t=0 Hamiltonian constraint (exterior): max|H| = ", Hmax, &
    "   rms|H| = ", Hrms
  if (hor_n > 0) &
    print '(A,F8.4,A,F8.4,A,ES12.4,A,ES12.4,A,F8.4)', "   horizon-shell [", r_ah, ",", &
      r_ah + shell_w, "]:  max|H|=", hor_H, "  max|M|=", hor_M, &
      "  worst r=", sqrt(kerr_radius_squared(xg(hor_i), zg(hor_k)))
  if (junk_n > 0) &
    print '(A,F8.4,A,F8.4,A,ES12.4,A,ES12.4,A)', "   inside-horizon [", r_excise, ",", &
      r_ah, "):  max|H|=", junk_H, "  max|M|=", junk_M, "  (not gated)"
  print *, " H at a few sample radii along the axis (x=0):"
  do k = kmin, kmax, max(1,(kmax-kmin)/8)
    print '(A,F8.4,A,ES12.4)', "   z=", zg(k), "   H=", Hgrid(imin,k)
  end do

  ! Representative axis point comfortably outside the excision radius, used
  ! for the lapse trace below (should evolve smoothly, unlike the frozen
  ! interior).
  axis_k = 1
  do k = kmin, kmax
    if (sqrt(kerr_radius_squared(xg(1), zg(k))) >= r_excise + 1.0_dp) then
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
    else if (slicing_mode == 3) then
      print *, " Slicing: tabulated Bona-Masso (route C stationary Kerr)"
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
  if (write_hdf5_output) then
    call output_write_diagnostics(0, t, dt, Nx, Nz, u, Hmax, Hrms, horizon_vals=hz_vals)
    call output_write_snapshot(0, t, 'output', Nx, Nz, xg, zg, u, horizon_vals=hz_vals)
  end if

  do n = 1, nsteps
    if (refresh_bg) call set_excision_background(u)
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
              rr = sqrt(kerr_radius_squared(xg(ii), zg(kk0)))
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
          rr = sqrt(kerr_radius_squared(xg(ii), zg(kk0)))
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
      call momentum_constraint(u, Mgrid, Mterms)

      ! Shells keyed to the apparent horizon.  r_excise sits inside r_+, so
      ! anchoring the exterior at r_excise + margin hides the only region in
      ! which inner-boundary error can reach the physical domain; anchoring a
      ! "near-mask" band at r_excise instead reports inside-horizon junk.
      r_ah = shell_radius()
      shell_w = near_mask_w*max(dx,dz)

      ! shell_stats leaves output indices untouched when the requested shell
      ! has no cells (notably GR_NEAR_MASK_W=0). Initialize them before each
      ! scan so optional per-cell diagnostics never consume stale stack data.
      hor_i=0; hor_k=0; ext_i=0; ext_k=0
      call shell_stats(r_excise, r_ah, junk_H, junk_R, junk_M, junk_n, skip_i, skip_k)
      call shell_stats(r_ah, r_ah + shell_w, hor_H, hor_R, hor_M, hor_n, hor_i, hor_k)
      call shell_stats(r_ah + shell_w, huge(1.0_dp), Hmax, Hrms, Mmax, npts, ext_i, ext_k)

      ! Minimum lapse over the evolved domain.  For slicing_mode = 3 this is
      ! the long-term indicator: it falls monotonically from the first cell
      ! outside the mask because an inward lapse mode is allowed to collapse
      ! there, and it is what drives f(alpha) and hence d_t alpha at long
      ! times.  See baseline/GAUGE_COLLAPSE.md.
      alpha_min = u(1,1,IALPHA)
      do k = kmin, kmax
        do i = imin, imax
          if (sqrt(kerr_radius_squared(xg(i), zg(k))) < r_excise) cycle
          alpha_min = min(alpha_min, u(i,k,IALPHA))
        end do
      end do

      print '(A,I5,A,F8.4,A,ES12.4,A,ES12.4,A,ES12.4,A,F10.6,A,F8.5)', &
        " step ", n, "  t=", t, "  max|H|(ext)=", Hmax, "  rms|H|(ext)=", Hrms, &
        "  max|M|(ext)=", Mmax, "  alpha(probe)=", u(1,axis_k,IALPHA), &
        "  alpha_min=", alpha_min

      ! The horizon shell is the only place a mask-edge error can enter the
      ! evolved domain, so it is reported separately and is not merged into
      ! the exterior statistics.
      if (near_mask_w > 0.0_dp .and. hor_n > 0) then
        write(*,'(A,F8.4,A,F8.4,A,I6,A,ES12.4,A,ES12.4,A,F8.4)') &
          "    horizon-shell [", r_ah, ",", r_ah + shell_w, "]  n=", hor_n, &
          "  max|H|=", hor_H, "  max|M|=", hor_M, &
          "  worst r=", sqrt(kerr_radius_squared(xg(hor_i), zg(hor_k)))
      end if
      if (hor_i > 0 .and. hor_k > 0) then
        write(*,'(A,7(1X,ES11.3))') "    momentum terms at worst shell H [1..7]=", &
          Mterms(hor_i,hor_k,1,:)
        shell_alpha = u(hor_i,hor_k,IALPHA)
        shell_phi = u(hor_i,hor_k,IPHI)
        call kerr_schild_excision_normal(xg(hor_i), zg(hor_k), bh_spin, r_excise, &
                                         shell_nx, shell_nz, shell_gnorm)
        shell_beta_n = u(hor_i,hor_k,IBETAX)*shell_nx + &
          u(hor_i,hor_k,IBETAZ)*shell_nz
        shell_gamma_nn = exp(-4.0_dp*shell_phi) * &
          (u(hor_i,hor_k,IGXX)*shell_nx*shell_nx + &
           2.0_dp*u(hor_i,hor_k,IGXZ)*shell_nx*shell_nz + &
           u(hor_i,hor_k,IGZZ)*shell_nz*shell_nz)
        if (shell_alpha > 0.0_dp .and. shell_gamma_nn > 0.0_dp) then
          call fo_published_speeds(shell_alpha, shell_phi, shell_gamma_nn, &
                                   shell_beta_n, published_gauge, published_nongauge)
          incoming_gauge = count(published_gauge > 0.0_dp)
          incoming_nongauge = count(published_nongauge > 0.0_dp)
          write(*,'(A,I0,A,I0,A)') "    published normal modes entering exterior: gauge=", &
            incoming_gauge, " non-gauge=", incoming_nongauge, &
            " (diagnostic; not a production characteristic map)"
        end if
      end if
      ! Everything from r_excise out to r_+ is inside the horizon: no
      ! characteristic leaves it, so a large residual there is expected and is
      ! not a pass/fail criterion.  It is reported so that growth toward the
      ! horizon -- which WOULD matter -- stays visible.
      if (junk_n > 0) then
        write(*,'(A,F8.4,A,F8.4,A,I6,A,ES12.4,A,ES12.4,A)') &
          "    inside-horizon [", r_excise, ",", r_ah, ")  n=", junk_n, &
          "  max|H|=", junk_H, "  max|M|=", junk_M, "  (not gated)"
      end if

      if (write_hdf5_output) then
        call output_write_diagnostics(n, t, dt, Nx, Nz, u, Hmax, Hrms, horizon_vals=hz_vals)
        call output_write_snapshot(n, t, 'output', Nx, Nz, xg, zg, u, horizon_vals=hz_vals)
      end if
    end if
  end do

  if (hz_every > 0) close(hz_unit)
  call output_finalize()

contains

  !---------------------------------------------------------------------------
  ! Radius of the shell boundary: the apparent-horizon radius if the finder
  ! has one, otherwise the analytic Kerr r_+.  Never r_excise: the mask is
  ! deliberately inside the horizon and is not a physical boundary.
  !---------------------------------------------------------------------------
  real(dp) function shell_radius() result(rr)
    rr = r_horizon
    if (hz_every > 0 .and. hz%found) rr = hz%hmin
  end function shell_radius

  !---------------------------------------------------------------------------
  ! Constraint statistics in the radial shell rlo <= r_KS < rhi.
  ! iw/kw receive the index of the largest |H| in the shell.  Pass exactly
  ! -1 in iw to skip recording; any other value (including the 0 reset used
  ! before each scan) is a valid "record here" request.
  !---------------------------------------------------------------------------
  subroutine shell_stats(rlo, rhi, hmax, hrms, mmax, ns, iw, kw)
    real(dp), intent(in)    :: rlo, rhi
    real(dp), intent(out)   :: hmax, hrms, mmax
    integer,  intent(out)   :: ns
    integer,  intent(inout) :: iw, kw
    integer  :: ii, kk
    real(dp) :: rr
    hmax = 0.0_dp; hrms = 0.0_dp; mmax = 0.0_dp; ns = 0
    do kk = kmin, kmax
      do ii = imin, imax
        rr = sqrt(kerr_radius_squared(xg(ii), zg(kk)))
        if (rr < rlo .or. rr >= rhi) cycle
        ns = ns + 1
        if (abs(Hgrid(ii,kk)) > hmax) then
          hmax = abs(Hgrid(ii,kk))
          if (iw /= -1) then
            iw = ii; kw = kk
          end if
        end if
        hrms = hrms + Hgrid(ii,kk)**2
        mmax = max(mmax, sqrt(Mgrid(ii,kk,1)**2 + Mgrid(ii,kk,2)**2 + Mgrid(ii,kk,3)**2))
      end do
    end do
    if (ns > 0) hrms = sqrt(hrms/real(ns, dp))
  end subroutine shell_stats

  !---------------------------------------------------------------------------
  ! Find the apparent horizon in the current state and log/print it.  The area,
  ! J and M_H of a stationary Kerr BH are gauge-invariant, so their drift from
  ! the exact values is a direct, slicing-independent accuracy monitor.
  !---------------------------------------------------------------------------
  subroutine update_horizon(step, tnow)
    integer,  intent(in) :: step
    real(dp), intent(in) :: tnow
    real(dp), parameter :: pi_ = 3.141592653589793238462643383279502884_dp
    real(dp) :: area_ex, j_ex, rejected_hmin, rejected_hmax, rejected_rmin_ks
    real(dp) :: reject_limit

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
      print '(A,I0,A,I0,A,ES9.2,A,ES9.2)', " [horizon] step ", step, &
        ": NOT FOUND (ierr=", hz_ierr, ")  last max|Theta|=", hz%resid, &
        "  best rejected r_KS,min=", hz%rmin_ks
      print '(A,ES12.4,A,ES12.4)', "            marginal probe Theta(in/out)=", &
        hz%theta_in, "/", hz%theta_out
      if (hz%rmin_ks > 0.0_dp) then
        print '(A,ES12.4,A,ES12.4)', "            rejected candidate residual/coverage limit=", &
          hz%rejected_resid, "/", r_excise + 0.5_dp*max(dx,dz)
      end if
      return
    end if

    ! The horizon finder interpolates the evolved metric.  A mathematically
    ! converged surface is not physical if it enters the excision/interpolation
    ! buffer, where the state is ghost-filled rather than evolved.
    reject_limit = r_excise + 0.5_dp*max(dx,dz)
    if (hz%rmin_ks <= reject_limit) then
      rejected_hmin = hz%hmin
      rejected_hmax = hz%hmax
      rejected_rmin_ks = hz%rmin_ks
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
        ": rejected surface inside excision buffer (r_KS,min=", rejected_rmin_ks, &
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
      inv_ref_mirr = hz%m_irr
      inv_ref_area = hz%area
    else
      drift = hz_ref - hz%hmin
      if (drift > hz_drift_tol) then
        print '(A,I0,A,ES10.4,A,ES10.4,A,ES10.4,A,ES10.4,A)', &
          " [horizon] step ", step, ": r_min moved ", hz_ref, " -> ", hz%hmin, &
          "  (d=", drift, ', rate=', drift/max(tnow,1.0e-12_dp), '/t)'
        print '(A)', "   NOTE: coordinate drift of the horizon position is not by"
        print '(A)', "   itself a failure; compare the invariants below."
        hz_drift_tol = 2.0_dp*drift      ! escalate to the next doubling
      end if
      dmirr = abs(hz%m_irr  - inv_ref_mirr )/max(abs(inv_ref_mirr ),1.0e-30_dp)
      darea = abs(hz%area  - inv_ref_area )/max(abs(inv_ref_area ),1.0e-30_dp)
      if (dmirr > inv_tol .or. darea > inv_tol) then
        print '(A,I0,A,ES10.4,A,ES10.4,A,ES10.4,A,ES10.4,A)', &
          " [horizon] step ", step, ": INVARIANT DRIFT  dM_irr/M_irr=", dmirr, &
          "  dA/A=", darea, "  (limit ", inv_tol, ")"
        print '(A)', "   -> the evolved black hole is NOT stationary. The"
        print '(A)', "   spacetime itself is changing, which is a genuine failure."
        if (hz_abort_drift) then
          print '(A,I0,A,I0,A)', " ABORT at step ", step, " (of ", nsteps, &
            " requested)."
          print '(A)', " Set GR_HZ_ABORT=0 to run past this and collect output."
          error stop 'horizon invariants drift: evolution is not stationary'
        end if
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
      print '(A,F7.4,A,F7.4)', "            WARNING: horizon r_KS,min=", hz%rmin_ks, &
        " is within 1.2*r_excise =", 1.2_dp*r_excise
  end subroutine update_horizon

  ! Write a self-describing record next to the output.  This is deliberately
  ! written before the first step so a failed run still says exactly which
  ! source, parameter set, and overrides produced it.
  subroutine write_run_info()
    integer :: u, ios, i
    integer :: v(8)
    character(len=32) :: envnames(16)
    character(len=256) :: envval, line
    character(len=32) :: stamp_date, stamp_time

    call date_and_time(values=v)
    write(stamp_date,'(I4.4,"-",I2.2,"-",I2.2)') v(1), v(2), v(3)
    write(stamp_time,'(I2.2,":",I2.2,":",I2.2)') v(5), v(6), v(7)

    ! Keep a verbatim copy of the parameter file the run actually parsed.
    call execute_command_line('cp "'//trim(par_file)//'" output/run_params.par 2>/dev/null')

    open(newunit=u, file='output/run_info.txt', status='replace', action='write', iostat=ios)
    if (ios /= 0) return
    write(u,'(A)') '# BSSN run metadata'
    write(u,'(A)') 'start_date  = '//trim(stamp_date)
    write(u,'(A)') 'start_time  = '//trim(stamp_time)
    write(u,'(A)') 'parameter_file = '//trim(par_file)
    write(u,'(A)') 'parameter_copy = output/run_params.par'
    write(u,'(A)') ''

    call shell_first_line('git rev-parse HEAD 2>/dev/null', 'git_hash', line)
    write(u,'(A)') 'git_hash    = '//trim(line)
    call shell_first_line('gfortran --version 2>/dev/null', 'compiler', line)
    write(u,'(A)') 'compiler    = '//trim(line)
    call shell_first_line('h5fc -show 2>/dev/null', 'hdf5_flags', line)
    write(u,'(A)') 'hdf5_flags  = '//trim(line)

    write(u,'(A)') ''
    write(u,'(A)') '[grid]'
    write(u,'(A,I0)') 'Nx = ', Nx
    write(u,'(A,I0)') 'Nz = ', Nz
    write(u,'(A,ES24.16)') 'xmax = ', xmax
    write(u,'(A,ES24.16)') 'zmax = ', zmax
    write(u,'(A,ES24.16)') 'dx   = ', dx
    write(u,'(A,ES24.16)') 'dz   = ', dz
    write(u,'(A)') ''
    write(u,'(A)') '[physics]'
    write(u,'(A,ES24.16)') 'bh_mass = ', bh_mass
    write(u,'(A,ES24.16)') 'bh_spin = ', bh_spin
    write(u,'(A,ES24.16)') 'r_excise = ', r_excise
    write(u,'(A,I0)') 'initial_data_mode = ', initial_data_mode
    write(u,'(A,I0)') 'slicing_mode = ', slicing_mode
    write(u,'(A,I0)') 'gamma_driver_mode = ', gamma_driver_mode
    write(u,'(A,ES24.16)') 'gauge_eta = ', gauge_eta
    write(u,'(A,I0)') 'excision_order = ', excision_order
    write(u,'(A,L1)') 'excision_sat = ', excision_sat
    write(u,'(A,ES24.16)') 'ko_eps = ', ko_eps
    write(u,'(A,ES24.16)') 'cfl = ', cfl
    write(u,'(A,I0)') 'nsteps = ', nsteps
    write(u,'(A,I0)') 'out_every = ', out_every
    write(u,'(A,I0)') 'hz_every = ', hz_every
    write(u,'(A,ES24.16)') 'start_t = ', 0.0_dp

    write(u,'(A)') ''
    write(u,'(A)') '[environment overrides]'
    envnames = [character(len=32) :: 'GR_NX', 'GR_NZ', 'GR_XMAX', 'GR_ZMAX', &
      'GR_INITIAL_DATA_MODE', 'GR_SLICING_MODE', 'GR_GAMMA_DRIVER_MODE', &
      'GR_R_EXCISE', 'GR_KO_EPS', 'GR_CFL', 'GR_NSTEPS', 'GR_OUT_EVERY', &
      'GR_HORIZON_EVERY', 'GR_HZ_ABORT', 'GR_WRITE_HDF5', 'GR_REFRESH_BG']
    do i = 1, size(envnames)
      call get_environment_variable(trim(envnames(i)), envval)
      if (len_trim(envval) > 0) write(u,'(A,A,A)') trim(envnames(i)), ' = ', trim(envval)
    end do
    close(u)
  end subroutine write_run_info

  ! Capture the first line of a shell command without requiring a temp file in
  ! the run directory naming scheme.  Failures return an empty string.
  subroutine shell_first_line(command, label, value)
    character(len=*), intent(in)  :: command, label
    character(len=*), intent(out) :: value
    integer :: u, ios
    character(len=512) :: tmp
    value = ''
    tmp = 'output/.run_'//trim(label)
    call execute_command_line(trim(command)//' > "'//trim(tmp)//'" 2>/dev/null')
    open(newunit=u, file=trim(tmp), status='old', action='read', iostat=ios)
    if (ios == 0) then
      read(u,'(A)', iostat=ios) value
      close(u, status='delete')
    end if
  end subroutine shell_first_line

end program main
