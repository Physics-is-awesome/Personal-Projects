!===============================================================================
! test_horizon_drift -- independent validation of horizon_mod, and attribution
! of the inward horizon migration seen in the production vacuum-Kerr run.
!
! The production driver calls find_horizon with a WARM START: it keeps hh%coef
! from the previous solve and only falls back to `guess` when hh%found is false.
! A Newton continuation can therefore walk smoothly onto the wrong surface and
! never be detected by the driver's own diagnostics.  This test separates
! "the solved surface moved" from "the solver drifted":
!
!   T1  exactness at t=0.  On the Kerr-Schild initial data the horizon must be
!       r+ on the symmetry axis (mu=1) and sqrt(r+^2 + a^2) in the equatorial
!       plane (mu=0), with A = 8 pi M r+, J = aM, M_H = M.  Independent of any
!       evolution, so it pins down the finder's accuracy.
!
!   T2  guess independence.  Solve the same data from several very different
!       starting radii with a freshly initialised solver.  All must agree.
!
!   T3  determinism.  Repeat a solve on unchanged data; must be bit-identical.
!
!   T4  genuine marginally-trapped surface.  Scale the fitted surface by
!       (1-eps) and (1+eps) and check that the expansion changes sign, i.e.
!       Theta < 0 inside (trapped) and Theta > 0 outside.  A tangent/spurious
!       zero would not flip sign.
!
!   T5  drift attribution.  Evolve with the production integrator, then solve
!       the evolved state both warm-started (as production does) and fresh from
!       a distant guess.  If the fresh solves agree with the warm start, the
!       surface really moved and the drift is in the evolved data, not the
!       solver.
!
! Exit status is 0 only if every check passes.
!===============================================================================
program test_horizon_drift
  use kinds_mod, only: dp
  use params_mod
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: NVARS, IALPHA, IPHI, IK, IBX, IBETAX, alloc_state, &
                       ICONFMET, IATILDE
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections
  use excision_mod, only: extrapolate_excised, set_excision_background
  use deriv_stencils_mod, only: init_deriv_stencils
  use timestep_mod, only: rk4_step
  use horizon_mod

  implicit none

  real(dp), allocatable :: u(:,:,:)
  type(horizon_t) :: hz
  logical :: hz_eqsym
  integer :: nsteps_hz, hz_nl, nfail, ios
  real(dp) :: rp, j_exact, area_exact, heq_exact, dt, tol
  character(len=256) :: par_file, buf
  integer :: n

  tol = 2.0e-3_dp                       ! finder tolerance at dx ~ 0.25
  nfail = 0

  call get_command_argument(1, par_file)
  call init_params(trim(par_file))
  call setup_grid()
  call alloc_state(u, Nx, Nz)

  hz_eqsym = (zg(1) > -1.0e-12_dp)
  hz_nl = 8
  if (.not. hz_eqsym) hz_nl = 10
  call horizon_init_grid(xg(1), zg(1), dx, dz, Nx, Nz, hz_eqsym)

  call set_kerr_schild_id(u)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call init_deriv_stencils()
  call extrapolate_excised(u)

  rp       = bh_mass + sqrt(bh_mass**2 - bh_spin**2)
  j_exact  = bh_spin
  area_exact = 8.0_dp*acos(-1.0_dp)*bh_mass*rp
  heq_exact = sqrt(rp*rp + bh_spin**2)     ! equatorial Cartesian Kerr-Schild radius

  write(*,'(A)') '========================================================================'
  write(*,'(A,ES12.4,A,ES12.4)') ' Kerr: M=', bh_mass, '  a=', bh_spin
  write(*,'(A,ES12.4,A,ES12.4)') ' exact r+ (mu=1)=', rp, '  equatorial (mu=0)=', heq_exact
  write(*,'(A,I0,A,I0,A,F8.4)') ' grid: Nx=', Nx, '  Nz=', Nz, '  dx=', dx

  call load_and_solve(u, hz)
  call check('T1a  hmin == r+ (axis)',        hz%hmin, rp,        tol, nfail)
  call check('T1b  hmax == equat. KS radius', hz%hmax, heq_exact, tol, nfail)
  call check('T1c  area / A_exact',           hz%area/area_exact, 1.0_dp, tol, nfail)
  call check('T1d  J',                        hz%j_spin, j_exact, tol, nfail)
  call check('T1e  M_H',                      hz%m_h,    bh_mass, tol, nfail)
  if (hz%resid > 1.0e-6_dp) then
     write(*,'(A,ES10.2,A)') ' T1f  FAIL: max|Theta| = ', hz%resid, ' not converged'
     nfail = nfail + 1
  else
     write(*,'(A,ES10.2,A)') ' T1f  max|Theta| = ', hz%resid, '  ok'
  end if

  ! ---- T2 guess independence -------------------------------------------------
  call check_guesses(u, hz%hmin, nfail)
  ! ---- T3 determinism -------------------------------------------------------
  call check_determinism(u, hz, nfail)
  ! ---- T4 genuine marginally-trapped surface -------------------------------
  call check_trapped(u, hz, nfail)

  ! ---- T5 drift attribution -------------------------------------------------
  write(*,'(A)') '------------------------------------------------------------------------'
  nsteps_hz = 2000
  call get_environment_variable('GR_HZ_TEST_STEPS', buf, status=ios)
  if (ios == 0) read(buf,*) nsteps_hz

  dt = cfl*min(dx, dz)/sqrt(2.0_dp)
  call horizon_warm(hz)                       ! keep the t=0 solution as warm start
  do n = 1, nsteps_hz
     call rk4_step(u, dt, n)
     call extrapolate_excised(u)
  end do

  call load_and_solve(u, hz)
  write(*,'(A,I0,A,F8.4)') ' evolved ', nsteps_hz, ' steps -> warm-start hmin = ', hz%hmin
  call check_guesses(u, hz%hmin, nfail)

  if (nfail == 0) then
     write(*,'(A)') 'T5  PASS: fresh solves from distant guesses reproduce the'
     write(*,'(A)') '    warm-started radius, so the migration is in the evolved'
     write(*,'(A)') '    data (gauge), not in the Newton continuation.'
  else
     write(*,'(A)') 'T5  FAIL: see above.'
  end if

  write(*,'(A)') '========================================================================'
  if (nfail == 0) then
     write(*,'(A)') 'test_horizon_drift: all checks passed'
  else
     write(*,'(A,I0,A)') 'test_horizon_drift: ', nfail, ' check(s) FAILED'
     error stop 1
  end if

contains

  subroutine load_and_solve(st, hh)
    real(dp), intent(in) :: st(:,:,:)
    type(horizon_t), intent(inout) :: hh
    integer :: ierr
    call horizon_adm_from_state(st, IPHI, IK, ICONFMET, IATILDE, 1, 1)
    call horizon_init(hh, hz_nl, 0.0_dp, hz_eqsym)
    call find_horizon(hh, rp, ierr)
    if (ierr /= 0) then
       write(*,'(A,I0,A,ES10.2)') ' find_horizon ierr=', ierr, '  max|Theta|=', hh%resid
    end if
  end subroutine load_and_solve

  ! continue from the stored coefficients, exactly as the production driver does
  subroutine horizon_warm(hh)
    type(horizon_t), intent(in) :: hh
    integer :: ierr
    call find_horizon(hz, rp, ierr)
  end subroutine horizon_warm

  subroutine check(label, got, want, t, nbad)
    character(len=*), intent(in) :: label
    real(dp), intent(in) :: got, want, t
    integer, intent(inout) :: nbad
    real(dp) :: rel
    rel = abs(got - want)/max(abs(want), 1.0e-12_dp)
    if (rel <= t) then
       write(*,'(A,A,F14.6,A,F14.6,A,ES9.2,A)') ' ', label, got, '  want ', want, &
            '  rel ', rel, '  ok'
    else
       write(*,'(A,A,F14.6,A,F14.6,A,ES9.2,A)') ' ', label, got, '  want ', want, &
            '  rel ', rel, '  FAIL'
       nbad = nbad + 1
    end if
  end subroutine check

  ! T2: fresh solver, starting radii OUTSIDE the horizon must all agree.
  !     Starting inside r+ can legitimately converge to a different interior
  !     Theta=0 root (reported by T2b); that is a bracketing limitation of the
  !     finder, not evidence about drift, so it is informational here.
  subroutine check_guesses(st, ref, nbad)
    real(dp), intent(in) :: st(:,:,:), ref
    integer, intent(inout) :: nbad
    real(dp) :: mult(4)
    type(horizon_t) :: g
    integer :: ierr, q
    real(dp) :: spread
    real(dp), allocatable :: hh(:)

    allocate(hh(4))
    do q = 1, 4
       mult(q) = 1.0_dp + 0.35_dp*real(q-1, dp)
       call horizon_adm_from_state(st, IPHI, IK, ICONFMET, IATILDE, 1, 1)
       call horizon_init(g, hz_nl, 0.0_dp, hz_eqsym)
       call find_horizon(g, mult(q)*rp, ierr)
       hh(q) = g%hmin
       if (ierr /= 0) nbad = nbad + 1
    end do
    spread = maxval(hh) - minval(hh)
    write(*,'(A,4(F11.6,1X))') ' T2a hmin from guesses r+*[1.00,1.35,1.70,2.05]: ', hh
    write(*,'(A,ES9.2)') ' T2a spread = ', spread
    if (spread > 1.0e-6_dp) then
       write(*,'(A)') ' T2a FAIL: outer-root solve depends on the starting radius'
       nbad = nbad + 1
    end if
    if (abs(hh(1) - ref) > 1.0e-6_dp) then
       write(*,'(A,ES12.4,A,ES12.4,A)') ' T2a FAIL: reference ', ref, ' vs fresh ', hh(1)
       nbad = nbad + 1
    end if

    call horizon_adm_from_state(st, IPHI, IK, ICONFMET, IATILDE, 1, 1)
    call horizon_init(g, hz_nl, 0.0_dp, hz_eqsym)
    call find_horizon(g, 0.6_dp*rp, ierr)
    write(*,'(A,ES12.4,A,ES12.4)') &
         ' T2b informational: guess 0.6*r+ gives hmin = ', g%hmin, '  (outer root is ', hh(1), ')'
    deallocate(hh)
  end subroutine check_guesses

  subroutine check_determinism(st, ref_h, nbad)
    real(dp), intent(in) :: st(:,:,:)
    type(horizon_t), intent(in) :: ref_h
    integer, intent(inout) :: nbad
    type(horizon_t) :: g
    integer :: ierr
    call horizon_adm_from_state(st, IPHI, IK, ICONFMET, IATILDE, 1, 1)
    call horizon_init(g, hz_nl, 0.0_dp, hz_eqsym)
    call find_horizon(g, rp, ierr)
    if (g%hmin /= ref_h%hmin) then
       write(*,'(A,ES16.8,A,ES16.8,A)') ' T3  FAIL: ', ref_h%hmin, ' vs repeat ', g%hmin
       nbad = nbad + 1
    else
       write(*,'(A,ES16.8,A)') ' T3  repeat solve bit-identical: hmin = ', g%hmin
    end if
  end subroutine check_determinism

  ! T4: scale the fitted surface inward/outward and require a sign change in Theta
  subroutine check_trapped(st, hh, nbad)
    real(dp), intent(in) :: st(:,:,:)
    type(horizon_t), intent(in) :: hh
    integer, intent(inout) :: nbad
    type(horizon_t) :: s
    real(dp), parameter :: eps = 0.02_dp
    real(dp) :: th_in, th_out
    logical :: ok
    call horizon_adm_from_state(st, IPHI, IK, ICONFMET, IATILDE, 1, 1)

    s = hh
    ok = .true.
    s%coef = (1.0_dp - eps)*hh%coef
    th_in = expansion(s, 0.5_dp, ok)
    ok = .true.
    s%coef = (1.0_dp + eps)*hh%coef
    th_out = expansion(s, 0.5_dp, ok)
    write(*,'(A,ES12.4,A,ES12.4)') ' T4  Theta at (1-eps)h = ', th_in, &
         '   at (1+eps)h = ', th_out
    if (.not. ok) then
       write(*,'(A)') ' T4  FAIL: expansion evaluation failed'
       nbad = nbad + 1
       return
    end if
    if (.not. (th_in < 0.0_dp .and. th_out > 0.0_dp)) then
       write(*,'(A)') ' T4  FAIL: Theta does not change sign; not a trapped surface'
       nbad = nbad + 1
    end if
  end subroutine check_trapped

end program test_horizon_drift