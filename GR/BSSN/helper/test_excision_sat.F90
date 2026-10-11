program test_excision_sat
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, init_params, r_excise, bh_spin, bh_mass
  use grid_mod, only: setup_grid, xg, zg
  use vars_mod, only: NVARS, IALPHA, IK, IGXX, IGYY, IGZZ
  use excision_mod, only: set_excision_background, extrapolate_excised
  use excision_sat_mod, only: apply_characteristic_sat, apply_legacy_excision_sat, &
                              exact_excision_characteristics, apply_kerr_excision_sat
  implicit none
  real(dp) :: state(3), target(3), speed(3), rhs(3)
  real(dp), allocatable :: u(:,:,:), legacy_rhs(:,:,:)
  integer :: i, k

  ! 1. Direct characteristic penalty primitive
  state = 1.0_dp; target = 0.0_dp; speed = [-1.0_dp, 1.0_dp, 0.0_dp]
  rhs = 0.0_dp
  call apply_characteristic_sat(state, target, speed, 1.0_dp, 0.5_dp, rhs)
  if (abs(rhs(1)-2.0_dp) > 1.0e-14_dp) error stop 'incoming SAT mismatch'
  if (maxval(abs(rhs(2:3))) > 1.0e-14_dp) error stop 'outgoing SAT was applied'

  ! 2. Check that exact characteristic flag is now enabled
  if (.not. exact_excision_characteristics) error stop 'characteristic SAT unexpectedly disabled'

  ! 3. Check inert behavior when enabled = .false.
  call init_params('missing-excision-sat.par')
  call setup_grid()
  allocate(u(Nx,Nz,NVARS), legacy_rhs(Nx,Nz,NVARS))
  u = 1.0_dp; legacy_rhs = 0.0_dp
  call apply_legacy_excision_sat(u, legacy_rhs, dx, .false.)
  if (maxval(abs(legacy_rhs)) > 1.0e-14_dp) error stop 'legacy SAT is not inert when enabled=.false.'

  ! 4. Active behaviour when enabled = .true.
  !
  !    Which variable to perturb follows from the published characteristic
  !    fields (Brown et al. PRD 85 084004, Sec. III.C):
  !
  !      * the PROPAGATING gauge mode is  G^{(alpha,+-)} = K -+ alpha_n/(alpha sqrt f)
  !        -- it is built from K and the lapse GRADIENT, not from the lapse;
  !      * the lapse alpha itself belongs to the ZERO-speed sector together with
  !        beta_n, beta_A, alpha_A, beta_AB and beta_An.
  !
  !    So a spatially uniform lapse offset is a gauge (zero-speed) mode and must
  !    NOT be damped by a penalty on the incoming +-speed families, whereas a
  !    perturbation of K is propagating and must be restored.  Both are checked
  !    below; asserting a restoring penalty on a uniform lapse offset (as an
  !    earlier version of this test did) requires manufacturing a propagating
  !    mode that does not exist in the published system.
  u = 0.0_dp
  u(:,:,IGXX) = 1.0_dp; u(:,:,IGYY) = 1.0_dp; u(:,:,IGZZ) = 1.0_dp
  u(:,:,IALPHA) = 1.0_dp
  call set_excision_background(u)
  call extrapolate_excised(u)

  ! -- 4a. uniform lapse offset: zero-speed mode, must NOT be damped
  u(:,:,IALPHA) = 1.1_dp
  legacy_rhs = 0.0_dp
  call apply_legacy_excision_sat(u, legacy_rhs, dx, .true.)
  if (minval(legacy_rhs(:,:,IALPHA)) < -1.0e-14_dp) then
    error stop 'excision SAT damped a zero-speed (gauge) lapse offset'
  end if
  write(*,'(A,ES12.4)') 'excision SAT: zero-speed lapse offset not damped, min d(alpha)/dt = ', &
    minval(legacy_rhs(:,:,IALPHA))

  ! -- 4b. K perturbation: propagating mode, must be restored
  u(:,:,IALPHA) = 1.0_dp
  u(:,:,IK)     = -0.1_dp
  legacy_rhs = 0.0_dp
  call apply_legacy_excision_sat(u, legacy_rhs, dx, .true.)
  if (maxval(legacy_rhs(:,:,IK)) <= 0.0_dp) then
    error stop 'excision SAT did not damp the propagating (K) gauge mode'
  end if
  write(*,'(A,ES12.4)') 'excision SAT: propagating K mode damped, max d(K)/dt = ', &
    maxval(legacy_rhs(:,:,IK))
end program test_excision_sat
