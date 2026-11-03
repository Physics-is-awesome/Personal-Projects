program test_excision_sat
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, init_params, r_excise, bh_spin, bh_mass
  use grid_mod, only: setup_grid, xg, zg
  use vars_mod, only: NVARS, IALPHA, IK, IGXX, IGYY, IGZZ
  use excision_mod, only: set_excision_background, extrapolate_excised
  use excision_sat_mod, only: apply_characteristic_sat, apply_legacy_excision_sat, &
                              exact_excision_characteristics, apply_kerr_excision_sat
  use fo_normal_system_mod, only: fo_normal_block_eigensystem, FO_BLOCK_LAPSE, &
                                  FO_BLOCK_GRAVITY, FO_BLOCK_SHIFT, FO_MAX_BLOCK
  implicit none
  real(dp) :: state(3), target(3), speed(3), rhs(3)
  real(dp) :: amat(FO_MAX_BLOCK,FO_MAX_BLOCK), lambda(FO_MAX_BLOCK)
  real(dp) :: rmat(FO_MAX_BLOCK,FO_MAX_BLOCK), lmat(FO_MAX_BLOCK,FO_MAX_BLOCK)
  real(dp) :: modal_rhs(FO_MAX_BLOCK), modal_target(FO_MAX_BLOCK), modal_zero(FO_MAX_BLOCK)
  character(len=32) :: labels(FO_MAX_BLOCK)
  integer :: n_modes, block, mode
  real(dp), allocatable :: u(:,:,:), legacy_rhs(:,:,:)
  integer :: i, k

  ! 1. Direct characteristic penalty primitive
  state = 1.0_dp; target = 0.0_dp; speed = [-1.0_dp, 1.0_dp, 0.0_dp]
  rhs = 0.0_dp
  call apply_characteristic_sat(state, target, speed, 1.0_dp, 0.5_dp, rhs)
  if (abs(rhs(1)) > 1.0e-14_dp) error stop 'outgoing SAT was applied'
  if (abs(rhs(2)+2.0_dp) > 1.0e-14_dp) error stop 'incoming SAT mismatch'
  if (abs(rhs(3)) > 1.0e-14_dp) error stop 'zero-speed SAT was applied'

  ! 1b. Exercise every reduced normal block with manufactured modal data.
  !     This checks the sign convention and the R/L transforms independently
  !     of the production second-order RHS.
  do block = FO_BLOCK_LAPSE, FO_BLOCK_SHIFT
    call fo_normal_block_eigensystem(block, 1.2_dp, 0.15_dp, 0.9_dp, 0.1_dp, &
                                     n_modes, amat, lambda, rmat, lmat, labels)
    if (maxval(abs(matmul(lmat(1:n_modes,1:n_modes), &
                          rmat(1:n_modes,1:n_modes)) - &
                identity_matrix(n_modes))) > 1.0e-14_dp) then
      error stop 'normal block characteristic transforms are not inverse'
    end if
    do mode = 1, n_modes
      modal_rhs = 0.0_dp
      modal_zero = 0.0_dp
      modal_target = 0.0_dp
      modal_target(mode) = 1.0_dp
      call apply_characteristic_sat(modal_target, modal_zero, lambda, &
                                    1.0_dp, 1.0_dp, modal_rhs)
      if (lambda(mode) > 0.0_dp) then
        if (abs(modal_rhs(mode)+lambda(mode)) > 1.0e-14_dp) &
          error stop 'incoming manufactured mode was not damped'
      else
        if (maxval(abs(modal_rhs)) > 1.0e-14_dp) &
          error stop 'outgoing or zero manufactured mode was damped'
      end if
    end do
  end do

  ! 2. The current Kerr penalty is intentionally reduced/experimental.  It
  !    must not be advertised as an exact characteristic treatment.
  if (exact_excision_characteristics) error stop 'reduced SAT incorrectly marked exact'

  ! 3. Check inert behavior when enabled = .false.
  call init_params('missing-excision-sat.par')
  call setup_grid()
  allocate(u(Nx,Nz,NVARS), legacy_rhs(Nx,Nz,NVARS))
  u = 1.0_dp; legacy_rhs = 0.0_dp
  call apply_legacy_excision_sat(u, legacy_rhs, dx, .false.)
  if (maxval(abs(legacy_rhs)) > 1.0e-14_dp) error stop 'legacy SAT is not inert when enabled=.false.'

  ! 4. Smoke-check the opt-in reduced penalty when enabled = .true.; this is
  ! not a validation of the missing full production characteristic map.
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
contains

  function identity_matrix(n) result(a)
    integer, intent(in) :: n
    real(dp) :: a(n,n)
    integer :: j
    a = 0.0_dp
    do j = 1, n
      a(j,j) = 1.0_dp
    end do
  end function identity_matrix

end program test_excision_sat
