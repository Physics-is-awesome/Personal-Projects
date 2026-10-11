program test_fo_complete_rhs
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, init_params
  use grid_mod, only: setup_grid
  use vars_mod, only: NVARS, IPHI, IGXX, IGYY, IGZZ, IALPHA
  use bssn_rhs_mod, only: compute_rhs_interior
  use excision_mod, only: set_excision_background, extrapolate_excised
  use deriv_stencils_mod, only: init_deriv_stencils
  use fo_complete_vars_mod
  use fo_complete_rhs_mod
  implicit none
  real(dp), allocatable :: u(:,:,:), q(:,:,:), rhs(:,:,:), ru(:,:,:)
  real(dp) :: norms(3), x
  integer :: i, k, st

  call init_params('missing-complete-rhs-test.par')
  call setup_grid()
  call init_deriv_stencils()
  allocate(u(Nx,Nz,NVARS), q(Nx,Nz,NFO_COMPLETE_VARS), rhs(Nx,Nz,NFO_COMPLETE_VARS), &
    ru(Nx,Nz,NVARS))
  call set_excision_background(u)
  u = 0.0_dp
  u(:,:,IGXX) = 1.0_dp
  u(:,:,IGYY) = 1.0_dp
  u(:,:,IGZZ) = 1.0_dp
  u(:,:,IALPHA) = 1.0_dp
  do k = 1, Nz
    do i = 1, Nx
      x = real(i-1,dp)*dx
      u(i,k,IPHI) = x*x + 0.1_dp*real(k-1,dp)*dz
    end do
    call extrapolate_excised(u)
  end do
  call pack_fo_complete_state(u, q, st)
  if (st /= FO_STATUS_OK) error stop 'complete pack unexpectedly unsupported'
  call complete_reduction_constraint_norms(q, norms, st)
  if (st /= FO_STATUS_OK .or. maxval(norms) > 1.0e-12_dp) error stop 'reduction consistency failed'
  q(3,3,fo_complete_index(IPHI,1)) = q(3,3,fo_complete_index(IPHI,1)) + 1.0_dp
  call complete_reduction_constraint_norms(q, norms, st)
  if (maxval(norms) <= 0.5_dp) error stop 'reduction defect was not detected'
  call pack_fo_complete_state(u, q, st)

  ru = 0.0_dp
  call compute_rhs_interior(u, ru)
  call compute_complete_state_rhs(q, rhs, st)
  if (st /= FO_STATUS_Y_UNSUPPORTED) error stop 'unsupported y RHS was not reported'
  if (maxval(abs(rhs(:,:,1:NVARS)-ru)) > 1.0e-12_dp) error stop 'base RHS mismatch'
  if (abs(rhs(3,3,fo_complete_index(IPHI,1)) - &
      (ru(4,3,IPHI)-ru(2,3,IPHI))/(2.0_dp*dx)) > 1.0e-12_dp) &
    error stop 'differentiated x RHS mismatch'
  if (maxval(abs(rhs(:,:,fo_complete_index(IPHI,2)))) > 0.0_dp) &
    error stop 'unsupported y RHS was fabricated'
  write(*,'(A,3(1X,ES10.3))') 'complete RHS reduction norms:', norms
end program test_fo_complete_rhs
