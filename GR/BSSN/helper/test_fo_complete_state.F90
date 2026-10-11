program test_fo_complete_state
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, init_params
  use grid_mod, only: setup_grid
  use vars_mod, only: NVARS, IPHI, IBETAX, IBETAY
  use fo_complete_vars_mod
  implicit none
  real(dp), allocatable :: u(:,:,:), q(:,:,:), roundtrip(:,:,:)
  real(dp) :: norms(3), x
  integer :: i, k, st

  call init_params('missing-complete-state-test.par')
  call setup_grid()
  call alloc_fo_complete_state(q, Nx, Nz)
  allocate(u(Nx,Nz,NVARS), roundtrip(Nx,Nz,NVARS))
  u = 0.0_dp
  do k = 1, Nz
    do i = 1, Nx
      x = real(i-1,dp)*dx
      u(i,k,IPHI) = 2.0_dp*x - 0.5_dp*real(k-1,dp)*dz
      u(i,k,IBETAX) = 1.0_dp
    end do
  end do
  call pack_fo_complete_state(u, q, st)
  if (st /= FO_STATUS_OK) error stop 'Cartoon y derivative unexpectedly unsupported'
  call unpack_fo_complete_state(q, roundtrip)
  if (maxval(abs(roundtrip-u)) > 1.0e-13_dp) error stop 'complete FO unpack failed'
  if (abs(q(3,3,fo_complete_index(IPHI,1))-2.0_dp) > 1.0e-12_dp) &
    error stop 'x derivative mismatch'
  if (abs(q(3,3,fo_complete_index(IPHI,3))+0.5_dp) > 1.0e-12_dp) &
    error stop 'z derivative mismatch'
  if (abs(q(3,3,fo_complete_index(IBETAY,2))-1.0_dp/(sqrt(5.0_dp)*dx)) > 2.0e-2_dp) &
    error stop 'Cartoon y derivative mismatch'
  call complete_reduction_constraint_norms(q, norms, st)
  if (st /= FO_STATUS_OK .or. maxval(norms) > 1.0e-13_dp) &
    error stop 'complete FO reduction diagnostic failed'
  write(*,'(A,3(1X,ES10.3))') 'complete FO reduction norms:', norms
end program test_fo_complete_state
