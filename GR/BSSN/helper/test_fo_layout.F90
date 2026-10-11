program test_fo_layout
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, init_params
  use fo_vars_mod, only: NFO_VARS, alloc_fo_state, IDPHI_FO, IDGT_FO, IDALPHA_FO, IDBETA_FO
  use fo_constraints_mod, only: reduction_constraint_norms
  implicit none
  real(dp), allocatable :: u(:,:,:)
  real(dp) :: cphi, cgt, ca, cb
  integer :: i, k, q, a, c

  call init_params('fo_layout_test.par')
  call alloc_fo_state(u, Nx, Nz)
  do k = 1, Nz
    do i = 1, Nx
      u(i,k,1) = 1.0_dp + 2.0_dp*real(i-1,dp)*dx + 3.0_dp*real(k-1,dp)*dz
      u(i,k,18) = 2.0_dp + real(i-1,dp)*dx - 2.0_dp*real(k-1,dp)*dz
      do q = 1, 6
        u(i,k,q+1) = real(q,dp) + 0.5_dp*real(i-1,dp)*dx - 0.25_dp*real(k-1,dp)*dz
      end do
      do a = 1, 3
        c = 18 + a
        u(i,k,c) = real(a,dp) + real(i-1,dp)*dx + 0.75_dp*real(k-1,dp)*dz
      end do
      u(i,k,IDPHI_FO(1)) = 2.0_dp
      u(i,k,IDPHI_FO(3)) = 3.0_dp
      do q = 1, 6
        u(i,k,IDGT_FO(1,q)) = 0.5_dp
        u(i,k,IDGT_FO(3,q)) = -0.25_dp
      end do
      u(i,k,IDALPHA_FO(1)) = 1.0_dp
      u(i,k,IDALPHA_FO(3)) = -2.0_dp
      do a = 1, 3
        u(i,k,IDBETA_FO(1,a)) = 1.0_dp
        u(i,k,IDBETA_FO(3,a)) = 0.75_dp
      end do
    end do
  end do

  call reduction_constraint_norms(u, cphi, cgt, ca, cb)
  write(*,'(A,I0,A,4(ES12.4,1X))') 'FO layout ', NFO_VARS, ' constraints: ', cphi, cgt, ca, cb
  if (max(cphi,cgt,ca,cb) > 1.0e-11_dp) error stop 'first-order reduction constraint test failed'
end program test_fo_layout
