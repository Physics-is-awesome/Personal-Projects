program test_fo_rhs
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, init_params, r_excise
  use grid_mod, only: setup_grid
  use vars_mod, only: NVARS
  use fo_vars_mod, only: NFO_VARS, alloc_fo_state, IDPHI_FO, IDGT_FO, IDALPHA_FO, IDBETA_FO
  use fo_rhs_mod, only: pack_fo_state, compute_fo_rhs
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections
  use excision_mod, only: set_excision_background, extrapolate_excised
  use deriv_stencils_mod, only: init_deriv_stencils
  use bssn_rhs_mod, only: compute_rhs_interior
  implicit none
  real(dp), allocatable :: u(:,:,:), q(:,:,:), r(:,:,:), rq(:,:,:)
  real(dp) :: err, expected
  integer :: i, k, v, a

  call init_params('missing-fo-rhs.par')
  r_excise = 0.0_dp
  call setup_grid()
  allocate(u(Nx,Nz,NVARS))
  call set_kerr_schild_id(u)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call extrapolate_excised(u)
  call init_deriv_stencils()
  call alloc_fo_state(q, Nx, Nz)
  call pack_fo_state(u, q)
  allocate(r(Nx,Nz,NVARS), rq(Nx,Nz,NFO_VARS))
  r = 0.0_dp
  call compute_rhs_interior(u, r)
  call compute_fo_rhs(q, rq)

  ! The reduction fields must be the differentiated legacy RHS (to roundoff
  ! at the same second-order derivative used by the wrapper), even though the
  ! Kerr-Schild RHS itself is only stationary up to truncation error.
  err = 0.0_dp
  do k = 2, Nz-1
    do i = 2, Nx-1
      expected = (r(i+1,k,1)-r(i-1,k,1))/(2.0_dp*dx)
      err = max(err, abs(rq(i,k,IDPHI_FO(1))-expected))
      expected = (r(i,k+1,1)-r(i,k-1,1))/(2.0_dp*dz)
      err = max(err, abs(rq(i,k,IDPHI_FO(3))-expected))
      do v = 1, 6
        expected = (r(i+1,k,v+1)-r(i-1,k,v+1))/(2.0_dp*dx)
        err = max(err, abs(rq(i,k,IDGT_FO(1,v))-expected))
        expected = (r(i,k+1,v+1)-r(i,k-1,v+1))/(2.0_dp*dz)
        err = max(err, abs(rq(i,k,IDGT_FO(3,v))-expected))
      end do
      expected = (r(i+1,k,18)-r(i-1,k,18))/(2.0_dp*dx)
      err = max(err, abs(rq(i,k,IDALPHA_FO(1))-expected))
      expected = (r(i,k+1,18)-r(i,k-1,18))/(2.0_dp*dz)
      err = max(err, abs(rq(i,k,IDALPHA_FO(3))-expected))
      do a = 1, 3
        expected = (r(i+1,k,18+a)-r(i-1,k,18+a))/(2.0_dp*dx)
        err = max(err, abs(rq(i,k,IDBETA_FO(1,a))-expected))
        expected = (r(i,k+1,18+a)-r(i,k-1,18+a))/(2.0_dp*dz)
        err = max(err, abs(rq(i,k,IDBETA_FO(3,a))-expected))
      end do
    end do
  end do
  if (err > 1.0e-12_dp) error stop 'FO reduction RHS is not derivative-consistent'
  if (any(r /= r) .or. any(rq /= rq)) error stop 'non-finite Kerr-Schild RHS'
  write(*,'(A,ES12.4,A,ES12.4)') 'stationary Kerr-Schild FO reduction max error = ', err, &
    ' legacy RHS max = ', maxval(abs(r))
end program test_fo_rhs
