! Report the semidiscrete BSSN residual of exact Kerr-Schild data with the
! Kerr-Schild lapse and shift frozen. This separates spatial/RHS truncation
! from gauge evolution and is intentionally diagnostic, not an acceptance
! test: residuals should be nonzero at finite resolution.
program diagnose_stationary_ks_rhs
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, r_excise, bh_spin, slicing_mode, gamma_driver_mode, init_params
  use grid_mod, only: setup_grid, xg, zg
  use vars_mod, only: NVARS, alloc_state
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections
  use excision_mod, only: set_excision_background, extrapolate_excised
  use deriv_stencils_mod, only: init_deriv_stencils
  use bssn_rhs_mod, only: compute_rhs_interior
  implicit none
  real(dp), allocatable :: u(:,:,:), rhs(:,:,:)
  real(dp) :: rho2, a2, rks2, rks, near_max(NVARS), bulk_max(NVARS)
  integer :: i, k, v, inear(NVARS), knear(NVARS), ibulk(NVARS), kbulk(NVARS)

  call init_params('parameter_files/vacuum_kerr.par')
  slicing_mode = 2
  gamma_driver_mode = 2
  call setup_grid()
  call alloc_state(u, Nx, Nz)
  allocate(rhs(Nx,Nz,NVARS))
  call set_kerr_schild_id(u)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call extrapolate_excised(u)
  call init_deriv_stencils()
  rhs = 0.0_dp
  call compute_rhs_interior(u, rhs)

  near_max = 0.0_dp; bulk_max = 0.0_dp
  inear = 0; knear = 0; ibulk = 0; kbulk = 0
  a2 = bh_spin*bh_spin
  do k = 2, Nz-1
    do i = 1, Nx-1
      rho2 = xg(i)*xg(i) + zg(k)*zg(k)
      rks2 = 0.5_dp*(rho2-a2 + sqrt((rho2-a2)*(rho2-a2) + &
                                    4.0_dp*a2*zg(k)*zg(k)))
      rks = sqrt(max(0.0_dp,rks2))
      if (rks < r_excise + 2.0_dp*max(abs(xg(2)-xg(1)),abs(zg(2)-zg(1)))) cycle
      if (rks > 4.0_dp) cycle
      do v = 1, NVARS
        if (rks < 2.3_dp) then
          if (abs(rhs(i,k,v)) > near_max(v)) then
            near_max(v) = abs(rhs(i,k,v))
            inear(v) = i; knear(v) = k
          end if
        else if (abs(rhs(i,k,v)) > bulk_max(v)) then
          bulk_max(v) = abs(rhs(i,k,v))
          ibulk(v) = i; kbulk(v) = k
        end if
      end do
    end do
  end do

  write(*,'(A,I0,A,I0,A,ES10.3)') 'Frozen Kerr-Schild RHS in inner exterior: Nx=', &
       Nx, ' Nz=', Nz, ' dx=', abs(xg(2)-xg(1))
  write(*,'(A)') ' idx    near max|RHS|   at (x,z)       bulk max|RHS|   at (x,z)'
  do v = 1, NVARS
    write(*,'(I4,2(2X,ES12.4,2X,2F9.4))') v, near_max(v), &
         xg(max(1,inear(v))), zg(max(1,knear(v))), bulk_max(v), &
         xg(max(1,ibulk(v))), zg(max(1,kbulk(v)))
  end do
  if (any(rhs /= rhs)) error stop 'non-finite frozen Kerr-Schild RHS'
end program diagnose_stationary_ks_rhs
