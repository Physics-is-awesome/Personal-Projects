! Probe the reduced excision SAT on a route-C equilibrium state.
! This never advances the solution and does not enable SAT in production.
program diagnose_route_c_sat
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, r_excise, bh_mass, bh_spin, init_params
  use grid_mod, only: setup_grid
  use vars_mod, only: NVARS, alloc_state
  use initial_data_mod, only: compute_conformal_connections
  use eq_kerr_bssn_data_mod, only: set_eq_kerr_trumpet_id
  use excision_mod, only: set_excision_background, extrapolate_excised
  use deriv_stencils_mod, only: init_deriv_stencils
  use bssn_rhs_mod, only: compute_rhs_interior
  use excision_sat_mod, only: apply_legacy_excision_sat, exact_excision_characteristics
  implicit none

  real(dp), allocatable :: u(:,:,:), rhs(:,:,:), sat(:,:,:)
  character(len=256) :: par_file
  real(dp) :: r0
  integer :: i, k
  real(dp) :: sat_max, sat_rms, all_max, all_rms, sum_sat, sum_all
  integer :: n

  r0 = bh_mass
  par_file = 'parameter_files/vacuum_kerr.par'
  call get_command_argument(1, par_file)
  call init_params(trim(par_file))
  call setup_grid()
  call alloc_state(u, Nx, Nz)
  allocate(rhs(Nx,Nz,NVARS), sat(Nx,Nz,NVARS))

  call set_eq_kerr_trumpet_id(u, bh_mass, bh_spin, r0)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call init_deriv_stencils()
  call extrapolate_excised(u)

  rhs = 0.0_dp
  call compute_rhs_interior(u, rhs)
  sat = 0.0_dp
  call apply_legacy_excision_sat(u, sat, dx, .true.)

  sat_max = 0.0_dp; sat_rms = 0.0_dp; sum_sat = 0.0_dp
  all_max = 0.0_dp; all_rms = 0.0_dp; sum_all = 0.0_dp; n = 0
  do k = 1, Nz
    do i = 1, Nx
      if (maxval(abs(sat(i,k,:))) > 0.0_dp) then
        sat_max = max(sat_max, maxval(abs(sat(i,k,:))))
        sum_sat = sum_sat + maxval(abs(sat(i,k,:)))**2
      end if
      all_max = max(all_max, maxval(abs(rhs(i,k,:)+sat(i,k,:))))
      sum_all = sum_all + maxval(abs(rhs(i,k,:)+sat(i,k,:)))**2
      n = n + 1
    end do
  end do
  sat_rms = sqrt(sum_sat/real(max(1,n),dp))
  all_rms = sqrt(sum_all/real(max(1,n),dp))

  write(*,'(A,F7.3,A,F7.3,A,F7.3)') 'route-C SAT probe: spin=', bh_spin, &
    ' r_excise=', r_excise, ' R0=', r0
  write(*,'(A,L1)') 'exact characteristic map advertised: ', exact_excision_characteristics
  write(*,'(A,2(1X,ES14.6))') 'SAT contribution max/RMS: ', sat_max, sat_rms
  write(*,'(A,2(1X,ES14.6))') 'combined RHS max/RMS:     ', all_max, all_rms
  if (any(u /= u) .or. any(sat /= sat)) error stop 'route-C SAT probe: non-finite result'
end program diagnose_route_c_sat
