! Isolate the semidiscrete RHS contributions of the production operators.
! This is a diagnostic only: it does not advance the state.
program diagnose_boundary_isolation
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, r_excise, init_params
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod, only: NVARS, alloc_state
  use initial_data_mod, only: set_kerr_schild_id, compute_conformal_connections
  use excision_mod, only: set_excision_background, extrapolate_excised, is_excised, &
                          kerr_radius_squared
  use deriv_stencils_mod, only: init_deriv_stencils
  use bssn_rhs_mod, only: compute_rhs_interior
  use dissipation_mod, only: add_ko_dissipation
  use sommerfeld_mod, only: add_sommerfeld_bc
  use excision_sat_mod, only: apply_legacy_excision_sat
  implicit none

  real(dp), allocatable :: u(:,:,:), rhs(:,:,:)
  character(len=256) :: par_file

  par_file = 'parameter_files/vacuum_kerr.par'
  call get_command_argument(1, par_file)
  call init_params(trim(par_file))
  call setup_grid()
  call alloc_state(u, Nx, Nz)
  allocate(rhs(Nx, Nz, NVARS))

  call set_kerr_schild_id(u)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call extrapolate_excised(u)
  call init_deriv_stencils()

  rhs = 0.0_dp
  call compute_rhs_interior(u, rhs)
  call report_operator('interior RHS', rhs)

  rhs = 0.0_dp
  call add_ko_dissipation(u, rhs)
  call report_operator('KO dissipation', rhs)

  rhs = 0.0_dp
  call add_sommerfeld_bc(u, rhs)
  call report_operator('outer boundary', rhs)

  rhs = 0.0_dp
  call apply_legacy_excision_sat(u, rhs, dx, .true.)
  call report_operator('legacy inner SAT', rhs)

  if (any(u /= u)) error stop 'boundary isolation: non-finite initial state'

contains

  subroutine report_operator(label, contribution)
    character(len=*), intent(in) :: label
    real(dp), intent(in) :: contribution(:,:,:)
    integer :: ii, kk, region, counts(3)
    real(dp) :: max_abs(3), sum_sq(3), value, rr
    character(len=12), parameter :: names(3) = [ 'near-mask   ', 'bulk        ', 'outer-face  ' ]

    counts = 0
    max_abs = 0.0_dp
    sum_sq = 0.0_dp
    do kk = 1, Nz
      do ii = 1, Nx
        if (is_excised(ii,kk)) cycle
        rr = sqrt(kerr_radius_squared(xg(ii), zg(kk)))
        if (ii == Nx .or. kk == 1 .or. kk == Nz) then
          region = 3
        else if (rr < r_excise + 3.0_dp*max(dx,dz)) then
          region = 1
        else
          region = 2
        end if
        value = maxval(abs(contribution(ii,kk,:)))
        counts(region) = counts(region) + 1
        max_abs(region) = max(max_abs(region), value)
        sum_sq(region) = sum_sq(region) + value*value
      end do
    end do

    write(*,'(A)') trim(label)
    write(*,'(A)') '  region          cells       max(field)       RMS(field)'
    do region = 1, 3
      write(*,'(A,5X,I8,2(5X,ES14.6))') names(region), counts(region), &
        max_abs(region), sqrt(sum_sq(region)/real(max(1,counts(region)),dp))
    end do
  end subroutine report_operator

end program diagnose_boundary_isolation
