! Measure the full production RHS for route-C data near excision.
program diagnose_equilibrium_ring
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, r_excise, bh_mass, bh_spin, init_params
  use grid_mod, only: setup_grid, xg, zg
  use vars_mod, only: NVARS, alloc_state
  use eq_kerr_bssn_data_mod, only: set_eq_kerr_trumpet_id
  use initial_data_mod, only: compute_conformal_connections
  use excision_mod, only: set_excision_background, extrapolate_excised, is_excised, &
                          kerr_radius_squared
  use deriv_stencils_mod, only: init_deriv_stencils
  use bssn_rhs_mod, only: compute_rhs_interior
  implicit none

  real(dp), allocatable :: u(:,:,:), u_raw(:,:,:), rhs(:,:,:), rhs_filled(:,:,:)
  real(dp) :: spin, r0, rplus, h, rr
  real(dp) :: maxv(3), sumv(3), value
  integer :: countv(3), i, k, region
  character(len=256) :: par_file, buf

  spin = 0.8_dp
  r0 = 1.0_dp
  par_file = 'parameter_files/vacuum_kerr.par'
  call get_command_argument(1, buf)
  if (len_trim(buf) > 0) read(buf,*) spin
  call get_command_argument(2, buf)
  if (len_trim(buf) > 0) read(buf,*) r0
  call get_command_argument(3, buf)
  if (len_trim(buf) > 0) par_file = buf

  call init_params(trim(par_file))
  if (r_excise <= r0) error stop 'equilibrium ring: excision must cover the trumpet surface'
  bh_spin = spin
  call setup_grid()
  call alloc_state(u, Nx, Nz)
  allocate(u_raw(Nx,Nz,NVARS), rhs(Nx, Nz, NVARS), rhs_filled(Nx,Nz,NVARS))

  call set_eq_kerr_trumpet_id(u, bh_mass, spin, r0)
  call compute_conformal_connections(u)
  u_raw = u
  call set_excision_background(u)
  call init_deriv_stencils()
  h = max(dx,dz)
  rplus = bh_mass + sqrt(max(0.0_dp,bh_mass**2-spin**2))
  ! Build the working mask once, then restore the analytic/projected state so
  ! the first RHS uses the same stencil classification without ghost values.
  call extrapolate_excised(u)
  u = u_raw
  rhs = 0.0_dp
  call compute_rhs_interior(u, rhs)
  call report_rhs('projected state before ghost fill', rhs)

  call extrapolate_excised(u)
  rhs_filled = 0.0_dp
  call compute_rhs_interior(u, rhs_filled)
  call report_rhs('production state after ghost fill', rhs_filled)
  call report_rhs('RHS change caused by ghost fill', rhs_filled-rhs)

  write(*,'(A,F6.3,A,F6.3,A,F8.4,A,F8.4)') &
    'Route-C equilibrium ring: spin=', spin, ' R0=', r0, &
    ' r_excise=', r_excise, ' r_plus=', rplus
  if (any(u /= u) .or. any(rhs_filled /= rhs_filled)) &
    error stop 'equilibrium ring: non-finite state'

contains

  subroutine report_rhs(label, values)
    character(len=*), intent(in) :: label
    real(dp), intent(in) :: values(:,:,:)
    real(dp) :: max_local(3), sum_local(3), rr_local, value_local
    integer :: count_local(3), i_local, k_local, region_local, n

    max_local = 0.0_dp
    sum_local = 0.0_dp
    count_local = 0
    do k_local = 2, Nz-1
      do i_local = 2, Nx-1
        if (is_excised(i_local,k_local)) cycle
        rr_local = sqrt(kerr_radius_squared(xg(i_local),zg(k_local)))
        if (rr_local < r_excise + 2.0_dp*h) then
          region_local = 1
        else if (rr_local < rplus + 2.0_dp*h) then
          region_local = 2
        else if (rr_local < 4.0_dp) then
          region_local = 3
        else
          cycle
        end if
        value_local = maxval(abs(values(i_local,k_local,:)))
        count_local(region_local) = count_local(region_local) + 1
        max_local(region_local) = max(max_local(region_local), value_local)
        sum_local(region_local) = sum_local(region_local) + value_local**2
      end do
    end do
    write(*,'(A)') trim(label)
    write(*,'(A)') 'region                 cells       max(field)       RMS(field)'
    do n = 1, 3
      write(*,'(A,I12,2(5X,ES14.6))') region_name(n), count_local(n), &
        max_local(n), sqrt(sum_local(n)/real(max(1,count_local(n)),dp))
    end do
  end subroutine report_rhs

  character(len=24) function region_name(n)
    integer, intent(in) :: n
    select case (n)
    case (1); region_name = 'excision-to-horizon'
    case (2); region_name = 'horizon-shell'
    case default; region_name = 'outer-bulk'
    end select
  end function region_name

end program diagnose_equilibrium_ring
