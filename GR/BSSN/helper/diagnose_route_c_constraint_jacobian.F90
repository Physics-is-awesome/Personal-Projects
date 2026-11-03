program diagnose_route_c_constraint_jacobian
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, r_excise, bh_mass, bh_spin, initial_data_mode, init_params
  use grid_mod, only: setup_grid, xg, zg
  use vars_mod
  use eq_kerr_bssn_data_mod, only: set_eq_kerr_trumpet_id
  use initial_data_mod, only: compute_conformal_connections
  use excision_mod, only: set_excision_background, extrapolate_excised, excision_status, is_excised
  use deriv_stencils_mod, only: init_deriv_stencils
  use constraints_mod, only: momentum_constraint
  use stencil_mod, only: stencil_t, compute_stencil
  use fo_complete_vars_mod, only: NFO_COMPLETE_VARS, fo_complete_index
  use fo_constraint_fields_mod, only: fo_momentum_constraint_full
  implicit none

  real(dp), allocatable :: u(:,:,:), up(:,:,:), um(:,:,:)
  real(dp), allocatable :: mp(:,:,:), mm(:,:,:), m0(:,:,:)
  real(dp) :: q(NFO_COMPLETE_VARS), qp(NFO_COMPLETE_VARS), qm(NFO_COMPLETE_VARS)
  real(dp) :: mf(3), mfp(3), mfm(3), dprod(3), dfo(3)
  real(dp) :: amp, wave, err_value, err_jac, max_value, max_jac
  integer, parameter :: probes = 6
  integer, parameter :: probe_vars(10) = [IGXX, IGXZ, IGZZ, IAXX, IAXZ, IAZZ, IK, IPHI, IGTX, IGTZ]
  integer :: pi, i, k, v, d, nprobe
  integer :: probe_i(probes), probe_k(probes)
  character(len=256) :: par_file

  par_file = 'parameter_files/vacuum_kerr.par'
  call get_command_argument(1, par_file)
  call init_params(trim(par_file))
  if (initial_data_mode /= 1) error stop 'diagnostic requires route-C initial_data_mode=1'
  if (r_excise <= bh_mass) error stop 'diagnostic requires r_excise > bh_mass'
  call setup_grid()
  call alloc_state(u, Nx, Nz)
  allocate(up(Nx,Nz,NVARS), um(Nx,Nz,NVARS), m0(Nx,Nz,3), &
           mp(Nx,Nz,3), mm(Nx,Nz,3))

  call set_eq_kerr_trumpet_id(u, bh_mass, bh_spin, bh_mass)
  call compute_conformal_connections(u)
  call set_excision_background(u)
  call init_deriv_stencils()
  call extrapolate_excised(u)
  call momentum_constraint(u, m0)

  call choose_probes(probe_i, probe_k, nprobe)
  if (nprobe == 0) error stop 'no valid route-C shell probes found'

  amp = 1.0e-7_dp
  wave = 0.71_dp
  max_value = 0.0_dp
  max_jac = 0.0_dp

  do pi = 1, nprobe
    i = probe_i(pi)
    k = probe_k(pi)
    call local_full_constraint(u, i, k, q, mf)
    err_value = maxval(abs(mf-m0(i,k,:)))
    max_value = max(max_value, err_value)
    write(*,'(A,I0,A,I0,A,2(1X,F10.5),A,ES12.4)') &
      'probe ',i,',',k,' x/z=',xg(i),zg(k),' |full-production|=',err_value

    do d = 1, 3, 2
      do v = 1, size(probe_vars)
        up = u
        um = u
        call perturb(up, probe_vars(v), d, amp, wave)
        call perturb(um, probe_vars(v), d, -amp, wave)
        call momentum_constraint(up, mp)
        call momentum_constraint(um, mm)
        dprod = (mp(i,k,:)-mm(i,k,:))/(2.0_dp*amp)
        call local_full_constraint(up, i, k, qp, mfp)
        call local_full_constraint(um, i, k, qm, mfm)
        dfo = (mfp-mfm)/(2.0_dp*amp)
        err_jac = maxval(abs(dprod-dfo))
        max_jac = max(max_jac, err_jac)
        if (err_jac > 5.0e-5_dp) then
          write(*,'(A,I0,A,I0,A,I0,A,I0,A,ES12.4)') &
            'JACOBIAN MISMATCH probe=',i,',',k,' dir=',d,' var=',probe_vars(v), &
            ' maxerr=',err_jac
        end if
      end do
    end do
  end do

  write(*,'(A,I0)') 'route-C curved momentum probes: ', nprobe
  write(*,'(A,ES12.4)') 'max full-vs-production value error: ', max_value
  write(*,'(A,ES12.4)') 'max full-vs-production Jacobian error: ', max_jac
  if (max_value > 2.0e-10_dp) error stop 'full constraint value mismatch'
  if (max_jac > 5.0e-5_dp) error stop 'full constraint Jacobian mismatch'
  write(*,'(A)') 'route-C curved constraint Jacobian: PASS'

contains

  subroutine local_full_constraint(state, ii, kk, qv, mv)
    real(dp), intent(in) :: state(:,:,:)
    integer, intent(in) :: ii, kk
    real(dp), intent(out) :: qv(NFO_COMPLETE_VARS), mv(3)
    type(stencil_t) :: ss
    integer :: j
    qv = 0.0_dp
    call compute_stencil(state, ii, kk, excision_status, ss)
    qv(1:NVARS) = ss%val
    do j = 1, NVARS
      qv(fo_complete_index(j,1)) = ss%d1x(j)
      qv(fo_complete_index(j,2)) = ss%d1y(j)
      qv(fo_complete_index(j,3)) = ss%d1z(j)
    end do
    call fo_momentum_constraint_full(qv, mv)
  end subroutine local_full_constraint

  subroutine perturb(state, variable, direction, amplitude, wavenumber)
    real(dp), intent(inout) :: state(:,:,:)
    integer, intent(in) :: variable, direction
    real(dp), intent(in) :: amplitude, wavenumber
    integer :: ii, kk
    do kk = 1, size(state,2)
      do ii = 1, size(state,1)
        if (direction == 1) then
          state(ii,kk,variable) = state(ii,kk,variable) + amplitude*sin(wavenumber*xg(ii))
        else
          state(ii,kk,variable) = state(ii,kk,variable) + amplitude*sin(wavenumber*zg(kk))
        end if
      end do
    end do
  end subroutine perturb

  subroutine choose_probes(ii_out, kk_out, count)
    integer, intent(out) :: ii_out(:), kk_out(:), count
    integer :: ii, kk
    real(dp) :: rks
    count = 0
    do kk = 2, Nz-1
      do ii = 2, Nx-1
        rks = kerr_radius(xg(ii), zg(kk))
        if (is_excised(ii,kk)) cycle
        if (rks < r_excise) cycle
        if (rks > r_excise + 10.0_dp*max(dx,dz)) cycle
        count = count + 1
        ii_out(count) = ii
        kk_out(count) = kk
        if (count == size(ii_out)) return
      end do
    end do
  end subroutine choose_probes

  real(dp) function kerr_radius(x, z) result(r)
    real(dp), intent(in) :: x, z
    real(dp) :: rho2, a2, r2
    rho2 = x*x + z*z
    a2 = bh_spin*bh_spin
    r2 = 0.5_dp*(rho2-a2 + sqrt((rho2-a2)**2 + 4.0_dp*a2*z*z))
    r = sqrt(max(r2,0.0_dp))
  end function kerr_radius

end program diagnose_route_c_constraint_jacobian
