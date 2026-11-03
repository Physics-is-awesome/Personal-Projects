program test_fo_constraint_principal
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, init_params
  use grid_mod, only: setup_grid, xg, zg
  use vars_mod, only: NVARS, IPHI, IGXX, IGYY, IGZZ, IK, IAXX, IAXY, IAXZ, &
                      IAYY, IAYZ, IAZZ, IGTX, IGTY, IGTZ, IALPHA, IBETAX, IBETAZ
  use deriv_stencils_mod, only: init_deriv_stencils
  use excision_mod, only: set_excision_background, extrapolate_excised
  use fo_complete_vars_mod, only: NFO_COMPLETE_VARS, fo_complete_index, &
                                  pack_fo_complete_state
  use fo_complete_rhs_mod, only: compute_complete_state_rhs
  use fo_constraint_fields_mod, only: fo_gamma_constraint, &
                                      fo_momentum_constraint_principal
  implicit none

  real(dp), allocatable :: u(:,:,:), ubase(:,:,:), q(:,:,:), rhs(:,:,:)
  real(dp), allocatable :: qbase(:,:,:), rhsbase(:,:,:)
  real(dp) :: q0(NFO_COMPLETE_VARS), qd(NFO_COMPLETE_VARS)
  real(dp) :: cg(3), cm(3), cgdot(3), cmdot(3)
  real(dp) :: amp, wave, phase, beta_n, expected
  integer :: ii, kk, d, c, st, var, ci
  integer, parameter :: gamma_vars(3) = [IGTX, IGTY, IGTZ]
  integer, parameter :: a_vars_x(3) = [IAXX, IAXY, IAXZ]
  integer, parameter :: a_vars_z(3) = [IAXZ, IAYZ, IAZZ]

  call init_params('missing-fo-constraint-principal.par')
  call setup_grid()
  call init_deriv_stencils()
  allocate(u(Nx,Nz,NVARS), ubase(Nx,Nz,NVARS), q(Nx,Nz,NFO_COMPLETE_VARS), &
           rhs(Nx,Nz,NFO_COMPLETE_VARS), qbase(Nx,Nz,NFO_COMPLETE_VARS), &
           rhsbase(Nx,Nz,NFO_COMPLETE_VARS))
  u = 0.0_dp
  u(:,:,IGXX) = 1.0_dp
  u(:,:,IGYY) = 1.0_dp
  u(:,:,IGZZ) = 1.0_dp
  u(:,:,IALPHA) = 1.0_dp
  u(:,:,IBETAX) = 0.2_dp
  u(:,:,IBETAZ) = 0.13_dp
  call set_excision_background(u)
  call extrapolate_excised(u)
  ubase = u
  call pack_fo_complete_state(ubase, qbase, st)
  call compute_complete_state_rhs(qbase, rhsbase, st)

  ii = max(6, Nx/2)
  kk = max(6, Nz/2)
  amp = 1.0e-6_dp
  wave = 0.71_dp

  do d = 1, 2
    if (d == 1) then
      phase = wave*xg(ii)
      beta_n = 0.2_dp
    else
      phase = wave*zg(kk)
      beta_n = 0.13_dp
    end if

    do c = 1, 3
      var = gamma_vars(c)
      call perturb(u, var, d, amp, wave)
      call measure_constraint_rhs(u, q, rhs, ii, kk, cg, cm, cgdot, cmdot, &
                                  qbase, rhsbase, st)
      call restore(u, var, d, amp, wave)
      if (st /= 0) error stop 'constraint RHS status failure'
      expected = beta_n*amp*wave*cos(phase)
      call check_close('Gamma '//itoa(c)//' advection', cgdot(c), expected, 3.0e-7_dp)
      if (maxval(abs(cgdot - expected*unit(c))) > 3.0e-7_dp) &
        error stop 'Gamma constraint cross-coupling detected'
      if (maxval(abs(cmdot)) > 3.0e-7_dp) error stop 'Gamma-to-momentum coupling detected'
    end do

    do c = 1, 3
      if (d == 1) then
        var = a_vars_x(c)
      else
        var = a_vars_z(c)
      end if
      call perturb(u, var, d, amp, wave)
      call measure_constraint_rhs(u, q, rhs, ii, kk, cg, cm, cgdot, cmdot, &
                                  qbase, rhsbase, st)
      call restore(u, var, d, amp, wave)
      if (st /= 0) error stop 'constraint RHS status failure'
      expected = -beta_n*amp*wave*wave*sin(phase)
      call check_close('Momentum '//itoa(c)//' advection', cmdot(c), expected, 4.0e-7_dp)
      call check_close('Momentum '//itoa(c)//' -> Gamma', cgdot(c), 2.0_dp*cm(c), 4.0e-7_dp)
      if (maxval(abs(cmdot - expected*unit(c))) > 4.0e-7_dp) &
        error stop 'Momentum constraint cross-coupling detected'
      if (maxval(abs(cgdot - 2.0_dp*cm(c)*unit(c))) > 4.0e-7_dp) &
        error stop 'unexpected momentum-to-Gamma coupling'
    end do
  end do

  write(*,'(A)') 'constraint-to-constraint principal matrix: PASS'

contains

  subroutine measure_constraint_rhs(state, qout, rout, i, k, g, m, gd, md, &
                                    qref, rref, status)
    real(dp), intent(in) :: state(:,:,:)
    real(dp), intent(out) :: qout(:,:,:), rout(:,:,:)
    integer, intent(in) :: i, k
    real(dp), intent(out) :: g(3), m(3), gd(3), md(3)
    real(dp), intent(in) :: qref(:,:,:), rref(:,:,:)
    integer, intent(out) :: status
    integer :: pack_status
    call pack_fo_complete_state(state, qout, pack_status)
    call compute_complete_state_rhs(qout, rout, status)
    if (status /= 0) return
    call local_constraints(qout(i,k,:), g, m)
    call linearized_constraints(qout(i,k,:), rout(i,k,:)-rref(i,k,:), gd, md)
  end subroutine measure_constraint_rhs

  subroutine local_constraints(qv, g, m)
    real(dp), intent(in) :: qv(NFO_COMPLETE_VARS)
    real(dp), intent(out) :: g(3), m(3)
    call fo_gamma_constraint(qv, g)
    call fo_momentum_constraint_principal(qv, m)
  end subroutine local_constraints

  subroutine linearized_constraints(qv, dqv, gdot, mdot)
    real(dp), intent(in) :: qv(NFO_COMPLETE_VARS), dqv(NFO_COMPLETE_VARS)
    real(dp), intent(out) :: gdot(3), mdot(3)
    real(dp) :: qp(NFO_COMPLETE_VARS), qm(NFO_COMPLETE_VARS)
    real(dp) :: gp(3), gm(3), mp(3), mm(3)
    real(dp), parameter :: eps = 1.0e-6_dp
    qp = qv + eps*dqv
    qm = qv - eps*dqv
    call local_constraints(qp, gp, mp)
    call local_constraints(qm, gm, mm)
    gdot = (gp-gm)/(2.0_dp*eps)
    mdot = (mp-mm)/(2.0_dp*eps)
  end subroutine linearized_constraints

  subroutine perturb(state, variable, direction, amplitude, wavenumber)
    real(dp), intent(inout) :: state(:,:,:)
    integer, intent(in) :: variable, direction
    real(dp), intent(in) :: amplitude, wavenumber
    integer :: i, k
    do k = 1, size(state,2)
      do i = 1, size(state,1)
        if (direction == 1) then
          state(i,k,variable) = state(i,k,variable) + amplitude*sin(wavenumber*xg(i))
        else
          state(i,k,variable) = state(i,k,variable) + amplitude*sin(wavenumber*zg(k))
        end if
      end do
    end do
  end subroutine perturb

  subroutine restore(state, variable, direction, amplitude, wavenumber)
    real(dp), intent(inout) :: state(:,:,:)
    integer, intent(in) :: variable, direction
    real(dp), intent(in) :: amplitude, wavenumber
    call perturb(state, variable, direction, -amplitude, wavenumber)
  end subroutine restore

  pure function unit(index) result(v)
    integer, intent(in) :: index
    real(dp) :: v(3)
    v = 0.0_dp
    v(index) = 1.0_dp
  end function unit

  subroutine check_close(label, actual, expected_value, tolerance)
    character(len=*), intent(in) :: label
    real(dp), intent(in) :: actual, expected_value, tolerance
    if (abs(actual-expected_value) > tolerance) then
      write(*,'(A,3(1X,ES14.6))') 'FAIL '//trim(label)//' actual/expected/error=', &
        actual, expected_value, actual-expected_value
      error stop 'constraint principal matrix mismatch'
    end if
    write(*,'(A,2(1X,ES12.4))') trim(label)//' actual/expected=', actual, expected_value
  end subroutine check_close

  function itoa(value) result(text)
    integer, intent(in) :: value
    character(len=4) :: text
    write(text,'(I0)') value
  end function itoa

end program test_fo_constraint_principal
