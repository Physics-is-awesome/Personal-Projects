program test_fo_symbol_consistency
  ! First production-RHS gate: isolate scalar transport in x and z.
  ! Metric, Ricci, and driver sectors remain separate gates until their
  ! zero-order and second-derivative couplings are completed.
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, init_params
  use grid_mod, only: setup_grid, xg, zg
  use vars_mod, only: NVARS, IPHI, IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ, &
                      IAXX, IALPHA, IBETAX, IBETAY, IBETAZ, IGTX, IGTY, IGTZ
  use excision_mod, only: set_excision_background, extrapolate_excised, excision_status
  use deriv_stencils_mod, only: init_deriv_stencils
  use stencil_mod, only: stencil_t, compute_stencil
  use geometry_mod, only: conformal_geometry_t, compute_conformal_geometry
  use bssn_rhs_mod, only: compute_rhs_point
  use fo_complete_vars_mod, only: NFO_COMPLETE_VARS, fo_complete_index
  use fo_normal_system_mod, only: fo_complete_normal_matrix
  implicit none

  real(dp), allocatable :: u(:,:,:)
  real(dp) :: q(NFO_COMPLETE_VARS), A(NFO_COMPLETE_VARS,NFO_COMPLETE_VARS)
  real(dp) :: lambda(NFO_COMPLETE_VARS), delta(NVARS), rhs1(NVARS), rhs_const(NVARS)
  real(dp) :: normal(3), amp, wave, phase, deriv, predicted, measured, error
  real(dp) :: expected_geometry, trace_delta, delta_r(3,3), delta_x(3,3)
  real(dp) :: expected_connection
  real(dp) :: gi(3,3), metric_base(3,3)
  type(stencil_t) :: Sbase, Spert
  type(conformal_geometry_t) :: Gbase, Gpert
  character(len=32) :: labels(NFO_COMPLETE_VARS)
  character(len=12) :: response_labels(9)
  logical :: supported(NFO_COMPLETE_VARS)
  integer :: v, d, ia, iw, ii, kk, c
  integer, parameter :: response_vars(9) = [IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ, &
                                             IGTX, IGTY, IGTZ]
  integer, parameter :: samples(1) = [IPHI]
  real(dp), parameter :: amplitudes(3) = [1.0e-5_dp, 1.0e-7_dp, 1.0e-9_dp]
  real(dp), parameter :: waves(2) = [0.37_dp, 0.91_dp]

  call init_params('missing-symbol-consistency.par')
  call setup_grid()
  call init_deriv_stencils()
  allocate(u(Nx,Nz,NVARS))
  u = 0.0_dp
  u(:,:,IGXX) = 1.0_dp
  u(:,:,IGYY) = 1.0_dp
  u(:,:,IGZZ) = 1.0_dp
  u(:,:,IALPHA) = 1.2_dp
  u(:,:,IBETAX) = 0.2_dp
  u(:,:,IBETAY) = 0.0_dp
  u(:,:,IBETAZ) = 0.13_dp
  call set_excision_background(u)
  call extrapolate_excised(u)
  u = 0.0_dp
  u(:,:,IGXX) = 1.0_dp
  u(:,:,IGYY) = 1.0_dp
  u(:,:,IGZZ) = 1.0_dp
  u(:,:,IALPHA) = 1.2_dp
  u(:,:,IBETAX) = 0.2_dp
  u(:,:,IBETAZ) = 0.13_dp
  ii = max(4, Nx-6)
  kk = max(4, Nz/2)
  do d = 1, 2
    normal = 0.0_dp
    if (d == 1) normal(1) = 1.0_dp
    if (d == 2) normal(3) = 1.0_dp
    q = 0.0_dp
    q(IGXX) = 1.0_dp; q(IGYY) = 1.0_dp; q(IGZZ) = 1.0_dp
    q(IALPHA) = 1.2_dp
    q(IBETAX:IBETAZ) = [0.2_dp, 0.0_dp, 0.13_dp]
    call fo_complete_normal_matrix(q, normal, A, lambda, labels, supported)

    do v = 1, size(samples)
      do ia = 1, size(amplitudes)
        amp = amplitudes(ia)
        do iw = 1, size(waves)
          wave = waves(iw)
          call perturb_constant(u, samples(v), amp)
          call compute_rhs_point(u, ii, kk, rhs_const)
          call restore_constant(u, samples(v), amp)
          call perturb_state(u, samples(v), d, amp, wave)
          call compute_rhs_point(u, ii, kk, rhs1)
          call restore_state(u, samples(v), d, amp, wave)
          delta = rhs1-rhs_const
          if (d == 1) then
            phase = wave*xg(ii)
          else
            phase = wave*zg(kk)
          end if
          deriv = amp*wave*cos(phase)
          if (abs(deriv) < 1.0e-12_dp) error stop 'symbol test chose zero derivative'
          measured = delta(samples(v))/deriv
          if (d == 1) then
            predicted = -A(samples(v),fo_complete_index(samples(v),1))
          else
            predicted = -A(samples(v),fo_complete_index(samples(v),3))
          end if
          error = abs(measured-predicted)
          if (error > 2.0e-4_dp*max(1.0_dp,abs(predicted))) then
            write(*,'(A,I0,A,ES10.3,A,ES10.3,A,3(1X,ES14.6))') 'FAIL direction=',d, &
              ' amplitude=',amp,' wave=',wave,' measured/predicted/error=', &
              measured, predicted, measured-predicted
            error stop 'production RHS symbol mismatch'
          end if
          write(*,'(A,I0,A,ES10.3,A,ES10.3,A,2(1X,ES11.3))') 'symbol direction=',d, &
            ' amplitude=',amp,' wave=',wave,' measured/predicted=', measured, predicted
        end do
      end do
    end do
  end do

  ! Production-geometry decomposition: compare each metric and connection
  ! component response to the exact Ricci/trace-free contribution returned by
  ! geometry.F90.  This retains Cartoon transverse terms and the
  ! connection-function reduction exactly as production uses them.
  amp = 1.0e-7_dp
  wave = 0.37_dp
  response_labels = [character(len=12) :: 'gxx', 'gxy', 'gxz', 'gyy', 'gyz', 'gzz', &
                     'Gamma-x', 'Gamma-y', 'Gamma-z']
  do d = 1, 2
    do c = 1, size(response_vars)
      call compute_stencil(u, ii, kk, excision_status, Sbase)
      call compute_conformal_geometry(Sbase, Gbase)
      call compute_rhs_point(u, ii, kk, rhs_const)
      call perturb_state(u, response_vars(c), d, amp, wave)
      call compute_stencil(u, ii, kk, excision_status, Spert)
      call compute_conformal_geometry(Spert, Gpert)
      call compute_rhs_point(u, ii, kk, rhs1)
      call restore_state(u, response_vars(c), d, amp, wave)
      delta_r = Gpert%Rt - Gbase%Rt
      metric_base = Gbase%gt
      gi = Gbase%gtinv
      delta_x = 1.2_dp*delta_r
      trace_delta = sum(gi*delta_x)
      expected_geometry = delta_x(1,1) - metric_base(1,1)*trace_delta/3.0_dp
      measured = rhs1(IAXX)-rhs_const(IAXX)
      if (abs(measured-expected_geometry) > 5.0e-7_dp) then
        write(*,'(A,I0,A,A,A,3(1X,ES14.6))') 'FAIL production geometry direction=', d, &
          ' component=', trim(response_labels(c)), ' measured/expected/error=', &
          measured, expected_geometry, measured-expected_geometry
        error stop 'production geometry component decomposition mismatch'
      end if
      write(*,'(A,I0,A,A,A,2(1X,ES11.3))') 'production geometry direction=', d, &
        ' component=', trim(response_labels(c)), ' measured/expected=', &
        measured, expected_geometry
    end do
  end do

  ! Independent connection-reduction check.  With a flat conformal metric,
  ! the connection-function part of the production Ricci tensor is
  !
  !   R_ij[Gamma] = 1/2 (d_j Gamma_i + d_i Gamma_j).
  !
  ! The Axx response is its trace-free projection.  This check deliberately
  ! does not use Gpert%Rt for the expected value.
  do d = 1, 2
    do c = 7, 9
      call compute_rhs_point(u, ii, kk, rhs_const)
      call perturb_state(u, response_vars(c), d, amp, wave)
      call compute_stencil(u, ii, kk, excision_status, Spert)
      call compute_rhs_point(u, ii, kk, rhs1)
      call restore_state(u, response_vars(c), d, amp, wave)
      if (d == 1) then
        deriv = Spert%d1x(response_vars(c))
      else
        deriv = Spert%d1z(response_vars(c))
      end if
      expected_connection = 0.0_dp
      if (d == 1 .and. c == 7) then
        ! Cartoon vector rotation contributes d_y Gamma^y =
        ! Gamma^x/x to the divergence at the symmetry plane.
        expected_connection = 1.2_dp*(2.0_dp*deriv/3.0_dp - &
                                      amp*sin(wave*xg(ii))/(3.0_dp*xg(ii)))
      else if (d == 2 .and. c == 9) then
        expected_connection = (-1.0_dp/3.0_dp)*1.2_dp*deriv
      end if
      measured = rhs1(IAXX)-rhs_const(IAXX)
      if (abs(measured-expected_connection) > 5.0e-7_dp) then
        write(*,'(A,I0,A,A,A,3(1X,ES14.6))') 'FAIL connection reduction direction=', d, &
          ' component=', trim(response_labels(c)), ' measured/expected/error=', &
          measured, expected_connection, measured-expected_connection
        error stop 'connection reduction principal response mismatch'
      end if
      write(*,'(A,I0,A,A,A,2(1X,ES11.3))') 'connection reduction direction=', d, &
        ' component=', trim(response_labels(c)), ' measured/expected=', &
        measured, expected_connection
    end do
  end do

contains

  subroutine perturb_state(state, var, direction, amplitude, wavenumber)
    real(dp), intent(inout) :: state(:,:,:)
    integer, intent(in) :: var, direction
    real(dp), intent(in) :: amplitude, wavenumber
    integer :: j, l
    do l = 1, size(state,2)
      do j = 1, size(state,1)
        if (direction == 1) then
          state(j,l,var) = state(j,l,var) + amplitude*sin(wavenumber*xg(j))
        else
          state(j,l,var) = state(j,l,var) + amplitude*sin(wavenumber*zg(l))
        end if
      end do
    end do
  end subroutine perturb_state

  subroutine restore_state(state, var, direction, amplitude, wavenumber)
    real(dp), intent(inout) :: state(:,:,:)
    integer, intent(in) :: var, direction
    real(dp), intent(in) :: amplitude, wavenumber
    integer :: j, l
    do l = 1, size(state,2)
      do j = 1, size(state,1)
        if (direction == 1) then
          state(j,l,var) = state(j,l,var) - amplitude*sin(wavenumber*xg(j))
        else
          state(j,l,var) = state(j,l,var) - amplitude*sin(wavenumber*zg(l))
        end if
      end do
    end do
  end subroutine restore_state

  subroutine perturb_constant(state, var, amplitude)
    real(dp), intent(inout) :: state(:,:,:)
    integer, intent(in) :: var
    real(dp), intent(in) :: amplitude
    state(:,:,var) = state(:,:,var) + amplitude
  end subroutine perturb_constant

  subroutine restore_constant(state, var, amplitude)
    real(dp), intent(inout) :: state(:,:,:)
    integer, intent(in) :: var
    real(dp), intent(in) :: amplitude
    state(:,:,var) = state(:,:,var) - amplitude
  end subroutine restore_constant

end program test_fo_symbol_consistency
