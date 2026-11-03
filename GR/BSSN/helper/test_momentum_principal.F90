program test_momentum_principal
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, init_params
  use grid_mod, only: setup_grid, xg, zg
  use vars_mod, only: NVARS, IK, IPHI, IGXX, IGYY, IGZZ, IAXX, IAXY, IAXZ, IAYY, IAZZ
  use excision_mod, only: set_excision_background, extrapolate_excised
  use deriv_stencils_mod, only: init_deriv_stencils
  use constraints_mod, only: momentum_constraint
  implicit none

  real(dp), allocatable :: u(:,:,:), mout(:,:,:), mterms(:,:,:,:)
  real(dp) :: amp, wave, expected, measured, tol
  integer :: ii, kk, v

  call init_params('missing-momentum-principal.par')
  call setup_grid()
  call init_deriv_stencils()
  allocate(u(Nx,Nz,NVARS), mout(Nx,Nz,3), mterms(Nx,Nz,3,7))

  u = 0.0_dp
  u(:,:,IGXX) = 1.0_dp
  u(:,:,IGYY) = 1.0_dp
  u(:,:,IGZZ) = 1.0_dp
  call set_excision_background(u)
  call extrapolate_excised(u)

  ii = max(6, Nx-8)
  kk = max(6, Nz/2)
  amp = 1.0e-6_dp
  wave = 0.71_dp
  tol = 2.0e-7_dp

  ! Axx=f(x): M^x = d_x Axx on the flat background.
  call perturb(u, IAXX, 1, amp, wave)
  call momentum_constraint(u, mout, mterms)
  measured = mout(ii,kk,1)
  expected = derivative_x(xg(ii), amp, wave)
  call restore(u, IAXX, 1, amp, wave)
  call check_response('Axx/x -> Mx', measured, expected, tol)

  ! Ayy=f(z): M^z = d_z Azz is tested separately below; this case verifies
  ! that a component with no derivative in the free x index does not leak
  ! into M^x on a flat background.
  call perturb(u, IAYY, 1, amp, wave)
  call momentum_constraint(u, mout, mterms)
  measured = mout(ii,kk,1)
  call restore(u, IAYY, 1, amp, wave)
  call check_response('Ayy/x -> Mx', measured, 0.0_dp, tol)

  ! Axy=f(x): M^y = d_x Axy.
  call perturb(u, IAXY, 1, amp, wave)
  call momentum_constraint(u, mout, mterms)
  measured = mout(ii,kk,2)
  expected = derivative_x(xg(ii), amp, wave)
  call restore(u, IAXY, 1, amp, wave)
  call check_response('Axy/x -> My', measured, expected, tol)

  ! Axz=f(z): M^x = d_z Axz.
  call perturb(u, IAXZ, 2, amp, wave)
  call momentum_constraint(u, mout, mterms)
  measured = mout(ii,kk,1)
  expected = derivative_z(zg(kk), amp, wave)
  call restore(u, IAXZ, 2, amp, wave)
  call check_response('Axz/z -> Mx', measured, expected, tol)

  call perturb(u, IAZZ, 2, amp, wave)
  call momentum_constraint(u, mout, mterms)
  measured = mout(ii,kk,3)
  expected = derivative_z(zg(kk), amp, wave)
  call restore(u, IAZZ, 2, amp, wave)
  call check_response('Azz/z -> Mz', measured, expected, tol)

  write(*,'(A)') 'momentum principal manufactured tests: PASS'

contains

  pure function derivative_x(x, amplitude, wavenumber) result(value)
    real(dp), intent(in) :: x, amplitude, wavenumber
    real(dp) :: value
    value = amplitude*wavenumber*cos(wavenumber*x)
  end function derivative_x

  pure function derivative_z(z, amplitude, wavenumber) result(value)
    real(dp), intent(in) :: z, amplitude, wavenumber
    real(dp) :: value
    value = amplitude*wavenumber*cos(wavenumber*z)
  end function derivative_z

  subroutine perturb(state, var, direction, amplitude, wavenumber)
    real(dp), intent(inout) :: state(:,:,:)
    integer, intent(in) :: var, direction
    real(dp), intent(in) :: amplitude, wavenumber
    integer :: i, k
    do k = 1, size(state,2)
      do i = 1, size(state,1)
        if (direction == 1) then
          state(i,k,var) = state(i,k,var) + amplitude*sin(wavenumber*xg(i))
        else
          state(i,k,var) = state(i,k,var) + amplitude*sin(wavenumber*zg(k))
        end if
      end do
    end do
  end subroutine perturb

  subroutine restore(state, var, direction, amplitude, wavenumber)
    real(dp), intent(inout) :: state(:,:,:)
    integer, intent(in) :: var, direction
    real(dp), intent(in) :: amplitude, wavenumber
    call perturb(state, var, direction, -amplitude, wavenumber)
  end subroutine restore

  subroutine check_response(label, actual, target, tolerance)
    character(len=*), intent(in) :: label
    real(dp), intent(in) :: actual, target, tolerance
    if (abs(actual-target) > tolerance) then
      write(*,'(A,3(1X,ES14.6))') 'FAIL '//trim(label)//' actual/expected/error=', &
        actual, target, actual-target
      error stop 'momentum principal manufactured test failed'
    end if
    write(*,'(A,2(1X,ES12.4))') trim(label)//' actual/expected=', actual, target
  end subroutine check_response

end program test_momentum_principal
