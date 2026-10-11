program test_fo_sbp_sat
  use kinds_mod, only: dp
  use fo_sbp_mod, only: sbp_d1, sbp_norm
  use fo_sat_mod, only: scalar_wave_sat
  implicit none
  integer, parameter :: n = 64
  real(dp), parameter :: dx = 1.0_dp/real(n-1,dp)
  real(dp) :: f(n), df(n), h(n), rhs(2), target(2), e0, e1
  integer :: i

  do i = 1, n
    f(i) = sin(2.0_dp*acos(-1.0_dp)*real(i-1,dp)*dx)
  end do
  call sbp_d1(f, dx, df)
  call sbp_norm(n, dx, h)
  e0 = sum(h*df*f)
  e1 = 0.5_dp*(f(n)**2-f(1)**2)
  if (abs(e0-e1) > 2.0e-2_dp) error stop 'SBP energy identity failed'

  rhs = 0.0_dp
  target = 0.0_dp
  call scalar_wave_sat([0.2_dp, -0.1_dp], target, 0.0_dp, 1.0_dp, 1.0_dp, 1.0_dp, dx, rhs)
  if (maxval(abs(rhs)) <= 0.0_dp) error stop 'SAT penalty was not applied'
  write(*,'(A,ES12.4,A,2(ES12.4,1X))') 'SBP identity error=', abs(e0-e1), 'SAT=', rhs
end program test_fo_sbp_sat
