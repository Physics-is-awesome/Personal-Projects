program test_fo_curved_sat
  use kinds_mod, only: dp
  use fo_curved_sat_mod
  implicit none
  real(dp) :: nx, nz, gn, rhs(3), state(3), target(3), speed(3)

  call interface_normal_2d(3.0_dp, 4.0_dp, nx, nz, gn)
  if (abs(nx-0.6_dp) > 1.0e-14_dp .or. abs(nz-0.8_dp) > 1.0e-14_dp) &
    error stop 'curved SAT normal test failed'
  state = [2.0_dp, 2.0_dp, 2.0_dp]
  target = 1.0_dp
  speed = [-1.0_dp, 0.0_dp, 1.0_dp]
  rhs = 0.0_dp
  call apply_curved_characteristic_sat(state, target, speed, 1.0_dp, 0.25_dp, &
                                       0.5_dp, nx, nz, rhs)
  if (rhs(1) <= 0.0_dp .or. abs(rhs(2)) > 1.0e-14_dp .or. abs(rhs(3)) > 1.0e-14_dp) &
    error stop 'curved SAT penalty test failed'
  write(*,'(A,3(1X,ES12.4))') 'curved SAT passed:', nx, nz, rhs(1)
end program test_fo_curved_sat
