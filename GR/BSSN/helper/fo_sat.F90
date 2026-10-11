module fo_sat_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: scalar_wave_sat, characteristic_speeds

contains

  subroutine characteristic_speeds(beta_n, alpha, gamma_nn, lambda)
    real(dp), intent(in) :: beta_n, alpha, gamma_nn
    real(dp), intent(out) :: lambda(2)
    if (alpha <= 0.0_dp .or. gamma_nn <= 0.0_dp) &
      error stop 'characteristic_speeds: nonphysical metric'
    lambda = [-beta_n-alpha*sqrt(gamma_nn), -beta_n+alpha*sqrt(gamma_nn)]
  end subroutine characteristic_speeds

  subroutine scalar_wave_sat(u, target, beta_n, alpha, gamma_nn, tau, h, rhs)
    real(dp), intent(in) :: u(2), target(2), beta_n, alpha, gamma_nn, tau, h
    real(dp), intent(inout) :: rhs(2)
    real(dp) :: lambda(2), incoming(2), penalty(2)
    call characteristic_speeds(beta_n, alpha, gamma_nn, lambda)
    incoming = 0.5_dp*(u-target)
    penalty = -tau*max(lambda,0.0_dp)*incoming
    rhs = rhs + penalty/h
  end subroutine scalar_wave_sat

end module fo_sat_mod
