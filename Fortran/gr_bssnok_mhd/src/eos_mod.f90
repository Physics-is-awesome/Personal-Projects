module eos_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: eos_gamma, set_eos_gamma, pressure, eps_from_rho_p, enthalpy, sound_speed_sq
  real(dp) :: eos_gamma = 4.0_dp/3.0_dp

contains

  subroutine set_eos_gamma(gamma)
    real(dp), intent(in) :: gamma
    if (gamma <= 1.0_dp) error stop "EOS Gamma must be greater than one"
    eos_gamma = gamma
  end subroutine set_eos_gamma

  real(dp) function pressure(rho, eps) result(p)
    real(dp), intent(in) :: rho, eps
    p = (eos_gamma - 1.0_dp) * rho * eps
  end function pressure

  real(dp) function eps_from_rho_p(rho, p) result(eps)
    real(dp), intent(in) :: rho, p
    eps = p / ((eos_gamma - 1.0_dp) * max(rho, 1.0e-300_dp))
  end function eps_from_rho_p

  ! specific enthalpy h = 1 + eps + p/rho
  real(dp) function enthalpy(rho, eps) result(h)
    real(dp), intent(in) :: rho, eps
    real(dp) :: p
    p = pressure(rho, eps)
    h = 1.0_dp + eps + p/max(rho, 1.0e-300_dp)
  end function enthalpy

  ! adiabatic sound speed squared, a^2 = Gamma p / (rho h)
  real(dp) function sound_speed_sq(rho, eps) result(a2)
    real(dp), intent(in) :: rho, eps
    real(dp) :: p, h
    p = pressure(rho, eps)
    h = enthalpy(rho, eps)
    a2 = eos_gamma * p / max(rho*h, 1.0e-300_dp)
  end function sound_speed_sq

end module eos_mod
