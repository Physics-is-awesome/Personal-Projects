module eos
  implicit none
  private
  public :: compute_temperature, compute_eta

  real(8), parameter :: gamma = 1.4d0

contains

  !------------------------------------------------------------
  ! Compute temperature T from specific entropy eta
  ! T = (gamma - 1) * eta
  !------------------------------------------------------------
  function compute_temperature(rho, eta) result(T)
    real(8), intent(in) :: rho, eta
    real(8) :: T

    T = (gamma - 1.0d0) * eta
  end function compute_temperature

  !------------------------------------------------------------
  ! Compute specific entropy eta from temperature
  ! eta = T / (gamma - 1)
  !------------------------------------------------------------
  function compute_eta(rho, T) result(eta)
    real(8), intent(in) :: rho, T
    real(8) :: eta

    eta = T / (gamma - 1.0d0)
  end function compute_eta

end module eos
