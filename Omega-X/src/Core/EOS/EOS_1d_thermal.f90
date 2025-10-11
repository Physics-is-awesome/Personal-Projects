module eos_1d
  use states_1d
  implicit none
  private
  public :: compute_temperature, compute_eta



contains

  !------------------------------------------------------------
  ! Compute temperature T from specific entropy eta
  ! T = (gamma - 1) * eta
  !------------------------------------------------------------
  function compute_temperature(eta_h) result(T_h)
    real(8), intent(in) :: eta_h
    real(8) :: T_h

    T_h = (gamma - 1.0d0) * eta_h
  end function compute_temperature

  !------------------------------------------------------------
  ! Compute specific entropy eta from temperature
  ! eta = T / (gamma - 1)
  !------------------------------------------------------------
  function compute_eta(T_h) result(eta_h)
    real(8), intent(in) :: T_h
    real(8) :: eta_h

    eta_h = T_h / (gamma - 1.0d0)
  end function compute_eta

end module eos_1d
