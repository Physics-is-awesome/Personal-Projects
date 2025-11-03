! Automatically generated module for rho_h
module rho_h_module
  implicit none
contains
  subroutine compute_rho_h(phi_m_i, phi_rho_i, phi_sigma_i, u_h, T_h, eta_h, drho_h_dt)
    implicit none
    ! Declare inputs and outputs as real
    real, intent(in) :: phi_m_i, phi_rho_i, phi_sigma_i, u_h, T_h, eta_h
    real, intent(out) :: drho_h_dt

    ! Evolution equation
    F_rho_h = dphi_rho_dx*rho_h*u_h
  end subroutine compute_rho_h
end module
