! Automatically generated module for sigma_h
module sigma_h_module
  implicit none
contains
  subroutine compute_sigma_h(phi_m_i, phi_rho_i, phi_sigma_i, u_h, T_h, eta_h, dsigma_h_dt)
    implicit none
    ! Declare inputs and outputs as real
    real, intent(in) :: phi_m_i, phi_rho_i, phi_sigma_i, u_h, T_h, eta_h
    real, intent(out) :: dsigma_h_dt

    ! Evolution equation
    F_sigma_h = (Pr*(gamma - 1)*(dphi_sigma_dx*sigma_h*u_h - du_h_dx**2* &
          phi_sigma_i/T_h) + dT_h_dx*dphi_sigma_dx*gamma/T_h - dT_h_dx**2* &
          gamma*phi_sigma_i/T_h**2)/(Pr*(gamma - 1))
  end subroutine compute_sigma_h
end module
