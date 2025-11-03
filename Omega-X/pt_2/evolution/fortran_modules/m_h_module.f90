! Automatically generated module for m_h
module m_h_module
  implicit none
contains
  subroutine compute_m_h(phi_m_i, phi_rho_i, phi_sigma_i, u_h, T_h, eta_h, dm_h_dt)
    implicit none
    ! Declare inputs and outputs as real
    real, intent(in) :: phi_m_i, phi_rho_i, phi_sigma_i, u_h, T_h, eta_h
    real, intent(out) :: dm_h_dt

    ! Evolution equation
    F_m_h = -dT_h_dx*phi_m_i*sigma_h - deta_h_dx*phi_m_i*rho_h + dphi_m_dx* &
          m_h*u_h - du_h_dx*m_h*phi_m_i - du_h_dx*phi_m_i/Re
  end subroutine compute_m_h
end module
