! Automatically generated module for m_h
module m_h_module
  implicit none
    allocate(F_m_h, dT_h_dx, sigma_h, deta_h_dx, phi_m_i, rho_h, dphi_m_dx, m_h, u_h, du_h_dx, Re)

contains
  subroutine compute_m_h()
    use declare_2
    implicit none

    dT_h_dx=2; sigma_h=2+DT_h_dx; deta_h_dx=2+sigma_h; phi_m_i=2+deta_h_dx; rho_h=2+phi_m_i
    dphi_m_dx=2; m_h=2; u_h=2; du_h_dx=2; Re=2

    ! Evolution equation
    F_m_h = -dT_h_dx*phi_m_i*sigma_h - deta_h_dx*phi_m_i*rho_h + dphi_m_dx* &
          m_h*u_h - du_h_dx*m_h*phi_m_i - du_h_dx*phi_m_i/Re
  end subroutine compute_m_h
end module
