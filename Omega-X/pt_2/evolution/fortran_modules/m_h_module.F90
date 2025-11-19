! Automatically generated module for m_h
module m_h_module
  implicit none
contains
  subroutine compute_m_h()
    use declare_2
    implicit none
 

    ! Evolution equation
    F_m_h = -dT_h_dx*phi_m_i*sigma_h - deta_h_dx*phi_m_i*rho_h + dphi_m_dx* &
          m_h*u_h - du_h_dx*m_h*phi_m_i - du_h_dx*phi_m_i/Re
  end subroutine compute_m_h
end module
