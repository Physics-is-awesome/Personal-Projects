! Automatically generated module for m_h
module m_h_module
  implicit none

contains
  subroutine compute_m_h()
    use declare_2
    implicit none
    ! allocate(F_m_h, dT_h_dx, sigma_h, deta_h_dx, phi_m_i, rho_h, dphi_m_dx, m_h, u_h, du_h_dx, Re)

    ! Evolution equation
    do i, nx
      F_m_h(i) = -dT_h_dx(i)*phi_m_i(i)*sigma_h(i) - deta_h_dx(i)*phi_m_i(i)*rho_h(i) + dphi_m_dx(i)* &
            m_h(i)*u_h(i) - du_h_dx(i)*m_h(i)*phi_m_i(i) - du_h_dx(i)*phi_m_i(i)/Re
    end do
  end subroutine compute_m_h
end module
