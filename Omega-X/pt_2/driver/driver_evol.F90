module evol
  use mass_matrix
  use read_config
  use m_h_module
  implicit none
contains
  subroutine driver_evolution()
    real(8) :: M(p+1, p+1)
    real(8) :: m_h = 1.0,phi_m_i = 1.0, phi_rho_i = 1.0, phi_sigma_i = 1.0, u_h = 1.0, T_h = 1.0, dT_h_dx = 1.0, sigma_h = 1.0, eta_h = 1.0, deta_h_dx = 1.0, rho_h = 1.0, dphi_m_dx = 1.0, dphi_m_i = 1.0, du_h_dx = 1.0, F_m_h
  ! get numbers from config 
    call read_file()
  ! run evolution in each program
    F_m = call compute_m_h(m_h, phi_m_i, phi_rho_i, phi_sigma_i, u_h, T_h, dT_h_dx, sigma_h, eta_h, deta_h_dx, rho_h, dphi_m_dx, dphi_m_i, du_h_dx, F_m_h)
    print*, F_m

  ! calculate mass matrix
    call compute_mass_matrix(p, a, b, M)
    print*, M

  ! multiplly evolution and mass matrix

  
  ! Time integrator 


  end subroutine driver_evolution
end module evol
  
  ! create hdf5 program and add data



  !
