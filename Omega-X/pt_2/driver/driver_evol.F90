module evol
  use mass_matrix
  use read_config
  use m_h_module
  implicit none
contains
  subroutine driver_evolution()
    real(8) :: M(p+1, p+1)
    real(8) :: phi_m_i = 1.0, phi_rho_i = 1.0, phi_sigma_i = 10.0, u_h = 1.0, T_h= 1.0, eta_h = 1.0, F_m_h
  ! get numbers from config 
    call read_file()
  ! run evolution in each program
    F_m = call compute_m_h(phi_m_i, phi_rho_i, phi_sigma_i, u_h, T_h, eta_h, F_m_h)
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
