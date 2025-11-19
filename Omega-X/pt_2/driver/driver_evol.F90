module evol
  use read_config
  use m_h_module
  use quad
  use basis
  use mass_matrix
  use declare
  use declare_2
  implicit none
  

contains
  subroutine driver_evolution()
    !real(8) :: M(p+1, p+1)
    !integer :: Re = 1
    !real(8) :: m_h = 1.0,phi_m_i = 1.0, phi_rho_i = 1.0, phi_sigma_i = 1.0, u_h = 1.0, T_h = 1.0, dT_h_dx = 1.0, sigma_h = 1.0, eta_h = 1.0, deta_h_dx = 1.0, rho_h = 1.0, dphi_m_dx = 1.0, dphi_m_i = 1.0, du_h_dx = 1.0, F_m_h
    real(8) :: x_L, X_R
    integer :: i
    ! Define physical element bounds
    x_L = 1.0d0
    x_R = 3.0d0
    ! get numbers from config 
    call read_file()

    ! call quadrature weights and nodes
    call quadrature_points(x_L, X_R, x_q, w_q)

    ! call basis functions
    call basis_functions(x_q, phi)
    ! run evolution in each program, to get week form
    do i=1, 10
      call compute_m_h()
    end do
    
    ! multiply by week form and sum for all nodes


    ! calculate mass matrix
    call compute_mass_matrix(w_q, phi, M)


  ! multiplly evolution and the inverse of the mass matrix

  
  ! Time integrator 
  ! create hdf5 program and add data
  print*, F_m_h
  end subroutine driver_evolution
end module evol
  


  !
