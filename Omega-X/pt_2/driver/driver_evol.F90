module evol
  use read_config
  use m_h_module
  use quad
  use basis
  use mass_matrix
  use declare_1
  ! use declare_2
  implicit none
  

contains
  subroutine driver_evolution()

    integer :: i



    ! call quadrature weights and nodes
    call quadrature_points(nx, w_q, xi_q)

    ! call basis functions
    call basis_functions(xi_q, phi)
    ! run evolution in each program, to get week form

    call compute_m_h()

    
    ! multiply by week form and sum for all nodes


    ! calculate mass matrix
    call compute_mass_matrix(w_q, phi, M)


  ! multiplly evolution and the inverse of the mass matrix

  
  ! Time integrator 
  ! create hdf5 program and add data
  end subroutine driver_evolution
end module evol
  


  !
