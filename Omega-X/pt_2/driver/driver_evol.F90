module evol
  use m_h_module
  use quad
  use basis
  use mass_matrix

  implicit none
  

contains
  subroutine driver_evolution()

    integer :: i



    ! call quadrature weights and nodes
    call quadrature_points(Omega)

    ! call basis functions
    call basis_functions(Omega)
    ! run evolution in each program, to get week form

    call compute_m_h()

    
    ! multiply by week form and sum for all nodes


    ! calculate mass matrix
    call compute_mass_matrix(Omega)


  ! multiplly evolution and the inverse of the mass matrix

  
  ! Time integrator 
  ! create hdf5 program and add data
  end subroutine driver_evolution
end module evol
  


  !
