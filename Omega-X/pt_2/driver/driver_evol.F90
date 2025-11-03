module evol
  use mass_matrix
  use read_config
  implicit none
contains
  subroutine driver_evolution()
    real(8) :: M(p+1, p+1)
  ! get numbers from config 
    call read_file()
  ! run evolution in each program

    

  ! calculate mass matrix
    call compute_mass_matrix(p, a, b, M)
    print*, M

  ! multiplly evolution and mass matrix

  
  ! Time integrator 


  end subroutine driver_evolution
end module evol
  
  ! create hdf5 program and add data



  !
