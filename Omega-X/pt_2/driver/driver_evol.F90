module evol
  use mass_matrix
  implicit none
contains
  subroutine driver_evolution()
  ! run evolution in each program

    

  ! calculate mass matrix
    integer, parameter :: p = 3
    real(8) :: M(p+1, p+1)
    real(8) :: a, b
    integer :: i, j

    a = 0.0d0
    b = 1.0d0
    call compute_mass_matrix(p, a, b, M)
    print*, M

  ! multiplly evolution and mass matrix

  
  ! Time integrator 

  
  ! create hdf5 program and add data



  !
