module mesh
    implicit none

    integer, parameter :: nx = 101               ! Number of nodes
    integer, parameter :: ne = nx - 1            ! Number of elements
    real, parameter    :: x_start = 0.0, x_end = 1.0

    real, dimension(nx)       :: x               ! Node positions
    real, dimension(ne)       :: dx              ! Element lengths
    integer, dimension(2, ne) :: conn            ! Connectivity: nodes per element

contains

    subroutine initialize_mesh()
        integer :: i

        ! Create node positions (uniform)
        do i = 1, nx
            x(i) = x_start + (i - 1) * (x_end - x_start) / (nx - 1)
        end do

        ! Connectivity and element size
        do i = 1, ne
            conn(1, i) = i
            conn(2, i) = i + 1
            dx(i) = x(i+1) - x(i)
        end do
    end subroutine initialize_mesh

end module mesh

