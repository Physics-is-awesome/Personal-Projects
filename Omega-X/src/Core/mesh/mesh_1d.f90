module mesh_1d
  implicit none
  private
  public :: initialize_mesh, x_nodes, dx, N, Mii

  ! Parameters
  integer, parameter :: N = 100         ! Number of spatial grid points (nodes)
  real(8), parameter :: L = 1.0d0       ! Length of the domain

  ! Mesh data
  real(8), dimension(N) :: x_nodes      ! Coordinates of each mesh node
  real(8) :: dx                         ! Uniform cell spacing

  ! Mass matrix (diagonal approximation here, for full FEM use full M)
  real(8), dimension(N) :: Mii          ! Lumped mass matrix diagonal

contains

  subroutine initialize_mesh()
    integer :: i

    dx = L / (N - 1)

    ! Initialize node positions
    do i = 1, N
      x_nodes(i) = (i - 1) * dx
    end do

    ! Lumped mass matrix: each diagonal entry is dx
    Mii = dx

    ! Uncomment below to use trapezoidal mass matrix
    ! Mii(1) = 0.5d0 * dx
    ! Mii(N) = 0.5d0 * dx
    ! Mii(2:N-1) = dx

  end subroutine initialize_mesh

end module mesh_1d
