module basis
  use quad
  use declare
  implicit none

contains

  subroutine basis_functions(xi_q, phi)
    real(8), INTENT(IN) :: xi_q(3)
    real(8), INTENT(OUT), allocatable :: phi(:,:)
    integer :: q
    allocate(phi(3,3))
    do q = 1, 3
      phi(1,q) = 0.5d0 * xi_q(q) * (xi_q(q) - 1.0d0)
      phi(2,q) = 1.0d0 - xi_q(q)**2
      phi(3,q) = 0.5d0 * xi_q(q) * (xi_q(q) + 1.0d0)
    end do
  end subroutine basis_functions
end module basis
