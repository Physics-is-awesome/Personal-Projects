module basis
  use quad
  use declare_1
  implicit none

contains
  ! expand more capacity(1d quadratic(?) =============================
  subroutine basis_functions()
    real(8) :: xi_q(3)
    real(8), allocatable :: phi(:,:)
    integer :: q
    allocate(phi(3,3))
    do q = 1, 3
      phi(1,q) = 0.5d0 * xi_q(q) * (xi_q(q) - 1.0d0)
      phi(2,q) = 1.0d0 - xi_q(q)**2
      phi(3,q) = 0.5d0 * xi_q(q) * (xi_q(q) + 1.0d0)
    end do
  end subroutine basis_functions
end module basis
