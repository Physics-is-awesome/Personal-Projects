module basis

  implicit none

contains
  !=========================================================
  ! Quadratic basis functions (1D, 3 nodes)
  !
  ! Arguments:
  !   xi_q   : input quadrature points (size nq)
  !   phi    : output basis functions (3 x nq)
  !=========================================================
  subroutine basis_functions(xi_q, phi)
    implicit none
    real(8), intent(in)  :: xi_q(:)        ! quadrature points
    real(8), intent(out) :: phi(3, size(xi_q)) ! basis functions
    integer :: q, nq

    nq = size(xi_q)

    do q = 1, nq
      phi(1,q) = 0.5d0 * xi_q(q) * (xi_q(q) - 1.0d0)
      phi(2,q) = 1.0d0 - xi_q(q)**2
      phi(3,q) = 0.5d0 * xi_q(q) * (xi_q(q) + 1.0d0)
    end do
  end subroutine basis_functions

end module basis
