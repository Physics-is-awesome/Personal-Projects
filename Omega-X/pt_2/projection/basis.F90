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
  subroutine basis_functions(Omega)
    type(Omega-X), intent(inout) :: Omega
    
    integer :: q

    

    do q = 1, Omega%m%p
      Omega%proj%phi(1,q) = 0.5d0 * Omega%proj%xi_q(q) * (Omega%proj%xi_q(q) - 1.0d0)
      Omega%proj%phi(2,q) = 1.0d0 - Omega%proj%xi_q(q)**2
      Omega%proj%phi(3,q) = 0.5d0 * Omega%proj%xi_q(q) * (Omega%proj%xi_q(q) + 1.0d0)
    end do
  end subroutine basis_functions

end module basis
