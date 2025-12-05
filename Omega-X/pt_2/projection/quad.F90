module quad

  implicit none
contains

  !=========================================================
  ! Subroutine: quadrature_points
  !
  ! Purpose:
  !   Computes Gauss–Legendre quadrature points and weights
  !   for a 1D element using 3-point quadrature. The routine
  !   maps reference points from [-1,1] to the physical
  !   element [x_L, x_R].
  !
  ! Arguments:
  !   nx    (in)  : integer, number of elements or domain size
  !   w_q   (out) : real(8) array of quadrature weights (size Nq)
  !   xi_q  (out) : real(8) array of reference quadrature points (size Nq)
  !
  ! Notes:
  !   - Nq is fixed at 3 (3-point Gauss–Legendre).
  !   - The Jacobian J maps reference coordinates to physical coordinates.
  !   - The mapped physical points x_q are computed internally.
  !=========================================================
  subroutine quadrature_points(Omega)
    type(Omega-X), intent(inout) :: Omega

    real(8) :: J                  ! Reference points and weights and Jacobian
    ! Mapped physical points and weights
    real(8) :: x_L, x_R            ! Element bounds 
    integer :: q
    x_L = 0.0d0
    x_R = dble(Omega%m%nx)   ! convert integer nx to real

    J = (x_R - x_L) / 2.0d0              ! Jacobian for mapping

    ! Gauss–Legendre quadrature points and weights on [-1, 1]
    Omega%proj%xi_q(1) = -sqrt(3.0d0 / 5.0d0)
    Omega%proj%xi_q(2) =  0.0d0
    Omega%proj%xi_q(3) =  sqrt(3.0d0 / 5.0d0)

    Omega%proj%w_q(1) = 5.0d0 / 9.0d0
    Omega%proj%w_q(2) = 8.0d0 / 9.0d0
    Omega%proj%w_q(3) = 5.0d0 / 9.0d0

    ! Map reference points to physical element
    do q = 1, Omega%m%p
      Omega%proj%x_q(q) = J * Omega%proj%xi_q(q) + (x_R + x_L) / 2.0d0
    end do

  end subroutine quadrature_points

end module quad

