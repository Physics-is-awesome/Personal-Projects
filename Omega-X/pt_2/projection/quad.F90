module quad
  use declare_1
  use states
  implicit none
contains

! this is only 1d and using 3-point Guass-Legendre =========================================
  subroutine quadrature_points()
    integer, parameter :: Nq = 3         ! Number of quadrature points # change to intent in rather than parameter at later point
    real(8) :: xi_q(Nq), J                  ! Reference points and weights and Jacobian
    real(8), allocatable :: x_q(:), w_q(:)     ! Mapped physical points and weights
    real(8) :: x_L, x_R            ! Element bounds 
    integer :: q
    allocate(x_q(Nq), w_q(Nq))
    x_L = 0.0d0
    x_R = dble(nx)   ! convert integer nx to real

    J = (x_R - x_L) / 2.0d0              ! Jacobian for mapping

    ! Gauss–Legendre quadrature points and weights on [-1, 1]
    xi_q(1) = -sqrt(3.0d0 / 5.0d0)
    xi_q(2) =  0.0d0
    xi_q(3) =  sqrt(3.0d0 / 5.0d0)

    w_q(1) = 5.0d0 / 9.0d0
    w_q(2) = 8.0d0 / 9.0d0
    w_q(3) = 5.0d0 / 9.0d0

    ! Map reference points to physical element
    do q = 1, Nq
      x_q(q) = J * xi_q(q) + (x_R + x_L) / 2.0d0
    end do

  end subroutine quadrature_points

end module quad
! hi
