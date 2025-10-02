module numerics
  use mesh
  implicit none
  contains

  subroutine compute_derivative(field, dfield)
    implicit none
    real(8), intent(in)  :: field(N)
    real(8), intent(out) :: dfield(N)
    integer :: i

    do i = 2, N-1
      dfield(i) = (field(i+1) - field(i-1)) / (2.0d0 * dx)
    end do

    dfield(1) = (field(2) - field(1)) / dx
    dfield(N) = (field(N) - field(N-1)) / dx
  end subroutine compute_derivative

end module numerics
