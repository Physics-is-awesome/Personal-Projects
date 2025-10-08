module velocity_1d
  use states_1d
  implicit none
contains

  subroutine compute_velocity(N, rho_h, m_h, u_h)
    implicit none

    ! Dummy arguments
    integer, intent(in) :: N
    real(8), intent(in) :: rho_h(N), m_h(N)
    real(8), intent(out) :: u_h(N)

    ! Local variables
    integer :: i
    real(8), parameter :: eps = 1.0d-12  ! Small number to avoid division by zero

    ! Compute velocity safely
    do i = 1, N
      if (rho_h(i) > eps) then
        u_h(i) = m_h(i) / rho_h(i)
      else
        u_h(i) = 0.0d0
      end if
    end do

  end subroutine compute_velocity

end module velocity_1d
