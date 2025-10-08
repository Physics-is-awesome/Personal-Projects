module io_1d
  use mesh_1d
  use states_1d
  use velocity_1d
  implicit none
  private
  public :: write_state_to_csv

contains

  subroutine write_state_to_csv(filename)
    implicit none
    character(len=*), intent(in) :: filename
    integer :: i
    real(8) :: u_h(N)

    ! Compute velocity u = m / rho
    call compute_velocity(N, rho_h, m_h, u_h)

    ! Open file for writing
    open(unit=10, file=filename, status="replace", action="write", form="formatted")

    ! Header
    write(10, '(A)') 'x,rho,m,sigma,u,T,eta'

    ! Data
    do i = 1, N
      write(10, '(F12.6,1x,F12.6,1x,F12.6,1x,F12.6,1x,F12.6,1x,F12.6,1x,F12.6)') &
        x_nodes(i), rho_h(i), m_h(i), sigma_h(i), u_h(i), T_h(i), eta_h(i)
    end do

    close(10)
  end subroutine write_state_to_csv

end module io_1d
