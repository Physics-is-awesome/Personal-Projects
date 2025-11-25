module calc_vals
  use states
  use declare_1
  use declare_2
  implicit none

  !subroutine derivative_of_temp_by_x()


  !=============================
  ! implicit mid-point in time
  !=============================
  funcion time_discr(z_n, dt, dn_dt) result(z_np1)
    implicit none
    real(8), intent(in) :: z_n, dt
    interface
      function f(z) result(val)
        real(8), intent(in) :: z
        real(8) :: val
      end function f
    end interface
    real(8) :: z_np1, z_half, res, tol
    integer :: iter, max_iter

    ! Initial guess: explicit Euler
    z_np1 = z_n + dt * f(z_n)

    tol = 1.0d-12
    max_iter = 20

    ! Newton iteration to solve implicit midpoint equation:
    ! z_{n+1} = z_n + dt * f( (z_n + z_{n+1})/2 )
    do iter = 1, max_iter
       z_half = 0.5d0 * (z_n + z_np1)
       res    = z_n + dt * f(z_half) - z_np1
       if (abs(res) < tol) exit
       ! Simple fixed-point correction
       z_np1 = z_np1 + res
    end do

  end function implicit_midpoint_step

  !======================
  ! Spacial implicit mid-point
  !======================
  function space_discr(u, dx) result(dudx)
    implicit none
    real(8), intent(in) :: u(:)
    real(8), intent(in) :: dx
    real(8) :: dudx(size(u))
    integer :: i, n

    n = size(u)

    ! Interior points: central difference
    do i = 2, n-1
       dudx(i) = (u(i+1) - u(i-1)) / (2.0d0*dx)
    end do

    ! Boundaries: one-sided differences
    dudx(1)   = (u(2) - u(1)) / dx
    dudx(n)   = (u(n) - u(n-1)) / dx

  end function central_diff

end module space_discretizers

end module calc_vals
