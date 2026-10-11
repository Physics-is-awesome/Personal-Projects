module fo_sbp_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: sbp_d1, sbp_norm, sbp_boundary_sign

contains

  subroutine sbp_d1(f, dx, df)
    real(dp), intent(in) :: f(:), dx
    real(dp), intent(out) :: df(:)
    integer :: n, i
    n = size(f)
    if (size(df) /= n .or. n < 3) error stop 'sbp_d1: invalid array'
    df = 0.0_dp
    df(1) = (-3.0_dp*f(1)+4.0_dp*f(2)-f(3))/(2.0_dp*dx)
    do i = 2, n-1
      df(i) = (f(i+1)-f(i-1))/(2.0_dp*dx)
    end do
    df(n) = (3.0_dp*f(n)-4.0_dp*f(n-1)+f(n-2))/(2.0_dp*dx)
  end subroutine sbp_d1

  subroutine sbp_norm(n, dx, h)
    integer, intent(in) :: n
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: h(:)
    if (size(h) /= n .or. n < 3) error stop 'sbp_norm: invalid array'
    h = dx
    h(1) = 0.5_dp*dx
    h(n) = 0.5_dp*dx
  end subroutine sbp_norm

  integer function sbp_boundary_sign(side)
    integer, intent(in) :: side
    if (side == 1) then
      sbp_boundary_sign = -1
    else if (side == 2) then
      sbp_boundary_sign = 1
    else
      error stop 'sbp_boundary_sign: side must be 1 or 2'
    end if
  end function sbp_boundary_sign

end module fo_sbp_mod
