module fd_weights_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: fd_weights

contains

  ! Weights c(j,k) for the k-th derivative (k=0..m) at point z, using
  ! nodes x(0:n). Works for any node spacing (uniform or not).
  ! Ref: Fornberg, SIAM Review 40(3), 685-691 (1998).
  subroutine fd_weights(z, x, n, m, c)
    real(dp), intent(in)  :: z
    integer,  intent(in)  :: n, m
    real(dp), intent(in)  :: x(0:n)
    real(dp), intent(out) :: c(0:n,0:m)
    real(dp) :: c1, c2, c3, c4, c5
    integer  :: i, j, k, mn

    c1 = 1.0_dp
    c4 = x(0) - z
    c  = 0.0_dp
    c(0,0) = 1.0_dp

    do i = 1, n
      mn = min(i, m)
      c2 = 1.0_dp
      c5 = c4
      c4 = x(i) - z
      do j = 0, i-1
        c3 = x(i) - x(j)
        c2 = c2 * c3
        if (j == i-1) then
          do k = mn, 1, -1
            c(i,k) = c1*(k*c(i-1,k-1) - c5*c(i-1,k)) / c2
          end do
          c(i,0) = -c1*c5*c(i-1,0) / c2
        end if
        do k = mn, 1, -1
          c(j,k) = (c4*c(j,k) - k*c(j,k-1)) / c3
        end do
        c(j,0) = c4*c(j,0) / c3
      end do
      c1 = c2
    end do
  end subroutine fd_weights

end module fd_weights_mod
