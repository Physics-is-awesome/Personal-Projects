module fo_sbp_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: sbp_d1, sbp_norm, sbp_boundary_sign
  public :: sbp_norm_inv, sbp_hinv, sbp_h_inv, sbp_inv_norm, sbp_norm_weights_inv
  public :: sbp_boundary_stencil, sbp_boundary_stencils, sbp_d1_boundary
  public :: sbp_boundary_left, sbp_boundary_right
  public :: sbp_left_boundary_stencil, sbp_right_boundary_stencil
  public :: sbp_boundary_coeffs, sbp_boundary_weights

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

  ! Inverse SBP norm weights: (H^{-1})_ii = 1 / H_ii.
  subroutine sbp_norm_inv(n, dx, hinv)
    integer, intent(in) :: n
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: hinv(:)
    real(dp) :: h(n)
    if (size(hinv) /= n .or. n < 3) error stop 'sbp_norm_inv: invalid array'
    call sbp_norm(n, dx, h)
    hinv = 1.0_dp / h
  end subroutine sbp_norm_inv

  subroutine sbp_hinv(n, dx, hinv)
    integer, intent(in) :: n
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: hinv(:)
    call sbp_norm_inv(n, dx, hinv)
  end subroutine sbp_hinv

  subroutine sbp_h_inv(n, dx, hinv)
    integer, intent(in) :: n
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: hinv(:)
    call sbp_norm_inv(n, dx, hinv)
  end subroutine sbp_h_inv

  subroutine sbp_inv_norm(n, dx, hinv)
    integer, intent(in) :: n
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: hinv(:)
    call sbp_norm_inv(n, dx, hinv)
  end subroutine sbp_inv_norm

  subroutine sbp_norm_weights_inv(n, dx, hinv)
    integer, intent(in) :: n
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: hinv(:)
    call sbp_norm_inv(n, dx, hinv)
  end subroutine sbp_norm_weights_inv

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

  ! Boundary stencil coefficients for d/dx at the domain boundary.
  ! side = 1 (left):  coefficients for points (1,2,3)
  ! side = 2 (right): coefficients for points (n-2,n-1,n)
  subroutine sbp_boundary_stencil(side, dx, coeffs)
    integer, intent(in) :: side
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: coeffs(:)
    if (size(coeffs) < 3) error stop 'sbp_boundary_stencil: coeffs must have size >= 3'
    coeffs = 0.0_dp
    if (side == 1) then
      coeffs(1) = -3.0_dp/(2.0_dp*dx)
      coeffs(2) =  4.0_dp/(2.0_dp*dx)
      coeffs(3) = -1.0_dp/(2.0_dp*dx)
    else if (side == 2) then
      coeffs(1) =  1.0_dp/(2.0_dp*dx)
      coeffs(2) = -4.0_dp/(2.0_dp*dx)
      coeffs(3) =  3.0_dp/(2.0_dp*dx)
    else
      error stop 'sbp_boundary_stencil: side must be 1 or 2'
    end if
  end subroutine sbp_boundary_stencil

  subroutine sbp_boundary_stencils(side, dx, coeffs)
    integer, intent(in) :: side
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: coeffs(:)
    call sbp_boundary_stencil(side, dx, coeffs)
  end subroutine sbp_boundary_stencils

  subroutine sbp_d1_boundary(side, dx, coeffs)
    integer, intent(in) :: side
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: coeffs(:)
    call sbp_boundary_stencil(side, dx, coeffs)
  end subroutine sbp_d1_boundary

  subroutine sbp_boundary_left(dx, coeffs)
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: coeffs(:)
    call sbp_boundary_stencil(1, dx, coeffs)
  end subroutine sbp_boundary_left

  subroutine sbp_boundary_right(dx, coeffs)
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: coeffs(:)
    call sbp_boundary_stencil(2, dx, coeffs)
  end subroutine sbp_boundary_right

  subroutine sbp_left_boundary_stencil(dx, coeffs)
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: coeffs(:)
    call sbp_boundary_stencil(1, dx, coeffs)
  end subroutine sbp_left_boundary_stencil

  subroutine sbp_right_boundary_stencil(dx, coeffs)
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: coeffs(:)
    call sbp_boundary_stencil(2, dx, coeffs)
  end subroutine sbp_right_boundary_stencil

  subroutine sbp_boundary_coeffs(side, dx, coeffs)
    integer, intent(in) :: side
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: coeffs(:)
    call sbp_boundary_stencil(side, dx, coeffs)
  end subroutine sbp_boundary_coeffs

  subroutine sbp_boundary_weights(side, dx, coeffs)
    integer, intent(in) :: side
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: coeffs(:)
    call sbp_boundary_stencil(side, dx, coeffs)
  end subroutine sbp_boundary_weights

end module fo_sbp_mod