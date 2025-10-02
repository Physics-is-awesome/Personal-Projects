module entropy_1d
  use states_1d
  implicit none
contains

  subroutine compute_entropy_rhs(N, sigma_h, u_h, T_h, dx, Re, Pr, gamma, rhs)
    implicit none
    ! Dummy arguments
    integer, intent(in) :: N
    real(8), intent(in) :: sigma_h(N), u_h(N), T_h(N)
    real(8), intent(in) :: dx, Re, Pr, gamma
    real(8), intent(out) :: rhs(N)

    ! Local variables
    real(8) :: flux(N), dx_u(N), dx_T(N)
    real(8) :: visc_prod(N), heat_prod(N), heat_grad(N), heat_term(N)
    integer :: i

    ! Compute flux
    do i = 1, N
      flux(i) = sigma_h(i) * u_h(i)
    end do

    ! Apply weak derivatives
    call apply_weak_derivative(flux, rhs)
    call apply_weak_derivative(u_h, dx_u)
    call apply_weak_derivative(T_h, dx_T)

    ! Viscous and thermal terms
    do i = 1, N
      visc_prod(i) = (dx_u(i)**2) / T_h(i)
      heat_grad(i) = dx_T(i) / T_h(i)
      heat_prod(i) = (dx_T(i)**2) / (T_h(i)**2)
      heat_term(i) = (gamma / (gamma - 1.0d0)) * (heat_prod(i) - heat_grad(i) / dx)
      rhs(i) = rhs(i) - (1.0d0 / Re) * visc_prod(i) * dx
      rhs(i) = rhs(i) - (1.0d0 / (Re * Pr)) * heat_term(i) * dx
    end do

    ! Divide by dx
    do i = 1, N
      rhs(i) = -rhs(i) / dx
    end do

    ! Boundary conditions
    rhs(1) = 0.0d0
    rhs(N) = 0.0d0

  end subroutine compute_entropy_rhs

end module entropy_1d
