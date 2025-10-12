module mass_1d
  use mesh_1d
  use projection_matrix_1d
  implicit none
contains

  subroutine compute_mass_flux(N, rho_h, u_h, rho_rhs)
    integer, intent(in) :: N
    real(8), intent(in) :: rho_h(N), u_h(N)
    real(8), intent(out) :: rho_rhs(N)
    real(8) :: flux(N)
    integer :: i

    ! Compute flux
    do i = 1, N
      flux(i) = rho_h(i) * u_h(i)
    end do

    ! Apply weak derivative
    call apply_weak_derivative(flux, rho_rhs)

    ! Divide by dx
    do i = 1, N
      rho_rhs(i) = -rho_rhs(i) / dx
    end do

    ! Boundary conditions
    rho_rhs(1) = 0.0d0
    rho_rhs(N) = 0.0d0

  end subroutine compute_mass_flux

end module mass_1d
