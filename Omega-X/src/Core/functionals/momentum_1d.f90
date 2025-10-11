module momentum_1d
  use states_1d, only: Re
  use mesh_1d
  implicit none
contains

  subroutine compute_momentum_rhs(N, rho_h, m_h, sigma_h, eta_h, T_h, rhs_m)
    integer, intent(in) :: N
    real(8), intent(in) :: rho_h(N), m_h(N), sigma_h(N), eta_h(N), T_h(N)
    real(8), intent(out) :: rhs_m(N)
    real(8) :: u_h(N), du_proj(N), mu(N), dmu_proj(N), deta_proj(N), dT_proj(N)
    integer :: i

    ! Compute velocity
    do i = 1, N
      if (rho_h(i) > 1.0d-12) then
        u_h(i) = m_h(i) / rho_h(i)
      else
        u_h(i) = 0.0d0
      end if
    end do

    ! Apply weak derivatives
    call apply_weak_derivative(u_h, du_proj)
    mu = m_h * u_h
    call apply_weak_derivative(mu, dmu_proj)
    call apply_weak_derivative(eta_h, deta_proj)
    call apply_weak_derivative(T_h, dT_proj)

    ! Compute RHS
    do i = 2, N-1
      rhs_m(i) = 0.0d0
      rhs_m(i) = rhs_m(i) - m_h(i) * du_proj(i) * dx
      rhs_m(i) = rhs_m(i) + dmu_proj(i) * dx
      rhs_m(i) = rhs_m(i) - rho_h(i) * deta_proj(i) * dx
      rhs_m(i) = rhs_m(i) - sigma_h(i) * dT_proj(i) * dx
      rhs_m(i) = rhs_m(i) - (1.0d0 / Re) * du_proj(i) * dx
    end do

    ! Boundary conditions
    rhs_m(1) = 0.0d0
    rhs_m(N) = 0.0d0

  end subroutine compute_momentum_rhs

end module momentum_1d
