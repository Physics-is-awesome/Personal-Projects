module time_integrator_1d
  use mesh_1d
  use states_1d
  use velocity_1d
  use eos_1d
  use mass_1d
  use momentum_1d
  use entropy_1d
  implicit none
  private
  public :: advance_one_step

contains

  subroutine advance_one_step(dt)
    implicit none
    real(8), intent(in) :: dt

    real(8) :: u_h(N), rhs_m(N), rho_rhs(N), rhs_sigma(N)
    integer :: i

    ! Step 1: Compute velocity
    call compute_velocity(m_h, rho_h, u_h)

    ! Step 2: Compute eta = sigma / rho
    do i = 1, N
      eta_h(i) = sigma_h(i) / rho_h(i)
    end do

    ! Step 3: Compute temperature via EOS
    do i = 1, N
      T_h(i) = compute_temperature(rho_h(i), eta_h(i))
    end do

    ! Step 4: Compute Galerkin RHS terms
    call compute_momentum_rhs_galerkin(rhs_m, rho_h, m_h, sigma_h, eta_h, T_h)
    call compute_mass_rhs_galerkin(rhs_rho, rho_h, u_h)
    call compute_entropy_rhs_galerkin(rhs_sigma, sigma_h, u_h, T_h)

    ! Step 5: Advance in time (Euler / RK1)
    do i = 1, N
      m_h(i)     = m_h(i)     + dt * rhs_m(i)
      rho_h(i)   = rho_h(i)   + dt * rho_rhs(i)
      sigma_h(i) = sigma_h(i) + dt * rhs_sigma(i)
    end do

  end subroutine advance_one_step

end module time_integrator

