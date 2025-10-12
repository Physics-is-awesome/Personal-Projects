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

  subroutine advance_one_step(dt, u_h, rhs_m, rho_rhs, rhs_sigma, T_h, eta_h)
    real(8), intent(in) :: dt
    real(8), intent(inout) :: u_h(N), rhs_m(N), rho_rhs(N), rhs_sigma(N)
    integer :: i

    ! Step 1: Compute velocity
    call compute_velocity(N, rho_h, m_h, u_h)
    
    ! Step 2: calculate tempeture
    do i = 1, N-1
      T_h(i) = compute_temperature(eta_h(i)) 
    end do
    ! step three: calculate Eta
    do i = 1, N-1
      eta_h(i) =compute_eta(T_h(i))
    end do
    ! Step 4: Compute Galerkin RHS terms
    call compute_momentum_rhs(N, rho_h, m_h, sigma_h, eta_h, T_h, rhs_m)
    call compute_mass_flux(N, rho_h, u_h, rho_rhs)
    call compute_entropy_rhs(N, sigma_h, u_h, T_h, rhs_sigma)

    ! Step 5: Advance in time (Euler / RK1)
    do i = 1, N
      m_h(i)     = m_h(i)     + dt * rhs_m(i)
      rho_h(i)   = rho_h(i)   + dt * rho_rhs(i)
      sigma_h(i) = sigma_h(i) + dt * rhs_sigma(i)
    end do

  end subroutine advance_one_step

end module time_integrator_1d

