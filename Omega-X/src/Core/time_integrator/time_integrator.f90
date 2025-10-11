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

  subroutine advance_one_step(Pr, Re, gamma, dx, dt)
    real(8), intent(in) :: dt, Pr, Re, gamma, dx
    real(8) :: u_h(N), rhs_m(N), rho_rhs(N), rhs_sigma(N)
    integer :: i

    ! Step 1: Compute velocity
    call compute_velocity(N, rho_h, m_h, u_h)
    
    ! Step 2: calculate tempeture
    do i = 1, N-1
      T_h = compute_temperature(eta_h) 
    end do
    ! step three: calculate Eta
    do i = 1, N-1
      eta_h =compute_eta(T_h)
    end do
    ! Step 4: Compute Galerkin RHS terms
    ! call temp and eta
    call compute_temperature(rho_h, eta_h) result(T_h)
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

