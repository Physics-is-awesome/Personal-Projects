module driver_init
  use read_config
  use states
  use calc_vals
  implicit none

  contains
  subroutine init()
  
    call read_file(mass_mean, temp_mean, momentum_mean, entropy_mean, mass_var, temp_var, momentum_var, entropy_var, dx, a, b, Re, gamma, mass_dist, temp_dist, momentum_dist, entropy_dist, mass, entropy, momentum, nx, ny, nz, dim_run, p)
    call init_state(s, nx, ny, nz)
    call init_momentum(s, nx, ny, nz, momentum_mean, momentum_var, momentum_dist)
    call init_entropy(s, nx, ny, nz, entropy_mean, entropy_var, entropy_dist)
    call init_mass(s, nx, ny, nz, mass_mean, mass_var, mass_dist)
    call calculate_variables(nx, gamma, dx, rho_h, m_h, sigma_h, e, &
                                 U, T_h, dT_h_dx, u_h, du_h_dx, s_h, T_S, &
                                 p, eta_h, deta_h_dx, ny, nz)

  end subroutine init
end module driver_init
