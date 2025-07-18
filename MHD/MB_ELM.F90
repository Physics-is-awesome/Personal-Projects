! Fortran program for ELM-MHD simulation using metriplectic 4-bracket formalism
! Simplified 2D Cartesian grid, implicit time-stepping
! State variables: rho (density), m (momentum), s (entropy), B (magnetic field)
! Includes resistivity, viscosity, heat conduction, and particle diffusion

program elm_mhd
  implicit none

  ! Constants
  real(8), parameter :: mu_0 = 1.25663706e-6  ! Magnetic permeability (H/m)
  real(8), parameter :: Lx = 1.0, Ly = 1.0   ! Domain size (m)
  integer, parameter :: nx = 100, ny = 100    ! Grid points
  real(8), parameter :: dx = Lx/nx, dy = Ly/ny ! Grid spacing
  real(8), parameter :: dt = 1.0e-6          ! Time step (s)
  integer, parameter :: n_steps = 1000        ! Number of time steps
  real(8), parameter :: eta = 1.0e-5         ! Resistivity (m^2/s)
  real(8), parameter :: mu = 1.0e-4          ! Viscosity (kg/m/s)
  real(8), parameter :: kappa = 1.0e-3       ! Thermal conductivity (W/m/K)
  real(8), parameter :: D = 1.0e-4           ! Particle diffusion coefficient (m^2/s)

  ! State variables (2D arrays)
  real(8) :: rho(nx,ny), m_x(nx,ny), m_y(nx,ny), s(nx,ny), B_x(nx,ny), B_y(nx,ny)
  real(8) :: rho_new(nx,ny), m_x_new(nx,ny), m_y_new(nx,ny), s_new(nx,ny), B_x_new(nx,ny), B_y_new(nx,ny)
  real(8) :: u_x(nx,ny), u_y(nx,ny), T(nx,ny), p(nx,ny), mu_chem(nx,ny)

  ! Temporary arrays for derivatives and fluxes
  real(8) :: grad_ux(2,nx,ny), grad_uy(2,nx,ny), grad_T(2,nx,ny), grad_mu(2,nx,ny), curl_B(nx,ny)
  real(8) :: tau(2,2,nx,ny), J_rho(2,nx,ny), q(2,nx,ny), J(2,nx,ny)

  ! Energy and entropy
  real(8) :: energy, entropy, entropy_prod

  ! Loop variables
  integer :: i, j, t

  ! Initialize fields
  call initialize_fields(rho, m_x, m_y, s, B_x, B_y, nx, ny)

  ! Main time-stepping loop
  do t = 1, n_steps
     ! Compute derived quantities
     call compute_derived(rho, m_x, m_y, s, u_x, u_y, T, p, mu_chem, nx, ny)

     ! Compute Poisson bracket (reversible dynamics)
     call compute_poisson(rho, m_x, m_y, s, B_x, B_y, u_x, u_y, p, &
                          rho_new, m_x_new, m_y_new, s_new, B_x_new, B_y_new, nx, ny)

     ! Compute 4-bracket (dissipative dynamics)
     call compute_4bracket(rho, m_x, m_y, s, B_x, B_y, u_x, u_y, T, mu_chem, &
                          grad_ux, grad_uy, grad_T, grad_mu, curl_B, tau, J_rho, q, J, &
                          rho_new, m_x_new, m_y_new, s_new, B_x_new, B_y_new, nx, ny)

     ! Update fields (implicit solve approximated by explicit update for simplicity)
     rho = rho + dt * rho_new
     m_x = m_x + dt * m_x_new
     m_y = m_y + dt * m_y_new
     s = s + dt * s_new
     B_x = B_x + dt * B_x_new
     B_y = B_y + dt * B_y_new

     ! Apply boundary conditions
     call apply_boundary_conditions(rho, m_x, m_y, s, B_x, B_y, nx, ny)

     ! Compute energy and entropy for consistency checks
     call compute_energy_entropy(rho, m_x, m_y, s, B_x, B_y, T, grad_ux, grad_T, curl_B, &
                                energy, entropy, entropy_prod, nx, ny)

     ! Output diagnostics
     if (mod(t, 100) == 0) then
        write(*, *) 'Step:', t, 'Energy:', energy, 'Entropy Production:', entropy_prod
     end if
  end do

  ! Save final state
  call save_fields(rho, m_x, m_y, s, B_x, B_y, nx, ny)

end program elm_mhd

! Subroutine to initialize fields (e.g., pedestal profile with ELM perturbation)
subroutine initialize_fields(rho, m_x, m_y, s, B_x, B_y, nx, ny)
  implicit none
  integer, intent(in) :: nx, ny
  real(8), intent(out) :: rho(nx,ny), m_x(nx,ny), m_y(nx,ny), s(nx,ny), B_x(nx,ny), B_y(nx,ny)
  integer :: i, j
  real(8) :: x, y, r, pedestal_width = 0.1, B0 = 1.0

  do i = 1, nx
     do j = 1, ny
        x = (i-1) * Lx / (nx-1)
        y = (j-1) * Ly / (ny-1)
        r = sqrt((x-0.5*Lx)**2 + (y-0.5*Ly)**2)
        ! Pedestal-like profile
        rho(i,j) = 1.0 + 0.5 * exp(-r**2 / pedestal_width**2)
        s(i,j) = 1.0 + 0.3 * exp(-r**2 / pedestal_width**2)
        m_x(i,j) = 0.0  ! Initial velocity = 0
        m_y(i,j) = 0.0
        B_x(i,j) = B0 * cos(0.1 * r)  ! Simplified magnetic field with perturbation
        B_y(i,j) = B0 * sin(0.1 * r)
     end do
  end do
end subroutine initialize_fields

! Subroutine to compute derived quantities (velocity, temperature, pressure, chemical potential)
subroutine compute_derived(rho, m_x, m_y, s, u_x, u_y, T, p, mu_chem, nx, ny)
  implicit none
  integer, intent(in) :: nx, ny
  real(8), intent(in) :: rho(nx,ny), m_x(nx,ny), m_y(nx,ny), s(nx,ny)
  real(8), intent(out) :: u_x(nx,ny), u_y(nx,ny), T(nx,ny), p(nx,ny), mu_chem(nx,ny)
  integer :: i, j
  real(8) :: e

  do i = 1, nx
     do j = 1, ny
        u_x(i,j) = m_x(i,j) / rho(i,j)
        u_y(i,j) = m_y(i,j) / rho(i,j)
        ! Simplified equation of state: T = s/rho, p = rho*T
        T(i,j) = s(i,j) / rho(i,j)
        p(i,j) = rho(i,j) * T(i,j)
        ! Chemical potential: mu = -|u|^2/2 + e + rho*de/drho (simplified)
        e = T(i,j)  ! Assume e = T for simplicity
        mu_chem(i,j) = -0.5 * (u_x(i,j)**2 + u_y(i,j)**2) + e
     end do
  end do
end subroutine compute_derived

! Subroutine to compute Poisson bracket (reversible dynamics)
subroutine compute_poisson(rho, m_x, m_y, s, B_x, B_y, u_x, u_y, p, &
                          drho_dt, dm_x_dt, dm_y_dt, ds_dt, dB_x_dt, dB_y_dt, nx, ny)
  implicit none
  integer, intent(in) :: nx, ny
  real(8), intent(in) :: rho(nx,ny), m_x(nx,ny), m_y(nx,ny), s(nx,ny), B_x(nx,ny), B_y(nx,ny)
  real(8), intent(in) :: u_x(nx,ny), u_y(nx,ny), p(nx,ny)
  real(8), intent(out) :: drho_dt(nx,ny), dm_x_dt(nx,ny), dm_y_dt(nx,ny), ds_dt(nx,ny), dB_x_dt(nx,ny), dB_y_dt(nx,ny)
  real(8) :: div_rho_u(nx,ny), div_m_u(nx,ny), div_BB(nx,ny), curl_uB(nx,ny)
  integer :: i, j

  ! Compute divergences and curls
  do i = 2, nx-1
     do j = 2, ny-1
        ! div(rho*u)
        div_rho_u(i,j) = (rho(i+1,j)*u_x(i+1,j) - rho(i-1,j)*u_x(i-1,j))/(2*dx) + &
                         (rho(i,j+1)*u_y(i,j+1) - rho(i,j-1)*u_y(i,j-1))/(2*dy)
        ! div(m*u + p*I - B*B/mu_0)
        div_m_u(i,j) = (m_x(i+1,j)*u_x(i+1,j) - m_x(i-1,j)*u_x(i-1,j))/(2*dx) + &
                       (m_x(i,j+1)*u_y(i,j+1) - m_x(i,j-1)*u_y(i,j-1))/(2*dy) + &
                       (p(i+1,j) - p(i-1,j))/(2*dx) - &
                       (B_x(i+1,j)*B_x(i+1,j) - B_x(i-1,j)*B_x(i-1,j))/(2*dx*mu_0)
        ! div(s*u)
        ds_dt(i,j) = (s(i+1,j)*u_x(i+1,j) - s(i-1,j)*u_x(i-1,j))/(2*dx) + &
                     (s(i,j+1)*u_y(i,j+1) - s(i,j-1)*u_y(i,j-1))/(2*dy)
        ! curl(u x B)
        curl_uB(i,j) = ( (u_y(i,j+1)*B_x(i,j+1) - u_y(i,j-1)*B_x(i,j-1))/(2*dy) - &
                         (u_x(i+1,j)*B_y(i+1,j) - u_x(i-1,j)*B_y(i-1,j))/(2*dx) )
     end do
  end do

  ! Poisson bracket terms
  drho_dt = -div_rho_u
  dm_x_dt = -div_m_u
  dm_y_dt = -div_m_u  ! Simplified for 2D
  ds_dt = -ds_dt
  dB_x_dt = curl_uB
  dB_y_dt = -curl_uB  ! Adjusted for 2D
end subroutine compute_poisson

! Subroutine to compute 4-bracket (dissipative dynamics)
subroutine compute_4bracket(rho, m_x, m_y, s, B_x, B_y, u_x, u_y, T, mu_chem, &
                           grad_ux, grad_uy, grad_T, grad_mu, curl_B, tau, J_rho, q, J, &
                           drho_dt, dm_x_dt, dm_y_dt, ds_dt, dB_x_dt, dB_y_dt, nx, ny)
  implicit none
  integer, intent(in) :: nx, ny
  real(8), intent(in) :: rho(nx,ny), m_x(nx,ny), m_y(nx,ny), s(nx,ny), B_x(nx,ny), B_y(nx,ny)
  real(8), intent(in) :: u_x(nx,ny), u_y(nx,ny), T(nx,ny), mu_chem(nx,ny)
  real(8), intent(out) :: grad_ux(2,nx,ny), grad_uy(2,nx,ny), grad_T(2,nx,ny), grad_mu(2,nx,ny)
  real(8), intent(out) :: curl_B(nx,ny), tau(2,2,nx,ny), J_rho(2,nx,ny), q(2,nx,ny), J(2,nx,ny)
  real(8), intent(inout) :: drho_dt(nx,ny), dm_x_dt(nx,ny), dm_y_dt(nx,ny), ds_dt(nx,ny), dB_x_dt(nx,ny), dB_y_dt(nx,ny)
  integer :: i, j
  real(8) :: div_tau(nx,ny), div_q(nx,ny), div_J_rho(nx,ny), curl_J(nx,ny)

  ! Compute gradients and curls
  do i = 2, nx-1
     do j = 2, ny-1
        grad_ux(1,i,j) = (u_x(i+1,j) - u_x(i-1,j))/(2*dx)
        grad_ux(2,i,j) = (u_x(i,j+1) - u_x(i,j-1))/(2*dy)
        grad_uy(1,i,j) = (u_y(i+1,j) - u_y(i-1,j))/(2*dx)
        grad_uy(2,i,j) = (u_y(i,j+1) - u_y(i,j-1))/(2*dy)
        grad_T(1,i,j) = (T(i+1,j) - T(i-1,j))/(2*dx)
        grad_T(2,i,j) = (T(i,j+1) - T(i,j-1))/(2*dy)
        grad_mu(1,i,j) = (mu_chem(i+1,j) - mu_chem(i-1,j))/(2*dx)
        grad_mu(2,i,j) = (mu_chem(i,j+1) - mu_chem(i,j-1))/(2*dy)
        curl_B(i,j) = (B_y(i+1,j) - B_y(i-1,j))/(2*dx) - (B_x(i,j+1) - B_x(i,j-1))/(2*dy)
     end do
  end do

  ! Compute fluxes
  do i = 2, nx-1
     do j = 2, ny-1
        ! Viscous stress tensor
        tau(1,1,i,j) = mu * (2 * grad_ux(1,i,j) - (2.0/3.0) * (grad_ux(1,i,j) + grad_uy(2,i,j)))
        tau(1,2,i,j) = mu * (grad_ux(2,i,j) + grad_uy(1,i,j))
        tau(2,1,i,j) = tau(1,2,i,j)
        tau(2,2,i,j) = mu * (2 * grad_uy(2,i,j) - (2.0/3.0) * (grad_ux(1,i,j) + grad_uy(2,i,j)))
        ! Particle flux
        J_rho(1,i,j) = -D * rho(i,j) * grad_mu(1,i,j)
        J_rho(2,i,j) = -D * rho(i,j) * grad_mu(2,i,j)
        ! Heat flux
        q(1,i,j) = -kappa * rho(i,j) * grad_T(1,i,j)
        q(2,i,j) = -kappa * rho(i,j) * grad_T(2,i,j)
        ! Current density
        J(1,i,j) = (eta/mu_0) * curl_B(i,j)
        J(2,i,j) = -(eta/mu_0) * curl_B(i,j)
     end do
  end do

  ! Compute divergences
  do i = 2, nx-1
     do j = 2, ny-1
        div_J_rho(i,j) = (J_rho(1,i+1,j) - J_rho(1,i-1,j))/(2*dx) + (J_rho(2,i,j+1) - J_rho(2,i,j-1))/(2*dy)
        div_tau(i,j) = (tau(1,1,i+1,j) - tau(1,1,i-1,j))/(2*dx) + (tau(1,2,i,j+1) - tau(1,2,i,j-1))/(2*dy)
        div_q(i,j) = (q(1,i+1,j) - q(1,i-1,j))/(2*dx) + (q(2,i,j+1) - q(2,i,j-1))/(2*dy)
        curl_J(i,j) = (J(2,i+1,j) - J(2,i-1,j))/(2*dx) - (J(1,i,j+1) - J(1,i,j-1))/(2*dy)
     end do
  end do

  ! Add dissipative terms
  drho_dt = drho_dt - div_J_rho
  dm_x_dt = dm_x_dt + div_tau
  dm_y_dt = dm_y_dt + div_tau
  ds_dt = ds_dt - div_q/(rho*T) + (mu * sum(grad_ux*grad_ux + grad_uy*grad_uy) + &
                                    eta/(mu_0**2 * T) * curl_B**2 + &
                                    D/T * sum(grad_mu*grad_mu))/T
  dB_x_dt = dB_x_dt - curl_J
  dB_y_dt = dB_y_dt + curl_J
end subroutine compute_4bracket

! Subroutine to apply boundary conditions (simplified: no-flux)
subroutine apply_boundary_conditions(rho, m_x, m_y, s, B_x, B_y, nx, ny)
  implicit none
  integer, intent(in) :: nx, ny
  real(8), intent(inout) :: rho(nx,ny), m_x(nx,ny), m_y(nx,ny), s(nx,ny), B_x(nx,ny), B_y(nx,ny)
  ! No-flux boundaries (simplified)
  rho(1,:) = rho(2,:); rho(nx,:) = rho(nx-1,:)
  rho(:,1) = rho(:,2); rho(:,ny) = rho(:,ny-1)
  m_x(1,:) = 0.0; m_x(nx,:) = 0.0; m_x(:,1) = 0.0; m_x(:,ny) = 0.0
  m_y(1,:) = 0.0; m_y(nx,:) = 0.0; m_y(:,1) = 0.0; m_y(:,ny) = 0.0
  s(1,:) = s(2,:); s(nx,:) = s(nx-1,:); s(:,1) = s(:,2); s(:,ny) = s(:,ny-1)
  B_x(1,:) = B_x(2,:); B_x(nx,:) = B_x(nx-1,:); B_x(:,1) = B_x(:,2); B_x(:,ny) = B_x(:,ny-1)
  B_y(1,:) = B_y(2,:); B_y(nx,:) = B_y(nx-1,:); B_y(:,1) = B_y(:,2); B_y(:,ny) = B_y(:,ny-1)
end subroutine apply_boundary_conditions

! Subroutine to compute energy and entropy
subroutine compute_energy_entropy(rho, m_x, m_y, s, B_x, B_y, T, grad_ux, grad_T, curl_B, &
                                 energy, entropy, entropy_prod, nx, ny)
  implicit none
  integer, intent(in) :: nx, ny
  real(8), intent(in) :: rho(nx,ny), m_x(nx,ny), m_y(nx,ny), s(nx,ny), B_x(nx,ny), B_y(nx,ny)
  real(8), intent(in) :: T(nx,ny), grad_ux(2,nx,ny), grad_T(2,nx,ny), curl_B(nx,ny)
  real(8), intent(out) :: energy, entropy, entropy_prod
  integer :: i, j
  real(8) :: e

  energy = 0.0
  entropy = 0.0
  entropy_prod = 0.0
  do i = 2, nx-1
     do j = 2, ny-1
        e = T(i,j)  ! Simplified internal energy
        energy = energy + (0.5 * (m_x(i,j)**2 + m_y(i,j)**2)/rho(i,j) + rho(i,j) * e + &
                          0.5 * (B_x(i,j)**2 + B_y(i,j)**2)/mu_0) * dx * dy
        entropy = entropy + s(i,j) * dx * dy
        entropy_prod = entropy_prod + (mu/T(i,j) * sum(grad_ux(:,i,j)**2) + &
                                      kappa/(T(i,j)**2) * sum(grad_T(:,i,j)**2) + &
                                      eta/(mu_0**2 * T(i,j)) * curl_B(i,j)**2) * rho(i,j) * T(i,j) * dx * dy
     end do
  end do
end subroutine compute_energy_entropy

! Subroutine to save fields (simplified: print to file)
subroutine save_fields(rho, m_x, m_y, s, B_x, B_y, nx, ny)
  implicit none
  integer, intent(in) :: nx, ny
  real(8), intent(in) :: rho(nx,ny), m_x(nx,ny), m_y(nx,ny), s(nx,ny), B_x(nx,ny), B_y(nx,ny)
  integer :: i, j
  open(unit=10, file='elm_mhd_output.dat', status='replace')
  do i = 1, nx
     do j = 1, ny
        write(10, *) i, j, rho(i,j), m_x(i,j), m_y(i,j), s(i,j), B_x(i,j), B_y(i,j)
     end do
  end do
  close(10)
end subroutine save_fields

end program elm_mhd
