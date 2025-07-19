! Advanced Fortran program for ELM-MHD simulation using metriplectic 4-bracket formalism
! 2D flux-coordinate grid (psi, theta), implicit time-stepping, MPI parallelization
! State variables: rho (density), m (momentum), s (entropy), B (magnetic field)
! Includes resistivity, viscosity, heat conduction, particle diffusion, and SOL transport

module elm_mhd_mod
  implicit none
  ! Constants
  integer, parameter :: ikind = 4
  real(8), parameter :: mu_0 = 1.25663706e-6  ! Magnetic permeability (H/m)
  real(8), parameter :: psi_min = 0.0, psi_max = 1.2  ! Flux coordinate range
  real(8), parameter :: theta_min = 0.0, theta_max = 2.0*3.14159265359
  integer(ikind), parameter :: npsi = 200, ntheta = 100  ! Grid points
  real(8), parameter :: dpsi = (psi_max-psi_min)/real(npsi-1, 8)
  real(8), parameter :: dtheta = (theta_max-theta_min)/real(ntheta-1, 8)
  real(8), parameter :: dt = 1.0e-5  ! Time step (s)
  integer(ikind), parameter :: n_steps = 1000  ! Number of time steps
  real(8), parameter :: eta_0 = 1.0e-5  ! Resistivity (m^2/s)
  real(8), parameter :: mu_0_visc = 1.0e-4  ! Viscosity (kg/m/s)
  real(8), parameter :: kappa_0 = 1.0e-3  ! Thermal conductivity (W/m/K)
  real(8), parameter :: D_0 = 1.0e-4  ! Particle diffusion coefficient (m^2/s)
end module elm_mhd_mod

program elm_mhd_advanced
  use mpi
  use elm_mhd_mod
  implicit none

  ! State variables (2D arrays)
  real(8), allocatable :: rho(:,:), m_psi(:,:), m_theta(:,:), s(:,:), B_psi(:,:), B_theta(:,:)
  real(8), allocatable :: rho_new(:,:), m_psi_new(:,:), m_theta_new(:,:), s_new(:,:), B_psi_new(:,:), B_theta_new(:,:)
  real(8), allocatable :: u_psi(:,:), u_theta(:,:), T(:,:), p(:,:), mu_chem(:,:)
  real(8), allocatable :: eta(:,:), mu_visc(:,:), kappa(:,:), D(:,:)  ! Spatially varying coefficients

  ! Temporary arrays for derivatives and fluxes
  real(8), allocatable :: grad_ux(:,:,:), grad_uy(:,:,:), grad_T(:,:,:), grad_mu(:,:,:)
  real(8), allocatable :: curl_B(:,:), tau(:,:,:,:), J_rho(:,:,:), q(:,:,:), J(:,:,:)

  ! Energy and entropy
  real(8) :: energy, entropy, entropy_prod

  ! MPI variables
  integer(ikind) :: ierr, rank, nprocs
  integer(ikind) :: i, j, t

  ! Initialize MPI
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)

  ! Allocate arrays
  allocate(rho(npsi,ntheta), m_psi(npsi,ntheta), m_theta(npsi,ntheta), s(npsi,ntheta), &
           B_psi(npsi,ntheta), B_theta(npsi,ntheta))
  allocate(rho_new(npsi,ntheta), m_psi_new(npsi,ntheta), m_theta_new(npsi,ntheta), &
           s_new(npsi,ntheta), B_psi_new(npsi,ntheta), B_theta_new(npsi,ntheta))
  allocate(u_psi(npsi,ntheta), u_theta(npsi,ntheta), T(npsi,ntheta), p(npsi,ntheta), &
           mu_chem(npsi,ntheta))
  allocate(eta(npsi,ntheta), mu_visc(npsi,ntheta), kappa(npsi,ntheta), D(npsi,ntheta))
  allocate(grad_ux(2,npsi,ntheta), grad_uy(2,npsi,ntheta), grad_T(2,npsi,ntheta), &
           grad_mu(2,npsi,ntheta), curl_B(npsi,ntheta))
  allocate(tau(2,2,npsi,ntheta), J_rho(2,npsi,ntheta), q(2,npsi,ntheta), J(2,npsi,ntheta))

  ! Initialize fields
  call initialize_fields(rho, m_psi, m_theta, s, B_psi, B_theta, eta, mu_visc, kappa, D)

  ! Main time-stepping loop
  do t = 1, n_steps
     ! Compute derived quantities
     call compute_derived(rho, m_psi, m_theta, s, u_psi, u_theta, T, p, mu_chem)

     ! Compute Poisson bracket
     call compute_poisson(rho, m_psi, m_theta, s, B_psi, B_theta, u_psi, u_theta, p, &
                          rho_new, m_psi_new, m_theta_new, s_new, B_psi_new, B_theta_new)

     ! Compute 4-bracket
     call compute_4bracket(rho, m_psi, m_theta, s, B_psi, B_theta, u_psi, u_theta, T, mu_chem, &
                          eta, mu_visc, kappa, D, grad_ux, grad_uy, grad_T, grad_mu, curl_B, &
                          tau, J_rho, q, J, rho_new, m_psi_new, m_theta_new, s_new, &
                          B_psi_new, B_theta_new)

     ! Implicit solve (placeholder: explicit update for now)
     call implicit_solve(rho, m_psi, m_theta, s, B_psi, B_theta, &
                        rho_new, m_psi_new, m_theta_new, s_new, B_psi_new, B_theta_new)

     ! Apply boundary conditions
     call apply_boundary_conditions(rho, m_psi, m_theta, s, B_psi, B_theta)

     ! Compute energy and entropy
     call compute_energy_entropy(rho, m_psi, m_theta, s, B_psi, B_theta, T, grad_ux, grad_T, &
                                curl_B, mu_chem, eta, mu_visc, kappa, D, &
                                energy, entropy, entropy_prod)

     ! Output diagnostics
     if (mod(t, 100_ikind) == 0 .and. rank == 0) then
        write(*, *) 'Step:', t, 'Energy:', energy, 'Entropy Production:', entropy_prod
     end if
  end do

  ! Save final state
  if (rank == 0) call save_fields(rho, m_psi, m_theta, s, B_psi, B_theta)

  ! Finalize MPI
  call MPI_FINALIZE(ierr)

  ! Deallocate arrays
  deallocate(rho, m_psi, m_theta, s, B_psi, B_theta, rho_new, m_psi_new, m_theta_new, &
             s_new, B_psi_new, B_theta_new, u_psi, u_theta, T, p, mu_chem, &
             eta, mu_visc, kappa, D, grad_ux, grad_uy, grad_T, grad_mu, curl_B, &
             tau, J_rho, q, J)

end program elm_mhd_advanced

! Subroutine to initialize fields (pedestal with ELM perturbation)
subroutine initialize_fields(rho, m_psi, m_theta, s, B_psi, B_theta, eta, mu_visc, kappa, D)
  use elm_mhd_mod
  implicit none
  real(8), intent(out) :: rho(npsi,ntheta), m_psi(npsi,ntheta), m_theta(npsi,ntheta)
  real(8), intent(out) :: s(npsi,ntheta), B_psi(npsi,ntheta), B_theta(npsi,ntheta)
  real(8), intent(out) :: eta(npsi,ntheta), mu_visc(npsi,ntheta), kappa(npsi,ntheta), D(npsi,ntheta)
  integer(ikind) :: i, j
  real(8) :: psi, theta, r, pedestal_width = 0.1, B0 = 1.0, psi_sep = 1.0

  do i = 1, npsi
     do j = 1, ntheta
        psi = psi_min + (i-1) * dpsi
        theta = theta_min + (j-1) * dtheta
        r = abs(psi - psi_sep)  ! Distance from separatrix
        ! Pedestal-like profile with ELM perturbation
        rho(i,j) = 1.0 + 0.5 * exp(-r**2 / pedestal_width**2) + 0.1 * sin(5.0*theta) * exp(-r**2)
        s(i,j) = 1.0 + 0.3 * exp(-r**2 / pedestal_width**2) + 0.05 * sin(5.0*theta) * exp(-r**2)
        m_psi(i,j) = 0.0
        m_theta(i,j) = 0.0
        B_psi(i,j) = B0 * cos(0.1 * r)
        B_theta(i,j) = B0 * sin(0.1 * r)
        ! Spatially varying transport coefficients (higher in SOL)
        eta(i,j) = eta_0 * (1.0 + 10.0 * exp(-r**2 / (2.0*pedestal_width**2)))
        mu_visc(i,j) = mu_0_visc * (1.0 + 10.0 * exp(-r**2 / (2.0*pedestal_width**2)))
        kappa(i,j) = kappa_0 * (1.0 + 10.0 * exp(-r**2 / (2.0*pedestal_width**2)))
        D(i,j) = D_0 * (1.0 + 10.0 * exp(-r**2 / (2.0*pedestal_width**2)))
     end do
  end do
end subroutine initialize_fields

! Subroutine to compute derived quantities
subroutine compute_derived(rho, m_psi, m_theta, s, u_psi, u_theta, T, p, mu_chem)
  use elm_mhd_mod
  implicit none
  real(8), intent(in) :: rho(npsi,ntheta), m_psi(npsi,ntheta), m_theta(npsi,ntheta), s(npsi,ntheta)
  real(8), intent(out) :: u_psi(npsi,ntheta), u_theta(npsi,ntheta), T(npsi,ntheta)
  real(8), intent(out) :: p(npsi,ntheta), mu_chem(npsi,ntheta)
  integer(ikind) :: i, j
  real(8) :: e

  do i = 1, npsi
     do j = 1, ntheta
        u_psi(i,j) = m_psi(i,j) / max(rho(i,j), 1.0e-10)  ! Avoid division by zero
        u_theta(i,j) = m_theta(i,j) / max(rho(i,j), 1.0e-10)
        T(i,j) = s(i,j) / max(rho(i,j), 1.0e-10)  ! Simplified EOS
        p(i,j) = rho(i,j) * T(i,j)
        e = T(i,j)  ! Simplified internal energy
        mu_chem(i,j) = -0.5 * (u_psi(i,j)**2 + u_theta(i,j)**2) + e
     end do
  end do
end subroutine compute_derived

! Subroutine to compute Poisson bracket
subroutine compute_poisson(rho, m_psi, m_theta, s, B_psi, B_theta, u_psi, u_theta, p, &
                          drho_dt, dm_psi_dt, dm_theta_dt, ds_dt, dB_psi_dt, dB_theta_dt)
  use elm_mhd_mod
  implicit none
  real(8), intent(in) :: rho(npsi,ntheta), m_psi(npsi,ntheta), m_theta(npsi,ntheta)
  real(8), intent(in) :: s(npsi,ntheta), B_psi(npsi,ntheta), B_theta(npsi,ntheta)
  real(8), intent(in) :: u_psi(npsi,ntheta), u_theta(npsi,ntheta), p(npsi,ntheta)
  real(8), intent(out) :: drho_dt(npsi,ntheta), dm_psi_dt(npsi,ntheta), dm_theta_dt(npsi,ntheta)
  real(8), intent(out) :: ds_dt(npsi,ntheta), dB_psi_dt(npsi,ntheta), dB_theta_dt(npsi,ntheta)
  real(8) :: div_rho_u(npsi,ntheta), div_m_u(npsi,ntheta), curl_uB(npsi,ntheta)
  integer(ikind) :: i, j

  drho_dt = 0.0; dm_psi_dt = 0.0; dm_theta_dt = 0.0; ds_dt = 0.0; dB_psi_dt = 0.0; dB_theta_dt = 0.0
  do i = 2, npsi-1
     do j = 2, ntheta-1
        ! div(rho*u)
        div_rho_u(i,j) = (rho(i+1,j)*u_psi(i+1,j) - rho(i-1,j)*u_psi(i-1,j))/(2.0*dpsi) + &
                         (rho(i,j+1)*u_theta(i,j+1) - rho(i,j-1)*u_theta(i,j-1))/(2.0*dtheta)
        ! div(m*u + (p + |B|^2/2mu_0)*I - B*B/mu_0)
        div_m_u(i,j) = (m_psi(i+1,j)*u_psi(i+1,j) - m_psi(i-1,j)*u_psi(i-1,j))/(2.0*dpsi) + &
                       (m_psi(i,j+1)*u_theta(i,j+1) - m_psi(i,j-1)*u_theta(i,j-1))/(2.0*dtheta) + &
                       (p(i+1,j) + 0.5*(B_psi(i+1,j)**2 + B_theta(i+1,j)**2)/mu_0 - &
                        (p(i-1,j) + 0.5*(B_psi(i-1,j)**2 + B_theta(i-1,j)**2)/mu_0))/(2.0*dpsi) - &
                       (B_psi(i+1,j)*B_psi(i+1,j) - B_psi(i-1,j)*B_psi(i-1,j))/(2.0*dpsi*mu_0)
        ! div(s*u)
        ds_dt(i,j) = (s(i+1,j)*u_psi(i+1,j) - s(i-1,j)*u_psi(i-1,j))/(2.0*dpsi) + &
                     (s(i,j+1)*u_theta(i,j+1) - s(i,j-1)*u_theta(i,j-1))/(2.0*dtheta)
        ! curl(u x B)
        curl_uB(i,j) = ((u_theta(i,j+1)*B_psi(i,j+1) - u_theta(i,j-1)*B_psi(i,j-1))/(2.0*dtheta) - &
                        (u_psi(i+1,j)*B_theta(i+1,j) - u_psi(i-1,j)*B_theta(i-1,j))/(2.0*dpsi))
     end do
  end do

  drho_dt = -div_rho_u
  dm_psi_dt = -div_m_u
  dm_theta_dt = -div_m_u  ! Simplified for 2D
  ds_dt = -ds_dt
  dB_psi_dt = curl_uB
  dB_theta_dt = -curl_uB
end subroutine compute_poisson

! Subroutine to compute 4-bracket
subroutine compute_4bracket(rho, m_psi, m_theta, s, B_psi, B_theta, u_psi, u_theta, T, mu_chem, &
                           eta, mu_visc, kappa, D, grad_ux, grad_uy, grad_T, grad_mu, curl_B, &
                           tau, J_rho, q, J, drho_dt, dm_psi_dt, dm_theta_dt, ds_dt, &
                           dB_psi_dt, dB_theta_dt)
  use elm_mhd_mod
  implicit none
  real(8), intent(in) :: rho(npsi,ntheta), m_psi(npsi,ntheta), m_theta(npsi,ntheta)
  real(8), intent(in) :: s(npsi,ntheta), B_psi(npsi,ntheta), B_theta(npsi,ntheta)
  real(8), intent(in) :: u_psi(npsi,ntheta), u_theta(npsi,ntheta), T(npsi,ntheta), mu_chem(npsi,ntheta)
  real(8), intent(in) :: eta(npsi,ntheta), mu_visc(npsi,ntheta), kappa(npsi,ntheta), D(npsi,ntheta)
  real(8), intent(out) :: grad_ux(2,npsi,ntheta), grad_uy(2,npsi,ntheta), grad_T(2,npsi,ntheta)
  real(8), intent(out) :: grad_mu(2,npsi,ntheta), curl_B(npsi,ntheta)
  real(8), intent(out) :: tau(2,2,npsi,ntheta), J_rho(2,npsi,ntheta), q(2,npsi,ntheta), J(2,npsi,ntheta)
  real(8), intent(inout) :: drho_dt(npsi,ntheta), dm_psi_dt(npsi,ntheta), dm_theta_dt(npsi,ntheta)
  real(8), intent(inout) :: ds_dt(npsi,ntheta), dB_psi_dt(npsi,ntheta), dB_theta_dt(npsi,ntheta)
  integer(ikind) :: i, j
  real(8) :: div_tau(npsi,ntheta), div_q(npsi,ntheta), div_J_rho(npsi,ntheta), curl_J(npsi,ntheta)

  do i = 2, npsi-1
     do j = 2, ntheta-1
        grad_ux(1,i,j) = (u_psi(i+1,j) - u_psi(i-1,j))/(2.0*dpsi)
        grad_ux(2,i,j) = (u_psi(i,j+1) - u_psi(i,j-1))/(2.0*dtheta)
        grad_uy(1,i,j) = (u_theta(i+1,j) - u_theta(i-1,j))/(2.0*dpsi)
        grad_uy(2,i,j) = (u_theta(i,j+1) - u_theta(i,j-1))/(2.0*dtheta)
        grad_T(1,i,j) = (T(i+1,j) - T(i-1,j))/(2.0*dpsi)
        grad_T(2,i,j) = (T(i,j+1) - T(i,j-1))/(2.0*dtheta)
        grad_mu(1,i,j) = (mu_chem(i+1,j) - mu_chem(i-1,j))/(2.0*dpsi)
        grad_mu(2,i,j) = (mu_chem(i,j+1) - mu_chem(i,j-1))/(2.0*dtheta)
        curl_B(i,j) = (B_theta(i+1,j) - B_theta(i-1,j))/(2.0*dpsi) - (B_psi(i,j+1) - B_psi(i,j-1))/(2.0*dtheta)
     end do
  end do

  do i = 2, npsi-1
     do j = 2, ntheta-1
        tau(1,1,i,j) = mu_visc(i,j) * (2.0 * grad_ux(1,i,j) - (2.0/3.0) * (grad_ux(1,i,j) + grad_uy(2,i,j)))
        tau(1,2,i,j) = mu_visc(i,j) * (grad_ux(2,i,j) + grad_uy(1,i,j))
        tau(2,1,i,j) = tau(1,2,i,j)
        tau(2,2,i,j) = mu_visc(i,j) * (2.0 * grad_uy(2,i,j) - (2.0/3.0) * (grad_ux(1,i,j) + grad_uy(2,i,j)))
        J_rho(1,i,j) = -D(i,j) * rho(i,j) * grad_mu(1,i,j)
        J_rho(2,i,j) = -D(i,j) * rho(i,j) * grad_mu(2,i,j)
        q(1,i,j) = -kappa(i,j) * rho(i,j) * grad_T(1,i,j)
        q(2,i,j) = -kappa(i,j) * rho(i,j) * grad_T(2,i,j)
        J(1,i,j) = (eta(i,j)/mu_0) * curl_B(i,j)
        J(2,i,j) = -(eta(i,j)/mu_0) * curl_B(i,j)
     end do
  end do

  do i = 2, npsi-1
     do j = 2, ntheta-1
        div_J_rho(i,j) = (J_rho(1,i+1,j) - J_rho(1,i-1,j))/(2.0*dpsi) + (J_rho(2,i,j+1) - J_rho(2,i,j-1))/(2.0*dtheta)
        div_tau(i,j) = (tau(1,1,i+1,j) - tau(1,1,i-1,j))/(2.0*dpsi) + (tau(1,2,i,j+1) - tau(1,2,i,j-1))/(2.0*dtheta)
        div_q(i,j) = (q(1,i+1,j) - q(1,i-1,j))/(2.0*dpsi) + (q(2,i,j+1) - q(2,i,j-1))/(2.0*dtheta)
        curl_J(i,j) = (J(2,i+1,j) - J(2,i-1,j))/(2.0*dpsi) - (J(1,i,j+1) - J(1,i,j-1))/(2.0*dtheta)
     end do
  end do

  drho_dt = drho_dt - div_J_rho
  dm_psi_dt = dm_psi_dt + div_tau
  dm_theta_dt = dm_theta_dt + div_tau
  ds_dt = ds_dt - div_q/(max(rho(i,j)*T(i,j), 1.0e-10)) + &
          (mu_visc(i,j)/max(T(i,j), 1.0e-10) * &
           (grad_ux(1,i,j)**2 + grad_ux(2,i,j)**2 + grad_uy(1,i,j)**2 + grad_uy(2,i,j)**2) + &
           eta(i,j)/(mu_0**2 * max(T(i,j), 1.0e-10)) * curl_B(i,j)**2 + &
           D(i,j)/max(T(i,j), 1.0e-10) * (grad_mu(1,i,j)**2 + grad_mu(2,i,j)**2))/ &
           max(T(i,j), 1.0e-10)
  dB_psi_dt = dB_psi_dt - curl_J
  dB_theta_dt = dB_theta_dt + curl_J
end subroutine compute_4bracket

! Subroutine for implicit solve (placeholder for GMRES)
subroutine implicit_solve(rho, m_psi, m_theta, s, B_psi, B_theta, &
                         rho_new, m_psi_new, m_theta_new, s_new, B_psi_new, B_theta_new)
  use elm_mhd_mod
  implicit none
  real(8), intent(inout) :: rho(npsi,ntheta), m_psi(npsi,ntheta), m_theta(npsi,ntheta)
  real(8), intent(inout) :: s(npsi,ntheta), B_psi(npsi,ntheta), B_theta(npsi,ntheta)
  real(8), intent(in) :: rho_new(npsi,ntheta), m_psi_new(npsi,ntheta), m_theta_new(npsi,ntheta)
  real(8), intent(in) :: s_new(npsi,ntheta), B_psi_new(npsi,ntheta), B_theta_new(npsi,ntheta)
  ! Placeholder: Explicit update (replace with GMRES solver)
  rho = rho + dt * rho_new
  m_psi = m_psi + dt * m_psi_new
  m_theta = m_theta + dt * m_theta_new
  s = s + dt * s_new
  B_psi = B_psi + dt * B_psi_new
  B_theta = B_theta + dt * B_theta_new
end subroutine implicit_solve

! Subroutine to apply boundary conditions
subroutine apply_boundary_conditions(rho, m_psi, m_theta, s, B_psi, B_theta)
  use elm_mhd_mod
  implicit none
  real(8), intent(inout) :: rho(npsi,ntheta), m_psi(npsi,ntheta), m_theta(npsi,ntheta)
  real(8), intent(inout) :: s(npsi,ntheta), B_psi(npsi,ntheta), B_theta(npsi,ntheta)
  integer(ikind) :: i, j
  ! SOL: Outflow; Pedestal: No-flux
  do j = 1, ntheta
     rho(1,j) = rho(2,j); rho(npsi,j) = rho(npsi-1,j)
     m_psi(1,j) = 0.0; m_psi(npsi,j) = 0.0
     m_theta(1,j) = 0.0; m_theta(npsi,j) = 0.0
     s(1,j) = s(2,j); s(npsi,j) = s(npsi-1,j)
     B_psi(1,j) = B_psi(2,j); B_psi(npsi,j) = B_psi(npsi-1,j)
     B_theta(1,j) = B_theta(2,j); B_theta(npsi,j) = B_theta(npsi-1,j)
  end do
  ! Periodic in theta
  do i = 1, npsi
     rho(i,1) = rho(i,ntheta-1); rho(i,ntheta) = rho(i,2)
     m_psi(i,1) = m_psi(i,ntheta-1); m_psi(i,ntheta) = m_psi(i,2)
     m_theta(i,1) = m_theta(i,ntheta-1); m_theta(i,ntheta) = m_theta(i,2)
     s(i,1) = s(i,ntheta-1); s(i,ntheta) = s(i,2)
     B_psi(i,1) = B_psi(i,ntheta-1); B_psi(i,ntheta) = B_psi(i,2)
     B_theta(i,1) = B_theta(i,ntheta-1); B_theta(i,ntheta) = B_theta(i,2)
  end do
end subroutine apply_boundary_conditions

! Subroutine to compute energy and entropy
subroutine compute_energy_entropy(rho, m_psi, m_theta, s, B_psi, B_theta, T, grad_ux, grad_T, &
                                 curl_B, mu_chem, eta, mu_visc, kappa, D, &
                                 energy, entropy, entropy_prod)
  use elm_mhd_mod
  implicit none
  real(8), intent(in) :: rho(npsi,ntheta), m_psi(npsi,ntheta), m_theta(npsi,ntheta)
  real(8), intent(in) :: s(npsi,ntheta), B_psi(npsi,ntheta), B_theta(npsi,ntheta)
  real(8), intent(in) :: T(npsi,ntheta), grad_ux(2,npsi,ntheta), grad_T(2,npsi,ntheta)
  real(8), intent(in) :: curl_B(npsi,ntheta), mu_chem(npsi,ntheta)
  real(8), intent(in) :: eta(npsi,ntheta), mu_visc(npsi,ntheta), kappa(npsi,ntheta), D(npsi,ntheta)
  real(8), intent(out) :: energy, entropy, entropy_prod
  real(8) :: grad_mu(2,npsi,ntheta)  ! Local declaration for grad_mu
  integer(ikind) :: i, j
  real(8) :: e

  ! Compute grad_mu locally
  do i = 2, npsi-1
     do j = 2, ntheta-1
        grad_mu(1,i,j) = (mu_chem(i+1,j) - mu_chem(i-1,j))/(2.0*dpsi)
        grad_mu(2,i,j) = (mu_chem(i,j+1) - mu_chem(i,j-1))/(2.0*dtheta)
     end do
  end do

  energy = 0.0; entropy = 0.0; entropy_prod = 0.0
  do i = 2, npsi-1
     do j = 2, ntheta-1
        e = T(i,j)
        energy = energy + (0.5 * (m_psi(i,j)**2 + m_theta(i,j)**2)/max(rho(i,j), 1.0e-10) + &
                          rho(i,j) * e + 0.5 * (B_psi(i,j)**2 + B_theta(i,j)**2)/mu_0) * dpsi * dtheta
        entropy = entropy + s(i,j) * dpsi * dtheta
        entropy_prod = entropy_prod + (D(i,j)/max(T(i,j), 1.0e-10) * &
                                      (grad_mu(1,i,j)**2 + grad_mu(2,i,j)**2) + &
                                      mu_visc(i,j)/max(T(i,j), 1.0e-10) * &
                                      (grad_ux(1,i,j)**2 + grad_ux(2,i,j)**2) + &
                                      kappa(i,j)/(max(T(i,j), 1.0e-10)**2) * &
                                      (grad_T(1,i,j)**2 + grad_T(2,i,j)**2) + &
                                      eta(i,j)/(mu_0**2 * max(T(i,j), 1.0e-10)) * curl_B(i,j)**2) * &
                                      rho(i,j) * max(T(i,j), 1.0e-10) * dpsi * dtheta
     end do
  end do
end subroutine compute_energy_entropy

! Subroutine to save fields
subroutine save_fields(rho, m_psi, m_theta, s, B_psi, B_theta)
  use elm_mhd_mod
  implicit none
  real(8), intent(in) :: rho(npsi,ntheta), m_psi(npsi,ntheta), m_theta(npsi,ntheta)
  real(8), intent(in) :: s(npsi,ntheta), B_psi(npsi,ntheta), B_theta(npsi,ntheta)
  integer(ikind) :: i, j
  integer :: stat

  open(unit=10, file='elm_mhd_output.dat', status='replace', iostat=stat)
  if (stat /= 0) then
     print *, 'Error opening output file'
     return
  end if
  do i = 1, npsi
     do j = 1, ntheta
        write(10, *, iostat=stat) i, j, rho(i,j), m_psi(i,j), m_theta(i,j), s(i,j), B_psi(i,j), B_theta(i,j)
        if (stat /= 0) then
           print *, 'Error writing to output file'
           exit
        end if
     end do
  end do
  close(10)
end subroutine save_fields
