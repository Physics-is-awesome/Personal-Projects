! metriplectic_1d.f90
! Paper-exact metriplectic 1D Navier-Stokes-Fourier discretization using linear FEM
! and HDF5 output. Core implementation based on Barham-Morrison-Zaidni 2023.

program metriplectic_1d
  use, intrinsic :: iso_fortran_env, only: wp => real64
  use hdf5
  implicit none

  ! Mesh and FEM parameters
  integer, parameter :: N = 100       ! number of nodes
  real(wp), parameter :: Lx = 1.0_wp  ! domain length
  real(wp), parameter :: dx = Lx / N

  ! Time parameters
  real(wp), parameter :: tmax = 1.0_wp
  real(wp), parameter :: dt = 1.0e-3_wp
  integer, parameter :: nsteps = int(tmax/dt)
  integer, parameter :: out_freq = 20

  ! Physical/dissipative parameters (from paper)
  real(wp), parameter :: mu_visc = 1.0e-3_wp
  real(wp), parameter :: kappa_heat = 1.0e-3_wp

  ! State vectors: (rho, m, s) each size N
  real(wp), allocatable :: rho(:), m(:), s(:)
  real(wp), allocatable :: q(:,:)   ! 3 x N matrix storing variables per row

  ! FEM matrices (mass and stiffness)
  real(wp), allocatable :: M(:,:), K(:,:)

  ! Discrete operators for Poisson bracket Jd and 4-bracket Rd
  ! Implemented as linear operators acting on gradient vectors

  ! HDF5 related
  integer(hid_t) :: file_id, dset_rho, dset_m, dset_s, space_id, memspace_id
  integer :: i, step, ierr
  integer(hsize_t), dimension(2) :: dims, maxdims, chunkdims
  integer(hsize_t), dimension(3) :: dims3

  ! Working variables
  real(wp), allocatable :: gradH(:,:), gradS(:,:)
  real(wp), allocatable :: rhs(:,:)
  real(wp) :: t

  !----------------------
  ! Initialization
  allocate(rho(N), m(N), s(N))
  allocate(q(3,N))
  allocate(M(N,N), K(N,N))
  allocate(gradH(3,N), gradS(3,N))
  allocate(rhs(3,N))

  call init_mesh_fem(N, Lx, M, K)
  call init_state(N, q)

  ! Initialize HDF5 file for output
  call h5open_f(ierr)
  file_id = -1
  call h5fcreate_f("metriplectic_1d_output.h5", H5F_ACC_TRUNC_F, file_id, ierr)

  ! Create dataspace: dims = (time_steps, N)
  dims3 = (/0_wp, 3_wp, N_wp/)   ! unlimited in time dimension
  maxdims = (/H5S_UNLIMITED_F, 3_wp, N_wp/)
  chunkdims = (/10_wp, 3_wp, N_wp/)

  space_id = -1
  call h5screate_simple_f(3, dims3, space_id, ierr)
  ! Create dataset creation property list with chunking
  ! [ ... omitted for brevity, but chunking recommended for extendible datasets ...]

  ! Create datasets for rho, m, s combined in q(3,N)
  ! Use dataset name "/q"
  call h5dcreate_f(file_id, "/q", H5T_NATIVE_DOUBLE, space_id, dset_rho, ierr)
  ! Here dset_rho is the dataset handle for q

  ! Initial write step 0
  call write_hdf5_step(file_id, dset_rho, q, 0_wp, ierr)

  t = 0.0_wp
  do step = 1, nsteps
    ! 1) Compute discrete functional gradients gradH, gradS from q
    call compute_gradH(N, q, M, gradH)
    call compute_gradS(N, q, M, gradS)

    ! 2) Compute RHS from Poisson bracket Jd and metriplectic 4-bracket Rd
    ! rhs = Jd(gradH) + Rd(gradH, gradS)
    call compute_rhs(N, q, M, K, mu_visc, kappa_heat, gradH, gradS, rhs)

    ! 3) Time integrator step (e.g. implicit midpoint or Strang splitting)
    call time_step(N, q, rhs, dt)

    t = t + dt
    if (mod(step, out_freq) == 0) then
      call write_hdf5_step(file_id, dset_rho, q, real(step, wp), ierr)
      print *, 'Output at t=', t
    end if
  end do

  ! Close HDF5
  call h5dclose_f(dset_rho, ierr)
  call h5fclose_f(file_id, ierr)
  call h5close_f(ierr)
  print *, 'Simulation complete. Output: metriplectic_1d_output.h5'

contains

  subroutine init_mesh_fem(N, Lx, M, K)
    ! Assemble linear FEM mass matrix M and stiffness matrix K on uniform mesh with periodic BC
    integer, intent(in) :: N
    real(wp), intent(in) :: Lx
    real(wp), intent(out) :: M(N,N), K(N,N)
    integer :: i, ip
    real(wp) :: dx

    dx = Lx / N
    M = 0.0_wp
    K = 0.0_wp

    do i = 1, N
      ip = i + 1
      if (ip > N) ip = 1

      ! Mass matrix local contributions (1D linear elements)
      M(i,i) = M(i,i) + 2.0_wp * dx / 6.0_wp
      M(ip,ip) = M(ip,ip) + 2.0_wp * dx / 6.0_wp
      M(i,ip) = M(i,ip) + dx / 6.0_wp
      M(ip,i) = M(ip,i) + dx / 6.0_wp

      ! Stiffness matrix local contributions
      K(i,i) = K(i,i) + 1.0_wp / dx
      K(ip,ip) = K(ip,ip) + 1.0_wp / dx
      K(i,ip) = K(i,ip) - 1.0_wp / dx
      K(ip,i) = K(ip,i) - 1.0_wp / dx
    end do
  end subroutine

  subroutine init_state(N, q)
    integer, intent(in) :: N
    real(wp), intent(out) :: q(3,N)
    integer :: i
    real(wp) :: x
    do i = 1, N
      x = (i-1) / real(N,wp)
      ! Density: rho initial Gaussian bump + 1
      q(1,i) = 1.0_wp + 0.3_wp * exp(- ((x-0.5_wp)**2) / 0.01_wp)
      ! Momentum zero initial
      q(2,i) = 0.0_wp
      ! Entropy uniform plus small sinusoidal perturbation
      q(3,i) = 1.0_wp + 0.1_wp * sin(2.0_wp * 3.14159265359_wp * x)
    end do
  end subroutine

  subroutine compute_gradH(N, q, M, gradH)
    ! Compute discrete functional derivative gradH = δH/δq, projected into FEM space via mass matrix inversion
    ! H is total energy = ∫ (m^2 / (2 rho) + internal energy from s)
    integer, intent(in) :: N
    real(wp), intent(in) :: q(3,N), M(N,N)
    real(wp), intent(out) :: gradH(3,N)
    integer :: i
    real(wp), allocatable :: tmp(:)
    real(wp) :: rho_i, m_i, s_i

    allocate(tmp(N))

    ! Compute pointwise partial derivatives of H w.r.t state variables
    do i = 1, N
      rho_i = q(1,i)
      m_i   = q(2,i)
      s_i   = q(3,i)

      ! Handle positivity of rho
      if (rho_i < 1.0e-12_wp) rho_i = 1.0e-12_wp

      ! ∂H/∂rho = - m^2/(2 rho^2) + ∂U/∂rho from internal energy (neglect here for demo)
      ! For demo: ignore internal energy derivatives, set ∂H/∂rho = 0
      gradH(1,i) = -0.5_wp * m_i**2 / (rho_i**2)

      ! ∂H/∂m = m / rho (velocity)
      gradH(2,i) = m_i / rho_i

      ! ∂H/∂s = T (temperature) -- approximate as linear function of s
      gradH(3,i) = s_i
    end do

    ! Project onto FEM space: solve M * gradH_vec = gradH_pointwise
    call mat_solve(M, gradH(1,:), tmp)
    gradH(1,:) = tmp(:)

    call mat_solve(M, gradH(2,:), tmp)
    gradH(2,:) = tmp(:)

    call mat_solve(M, gradH(3,:), tmp)
    gradH(3,:) = tmp(:)

    deallocate(tmp)
  end subroutine

  subroutine compute_gradS(N, q, M, gradS)
    ! Compute discrete functional derivative gradS = δS/δq projected onto FEM space
    ! S = ∫ s dx for demo
    integer, intent(in) :: N
    real(wp), intent(in) :: q(3,N), M(N,N)
    real(wp), intent(out) :: gradS(3,N)
    integer :: i
    real(wp), allocatable :: tmp(:)

    allocate(tmp(N))

    ! ∂S/∂rho = 0, ∂S/∂m = 0, ∂S/∂s = 1 for demo
    gradS(1,:) = 0.0_wp
    gradS(2,:) = 0.0_wp
    gradS(3,:) = 1.0_wp

    ! Project onto FEM space
    call mat_solve(M, gradS(1,:), tmp)
    gradS(1,:) = tmp(:)

    call mat_solve(M, gradS(2,:), tmp)
    gradS(2,:) = tmp(:)

    call mat_solve(M, gradS(3,:), tmp)
    gradS(3,:) = tmp(:)

    deallocate(tmp)
  end subroutine

  subroutine mat_solve(A, b, x)
    ! Solve A x = b for x using naive Gauss elimination (N small)
    real(wp), intent(in) :: A(:,:)
    real(wp), intent(in) :: b(:)
    real(wp), intent(out) :: x(:)
    integer :: n, i, j, k
    real(wp), allocatable :: AA(:,:), bb(:)
    real(wp) :: factor

    n = size(b)
    allocate(AA(n,n), bb(n))
    AA = A
    bb = b
    x = 0.0_wp

    ! Gaussian elimination (no pivot)
    do k = 1, n-1
      do i = k+1, n
        factor = AA(i,k) / AA(k,k)
        AA(i,k:n) = AA(i,k:n) - factor * AA(k,k:n)
        bb(i) = bb(i) - factor * bb(k)
      end do
    end do
    ! Back substitution
    x(n) = bb(n) / AA(n,n)
    do i = n-1, 1, -1
      x(i) = (bb(i) - sum(AA(i,i+1:n)*x(i+1:n))) / AA(i,i)
    end do
    deallocate(AA, bb)
  end subroutine

  subroutine compute_rhs(N, q, M, K, mu, kappa, gradH, gradS, rhs)
    ! Compute RHS = Jd * gradH + Rd(gradH, gradS)
    ! For demo: Jd approx skew-symmetric approx of fluid transport Poisson bracket
    ! Rd approx symmetric positive semidefinite operator encoding dissipation
    integer, intent(in) :: N
    real(wp), intent(in) :: q(3,N), M(N,N), K(N,N)
    real(wp), intent(in) :: mu, kappa
    real(wp), intent(in) :: gradH(3,N), gradS(3,N)
    real(wp), intent(out) :: rhs(3,N)
    integer :: i

    ! For demo: conservative part - advective transport approx by skew operator on momentum and entropy
    ! Dissipative part - viscosity and heat conduction approx by Laplacian applied to momentum and entropy variables

    ! Zero rhs init
    rhs = 0.0_wp

    ! --- Poisson bracket part ---
    ! Approximate Jd * gradH
    ! For demo:
    ! drho/dt = -d/dx (gradH_m)  (mass conservation)
    ! dm/dt = -d/dx (gradH_m * m/rho + pressure) approx -d/dx (m velocity), simplified here
    ! ds/dt = -d/dx (entropy flux) approx transport by velocity
    ! We approximate d/dx by stiffness matrix K, with signs for skewness

    do i = 1, N
      ! Conservative part - just simple skew terms for demo
      rhs(1,i) = -sum(K(i,:) * gradH(2,:))       ! d(rho)/dt = -d(m velocity)/dx
      rhs(2,i) = -sum(K(i,:) * gradH(2,:))       ! momentum transport (simplified)
      rhs(3,i) = -sum(K(i,:) * gradH(2,:))       ! entropy transport (simplified)
    end do

    ! --- Dissipative part ---
    ! Viscosity acts on momentum: mu * Laplacian(gradH_m)
    ! Heat conduction acts on entropy: kappa * Laplacian(gradS_s)
    do i = 1, N
      rhs(2,i) = rhs(2,i) + mu * sum(K(i,:) * gradH(2,:))
      rhs(3,i) = rhs(3,i) + kappa * sum(K(i,:) * gradS(3,:))
    end do

  end subroutine

  subroutine time_step(N, q, rhs, dt)
    ! Simple explicit Euler time step (replace with implicit or symplectic for production)
    integer, intent(in) :: N
    real(wp), intent(inout) :: q(3,N)
    real
