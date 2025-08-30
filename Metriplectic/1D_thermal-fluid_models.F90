!--------------------------------------------------------------
! Fortran90 code for metriplectic 1D thermal-fluid model
!--------------------------------------------------------------

program metriplectic_1d_full
  implicit none
  !----------------------------------------------------------------------
  ! Parameters / configuration
  !----------------------------------------------------------------------
  integer, parameter :: dp = selected_real_kind(15, 307)
  integer, parameter :: N = 200          ! number of grid points
  real(dp), parameter :: L = 1.0_dp      ! domain length [0,L) (periodic)
  real(dp), parameter :: dx = L / N
  real(dp), parameter :: gamma = 1.4_dp
  real(dp), parameter :: Re = 100.0_dp   ! Reynolds number (1/viscosity scale)
  real(dp), parameter :: Pr = 1.0_dp     ! Prandtl number
  real(dp), parameter :: Tfinal = 0.5_dp
  real(dp), parameter :: CFL = 0.25_dp
  integer, parameter :: max_picard = 30
  real(dp), parameter :: picard_tol = 1.0e-10_dp
  !----------------------------------------------------------------------
  ! State arrays
  !----------------------------------------------------------------------
  real(dp), allocatable :: x(:)
  real(dp), allocatable :: U(:,:), Unew(:,:), Umid(:,:), rhs(:,:)
  real(dp), allocatable :: D(:,:), D2(:,:)
  real(dp), allocatable :: rho(:), m(:), sigma(:), vel(:), s(:), T(:), p(:)
  integer :: i, it, step, nsteps
  real(dp) :: t, umax, dt
  real(dp) :: time_out
  ! Diagnostics
  real(dp) :: Htot, S_tot
  integer :: picard_iter
  ! I/O
  integer :: ios

  !----------------------------------------------------------------------
  ! Allocate
  !----------------------------------------------------------------------
  allocate(x(N))
  allocate(U(3,N), Unew(3,N), Umid(3,N), rhs(3,N))
  allocate(D(N,N), D2(N,N))
  allocate(rho(N), m(N), sigma(N), vel(N), s(N), T(N), p(N))

  !----------------------------------------------------------------------
  ! Grid and derivative operators (periodic)
  !----------------------------------------------------------------------
  do i = 1, N
    x(i) = (i - 0.5_dp) * dx
  end do

  call build_D_matrices(N, dx, D, D2)

  !----------------------------------------------------------------------
  ! Initial condition: smooth perturbation around rho=1, u=0, s=0
  !----------------------------------------------------------------------
  do i = 1, N
    rho(i) = 1.0_dp + 0.1_dp * exp(-50.0_dp*( (x(i)-0.5_dp*L)**2 ))
    vel(i) = 0.0_dp
    s(i) = 0.0_dp
    m(i) = rho(i) * vel(i)
    sigma(i) = rho(i) * s(i)
    U(1,i) = rho(i)
    U(2,i) = m(i)
    U(3,i) = sigma(i)
  end do

  !----------------------------------------------------------------------
  ! Time stepping setup (estimate time step from CFL on velocity + sound)
  ! Sound speed for chosen nondim model ~ sqrt(T * gamma?) We'll use local estimate
  !----------------------------------------------------------------------
  call thermodynamics_from_state(U, gamma, rho, vel, s, T, p)
  umax = maxval(abs(vel) + sqrt(gamma * T))   ! conservative estimate
  if (umax <= 0.0_dp) umax = 1.0_dp
  dt = CFL * dx / umax
  if (dt <= 0.0_dp) dt = 1.0e-6_dp
  nsteps = int( max(1.0_dp, Tfinal / dt) )
  write(*,*) "N=", N, " dx=", dx, " dt=", dt, " nsteps ~", nsteps

  open(unit=20, file='energy.dat', status='replace')
  open(unit=21, file='entropy.dat', status='replace')
  write(20,'(A)') "# t H_tot"
  write(21,'(A)') "# t S_tot"

  t = 0.0_dp
  step = 0

  do while (t < Tfinal .and. step < nsteps)
    step = step + 1
    ! -----------------------------------------------------------------
    ! Compute RHS at midpoint via implicit midpoint (Picard iteration)
    ! U_mid satisfies: U_mid = U + (dt/2) * RHS(U_mid)
    ! Solve by fixed-point (Picard): iterate RHS(U_mid_old) -> update U_mid
    ! -----------------------------------------------------------------
    Umid = U   ! initial guess
    picard_iter = 0
    do
      picard_iter = picard_iter + 1
      call compute_rhs(Umid, gamma, Re, Pr, D, D2, rhs)
      Umid = U + 0.5_dp * dt * rhs
      ! compute new residual to test convergence (norm of change)
      if (picard_iter >= max_picard) exit
      if (maxval(abs(0.5_dp * dt * rhs)) < picard_tol) exit
    end do
    ! Now evaluate RHS at Umid for full step
    call compute_rhs(Umid, gamma, Re, Pr, D, D2, rhs)
    Unew = U + dt * rhs

    ! Update for next step
    U = Unew
    t = t + dt

    ! diagnostics every so often
    if (mod(step,10) == 0 .or. t >= Tfinal) then
      call thermodynamics_from_state(U, gamma, rho, vel, s, T, p)
      Htot = compute_total_H(rho, m, sigma, gamma)
      S_tot = sum(sigma) * dx
      write(20,'(F12.6,2X,ES16.9)') t, Htot
      write(21,'(F12.6,2X,ES16.9)') t, S_tot
      write(*,'(A,F8.4,A,I4,A,I3)') "t=", t, " step=", step, " picard_iter=", picard_iter
    end if
  end do

  ! Final write-out
  call thermodynamics_from_state(U, gamma, rho, vel, s, T, p)
  open(unit=30, file='solution.dat', status='replace')
  write(30,'(A)') "# x rho vel p"
  do i = 1, N
    write(30,'(F8.5,3X,3E12.5)') x(i), rho(i), vel(i), p(i)
  end do
  close(30)
  close(20)
  close(21)

  write(*,*) "Finished. Output: solution.dat, energy.dat, entropy.dat"

contains

  !--------------------------------------------------------------------
  subroutine build_D_matrices(N, dx, D, D2)
    implicit none
    integer, intent(in) :: N
    real(dp), intent(in) :: dx
    real(dp), intent(out) :: D(N,N), D2(N,N)
    integer :: i, j, ip, im

    D = 0.0_dp
    D2 = 0.0_dp
    ! 1st derivative central difference (periodic), skew-symmetric
    do i = 1, N
      ip = i + 1
      if (ip > N) ip = 1
      im = i - 1
      if (im < 1) im = N
      D(i,ip) =  0.5_dp / dx
      D(i,im) = -0.5_dp / dx
    end do
    ! 2nd derivative (periodic)
    do i = 1, N
      ip = i + 1
      if (ip > N) ip = 1
      im = i - 1
      if (im < 1) im = N
      D2(i,i) = -2.0_dp / (dx*dx)
      D2(i,ip) = 1.0_dp / (dx*dx)
      D2(i,im) = 1.0_dp / (dx*dx)
    end do
  end subroutine build_D_matrices

  !--------------------------------------------------------------------
  subroutine thermodynamics_from_state(U, gamma, rho, vel, s, T, p)
    implicit none
    real(dp), intent(in) :: U(3,:), gamma
    real(dp), intent(out) :: rho(:), vel(:), s(:), T(:), p(:)
    integer :: i, Nloc
    real(dp) :: sigma_loc
    Nloc = size(rho)
    do i = 1, Nloc
      rho(i) = U(1,i)
      if (rho(i) <= 1.0e-12_dp) rho(i) = 1.0e-12_dp
      vel(i) = U(2,i) / rho(i)
      sigma_loc = U(3,i)
      s(i) = sigma_loc / rho(i)
      ! From paper's nondimensional ideal gas thermodynamics:
      ! T = rho^(gamma-1) * exp((gamma-1) * s)
      T(i) = rho(i)**(gamma-1) * exp( (gamma-1) * s(i) )
      p(i) = rho(i) * T(i)
    end do
  end subroutine thermodynamics_from_state

  !--------------------------------------------------------------------
  subroutine compute_rhs(Uin, gamma, Re, Pr, D, D2, rhs)
    implicit none
    real(dp), intent(in) :: Uin(3,:), gamma, Re, Pr
    real(dp), intent(in) :: D(:,:), D2(:,:)
    real(dp), intent(out) :: rhs(3,:)
    integer :: Nloc, i, j
    real(dp), allocatable :: rho(:), m(:), sigma(:), vel(:), s(:), T(:), p(:)
    real(dp), allocatable :: du_dx(:), d2u_dx2(:), dT_dx(:), d2T_dx2(:)
    real(dp) :: tmp

    Nloc = size(Uin,2)
    allocate(rho(Nloc), m(Nloc), sigma(Nloc), vel(Nloc), s(Nloc), T(Nloc), p(Nloc))
    allocate(du_dx(Nloc), d2u_dx2(Nloc), dT_dx(Nloc), d2T_dx2(Nloc))

    ! primitive thermodynamic fields
    call thermodynamics_from_state(Uin, gamma, rho, vel, s, T, p)
    do i = 1, Nloc
      m(i) = Uin(2,i)
      sigma(i) = Uin(3,i)
    end do

    ! Compute spatial derivatives via matrices D and D2:
    du_dx = matvec(D, vel)
    d2u_dx2 = matvec(D2, vel)
    dT_dx = matvec(D, T)
    d2T_dx2 = matvec(D2, T)

    ! Continuity: drho/dt = - ∂x m
    rhs(1,:) = - matvec(D, m)

    ! Momentum: dm/dt = - ∂x (m*u) - ∂x p + ∂x( (1/Re) ∂x u )
    ! Discretize nonconservative term carefully: compute advective flux m*u elementwise
    do i = 1, Nloc
      tmp = m(i) * vel(i)
      ! use central D applied to flux array
    end do
    rhs(2,:) = -matvec(D, m*vel) - matvec(D, p) + (1.0_dp/Re) * matvec(D2, vel)

    ! Entropy density σ evolution:
    ! dσ/dt = - ∂x(σ u) + (1/Re) * (1/T) * (∂x u)^2 + ∂x( (1/(Re*Pr)) * (1/T) ∂x T ) + (1/(Re*Pr))*(1/T^2)*(∂x T)^2
    ! (this follows nondimensional eq. (15) and weak forms, see paper). :contentReference[oaicite:7]{index=7}
    do i = 1, Nloc
      ! pointwise production terms
      rhs(3,i) = - matvec(D, sigma*vel)(i)
      rhs(3,i) = rhs(3,i) + (1.0_dp/Re) * (1.0_dp / T(i)) * (du_dx(i)**2)
    end do
    ! thermal diffusion: ∂x( (1/(Re*Pr)) * 1/T * ∂x T ) implemented as D applied to flux
    do i = 1, Nloc
      ! compute flux f = (1/(Re*Pr))*(1/T)*dT_dx
    end do
    ! build flux array and apply D:
    rhs(3,:) = rhs(3,:) + matvec(D, (1.0_dp/(Re*Pr)) * (dT_dx / T) )
    ! additional nonlinear correction (1/(Re*Pr))*(1/T^2)*(dT_dx)^2
    rhs(3,:) = rhs(3,:) + (1.0_dp/(Re*Pr)) * ( (dT_dx**2) / (T**2) )

    deallocate(rho, m, sigma, vel, s, T, p, du_dx, d2u_dx2, dT_dx, d2T_dx2)
  end subroutine compute_rhs

  !--------------------------------------------------------------------
  pure function matvec(A, v) result(w)
    implicit none
    real(dp), intent(in) :: A(:,:), v(:)
    real(dp) :: w(size(v))
    integer :: i, j, n
    n = size(v)
    w = 0.0_dp
    do i = 1, n
      w(i) = sum(A(i,1:n) * v(1:n))
    end do
  end function matvec

  !--------------------------------------------------------------------
  function compute_total_H(rho, m, sigma, gamma) result(H)
    implicit none
    real(dp), intent(in) :: rho(:), m(:), sigma(:), gamma
    integer :: n, i
    real(dp) :: H, s_i, Uval
    n = size(rho)
    H = 0.0_dp
    do i = 1, n
      s_i = sigma(i) / rho(i)
      ! Specific internal energy U( rho, s ) = rho^(gamma-1)/(gamma-1) * exp((gamma-1)*s)
      Uval = rho(i)**(gamma-1) / (gamma-1) * exp( (gamma-1) * s_i )
      H = H + (0.5_dp * m(i)**2 / rho(i) + rho(i) * Uval)
    end do
    H = H * dx
  end function compute_total_H

end program metriplectic_1d_full
