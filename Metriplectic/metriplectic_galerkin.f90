!=======================================================================
! metriplectic_galerkin.f90
! Minimal Galerkin implementation for 1D thermal-fluid metriplectic model
! (Part B requested). Linear (P1) elements, periodic domain.
!
! Assembles:
!   - global consistent mass matrix M (N x N)
!   - global stiffness matrix K (N x N)
!   - block Poisson matrix P (3N x 3N) assembled from weak templates
!   - block Metric matrix G (3N x 3N) assembled from weak templates
!
! Advances using explicit midpoint (RK2) for demonstration.
! For a production, energy-preserving integrator use implicit midpoint + Newton.
!=======================================================================
module Parameters
  implicit none
  integer, parameter :: dp = selected_real_kind(15,307)
  integer, parameter :: Nnodes = 80        ! number of nodes (periodic)
  integer, parameter :: N = Nnodes
  real(dp), parameter :: L = 1.0_dp
  real(dp), parameter :: h = L / N
  real(dp), parameter :: gamma = 1.4_dp
  real(dp), parameter :: Re = 100.0_dp
  real(dp), parameter :: Pr = 1.0_dp
  real(dp), parameter :: CFL = 0.25_dp
  real(dp), parameter :: Tfinal = 0.2_dp
end module Parameters

module FEM_Assembly
  use Parameters
  implicit none
contains

  subroutine assemble_FE_mass_stiff(Nloc, hloc, Mout, Kout)
    ! Assemble consistent linear FE mass and stiffness on a periodic mesh
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: hloc
    real(dp), intent(out) :: Mout(Nloc,Nloc), Kout(Nloc,Nloc)
    integer :: i, ip, im

    Mout = 0.0_dp
    Kout = 0.0_dp

    do i = 1, Nloc
      ip = i + 1
      if (ip > Nloc) ip = 1
      im = i - 1
      if (im < 1) im = Nloc

      ! local element contributions for linear elements (2-node)
      ! mass local: (h/6) * [2 1; 1 2] on each element
      Mout(i,i) = Mout(i,i) + 2.0_dp * hloc / 6.0_dp
      Mout(i,ip) = Mout(i,ip) + 1.0_dp * hloc / 6.0_dp
      Mout(ip,i) = Mout(ip,i) + 1.0_dp * hloc / 6.0_dp
      Mout(ip,ip) = Mout(ip,ip) + 2.0_dp * hloc / 6.0_dp

      ! stiffness local: (1/h) * [1 -1; -1 1]
      Kout(i,i) = Kout(i,i) + 1.0_dp / hloc
      Kout(i,ip) = Kout(i,ip) - 1.0_dp / hloc
      Kout(ip,i) = Kout(ip,i) - 1.0_dp / hloc
      Kout(ip,ip) = Kout(ip,ip) + 1.0_dp / hloc
    end do
  end subroutine assemble_FE_mass_stiff

  subroutine lump_mass_diag(Nloc, M, Mdiag)
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: M(Nloc,Nloc)
    real(dp), intent(out) :: Mdiag(Nloc)
    integer :: i
    Mdiag = 0.0_dp
    do i = 1, Nloc
      Mdiag(i) = sum(M(i,1:Nloc))
      if (Mdiag(i) == 0.0_dp) Mdiag(i) = 1.0_dp   ! safety
    end do
  end subroutine lump_mass_diag

end module FEM_Assembly

module LinearAlgebraHelpers
  use Parameters
  implicit none
contains

  function matvec(A, v, n) result(w)
    integer, intent(in) :: n
    real(dp), intent(in) :: A(n,n), v(n)
    real(dp) :: w(n)
    integer :: i
    w = 0.0_dp
    do i = 1, n
      w(i) = sum(A(i,1:n) * v(1:n))
    end do
  end function matvec

  function matmat(A, B, n, m, p) result(C)
    integer, intent(in) :: n, m, p
    real(dp), intent(in) :: A(n,m), B(m,p)
    real(dp) :: C(n,p)
    integer :: i, j, k
    C = 0.0_dp
    do i = 1, n
      do j = 1, p
        do k = 1, m
          C(i,j) = C(i,j) + A(i,k) * B(k,j)
        end do
      end do
    end do
  end function matmat

  function vec_add(a,b,n) result(c)
    integer, intent(in) :: n
    real(dp), intent(in) :: a(n), b(n)
    real(dp) :: c(n)
    integer :: i
    do i = 1, n
      c(i) = a(i) + b(i)
    end do
  end function vec_add

end module LinearAlgebraHelpers

program metriplectic_galerkin
  use Parameters
  use FEM_Assembly
  use LinearAlgebraHelpers
  implicit none

  ! sizes
  integer, parameter :: NN = N
  integer, parameter :: Mdim = 3*NN

  ! FE matrices
  real(dp), allocatable :: Mass(:,:), K(:,:)
  real(dp), allocatable :: Mdiag(:)       ! lumped mass diag
  real(dp), allocatable :: P(:,:), G(:,:) ! block Poisson and Metric
  real(dp), allocatable :: U(:), dH(:), dS(:), rhs(:)
  real(dp), allocatable :: x(:)
  integer :: i, j, step, nsteps
  real(dp) :: umax, dt, t
  real(dp) :: H0, S0

  ! allocate
  allocate(Mass(NN,NN))
  allocate(K(NN,NN))
  allocate(Mdiag(NN))
  allocate(P(Mdim,Mdim))
  allocate(G(Mdim,Mdim))
  allocate(U(Mdim), dH(Mdim), dS(Mdim), rhs(Mdim))
  allocate(x(NN))

  ! grid nodes (cell centers at nodes for simplicity)
  do i = 1, NN
    x(i) = (i - 0.5_dp) * h
  end do

  ! assemble FE mass and stiffness
  call assemble_FE_mass_stiff(NN, h, Mass, K)
  call lump_mass_diag(NN, Mass, Mdiag)

  ! assemble block operators (weak-form inspired)
  call assemble_block_P_G(NN, Mass, K, P, G)

  ! initialize U: rho = 1 + bump, m = 0, sigma = rho*s (s=0)
  call initialize_U(NN, x, U)

  ! compute initial variational derivatives and diagnostics
  call compute_variational_derivatives(NN, Mass, Mdiag, U, dH, dS)
  H0 = compute_total_H(NN, Mass, U)
  S0 = compute_total_S(NN, Mass, U)
  write(*,*) 'Initial H=', H0, ' S=', S0

  ! estimate dt using lumped mass velocities and local sound speed
  call primitive_from_U(NN, U, Mass, Mdiag, umax)
  if (umax <= 0.0_dp) umax = 1.0_dp
  dt = CFL * h / umax
  if (dt <= 0.0_dp) dt = 1.0e-8_dp
  nsteps = max(1, int(Tfinal / dt))
  write(*,*) 'dt=', dt, ' nsteps=', nsteps

  ! time loop (explicit midpoint RK2 as demonstration)
  t = 0.0_dp
  do step = 1, nsteps
    ! compute rhs1 = P*dH + G*dS at current state
    call compute_variational_derivatives(NN, Mass, Mdiag, U, dH, dS)
    rhs = matvec(P, dH, Mdim) + matvec(G, dS, Mdim)

    ! half-step
    U = U + 0.5_dp * dt * rhs

    ! recompute derivatives at midpoint
    call compute_variational_derivatives(NN, Mass, Mdiag, U, dH, dS)
    rhs = matvec(P, dH, Mdim) + matvec(G, dS, Mdim)

    ! full step
    U = U + dt * rhs

    t = t + dt
    if (mod(step, max(1, nsteps/10)) == 0) then
      write(*,'(A,F8.5)') 'time=', t
      write(*,'(A,ES14.7)') '   H=', compute_total_H(NN, Mass, U)
      write(*,'(A,ES14.7)') '   S=', compute_total_S(NN, Mass, U)
    end if
  end do

  write(*,*) 'Done. Final H,S =', compute_total_H(NN, Mass, U), compute_total_S(NN, Mass, U)

contains

  subroutine assemble_block_P_G(Nn, Mass, K, Pout, Gout)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Mass(Nn,Nn), K(Nn,Nn)
    real(dp), intent(out) :: Pout(3*Nn,3*Nn), Gout(3*Nn,3*Nn)
    integer :: i,j
    real(dp), allocatable :: Minvdiag(:)
    allocate(Minvdiag(Nn))
    ! approximate inverse mass via lumping diag inverse
    do i = 1, Nn
      Minvdiag(i) = 1.0_dp / sum(Mass(i,1:Nn))
    end do

    ! zero
    Pout = 0.0_dp
    Gout = 0.0_dp

    ! Build Poisson block structure in weak form:
    !    [ 0  -D   0
    !      D   0   B
    !      0  -B^T 0 ]
    ! We use D = Minv * K (weak derivative-like mapping) and B = K (toy coupling)
    do i = 1, Nn
      do j = 1, Nn
        Pout(i, Nn + j) = - Minvdiag(i) * K(i,j)
        Pout(Nn + j, i) =   Minvdiag(j) * K(j,i)
        Pout(Nn + i, 2*Nn + j) = K(i,j)
        Pout(2*Nn + j, Nn + i) = -K(j,i)
      end do
    end do

    ! Metric: symmetric positive semidefinite blocks on momentum and sigma
    do i = 1, Nn
      do j = 1, Nn
        Gout(Nn + i, Nn + j) = (1.0_dp / Re) * K(i,j)         ! viscous metric
        Gout(2*Nn + i, 2*Nn + j) = (1.0_dp / (Re*Pr)) * K(i,j) ! thermal metric
      end do
    end do

    deallocate(Minvdiag)
  end subroutine assemble_block_P_G

  subroutine initialize_U(Nn, xnodes, Uout)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: xnodes(Nn)
    real(dp), intent(out) :: Uout(3*Nn)
    integer :: ii
    real(dp) :: rho0, s0
    do ii = 1, Nn
      rho0 = 1.0_dp + 0.1_dp * exp(-50.0_dp * (xnodes(ii) - 0.5_dp)**2)
      s0 = 0.0_dp
      Uout(ii) = rho0
      Uout(Nn + ii) = 0.0_dp
      Uout(2*Nn + ii) = rho0 * s0
    end do
  end subroutine initialize_U

  subroutine compute_variational_derivatives(Nn, Mass, Mdiag, Uin, dHout, dSout)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Mass(Nn,Nn), Mdiag(Nn)
    real(dp), intent(in) :: Uin(3*Nn)
    real(dp), intent(out) :: dHout(3*Nn), dSout(3*Nn)
    integer :: ii
    real(dp), allocatable :: rho(:), m(:), sigma(:), vel(:), s(:), T(:), p(:)
    allocate(rho(Nn), m(Nn), sigma(Nn), vel(Nn), s(Nn), T(Nn), p(Nn))
    do ii = 1, Nn
      rho(ii) = Uin(ii)
      m(ii)   = Uin(Nn + ii)
      sigma(ii) = Uin(2*Nn + ii)
      if (rho(ii) <= 1.0e-14_dp) rho(ii) = 1.0e-14_dp
      vel(ii) = m(ii) / rho(ii)
      s(ii) = sigma(ii) / rho(ii)
      T(ii) = rho(ii)**(gamma-1) * exp( (gamma-1) * s(ii) )
      p(ii) = rho(ii) * T(ii)
    end do

    ! Variational derivatives in nodal (coefficient) form.
    ! For a Galerkin scheme we would map dH/dU (functional derivatives)
    ! into coefficient space using the mass matrix inverse. Here we
    ! compute nodal values of functional derivatives and (for compactness)
    ! use the lumped mass diag to form approximate coefficient values.
    do ii = 1, Nn
      ! specific internal energy per paper scaled form
      dHout(ii) = 0.5_dp * vel(ii)**2 + ( rho(ii)**(gamma-1) / (gamma-1) ) * exp( (gamma-1) * s(ii) )
      dHout(Nn + ii) = vel(ii)
      dHout(2*Nn + ii) = 0.0_dp

      dSout(ii) = -gamma + s(ii)
      dSout(Nn + ii) = vel(ii)
      dSout(2*Nn + ii) = 0.0_dp

      ! If mapping through mass matrix is desired, one would solve M * alpha = dHnodal
      ! and use alpha as coefficients. To keep this contained we approximate that here
      ! by dividing by lumped mass diag to get coefficient-like values:
      if (Mdiag(ii) > 0.0_dp) then
        dHout(ii) = dHout(ii) / Mdiag(ii)
        dHout(Nn + ii) = dHout(Nn + ii) / Mdiag(ii)
        dHout(2*Nn + ii) = dHout(2*Nn + ii) / Mdiag(ii)
        dSout(ii) = dSout(ii) / Mdiag(ii)
        dSout(Nn + ii) = dSout(Nn + ii) / Mdiag(ii)
        dSout(2*Nn + ii) = dSout(2*Nn + ii) / Mdiag(ii)
      end if
    end do

    deallocate(rho, m, sigma, vel, s, T, p)
  end subroutine compute_variational_derivatives

  function compute_total_H(Nn, Mass, Uin) result(H)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Mass(Nn,Nn)
    real(dp), intent(in) :: Uin(3*Nn)
    real(dp) :: H
    integer :: ii
    real(dp), allocatable :: rho(:), m(:), sigma(:), s(:)
    allocate(rho(Nn), m(Nn), sigma(Nn), s(Nn))
    do ii = 1, Nn
      rho(ii) = Uin(ii)
      m(ii) = Uin(Nn + ii)
      sigma(ii) = Uin(2*Nn + ii)
      if (rho(ii) <= 1.0e-14_dp) rho(ii) = 1.0e-14_dp
      s(ii) = sigma(ii) / rho(ii)
    end do
    H = 0.0_dp
    do ii = 1, Nn
      H = H + 0.5_dp * m(ii)**2 / rho(ii) + rho(ii) * ( rho(ii)**(gamma-1) / (gamma-1) * exp((gamma-1)*s(ii)) )
    end do
    H = H * h
    deallocate(rho, m, sigma, s)
  end function compute_total_H

  function compute_total_S(Nn, Mass, Uin) result(S)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Mass(Nn,Nn)
    real(dp), intent(in) :: Uin(3*Nn)
    real(dp) :: S
    integer :: ii
    S = 0.0_dp
    do ii = 1, Nn
      S = S + Uin(2*Nn + ii)
    end do
    S = S * h
  end function compute_total_S

  subroutine primitive_from_U(Nn, Uin, Mass, Mdiag, umax)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Uin(3*Nn)
    real(dp), intent(in) :: Mass(Nn,Nn), Mdiag(Nn)
    real(dp), intent(out) :: umax
    integer :: ii
    real(dp) :: localc
    umax = 0.0_dp
    do ii = 1, Nn
      if (Uin(ii) <= 0.0_dp) then
        localc = 1.0_dp
      else
        localc = sqrt(gamma * Uin(ii)**(gamma-1))
      end if
      umax = max(umax, abs(Uin(Nn + ii) / max(1.0e-14_dp, Uin(ii))) + localc)
    end do
  end subroutine primitive_from_U

end program metriplectic_galerkin
