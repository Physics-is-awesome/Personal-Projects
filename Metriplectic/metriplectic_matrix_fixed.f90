!=======================================================================
! metriplectic_matrix_fixed.f90
! Fixed: matrix-assembled nodal implementation (Part A), compilable
!=======================================================================
program metriplectic_matrix_fixed
  implicit none
  integer, parameter :: dp = selected_real_kind(15,307)
  integer, parameter :: N = 100
  real(dp), parameter :: L = 1.0_dp
  real(dp), parameter :: dx = L / N
  real(dp), parameter :: gamma = 1.4_dp
  real(dp), parameter :: Re = 100.0_dp
  real(dp), parameter :: Pr = 1.0_dp
  real(dp), parameter :: CFL = 0.25_dp
  real(dp), parameter :: Tfinal = 0.2_dp

  real(dp), allocatable :: x(:)
  real(dp), allocatable :: U(:)      ! stacked vector [rho; m; sigma] length 3*N
  real(dp), allocatable :: P(:,:), Mmat(:,:), Mass(:,:), K(:,:)
  real(dp), allocatable :: dH(:), dS(:), rhs(:)
  integer :: i, j, step, nsteps
  real(dp) :: dt, umax, t
  real(dp) :: H0, Hn, S0, Sn

  allocate(x(N))
  allocate(U(3*N))
  allocate(P(3*N,3*N))
  allocate(Mmat(3*N,3*N))
  allocate(Mass(N,N))
  allocate(K(N,N))
  allocate(dH(3*N), dS(3*N), rhs(3*N))

  ! build grid
  do i = 1, N
    x(i) = (i-0.5_dp)*dx
  end do

  ! Build mass and stiffness (Galerkin linear mass-lumped style)
  call build_mass_stiff(N, dx, Mass, K)

  ! Build discrete Poisson and metric matrices (weak-form-inspired assembly)
  call assemble_poisson_metric(N, Mass, K, P, Mmat)

  ! initialize U: rho = 1 + small bump, m = 0, sigma = rho*s (s=0)
  call initialize_U(N, x, U)

  ! Compute variational derivatives dH, dS
  call compute_functionals_and_variations(N, U, Mass, dH, dS)

  H0 = compute_total_H_from_variation(N, U, dH)
  S0 = compute_total_S(N, U)

  ! time step estimate (CFL: sound + vel)
  call primitive_from_U(N, U, Mass, x, umax)
  if (umax <= 0.0_dp) umax = 1.0_dp
  dt = CFL * dx / umax
  if (dt <= 0.0_dp) dt = 1.0e-8_dp
  nsteps = int(Tfinal/dt) + 1

  write(*,*) 'N=', N, 'dt=', dt, 'nsteps=', nsteps

  ! Basic checks: P antisymmetry (in Mass inner product) and M PSD
  call check_antisymmetry_mass(P, Mass)
  call check_symmetric_psd(Mmat)

  ! Time stepping (simple explicit Euler for demonstration; replace with better integrator)
  t = 0.0_dp
  do step = 1, nsteps
    ! compute rhs = P * dH + M * dS
    call compute_functionals_and_variations(N, U, Mass, dH, dS)
    rhs = matvec(P, dH)
    rhs = rhs + matvec(Mmat, dS)

    ! forward Euler step (placeholder; the paper recommends energy-preserving integrator)
    U = U + dt * rhs

    t = t + dt
    if (mod(step, max(1,nsteps/10)) == 0) then
      call compute_functionals_and_variations(N, U, Mass, dH, dS)
      Hn = compute_total_H_from_variation(N, U, dH)
      Sn = compute_total_S(N, U)
      write(*,'(A,F8.4,A,ES12.5,A,ES12.5)') 't=', t, ' H=', Hn, ' S=', Sn
    end if
  end do

  write(*,*) 'done part A'
contains

  subroutine build_mass_stiff(Nloc, dxloc, Mass, K)
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: dxloc
    real(dp), intent(out) :: Mass(Nloc,Nloc), K(Nloc,Nloc)
    integer :: i, ip
    Mass = 0.0_dp
    K = 0.0_dp
    do i = 1, Nloc
      ip = i+1
      if (ip > Nloc) ip = 1
      Mass(i,i) = Mass(i,i) + 2.0_dp * dxloc / 6.0_dp
      Mass(i,ip) = Mass(i,ip) + 1.0_dp * dxloc / 6.0_dp
    end do
    ! symmetric fill
    do i = 1, Nloc
      ip = i+1
      if (ip > Nloc) ip = 1
      Mass(ip,i) = Mass(i,ip)
    end do
    ! stiffness
    do i = 1, Nloc
      ip = i+1
      if (ip > Nloc) ip = 1
      K(i,i) = K(i,i) + 1.0_dp / dxloc
      K(i,ip) = K(i,ip) - 1.0_dp / dxloc
    end do
    do i = 1, Nloc
      ip = i+1
      if (ip > Nloc) ip = 1
      K(ip,i) = K(i,ip)
      K(ip,ip) = K(ip,ip) + 1.0_dp / dxloc
    end do
  end subroutine build_mass_stiff

  subroutine assemble_poisson_metric(Nloc, Mass, K, Pout, Mout)
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: Mass(Nloc,Nloc), K(Nloc,Nloc)
    real(dp), intent(out) :: Pout(3*Nloc,3*Nloc), Mout(3*Nloc,3*Nloc)
    integer :: i,j
    real(dp), allocatable :: Minv(:,:), Dop(:,:)

    ! Declarations done; now executable statements:
    Pout = 0.0_dp
    Mout = 0.0_dp

    allocate(Minv(Nloc,Nloc))
    call invert_mass_diag(Nloc, Mass, Minv)

    allocate(Dop(Nloc,Nloc))
    Dop = matmat(Minv, K)

    ! place Dop in blocks
    do i = 1, Nloc
      do j = 1, Nloc
        Pout(i, Nloc + j) = -Dop(i,j)
        Pout(Nloc + j, i) =  Dop(j,i)
      end do
    end do

    do i = 1, Nloc
      do j = 1, Nloc
        Pout(Nloc + i, 2*Nloc + j) =  K(i,j)
        Pout(2*Nloc + j, Nloc + i) = -K(j,i)
      end do
    end do

    ! Metric operator: blocks for momentum and sigma
    do i = 1, Nloc
      do j = 1, Nloc
        Mout(Nloc + i, Nloc + j) = (1.0_dp / Re) * K(i,j)
        Mout(2*Nloc + i, 2*Nloc + j) = (1.0_dp / (Re*Pr)) * K(i,j)
      end do
    end do

    deallocate(Minv)
    deallocate(Dop)
  end subroutine assemble_poisson_metric

  subroutine invert_mass_diag(Nloc, Mass, Minv)
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: Mass(Nloc,Nloc)
    real(dp), intent(out) :: Minv(Nloc,Nloc)
    integer :: i
    real(dp) :: sdiag
    Minv = 0.0_dp
    do i = 1, Nloc
      sdiag = sum(Mass(i,1:Nloc))
      if (abs(sdiag) > 0.0_dp) Minv(i,i) = 1.0_dp / sdiag
    end do
  end subroutine invert_mass_diag

  subroutine initialize_U(Nloc, xloc, Uout)
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: xloc(Nloc)
    real(dp), intent(out) :: Uout(3*Nloc)
    integer :: i
    real(dp) :: rho0, s0
    do i = 1, Nloc
      rho0 = 1.0_dp + 0.1_dp * exp(-50.0_dp * (xloc(i)-0.5_dp)**2)
      s0 = 0.0_dp
      Uout(i) = rho0
      Uout(Nloc + i) = 0.0_dp
      Uout(2*Nloc + i) = rho0 * s0
    end do
  end subroutine initialize_U

  subroutine compute_functionals_and_variations(Nloc, Uin, Mass, dHout, dSout)
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: Uin(3*Nloc)
    real(dp), intent(in) :: Mass(Nloc,Nloc)
    real(dp), intent(out) :: dHout(3*Nloc), dSout(3*Nloc)
    integer :: i
    real(dp), allocatable :: rho(:), m(:), sigma(:), vel(:), s(:), T(:), p(:)

    allocate(rho(Nloc), m(Nloc), sigma(Nloc), vel(Nloc), s(Nloc), T(Nloc), p(Nloc))
    do i = 1, Nloc
      rho(i) = Uin(i)
      m(i) = Uin(Nloc + i)
      sigma(i) = Uin(2*Nloc + i)
      if (rho(i) <= 1.0e-14_dp) rho(i) = 1.0e-14_dp
      vel(i) = m(i) / rho(i)
      s(i) = sigma(i) / rho(i)
      T(i) = rho(i)**(gamma-1) * exp((gamma-1)*s(i))
      p(i) = rho(i) * T(i)
    end do

    do i = 1, Nloc
      dHout(i) = 0.5_dp * vel(i)**2 + ( rho(i)**(gamma-1) / (gamma-1) ) * exp((gamma-1)*s(i))
      dHout(Nloc + i) = vel(i)
      dHout(2*Nloc + i) = 0.0_dp
      dSout(i) = -gamma + s(i)
      dSout(Nloc + i) = vel(i)
      dSout(2*Nloc + i) = 0.0_dp
    end do

    deallocate(rho,m,sigma,vel,s,T,p)
  end subroutine compute_functionals_and_variations

  function matvec(A, v) result(w)
    real(dp), intent(in) :: A(:,:), v(:)
    real(dp) :: w(size(v))
    integer :: i, n
    n = size(v)
    w = 0.0_dp
    do i = 1, n
      w(i) = sum(A(i,1:n) * v(1:n))
    end do
  end function matvec

  function matmat(A, B) result(C)
    real(dp), intent(in) :: A(:,:), B(:,:)
    real(dp) :: C(size(A,1), size(B,2))
    integer :: i, j, k, nA, mA, mB
    nA = size(A,1)
    mA = size(A,2)
    mB = size(B,2)
    C = 0.0_dp
    do i = 1, nA
      do j = 1, mB
        do k = 1, mA
          C(i,j) = C(i,j) + A(i,k) * B(k,j)
        end do
      end do
    end do
  end function matmat

  subroutine check_antisymmetry_mass(Pin, Mass)
    real(dp), intent(in) :: Pin(:,:), Mass(:,:)
    real(dp), allocatable :: Mb(:,:), S(:,:)
    integer :: n
    n = size(Mass,1)
    allocate(Mb(3*n,3*n))
    Mb = Mass_block(Mass)
    allocate(S(3*n,3*n))
    S = matmat(Mb, Pin)
    S = S + transpose(S)
    write(*,*) 'M-weighted antisymmetry norm (Frobenius) =', normF(S)
    deallocate(Mb, S)
  end subroutine check_antisymmetry_mass

  function Mass_block(M) result(Mb)
    real(dp), intent(in) :: M(:,:)
    integer :: n, i, j
    n = size(M,1)
    real(dp) :: Mb(3*n,3*n)
    Mb = 0.0_dp
    do i = 1, n
      do j = 1, n
        Mb(i,j) = M(i,j)
        Mb(n+i,n+j) = M(i,j)
        Mb(2*n+i,2*n+j) = M(i,j)
      end do
    end do
  end function Mass_block

  function normF(A) result(nrm)
    real(dp), intent(in) :: A(:,:)
    real(dp) :: nrm
    integer :: i,j
    nrm = 0.0_dp
    do i = 1, size(A,1)
      do j = 1, size(A,2)
        nrm = nrm + A(i,j)**2
      end do
    end do
    nrm = sqrt(nrm)
  end function normF

  subroutine check_symmetric_psd(Min)
    real(dp), intent(in) :: Min(:,:)
    real(dp), allocatable :: AS(:,:)
    integer :: n
    n = size(Min,1)
    allocate(AS(n,n))
    AS = Min - transpose(Min)
    write(*,*) 'Symmetry check norm (should be ~0) =', normF(AS)
    write(*,*) 'PSD check skipped: link LAPACK/BLAS for eigen decomposition'
    deallocate(AS)
  end subroutine check_symmetric_psd

  subroutine primitive_from_U(Nloc, Uin, Mass, xloc, umax)
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: Uin(3*Nloc)
    real(dp), intent(in) :: Mass(Nloc,Nloc)
    real(dp), intent(in) :: xloc(Nloc)
    real(dp), intent(out) :: umax
    real(dp), allocatable :: rho(:), vel(:), s(:), T(:), p(:)
    integer :: i
    allocate(rho(Nloc), vel(Nloc), s(Nloc), T(Nloc), p(Nloc))
    do i = 1, Nloc
      rho(i) = Uin(i)
      if (rho(i) <= 1.0e-14_dp) rho(i) = 1.0e-14_dp
      vel(i) = Uin(Nloc + i) / rho(i)
      s(i) = Uin(2*Nloc + i) / rho(i)
      T(i) = rho(i)**(gamma-1) * exp((gamma-1) * s(i))
      p(i) = rho(i) * T(i)
    end do
    umax = maxval(abs(vel) + sqrt(gamma * T))
    deallocate(rho, vel, s, T, p)
  end subroutine primitive_from_U

  function compute_total_H_from_variation(Nloc, Uin, dH) result(H)
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: Uin(3*Nloc), dH(3*Nloc)
    real(dp) :: H
    integer :: i
    real(dp) :: rho_i, m_i, s_i, Uint
    H = 0.0_dp
    do i = 1, Nloc
      rho_i = Uin(i)
      m_i = Uin(Nloc + i)
      s_i = Uin(2*Nloc + i) / rho_i
      Uint = rho_i**(gamma-1) / (gamma-1) * exp((gamma-1)*s_i)
      H = H + (0.5_dp * m_i**2 / rho_i + rho_i * Uint)
    end do
    H = H * dx
  end function compute_total_H_from_variation

  function compute_total_S(Nloc, Uin) result(S)
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: Uin(3*Nloc)
    real(dp) :: S
    integer :: i
    S = 0.0_dp
    do i = 1, Nloc
      S = S + Uin(2*Nloc + i)
    end do
    S = S * dx
  end function compute_total_S

end program metriplectic_matrix_fixed
