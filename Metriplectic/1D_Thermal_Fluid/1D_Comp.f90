!=======================================================================
! metriplectic_galerkin_new.f90
! Paper-faithful Galerkin metriplectic 1D implementation
! - Linear P1 FE on periodic domain
! - Exact Mass & Stiffness assembly
! - Exact block P (antisymmetric in mass inner product) and G (symmetric PSD)
! - Implicit midpoint (paper recommended) solved with Newton using analytic Jacobian
! - Self-contained dense linear algebra (Gaussian elimination)
!=======================================================================
program metriplectic_galerkin_new
  implicit none
  integer, parameter :: dp = selected_real_kind(15,307)

  ! ---------------- user parameters ----------------
  integer, parameter :: Nnodes = 60        ! number of FE nodes (periodic)
  integer, parameter :: N = Nnodes
  real(dp), parameter :: L = 1.0_dp
  real(dp), parameter :: h = L / N
  real(dp), parameter :: gamma = 1.4_dp
  real(dp), parameter :: Re = 100.0_dp
  real(dp), parameter :: Pr = 1.0_dp
  real(dp), parameter :: CFL = 0.25_dp
  real(dp), parameter :: Tfinal = 0.2_dp
  integer, parameter :: max_newton = 30
  real(dp), parameter :: newton_tol = 1.0e-10_dp
  integer, parameter :: max_out = 20
  ! -------------------------------------------------

  integer :: Mdim, i, j, step, nsteps, newton_iters
  real(dp) :: umax, dt, t
  real(dp), allocatable :: Mass(:,:), Ks(:,:), Minv(:,:)
  real(dp), allocatable :: P(:,:), G(:,:)
  real(dp), allocatable :: U(:), Unew(:), Uold(:)
  real(dp), allocatable :: dHnod(:), dSnod(:), dHcoef(:), dScoef(:)
  real(dp), allocatable :: R(:), delta(:)
  real(dp), allocatable :: Dloc(:,:)   ! block-diagonal derivative matrix (3N x 3N)
  real(dp), allocatable :: Minv_full(:,:) ! blockdiag(Minv,Minv,Minv)
  real(dp), allocatable :: x(:)
  real(dp) :: Htot, Stot
  real(dp) :: normPantisym, normGsym
  integer :: info

  Mdim = 3 * N
  allocate(Mass(N,N), Ks(N,N), Minv(N,N))
  allocate(P(Mdim,Mdim), G(Mdim,Mdim))
  allocate(U(Mdim), Unew(Mdim), Uold(Mdim))
  allocate(dHnod(Mdim), dSnod(Mdim), dHcoef(Mdim), dScoef(Mdim))
  allocate(R(Mdim), delta(Mdim))
  allocate(Dloc(Mdim,Mdim), Minv_full(Mdim,Mdim))
  allocate(x(N))

  !---- grid ----
  do i = 1, N
    x(i) = (i - 0.5_dp) * h
  end do

  ! Assemble FE Mass and Stiffness matrices
  call assemble_FE_mass_stiff(N, h, Mass, Ks)

  ! Compute Minv (dense) by solving Mass * Minv_col = e_j for each column
  call invert_dense_mass(N, Mass, Minv)

  ! Build Minv_full = blockdiag(Minv, Minv, Minv)
  call build_block_Minv(N, Minv, Minv_full)

  ! Assemble P and G exactly using Minv and K (paper weak-form)
  call assemble_block_P_G_exact(N, Minv, Ks, P, G, Re, Pr)

  ! Diagnostics: check antisymmetry of P in Mass inner product and symmetry of G
  call check_P_G_properties(N, Mass, P, G, normPantisym, normGsym)
  write(*,'(A,ES12.6)') 'Mass-weighted antisymmetry norm ||Mblock*P + (Mblock*P)^T|| =', normPantisym
  write(*,'(A,ES12.6)') 'G symmetry norm ||G - G^T|| =', normGsym

  ! initialize U (rho, m, sigma)
  call initialize_U(N, x, U)

  ! initial diagnostics
  Htot = compute_total_H(N, Mass, U, gamma)
  Stot = compute_total_S(N, Mass, U)
  write(*,'(A,ES14.8)') 'Initial H =', Htot
  write(*,'(A,ES14.8)') 'Initial S =', Stot

  ! estimate dt from CFL using local velocity+sound
  call primitive_from_U(N, U, umax, gamma)
  if (umax <= 0.0_dp) umax = 1.0_dp
  dt = CFL * h / umax
  if (dt <= 0.0_dp) dt = 1.0e-8_dp
  nsteps = max(1, int(Tfinal / dt))
  write(*,'(A,F8.4,A,I6)') 'dt=', dt, ' nsteps=', nsteps

  ! open outputs
  open(unit=11, file='energy.dat', status='replace', action='write')
  open(unit=12, file='entropy.dat', status='replace', action='write')
  write(11,'(A)') '# t  H'
  write(12,'(A)') '# t  S'

  t = 0.0_dp
  do step = 1, nsteps
    Uold = U

    ! initial predictor: explicit Euler (good initial guess)
    call compute_functionals_and_variations(N, Uold, dHnod, dSnod, gamma)
    call map_nodal_to_coef_using_Minv(N, Minv, dHnod, dHcoef)
    call map_nodal_to_coef_using_Minv(N, Minv, dSnod, dScoef)  ! dSnod mostly zeros + ones for sigma
    R = matvec(P, dHcoef, Mdim) + matvec(G, dScoef, Mdim)
    Unew = Uold + dt * R

    ! Newton iterations with analytic Jacobian
    do newton_iters = 1, max_newton
      ! compute Umid = 0.5*(Uold+Unew)
      ! build Dloc = d(dHnod)/dU evaluated at Umid (block-diagonal 3x3 per node)
      call build_Dloc_blockdiag(N, Uold, Unew, Dloc, gamma)  ! uses Umid internally

      ! compute dHnod,dSnod at Umid and map to coefficients
      call compute_functionals_and_variations_midpoint(N, Uold, Unew, dHnod, dSnod, gamma)
      call map_nodal_to_coef_using_Minv(N, Minv, dHnod, dHcoef)
      call map_nodal_to_coef_using_Minv(N, Minv, dSnod, dScoef)

      ! residual R = Unew - Uold - dt*(P*dHcoef + G*dScoef)
      R = Unew - Uold - dt * ( matvec(P, dHcoef, Mdim) + matvec(G, dScoef, Mdim) )

      if (norm2(R) < newton_tol) exit

      ! Build Jacobian J = I - 0.5*dt * ( P * Minv_full * Dloc )
      ! We'll compute J matrix explicitly and solve J*delta = -R
      call build_Jacobian_and_solve(Mdim, P, Minv_full, Dloc, dt, delta, R)

      ! update
      Unew = Unew + delta

      if (norm2(delta) < 1.0e-12_dp) exit
    end do

    if (newton_iters >= max_newton) then
      write(*,*) 'Warning: Newton failed to converge in max_newton iterations at step', step
    end if

    ! accept update
    U = Unew
    t = t + dt

    if (mod(step, max(1,nsteps/ max_out)) == 0 .or. step == nsteps) then
      Htot = compute_total_H(N, Mass, U, gamma)
      Stot = compute_total_S(N, Mass, U)
      write(11,'(F12.6,2X,ES16.9)') t, Htot
      write(12,'(F12.6,2X,ES16.9)') t, Stot
      write(*,'(A,F8.6,A,I6,A,I3)') 't=', t, ' step=', step, ' newton_iters=', newton_iters
      write(*,'(A,ES16.9,A,ES16.9)') '  H=', Htot, '  S=', Stot
    end if
  end do

  ! output final solution
  call output_solution(N, x, U, gamma)

  close(11); close(12)
  write(*,*) 'Done. Outputs: solution.dat, energy.dat, entropy.dat'

contains

  ! ---------------------------------------------------------------------
  subroutine assemble_FE_mass_stiff(Nloc, hloc, Mout, Kout)
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: hloc
    real(dp), intent(out) :: Mout(Nloc,Nloc), Kout(Nloc,Nloc)
    integer :: i, ip
    Mout = 0.0_dp
    Kout = 0.0_dp
    do i = 1, Nloc
      ip = i + 1
      if (ip > Nloc) ip = 1
      Mout(i,i) = Mout(i,i) + 2.0_dp * hloc / 6.0_dp
      Mout(i,ip) = Mout(i,ip) + 1.0_dp * hloc / 6.0_dp
      Mout(ip,i) = Mout(ip,i) + 1.0_dp * hloc / 6.0_dp
      Mout(ip,ip) = Mout(ip,ip) + 2.0_dp * hloc / 6.0_dp

      Kout(i,i) = Kout(i,i) + 1.0_dp / hloc
      Kout(i,ip) = Kout(i,ip) - 1.0_dp / hloc
      Kout(ip,i) = Kout(ip,i) - 1.0_dp / hloc
      Kout(ip,ip) = Kout(ip,ip) + 1.0_dp / hloc
    end do
  end subroutine assemble_FE_mass_stiff

  ! ---------------------------------------------------------------------
  subroutine invert_dense_mass(Nloc, M, Minv_out)
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: M(Nloc,Nloc)
    real(dp), intent(out) :: Minv_out(Nloc,Nloc)
    integer :: j
    real(dp), allocatable :: e(:), sol(:)
    allocate(e(Nloc), sol(Nloc))
    do j = 1, Nloc
      e = 0.0_dp
      e(j) = 1.0_dp
      call solve_linear(M, e, sol, Nloc)
      Minv_out(:, j) = sol(:)
    end do
    deallocate(e, sol)
  end subroutine invert_dense_mass

  ! ---------------------------------------------------------------------
  subroutine build_block_Minv(Nloc, Minv_small, Minv_full)
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: Minv_small(Nloc,Nloc)
    real(dp), intent(out) :: Minv_full(3*Nloc,3*Nloc)
    integer :: i, j, k
    Minv_full = 0.0_dp
    do k = 0, 2
      do i = 1, Nloc
        do j = 1, Nloc
          Minv_full(k*Nloc + i, k*Nloc + j) = Minv_small(i,j)
        end do
      end do
    end do
  end subroutine build_block_Minv

  ! ---------------------------------------------------------------------
  subroutine assemble_block_P_G_exact(Nn, Minv, Ks, Pout, Gout, Re_in, Pr_in)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Minv(Nn,Nn), Ks(Nn,Nn)
    real(dp), intent(out) :: Pout(3*Nn,3*Nn), Gout(3*Nn,3*Nn)
    real(dp), intent(in) :: Re_in, Pr_in
    integer :: i, j, k
    real(dp), allocatable :: D(:,:)
    allocate(D(Nn,Nn))
    ! Compute D = Minv * Ks exactly (represents weak derivative mapping)
    D = matmat(Minv, Ks, Nn, Nn, Nn)

    Pout = 0.0_dp
    Gout = 0.0_dp

    ! Place blocks so that P is antisymmetric (in block sense)
    ! block ordering: [rho | m | sigma]
    do i = 1, Nn
      do j = 1, Nn
        Pout(i, Nn + j)     = - D(i,j)        ! P(rho, m) = -D
        Pout(Nn + j, i)     =   D(j,i)        ! P(m, rho) = D^T  (so antisymm)
        Pout(Nn + i, 2*Nn + j) =  Ks(i,j)      ! P(m, sigma) = K
        Pout(2*Nn + j, Nn + i) = -Ks(j,i)      ! P(sigma, m) = -K^T
      end do
    end do

    ! Metric G: symmetric PSD from stiffness K (viscosity & thermal)
    do i = 1, Nn
      do j = 1, Nn
        Gout(Nn + i, Nn + j) = (1.0_dp / Re_in) * Ks(i,j)
        Gout(2*Nn + i, 2*Nn + j) = (1.0_dp / (Re_in * Pr_in)) * Ks(i,j)
      end do
    end do

    ! symmetrize G explicitly
    do i = 1, 3*Nn
      do j = i+1, 3*Nn
        Gout(i,j) = 0.5_dp * (Gout(i,j) + Gout(j,i))
        Gout(j,i) = Gout(i,j)
      end do
    end do

    deallocate(D)
  end subroutine assemble_block_P_G_exact

  ! ---------------------------------------------------------------------
  subroutine check_P_G_properties(Nn, Mass, P, G, outPnorm, outGnorm)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Mass(Nn,Nn), P(3*Nn,3*Nn), G(3*Nn,3*Nn)
    real(dp), intent(out) :: outPnorm, outGnorm
    real(dp), allocatable :: Mblock(:,:), S(:,:)
    integer :: n
    n = 3 * Nn
    allocate(Mblock(n,n), S(n,n))
    Mblock = 0.0_dp
    ! block diag of Mass
    do i = 1, Nn
      do j = 1, Nn
        Mblock(i,j) = Mass(i,j)
        Mblock(Nn + i, Nn + j) = Mass(i,j)
        Mblock(2*Nn + i, 2*Nn + j) = Mass(i,j)
      end do
    end do
    S = matmat(Mblock, P, n, n, n)
    S = S + transpose(S)
    outPnorm = normF(S)
    S = G - transpose(G)
    outGnorm = normF(S)
    deallocate(Mblock, S)
  end subroutine check_P_G_properties

  ! ---------------------------------------------------------------------
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

  ! ---------------------------------------------------------------------
  subroutine compute_functionals_and_variations(Nn, Uin, dHnod, dSnod, gamma_in)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Uin(3*Nn)
    real(dp), intent(out) :: dHnod(3*Nn), dSnod(3*Nn)
    real(dp), intent(in) :: gamma_in
    integer :: i
    real(dp) :: r, m, sigma, vel, s, A, E, u, u_s, u_r, eint
    do i = 1, Nn
      r = Uin(i)
      m = Uin(Nn + i)
      sigma = Uin(2*Nn + i)
      if (r <= 1.0e-14_dp) r = 1.0e-14_dp
      vel = m / r
      s = sigma / r
      E = exp((gamma_in - 1.0_dp) * s)
      A = r**(gamma_in - 1.0_dp) / (gamma_in - 1.0_dp)
      u = A * E                         ! internal energy per unit mass
      u_s = (gamma_in - 1.0_dp) * u     ! ∂u/∂s
      u_r = r**(gamma_in - 2.0_dp) * E  ! ∂u/∂r (holding s constant)
      ! nodal functional derivatives (consistent with sigma = rho*s)
      dHnod(i) = -0.5_dp * m**2 / r**2 + u + r * u_r - s * u_s  ! δH/δρ
      dHnod(Nn + i) = m / r                                    ! δH/δm
      dHnod(2*Nn + i) = u_s                                     ! δH/δσ
      ! S = ∫ σ dx  => δS/δσ = 1, other components 0
      dSnod(i) = 0.0_dp
      dSnod(Nn + i) = 0.0_dp
      dSnod(2*Nn + i) = 1.0_dp
    end do
  end subroutine compute_functionals_and_variations

  ! ---------------------------------------------------------------------
  subroutine compute_functionals_and_variations_midpoint(Nn, Uold, Unew, dHnod, dSnod, gamma_in)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Uold(3*Nn), Unew(3*Nn)
    real(dp), intent(out) :: dHnod(3*Nn), dSnod(3*Nn)
    real(dp), intent(in) :: gamma_in
    real(dp), allocatable :: Umid(:)
    integer :: i
    allocate(Umid(3*Nn))
    Umid = 0.5_dp * (Uold + Unew)
    call compute_functionals_and_variations(Nn, Umid, dHnod, dSnod, gamma_in)
    deallocate(Umid)
  end subroutine compute_functionals_and_variations_midpoint

  ! ---------------------------------------------------------------------
  subroutine map_nodal_to_coef_using_Minv(Nn, Minv, nodal, coef)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Minv(Nn,Nn)
    real(dp), intent(in) :: nodal(3*Nn)
    real(dp), intent(out) :: coef(3*Nn)
    integer :: i, offset
    real(dp), allocatable :: tmp(:), sol(:)
    allocate(tmp(Nn), sol(Nn))
    do offset = 0, 2
      do i = 1, Nn
        tmp(i) = nodal(offset * Nn + i)
      end do
      ! solve M * sol = tmp using precomputed Minv via multiplication
      ! sol = Minv * tmp
      sol = matvec(Minv, tmp, Nn)
      do i = 1, Nn
        coef(offset * Nn + i) = sol(i)
      end do
    end do
    deallocate(tmp, sol)
  end subroutine map_nodal_to_coef_using_Minv

  ! ---------------------------------------------------------------------
  subroutine primitive_from_U(Nn, Uin, umax_out, gamma_in)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Uin(3*Nn)
    real(dp), intent(out) :: umax_out
    real(dp), intent(in) :: gamma_in
    integer :: i
    real(dp) :: rho_i, vel_i, T_i
    umax_out = 0.0_dp
    do i = 1, Nn
      rho_i = Uin(i)
      if (rho_i <= 1.0e-12_dp) rho_i = 1.0e-12_dp
      vel_i = Uin(Nn + i) / rho_i
      T_i = rho_i**(gamma_in - 1.0_dp)  ! proxy for temperature magnitude
      umax_out = max(umax_out, abs(vel_i) + sqrt(gamma_in * T_i))
    end do
  end subroutine primitive_from_U

  ! ---------------------------------------------------------------------
  subroutine build_Dloc_blockdiag(Nn, Uold, Unew, Dloc_out, gamma_in)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Uold(3*Nn), Unew(3*Nn)
    real(dp), intent(out) :: Dloc_out(3*Nn,3*Nn)
    real(dp), intent(in) :: gamma_in
    integer :: i, row, col
    real(dp) :: r, m, sigma, vel, s, E, A, u
    real(dp) :: B, A11, A12, A13, B11, B12, B13, C11, C12, C13
    real(dp), allocatable :: Umid(:)
    allocate(Umid(3*Nn))
    Umid = 0.5_dp * (Uold + Unew)

    Dloc_out = 0.0_dp
    do i = 1, Nn
      r = Umid(i)
      m = Umid(Nn + i)
      sigma = Umid(2*Nn + i)
      if (r <= 1.0e-14_dp) r = 1.0e-14_dp
      vel = m / r
      s = sigma / r
      E = exp((gamma_in - 1.0_dp) * s)
      A = r**(gamma_in - 1.0_dp) / (gamma_in - 1.0_dp)
      u = A * E
      ! helper B = gamma - (gamma-1)*s
      B = gamma_in - (gamma_in - 1.0_dp) * s

      ! local derivative block (3x3) for ∂(dHnod)/∂(U) at node i
      ! Row 1: derivatives of dH_rho
      A11 = m**2 / r**3 + (gamma_in - 1.0_dp) * u / r * (B + s)
      A12 = - m / r**2
      A13 = (gamma_in - 1.0_dp) * u * (B - 1.0_dp) / r

      ! Row 2: derivatives of dH_m
      B11 = - m / r**2
      B12 = 1.0_dp / r
      B13 = 0.0_dp

      ! Row 3: derivatives of dH_sigma (u_s and its derivatives)
      C11 = (gamma_in - 1.0_dp)**2 * u / r
      C12 = 0.0_dp
      C13 = (gamma_in - 1.0_dp)**2 * u / r

      ! place these into Dloc_out at appropriate global indices
      row = i
      col = i
      Dloc_out(row, col) = A11
      Dloc_out(row, Nn + col) = A12
      Dloc_out(row, 2*Nn + col) = A13

      Dloc_out(Nn + row, col) = B11
      Dloc_out(Nn + row, Nn + col) = B12
      Dloc_out(Nn + row, 2*Nn + col) = B13

      Dloc_out(2*Nn + row, col) = C11
      Dloc_out(2*Nn + row, Nn + col) = C12
      Dloc_out(2*Nn + row, 2*Nn + col) = C13
    end do

    deallocate(Umid)
  end subroutine build_Dloc_blockdiag

  ! ---------------------------------------------------------------------
  subroutine build_Jacobian_and_solve(nvar, Pmat, Minv_full_mat, Dloc_mat, dt, delta_out, Rvec)
    integer, intent(in) :: nvar
    real(dp), intent(in) :: Pmat(nvar,nvar), Minv_full_mat(nvar,nvar), Dloc_mat(nvar,nvar)
    real(dp), intent(in) :: dt
    real(dp), intent(in) :: Rvec(nvar)
    real(dp), intent(out) :: delta_out(nvar)
    real(dp), allocatable :: J(:,:), tmp(:,:)
    integer :: n
    n = nvar
    allocate(J(n,n), tmp(n,n))
    ! tmp = Minv_full * Dloc
    tmp = matmat(Minv_full_mat, Dloc_mat, n, n, n)
    ! J = I - 0.5*dt * P * tmp
    J = 0.0_dp
    J = matmat(Pmat, tmp, n, n, n)
    J = (-0.5_dp * dt) * J
    do i = 1, n
      J(i,i) = J(i,i) + 1.0_dp
    end do
    ! solve J * delta = -Rvec
    call solve_linear(J, -Rvec, delta_out, n)
    deallocate(J, tmp)
  end subroutine build_Jacobian_and_solve

  ! ---------------------------------------------------------------------
  function matvec(A, v, n) result(w)
    real(dp), intent(in) :: A(n,n), v(n)
    integer, intent(in) :: n
    real(dp) :: w(n)
    integer :: i
    w = 0.0_dp
    do i = 1, n
      w(i) = sum(A(i,1:n) * v(1:n))
    end do
  end function matvec

  ! ---------------------------------------------------------------------
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

  ! ---------------------------------------------------------------------
  subroutine solve_linear(Ain, b, xout, n)
    ! dense Gaussian elimination with partial pivoting
    real(dp), intent(in) :: Ain(n,n), b(n)
    real(dp), intent(out) :: xout(n)
    integer, intent(in) :: n
    real(dp), allocatable :: tmp(:,:)
    integer :: i, j, k, piv
    real(dp) :: maxv, factor, pivot
    allocate(tmp(n,n+1))
    tmp(:,1:n) = Ain(:,1:n)
    tmp(:,n+1) = b(:)
    do k = 1, n-1
      maxv = 0.0_dp
      piv = k
      do i = k, n
        if (abs(tmp(i,k)) > maxv) then
          maxv = abs(tmp(i,k)); piv = i
        end if
      end do
      if (maxv < 1.0e-16_dp) then
        tmp(k,k) = tmp(k,k) + 1.0e-16_dp
      end if
      if (piv /= k) then
        tmp([k,piv],:) = tmp([piv,k],:)
      end if
      pivot = tmp(k,k)
      do i = k+1, n
        factor = tmp(i,k) / pivot
        tmp(i,k) = 0.0_dp
        do j = k+1, n+1
          tmp(i,j) = tmp(i,j) - factor * tmp(k,j)
        end do
      end do
    end do
    ! back substitution
    if (abs(tmp(n,n)) < 1.0e-16_dp) tmp(n,n) = tmp(n,n) + 1.0e-16_dp
    xout(n) = tmp(n,n+1) / tmp(n,n)
    do i = n-1, 1, -1
      xout(i) = (tmp(i,n+1) - sum(tmp(i,i+1:n) * xout(i+1:n))) / tmp(i,i)
    end do
    deallocate(tmp)
  end subroutine solve_linear

  ! ---------------------------------------------------------------------
  function norm2(v) result(nrm)
    real(dp), intent(in) :: v(:)
    real(dp) :: nrm
    integer :: i
    nrm = 0.0_dp
    do i = 1, size(v)
      nrm = nrm + v(i)**2
    end do
    nrm = sqrt(nrm)
  end function norm2

  ! ---------------------------------------------------------------------
  function normF(A) result(nrm)
    real(dp), intent(in) :: A(:,:)
    real(dp) :: nrm
    integer :: i, j
    nrm = 0.0_dp
    do i = 1, size(A,1)
      do j = 1, size(A,2)
        nrm = nrm + A(i,j)**2
      end do
    end do
    nrm = sqrt(nrm)
  end function normF

  ! ---------------------------------------------------------------------
  function compute_total_H(Nn, Mass, Uin, gamma_in) result(H)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Mass(Nn,Nn), Uin(3*Nn)
    real(dp), intent(in) :: gamma_in
    real(dp) :: H
    integer :: i
    real(dp) :: r, m, sigma, s, A, E, eint
    H = 0.0_dp
    do i = 1, Nn
      r = Uin(i)
      m = Uin(Nn + i)
      sigma = Uin(2*Nn + i)
      if (r <= 1.0e-14_dp) r = 1.0e-14_dp
      s = sigma / r
      E = exp((gamma_in - 1.0_dp) * s)
      A = r**(gamma_in - 1.0_dp) / (gamma_in - 1.0_dp)
      eint = A * E
      H = H + 0.5_dp * m**2 / r + r * eint
    end do
    H = H * h
  end function compute_total_H

  ! ---------------------------------------------------------------------
  function compute_total_S(Nn, Mass, Uin) result(S)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: Mass(Nn,Nn), Uin(3*Nn)
    real(dp) :: S
    integer :: i
    S = 0.0_dp
    do i = 1, Nn
      S = S + Uin(2*Nn + i)
    end do
    S = S * h
  end function compute_total_S

  ! ---------------------------------------------------------------------
  subroutine output_solution(Nn, xnodes, Uin, gamma_in)
    integer, intent(in) :: Nn
    real(dp), intent(in) :: xnodes(Nn)
    real(dp), intent(in) :: Uin(3*Nn)
    real(dp), intent(in) :: gamma_in
    integer :: i
    real(dp) :: rho, m, sigma, vel, s, E, A, u, p
    open(unit=30, file='solution.dat', status='replace', action='write')
    write(30,'(A)') '# x rho vel p'
    do i = 1, Nn
      rho = Uin(i)
      m = Uin(Nn + i)
      sigma = Uin(2*Nn + i)
      if (rho <= 1.0e-14_dp) rho = 1.0e-14_dp
      vel = m / rho
      s = sigma / rho
      E = exp((gamma_in - 1.0_dp) * s)
      A = rho**(gamma_in - 1.0_dp) / (gamma_in - 1.0_dp)
      u = A * E
      p = rho * rho**(gamma_in - 1.0_dp) * E  ! p = rho * T, with T = rho^(gamma-1)*exp((gamma-1)s)
      write(30,'(F8.5,3X,3E12.5)') xnodes(i), rho, vel, p
    end do
    close(30)
  end subroutine output_solution

end program metriplectic_galerkin_new
