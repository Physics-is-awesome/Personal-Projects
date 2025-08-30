!--------------------------------------------------------------
! Fortran90 code for metriplectic 1D thermal-fluid model
!--------------------------------------------------------------

module Parameters
  implicit none
  integer, parameter :: dp = selected_real_kind(15, 307)
  integer, parameter :: N = 200
  real(dp), parameter :: L = 1.0_dp
  real(dp), parameter :: dx = L / N
  real(dp), parameter :: gamma = 1.4_dp
  real(dp), parameter :: Re = 100.0_dp
  real(dp), parameter :: Pr = 1.0_dp
  real(dp), parameter :: Tfinal = 0.5_dp
  real(dp), parameter :: CFL = 0.25_dp
  integer, parameter :: max_picard = 30
  real(dp), parameter :: picard_tol = 1.0e-10_dp
end module Parameters

!-----------------------------------------------------------------------
module Operators
  use Parameters
  implicit none
contains
  subroutine build_D_matrices(Nloc, dxloc, D, D2)
    integer, intent(in) :: Nloc
    real(dp), intent(in) :: dxloc
    real(dp), intent(out) :: D(Nloc,Nloc), D2(Nloc,Nloc)
    integer :: i, ip, im

    D = 0.0_dp
    D2 = 0.0_dp

    do i = 1, Nloc
      ip = i + 1
      if (ip > Nloc) ip = 1
      im = i - 1
      if (im < 1) im = Nloc
      D(i,ip) =  0.5_dp / dxloc
      D(i,im) = -0.5_dp / dxloc
    end do

    do i = 1, Nloc
      ip = i + 1
      if (ip > Nloc) ip = 1
      im = i - 1
      if (im < 1) im = Nloc
      D2(i,i) = -2.0_dp / (dxloc*dxloc)
      D2(i,ip) = 1.0_dp / (dxloc*dxloc)
      D2(i,im) = 1.0_dp / (dxloc*dxloc)
    end do
  end subroutine build_D_matrices

  pure function matvec(A, v, Nloc) result(w)
    real(dp), intent(in) :: A(Nloc,Nloc), v(Nloc)
    integer, intent(in) :: Nloc
    real(dp) :: w(Nloc)
    integer :: i
    w = 0.0_dp
    do i = 1, Nloc
      w(i) = sum(A(i,1:Nloc) * v(1:Nloc))
    end do
  end function matvec

end module Operators

!-----------------------------------------------------------------------
module Thermo
  use Parameters
  implicit none
contains
  subroutine thermodynamics_from_state(U, rho, vel, s, T, p)
    ! Input: U(1,:) = rho, U(2,:) = m, U(3,:) = sigma
    real(dp), intent(in) :: U(3, N)
    real(dp), intent(out) :: rho(N), vel(N), s(N), T(N), p(N)
    integer :: i
    real(dp) :: sigma_loc

    do i = 1, N
      rho(i) = U(1,i)
      if (rho(i) <= 1.0e-12_dp) rho(i) = 1.0e-12_dp
      vel(i) = U(2,i) / rho(i)
      sigma_loc = U(3,i)
      s(i) = sigma_loc / rho(i)
      ! paper's nondimensional ideal gas thermodynamics:
      T(i) = rho(i)**(gamma-1) * exp( (gamma-1) * s(i) )
      p(i) = rho(i) * T(i)
    end do
  end subroutine thermodynamics_from_state

end module Thermo

!-----------------------------------------------------------------------
module RHS_module
  use Parameters
  use Operators, only: matvec
  use Thermo
  implicit none
contains
  subroutine compute_rhs(Uin, D, D2, rhs)
    real(dp), intent(in) :: Uin(3,N)
    real(dp), intent(in) :: D(N,N), D2(N,N)
    real(dp), intent(out) :: rhs(3,N)

    real(dp) :: rho(N), vel(N), s(N), T(N), p(N)
    real(dp) :: m(N), sigma(N)
    real(dp) :: du_dx(N), d2u_dx2(N), dT_dx(N), d2T_dx2(N)
    real(dp) :: flux_mu(N), flux_sigma_u(N)
    integer :: i

    ! compute primitive fields
    call thermodynamics_from_state(Uin, rho, vel, s, T, p)

    do i = 1, N
      m(i) = Uin(2,i)
      sigma(i) = Uin(3,i)
    end do

    ! derivatives
    du_dx = matvec(D, vel, N)
    d2u_dx2 = matvec(D2, vel, N)
    dT_dx = matvec(D, T, N)
    d2T_dx2 = matvec(D2, T, N)

    ! continuity: drho/dt = -d_x m
    rhs(1,:) = - matvec(D, m, N)

    ! momentum: dm/dt = - d_x (m*u) - d_x p + (1/Re) * d2 u
    do i = 1, N
      flux_mu(i) = m(i) * vel(i)
    end do
    rhs(2,:) = - matvec(D, flux_mu, N) - matvec(D, p, N) + (1.0_dp / Re) * d2u_dx2

    ! entropy density sigma evolution:
    ! dσ/dt = - d_x (σ u) + viscous heating term + thermal diffusion term
    do i = 1, N
      flux_sigma_u(i) = sigma(i) * vel(i)
    end do
    ! -d_x(σ u)
    rhs(3,:) = - matvec(D, flux_sigma_u, N)

    ! add viscous heating : (1/Re)*(1/T)*(du_dx)^2 (pointwise)
    do i = 1, N
      rhs(3,i) = rhs(3,i) + (1.0_dp / Re) * (1.0_dp / T(i)) * (du_dx(i)**2)
    end do

    ! thermal diffusion flux: f = (1/(Re*Pr))*(1/T) * dT_dx ; add -d_x(f)
    do i = 1, N
      ! compute f(i)
      ! we will compute vector f and apply D to it
    end do
    rhs(3,:) = rhs(3,:) + matvec(D, (1.0_dp/(Re*Pr)) * (dT_dx / T), N)

    ! additional correction (nonlinear) (1/(Re*Pr))*(1/T^2)*(dT_dx)^2
    do i = 1, N
      rhs(3,i) = rhs(3,i) + (1.0_dp/(Re*Pr)) * ( (dT_dx(i)**2) / (T(i)**2) )
    end do

  end subroutine compute_rhs

end module RHS_module

!-----------------------------------------------------------------------
module Diagnostics
  use Parameters
  implicit none
contains
  function compute_total_H(Uin) result(H)
    real(dp), intent(in) :: Uin(3,N)
    real(dp) :: H
    integer :: i
    real(dp) :: rho_i, s_i, Uval, m_i

    H = 0.0_dp
    do i = 1, N
      rho_i = Uin(1,i)
      m_i = Uin(2,i)
      s_i = Uin(3,i) / rho_i
      Uval = rho_i**(gamma-1) / (gamma-1) * exp( (gamma-1) * s_i )
      H = H + (0.5_dp * m_i**2 / rho_i + rho_i * Uval)
    end do
    H = H * dx
  end function compute_total_H
end module Diagnostics

!-----------------------------------------------------------------------
program main
  use Parameters
  use Operators
  use RHS_module
  use Thermo
  use Diagnostics
  implicit none

  real(dp) :: x(N)
  real(dp) :: D(N,N), D2(N,N)
  real(dp) :: U(3,N), Unew(3,N), Umid(3,N), rhs(3,N)
  real(dp) :: rho(N), vel(N), s(N), T(N), p(N)
  real(dp) :: umax, dt
  integer :: step, nsteps, picard_iter, i
  real(dp) :: t
  real(dp) :: Htot, Stot

  ! initialize grid
  do i = 1, N
    x(i) = (i - 0.5_dp) * dx
  end do

  call build_D_matrices(N, dx, D, D2)

  ! initial condition
  do i = 1, N
    rho(i) = 1.0_dp + 0.1_dp * exp(-50.0_dp * ( (x(i)-0.5_dp*L)**2 ))
    vel(i) = 0.0_dp
    s(i) = 0.0_dp
    U(1,i) = rho(i)
    U(2,i) = rho(i) * vel(i)
    U(3,i) = rho(i) * s(i)
  end do

  ! initial dt
  call thermodynamics_from_state(U, rho, vel, s, T, p)
  umax = maxval(abs(vel) + sqrt(gamma * T))
  if (umax <= 0.0_dp) umax = 1.0_dp
  dt = CFL * dx / umax
  if (dt <= 0.0_dp) dt = 1.0e-8_dp
  nsteps = int(max(1.0_dp, Tfinal / dt))

  write(*,*) "N=", N, " dx=", dx, " dt=", dt, " nsteps=", nsteps

  open(unit=20, file='energy.dat', status='replace', action='write')
  open(unit=21, file='entropy.dat', status='replace', action='write')
  write(20,'(A)') "# t H_tot"
  write(21,'(A)') "# t S_tot"

  t = 0.0_dp
  step = 0

  do while (t < Tfinal .and. step < nsteps)
    step = step + 1

    ! Picard iterations for implicit midpoint
    Umid = U
    picard_iter = 0
    do
      picard_iter = picard_iter + 1
      call compute_rhs(Umid, D, D2, rhs)
      Umid = U + 0.5_dp * dt * rhs
      ! convergence test: max change
      if (maxval(abs(0.5_dp * dt * rhs)) < picard_tol) exit
      if (picard_iter >= max_picard) exit
    end do

    call compute_rhs(Umid, D, D2, rhs)
    Unew = U + dt * rhs

    U = Unew
    t = t + dt

    if (mod(step,10) == 0 .or. t >= Tfinal) then
      call thermodynamics_from_state(U, rho, vel, s, T, p)
      Htot = compute_total_H(U)
      Stot = sum(U(3,:)) * dx
      write(20,'(F12.6,2X,ES16.9)') t, Htot
      write(21,'(F12.6,2X,ES16.9)') t, Stot
      write(*,'(A,F8.6,A,I6,A,I3)') "t=", t, " step=", step, " picard_iter=", picard_iter
    end if

  end do

  call thermodynamics_from_state(U, rho, vel, s, T, p)
  open(unit=30, file='solution.dat', status='replace', action='write')
  write(30,'(A)') "# x rho vel p"
  do i = 1, N
    write(30,'(F8.5,3X,3E12.5)') x(i), rho(i), vel(i), p(i)
  end do
  close(30)
  close(20)
  close(21)

  write(*,*) "Finished. Outputs: solution.dat, energy.dat, entropy.dat"

end program main
