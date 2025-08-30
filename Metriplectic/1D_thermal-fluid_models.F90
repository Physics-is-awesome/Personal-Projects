!--------------------------------------------------------------
! Fortran90 code for metriplectic 1D thermal-fluid model
!--------------------------------------------------------------

module Parameters
  implicit none
  real, parameter :: gamma = 1.4
  real, parameter :: Rgas  = 287.0
  integer, parameter :: N  = 200
  real,    parameter :: L  = 100.0
  real,    parameter :: dx = L / N
  real, parameter :: Tfinal = 10.0
  real, parameter :: CFL    = 0.5

contains
  real function dt(umax)
    real, intent(in) :: umax
    dt = CFL * dx / umax
  end function dt
end module Parameters

module Grid
  use Parameters
  implicit none
  real, allocatable :: x(:)
  real, allocatable :: U(:,:), V(:,:)
contains
  subroutine initialize_grid()
    integer :: i
    allocate(x(N), U(3,N), V(3,N))
    do i = 1, N
      x(i) = (i - 0.5) * dx
      ! Uniform initial state: density 1, zero momentum, unit energy
      U(1,i) = 1.0
      U(2,i) = 0.0
      U(3,i) = 1.0 / (gamma - 1.0)
    end do
  end subroutine initialize_grid
end module Grid

module Functionals
  use Grid, only: U
  use Parameters
  implicit none
contains
  subroutine compute_derivatives(dH, dS)
    real, allocatable, intent(out) :: dH(:,:), dS(:,:)
    integer :: i
    real :: rho, u, E, p, s

    allocate(dH(3,N), dS(3,N))
    do i = 1, N
      rho = U(1,i)
      u   = U(2,i) / rho
      E   = U(3,i) / rho
      p   = (gamma - 1.0) * rho * (E - 0.5 * u * u)
      s   = log(p / rho**gamma)

      ! δH/δU
      dH(1,i) = 0.5 * u * u - (E - 0.5 * u * u) * (gamma - 1.0)
      dH(2,i) = u
      dH(3,i) = 1.0

      ! δS/δU
      dS(1,i) = s - gamma
      dS(2,i) = u
      dS(3,i) = 0.0
    end do
  end subroutine compute_derivatives
end module Functionals

module Brackets
  use Grid, only: U, V
  use Functionals, only: compute_derivatives
  use Parameters
  implicit none
contains
  subroutine eval_brackets(RHS)
    real, allocatable, intent(out) :: RHS(:,:)
    real, allocatable :: dH(:,:), dS(:,:)
    integer :: i

    allocate(RHS(3,N))
    call compute_derivatives(dH, dS)
    RHS = 0.0

    do i = 2, N-1
      ! Central difference for ∂ₓU
      V(:,i) = (U(:,i+1) - U(:,i-1)) / (2.0 * dx)

      ! Conservative (Poisson) term
      RHS(:,i) = RHS(:,i) + cross_poisson(dH(:,i), V(:,i))

      ! Dissipative (metriplectic) term
      RHS(:,i) = RHS(:,i) + cross_metrip(dS(:,i), V(:,i))
    end do

    ! Zero‐flux boundary conditions
    RHS(:,1) = RHS(:,2)
    RHS(:,N) = RHS(:,N-1)
  end subroutine eval_brackets

  function cross_poisson(dHloc, Vloc) result(Pout)
    real, intent(in) :: dHloc(3), Vloc(3)
    real :: Pout(3)
    ! Skew‐symmetric coupling
    Pout(1) = 0.0
    Pout(2) = -dHloc(3) * Vloc(1) + dHloc(1) * Vloc(3)
    Pout(3) =  dHloc(2) * Vloc(1) - dHloc(1) * Vloc(2)
  end function cross_poisson

  function cross_metrip(dSloc, Vloc) result(Dout)
    real, intent(in) :: dSloc(3), Vloc(3)
    real :: Dout(3)
    ! Symmetric dissipation ensuring positive entropy production
    Dout = dSloc * (Vloc(1)**2 + Vloc(2)**2 + Vloc(3)**2)
  end function cross_metrip
end module Brackets

module TimeStepper
  use Grid, only: U
  use Brackets, only: eval_brackets
  use Parameters, only: dt
  implicit none
contains
  subroutine step_forward()
    real :: t, umax, dti
    real, allocatable :: RHSn(:,:), RHSm(:,:), Un(:,:)
    integer :: nsteps, it

    t = 0.0
    nsteps = int(Tfinal / (dt(1.0) + 1.0e-10))
    allocate(RHSn(3,N), RHSm(3,N), Un(3,N))

    do it = 1, nsteps
      ! Adaptive time step based on CFL condition
      umax = maxval(abs(U(2,:) / U(1,:)) + sqrt(gamma * (gamma - 1.0) * (U(3,:) / U(1,:))))
      dti  = dt(umax)

      ! Implicit midpoint predictor
      call eval_brackets(RHSn)
      Un = U + 0.5 * dti * RHSn

      ! Corrector step at midpoint
      call eval_brackets(RHSm)
      U = U + dti * RHSm

      t = t + dti
      if (t >= Tfinal) exit
    end do
  end subroutine step_forward
end module TimeStepper

program Metriplectic1D
  use Grid,    only: initialize_grid, x, U
  use TimeStepper, only: step_forward
  implicit none

  call initialize_grid()
  call step_forward()
  call output_results()

contains
  subroutine output_results()
    real :: u, p
    integer :: i
    open(unit = 10, file = 'solution.dat', status = 'unknown')
    write(10, '(A)') '# x rho u p'
    do i = 1, N
      u = U(2,i) / U(1,i)
      p = (gamma - 1.0) * (U(3,i) - 0.5 * U(2,i)**2 / U(1,i))
      write(10, '(F8.3, 3X, 3E12.5)') x(i), U(1,i), u, p
    end do
    close(10)
  end subroutine output_results
end program Metriplectic1D
