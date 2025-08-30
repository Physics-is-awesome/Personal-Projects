!--------------------------------------------------------------
! Fortran90 code for metriplectic 1D thermal-fluid model
!--------------------------------------------------------------
module Parameters
  implicit none
  real, parameter :: gamma = 1.4
  integer, parameter :: N = 200
  real, parameter :: L = 100.0
  real, parameter :: dx = L / N
  real, parameter :: Tfinal = 10.0
  real, parameter :: CFL = 0.5
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
  real, allocatable :: U(:,:), V(:,:), dHdU(:,:), dSdU(:,:)
contains
  subroutine initialize_grid()
    integer :: i
    allocate(x(N), U(3,N), V(3,N), dHdU(3,N), dSdU(3,N))
    do i = 1, N
      x(i) = (i - 0.5) * dx
      U(1,i) = 1.0
      U(2,i) = 0.0
      U(3,i) = 1.0 / (gamma - 1.0)
    end do
  end subroutine initialize_grid
end module Grid

module Functionals
  use Parameters
  use Grid, only: U, dHdU, dSdU
  implicit none
contains
  subroutine compute_variational_derivatives()
    integer :: i
    real :: rho, vel, e, p, s
    do i = 1, N
      rho = U(1,i)
      vel = U(2,i) / rho
      e = U(3,i) / rho - 0.5 * vel * vel
      p = (gamma - 1.0) * rho * e
      s = log(p / rho**gamma)

      dHdU(1,i) = 0.5 * vel * vel + e - (gamma - 1.0) * e
      dHdU(2,i) = vel
      dHdU(3,i) = 1.0

      dSdU(1,i) = -gamma + s
      dSdU(2,i) = vel
      dSdU(3,i) = 0.0
    end do
  end subroutine compute_variational_derivatives
end module Functionals

module Brackets
  use Parameters
  use Grid, only: U, V, dHdU, dSdU
  use Functionals, only: compute_variational_derivatives
  implicit none
contains
  subroutine compute_brackets(RHS)
    real, allocatable, intent(out) :: RHS(:,:)
    integer :: i
    allocate(RHS(3,N))
    call compute_variational_derivatives()
    RHS = 0.0
    do i = 2, N-1
      V(:,i) = (U(:,i+1) - U(:,i-1)) / (2.0 * dx)
      RHS(:,i) = poisson_bracket(i) + metric_bracket(i)
    end do
    RHS(:,1) = RHS(:,2)
    RHS(:,N) = RHS(:,N-1)
  end subroutine compute_brackets

  function poisson_bracket(i) result(Pout)
    integer, intent(in) :: i
    real :: Pout(3)
    Pout(1) = 0.0
    Pout(2) = -dHdU(3,i)*V(1,i) + dHdU(1,i)*V(3,i)
    Pout(3) =  dHdU(2,i)*V(1,i) - dHdU(1,i)*V(2,i)
  end function poisson_bracket

  function metric_bracket(i) result(Dout)
    integer, intent(in) :: i
    real :: Dout(3)
    real :: normV2
    normV2 = V(1,i)**2 + V(2,i)**2 + V(3,i)**2
    Dout = dSdU(:,i) * normV2
  end function metric_bracket
end module Brackets

module TimeStepper
  use Parameters, only: dt, Tfinal
  use Grid, only: U
  use Brackets, only: compute_brackets
  implicit none
contains
  subroutine step_forward()
    real :: t, umax, dti
    real, allocatable :: RHS(:,:), Utemp(:,:)
    integer :: it, nsteps
    t = 0.0
    allocate(RHS(3,N), Utemp(3,N))
    nsteps = int(Tfinal / dt(1.0))
    do it = 1, nsteps
      umax = maxval(abs(U(2,:) / U(1,:)) + sqrt(gamma * (gamma - 1.0) * (U(3,:) / U(1,:))))
      dti = dt(umax)
      call compute_brackets(RHS)
      Utemp = U + 0.5 * dti * RHS
      call compute_brackets(RHS)
      U = U + dti * RHS
      t = t + dti
      if (t >= Tfinal) exit
    end do
  end subroutine step_forward
end module TimeStepper

program MetriplecticFull
  use Grid, only: initialize_grid, x, U
  use TimeStepper, only: step_forward
  implicit none
  call initialize_grid()
  call step_forward()
  call output_results()
contains
  subroutine output_results()
    real :: u, p
    integer :: i
    open(unit=10, file="solution_full.dat", status="unknown")
    write(10, '(A)') "# x rho u p"
    do i = 1, size(x)
      vel = U(2,i) / U(1,i)
      p = (gamma - 1.0) * (U(3,i) - 0.5 * U(2,i)**2 / U(1,i))
      write(10, '(F8.3, 3X, 3E12.5)') x(i), U(1,i), vel, p
    end do
    close(10)
  end subroutine output_results
end program MetriplecticFull

