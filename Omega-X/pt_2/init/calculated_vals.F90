module calc_vals
  use states
  use declare_1
  ! use declare_2
  implicit none
contains
  subroutine calculate_variables()
    integer :: i


    ! calculate internal energy(ideal, change later) =================================\
    do i = 1, nx
      U(i) = rho_h(i) ** (gamma - 1) * 2.718281828459045 ** ((gamma-1)*s_h(i))
    end do
    ! Tempature(derivative of internal energy by entropy)
    call midpoint_derivative(U, sigma_h, T_h, ds=1.0d-3)
    ! Spacial derivative of tempature
    dT_h_dx = space_discr(T_h, dx)


    ! Calculate velocity
    do i = 1, nx
      u_h(i) = m_h(i)/rho_h(i)
    end do

    ! space derivative of velocity
    du_h_dx = space_discr(u_h, dx)

    ! calculate spesific entropy
    do i = 1, nx
      s_h(i) = sigma_h(i)/rho_h(i)
    end do


    ! calculate  ratio of tempeture to spesific entropy
    do i = 1, nx
      T_S(i) = T_h(i)/s_h(i)
    end do

    ! calculating pressure (ideal, change later)======================================
    do i = 1, nx
      p(i) = (gamma-1) * rho_h(i) ** (gamma) * 2.718281828459045 ** ((gamma-1)*s_h(i)) 
    end do

    ! calculating eta
    do i = 1, nx
      eta_h(i) = (m_h(i)**2)/(2*rho_h(i)**2) + e(i) + p/rho_h(i) - s_h(i) * T_h(i)
    end do

    ! space derivative of eta
    deta_h_dx = space_discr(eta_h, dx)

  
    ! space derivative of phi

  end subroutine calculate_variables
  !=============================
  ! implicit mid-point in time
  !=============================
  !function time_discr(z_n, dt, dn_dt) result(z_np1)
   ! real(8), intent(in) :: z_n, dt
    !interface
     ! function f(z) result(val)
      !  real(8), intent(in) :: z
       ! real(8) :: val
     ! end function f
   ! end interface
  !  real(8) :: z_np1, z_half, res, tol
   ! integer :: iter, max_iter

    ! Initial guess: explicit Euler
   ! z_np1 = z_n + dt * f(z_n)

    !tol = 1.0d-12
    !max_iter = 20

    ! Newton iteration to solve implicit midpoint equation:
    ! z_{n+1} = z_n + dt * f( (z_n + z_{n+1})/2 )
    !do iter = 1, max_iter
     !  z_half = 0.5d0 * (z_n + z_np1)
      ! res    = z_n + dt * f(z_half) - z_np1
      ! if (abs(res) < tol) exit
       ! Simple fixed-point correction
     !  z_np1 = z_np1 + res
   ! end do

 ! end function time_discr

  !======================
  ! Spacial implicit mid-point
  !======================
  function space_discr(u, dx) result(dudx)
    real(8), intent(in) :: u(:)
    real(8), intent(in) :: dx
    real(8) :: dudx(size(u))
    integer :: i, n

    n = size(u)

    ! Interior points: central difference
    do i = 2, n-1
       dudx(i) = (u(i+1) - u(i-1)) / (2.0d0*dx)
    end do

    ! Boundaries: one-sided differences
    dudx(1)   = (u(2) - u(1)) / dx
    dudx(n)   = (u(n) - u(n-1)) / dx

  end function space_discr

  !=========================================================
  ! General midpoint derivative helper
  ! y: dependent variable array (e.g. U)
  ! x: independent variable array (e.g. s or T_h)
  ! dy_dx: output array of derivatives dy/dx
  ! ds: step size for finite difference
  !=========================================================
  subroutine midpoint_derivative(y, x, dy_dx, ds)
    implicit none
    real(8), intent(in)  :: y(:), x(:)
    real(8), intent(out) :: dy_dx(size(y))
    real(8), intent(in)  :: ds
    integer :: i
    real(8) :: yp, ym, xp, xm

    do i = 1, size(y)
       ! Perturb independent variable symmetrically
       xp = x(i) + ds/2.0d0
       xm = x(i) - ds/2.0d0

       ! Approximate dependent variable at perturbed points
       ! Here we assume linear interpolation between neighbors
       if (i > 1 .and. i < size(y)) then
          yp = y(i+1)   ! forward neighbor
          ym = y(i-1)   ! backward neighbor
          dy_dx(i) = (yp - ym) / (xp - xm)
       else
          dy_dx(i) = 0.0d0   ! boundary handling
       end if
    end do
  end subroutine midpoint_derivative


end module calc_vals
