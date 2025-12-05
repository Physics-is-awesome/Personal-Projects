module calc_vals


  implicit none
contains
  subroutine calculate_variables(nx, gamma, dx, rho_h, m_h, sigma_h, e, &
                                 U, T_h, dT_h_dx, u_h, du_h_dx, s_h, T_S, &
                                 p, eta_h, deta_h_dx, ny, nz)
    ! Inputs
    real(8), intent(in) :: gamma, dx
    real(8), intent(in) :: rho_h(:,:,:), m_h(:,:,:), sigma_h(:,:,:), e(:,:,:)
    integer, intent(in) :: nx, ny, nz
    ! Outputs
    real(8), intent(out) :: U(:,:,:), T_h(:,:,:), dT_h_dx(:,:,:)
    real(8), intent(out) :: u_h(:,:,:), du_h_dx(:,:,:), s_h(:,:,:), T_S(:,:,:)
    real(8), intent(out) :: p(:,:,:), eta_h(:,:,:), deta_h_dx(:,:,:)
  
    ! Local variables
    real(8) :: x(nx,ny,nz)
    integer :: i, j, k
    




    ! Internal energy (ideal EOS placeholder)
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          U(i,j,k) = rho_h(i,j,k) ** (gamma - 1) * exp((gamma - 1) * s_h(i,j,k))
        end do
      end do
    end do

    ! Temperature via midpoint derivative (vectorized helper)
    call midpoint_derivative(U, sigma_h, T_h, ds=1.0d-3)

    ! Spatial derivative of temperature
    call midpoint_derivative(T_h, x, dT_h_dx, dx)

    ! Velocity
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          u_h(i,j,k) = m_h(i,j,k) / rho_h(i,j,k)
        end do
      end do
    end do

    ! Spatial derivative of velocity
    call midpoint_derivative(u_h, x, du_h_dx, dx)

    ! Eta/spesific entropy
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          eta_h(i,j,k) = sigma_h(i,j,k) / rho_h(i,j,k)
        end do
      end do
    end do

    ! Ratio T/s
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          T_S(i,j,k) = T_h(i,j,k) / eta(i,j,k)
        end do
      end do
    end do

    ! Pressure (ideal EOS placeholder)
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          pressure(i,j,k) = (gamma - 1) * rho_h(i,j,k) ** gamma * exp((gamma - 1) * eta(i,j,k))
        end do
      end do
    end do



    ! Spatial derivative of eta
    call midpoint_derivative(eta_h, x, deta_h_dx, dx)


  
    ! space derivative of phi

  end subroutine calculate_variables

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
    ! Inputs
    real(8), intent(in)  :: y(:,:,:), x(:,:,:)
    real(8), intent(in)  :: ds
    ! Outputs
    real(8), intent(out) :: dy_dx(:,:,:)
  
    ! Locals
    integer :: i, j, k, nx, ny, nz
    real(8) :: yp, ym, xp, xm
  
    nx = size(y,1)
    ny = size(y,2)
    nz = size(y,3)
  
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          xp = x(i,j,k) + ds/2.0d0
          xm = x(i,j,k) - ds/2.0d0
  
          if (i > 1 .and. i < nx) then
            yp = y(i+1,j,k)
            ym = y(i-1,j,k)
            dy_dx(i,j,k) = (yp - ym) / (xp - xm)
          else
            ! One-sided difference at boundaries
            if (i == 1) then
              dy_dx(i,j,k) = (y(2,j,k) - y(1,j,k)) / (x(2,j,k) - x(1,j,k))
            else if (i == nx) then
              dy_dx(i,j,k) = (y(nx,j,k) - y(nx-1,j,k)) / (x(nx,j,k) - x(nx-1,j,k))
            end if
          end if
        end do
      end do
    end do
  end subroutine midpoint_derivative




end module calc_vals
