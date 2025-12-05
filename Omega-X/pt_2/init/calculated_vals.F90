module calc_vals


  implicit none
contains
  subroutine calculate_variables(Omega)
    type(Omega-X), intent(inout) :: Omega

  
    ! Local variables
    real(8) :: x(Omega%m%nx, Omega%m%ny, Omega%m%nz)
    integer :: i, j, k
    
    ! Eta/spesific entropy
    do k = 1, Omega%m%nz
      do j = 1, Omega%m%ny
        do i = 1, Omega%m%nx
          Omega%dv%eta_h(i,j,k) = Omega%S%sigma_h(i,j,k) / Omega%S%rho_h(i,j,k)
        end do
      end do
    end do

    ! Internal energy (ideal EOS placeholder)
    do k = 1, Omega%m%nz
      do j = 1, Omega%m%ny
        do i = 1, Omega%m%nx
          Omega%dv%U(i,j,k) = Omega%S%rho_h(i,j,k) ** (Omega%const%gamma - 1) * exp((Omega%const%gamma - 1) * Omega%dv%eta(i,j,k))
        end do
      end do
    end do

    ! Temperature by internal energy derivitive of entropy via midpoint derivative 
    call midpoint_derivative(Omega%dv%U, Omega%S%sigma_h, Omega%dv%T_h, ds=1.0d-3)

    ! Spatial derivative of temperature
    call midpoint_derivative(Omega%dv%T_h, x, Omega%dv%dT_h_dx, Omega%m%dx)

    ! Velocity
    do k = 1, Omega%m%nz
      do j = 1, Omega%m%ny
        do i = 1, Omega%m%nx
          Omega%dv%u_h(i,j,k) = Omega%S%m_h(i,j,k) / Omega%S%rho_h(i,j,k)
        end do
      end do
    end do

    ! Spatial derivative of velocity
    call midpoint_derivative(Omega%dv%u_h, x, Omega%dv%du_h_dx, Omega%m%dx)



    ! Ratio T/s
    do k = 1, Omega%m%nz
      do j = 1, Omega%m%ny
        do i = 1, Omega%m%nx
          Omega%dv%T_S(i,j,k) = Omega%dv%T_h(i,j,k) / Omega%dv%eta(i,j,k)
        end do
      end do
    end do

    ! Pressure (ideal EOS placeholder)
    do k = 1, Omega%m%nz
      do j = 1, Omega%m%ny
        do i = 1, Omega%m%nx
          Omega%dv%pressure(i,j,k) = (Omega%const%gamma - 1) * Omega%S%rho_h(i,j,k) ** Omega%const%gamma * exp((Omega%const%gamma - 1) * Omega%dv%eta(i,j,k))
        end do
      end do
    end do



    ! Spatial derivative of eta
    call midpoint_derivative(Omega%dv%eta_h, x, Omega%dv%deta_h_dx, Omega%m%dx)


  
    ! space derivative of phi

  end subroutine calculate_variables



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
