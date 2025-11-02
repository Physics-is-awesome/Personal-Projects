program states
  use read_config
  implicit none
  
#include "../config/dim_config.h"
#if dim == 1
  real, allocatable :: mass(:), temp(:), entropy(:), momentum(:)

#elif dim == 2
  real, allocatable :: mass(:, :), temp(:, :), entropy(:, :), momentum(:, :)
#elif dim == 3
  real, allocatable :: mass(:, :, :), temp(:, :, :), entropy(:, :, :), momentum(:, :, :)
#endif
  call read_file()
  call init_temp()
  print*, temp
contains 

  
    
  subroutine init_temp()
    integer :: i, j, k
#if dim == 1
    if (trim(temp_dist) == "gaussian") then 
      do i=1, nx
        temp(i) = normal(temp_mean, temp_var)
      end do
    end if

#elif dim == 2 
    if (trim(temp_dist) == "gaussian") then 
      do j=1, ny
        do i=1, nx
          
          temp(i,j) = normal(temp_mean, temp_var)
        end do
      end do
    end if

#elif dim == 3
    allocate(temp(nx, ny, nz))
    print*, "dim is equal to 3"
    if (trim(temp_dist) == "gaussian") then 
      print*, "it is gaussian"
      do k=1, nz
        do j=1, ny
          do i=1, nx
            temp(i,j, k) = normal(temp_mean, temp_var)
            print*, "it did it"
          end do
        end do
      end do
    end if
#endif
  end subroutine init_temp
      
  
! function for normal distribution(without special start)
  function normal(mu, sigma) result(val)
    real(8), intent(in) :: mu, sigma
    real(8) :: val
    real(8) :: u1, u2, r, theta

    call random_number(u1)
    call random_number(u2)

    if (u1 < 1.0d-12) u1 = 1.0d-12   ! avoid log(0)

    r = sqrt(-2.0d0 * log(u1))
    theta = 2.0d0 * acos(-1.0d0) * u2

    val = mu + sigma * r * cos(theta)
  end function normal
end program states
