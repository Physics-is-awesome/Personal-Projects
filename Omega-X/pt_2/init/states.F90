module states
  use read_config
  implicit none
  
#include "../config/dim_config.h"
#if dim == 1
  real(8), allocatable :: mass(:), temp(:), entropy(:), momentum(:)

#elif dim == 2
  real(8), allocatable :: mass(:, :), temp(:, :), entropy(:, :), momentum(:, :)
#elif dim == 3
  real(8), allocatable :: mass(:, :, :), temp(:, :, :), entropy(:, :, :), momentum(:, :, :)
#endif
  call init_temp()
  call init_momentum()
  call init_entropy()
contains 

 
  ! ----------------------------------------
  ! Tempature 
  ! ----------------------------------------
  subroutine init_temp()
    integer :: i, j, k
#if dim == 1
    allocate(temp(nx))
    if (trim(temp_dist) == "gaussian") then 
      do i=1, nx
        temp(i) = normal(temp_mean, temp_var)
      end do
    end if

#elif dim == 2 
    allocate(temp(nx, ny)
    if (trim(temp_dist) == "gaussian") then 
      do j=1, ny
        do i=1, nx
          
          temp(i,j) = normal(temp_mean, temp_var)
        end do
      end do
    end if

#elif dim == 3
    allocate(temp(nx, ny, nz))
    if (trim(temp_dist) == "gaussian") then 
      do k=1, nz
        do j=1, ny
          do i=1, nx
            temp(i,j, k) = normal(temp_mean, temp_var)
          end do
        end do
      end do
    end if
#endif
  end subroutine init_temp
 
  ! ----------------------------------------
  ! mass 
  ! ----------------------------------------
  subroutine init_mass()
    integer :: i, j, k
#if dim == 1
    allocate(mass(nx))
    if (trim(mass_dist) == "gaussian") then 
      do i=1, nx
        mass(i) = normal(mass_mean, mass_var)
      end do
    end if

#elif dim == 2 
    allocate(mass(nx, ny)
    if (trim(mass_dist) == "gaussian") then 
      do j=1, ny
        do i=1, nx
          
          mass(i,j) = normal(mass_mean, mass_var)
        end do
      end do
    end if

#elif dim == 3
    allocate(mass(nx, ny, nz))
    if (trim(mass_dist) == "gaussian") then 
      do k=1, nz
        do j=1, ny
          do i=1, nx
            temp(i,j, k) = normal(mass_mean, mass_var)
          end do
        end do
      end do
    end if
#endif
  end subroutine init_mass


 
  ! ----------------------------------------
  ! Momentum 
  ! ----------------------------------------
  subroutine init_momentum()
    integer :: i, j, k
#if dim == 1
    allocate(momentum(nx))
    if (trim(momentum_dist) == "gaussian") then 
      do i=1, nx
        momentum(i) = normal(momentum_mean, momentum_var)
      end do
    end if

#elif dim == 2 
    allocate(momentum(nx, ny)
    if (trim(momentum_dist) == "gaussian") then 
      do j=1, ny
        do i=1, nx
          
          momentum(i,j) = normal(momentum_mean, momentum_var)
        end do
      end do
    end if

#elif dim == 3
    allocate(momentum(nx, ny, nz))
    if (trim(momentum_dist) == "gaussian") then 
      do k=1, nz
        do j=1, ny
          do i=1, nx
            momentum(i,j, k) = normal(momentum_mean, momentum_var)
          end do
        end do
      end do
    end if
#endif
  end subroutine init_momentum

  ! ----------------------------------------
  ! Entropy 
  ! ----------------------------------------
  subroutine init_entropy()
    integer :: i, j, k
#if dim == 1
    allocate(entropy(nx))
    if (trim(entropy_dist) == "gaussian") then 
      do i=1, nx
        entropy(i) = normal(entropy_mean, entropy_var)
      end do
    end if

#elif dim == 2 
    allocate(entropy(nx, ny)
    if (trim(entropy_dist) == "gaussian") then 
      do j=1, ny
        do i=1, nx
          
          entropy(i,j) = normal(entropy_mean, entropy_var)
        end do
      end do
    end if

#elif dim == 3
    allocate(entropy(nx, ny, nz))
    if (trim(entropy_dist) == "gaussian") then 
      do k=1, nz
        do j=1, ny
          do i=1, nx
            entropy(i,j, k) = normal(entropy_mean, entropy_var)
          end do
        end do
      end do
    end if
#endif
  end subroutine init_entropy

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
end module states


