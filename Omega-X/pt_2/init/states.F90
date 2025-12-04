module states
  implicit none

  type :: State
    real(8), allocatable :: rho(:,:,:)
    real(8), allocatable :: T(:,:,:)
    real(8), allocatable :: sigma(:,:,:)
    real(8), allocatable :: m(:,:,:)
  end type State

contains

  ! Initialize the full state arrays
  subroutine init_state(s, nx, ny, nz)
    type(State), intent(inout) :: s
    integer, intent(in) :: nx
    integer, intent(in), optional :: ny, nz
    integer :: ny_, nz_

    ! Defaults for missing dimensions
    ny_ = merge(ny, 1, present(ny))
    nz_ = merge(nz, 1, present(nz))

    allocate(s%rho(nx, ny_, nz_))
    allocate(s%T(nx, ny_, nz_))
    allocate(s%sigma(nx, ny_, nz_))
    allocate(s%m(nx, ny_, nz_))

    s%rho   = 0.0d0
    s%T     = 0.0d0
    s%sigma = 0.0d0
    s%m     = 0.0d0
  end subroutine init_state

  !================================================================
  ! Initialize temperature field with chosen distribution 
  !================================================================
  subroutine init_temp(s, nx, ny, nz, temp_mean, temp_var, temp_dist)
    type(State), intent(inout) :: s
    integer, intent(in) :: nx
    integer, intent(in), optional :: ny, nz
    real(8), intent(in) :: temp_mean, temp_var
    character(len=*), intent(in) :: temp_dist

    integer :: i, j, k, ny_, nz_

    ny_ = merge(ny, 1, present(ny))
    nz_ = merge(nz, 1, present(nz))

    if (.not. allocated(s%T)) allocate(s%T(nx, ny_, nz_))

    if (trim(temp_dist) == "gaussian") then
      do k = 1, nz_
        do j = 1, ny_
          do i = 1, nx
            s%T_h(i,j,k) = normal(temp_mean, temp_var)
          end do
        end do
      end do
    else if (trim(temp_dist) == "uniform") then
      s%T_h = temp_mean
    end if
  end subroutine init_temp


  !================================================================
  ! Initialize momentum field with chosen distribution 
  !================================================================
  subroutine init_momentum(s, nx, ny, nz, momentum_mean, momentum_var, momentum_dist)
    type(State), intent(inout) :: s
    integer, intent(in) :: nx
    integer, intent(in), optional :: ny, nz
    real(8), intent(in) :: momentum_mean, momentum_var
    character(len=*), intent(in) :: momentum_dist

    integer :: i, j, k, ny_, nz_

    ny_ = merge(ny, 1, present(ny))
    nz_ = merge(nz, 1, present(nz))

    if (.not. allocated(s%T)) allocate(s%m_h(nx, ny_, nz_))

    if (trim(temp_dist) == "gaussian") then
      do k = 1, nz_
        do j = 1, ny_
          do i = 1, nx
            s%m_h(i,j,k) = normal(momentum_mean, momentum_var)
          end do
        end do
      end do
    else if (trim(momentum_dist) == "uniform") then
      s%m_h = momentum_mean
    end if
  end subroutine init_momentum

  !================================================================
  ! Initialize entropy field with chosen distribution 
  !================================================================
  subroutine init_entropy(s, nx, ny, nz, entropy_mean, entropy_var, entropy_dist)
    type(State), intent(inout) :: s
    integer, intent(in) :: nx
    integer, intent(in), optional :: ny, nz
    real(8), intent(in) :: entropy_mean, entropy_var
    character(len=*), intent(in) :: entropy_dist

    integer :: i, j, k, ny_, nz_

    ny_ = merge(ny, 1, present(ny))
    nz_ = merge(nz, 1, present(nz))

    if (.not. allocated(s%T)) allocate(s%sigma_h(nx, ny_, nz_))

    if (trim(entropy_dist) == "gaussian") then
      do k = 1, nz_
        do j = 1, ny_
          do i = 1, nx
            s%sigma_h(i,j,k) = normal(entropy_mean, entropy_var)
          end do
        end do
      end do
    else if (trim(entropy_dist) == "uniform") then
      s%sigma_h = entropy_mean
    end if
  end subroutine init_entropy

  !================================================================
  ! Initialize mass field with chosen distribution 
  !================================================================
  subroutine init_mass(s, nx, ny, nz, mass_mean, mass_var, mass_dist)
    type(State), intent(inout) :: s
    integer, intent(in) :: nx
    integer, intent(in), optional :: ny, nz
    real(8), intent(in) :: mass_mean, mass_var
    character(len=*), intent(in) :: mass_dist

    integer :: i, j, k, ny_, nz_

    ny_ = merge(ny, 1, present(ny))
    nz_ = merge(nz, 1, present(nz))

    if (.not. allocated(s%rho_h)) allocate(s%rho_h(nx, ny_, nz_))

    if (trim(mass_dist) == "gaussian") then
      do k = 1, nz_
        do j = 1, ny_
          do i = 1, nx
            s%rho_h(i,j,k) = normal(mass_mean, mass_var)
          end do
        end do
      end do
    else if (trim(rho_h_dist) == "uniform") then
      s%rho_h = rho_h_mean
    end if
  end subroutine init_mass


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


