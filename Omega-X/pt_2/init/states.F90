module states
  implicit none



contains

  ! Initialize the full state arrays
  subroutine init_state(Omega)
    type(Omega-X), intent(inout) :: Omega

    integer :: ny_, nz_

    ! Defaults for missing dimensions
    ny_ = merge(Omega%m%ny, 1, present(ny))
    nz_ = merge(Omega%m%nz, 1, present(nz))

    allocate(Omega%S%rho_h(Omega%m%nx, ny_, nz_))
    allocate(Omega%S%sigma_h(Omega%m%nx, ny_, nz_))
    allocate(Omega%S%m_h(Omega%m%nx, ny_, nz_))

    Omega%S%rho_h   = 0.0d0
    Omega%S%sigma_h = 0.0d0
    Omega%S%m_h     = 0.0d0
  end subroutine init_state

 
  !================================================================
  ! Initialize momentum field with chosen distribution 
  !================================================================
  subroutine init_momentum(Omega)
    type(Omega-X), intent(inout) :: Omega


    integer :: i, j, k, ny_, nz_

    ny_ = merge(Omega%m%ny, 1, present(Omega%m%ny))
    nz_ = merge(Omega%m%nz, 1, present(Omega%m%nz))

    if (.not. allocated(Omega%S%m_h)) allocate(Omega%S%m_h(Omega%m%nx, ny_, nz_))

    if (trim(momentum_dist) == "gaussian") then
      do k = 1, nz_
        do j = 1, ny_
          do i = 1, Omega%m%nx
            Omega%S%m_h(i,j,k) = normal(Omega%s_int%momentum_mean, Omega%s_int%momentum_var)
          end do
        end do
      end do
    else if (trim(Omega%s_int%momentum_dist) == "uniform") then
      Omega%S%m_h = Omega%s_int%momentum_mean
    end if
  end subroutine init_momentum

  !================================================================
  ! Initialize entropy field with chosen distribution 
  !================================================================
  subroutine init_entropy(Omega)
    type(Omega-X), intent(inout) :: Omega

    integer :: i, j, k, ny_, nz_

    ny_ = merge(Omega%m%ny, 1, present(Omega%m%ny))
    nz_ = merge(Omega%m%nz, 1, present(Omega%m%nz))

    if (.not. allocated(Omega%S%sigma_h)) allocate(Omega%S%sigma_h(Omega%m%nx, ny_, nz_))

    if (trim(Omega%s_int%entropy_dist) == "gaussian") then
      do k = 1, nz_
        do j = 1, ny_
          do i = 1, Omega%m%nx
            Omega%S%sigma_h(i,j,k) = normal(Omega%s_int%entropy_mean, Omega%s%int%entropy_var)
          end do
        end do
      end do
    else if (trim(Omega%s_int%entropy_dist) == "uniform") then
      Omega%S%sigma_h = Omega%s_int%entropy_mean
    end if
  end subroutine init_entropy

  !================================================================
  ! Initialize mass field with chosen distribution 
  !================================================================
  subroutine init_mass(Omega)
    type(Omega-X), intent(inout) :: Omega


    integer :: i, j, k, ny_, nz_

    ny_ = merge(Omega%m%ny, 1, present(Omega%m%ny))
    nz_ = merge(Omega%m%nz, 1, present(Omega%m%nz))

    if (.not. allocated(Omega%S%rho_h)) allocate(Omega%S%rho_h(Omega%m%nx, ny_, nz_))

    if (trim(Omega%s_int%mass_dist) == "gaussian") then
      do k = 1, nz_
        do j = 1, ny_
          do i = 1, Omega%m$nx
            Omega%S%rho_h(i,j,k) = normal(Omega%s_int%mass_mean, Omega%s_int%mass_var)
          end do
        end do
      end do
    else if (trim(Omega%s_int%mass_dist) == "uniform") then
      Omega%S%rho_h = Omega%s_int%mass_mean
    end if
  end subroutine init_mass

!===============================================================
! function for normal distribution(without special start)
!===============================================================
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


