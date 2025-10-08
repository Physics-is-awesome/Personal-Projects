module states_1d
  use mesh_1d
  implicit none
  private
  public :: m_h, rho_h, sigma_h, eta_h, T_h
  public :: initialize_states

  ! Field arrays
  real(8), allocatable :: m_h(:)       ! Momentum density
  real(8), allocatable :: rho_h(:)     ! Mass density
  real(8), allocatable :: sigma_h(:)   ! Entropy density
  real(8), allocatable :: eta_h(:)     ! Specific entropy (diagnostic)
  real(8), allocatable :: T_h(:)       ! Temperature (diagnostic)
  real(8)  :: gamma, Pr, Re

contains
  subroutine init_constants()
    gamma = 1.4
    Pr = 0.71
    Re = 1000.0
  end subroutine init_constants
  subroutine initialize_states()
    implicit none
    integer :: i

    ! Allocate fields
    allocate(m_h(N))
    allocate(rho_h(N))
    allocate(sigma_h(N))
    allocate(eta_h(N))
    allocate(T_h(N))

    ! Set initial conditions (example: Gaussian density bump)
    do i = 1, N
      rho_h(i) = 1.0d0 + 0.2d0 * exp(-100.0d0 * (x_nodes(i) - 0.5d0)**2)
      eta_h(i) = 1.0d0
      sigma_h(i) = rho_h(i) * eta_h(i)
      m_h(i) = 0.0d0
      T_h(i) = (0.4d0) * eta_h(i)  ! Ideal gas ! Uses EOS
    end do

  end subroutine initialize_states

end module states_1d

