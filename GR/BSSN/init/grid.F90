module grid_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz
  implicit none
  private
  public :: xg, zg, setup_grid

  real(dp), allocatable :: xg(:)   ! x(1) = 0, x(Nx) = xmax
  real(dp), allocatable :: zg(:)   ! z(1) = -zmax, z(Nz) = +zmax

contains

  subroutine setup_grid() ! cartoon-method
    integer :: i, k
    allocate(xg(Nx), zg(Nz))
    do i = 1, Nx
      xg(i) = real(i - 1, dp) * dx  ! starts at 0
    end do
    do k = 1, Nz
      zg(k) = -0.5_dp * real(Nz - 1, dp) * dz + real(k - 1, dp) * dz  ! centered around 0
    end do
  end subroutine setup_grid

end module grid_mod
