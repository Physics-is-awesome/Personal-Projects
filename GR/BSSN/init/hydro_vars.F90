module hydro_vars_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: NCONS, ID, ISX, ISY, ISZ, ITAU
  public :: NPRIM, IRHO, IVX, IVY, IVZ, IEPS
  public :: alloc_cons, alloc_prim

  integer, parameter :: NCONS = 5
  integer, parameter :: ID   = 1
  integer, parameter :: ISX  = 2
  integer, parameter :: ISY  = 3
  integer, parameter :: ISZ  = 4
  integer, parameter :: ITAU = 5

  integer, parameter :: NPRIM = 5
  integer, parameter :: IRHO = 1
  integer, parameter :: IVX  = 2
  integer, parameter :: IVY  = 3
  integer, parameter :: IVZ  = 4
  integer, parameter :: IEPS = 5

contains

  subroutine alloc_cons(u, Nx, Nz)
    real(dp), allocatable, intent(inout) :: u(:,:,:)
    integer, intent(in) :: Nx, Nz
    allocate(u(Nx, Nz, NCONS))
    u = 0.0_dp
  end subroutine alloc_cons

  subroutine alloc_prim(u, Nx, Nz)
    real(dp), allocatable, intent(inout) :: u(:,:,:)
    integer, intent(in) :: Nx, Nz
    allocate(u(Nx, Nz, NPRIM))
    u = 0.0_dp
  end subroutine alloc_prim

end module hydro_vars_mod
