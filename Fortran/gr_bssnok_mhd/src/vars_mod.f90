module vars_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: NVARS
  public :: IPHI, IGXX, IGXY, IGXZ, IGYY, IGYZ, IGZZ
  public :: IK, IAXX, IAXY, IAXZ, IAYY, IAYZ, IAZZ
  public :: IGTX, IGTY, IGTZ
  public :: IALPHA, IBETAX, IBETAY, IBETAZ
  public :: IBX, IBY, IBZ
  public :: alloc_state, is_tensor_var, is_vector_var

  integer, parameter :: NVARS = 24

  integer, parameter :: IPHI   = 1
  integer, parameter :: IGXX   = 2
  integer, parameter :: IGXY   = 3
  integer, parameter :: IGXZ   = 4
  integer, parameter :: IGYY   = 5
  integer, parameter :: IGYZ   = 6
  integer, parameter :: IGZZ   = 7
  integer, parameter :: IK     = 8
  integer, parameter :: IAXX   = 9
  integer, parameter :: IAXY   = 10
  integer, parameter :: IAXZ   = 11
  integer, parameter :: IAYY   = 12
  integer, parameter :: IAYZ   = 13
  integer, parameter :: IAZZ   = 14
  integer, parameter :: IGTX   = 15
  integer, parameter :: IGTY   = 16
  integer, parameter :: IGTZ   = 17
  integer, parameter :: IALPHA = 18
  integer, parameter :: IBETAX = 19
  integer, parameter :: IBETAY = 20
  integer, parameter :: IBETAZ = 21
  integer, parameter :: IBX    = 22
  integer, parameter :: IBY    = 23
  integer, parameter :: IBZ    = 24

contains

  subroutine alloc_state(u, Nx, Nz)
    real(dp), allocatable, intent(inout) :: u(:,:,:)
    integer, intent(in) :: Nx, Nz
    allocate(u(Nx, Nz, NVARS))
    u = 0.0_dp
  end subroutine alloc_state

  ! Used by cartoon_mod to know how to rotate each field under the
  ! z-axis rotation: tensor fields (rank-2 symmetric, xx/xy/xz/yy/yz/zz),
  ! vector fields (x/y/z components), or scalars (unchanged).
  logical function is_tensor_var(ivar) result(r)
    integer, intent(in) :: ivar
    r = (ivar == IGXX) .or. (ivar == IAXX)
  end function is_tensor_var

  logical function is_vector_var(ivar) result(r)
    integer, intent(in) :: ivar
    r = (ivar == IGTX) .or. (ivar == IBETAX) .or. (ivar == IBX)
  end function is_vector_var

end module vars_mod
