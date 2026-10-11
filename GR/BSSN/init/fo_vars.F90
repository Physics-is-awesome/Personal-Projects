module fo_vars_mod
  use kinds_mod, only: dp
  use vars_mod, only: NVARS
  implicit none
  ! The 57-field layout is (24 legacy fields, then spatial derivatives).
  ! The axisymmetric legacy driver supplies x/z derivatives; y slots are
  ! retained for layout compatibility and are not inferred by this module.
  private
  public :: NFO_VARS, alloc_fo_state
  public :: IPHI_FO, IDPHI_FO, IDGT_FO, IDALPHA_FO, IDBETA_FO

  integer, parameter :: NFO_VARS = NVARS + 3 + 18 + 3 + 9
  integer, parameter :: IPHI_FO = 1
  integer, parameter :: IDPHI_FO(3) = [25,26,27]
  integer, parameter :: IDGT_FO(3,6) = reshape([28,29,30,31,32,33, &
                                                 34,35,36,37,38,39, &
                                                 40,41,42,43,44,45], [3,6])
  integer, parameter :: IDALPHA_FO(3) = [46,47,48]
  integer, parameter :: IDBETA_FO(3,3) = reshape([49,50,51,52,53,54,55,56,57], [3,3])

contains

  subroutine alloc_fo_state(u, nx, nz)
    real(dp), allocatable, intent(out) :: u(:,:,:)
    integer, intent(in) :: nx, nz
    allocate(u(nx, nz, NFO_VARS))
    u = 0.0_dp
  end subroutine alloc_fo_state

end module fo_vars_mod
