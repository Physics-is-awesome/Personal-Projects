! Closed first-order state inventory for the 24-variable BSSN/gauge state.
!
! The existing fo_vars_mod is retained for compatibility with its verified
! reduction-wrapper tests.  This module is the formulation-migration layout:
! every evolved field has an independent x/y/z derivative slot.  A complete
! RHS must evolve these slots from the differentiated first-order equations;
! finite-differencing the legacy RHS is not sufficient.
module fo_complete_vars_mod
  use kinds_mod, only: dp
  use vars_mod, only: NVARS
  use params_mod, only: Nx, Nz, dx, dz, dy
  use grid_mod, only: xg
  use cartoon_mod, only: get_neighbor
  implicit none
  private
  public :: NFO_COMPLETE_VARS, FO_BASE_OFFSET, FO_DERIV_OFFSET
  public :: FO_STATUS_OK, FO_STATUS_Y_UNSUPPORTED
  public :: fo_complete_index, alloc_fo_complete_state
  public :: pack_fo_complete_state, unpack_fo_complete_state
  public :: complete_reduction_constraint_norms

  integer, parameter :: NFO_COMPLETE_VARS = NVARS + 3*NVARS
  integer, parameter :: FO_BASE_OFFSET = 0
  integer, parameter :: FO_DERIV_OFFSET = NVARS
  integer, parameter :: FO_STATUS_OK = 0
  integer, parameter :: FO_STATUS_Y_UNSUPPORTED = 1

contains

  pure integer function fo_complete_index(base_var, direction) result(index)
    integer, intent(in) :: base_var, direction
    if (base_var < 1 .or. base_var > NVARS) error stop 'invalid first-order base variable'
    if (direction < 1 .or. direction > 3) error stop 'invalid first-order direction'
    index = FO_DERIV_OFFSET + (direction-1)*NVARS + base_var
  end function fo_complete_index

  subroutine alloc_fo_complete_state(u, nx, nz)
    real(dp), allocatable, intent(out) :: u(:,:,:)
    integer, intent(in) :: nx, nz
    allocate(u(nx, nz, NFO_COMPLETE_VARS))
    u = 0.0_dp
  end subroutine alloc_fo_complete_state

  subroutine pack_fo_complete_state(u, q, status)
    real(dp), intent(in) :: u(:,:,:)
    real(dp), intent(out) :: q(:,:,:)
    integer, intent(out), optional :: status
    integer :: i, k, v
    real(dp) :: vp(NVARS), vm(NVARS)

    if (size(u,1) /= size(q,1) .or. size(u,2) /= size(q,2) .or. &
        size(u,3) < NVARS .or. size(q,3) < NFO_COMPLETE_VARS) then
      error stop 'pack_fo_complete_state: inconsistent array shape'
    end if
    q = 0.0_dp
    q(:,:,1:NVARS) = u(:,:,1:NVARS)
    if (present(status)) status = FO_STATUS_OK

    ! Match the reduction wrapper's second-order centered interior convention.
    do k = 2, size(u,2)-1
      do i = 2, size(u,1)-1
        do v = 1, NVARS
          q(i,k,fo_complete_index(v,1)) = &
            (u(i+1,k,v)-u(i-1,k,v))/(2.0_dp*dx)
          q(i,k,fo_complete_index(v,3)) = &
            (u(i,k+1,v)-u(i,k-1,v))/(2.0_dp*dz)
        end do

        ! The stored domain is y=0.  Cartoon supplies the two physical
        ! neighbors and performs the required vector/tensor rotations.
        if (allocated(xg)) then
          vp = get_neighbor(u, i, k, 0, 0, dy)
          vm = get_neighbor(u, i, k, 0, 0, -dy)
          do v = 1, NVARS
            q(i,k,fo_complete_index(v,2)) = (vp(v)-vm(v))/(2.0_dp*dy)
          end do
        else if (present(status)) then
          status = FO_STATUS_Y_UNSUPPORTED
        end if
      end do
    end do
  end subroutine pack_fo_complete_state

  subroutine unpack_fo_complete_state(q, u)
    real(dp), intent(in) :: q(:,:,:)
    real(dp), intent(out) :: u(:,:,:)
    if (size(q,1) /= size(u,1) .or. size(q,2) /= size(u,2) .or. &
        size(q,3) < NFO_COMPLETE_VARS) error stop &
        'unpack_fo_complete_state: inconsistent array shape'
    if (size(u,3) == NVARS) then
      u = q(:,:,1:NVARS)
    else if (size(u,3) >= NFO_COMPLETE_VARS) then
      u(:,:,1:NFO_COMPLETE_VARS) = q(:,:,1:NFO_COMPLETE_VARS)
    else
      error stop 'unpack_fo_complete_state: output must have 24 or 96 fields'
    end if
  end subroutine unpack_fo_complete_state

  subroutine complete_reduction_constraint_norms(q, norms, status)
    real(dp), intent(in) :: q(:,:,:)
    real(dp), intent(out) :: norms(3)
    integer, intent(out), optional :: status
    real(dp), allocatable :: base(:,:,:), expected(:,:,:)
    integer :: d, v, i, k, st

    if (size(q,3) < NFO_COMPLETE_VARS) error stop &
      'complete_reduction_constraint_norms: incomplete state'
    allocate(base(size(q,1),size(q,2),NVARS), expected(size(q,1),size(q,2),NFO_COMPLETE_VARS))
    call unpack_fo_complete_state(q, base)
    call pack_fo_complete_state(base, expected, st)
    norms = 0.0_dp
    do d = 1, 3
      do v = 1, NVARS
        do k = 1, size(q,2)
          do i = 1, size(q,1)
            norms(d) = max(norms(d), abs(q(i,k,fo_complete_index(v,d)) - &
              expected(i,k,fo_complete_index(v,d))))
          end do
        end do
      end do
    end do
    if (present(status)) status = st
    deallocate(base, expected)
  end subroutine complete_reduction_constraint_norms

end module fo_complete_vars_mod
