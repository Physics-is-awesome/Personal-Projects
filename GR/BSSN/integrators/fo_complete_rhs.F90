! Experimental complete-state RHS for the 96-variable first-order layout.
!
! This module is intentionally not wired into a production integrator.  The
! 24 base fields use the verified legacy BSSN RHS.  x/z derivative slots are
! evolved by differentiating that RHS with the same centered operator used by
! the complete packer.  An independent y derivative RHS is not available in
! the axisymmetric cartoon grid; callers must inspect status (or omit it only
! when they want an error stop).
module fo_complete_rhs_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz
  use vars_mod, only: NVARS
  use fo_complete_vars_mod, only: NFO_COMPLETE_VARS, FO_DERIV_OFFSET, &
    FO_STATUS_OK, FO_STATUS_Y_UNSUPPORTED, fo_complete_index, &
    unpack_fo_complete_state
  use bssn_rhs_mod, only: compute_rhs_interior
  implicit none
  private
  public :: compute_complete_state_rhs, compute_complete_derivative_rhs

contains

  subroutine compute_complete_state_rhs(q, rhs, status)
    real(dp), intent(in) :: q(:,:,:)
    real(dp), intent(out) :: rhs(:,:,:)
    integer, intent(out), optional :: status
    real(dp), allocatable :: u(:,:,:), ru(:,:,:)
    integer :: i, k, v, st

    if (size(q,1) /= Nx .or. size(q,2) /= Nz .or. size(q,3) < NFO_COMPLETE_VARS .or. &
        size(rhs,1) /= Nx .or. size(rhs,2) /= Nz .or. size(rhs,3) < NFO_COMPLETE_VARS) &
      error stop 'compute_complete_state_rhs: expected Nx,Nz,96 arrays'

    allocate(u(Nx,Nz,NVARS), ru(Nx,Nz,NVARS))
    call unpack_fo_complete_state(q, u)
    ru = 0.0_dp
    call compute_rhs_interior(u, ru)
    rhs = 0.0_dp
    rhs(:,:,1:NVARS) = ru

    ! This is the explicitly named differentiated-field API.  It fills the
    ! supported x/z slots and reports that y slots require a future 3-D/cartoon
    ! differentiated operator rather than fabricating values.
    call compute_complete_derivative_rhs(u, ru, rhs, st)
    if (present(status)) then
      status = st
    else if (st /= FO_STATUS_OK) then
      error stop 'compute_complete_state_rhs: y derivative RHS unsupported'
    end if
    deallocate(u, ru)
  end subroutine compute_complete_state_rhs

  subroutine compute_complete_derivative_rhs(u, ru, rhs, status)
    real(dp), intent(in) :: u(:,:,:), ru(:,:,:)
    real(dp), intent(inout) :: rhs(:,:,:)
    integer, intent(out), optional :: status
    integer :: i, k, v
    logical :: bad_shape

    bad_shape = size(u,1) /= Nx .or. size(u,2) /= Nz .or. size(u,3) < NVARS .or. &
      size(ru,1) /= Nx .or. size(ru,2) /= Nz .or. size(ru,3) < NVARS .or. &
      size(rhs,1) /= Nx .or. size(rhs,2) /= Nz .or. size(rhs,3) < NFO_COMPLETE_VARS
    if (bad_shape) error stop 'compute_complete_derivative_rhs: inconsistent array shape'
    rhs(:,:,FO_DERIV_OFFSET+1:NFO_COMPLETE_VARS) = 0.0_dp

    do k = 2, Nz-1
      do i = 2, Nx-1
        do v = 1, NVARS
          rhs(i,k,fo_complete_index(v,1)) = (ru(i+1,k,v)-ru(i-1,k,v))/(2.0_dp*dx)
          rhs(i,k,fo_complete_index(v,3)) = (ru(i,k+1,v)-ru(i,k-1,v))/(2.0_dp*dz)
        end do
      end do
    end do
    if (present(status)) then
      status = FO_STATUS_Y_UNSUPPORTED
    else
      error stop 'compute_complete_derivative_rhs: y derivative RHS unsupported'
    end if
  end subroutine compute_complete_derivative_rhs

end module fo_complete_rhs_mod
