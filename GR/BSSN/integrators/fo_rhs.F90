! First-order wrapper around the legacy 24-variable BSSN operator.
!
! This is deliberately a reduction wrapper, not a new characteristic
! formulation.  The evolution of the derivative fields is obtained by
! differentiating the legacy RHS and adding algebraic reduction damping.
module fo_rhs_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz
  use vars_mod, only: NVARS
  use fo_vars_mod, only: NFO_VARS, IDPHI_FO, IDGT_FO, IDALPHA_FO, IDBETA_FO
  use bssn_rhs_mod, only: compute_rhs_interior
  implicit none
  private
  public :: pack_fo_state, unpack_fo_state, compute_fo_rhs
  ! compute_fo_rhs has the same setup contract as compute_rhs_interior:
  ! setup_grid, excision ghost/mask initialization, and derivative stencils
  ! must be completed by the caller.

contains

  subroutine pack_fo_state(u, q)
    real(dp), intent(in)  :: u(:,:,:)
    real(dp), intent(out) :: q(:,:,:)
    integer :: i, k, v, a
    q = 0.0_dp
    q(:,:,1:NVARS) = u(:,:,1:NVARS)
    do k = 2, Nz-1
      do i = 2, Nx-1
        q(i,k,IDPHI_FO(1)) = (u(i+1,k,1)-u(i-1,k,1))/(2.0_dp*dx)
        q(i,k,IDPHI_FO(3)) = (u(i,k+1,1)-u(i,k-1,1))/(2.0_dp*dz)
        do v = 1, 6
          q(i,k,IDGT_FO(1,v)) = (u(i+1,k,v+1)-u(i-1,k,v+1))/(2.0_dp*dx)
          q(i,k,IDGT_FO(3,v)) = (u(i,k+1,v+1)-u(i,k-1,v+1))/(2.0_dp*dz)
        end do
        q(i,k,IDALPHA_FO(1)) = (u(i+1,k,18)-u(i-1,k,18))/(2.0_dp*dx)
        q(i,k,IDALPHA_FO(3)) = (u(i,k+1,18)-u(i,k-1,18))/(2.0_dp*dz)
        do a = 1, 3
          q(i,k,IDBETA_FO(1,a)) = (u(i+1,k,18+a)-u(i-1,k,18+a))/(2.0_dp*dx)
          q(i,k,IDBETA_FO(3,a)) = (u(i,k+1,18+a)-u(i,k-1,18+a))/(2.0_dp*dz)
        end do
      end do
    end do
  end subroutine pack_fo_state

  subroutine unpack_fo_state(q, u)
    real(dp), intent(in)  :: q(:,:,:)
    real(dp), intent(out) :: u(:,:,:)
    u = q(:,:,1:NVARS)
  end subroutine unpack_fo_state

  subroutine compute_fo_rhs(q, rhs, reduction_damping)
    real(dp), intent(in) :: q(:,:,:)
    real(dp), intent(out) :: rhs(:,:,:)
    real(dp), intent(in), optional :: reduction_damping
    real(dp), allocatable :: u(:,:,:), ru(:,:,:)
    real(dp) :: sigma
    integer :: i, k, v, a

    sigma = 0.0_dp
    if (present(reduction_damping)) sigma = reduction_damping
    allocate(u(Nx,Nz,NVARS), ru(Nx,Nz,NVARS))
    u = q(:,:,1:NVARS)
    ru = 0.0_dp
    call compute_rhs_interior(u, ru)
    rhs = 0.0_dp
    rhs(:,:,1:NVARS) = ru

    ! Only x and z derivatives are represented by the axisymmetric legacy
    ! driver.  The y slots remain explicit zero rather than pretending that
    ! cartoon data provide an independent y derivative.
    do k = 2, Nz-1
      do i = 2, Nx-1
        rhs(i,k,IDPHI_FO(1)) = (ru(i+1,k,1)-ru(i-1,k,1))/(2.0_dp*dx) + &
          sigma*((u(i+1,k,1)-u(i-1,k,1))/(2.0_dp*dx)-q(i,k,IDPHI_FO(1)))
        rhs(i,k,IDPHI_FO(3)) = (ru(i,k+1,1)-ru(i,k-1,1))/(2.0_dp*dz) + &
          sigma*((u(i,k+1,1)-u(i,k-1,1))/(2.0_dp*dz)-q(i,k,IDPHI_FO(3)))
        do v = 1, 6
          rhs(i,k,IDGT_FO(1,v)) = (ru(i+1,k,v+1)-ru(i-1,k,v+1))/(2.0_dp*dx) + &
            sigma*((u(i+1,k,v+1)-u(i-1,k,v+1))/(2.0_dp*dx)-q(i,k,IDGT_FO(1,v)))
          rhs(i,k,IDGT_FO(3,v)) = (ru(i,k+1,v+1)-ru(i,k-1,v+1))/(2.0_dp*dz) + &
            sigma*((u(i,k+1,v+1)-u(i,k-1,v+1))/(2.0_dp*dz)-q(i,k,IDGT_FO(3,v)))
        end do
        rhs(i,k,IDALPHA_FO(1)) = (ru(i+1,k,18)-ru(i-1,k,18))/(2.0_dp*dx) + &
          sigma*((u(i+1,k,18)-u(i-1,k,18))/(2.0_dp*dx)-q(i,k,IDALPHA_FO(1)))
        rhs(i,k,IDALPHA_FO(3)) = (ru(i,k+1,18)-ru(i,k-1,18))/(2.0_dp*dz) + &
          sigma*((u(i,k+1,18)-u(i,k-1,18))/(2.0_dp*dz)-q(i,k,IDALPHA_FO(3)))
        do a = 1, 3
          rhs(i,k,IDBETA_FO(1,a)) = (ru(i+1,k,18+a)-ru(i-1,k,18+a))/(2.0_dp*dx) + &
            sigma*((u(i+1,k,18+a)-u(i-1,k,18+a))/(2.0_dp*dx)-q(i,k,IDBETA_FO(1,a)))
          rhs(i,k,IDBETA_FO(3,a)) = (ru(i,k+1,18+a)-ru(i,k-1,18+a))/(2.0_dp*dz) + &
            sigma*((u(i,k+1,18+a)-u(i,k-1,18+a))/(2.0_dp*dz)-q(i,k,IDBETA_FO(3,a)))
        end do
      end do
    end do
    deallocate(u, ru)
  end subroutine compute_fo_rhs

end module fo_rhs_mod
