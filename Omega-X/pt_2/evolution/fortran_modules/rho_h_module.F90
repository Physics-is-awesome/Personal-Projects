! Automatically generated module for rho_h
! Purpose:
!   Compute the weak-form RHS for rho_h over a 3D grid (assumed-shape arrays).
!   Evaluation is element-wise with do concurrent loops; trivial dimensions (size=1) are supported.
!
! Interface:
!   subroutine compute_rho_h(dphi_rho_dx, rho_h, u_h, F_rho_h)
!     Scalars (intent in): F_rho_h
!     Arrays  (intent in): dphi_rho_dx, rho_h, u_h
!     Output  (intent out): F_rho_h(:,:,:)
!
! Notes:
!   - Arrays are 3D assumed-shape; pass size-1 dimensions for 1D/2D.
!   - Edit equations in Python; regenerated modules adapt automatically.
module rho_h_module
  implicit none
contains
  subroutine compute_rho_h(dphi_rho_dx, rho_h, u_h, F_rho_h)
    implicit none
    real(8), intent(in) :: dphi_rho_dx(:,:,:)
    real(8), intent(in) :: rho_h(:,:,:)
    real(8), intent(in) :: u_h(:,:,:)
    real(8), intent(out) :: F_rho_h(:,:,:)
    integer :: i, j, k

    do concurrent (k = 1:size(F_rho_h,3), j = 1:size(F_rho_h,2), i = 1:size(F_rho_h,1))
      F_rho_h(i,j,k) = dphi_rho_dx(i,j,k)*rho_h(i,j,k)*u_h(i,j,k)
    end do concurrent

  end subroutine compute_rho_h
end module
