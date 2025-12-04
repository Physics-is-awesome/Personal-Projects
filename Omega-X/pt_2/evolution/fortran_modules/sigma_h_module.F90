! Automatically generated module for sigma_h
! Purpose:
!   Compute the weak-form RHS for sigma_h over a 3D grid (assumed-shape arrays).
!   Evaluation is element-wise with do concurrent loops; trivial dimensions (size=1) are supported.
!
! Interface:
!   subroutine compute_sigma_h(Pr, gamma, T_h, dT_h_dx, dphi_sigma_dx, du_h_dx, phi_sigma_i, sigma_h, u_h, F_sigma_h)
!     Scalars (intent in): Pr, gamma, F_sigma_h
!     Arrays  (intent in): T_h, dT_h_dx, dphi_sigma_dx, du_h_dx, phi_sigma_i, sigma_h, u_h
!     Output  (intent out): F_sigma_h(:,:,:)
!
! Notes:
!   - Arrays are 3D assumed-shape; pass size-1 dimensions for 1D/2D.
!   - Edit equations in Python; regenerated modules adapt automatically.
module sigma_h_module
  implicit none
contains
  subroutine compute_sigma_h(Pr, gamma, T_h, dT_h_dx, dphi_sigma_dx, du_h_dx, phi_sigma_i, sigma_h, u_h, F_sigma_h)
    implicit none
    real(8), intent(in) :: Pr
    real(8), intent(in) :: gamma
    real(8), intent(in) :: T_h(:,:,:)
    real(8), intent(in) :: dT_h_dx(:,:,:)
    real(8), intent(in) :: dphi_sigma_dx(:,:,:)
    real(8), intent(in) :: du_h_dx(:,:,:)
    real(8), intent(in) :: phi_sigma_i(:,:,:)
    real(8), intent(in) :: sigma_h(:,:,:)
    real(8), intent(in) :: u_h(:,:,:)
    real(8), intent(out) :: F_sigma_h(:,:,:)
    integer :: i, j, k

    do concurrent (k = 1:size(F_sigma_h,3), j = 1:size(F_sigma_h,2), i = 1:size(F_sigma_h,1))
      F_sigma_h(i,j,k) = (Pr*(gamma - 1)*(dphi_sigma_dx(i,j,k)*sigma_h(i,j,k)*u_h(i,j,k) - du_h_dx(i,j,k)**2*phi_sigma_i(i,j,k)/T_h(i,j,k) &
      ) + dT_h_dx(i,j,k)*dphi_sigma_dx(i,j,k)*gamma/T_h(i,j,k) - dT_h_dx(i,j,k)**2*gamma* &
      phi_sigma_i(i,j,k)/T_h(i,j,k)**2)/(Pr*(gamma - 1))
    end do 

  end subroutine compute_sigma_h
end module
