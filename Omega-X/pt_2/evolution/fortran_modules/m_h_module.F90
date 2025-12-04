! Automatically generated module for m_h
! Purpose:
!   Compute the weak-form RHS for m_h over a 3D grid (assumed-shape arrays).
!   Evaluation is element-wise with do concurrent loops; trivial dimensions (size=1) are supported.
!
! Interface:
!   subroutine compute_m_h(Re, dT_h_dx, deta_h_dx, dphi_m_dx, du_h_dx, m_h, phi_m_i, rho_h, sigma_h, u_h, F_m_h)
!     Scalars (intent in): Re, F_m_h
!     Arrays  (intent in): dT_h_dx, deta_h_dx, dphi_m_dx, du_h_dx, m_h, phi_m_i, rho_h, sigma_h, u_h
!     Output  (intent out): F_m_h(:,:,:)
!
! Notes:
!   - Arrays are 3D assumed-shape; pass size-1 dimensions for 1D/2D.
!   - Edit equations in Python; regenerated modules adapt automatically.
module m_h_module
  implicit none
contains
  subroutine compute_m_h(Re, dT_h_dx, deta_h_dx, dphi_m_dx, du_h_dx, m_h, phi_m_i, rho_h, sigma_h, u_h, F_m_h)
    implicit none
    real(8), intent(in) :: Re
    real(8), intent(in) :: dT_h_dx(:,:,:)
    real(8), intent(in) :: deta_h_dx(:,:,:)
    real(8), intent(in) :: dphi_m_dx(:,:,:)
    real(8), intent(in) :: du_h_dx(:,:,:)
    real(8), intent(in) :: m_h(:,:,:)
    real(8), intent(in) :: phi_m_i(:,:,:)
    real(8), intent(in) :: rho_h(:,:,:)
    real(8), intent(in) :: sigma_h(:,:,:)
    real(8), intent(in) :: u_h(:,:,:)
    real(8), intent(out) :: F_m_h(:,:,:)
    integer :: i, j, k

    do concurrent (k = 1:size(F_m_h,3), j = 1:size(F_m_h,2), i = 1:size(F_m_h,1))
      F_m_h(i,j,k) = -dT_h_dx(i,j,k)*phi_m_i(i,j,k)*sigma_h(i,j,k) - deta_h_dx(i,j,k)*phi_m_i(i,j,k)*rho_h(i,j,k) + dphi_m_dx(i,j,k)*m_h(i,j,k)*u_h(i,j,k) - &
      du_h_dx(i,j,k)*m_h(i,j,k)*phi_m_i(i,j,k) - du_h_dx(i,j,k)*phi_m_i(i,j,k)/Re
    end do concurrent

  end subroutine compute_m_h
end module
