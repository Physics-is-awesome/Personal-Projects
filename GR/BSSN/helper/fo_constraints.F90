module fo_constraints_mod
  use kinds_mod, only: dp
  use fo_vars_mod, only: IDPHI_FO, IDGT_FO, IDALPHA_FO, IDBETA_FO
  use params_mod, only: Nx, Nz, dx, dz
  implicit none
  private
  public :: reduction_constraint_norms

contains

  subroutine reduction_constraint_norms(u, max_phi, max_gt, max_alpha, max_beta)
    real(dp), intent(in) :: u(:,:,:)
    real(dp), intent(out) :: max_phi, max_gt, max_alpha, max_beta
    integer :: i, k, q, a, c
    real(dp) :: d

    max_phi = 0.0_dp
    max_gt = 0.0_dp
    max_alpha = 0.0_dp
    max_beta = 0.0_dp

    do k = 2, Nz-1
      do i = 2, Nx-1
        d = (u(i+1,k,1)-u(i-1,k,1))/(2.0_dp*dx)
        max_phi = max(max_phi, abs(u(i,k,IDPHI_FO(1))-d))
        d = (u(i,k+1,1)-u(i,k-1,1))/(2.0_dp*dz)
        max_phi = max(max_phi, abs(u(i,k,IDPHI_FO(3))-d))

        do q = 1, 6
          d = (u(i+1,k,q+1)-u(i-1,k,q+1))/(2.0_dp*dx)
          max_gt = max(max_gt, abs(u(i,k,IDGT_FO(1,q))-d))
          d = (u(i,k+1,q+1)-u(i,k-1,q+1))/(2.0_dp*dz)
          max_gt = max(max_gt, abs(u(i,k,IDGT_FO(3,q))-d))
        end do

        d = (u(i+1,k,18)-u(i-1,k,18))/(2.0_dp*dx)
        max_alpha = max(max_alpha, abs(u(i,k,IDALPHA_FO(1))-d))
        d = (u(i,k+1,18)-u(i,k-1,18))/(2.0_dp*dz)
        max_alpha = max(max_alpha, abs(u(i,k,IDALPHA_FO(3))-d))

        do a = 1, 3
          c = 18 + a
          d = (u(i+1,k,c)-u(i-1,k,c))/(2.0_dp*dx)
          max_beta = max(max_beta, abs(u(i,k,IDBETA_FO(1,a))-d))
          d = (u(i,k+1,c)-u(i,k-1,c))/(2.0_dp*dz)
          max_beta = max(max_beta, abs(u(i,k,IDBETA_FO(3,a))-d))
        end do
      end do
    end do
  end subroutine reduction_constraint_norms

end module fo_constraints_mod
