module matter_source_grid_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz
  use hydro_vars_mod, only: NPRIM
  use matter_source_mod, only: matter_source
  use live_metric_mod, only: live_metric_value
  implicit none
  private
  public :: build_matter_source_grid

contains

  ! msrc(Nx,Nz,10): E, Sx,Sy,Sz, Sxx,Sxy,Sxz,Syy,Syz,Szz
  subroutine build_matter_source_grid(prim, bssn_u, msrc)
    real(dp), intent(in)    :: prim(:,:,:), bssn_u(:,:,:)
    real(dp), intent(inout) :: msrc(:,:,:)
    integer :: i, k
    real(dp) :: alpha, beta_up(3), gamma_ij(3,3), gamma_inv(3,3)
    real(dp) :: E, Slow(3), Sij(3,3)

    do k = 1, Nz
      do i = 1, Nx
        call live_metric_value(bssn_u, i, k, alpha, beta_up, gamma_ij, gamma_inv)
        call matter_source(prim(i,k,:), gamma_ij, E, Slow, Sij)
        msrc(i,k,1) = E
        msrc(i,k,2:4) = Slow
        msrc(i,k,5) = Sij(1,1); msrc(i,k,6) = Sij(1,2); msrc(i,k,7) = Sij(1,3)
        msrc(i,k,8) = Sij(2,2); msrc(i,k,9) = Sij(2,3); msrc(i,k,10) = Sij(3,3)
      end do
    end do
  end subroutine build_matter_source_grid

end module matter_source_grid_mod
