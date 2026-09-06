module kerr_tov_id_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, bh_center_x, bh_center_z, star_center_x, star_center_z
  use grid_mod, only: xg, zg
  use vars_mod
  use geometry_mod, only: sym3_inverse
  use hydro_vars_mod, only: NPRIM, IRHO, IVX, IVY, IVZ, IEPS
  use initial_data_mod, only: kerr_schild_adm
  use tov_mod, only: tov_profile_t, tov_lookup
  use riemann_mod, only: prim_to_cons
  use primitive_recovery_mod, only: rho_floor, p_floor
  use eos_mod, only: eps_from_rho_p
  implicit none
  private
  public :: set_kerr_tov_id

contains

  ! Approximate superposition: KS ADM data for the hole plus the star's
  ! isotropic conformal-factor excess. This is not constraint-solved data.
  subroutine set_kerr_tov_id(bssn_u, hydro_cons, prof)
    real(dp), intent(inout) :: bssn_u(:,:,:), hydro_cons(:,:,:)
    type(tov_profile_t), intent(in) :: prof
    integer :: i, k
    real(dp) :: gks(3,3), ginv(3,3), alpha_ks, beta(3), Kij(3,3)
    real(dp) :: g(3,3), gt(3,3), Aij(3,3), phi, Ktrace, det
    real(dp) :: rho, p, eps, alpha_star, psi_star, rs, prim(NPRIM)
    real(dp) :: eye(3,3)

    eye = 0.0_dp
    eye(1,1)=1.0_dp; eye(2,2)=1.0_dp; eye(3,3)=1.0_dp
    do k = 1, Nz
      do i = 1, Nx
        call kerr_schild_adm(xg(i)-bh_center_x, 0.0_dp, zg(k)-bh_center_z, &
                             gks, ginv, alpha_ks, beta, Kij)
        rs = sqrt((xg(i)-star_center_x)**2 + (zg(k)-star_center_z)**2)
        call tov_lookup(prof, rs, rho, p, eps, alpha_star, psi_star)

        ! Add only the star contribution beyond flat space, retaining the
        ! Kerr-Schild anisotropy and shift. Lapse product is a regular
        ! superposition choice, useful for a dynamical initial-data path.
        g = gks + (psi_star**4 - 1.0_dp)*eye
        call sym3_inverse(g, ginv, det)
        phi = log(max(det, 1.0e-300_dp))/12.0_dp
        gt = exp(-4.0_dp*phi)*g
        Ktrace = sum(ginv*Kij)
        Aij = exp(-4.0_dp*phi)*(Kij - g*Ktrace/3.0_dp)

        bssn_u(i,k,IPHI)=phi
        bssn_u(i,k,IGXX)=gt(1,1); bssn_u(i,k,IGXY)=gt(1,2); bssn_u(i,k,IGXZ)=gt(1,3)
        bssn_u(i,k,IGYY)=gt(2,2); bssn_u(i,k,IGYZ)=gt(2,3); bssn_u(i,k,IGZZ)=gt(3,3)
        bssn_u(i,k,IK)=Ktrace
        bssn_u(i,k,IAXX)=Aij(1,1); bssn_u(i,k,IAXY)=Aij(1,2); bssn_u(i,k,IAXZ)=Aij(1,3)
        bssn_u(i,k,IAYY)=Aij(2,2); bssn_u(i,k,IAYZ)=Aij(2,3); bssn_u(i,k,IAZZ)=Aij(3,3)
        bssn_u(i,k,IALPHA)=max(alpha_ks*alpha_star, 1.0e-8_dp)
        bssn_u(i,k,IBETAX)=beta(1); bssn_u(i,k,IBETAY)=beta(2); bssn_u(i,k,IBETAZ)=beta(3)
        bssn_u(i,k,IGTX)=0.0_dp; bssn_u(i,k,IGTY)=0.0_dp; bssn_u(i,k,IGTZ)=0.0_dp
        bssn_u(i,k,IBX)=0.0_dp; bssn_u(i,k,IBY)=0.0_dp; bssn_u(i,k,IBZ)=0.0_dp

        if (rho <= 0.0_dp) then
          rho=rho_floor; p=p_floor; eps=eps_from_rho_p(rho,p)
        end if
        prim(IRHO)=rho; prim(IVX)=0.0_dp; prim(IVY)=0.0_dp; prim(IVZ)=0.0_dp; prim(IEPS)=eps
        call prim_to_cons(prim, g, hydro_cons(i,k,:))
      end do
    end do
  end subroutine set_kerr_tov_id

end module kerr_tov_id_mod
