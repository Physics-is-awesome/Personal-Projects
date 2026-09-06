module tov_id_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz
  use grid_mod, only: xg, zg
  use vars_mod
  use hydro_vars_mod, only: NPRIM, IRHO, IVX, IVY, IVZ, IEPS
  use tov_mod, only: tov_profile_t, tov_lookup
  use riemann_mod, only: prim_to_cons
  use primitive_recovery_mod, only: rho_floor, p_floor
  use eos_mod, only: eps_from_rho_p
  implicit none
  private
  public :: set_tov_id

contains

  ! Conformally-flat TOV data in Cartesian (Cartoon) coordinates:
  !   gamma_ij = psi^4 delta_ij  =>  phi = ln(psi), gamma-tilde_ij = delta_ij
  !   K_ij = 0 (static, time-symmetric slice)  =>  K = 0, A-tilde_ij = 0
  !   Gamma-tilde^i = 0 identically (flat conformal metric)
  !   alpha = TOV lapse, beta^i = 0
  subroutine set_tov_id(bssn_u, hydro_cons, prof)
    real(dp), intent(inout) :: bssn_u(:,:,:), hydro_cons(:,:,:)
    type(tov_profile_t), intent(in) :: prof
    integer :: i, k
    real(dp) :: riso, rho, p, eps, alpha, psi
    real(dp) :: prim(NPRIM), gamma_ij(3,3)

    do k = 1, Nz
      do i = 1, Nx
        riso = sqrt(xg(i)**2 + zg(k)**2)
        call tov_lookup(prof, riso, rho, p, eps, alpha, psi)
        if (rho <= 0.0_dp) then
          rho = rho_floor
          eps = eps_from_rho_p(rho_floor, p_floor)
        end if

        bssn_u(i,k,IPHI) = log(psi)
        bssn_u(i,k,IGXX) = 1.0_dp; bssn_u(i,k,IGYY) = 1.0_dp; bssn_u(i,k,IGZZ) = 1.0_dp
        bssn_u(i,k,IGXY) = 0.0_dp; bssn_u(i,k,IGXZ) = 0.0_dp; bssn_u(i,k,IGYZ) = 0.0_dp
        bssn_u(i,k,IK)   = 0.0_dp
        bssn_u(i,k,IAXX) = 0.0_dp; bssn_u(i,k,IAXY) = 0.0_dp; bssn_u(i,k,IAXZ) = 0.0_dp
        bssn_u(i,k,IAYY) = 0.0_dp; bssn_u(i,k,IAYZ) = 0.0_dp; bssn_u(i,k,IAZZ) = 0.0_dp
        bssn_u(i,k,IGTX) = 0.0_dp; bssn_u(i,k,IGTY) = 0.0_dp; bssn_u(i,k,IGTZ) = 0.0_dp
        bssn_u(i,k,IALPHA) = alpha
        bssn_u(i,k,IBETAX) = 0.0_dp; bssn_u(i,k,IBETAY) = 0.0_dp; bssn_u(i,k,IBETAZ) = 0.0_dp
        bssn_u(i,k,IBX) = 0.0_dp; bssn_u(i,k,IBY) = 0.0_dp; bssn_u(i,k,IBZ) = 0.0_dp

        prim(IRHO) = rho; prim(IVX) = 0.0_dp; prim(IVY) = 0.0_dp; prim(IVZ) = 0.0_dp; prim(IEPS) = eps
        gamma_ij = 0.0_dp
        gamma_ij(1,1) = psi**4; gamma_ij(2,2) = psi**4; gamma_ij(3,3) = psi**4
        call prim_to_cons(prim, gamma_ij, hydro_cons(i,k,:))
      end do
    end do
  end subroutine set_tov_id

end module tov_id_mod
