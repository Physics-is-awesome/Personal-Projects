module hydro_rhs_dynamic_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dy, dz, r_excise
  use grid_mod, only: xg, zg
  use hydro_vars_mod, only: NCONS, NPRIM, ID, ISX, ISY, ISZ, ITAU, IRHO, IVX, IVY, IVZ, IEPS
  use primitive_recovery_mod, only: con2prim
  use riemann_mod, only: hlle_flux, physical_flux, prim_to_cons, char_speeds_v2
  use hydro_cartoon_mod, only: get_neighbor_prim
  use live_metric_mod, only: live_metric_at_cell, live_metric_at_ghost, live_metric_value
  use reconstruction_mod, only: reconstruct_x_faces, reconstruct_z_faces
  implicit none
  private
  public :: recover_all_primitives_dynamic, compute_hydro_rhs_dynamic, max_characteristic_speed

contains

  real(dp) function det3(M) result(d)
    real(dp), intent(in) :: M(3,3)
    d = M(1,1)*(M(2,2)*M(3,3)-M(2,3)*M(3,2)) &
      - M(1,2)*(M(2,1)*M(3,3)-M(2,3)*M(3,1)) &
      + M(1,3)*(M(2,1)*M(3,2)-M(2,2)*M(3,1))
  end function det3

  subroutine recover_all_primitives_dynamic(cons, bssn_u, prim)
    real(dp), intent(in)  :: cons(:,:,:), bssn_u(:,:,:)
    real(dp), intent(inout) :: prim(:,:,:)
    integer :: i, k
    real(dp) :: alpha, beta_up(3), gamma_ij(3,3), gamma_inv(3,3)
    real(dp) :: rho, vx, vy, vz, eps
    logical :: ok

    do k = 1, Nz
      do i = 1, Nx
        call live_metric_value(bssn_u, i, k, alpha, beta_up, gamma_ij, gamma_inv)
        call con2prim(cons(i,k,ID), cons(i,k,ISX), cons(i,k,ISY), cons(i,k,ISZ), cons(i,k,ITAU), &
                      gamma_inv, rho, vx, vy, vz, eps, ok)
        prim(i,k,IRHO)=rho; prim(i,k,IVX)=vx; prim(i,k,IVY)=vy; prim(i,k,IVZ)=vz; prim(i,k,IEPS)=eps
      end do
    end do
  end subroutine recover_all_primitives_dynamic

  real(dp) function max_characteristic_speed(bssn_u, cons) result(speed)
    real(dp), intent(in) :: bssn_u(:,:,:), cons(:,:,:)
    real(dp), allocatable :: prim(:,:,:)
    real(dp) :: alpha, beta(3), gij(3,3), ginv(3,3)
    real(dp) :: lmin, lmax, v2, local_speed
    real(dp) :: beta_speed, metric_speed
    integer :: i, k, dir

    allocate(prim(Nx,Nz,NPRIM))
    call recover_all_primitives_dynamic(cons, bssn_u, prim)
    speed = 1.0_dp
    do k = 2, Nz - 1
      do i = 1, Nx - 1
        if (sqrt(xg(i)**2 + zg(k)**2) < r_excise) cycle
        call live_metric_value(bssn_u, i, k, alpha, beta, gij, ginv)
        do dir = 1, 3
          v2 = v2_of(prim(i,k,:), gij)
          call char_speeds_v2(prim(i,k,:), alpha, beta(dir), ginv(dir,dir), v2, dir, lmin, lmax)
          local_speed = max(abs(lmin), abs(lmax))
          speed = max(speed, local_speed)
          beta_speed = abs(beta(dir)) + alpha*sqrt(max(ginv(dir,dir), 0.0_dp))
          metric_speed = abs(beta(dir)) + sqrt(2.0_dp)*alpha*sqrt(max(ginv(dir,dir), 0.0_dp))
          speed = max(speed, beta_speed, metric_speed)
        end do
      end do
    end do
    deallocate(prim)
  end function max_characteristic_speed

  real(dp) function v2_of(prim, gamma_ij) result(v2)
    real(dp), intent(in) :: prim(NPRIM), gamma_ij(3,3)
    real(dp) :: vup(3), vlow(3)
    vup = (/ prim(IVX), prim(IVY), prim(IVZ) /)
    vlow = matmul(gamma_ij, vup)
    v2 = min(sum(vup*vlow), 1.0_dp - 1.0e-12_dp)
  end function v2_of

  subroutine compute_hydro_rhs_dynamic(cons, bssn_u, rhs)
    real(dp), intent(in)    :: cons(:,:,:), bssn_u(:,:,:)
    real(dp), intent(inout) :: rhs(:,:,:)
    real(dp), allocatable :: prim(:,:,:)
    integer :: i, k

    allocate(prim(Nx,Nz,NPRIM))
    call recover_all_primitives_dynamic(cons, bssn_u, prim)

    do k = 2, Nz - 1
      do i = 1, Nx - 1
        if (sqrt(xg(i)**2 + zg(k)**2) < r_excise) then
          rhs(i,k,:) = 0.0_dp
          cycle
        end if
        call rhs_point(prim, bssn_u, i, k, rhs(i,k,:))
      end do
    end do

    deallocate(prim)
  end subroutine compute_hydro_rhs_dynamic

  subroutine rhs_point(prim, bssn_u, i, k, rhs_out)
    real(dp), intent(in)  :: prim(:,:,:), bssn_u(:,:,:)
    integer,  intent(in)  :: i, k
    real(dp), intent(out) :: rhs_out(NCONS)

    real(dp) :: alpha0, beta_up0(3), gamma_ij0(3,3), gamma_inv0(3,3), Kij0(3,3)
    real(dp) :: dalpha0(3), dbeta0(3,3), dgamma0(3,3,3)
    real(dp) :: sqrtg0

    real(dp) :: alphaA, beta_upA(3), gijA(3,3), ginvA(3,3)
    real(dp) :: alphaB, beta_upB(3), gijB(3,3), ginvB(3,3)
    real(dp) :: alphaF, beta_upF(3), gijF(3,3), ginvF(3,3), sqrtgF
    real(dp) :: primL(NPRIM), primR(NPRIM), fluxF(NCONS)
    real(dp) :: Fxp(NCONS), Fxm(NCONS), Fzp(NCONS), Fzm(NCONS)
    real(dp) :: Fyp(NCONS), Fym(NCONS)
    real(dp) :: primYp(NPRIM), primYm(NPRIM)
    real(dp) :: alphaYp, betaYp(3), gijYp(3,3), ginvYp(3,3)
    real(dp) :: alphaYm, betaYm(3), gijYm(3,3), ginvYm(3,3)
    real(dp) :: v2L, v2R
    real(dp) :: flux_div(NCONS)
    real(dp) :: rho0, eps0, W0, h0, p0
    real(dp) :: vup0(3), vlow0(3), Slow0(3), Sup0(3), Sij0(3,3), Sup2_0(3,3)
    real(dp) :: src_S(3), src_tau, EplusD0
    integer :: a, b, j

    call live_metric_at_cell(bssn_u, i, k, alpha0, beta_up0, gamma_ij0, gamma_inv0, Kij0, &
                              dalpha0, dbeta0, dgamma0)
    sqrtg0 = sqrt(max(det3(gamma_ij0), 1.0e-300_dp))

    ! ---- x-direction Godunov fluxes (face metric = average of the two
    ! neighboring cells' live values; primitives reconstructed via PLM) ----
    block
      real(dp) :: pLp(NPRIM), pRp(NPRIM), pLm(NPRIM), pRm(NPRIM)
      call reconstruct_x_faces(prim, i, k, pLp, pRp, pLm, pRm)

      call live_metric_value(bssn_u, i, k, alphaA, beta_upA, gijA, ginvA)
      call live_metric_value(bssn_u, i+1, k, alphaB, beta_upB, gijB, ginvB)
      alphaF = 0.5_dp*(alphaA+alphaB); beta_upF = 0.5_dp*(beta_upA+beta_upB); gijF = 0.5_dp*(gijA+gijB)
      call sym3_inv_local(gijF, ginvF); sqrtgF = sqrt(max(det3(gijF),1.0e-300_dp))
      primL = pLp; primR = pRp
      v2L = v2_of(primL, gijF); v2R = v2_of(primR, gijF)
      call hlle_flux(primL, primR, v2L, v2R, alphaF, beta_upF, gijF, ginvF(1,1), 1, fluxF)
      Fxp = sqrtgF * fluxF

      call live_metric_value(bssn_u, i, k, alphaB, beta_upB, gijB, ginvB)  ! reuse as "self"
      call live_metric_at_ghost(bssn_u, i, k, -1, 0, 0.0_dp, alphaA, beta_upA, gijA, ginvA)
      alphaF = 0.5_dp*(alphaA+alphaB); beta_upF = 0.5_dp*(beta_upA+beta_upB); gijF = 0.5_dp*(gijA+gijB)
      call sym3_inv_local(gijF, ginvF); sqrtgF = sqrt(max(det3(gijF),1.0e-300_dp))
      primL = pLm; primR = pRm
      v2L = v2_of(primL, gijF); v2R = v2_of(primR, gijF)
      call hlle_flux(primL, primR, v2L, v2R, alphaF, beta_upF, gijF, ginvF(1,1), 1, fluxF)
      Fxm = sqrtgF * fluxF
    end block

    ! ---- z-direction Godunov fluxes ----
    block
      real(dp) :: pLp(NPRIM), pRp(NPRIM), pLm(NPRIM), pRm(NPRIM)
      call reconstruct_z_faces(prim, i, k, pLp, pRp, pLm, pRm)

      call live_metric_value(bssn_u, i, k, alphaA, beta_upA, gijA, ginvA)
      call live_metric_value(bssn_u, i, k+1, alphaB, beta_upB, gijB, ginvB)
      alphaF = 0.5_dp*(alphaA+alphaB); beta_upF = 0.5_dp*(beta_upA+beta_upB); gijF = 0.5_dp*(gijA+gijB)
      call sym3_inv_local(gijF, ginvF); sqrtgF = sqrt(max(det3(gijF),1.0e-300_dp))
      primL = pLp; primR = pRp
      v2L = v2_of(primL, gijF); v2R = v2_of(primR, gijF)
      call hlle_flux(primL, primR, v2L, v2R, alphaF, beta_upF, gijF, ginvF(3,3), 3, fluxF)
      Fzp = sqrtgF * fluxF

      call live_metric_value(bssn_u, i, k-1, alphaA, beta_upA, gijA, ginvA)
      call live_metric_value(bssn_u, i, k, alphaB, beta_upB, gijB, ginvB)
      alphaF = 0.5_dp*(alphaA+alphaB); beta_upF = 0.5_dp*(beta_upA+beta_upB); gijF = 0.5_dp*(gijA+gijB)
      call sym3_inv_local(gijF, ginvF); sqrtgF = sqrt(max(det3(gijF),1.0e-300_dp))
      primL = pLm; primR = pRm
      v2L = v2_of(primL, gijF); v2R = v2_of(primR, gijF)
      call hlle_flux(primL, primR, v2L, v2R, alphaF, beta_upF, gijF, ginvF(3,3), 3, fluxF)
      Fzm = sqrtgF * fluxF
    end block

    ! ---- y-direction: centered-difference of the physical flux ----
    primYp = get_neighbor_prim(prim, i, k, 0, 0,  dy)
    primYm = get_neighbor_prim(prim, i, k, 0, 0, -dy)
    call live_metric_at_ghost(bssn_u, i, k, 0, 0,  dy, alphaYp, betaYp, gijYp, ginvYp)
    call live_metric_at_ghost(bssn_u, i, k, 0, 0, -dy, alphaYm, betaYm, gijYm, ginvYm)
    call physical_flux(primYp, alphaYp, betaYp, gijYp, 2, fluxF); Fyp = sqrt(max(det3(gijYp),1.0e-300_dp))*fluxF
    call physical_flux(primYm, alphaYm, betaYm, gijYm, 2, fluxF); Fym = sqrt(max(det3(gijYm),1.0e-300_dp))*fluxF

    flux_div = (Fxp - Fxm)/dx + (Fzp - Fzm)/dz + (Fyp - Fym)/(2.0_dp*dy)

    ! ---- geometric source terms (cell-centered) ----
    rho0 = prim(i,k,IRHO); eps0 = prim(i,k,IEPS)
    vup0 = (/ prim(i,k,IVX), prim(i,k,IVY), prim(i,k,IVZ) /)
    vlow0 = matmul(gamma_ij0, vup0)
    W0 = 1.0_dp/sqrt(max(1.0_dp - sum(vup0*vlow0), 1.0e-14_dp))
    call local_pressure(rho0, eps0, p0)
    h0 = 1.0_dp + eps0 + p0/max(rho0,1.0e-300_dp)

    Slow0 = rho0*h0*W0*W0*vlow0
    Sup0 = matmul(gamma_inv0, Slow0)
    do a = 1, 3
      do b = 1, 3
        Sij0(a,b) = rho0*h0*W0*W0*vlow0(a)*vlow0(b) + p0*gamma_ij0(a,b)
      end do
    end do
    Sup2_0 = matmul(gamma_inv0, matmul(Sij0, gamma_inv0))
    EplusD0 = rho0*h0*W0*W0 - p0

    do j = 1, 3
      src_S(j) = 0.5_dp*alpha0*sum(Sup2_0 * dgamma0(:,:,j)) + sum(Slow0*dbeta0(j,:)) &
                 - EplusD0*dalpha0(j)
    end do

    src_tau = alpha0*sum(Sup2_0*Kij0) - sum(Sup0*dalpha0)

    rhs_out(ID)   = -flux_div(ID)/sqrtg0
    rhs_out(ISX)  = -flux_div(ISX)/sqrtg0 + src_S(1)
    rhs_out(ISY)  = -flux_div(ISY)/sqrtg0 + src_S(2)
    rhs_out(ISZ)  = -flux_div(ISZ)/sqrtg0 + src_S(3)
    rhs_out(ITAU) = -flux_div(ITAU)/sqrtg0 + src_tau

  end subroutine rhs_point

  subroutine local_pressure(rho, eps, p)
    use eos_mod, only: pressure
    real(dp), intent(in) :: rho, eps
    real(dp), intent(out) :: p
    p = pressure(rho, eps)
  end subroutine local_pressure

  subroutine sym3_inv_local(M, Minv)
    real(dp), intent(in)  :: M(3,3)
    real(dp), intent(out) :: Minv(3,3)
    real(dp) :: det, c11,c12,c13,c22,c23,c33
    c11 =  M(2,2)*M(3,3) - M(2,3)*M(2,3)
    c12 = -(M(1,2)*M(3,3) - M(1,3)*M(2,3))
    c13 =  M(1,2)*M(2,3) - M(1,3)*M(2,2)
    c22 =  M(1,1)*M(3,3) - M(1,3)*M(1,3)
    c23 = -(M(1,1)*M(2,3) - M(1,3)*M(1,2))
    c33 =  M(1,1)*M(2,2) - M(1,2)*M(1,2)
    det = M(1,1)*c11 + M(1,2)*c12 + M(1,3)*c13
    Minv(1,1)=c11/det; Minv(1,2)=c12/det; Minv(1,3)=c13/det
    Minv(2,1)=c12/det; Minv(2,2)=c22/det; Minv(2,3)=c23/det
    Minv(3,1)=c13/det; Minv(3,2)=c23/det; Minv(3,3)=c33/det
  end subroutine sym3_inv_local

end module hydro_rhs_dynamic_mod
