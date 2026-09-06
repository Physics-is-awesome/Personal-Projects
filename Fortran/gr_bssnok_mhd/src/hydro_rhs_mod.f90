module hydro_rhs_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dy, dz, r_excise
  use grid_mod, only: xg, zg
  use hydro_vars_mod, only: NCONS, NPRIM, ID, ISX, ISY, ISZ, ITAU, IRHO, IVX, IVY, IVZ, IEPS
  use primitive_recovery_mod, only: con2prim
  use riemann_mod, only: hlle_flux, physical_flux, prim_to_cons
  use hydro_cartoon_mod, only: get_neighbor_prim
  use background_mod
  implicit none
  private
  public :: recover_all_primitives, compute_hydro_rhs

contains

  subroutine unpack_bg(bg, i, k, alpha, beta_up, gamma_ij, gamma_inv, Kij, dalpha, dbeta, dgamma)
    real(dp), intent(in)  :: bg(:,:,:)
    integer,  intent(in)  :: i, k
    real(dp), intent(out) :: alpha, beta_up(3), gamma_ij(3,3), gamma_inv(3,3), Kij(3,3)
    real(dp), intent(out) :: dalpha(3), dbeta(3,3), dgamma(3,3,3)
    integer :: a, b, j, c6

    alpha = bg(i,k,IBGA)
    beta_up = (/ bg(i,k,IBGBX), bg(i,k,IBGBY), bg(i,k,IBGBZ) /)

    gamma_ij(1,1)=bg(i,k,IBGGXX); gamma_ij(1,2)=bg(i,k,IBGGXY); gamma_ij(1,3)=bg(i,k,IBGGXZ)
    gamma_ij(2,1)=bg(i,k,IBGGXY); gamma_ij(2,2)=bg(i,k,IBGGYY); gamma_ij(2,3)=bg(i,k,IBGGYZ)
    gamma_ij(3,1)=bg(i,k,IBGGXZ); gamma_ij(3,2)=bg(i,k,IBGGYZ); gamma_ij(3,3)=bg(i,k,IBGGZZ)

    gamma_inv(1,1)=bg(i,k,IBGIXX); gamma_inv(1,2)=bg(i,k,IBGIXY); gamma_inv(1,3)=bg(i,k,IBGIXZ)
    gamma_inv(2,1)=bg(i,k,IBGIXY); gamma_inv(2,2)=bg(i,k,IBGIYY); gamma_inv(2,3)=bg(i,k,IBGIYZ)
    gamma_inv(3,1)=bg(i,k,IBGIXZ); gamma_inv(3,2)=bg(i,k,IBGIYZ); gamma_inv(3,3)=bg(i,k,IBGIZZ)

    Kij(1,1)=bg(i,k,IBGKXX); Kij(1,2)=bg(i,k,IBGKXY); Kij(1,3)=bg(i,k,IBGKXZ)
    Kij(2,1)=bg(i,k,IBGKXY); Kij(2,2)=bg(i,k,IBGKYY); Kij(2,3)=bg(i,k,IBGKYZ)
    Kij(3,1)=bg(i,k,IBGKXZ); Kij(3,2)=bg(i,k,IBGKYZ); Kij(3,3)=bg(i,k,IBGKZZ)

    dalpha = (/ bg(i,k,IBGDAX), bg(i,k,IBGDAY), bg(i,k,IBGDAZ) /)

    do j = 1, 3
      dbeta(j,1) = bg(i,k, IBGDB + (j-1)*3 + 1)
      dbeta(j,2) = bg(i,k, IBGDB + (j-1)*3 + 2)
      dbeta(j,3) = bg(i,k, IBGDB + (j-1)*3 + 3)
    end do

    do a = 1, 3
      do b = 1, 3
        c6 = comp6(a,b)
        do j = 1, 3
          dgamma(a,b,j) = bg(i,k, IBGDG + (c6-1)*3 + j)
        end do
      end do
    end do
  end subroutine unpack_bg

  integer function comp6(a, b) result(c)
    integer, intent(in) :: a, b
    if ((a==1.and.b==1)) then; c=1
    else if ((a==1.and.b==2).or.(a==2.and.b==1)) then; c=2
    else if ((a==1.and.b==3).or.(a==3.and.b==1)) then; c=3
    else if (a==2.and.b==2) then; c=4
    else if ((a==2.and.b==3).or.(a==3.and.b==2)) then; c=5
    else; c=6
    end if
  end function comp6

  subroutine recover_all_primitives(cons, bg, prim)
    real(dp), intent(in)  :: cons(:,:,:), bg(:,:,:)
    real(dp), intent(inout) :: prim(:,:,:)
    integer :: i, k
    real(dp) :: alpha, beta_up(3), gamma_ij(3,3), gamma_inv(3,3), Kij(3,3)
    real(dp) :: dalpha(3), dbeta(3,3), dgamma(3,3,3)
    real(dp) :: rho, vx, vy, vz, eps
    logical :: ok

    do k = 1, Nz
      do i = 1, Nx
        call unpack_bg(bg, i, k, alpha, beta_up, gamma_ij, gamma_inv, Kij, dalpha, dbeta, dgamma)
        call con2prim(cons(i,k,ID), cons(i,k,ISX), cons(i,k,ISY), cons(i,k,ISZ), cons(i,k,ITAU), &
                      gamma_inv, rho, vx, vy, vz, eps, ok)
        prim(i,k,IRHO)=rho; prim(i,k,IVX)=vx; prim(i,k,IVY)=vy; prim(i,k,IVZ)=vz; prim(i,k,IEPS)=eps
      end do
    end do
  end subroutine recover_all_primitives

  real(dp) function v2_of(prim, gamma_ij) result(v2)
    real(dp), intent(in) :: prim(NPRIM), gamma_ij(3,3)
    real(dp) :: vup(3), vlow(3)
    vup = (/ prim(IVX), prim(IVY), prim(IVZ) /)
    vlow = matmul(gamma_ij, vup)
    v2 = min(sum(vup*vlow), 1.0_dp - 1.0e-12_dp)
  end function v2_of

  subroutine compute_hydro_rhs(cons, bg, rhs)
    real(dp), intent(in)    :: cons(:,:,:), bg(:,:,:)
    real(dp), intent(inout) :: rhs(:,:,:)
    real(dp), allocatable :: prim(:,:,:)
    integer :: i, k

    allocate(prim(Nx,Nz,NPRIM))
    call recover_all_primitives(cons, bg, prim)

    do k = 2, Nz - 1
      do i = 1, Nx - 1
        if (sqrt(xg(i)**2 + zg(k)**2) < r_excise) then
          rhs(i,k,:) = 0.0_dp
          cycle
        end if
        call rhs_point(prim, bg, i, k, rhs(i,k,:))
      end do
    end do

    deallocate(prim)
  end subroutine compute_hydro_rhs

  subroutine rhs_point(prim, bg, i, k, rhs_out)
    real(dp), intent(in)  :: prim(:,:,:), bg(:,:,:)
    integer,  intent(in)  :: i, k
    real(dp), intent(out) :: rhs_out(NCONS)

    real(dp) :: alpha0, beta_up0(3), gamma_ij0(3,3), gamma_inv0(3,3), Kij0(3,3)
    real(dp) :: dalpha0(3), dbeta0(3,3), dgamma0(3,3,3)
    real(dp) :: sqrtg0

    real(dp) :: alphaF, beta_upF(3), gijF(3,3), ginvF(3,3), sqrtgF
    real(dp) :: primL(NPRIM), primR(NPRIM), fluxF(NCONS)
    real(dp) :: Fxp(NCONS), Fxm(NCONS), Fzp(NCONS), Fzm(NCONS)
    real(dp) :: Fyp(NCONS), Fym(NCONS)
    real(dp) :: primYp(NPRIM), primYm(NPRIM)
    real(dp) :: alphaYp, betaYp(3), gijYp(3,3), ginvYp(3,3), sqrtgYp
    real(dp) :: alphaYm, betaYm(3), gijYm(3,3), ginvYm(3,3), sqrtgYm
    real(dp) :: v2L, v2R
    real(dp) :: flux_div(NCONS)
    real(dp) :: rho0, eps0, W0, h0, p0
    real(dp) :: vup0(3), vlow0(3), Slow0(3), Sup0(3), Sij0(3,3), Sup2_0(3,3)
    real(dp) :: src_S(3), src_tau, EplusD0
    integer :: a, b, j

    call unpack_bg(bg, i, k, alpha0, beta_up0, gamma_ij0, gamma_inv0, Kij0, dalpha0, dbeta0, dgamma0)
    sqrtg0 = bg(i,k,IBGSQRTG)

    ! ---- x-direction Godunov fluxes ----
    call metric_at(0.5_dp*(xg(i)+xg(i+1)), 0.0_dp, zg(k), alphaF, beta_upF, gijF, ginvF, sqrtgF)
    primL = prim(i,k,:); primR = get_neighbor_prim(prim, i, k, 1, 0, 0.0_dp)
    v2L = v2_of(primL, gijF); v2R = v2_of(primR, gijF)
    call hlle_flux(primL, primR, v2L, v2R, alphaF, beta_upF, gijF, ginvF(1,1), 1, fluxF)
    Fxp = sqrtgF * fluxF

    call metric_at(xg(i)-0.5_dp*dx, 0.0_dp, zg(k), alphaF, beta_upF, gijF, ginvF, sqrtgF)
    primL = get_neighbor_prim(prim, i, k, -1, 0, 0.0_dp); primR = prim(i,k,:)
    v2L = v2_of(primL, gijF); v2R = v2_of(primR, gijF)
    call hlle_flux(primL, primR, v2L, v2R, alphaF, beta_upF, gijF, ginvF(1,1), 1, fluxF)
    Fxm = sqrtgF * fluxF

    ! ---- z-direction Godunov fluxes ----
    call metric_at(xg(i), 0.0_dp, 0.5_dp*(zg(k)+zg(k+1)), alphaF, beta_upF, gijF, ginvF, sqrtgF)
    primL = prim(i,k,:); primR = prim(i,k+1,:)
    v2L = v2_of(primL, gijF); v2R = v2_of(primR, gijF)
    call hlle_flux(primL, primR, v2L, v2R, alphaF, beta_upF, gijF, ginvF(3,3), 3, fluxF)
    Fzp = sqrtgF * fluxF

    call metric_at(xg(i), 0.0_dp, zg(k)-0.5_dp*dz, alphaF, beta_upF, gijF, ginvF, sqrtgF)
    primL = prim(i,k-1,:); primR = prim(i,k,:)
    v2L = v2_of(primL, gijF); v2R = v2_of(primR, gijF)
    call hlle_flux(primL, primR, v2L, v2R, alphaF, beta_upF, gijF, ginvF(3,3), 3, fluxF)
    Fzm = sqrtgF * fluxF

    ! ---- y-direction: centered-difference of the physical flux (not a
    ! Godunov face -- there is no independently-resolved y-grid, exactly as
    ! for the BSSN sector) ----
    primYp = get_neighbor_prim(prim, i, k, 0, 0,  dy)
    primYm = get_neighbor_prim(prim, i, k, 0, 0, -dy)
    call metric_at(xg(i),  dy, zg(k), alphaYp, betaYp, gijYp, ginvYp, sqrtgYp)
    call metric_at(xg(i), -dy, zg(k), alphaYm, betaYm, gijYm, ginvYm, sqrtgYm)
    call physical_flux(primYp, alphaYp, betaYp, gijYp, 2, fluxF); Fyp = sqrtgYp*fluxF
    call physical_flux(primYm, alphaYm, betaYm, gijYm, 2, fluxF); Fym = sqrtgYm*fluxF

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
    Sup2_0 = matmul(gamma_inv0, matmul(Sij0, gamma_inv0))  ! S^{ab}
    EplusD0 = rho0*h0*W0*W0 - p0   ! = tau + D

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

end module hydro_rhs_mod
