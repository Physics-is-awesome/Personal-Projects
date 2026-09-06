module bssn_rhs_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, gauge_eta, r_excise
  use grid_mod, only: xg, zg
  use vars_mod
  use stencil_mod, only: stencil_t, compute_stencil
  use geometry_mod, only: conformal_geometry_t, compute_conformal_geometry, pack6, pack6A
  implicit none
  private
  public :: compute_rhs_interior

contains

  subroutine compute_rhs_interior(u, rhs, msrc)
    real(dp), intent(in)    :: u(:,:,:)
    real(dp), intent(inout) :: rhs(:,:,:)
    real(dp), intent(in), optional :: msrc(:,:,:)  ! (Nx,Nz,10): E,Sx,Sy,Sz,Sxx,Sxy,Sxz,Syy,Syz,Szz
    integer :: i, k

    do k = 2, Nz - 1
      do i = 1, Nx - 1
        if (sqrt(xg(i)**2 + zg(k)**2) < r_excise) cycle  ! frozen inside excision radius
        if (present(msrc)) then
          call rhs_point(u, i, k, rhs(i,k,:), msrc(i,k,:))
        else
          call rhs_point(u, i, k, rhs(i,k,:))
        end if
      end do
    end do
  end subroutine compute_rhs_interior

  subroutine rhs_point(u, i, k, rhs_out, msrc_pt)
    real(dp), intent(in)  :: u(:,:,:)
    integer,  intent(in)  :: i, k
    real(dp), intent(out) :: rhs_out(NVARS)
    real(dp), intent(in), optional :: msrc_pt(10)

    type(stencil_t) :: S
    type(conformal_geometry_t) :: G

    real(dp) :: gt(3,3), gtinv(3,3), Amat(3,3), Aup(3,3), AikAkj(3,3)
    real(dp) :: R(3,3), X(3,3), XTF(3,3)
    real(dp) :: dg(3,3,3)              ! dg(m,i,j) = d_m gt_ij
    real(dp) :: dA(3,3,3)              ! dA(m,i,j) = d_m A_ij
    real(dp) :: dbeta(3,3)             ! dbeta(m,c) = d_m beta^c
    real(dp) :: d2beta(3,3,3)          ! d2beta(a,b,c) = d_a d_b beta^c
    real(dp) :: dGt(3,3)               ! dGt(m,k) = d_m Gamma-tilde^k
    real(dp) :: Gtvec(3), beta(3), dphi_l(3), dalpha_l(3), dK(3)
    real(dp) :: div_beta, trace_X, AijAij, gdphi_da
    real(dp) :: alpha, phi, Ktr
    real(dp) :: divbeta_deriv(3)
    real(dp) :: RHS_gt(3,3), RHS_A(3,3)
    real(dp) :: RHS_Gam(3), term_a(3), term_b(3), term_c(3), term_d(3)
    integer  :: a, b, c

    call compute_stencil(u, i, k, S)
    call compute_conformal_geometry(S, G)

    gt    = G%gt
    gtinv = G%gtinv
    Amat     = pack6A(S%val)
    phi   = S%val(IPHI)
    Ktr     = S%val(IK)
    alpha = S%val(IALPHA)
    beta  = (/ S%val(IBETAX), S%val(IBETAY), S%val(IBETAZ) /)
    Gtvec = (/ S%val(IGTX), S%val(IGTY), S%val(IGTZ) /)
    dphi_l   = G%dphi
    dalpha_l = G%dalpha
    dK = (/ S%d1x(IK), S%d1y(IK), S%d1z(IK) /)

    Aup    = matmul(gtinv, matmul(Amat, gtinv))
    AikAkj = matmul(matmul(Amat, gtinv), Amat)
    AijAij = sum(Amat * Aup)

    R = G%Rt + G%Rphi

    dg(1,:,:) = pack6(S%d1x);  dg(2,:,:) = pack6(S%d1y);  dg(3,:,:) = pack6(S%d1z)
    dA(1,:,:) = pack6A(S%d1x); dA(2,:,:) = pack6A(S%d1y); dA(3,:,:) = pack6A(S%d1z)

    dbeta(1,:) = (/ S%d1x(IBETAX), S%d1x(IBETAY), S%d1x(IBETAZ) /)
    dbeta(2,:) = (/ S%d1y(IBETAX), S%d1y(IBETAY), S%d1y(IBETAZ) /)
    dbeta(3,:) = (/ S%d1z(IBETAX), S%d1z(IBETAY), S%d1z(IBETAZ) /)
    div_beta = dbeta(1,1) + dbeta(2,2) + dbeta(3,3)

    ! d2beta(a,b,c) = d_a d_b beta^c, for c = x,y,z <-> IBETAX,IBETAY,IBETAZ
    do c = 1, 3
      call fill_d2(S, IBETAX + (c-1), d2beta(:,:,c))
    end do

    dGt(1,:) = (/ S%d1x(IGTX), S%d1x(IGTY), S%d1x(IGTZ) /)
    dGt(2,:) = (/ S%d1y(IGTX), S%d1y(IGTY), S%d1y(IGTZ) /)
    dGt(3,:) = (/ S%d1z(IGTX), S%d1z(IGTY), S%d1z(IGTZ) /)

    gdphi_da = 0.0_dp
    do a = 1, 3
      do b = 1, 3
        gdphi_da = gdphi_da + gtinv(a,b)*dphi_l(b)*dalpha_l(a)
      end do
    end do

    ! ---------- phi ----------
    rhs_out(IPHI) = -alpha*Ktr/6.0_dp + sum(beta*dphi_l) + div_beta/6.0_dp

    ! ---------- gamma-tilde_ij ----------
    RHS_gt = -2.0_dp*alpha*Amat
    do a = 1, 3
      RHS_gt = RHS_gt + beta(a)*dg(a,:,:)
    end do
    RHS_gt = RHS_gt + matmul(gt, transpose(dbeta)) + transpose(matmul(gt, transpose(dbeta)))
    RHS_gt = RHS_gt - (2.0_dp/3.0_dp)*gt*div_beta

    rhs_out(IGXX) = RHS_gt(1,1); rhs_out(IGXY) = RHS_gt(1,2); rhs_out(IGXZ) = RHS_gt(1,3)
    rhs_out(IGYY) = RHS_gt(2,2); rhs_out(IGYZ) = RHS_gt(2,3); rhs_out(IGZZ) = RHS_gt(3,3)

    ! ---------- Ktr ----------
    rhs_out(IK) = -exp(-4.0_dp*phi)*sum(gtinv*G%DDa) &
                  + alpha*(AijAij + Ktr*Ktr/3.0_dp) + sum(beta*dK)

    ! ---------- Amat-tilde_ij ----------
    X = -G%DDa + alpha*R
    trace_X = sum(gtinv*X)

    if (present(msrc_pt)) then
      block
        real(dp) :: Emat, Smat_ij(3,3), Strace, gphysinv(3,3)
        real(dp) :: fourpi
        fourpi = 4.0_dp*3.14159265358979323846_dp
        Emat = msrc_pt(1)
        Smat_ij(1,1)=msrc_pt(5); Smat_ij(1,2)=msrc_pt(6); Smat_ij(1,3)=msrc_pt(7)
        Smat_ij(2,1)=msrc_pt(6); Smat_ij(2,2)=msrc_pt(8); Smat_ij(2,3)=msrc_pt(9)
        Smat_ij(3,1)=msrc_pt(7); Smat_ij(3,2)=msrc_pt(9); Smat_ij(3,3)=msrc_pt(10)

        gphysinv = exp(-4.0_dp*phi)*gtinv
        Strace = sum(gphysinv*Smat_ij)

        rhs_out(IK) = rhs_out(IK) + fourpi*alpha*(Emat + Strace)
        X = X - 2.0_dp*fourpi*alpha*Smat_ij
      end block
    end if

    XTF = X - (1.0_dp/3.0_dp)*gt*trace_X

    RHS_A = exp(-4.0_dp*phi)*XTF + alpha*(Ktr*Amat - 2.0_dp*AikAkj)
    do a = 1, 3
      RHS_A = RHS_A + beta(a)*dA(a,:,:)
    end do
    RHS_A = RHS_A + matmul(Amat, transpose(dbeta)) + transpose(matmul(Amat, transpose(dbeta)))
    RHS_A = RHS_A - (2.0_dp/3.0_dp)*Amat*div_beta

    rhs_out(IAXX) = RHS_A(1,1); rhs_out(IAXY) = RHS_A(1,2); rhs_out(IAXZ) = RHS_A(1,3)
    rhs_out(IAYY) = RHS_A(2,2); rhs_out(IAYZ) = RHS_A(2,3); rhs_out(IAZZ) = RHS_A(3,3)

    ! ---------- Gamma-tilde^i ----------
    do a = 1, 3
      divbeta_deriv(a) = d2beta(a,1,1) + d2beta(a,2,2) + d2beta(a,3,3)
    end do

    do a = 1, 3
      term_a(a) = 0.0_dp
      do b = 1, 3
        do c = 1, 3
          term_a(a) = term_a(a) + gtinv(b,c)*d2beta(b,c,a)
        end do
      end do
      term_a(a) = term_a(a) + (1.0_dp/3.0_dp)*sum(gtinv(a,:)*divbeta_deriv(:))
    end do

    do a = 1, 3
      term_b(a) = sum(beta(:)*dGt(:,a)) - sum(Gtvec(:)*dbeta(:,a)) &
                  + (2.0_dp/3.0_dp)*Gtvec(a)*div_beta
    end do

    do a = 1, 3
      term_c(a) = -2.0_dp*sum(Aup(a,:)*dalpha_l(:))
    end do

    do a = 1, 3
      term_d(a) = 0.0_dp
      do b = 1, 3
        do c = 1, 3
          term_d(a) = term_d(a) + G%Gam(a,b,c)*Aup(b,c)
        end do
      end do
      term_d(a) = term_d(a) - (2.0_dp/3.0_dp)*sum(gtinv(a,:)*dK(:)) &
                  + 6.0_dp*sum(Aup(a,:)*dphi_l(:))
      term_d(a) = 2.0_dp*alpha*term_d(a)
    end do

    RHS_Gam = term_a + term_b + term_c + term_d

    if (present(msrc_pt)) then
      block
        real(dp) :: fourpi, Smat_low(3), term_mom(3)
        fourpi = 4.0_dp*3.14159265358979323846_dp
        Smat_low = msrc_pt(2:4)
        do a = 1, 3
          term_mom(a) = -4.0_dp*fourpi*alpha*sum(gtinv(a,:)*Smat_low(:))
        end do
        RHS_Gam = RHS_Gam + term_mom
      end block
    end if

    rhs_out(IGTX) = RHS_Gam(1); rhs_out(IGTY) = RHS_Gam(2); rhs_out(IGTZ) = RHS_Gam(3)

    ! ---------- gauge: 1+log slicing, Gamma-driver shift ----------
    rhs_out(IALPHA) = -2.0_dp*alpha*Ktr + sum(beta*dalpha_l)
    rhs_out(IBETAX) = 0.75_dp*S%val(IBX)
    rhs_out(IBETAY) = 0.75_dp*S%val(IBY)
    rhs_out(IBETAZ) = 0.75_dp*S%val(IBZ)
    rhs_out(IBX) = RHS_Gam(1) - gauge_eta*S%val(IBX)
    rhs_out(IBY) = RHS_Gam(2) - gauge_eta*S%val(IBY)
    rhs_out(IBZ) = RHS_Gam(3) - gauge_eta*S%val(IBZ)

  end subroutine rhs_point

  ! Fills a 3x3 symmetric matrix of second derivatives (d2x,d2y,d2z,d2xy,d2xz,d2yz)
  ! for a single scalar field component ivar of the stencil bundle.
  subroutine fill_d2(S, ivar, M)
    type(stencil_t), intent(in) :: S
    integer, intent(in) :: ivar
    real(dp), intent(out) :: M(3,3)
    M(1,1) = S%d2x(ivar);  M(2,2) = S%d2y(ivar);  M(3,3) = S%d2z(ivar)
    M(1,2) = S%d2xy(ivar); M(2,1) = M(1,2)
    M(1,3) = S%d2xz(ivar); M(3,1) = M(1,3)
    M(2,3) = S%d2yz(ivar); M(3,2) = M(2,3)
  end subroutine fill_d2

end module bssn_rhs_mod
