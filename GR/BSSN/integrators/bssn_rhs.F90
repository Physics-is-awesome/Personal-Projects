module bssn_rhs_mod
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, gauge_eta, puncture_sign, gamma_driver_mode, &
                        slicing_mode
  use params_mod, only: bh_mass, bh_spin, bh_center_x, bh_center_z
  use grid_mod, only: xg, zg
  use vars_mod
  use stencil_mod, only: stencil_t, compute_stencil
  use geometry_mod, only: conformal_geometry_t, compute_conformal_geometry, pack6, pack6A
  use excision_mod, only: is_excised, excision_status
  implicit none
  private
  public :: compute_rhs_interior, compute_rhs_point, bm_f_of_alpha

contains

  subroutine compute_rhs_interior(u, rhs, msrc)
    real(dp), intent(in)    :: u(:,:,:)
    real(dp), intent(inout) :: rhs(:,:,:)
    real(dp), intent(in), optional :: msrc(:,:,:)  ! (Nx,Nz,10): E,Sx,Sy,Sz,Sxx,Sxy,Sxz,Syy,Syz,Szz
    real(dp) :: pt_rhs(NVARS)
    integer :: i, k

    do k = 2, Nz - 1
      do i = 1, Nx - 1
        if (is_excised(i,k)) cycle  ! frozen inside excision radius; refreshed by extrapolate_excised() each step
        if (present(msrc)) then
          call rhs_point(u, i, k, pt_rhs, msrc(i,k,:))
        else
          call rhs_point(u, i, k, pt_rhs)
        end if
        rhs(i,k,:) = pt_rhs
      end do
    end do
  end subroutine compute_rhs_interior

  ! Point-level entry point used by reduced/first-order drivers.  Keeping the
  ! legacy point operator as the single source of truth avoids silently
  ! introducing a second (and inevitably different) BSSN evolution system.
  subroutine compute_rhs_point(u, i, k, rhs_out)
    real(dp), intent(in)  :: u(:,:,:)
    integer, intent(in)   :: i, k
    real(dp), intent(out) :: rhs_out(NVARS)
    call rhs_point(u, i, k, rhs_out)
  end subroutine compute_rhs_point

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
    real(dp) :: Gpun(3)
    real(dp) :: dBx, dBy, dBz, lbeta_gam1, lbeta_gam2, lbeta_gam3
    real(dp) :: lbeta_B1, lbeta_B2, lbeta_B3
    integer  :: a, b, c

    call compute_stencil(u, i, k, excision_status, S)
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
        real(dp) :: fourpi, Smat_low(3), term_mom(3), gphysinv_mom(3,3)
        fourpi = 4.0_dp*3.14159265358979323846_dp
        Smat_low = msrc_pt(2:4)
        ! S^i = gamma^{ij} S_j uses the PHYSICAL inverse metric
        ! exp(-4phi)*gtinv, matching the gphysinv used for Strace above --
        ! not the bare conformal gtinv.
        gphysinv_mom = exp(-4.0_dp*phi)*gtinv
        do a = 1, 3
          term_mom(a) = -4.0_dp*fourpi*alpha*sum(gphysinv_mom(a,:)*Smat_low(:))
        end do
        RHS_Gam = RHS_Gam + term_mom
      end block
    end if

    rhs_out(IGTX) = RHS_Gam(1); rhs_out(IGTY) = RHS_Gam(2); rhs_out(IGTZ) = RHS_Gam(3)

    ! ---------- gauge: 1+log slicing, puncture Gamma-driver shift ----------
    !
    ! Gamma-driver with puncture:
    !
    !     d_t beta^a = (3/4) Gbar^a
    !     d_t Gbar^a = -eta (Gbar^a - Gamma^a),   Gamma^a = s * 2 M x^a / r^3
    !
    ! Gbar is carried in IBX/IBY/IBZ and is initialised from the data by
    ! compute_conformal_connections.  Note that d_t Gbar has NO Gtilde RHS:
    ! in a Gamma-driver the driver variable is driven purely towards the
    ! analytic profile.  Feeding it RHS_Gam (the Gtilde equations) as well --
    ! as this code used to -- couples Gbar to a different variable and lets
    ! the shift drift instead of being anchored.
    !
    ! s = puncture_sign: the equilibrium shift must point INWARD for a black
    ! hole, so the default is -1.
    ! Bona-Masso slicing  d_perp alpha = -alpha^2 f K  (Brown et al. eq. 2.11).
    !   f = 2/alpha -> 1+log (d_perp alpha = -2 alpha K)   [slicing_mode 0]
    !   f = 1/alpha -> maximal  (d_perp alpha = -alpha K)   [slicing_mode 1]
    if (slicing_mode == 2) then
      ! Freeze the Kerr-Schild lapse for the stationary-background control.
      rhs_out(IALPHA) = 0.0_dp
    else if (slicing_mode == 1) then
      rhs_out(IALPHA) = -alpha*Ktr + sum(beta*dalpha_l)
    else if (slicing_mode == 3) then
      ! Stationary Kerr slicing (route C).  Bona-Masso d_perp alpha =
      ! -alpha^2 f(alpha) K, with f(alpha) TABULATED, not closed form: the
      ! Kerr trumpet family is not 1+log and not maximal at any R0, but it IS
      ! stationary under this f.  See the MEASURED RESULT block in
      ! helper/eq_kerr_trumpet.F90 and helper/test_eq_kerr_data.F90 Parts
      ! D-H.  The alpha-dependence of f is REAL, not a finite-difference
      ! artefact: the spread is converged by h=1e-2 and settles at 4.3e-2.
      rhs_out(IALPHA) = -alpha*alpha*bm_f_of_alpha(alpha,bh_spin)*Ktr &
                       + sum(beta*dalpha_l)
    else
      rhs_out(IALPHA) = -2.0_dp*alpha*Ktr + sum(beta*dalpha_l)
    end if
    if (gamma_driver_mode == 2) then
      ! Freeze the Kerr-Schild shift and its unused driver variable.
      rhs_out(IBETAX:IBETAZ) = 0.0_dp
      rhs_out(IBX:IBZ) = 0.0_dp
    else if (gamma_driver_mode == 3) then
      ! Advective Gamma-driver in the Brown et al. convention:
      !   (d_t - beta^j D_j) beta^i = 3/4 B^i.
      rhs_out(IBETAX) = 0.75_dp*S%val(IBX) + sum(beta*dbeta(:,1))
      rhs_out(IBETAY) = 0.75_dp*S%val(IBY) + sum(beta*dbeta(:,2))
      rhs_out(IBETAZ) = 0.75_dp*S%val(IBZ) + sum(beta*dbeta(:,3))
    else
      rhs_out(IBETAX) = 0.75_dp*S%val(IBX)
      rhs_out(IBETAY) = 0.75_dp*S%val(IBY)
      rhs_out(IBETAZ) = 0.75_dp*S%val(IBZ)
    end if
    call puncture_gamma(i, k, Gpun)
    if (gamma_driver_mode == 1) then
      rhs_out(IBX) = -gauge_eta*(S%val(IBX) - puncture_sign*Gpun(1))
      rhs_out(IBY) = -gauge_eta*(S%val(IBY) - puncture_sign*Gpun(2))
      rhs_out(IBZ) = -gauge_eta*(S%val(IBZ) - puncture_sign*Gpun(3))
    else if (gamma_driver_mode == 4) then
      ! Non-advective Gamma-driver:
      !   d_t beta^i = 3/4 B^i,
      !   d_t B^i    = d_t Lambda-tilde^i - eta B^i.
      ! This form admits a stationary B=0 solution whenever the BSSN fields
      ! and conformal connection are stationary.  Keep it separate from mode
      ! 0, whose material/Lie-derivative conversions are retained unchanged.
      rhs_out(IBX) = RHS_Gam(1) - gauge_eta*S%val(IBX)
      rhs_out(IBY) = RHS_Gam(2) - gauge_eta*S%val(IBY)
      rhs_out(IBZ) = RHS_Gam(3) - gauge_eta*S%val(IBZ)
    else if (gamma_driver_mode == 0) then
      ! The driver equations use d_perp B^i = d_perp Lambda^i - eta B^i.
      ! RHS_Gam is d_t Lambda^i and already contains beta.dLambda. Convert
      ! both material derivatives to partial time derivatives explicitly.
      dBx = beta(1)*S%d1x(IBX) + beta(2)*S%d1y(IBX) + beta(3)*S%d1z(IBX)
      dBy = beta(1)*S%d1x(IBY) + beta(2)*S%d1y(IBY) + beta(3)*S%d1z(IBY)
      dBz = beta(1)*S%d1x(IBZ) + beta(2)*S%d1y(IBZ) + beta(3)*S%d1z(IBZ)
      lbeta_gam1 = sum(beta*dbeta(:,1)) + Gtvec(1)*div_beta
      lbeta_gam2 = sum(beta*dbeta(:,2)) + Gtvec(2)*div_beta
      lbeta_gam3 = sum(beta*dbeta(:,3)) + Gtvec(3)*div_beta
      lbeta_B1   = dBx + S%val(IBX)*div_beta
      lbeta_B2   = dBy + S%val(IBY)*div_beta
      lbeta_B3   = dBz + S%val(IBZ)*div_beta
      rhs_out(IBX) = RHS_Gam(1) - lbeta_gam1 - gauge_eta*S%val(IBX) + lbeta_B1
      rhs_out(IBY) = RHS_Gam(2) - lbeta_gam2 - gauge_eta*S%val(IBY) + lbeta_B2
      rhs_out(IBZ) = RHS_Gam(3) - lbeta_gam3 - gauge_eta*S%val(IBZ) + lbeta_B3
    else if (gamma_driver_mode == 3) then
      ! The standard choices in Brown et al. have H=e^(4 phi), so the
      ! e^(-4 phi) H factor is one.  Apply d_0 = d_t-beta^j D_j to both
      ! Lambda-tilde and B, then solve for d_t B:
      !   d_t B^i = RHS_Gam^i - beta^j d_j Lambda-tilde^i
      !             + beta^j d_j B^i - eta B^i.
      dBx = beta(1)*S%d1x(IBX) + beta(2)*S%d1y(IBX) + beta(3)*S%d1z(IBX)
      dBy = beta(1)*S%d1x(IBY) + beta(2)*S%d1y(IBY) + beta(3)*S%d1z(IBY)
      dBz = beta(1)*S%d1x(IBZ) + beta(2)*S%d1y(IBZ) + beta(3)*S%d1z(IBZ)
      rhs_out(IBX) = RHS_Gam(1) - sum(beta*dGt(:,1)) + dBx - gauge_eta*S%val(IBX)
      rhs_out(IBY) = RHS_Gam(2) - sum(beta*dGt(:,2)) + dBy - gauge_eta*S%val(IBY)
      rhs_out(IBZ) = RHS_Gam(3) - sum(beta*dGt(:,3)) + dBz - gauge_eta*S%val(IBZ)
    end if


  end subroutine rhs_point

  ! ---------------------------------------------------------------------------
  !  f(alpha) for the stationary Kerr slicing (slicing_mode 3).
  !
  !  At a = 0 the closed form is exact:  f = (1 - alpha) / (3 alpha).
  !  At a = 0.8 it is tabulated below.  The table was emitted by Part H of
  !  helper/test_eq_kerr_data.F90 at a converged step h = 1e-4.
  !
  !  SPIN DEPENDENCE IS INTERPOLATED, NOT MEASURED.  Between a = 0 and
  !  a = 0.8 this blends the closed form into the table, which reproduces both
  !  endpoints exactly.  Interior spins are an unvalidated interpolation; if a
  !  different spin matters, regenerate the table for it rather than trusting
  !  the blend.
  !
  !  Below the first tabulated alpha the closed form is used, because it has
  !  the correct trumpet divergence f ~ 1/(3 alpha); above the last entry the
  !  value is held, since f is smoothly decreasing and flattening there.
  ! ---------------------------------------------------------------------------
  pure function bm_f_of_alpha(alpha, a) result(f)
    real(dp), intent(in) :: alpha, a
    real(dp) :: f, w, f0
    integer, parameter :: NTAB = 12
    real(dp), parameter :: tab_a(NTAB) = [ &
         0.37628835_dp, 0.48931866_dp, 0.57076556_dp, 0.63116088_dp, &
         0.67729248_dp, 0.71348553_dp, 0.74254733_dp, 0.76635005_dp, &
         0.78617810_dp, 0.80293661_dp, 0.81727923_dp, 0.82968850_dp ]
    real(dp), parameter :: tab_f(NTAB) = [ &
         0.53371056_dp, 0.35774236_dp, 0.26516450_dp, 0.20851406_dp, &
         0.17067473_dp, 0.14384404_dp, 0.12395662_dp, 0.10869775_dp, &
         0.09666101_dp, 0.08694744_dp, 0.07895832_dp, 0.07228104_dp ]
    real(dp), parameter :: A_TAB = 0.8_dp

    if (alpha <= 1.0e-12_dp) then
      f = huge(1.0_dp)
      return
    end if
    f0 = bm_f_tab(alpha)
    if (a <= 0.0_dp) then
      f = f0
    else if (a >= A_TAB) then
      f = bm_f_tab(alpha)
    else
      w = a/A_TAB
      f = (1.0_dp - w)*f0 + w*bm_f_closed(alpha)
    end if
  end function bm_f_of_alpha

  ! Interpolated table value alone (the a = 0.8 measurement).
  pure function bm_f_tab(alpha) result(f)
    real(dp), intent(in) :: alpha
    real(dp) :: f
    integer, parameter :: NTAB = 12
    real(dp), parameter :: tab_a(NTAB) = [ &
         0.37628835_dp, 0.48931866_dp, 0.57076556_dp, 0.63116088_dp, &
         0.67729248_dp, 0.71348553_dp, 0.74254733_dp, 0.76635005_dp, &
         0.78617810_dp, 0.80293661_dp, 0.81727923_dp, 0.82968850_dp ]
    real(dp), parameter :: tab_f(NTAB) = [ &
         0.53371056_dp, 0.35774236_dp, 0.26516450_dp, 0.20851406_dp, &
         0.17067473_dp, 0.14384404_dp, 0.12395662_dp, 0.10869775_dp, &
         0.09666101_dp, 0.08694744_dp, 0.07895832_dp, 0.07228104_dp ]
    integer :: lo, hi, mid
    if (alpha <= tab_a(1)) then
      f = (1.0_dp - alpha)/(3.0_dp*alpha)      ! closed form: right divergence
      return
    end if
    if (alpha >= tab_a(NTAB)) then
      f = tab_f(NTAB)
      return
    end if
    lo = 1;  hi = NTAB
    do while (hi - lo > 1)
      mid = (lo + hi)/2
      if (alpha < tab_a(mid)) then
        hi = mid
      else
        lo = mid
      end if
    end do
    f = tab_f(lo) + (tab_f(hi) - tab_f(lo))* &
        (alpha - tab_a(lo))/(tab_a(hi) - tab_a(lo))
  end function bm_f_tab

  pure function bm_f_closed(alpha) result(f)
    real(dp), intent(in) :: alpha
    real(dp) :: f
    f = (1.0_dp - alpha)/(3.0_dp*alpha)
  end function bm_f_closed



  ! ---------------------------------------------------------------------------
  ! Analytic puncture driver profile  Gamma^a = 2 M x^a / r^3, with x measured
  ! from the black-hole centre and r the Kerr-Schild radius (which for a=0
  ! reduces to the ordinary Euclidean radius).  The Cartesian y component
  ! vanishes on the stored plane by symmetry.
  !
  ! Returns 0 outside the excision region so the 1/r^3 singularity can never be
  ! evaluated at the puncture; those points are frozen anyway.
  ! ---------------------------------------------------------------------------
  subroutine puncture_gamma(i, k, Gpun)
    integer, intent(in) :: i, k
    real(dp), intent(out) :: Gpun(3)
    real(dp) :: xr, zr, rho2, a2, r2, r3

    Gpun = 0.0_dp
    if (.not. allocated(xg)) return

    xr = xg(i) - bh_center_x
    zr = zg(k) - bh_center_z
    a2 = bh_spin*bh_spin
    rho2 = xr*xr + zr*zr
    r2 = 0.5_dp*(rho2 - a2 + sqrt((rho2 - a2)*(rho2 - a2) + 4.0_dp*a2*zr*zr))
    if (r2 <= 0.0_dp) return
    r3 = r2*sqrt(r2)
    Gpun(1) = 2.0_dp*bh_mass*xr/r3
    Gpun(3) = 2.0_dp*bh_mass*zr/r3
    Gpun(2) = 0.0_dp
  end subroutine puncture_gamma

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
