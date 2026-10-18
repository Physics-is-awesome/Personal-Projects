! ===========================================================================
!  Stationary Kerr "trumpet" data (analytic family).
!
!  Reference
!  ---------
!  K. A. Dennison, T. W. Baumgarte, P. J. Montero, "Trumpet Slices in Kerr
!  Spacetimes", Phys. Rev. Lett. 113, 261101 (2014), arXiv:1409.1887.
!
!  What this provides
!  -----------------
!  Lapse, shift and spatial metric for vacuum Kerr (Lambda = 0, Q = 0) in the
!  DBM coordinate family, at arbitrary Cartesian points on the y = 0 plane.
!  This is the Kerr analogue of eq_schwarzschild_data_mod and is what
!  production initial data should use instead of Kerr-Schild: Kerr-Schild is a
!  stationary ADM slice but is NOT a slice of the slicing condition this code
!  evolves by, which is why the horizon migrates and the run dies.
!
!  The family parameter
!  --------------------
!  DBM start from an arbitrary radial function f(R); the slice condition is set
!  by that choice, so R0 IS the slicing parameter.  The paper requires
!
!      M - sqrt(M^2 - a^2)  <=  R0  <=  M
!
!  for reality of g_tR.  R0 = M is the common choice (correct ADM mass, and it
!  reduces to Boyer-Lindquist at extremality), and it is what earlier versions
!  of this module hardcoded.
!
!  MEASURED RESULT -- THE FAMILY IS USABLE, BUT NOT UNDER 1+log
!  ==============================================================
!  Established by helper/test_eq_kerr_data.F90 (Parts A-H).  Read those, do not
!  re-derive them.
!
!  (1) The closed forms, the coordinate map and the trumpet geometry are sound
!      (Part A): lapse -> 1, shift -> 0, metric -> flat at infinity; symmetric
!      to machine zero; positive definite; det > 0 across a = 0..0.8; and
!      g_xx diverges as 1/(R-R0)^2 approaching the trumpet (ratio 3.992 vs 4).
!
!  (2) The family is NOT 1+log and NOT maximal (Part C).  Scanning R0 over its
!      admissible range [M - sqrt(M^2-a^2), M] = [0.4, 1.0] at a = 0.8, both
!      slicing residuals fall monotonically toward R0 = M and neither
!      approaches zero (1+log 9.4e-2, maximal 5.0e-2).
!
!  (3) BUT the family IS stationary under some Bona-Masso gauge.  For a
!      stationary slice  beta.d(alpha) = alpha^2 f K, so
!
!          f(alpha) = (beta.d(alpha)) / (alpha^2 K)
!
!      is a function of alpha ALONE (Part E: binning by alpha across many
!      (R,theta), the spread of f inside each bin is 0.3-0.7%).  This required
!      the full 3-D reconstruction (ktr_kerr_xyz): an in-plane-only K omits the
!      azimuthal contribution and inflates the apparent spread to 25-43%, which
!      is a spurious negative result.
!
!  (4) For a = 0 the closed form is exact:  f = (1-alpha)/(3 alpha).  For
!      a = 0.8 it is close to  c(1/alpha - 1) but c is NOT constant -- it varies
!      ~4.3% across the alpha range.  That variation is REAL: the finite
!      differences are converged by h = 1e-2 and the spread settles at 0.0435
!      rather than decaying (Part G).  So f(alpha) must be TABULATED, not
!      written in closed form.
!
!  (5) Part H emits the converged f(alpha) table to paste into the slicing mode.
!
!  CONSEQUENCE: no numerical stationary-gauge solve is needed.  Add a
!  slicing_mode whose RHS is  d_t alpha = -alpha^2 f(alpha) K  with f from that
!  table, keep a shift driver that has these beta as a fixed point, and supply
!  initial data from this module with K from the validated
!  K_ij = (L_beta gamma)_ij / (2 alpha) route.
!
!  TRAPS, all hit during this work:
!   * The "Kerr radius" in helper/excision.F90 is the Kerr-SCHILD radial
!     coordinate, NOT the areal radius used here.
!   * beta.d(alpha) uses the ALPHA derivatives, not the derivatives of beta;
!     using dB gives a divergence instead and flips the sign of f.
!   * K must be reconstructed with all three directions; see (3).
!
!  The line element (vacuum)
!  -------------------------
!      rho^2 = R^2 + a^2 cos^2(theta)
!      S     = sqrt(R0^2 + 2 R (M - R0) - a^2)
!      xi_1  = rho^2 (R^2 + a^2) + 2 a^2 M R sin^2(theta)
!      xi_2  = (R^2 + a^2)^2 - a^2 (R - R0)^2 sin^2(theta)
!
!      ds^2 = -(rho^2 - 2MR)/rho^2 dt^2 + 2 S/(R-R0) dt dR
!            - 4 a M R sin^2(theta)/rho^2 dt dphi
!            + rho^2/(R-R0)^2 dR^2
!            - 2 a S sin^2(theta)/(R-R0) dR dphi
!            + rho^2 dtheta^2 + (sin^2(theta)/rho^2) xi_1 dphi^2
!
!      alpha    = rho (R - R0) / sqrt(xi_2)
!      beta^R   = (R^2 + a^2)/rho^2 * alpha^2 * g_tR,  g_tR = S/(R-R0)
!      beta^phi= -a [R^2 + a^2 - (R-R0)^2] / [rho^2 (R-R0)^2] * alpha^2
!      gamma_ij = g_ij  (the spacetime spatial block)
!
!  Coordinate map
!  --------------
!  On the y = 0 plane with x >= 0 we have phi = 0 and the areal radius obeys
!      R^4 - (x^2 + z^2) R^2 + a^2 z^2 = 0,
!  whose physical root is R^2 = [(x^2+z^2) + sqrt((x^2+z^2)^2 - 4 a^2 z^2)]/2.
!  NOTE this is the AREAL radius.  The "Kerr radius" of helper/excision.F90 is
!  the Kerr-Schild radial coordinate, which is a different quantity.
! ===========================================================================
module eq_kerr_trumpet_mod
  use kinds_mod, only: dp
  implicit none
  private

  public :: ktr_kerr_alpha
  public :: ktr_kerr_shift
  public :: ktr_kerr_spatial, ktr_kerr_spatial_orth
  public :: ktr_kerr_rtheta
  public :: ktr_kerr_cartesian
  public :: ktr_kerr_xyz
  public :: ktr_trumpet_valid
  public :: ktr_r0_range

contains

  ! Smallest and largest admissible R0 for a given spin.
  pure subroutine ktr_r0_range(m,a,rlo,rhi)
    real(dp),intent(in)  :: m,a
    real(dp),intent(out) :: rlo,rhi
    rlo = m - sqrt(max(m*m - a*a, 0.0_dp))
    rhi = m
  end subroutine ktr_r0_range

  pure subroutine ktr_kerr_rtheta(x,z,m,a,r,ct)
    real(dp),intent(in)  :: x,z,m,a
    real(dp),intent(out) :: r,ct
    real(dp) :: sum2, disc
    sum2 = x*x + z*z
    disc = max(sum2*sum2 - 4.0_dp*a*a*z*z, 0.0_dp)
    r = sqrt(max(0.5_dp*(sum2 + sqrt(disc)), 0.0_dp))
    if (r > 0.0_dp) then
      ct = z/r
    else
      ct = 1.0_dp
    end if
  end subroutine ktr_kerr_rtheta

  ! Lapse.  Vanishes at R = R0, the trumpet surface.
  pure function ktr_kerr_alpha(r,ct,m,a,r0) result(al)
    real(dp),intent(in) :: r,ct,m,a,r0
    real(dp) :: al, rho2, xi2, st2, rm
    rho2 = r*r + a*a*ct*ct
    st2  = 1.0_dp - ct*ct
    rm   = r - r0
    xi2  = (r*r+a*a)*(r*r+a*a) - a*a*rm*rm*st2
    if (xi2 <= 0.0_dp) then
      al = 0.0_dp
      return
    end if
    al = sqrt(rho2)*rm/sqrt(xi2)
  end function ktr_kerr_alpha

  pure subroutine ktr_kerr_shift(r,ct,m,a,r0,bR,bT,bP)
    real(dp),intent(in)  :: r,ct,m,a,r0
    real(dp),intent(out) :: bR,bT,bP
    real(dp) :: rho2, al, gtr, rm, s, s2
    rm = r - r0
    rho2 = r*r + a*a*ct*ct
    if (rho2 <= 0.0_dp .or. abs(rm) <= tiny(1.0_dp)) then
      bR=0.0_dp; bT=0.0_dp; bP=0.0_dp
      return
    end if
    al = ktr_kerr_alpha(r,ct,m,a,r0)
    s2 = r0*r0 + 2.0_dp*r*(m-r0) - a*a
    s  = sqrt(max(s2,0.0_dp))
    gtr = s/rm
    bR = (r*r+a*a)/rho2 * al*al * gtr
    bP = -a*( r*r + a*a - rm*rm )/(rho2*rm*rm) * al*al
    bT = 0.0_dp
  end subroutine ktr_kerr_shift

  pure subroutine ktr_kerr_spatial(r,ct,m,a,r0,g)
    real(dp),intent(in)  :: r,ct,m,a,r0
    real(dp),intent(out) :: g(3,3)
    real(dp) :: rho2, xi1, st2, rm
    rm = r - r0
    rho2 = r*r + a*a*ct*ct
    st2 = 1.0_dp - ct*ct
    xi1 = rho2*(r*r+a*a) + 2.0_dp*a*a*m*r*st2
    g = 0.0_dp
    g(1,1) = rho2/(rm*rm)
    g(2,2) = rho2
    g(3,3) = st2/rho2*xi1
  end subroutine ktr_kerr_spatial

  ! The SAME metric directly in the ORTHONORMAL frame.  Preferred by callers
  ! that rotate to Cartesian: the sin^2(theta) factor is cancelled ANALYTICALLY
  ! here instead of being divided out, so there is no catastrophic cancellation
  ! on the axis and no special case is needed at varpi = 0.
  pure subroutine ktr_kerr_spatial_orth(r,ct,m,a,r0,h)
    real(dp),intent(in) :: r,ct,m,a,r0
    real(dp),intent(out) :: h(3,3)
    real(dp) :: rho2, xi1, st2, rm
    rm = r - r0
    rho2 = r*r + a*a*ct*ct
    st2 = 1.0_dp - ct*ct
    xi1 = rho2*(r*r+a*a) + 2.0_dp*a*a*m*r*st2
    h = 0.0_dp
    h(1,1) = rho2/(rm*rm)
    h(2,2) = rho2/(r*r)
    h(3,3) = xi1/(rho2*r*r)      ! = (st2*xi1/rho2)/(r^2 st2), no cancellation
  end subroutine ktr_kerr_spatial_orth

  pure function ktr_trumpet_valid(r,m,a,r0) result(ok)
    real(dp),intent(in) :: r,m,a,r0
    logical :: ok
    ok = (r > r0*(1.0_dp+1.0e-12_dp)) .and. (r == r) .and. &
         (abs(a) < m) .and. (m > 0.0_dp) .and. (r0 > 0.0_dp)
  end function ktr_trumpet_valid

  ! Cartesian assembly on the y = 0 plane, phi = 0.  With phi = 0 the spherical
  ! frame has e_R = (sin th,0,cos th), e_th = (cos th,0,-sin th), e_ph = (0,1,0),
  ! so the transform is a real 3x3 rotation carrying both the metric and shift.
  subroutine ktr_kerr_cartesian(x,z,m,a,r0,alpha,bcart,gcart,ok)
    real(dp),intent(in)  :: x,z,m,a,r0
    real(dp),intent(out) :: alpha,bcart(3),gcart(3,3)
    logical,intent(out)   :: ok
    real(dp) :: r, ct, st, bR, bT, bP, g(3,3), E(3,3), h(3,3)

    call ktr_kerr_rtheta(x,z,m,a,r,ct)
    ok = ktr_trumpet_valid(r,m,a,r0)
    if (.not. ok) then
      alpha = 0.0_dp
      bcart = 0.0_dp
      gcart = 0.0_dp
      return
    end if
    st = sqrt(max(1.0_dp-ct*ct, 0.0_dp))

    alpha = ktr_kerr_alpha(r,ct,m,a,r0)
    call ktr_kerr_shift(r,ct,m,a,r0,bR,bT,bP)
    call ktr_kerr_spatial(r,ct,m,a,r0,g)

    E = 0.0_dp
    E(1,1)=st;  E(2,1)=0.0_dp; E(3,1)=ct
    E(1,2)=ct;  E(2,2)=0.0_dp; E(3,2)=-st
    E(1,3)=0.0_dp; E(2,3)=1.0_dp; E(3,3)=0.0_dp

    ! FRAME CONVERSION.  ktr_kerr_spatial returns the metric in the
    ! COORDINATE basis (r, theta, phi): at large r it gives
    ! (1, r^2, r^2 sin^2 theta), which is exactly flat space in spherical
    ! coordinates, so g_phi_phi legitimately carries a sin^2(theta) factor and
    ! det(g) -> 0 on the axis.
    !
    ! Rotating that with T = E g E^T -- as this code used to -- treats it as an
    ! ORTHONORMAL-frame tensor and produces a Cartesian metric that is wrong
    ! everywhere and singular on the axis.  The correct relation is
    !
    !     g_cart = J^{-T} g_coord J^{-1},    J_ia = dx^i / dq^a
    !
    ! which for the orthonormal frame E with scale factors s = (1, r, r sin th)
    ! (J = s E) is equivalent to first converting to the orthonormal frame,
    ! h_ab = g_ab / (s_a s_b), and THEN rotating:  g_cart = E h E^T.
    ! Verified against flat space, where only this form returns g_xx = 1.
    call ktr_kerr_spatial_orth(r,ct,m,a,r0,h)

    ! The shift is a vector: beta^i = beta^r e_r^i + r beta^th e_th^i
    !                             + r sin(th) beta^ph e_ph^i
    ! so it needs the SAME scale factors.  Dropping them made beta wrong by
    ! factors of r and r sin(th).
    bcart = bR*E(:,1) + r*bT*E(:,2) + r*st*bP*E(:,3)
    gcart = matmul(E,matmul(h,transpose(E)))
  end subroutine ktr_kerr_cartesian

  ! -------------------------------------------------------------------------
  !  Full 3-D evaluation at an arbitrary Cartesian point (x, y, z).
  !
  !  ktr_kerr_cartesian is confined to the y = 0 plane (phi = 0) because that
  !  is all the storage plane holds.  That is enough to build initial data but
  !  NOT enough to measure y-derivatives, which the full mean curvature needs
  !  away from the equatorial plane.  This routine evaluates anywhere, so the
  !  caller can difference in y.
  !
  !  varpi = sqrt(x^2+y^2),  phi = atan2(y,x),  z = R cos(theta)
  !  and the areal radius obeys
  !      R^4 - (varpi^2 + z^2) R^2 + a^2 z^2 = 0.
  !  The spherical frame is
  !      e_R  = ( sin th cos phi, sin th sin phi,  cos th  )
  !      e_th = ( cos th cos phi, cos th sin phi, -sin th  )
  !      e_ph = ( -sin phi,        cos phi,          0      )
  ! -------------------------------------------------------------------------
  subroutine ktr_kerr_xyz(x,y,z,m,a,r0,alpha,bcart,gcart,ok)
    real(dp),intent(in)  :: x,y,z,m,a,r0
    real(dp),intent(out) :: alpha,bcart(3),gcart(3,3)
    logical,intent(out)   :: ok
    real(dp) :: varpi, phi, r, ct, st, bR, bT, bP, g(3,3), E(3,3), h(3,3)
    real(dp) :: sum2, disc, cp, sp


    varpi = sqrt(x*x + y*y)
    sum2 = varpi*varpi + z*z
    disc = max(sum2*sum2 - 4.0_dp*a*a*z*z, 0.0_dp)
    r  = sqrt(max(0.5_dp*(sum2 + sqrt(disc)), 0.0_dp))
    ct = 1.0_dp
    if (r > 0.0_dp) ct = z/r
    ok = ktr_trumpet_valid(r,m,a,r0)
    if (.not. ok) then
      alpha = 0.0_dp
      bcart = 0.0_dp
      gcart = 0.0_dp
      return
    end if
    st = sqrt(max(1.0_dp-ct*ct, 0.0_dp))
    if (varpi > 0.0_dp) then
      phi = atan2(y,x)
    else
      phi = 0.0_dp
    end if
    cp = cos(phi); sp = sin(phi)

    alpha = ktr_kerr_alpha(r,ct,m,a,r0)
    call ktr_kerr_shift(r,ct,m,a,r0,bR,bT,bP)
    call ktr_kerr_spatial(r,ct,m,a,r0,g)

    E = 0.0_dp
    E(1,1)=st*cp; E(2,1)=st*sp; E(3,1)=ct
    E(1,2)=ct*cp; E(2,2)=ct*sp; E(3,2)=-st
    E(1,3)=-sp;   E(2,3)=cp;   E(3,3)=0.0_dp

    ! Coordinate basis -> orthonormal -> Cartesian.  See the long note in
    ! ktr_kerr_cartesian: g_phi_phi carries sin^2(theta) because it is a
    ! coordinate-basis component, so it must be divided by the scale factors
    ! BEFORE rotating, and the shift needs those same scale factors.
    call ktr_kerr_spatial_orth(r,ct,m,a,r0,h)
    bcart = bR*E(:,1) + r*bT*E(:,2) + r*st*bP*E(:,3)
    gcart = matmul(E,matmul(h,transpose(E)))
  end subroutine ktr_kerr_xyz

end module eq_kerr_trumpet_mod