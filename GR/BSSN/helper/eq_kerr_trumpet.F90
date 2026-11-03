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
  public :: ktr_kerr_spatial, ktr_kerr_spatial_orth, ktr_kerr_spatial_full
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
    real(dp) :: sum2, disc, a2
    sum2 = x*x + z*z
    a2 = a*a
    ! Standard Kerr-Schild / Boyer-Lindquist radius:
    !   r^2 = 1/2 [ (rho^2 - a^2) + sqrt((rho^2 - a^2)^2 + 4 a^2 z^2) ],
    !   rho^2 = x^2 + y^2 + z^2,
    ! equivalently rho^2 = r^2 + a^2 sin^2(th).  This is the SAME radius used by
    ! kerr_radius_squared() in the excision module, by the horizon finder, and
    ! by the Kerr-Schild initial data.
    !
    ! The previous code inverted rho^2 = r^2 + a^2 cos^2(th) instead.  That is
    ! the Boyer-Lindquist METRIC function, not the Euclidean radius.  Its map
    ! has a turning point at rho = 2a|cos(th)| where the Jacobian vanishes, and
    ! at a = 0.8 that point (2a = 1.6) coincides with the horizon r_+ = 1.6.
    disc = max((sum2-a2)*(sum2-a2) + 4.0_dp*a2*z*z, 0.0_dp)
    r = sqrt(max(0.5_dp*(sum2 - a2 + sqrt(disc)), 0.0_dp))
    ! ct is the STANDARD polar angle, cos(th) = z/rho.
    if (sum2 > 0.0_dp) then
      ct = z/sqrt(sum2)
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

  ! The FULL spatial metric in the (R, theta, phi) COORDINATE basis, including
  ! the off-diagonal g_Rphi that ktr_kerr_spatial omits.
  !
  ! From Dennison, Baumgarte & Montero (2014) eq. (12):
  !     g_RR   = rho^2/(R-R0)^2
  !     g_thth = rho^2
  !     g_phph = sin^2(th) xi1 / rho^2
  !     g_Rphi = -a sqrt(R0^2+2R(M-R0)-a^2)/(R-R0) sin^2(th)
  ! NOTE the factor: eq. (12) writes the cross term as
  !     -2a s/(R-R0) sin^2(th) dR dphi
  ! in the LINE ELEMENT, whose coefficient is 2*g_Rphi.  The metric component
  ! itself therefore carries a SINGLE factor a (this was an off-by-two that
  ! left an O(a) constraint violation concentrated near the equator).
  pure subroutine ktr_kerr_spatial_full(r,ct,st2,m,a,r0,gq)
    real(dp),intent(in) :: r,ct,st2,m,a,r0
    real(dp),intent(out) :: gq(3,3)
    real(dp) :: rho2, xi1, rm, s
    rm = r - r0
    rho2 = r*r + a*a*ct*ct
    xi1 = rho2*(r*r+a*a) + 2.0_dp*a*a*m*r*st2
    s = sqrt(max(r0*r0 + 2.0_dp*r*(m-r0) - a*a, 0.0_dp))
    gq = 0.0_dp
    gq(1,1) = rho2/(rm*rm)
    gq(2,2) = rho2
    gq(3,3) = st2/rho2*xi1
    gq(1,3) = -a*s*st2/rm
    gq(3,1) = gq(1,3)
  end subroutine ktr_kerr_spatial_full

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
  !  varpi = sqrt(x^2+y^2),  phi = atan2(y,x),  z = rho cos(theta),
  !  rho^2 = varpi^2 + z^2 and the Boyer-Lindquist radius obeys
  !      rho^2 = R^2 + a^2 sin^2(theta),
  !  i.e. R^2 = 1/2[(rho^2-a^2) + sqrt((rho^2-a^2)^2 + 4 a^2 z^2)].
  !  The spherical frame is
  !      e_R  = ( sin th cos phi, sin th sin phi,  cos th  )
  !      e_th = ( cos th cos phi, cos th sin phi, -sin th  )
  !      e_ph = ( -sin phi,        cos phi,          0      )
  ! -------------------------------------------------------------------------
  subroutine ktr_kerr_xyz(x,y,z,m,a,r0,alpha,bcart,gcart,ok)
    real(dp),intent(in)  :: x,y,z,m,a,r0
    real(dp),intent(out) :: alpha,bcart(3),gcart(3,3)
    logical,intent(out)   :: ok
    real(dp) :: varpi, phi, r, ct, st, bR, bT, bP, gq(3,3), E(3,3)
    real(dp) :: sum2, disc, cp, sp, re, Aval, detT, a11, a12, a21, a22, a33
    real(dp) :: Minv(3,3), tmp(3,3), h(3,3)
    integer :: p, q, i, q2

    varpi = sqrt(x*x + y*y)
    sum2 = varpi*varpi + z*z
    ! Standard Kerr-Schild / Boyer-Lindquist radius, the same one used by
    ! kerr_radius_squared() and by the horizon finder:
    !   rho^2 = r^2 + a^2 sin^2(th),  ct = z/rho.
    ! The old inversion of rho^2 = r^2 + a^2 cos^2(th) had a turning point at
    ! rho = 2a|cos(th)| where drho/dr = 0; at a = 0.8 that landed on the horizon.
    disc = max((sum2-a*a)*(sum2-a*a) + 4.0_dp*a*a*z*z, 0.0_dp)
    r  = sqrt(max(0.5_dp*(sum2 - a*a + sqrt(disc)), 0.0_dp))
    ! The DBM metric is written in Boyer-Lindquist (r,theta,phi), embedded as
    !   x = sqrt(r^2+a^2) sin(theta) cos(phi),  z = r cos(theta).
    ! Hence cos(theta) = z/r (the areal radius), NOT z/rho_E.  Using the
    ! Euclidean polar angle here mixes two charts and breaks stationarity.
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
    ! Floor st away from zero.  g_phph and g_Rphi both carry st^2 while the
    ! Jacobian inverse carries 1/(r_E st); the cancellation is exact, but it
    ! must be done with the SAME st in both places or the axis gives 0*Inf.
    st = max(st, 1.0e-100_dp)
    if (varpi > 0.0_dp) then
      phi = atan2(y,x)
    else
      phi = 0.0_dp
    end if
    cp = cos(phi); sp = sin(phi)

    alpha = ktr_kerr_alpha(r,ct,m,a,r0)
    call ktr_kerr_shift(r,ct,m,a,r0,bR,bT,bP)
    call ktr_kerr_spatial_full(r,ct,st*st,m,a,r0,gq)

    E = 0.0_dp
    E(1,1)=st*cp; E(2,1)=st*sp; E(3,1)=ct
    E(1,2)=ct*cp; E(2,2)=ct*sp; E(3,2)=-st
    E(1,3)=-sp;   E(2,3)=cp;   E(3,3)=0.0_dp

    ! ---------------------------------------------------------------------
    !  JACOBIAN TREATMENT.  The chart (R, theta, phi) is NOT orthogonal, and
    !  the Euclidean radius depends on theta as well as R, so the Jacobian
    !  d(x)/d(R,theta,phi) is upper triangular in the spherical frame:
    !
    !      T = [ d r_E/dR   d r_E/dth   0 ]
    !          [    0          r_E       0 ]
    !          [    0           0     r_E st]
    !
    !  and the metric transforms as  g_cart = E (T^{-T} gq T^{-1}) E^T.
    !  Using the inverse (not a diagonal approximation) is what makes the
    !  off-diagonal g_Rphi land correctly; a bolt-on cannot do it.
    !
    !  r_E = rho obeys rho^2 = R^2 + a^2 sin^2(th), so
    !      d rho/dR = R/rho,   d rho/dth = a^2 sin(th)cos(th)/rho,
    !  and at a = 0 this reduces to rho = R, d rho/dR = 1, d rho/dth = 0,
    !  i.e. T = diag(1, R, R sin th), which reproduces the exactly-correct
    !  Schwarzschild trumpet metric.
    ! ---------------------------------------------------------------------
    re = sqrt(sum2)
    if (re <= 0.0_dp) then
      ok = .false.
      alpha = 0.0_dp;  bcart = 0.0_dp;  gcart = 0.0_dp
      return
    end if
    ! Boyer-Lindquist embedding Jacobian in the orthonormal BL frame:
    !   j_r = d(x,y,z)/dr,  j_th = d(x,y,z)/dth,  j_ph = d(x,y,z)/dph.
    ! With A = sqrt(r^2+a^2),
    !   T = [ (r/A)st^2+ct^2   st ct (A-r)    0 ]
    !       [ st ct (r/A-1)    A ct^2+r st^2  0 ]
    !       [ 0                0               A st ]
    ! The (r,th) block is NOT triangular (unlike the Euclidean-angle chart),
    ! so the full 2x2 inverse is required.
    Aval = sqrt(r*r + a*a)
    a11 = (r/Aval)*st*st + ct*ct
    a21 = st*ct*(r/Aval - 1.0_dp)
    a12 = st*ct*(Aval - r)
    a22 = Aval*ct*ct + r*st*st
    a33 = Aval*st
    detT = a11*a22 - a12*a21
    if (abs(detT) <= tiny(1.0_dp) .or. a33 == 0.0_dp) then
      ok = .false.
      alpha = 0.0_dp;  bcart = 0.0_dp;  gcart = 0.0_dp
      return
    end if

    Minv = 0.0_dp
    Minv(1,1) =  a22/detT
    Minv(1,2) = -a12/detT
    Minv(2,1) = -a21/detT
    Minv(2,2) =  a11/detT
    Minv(3,3) = 1.0_dp/a33

    ! h = M^T gq M
    do p = 1, 3
      do q = 1, 3
        tmp(p,q) = 0.0_dp
        do i = 1, 3
          tmp(p,q) = tmp(p,q) + gq(p,i)*Minv(i,q)
        end do
      end do
    end do
    do p = 1, 3
      do q = 1, 3
        h(p,q) = 0.0_dp
        do i = 1, 3
          h(p,q) = h(p,q) + Minv(i,p)*tmp(i,q)
        end do
      end do
    end do

    do p = 1, 3
      do q = 1, 3
        gcart(p,q) = 0.0_dp
        do i = 1, 3
          do q2 = 1, 3
            gcart(p,q) = gcart(p,q) + E(p,i)*h(i,q2)*E(q,q2)
          end do
        end do
      end do
    end do

    ! The shift is a vector: beta^i = beta^r dx^i/dr + beta^th dx^i/dth
    !                             + beta^ph dx^i/dph.  Use the same full
    ! Jacobian columns as the metric transform.
    do i = 1, 3
      bcart(i) = bR*(a11*E(i,1) + a21*E(i,2)) &
               + bT*(a12*E(i,1) + a22*E(i,2)) &
               + bP*a33*E(i,3)
    end do
  end subroutine ktr_kerr_xyz


end module eq_kerr_trumpet_mod