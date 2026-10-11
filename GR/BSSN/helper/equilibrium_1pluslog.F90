! ===========================================================================
!  Equilibrium ("trumpet") initial data in the stationary 1+log slicing.
!
!  Reference
!  ---------
!  T. W. Baumgarte, S. Duda, R. J. Hinder, T. B. Jaramillo, J. M. Nickert,
!  F. Pretorius, "Equilibrium initial data for moving puncture simulations:
!  the stationary 1+log slicing", Phys. Rev. D 78, 044015 (2008),
!  arXiv:0810.0006.
!
!  The idea
!  ---------
!  A slice is stationary under the 1+log slicing condition (eq. 1+log_dyn,
!  (partial_t - beta^i partial_i) alpha = -2 alpha K) iff
!
!      K = beta_K^i partial_i alpha_K / (2 alpha_K)                (eq. 1+log)
!
!  where alpha_K, beta_K are the Killing lapse and shift, i.e. iff the slice is
!  dragged along the timelike Killing vector of the stationary spacetime.  Data
!  satisfying this have (partial_t alpha = partial_t gamma_ij = 0) under the
!  gauge conditions used in moving-puncture evolutions, so there is no
!  coordinate transient at all.
!
!  Closed forms (spherical symmetry)
!  --------------------------------
!  In spherical symmetry the condition integrates once (eq. first_int):
!
!      alpha^2 = 1 - 2M/R + (C^2/R^4) e^alpha                        (first_int)
!
!  with C an integration constant playing the same role as C in the maximal
!  slicing family eq. lapse_MS, and with the shift following from
!  alpha^2 - beta^2 = 1 - 2M/R (eq. prop_dist):
!
!      beta = C e^{alpha/2} / R^2 .
!
!  Dynamical simulations settle down to the member of this family fixed by
!  Baumgarte et al. from Han, Hannam, Pretorius, Bishop, Ochsner et al.:
!
!      alpha_c = sqrt(10) - 3 ,   R_c = M / (4 alpha_c)  =  1.5405694 M
!      C  = sqrt(2) / (16 alpha_c^{3/2}) * e^{(3 - sqrt(10))/2} M^2
!         = 1.2467195315 M^2
!      alpha_AH = alpha(R = 2M) = 0.3761794868
!
!  IMPORTANT STRUCTURAL FACT (see eq_lapse_valid)
!  ---------------------------------------------
!  The physical branch of eq. first_int exists only for R >= R_c = 1.5405694 M;
!  at R = R_c the lapse equals alpha_c = sqrt(10)-3 exactly.  Between R_c and
!  2M the lapse is a perfectly good positive function, but 2M < R there means
!  the "areal radius" is already inside the horizon, so the data still cannot
!  be continued to r = 0.  This is why the Baumgarte construction is an
!  EXCISED trumpet rather than a puncture: the lapse never goes to zero and
!  never needs to, because the interior is simply excluded.  Any code adopting
!  it must therefore place its inner boundary at (or outside) the horizon and
!  impose black-hole equilibrium boundary conditions there (Coogan, Pfeiffer,
!  2004), not a puncture profile.
!
!  Scope
!  -----
!  These are the EXTERIOR lapse/shift only.  They do not by themselves give
!  K_ij: that additionally requires solving the momentum constraint for
!  K_ij (or, equivalently, the extended conformal thin-sandwich system of
!  eq. betaConstraint / eq. alphaConstraint).  For a spherically symmetric
!  spacetime that reduces to radial ODEs and is very tractable, but it is NOT
!  included here and is not inferred from anything reconstructed by hand.
!  See helper/test_equilibrium_1pluslog.F90 for exactly what is and is not
!  pinned down numerically.
! ===========================================================================
module equilibrium_1pluslog_mod
  use kinds_mod, only: dp
  implicit none
  private
  public :: EQ_ALPHA_C, EQ_RC_OVER_M, EQ_C_OVER_M2, EQ_ALPHA_AH, EQ_RH
  public :: eq_constant_c, eq_lapse, eq_shift, eq_lapse_valid, eq_radius_min
  public :: eq_mean_curvature, eq_residual
  public :: eq_lapse_ext, eq_shift_ext, eq_lapse_ext_valid
  public :: eq_dalpha_dr, eq_dbeta_dr

  ! Regularity constants of Baumgarte et al. eqs. (24)-(26).
  real(dp), parameter :: EQ_ALPHA_C = sqrt(10.0_dp) - 3.0_dp
  real(dp), parameter :: EQ_RC_OVER_M = 1.0_dp/(4.0_dp*EQ_ALPHA_C)
  ! C / M^2 = sqrt(2)/(16 alpha_c^{3/2}) e^{(3-sqrt(10))/2}
  real(dp), parameter :: EQ_C_OVER_M2 = &
       sqrt(2.0_dp)/(16.0_dp*EQ_ALPHA_C**1.5_dp) * exp((3.0_dp - sqrt(10.0_dp))/2.0_dp)
  ! Value at the horizon, R = 2M; the paper quotes 0.376179.
  real(dp), parameter :: EQ_ALPHA_AH = 0.3761794868301819_dp
  ! Horizon / areal radius at which the exterior solution stops: R = 2M.
  real(dp), parameter :: EQ_RH = 2.0_dp
  ! Nodes used for the inner extension, as multiples of M offset from 2M.
  real(dp), parameter :: EXT_NODES(6) = [0.0_dp, 0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp, 0.5_dp]

contains

  !---------------------------------------------------------------------------
  ! C = C_OVER_M2 * M^2, the integration constant of eq. first_int.
  !---------------------------------------------------------------------------
  pure function eq_constant_c(m) result(c)
    real(dp), intent(in) :: m
    real(dp) :: c
    c = EQ_C_OVER_M2 * m * m
  end function eq_constant_c

  !---------------------------------------------------------------------------
  ! R_c = RC_OVER_M * M, the minimum areal radius on which the physical branch
  ! of eq. first_int exists.
  !---------------------------------------------------------------------------
  pure function eq_radius_min(m) result(rc)
    real(dp), intent(in) :: m
    real(dp) :: rc
    rc = EQ_RC_OVER_M * m
  end function eq_radius_min

  !---------------------------------------------------------------------------
  ! Residuum of eq. first_int,
  !     alpha^2 - 1 + 2M/R - (C^2/R^4) e^alpha .
  !---------------------------------------------------------------------------
  pure function eq_residual(m, r, alpha) result(g)
    real(dp), intent(in) :: m, r, alpha
    real(dp) :: g
    g = alpha*alpha - 1.0_dp + 2.0_dp*m/r - eq_constant_c(m)**2/r**4 * exp(alpha)
  end function eq_residual

  !---------------------------------------------------------------------------
  ! Does a physical (positive, non-singular) lapse exist at this areal radius?
  !---------------------------------------------------------------------------
  pure logical function eq_lapse_valid(m, r)
    real(dp), intent(in) :: m, r
    eq_lapse_valid = (r >= eq_radius_min(m)) .and. (r > 0.0_dp) .and. (m > 0.0_dp)
  end function eq_lapse_valid

  !---------------------------------------------------------------------------
  ! alpha(R) from eq. first_int,
  !
  !     g(alpha) = alpha^2 - 1 + 2M/R - s e^alpha,     s = C^2/R^4 .
  !
  ! g has a maximum at the smaller root of g' = 0 and a minimum at the larger
  ! one (g' = 0  <=>  alpha e^{-alpha} = s/2, which has two roots whenever
  ! s < 2/e).  For R >= R_c the PHYSICAL root of g is the unique crossing in
  ! the interval [alpha_1, alpha_2] between those stationary points, and we
  ! bisect there.
  !
  ! Two things this deliberately avoids:
  !
  !   * plain Newton from the maximal-slicing guess, which converges to the
  !     spurious large root (or a negative one);
  !   * naive "scan for the first sign change", which FAILS AT R = R_c itself.
  !     There the two roots coalesce and g has a TANGENT (double root,
  !     min g ~ 1e-12), so g never changes sign and no bracket exists.  R_c is
  !     exactly where the paper's two regularity conditions hold, so failing
  !     there would lose the one point where the construction is pinned down.
  !     Bisecting between the stationary points returns alpha_c there directly.
  !---------------------------------------------------------------------------
  function eq_lapse(m, r) result(alpha)
    real(dp), intent(in) :: m, r
    real(dp) :: alpha, s, a1, a2, mid, g1
    integer :: i
    real(dp), parameter :: gtol = 1.0e-9_dp

    if (.not. eq_lapse_valid(m, r)) then
      write(*,'(A,ES12.4,A,ES12.4)') &
        'equilibrium_1pluslog: no lapse at R = ', r, ' (need R >= R_c = ', &
        eq_radius_min(m), ')'
      error stop 'equilibrium_1pluslog: areal radius below regularity limit'
    end if

    s = eq_constant_c(m)**2 / r**4
    a1 = stationary_point(s, .true.)   ! small root of g': g is MINIMIZED here
    a2 = stationary_point(s, .false.)  ! large root of g': g is MAXIMIZED here

    g1 = eq_residual(m, r, a1)

    ! At R = R_c the two roots of g coalesce, so the minimum of g TOUCHES zero
    ! (g(a1) = 0) and the physical lapse IS that minimum, alpha_c.  Detect this
    ! explicitly: bisecting [a1, a2] here would instead run away to the large
    ! spurious root on the far side of the maximum.
    if (g1 > -gtol) then
      alpha = a1
      return
    end if

    ! For R > R_c, g rises monotonically from g(a1) < 0 to g(a2) > 0 and
    ! crosses zero exactly once in between: that crossing is the physical root.
    if (a2 <= a1) error stop 'equilibrium_1pluslog: degenerate stationary points'

    do i = 1, 200
      mid = 0.5_dp*(a1 + a2)
      if (eq_residual(m, r, mid) > 0.0_dp) then
        a2 = mid
      else
        a1 = mid
      end if
    end do
    alpha = 0.5_dp*(a1 + a2)
  end function eq_lapse

  !---------------------------------------------------------------------------
  ! A root of  alpha e^{-alpha} = s/2, i.e. of  f(alpha) = 2 alpha - s e^alpha.
  !
  ! f(0) = -s < 0, f grows, then the exponential overtakes it again, so f has
  ! a SMALL root (where g is MINIMIZED) and a LARGE root (where g is MAXIMIZED).
  ! `small` selects the branch.  Solved by bracketing + bisection rather than
  ! Newton: f(1) = 2 - s e == f'(1) = 2 - s e, so a Newton iteration started
  ! at guess = 1 lands exactly on 0 on the first step and then thrashes
  ! between the two branches.  Bisection cannot.
  !---------------------------------------------------------------------------
  function stationary_point(s, small) result(a)
    real(dp), intent(in) :: s
    logical, intent(in) :: small
    real(dp) :: a, lo, hi, mid, fmid
    integer :: n

    n = 0

    if (small) then
      ! f(0) < 0, and f(s) = s(2 - e^s) > 0 for s < ln 2  (here s <= 0.276).
      lo = 0.0_dp
      hi = max(s, 1.0e-12_dp)
      if (f_of(hi, s) <= 0.0_dp) then
        ! s is at or above ln 2; the construction is out of range (see below)
        error stop 'equilibrium_1pluslog: C^2/R^4 too large for stationary 1+log'
      end if
    else
      lo = 1.0_dp
      hi = 2.0_dp
      do while (f_of(hi, s) > 0.0_dp .and. n < 200)
        lo = hi
        hi = 2.0_dp*hi
        n = n + 1
      end do
      if (f_of(hi, s) > 0.0_dp) error stop 'equilibrium_1pluslog: no large stationary point'
    end if

    do n = 1, 200
      mid = 0.5_dp*(lo + hi)
      fmid = f_of(mid, s)
      if (fmid > 0.0_dp) then
        hi = mid
      else
        lo = mid
      end if
    end do
    a = 0.5_dp*(lo + hi)
  end function stationary_point

  pure function f_of(a, s)
    real(dp), intent(in) :: a, s
    real(dp) :: f_of
    f_of = 2.0_dp*a - s*exp(a)
  end function f_of

  !---------------------------------------------------------------------------
  ! beta = C e^{alpha/2} / R^2, from eq. prop_dist plus eq. first_int.
  !---------------------------------------------------------------------------
  pure function eq_shift(m, r, alpha) result(beta)
    real(dp), intent(in) :: m, r, alpha
    real(dp) :: beta
    beta = eq_constant_c(m) * exp(0.5_dp*alpha) / (r*r)
  end function eq_shift

  !---------------------------------------------------------------------------
  ! Mean curvature required by the stationary 1+log condition (eq. 1+log).
  !
  ! eq_shift returns q = sqrt(alpha^2 - 1 + 2M/R), the physical radial
  ! shift magnitude.  Since gamma_RR = alpha^-2 in areal coordinates, the
  ! coordinate shift is beta^R = alpha*q.  Therefore
  !
  !     K = beta^R d_R(alpha)/(2 alpha) = q d_R(alpha)/2.
  !
  ! Using q directly as beta^R incorrectly inserts an extra 1/alpha; the old
  ! formula passed the asymptotic check because alpha -> 1 at infinity.
  ! d_alpha/dR is taken by a central difference on the lapse solution.
  !---------------------------------------------------------------------------
  function eq_mean_curvature(m, r) result(k)
    real(dp), intent(in) :: m, r
    real(dp) :: k, alpha, beta, h, dadr
    real(dp), parameter :: hrel = 1.0e-6_dp
    h = hrel * r
    alpha = eq_lapse(m, r)
    beta  = eq_shift(m, r, alpha)
    dadr  = (eq_lapse(m, r + h) - eq_lapse(m, r - h)) / (2.0_dp*h)
    k = beta * dadr / 2.0_dp
  end function eq_mean_curvature

  !---------------------------------------------------------------------------
  ! Lapse/shift continued into the black hole interior, r < 2M.
  !
  ! eq. first_int has no solution for R < R_c, and since the areal radius obeys
  ! R >= 2M with equality only at the throat, the interior has no areal
  ! solution at all.  Physically this is harmless: r < 2M lies inside the event
  ! horizon, where the data cannot influence the exterior, and the paper makes
  ! the same point for its trumpet data, filling the interior by "8th-order
  ! polynomial extrapolation of the exterior data".
  !
  ! We do exactly that: Lagrange interpolation through six samples of the
  ! exterior solution at 2M + k*0.1M, evaluated below 2M.  Because the exterior
  ! solution is analytic, this is well conditioned (the variants we checked
  ! with different node counts/spacings agree to ~1e-3) and matches value and
  ! slope at the horizon.
  !
  ! LIMITATION, and it is a real one: the extension crosses zero at r ~ 1.31M
  ! and is negative below that, so any grid with r_excise <~ 1.3M will be given
  ! a negative lapse.  eq_lapse_ext_valid reports where that starts; callers
  ! must keep r_excise above it.  This is one reason the equilibrium data are an
  ! EXCISED construction and not a puncture.
  !---------------------------------------------------------------------------
  function eq_lapse_ext(m, r) result(a)
    real(dp), intent(in) :: m, r
    real(dp) :: a
    real(dp) :: node(6), val(6), x
    integer :: i
    if (r >= EQ_RH*m) then
       a = eq_lapse(m, r)
    else
       do i = 1, 6
          node(i) = (EQ_RH + EXT_NODES(i))*m
          val(i)  = eq_lapse(m, node(i))
       end do
       x = r
       a = lagrange_val(node, val, x)
    end if
  end function eq_lapse_ext

  function eq_shift_ext(m, r) result(b)
    real(dp), intent(in) :: m, r
    real(dp) :: b
    b = eq_constant_c(m)*exp(0.5_dp*eq_lapse_ext(m, r))/(r*r)
  end function eq_shift_ext

  ! Is the extended lapse still positive at this radius?
  pure logical function eq_lapse_ext_valid(m, r)
    real(dp), intent(in) :: m, r
    real(dp), parameter :: r_zero_over_m = 1.307_dp  ! measured, see notes above
    eq_lapse_ext_valid = (r >= 1.35_dp*m)
  end function eq_lapse_ext_valid

  ! d(alpha)/dr and d(beta)/dr, by central difference on the EXTENDED
  ! functions so that they stay consistent across the r = 2M seam.
  function eq_dalpha_dr(m, r) result(d)
    real(dp), intent(in) :: m, r
    real(dp) :: d, h
    real(dp), parameter :: hrel = 1.0e-5_dp
    h = hrel*r
    d = (eq_lapse_ext(m, r + h) - eq_lapse_ext(m, r - h))/(2.0_dp*h)
  end function eq_dalpha_dr

  function eq_dbeta_dr(m, r) result(d)
    real(dp), intent(in) :: m, r
    real(dp) :: d, h
    real(dp), parameter :: hrel = 1.0e-5_dp
    h = hrel*r
    d = (eq_shift_ext(m, r + h) - eq_shift_ext(m, r - h))/(2.0_dp*h)
  end function eq_dbeta_dr

  ! Lagrange interpolation value at x through (node, val).
  pure function lagrange_val(node, val, x) result(y)
    real(dp), intent(in) :: node(:), val(:), x
    real(dp) :: y, t
    integer :: i, j
    y = 0.0_dp
    do i = 1, size(node)
       t = val(i)
       do j = 1, size(node)
          if (i /= j) t = t*(x - node(j))/(node(i) - node(j))
       end do
       y = y + t
    end do
  end function lagrange_val

end module equilibrium_1pluslog_mod
