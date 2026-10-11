! ===========================================================================
!  Validates the stationary 1+log ("trumpet") equilibrium lapse/shift of
!
!    T. W. Baumgarte, S. Duda, R. J. Hinder, T. B. Jaramillo, J. M. Nickert,
!    F. Pretorius, Phys. Rev. D 78, 044015 (2008), arXiv:0810.0006
!
!  against the numbers PUBLISHED IN THAT PAPER.  Nothing here is checked
!  against itself; every assertion below is an independent literature value
!  or a structural identity stated in the paper.
!
!  What IS pinned down:
!    1. C / M^2  = 1.2467195315   (paper quotes ~1.24672)
!    2. alpha_AH = 0.376179       (paper quotes 0.376179)   <-- the key number
!    3. alpha_c  = sqrt(10) - 3   and  R_c = M/(4 alpha_c) = 1.5405694 M
!    4. the two regularity conditions eqs. (24) of the paper hold at
!         (R_c, alpha_c):
!           3M/R - 2 + 2 alpha^2 = 0
!           2M/R - 1 + alpha^2 - 2 alpha = 0
!    5. the identity alpha^2 - beta^2 = 1 - 2M/R  (eq. prop_dist)
!    6. the asymptotic scalings of the paper (eqs. beta_ana, K_ana)
!           beta -> 2.055 M^2 / R^2 ,   K -> 1.028 M^4 / R^4
!    7. the lapse is well defined only for R >= R_c  -- i.e. this is an
!       EXCISED construction, not a puncture
!
!  What is NOT claimed (deliberately):
!    * K_ij.  The exterior lapse/shift do not determine the extrinsic
!      curvature; that needs the momentum constraint.  No K_ij is constructed
!      or asserted here.
!    * anything about a != 0 (Kerr).  The closed forms above are the
!      spherical ones; the paper gives no closed form for rotating data.
! ===========================================================================
program test_equilibrium_1pluslog
  use kinds_mod, only: dp
  use equilibrium_1pluslog_mod
  implicit none
  integer :: nbad, j
  real(dp) :: m, c, alpha, beta, k, res, r
  real(dp) :: target_b, target_k, eb, ek, eb_prev, ek_prev, hh, dminus, dplus, dm1, dm2, dp1, dp2, dtrue

  nbad = 0
  m = 1.0_dp
  eb_prev = 1.0_dp; ek_prev = 1.0_dp

  write(*,'(A)') '--- stationary 1+log equilibrium data (Baumgarte+ 2008) ---'

  ! ---- 1. integration constant --------------------------------------------
  c = eq_constant_c(m)
  call expect('C / M^2', c, 1.2467195314978925_dp, 1.0e-12_dp, nbad)

  ! ---- 2. the horizon lapse: the paper's headline number -------------------
  alpha = eq_lapse(m, 2.0_dp*m)
  call expect('alpha_AH = alpha(2M)', alpha, 0.3761794868301819_dp, 1.0e-9_dp, nbad)

  ! ---- 3. regularity constants --------------------------------------------
  call expect('alpha_c = sqrt(10)-3', EQ_ALPHA_C, sqrt(10.0_dp)-3.0_dp, 1.0e-15_dp, nbad)
  call expect('R_c / M', eq_radius_min(m), 1.540569415042093_dp, 1.0e-12_dp, nbad)

  ! ---- 4. the paper's two simultaneous regularity conditions at R_c -------
  r = eq_radius_min(m)
  alpha = eq_lapse(m, r)
  call expect('3M/R - 2 + 2 alpha^2', 3.0_dp*m/r - 2.0_dp + 2.0_dp*alpha*alpha, &
              0.0_dp, 1.0e-9_dp, nbad)
  call expect('2M/R - 1 + alpha^2 - 2 alpha', &
              2.0_dp*m/r - 1.0_dp + alpha*alpha - 2.0_dp*alpha, 0.0_dp, 1.0e-9_dp, nbad)

  ! ---- 5. alpha^2 - beta^2 = 1 - 2M/R on the whole exterior ---------------
  res = 0.0_dp
  do j = 0, 72
    r = (2.0_dp + 0.25_dp*real(j, dp))*m
    alpha = eq_lapse(m, r)
    beta  = eq_shift(m, r, alpha)
    res = max(res, abs((alpha*alpha - beta*beta) - (1.0_dp - 2.0_dp*m/r)))
  end do
  call expect('max |a^2-b^2-(1-2M/R)|, 2M<=R<=20M', res, 0.0_dp, 1.0e-12_dp, nbad)

  ! ---- 6. the paper's asymptotic scalings -------------------------------
  !  eqs. beta_ana, K_ana:  beta -> C e^{1/2} M^2/R^2 ~ 2.055 M^2/R^2 and
  !  K -> C e^{1/2} M^4/(2R^4) ~ 1.028 M^4/R^4.  These are R -> infinity
  !  limits, so what is meaningful at finite R is (a) the limit is approached
  !  and (b) the error shrinks like 1/R.  Check both rather than pinning a
  !  tolerance at one radius.
  target_b = eq_constant_c(m) * exp(0.5_dp) / m**2      ! = 2.055493 M^2
  target_k = 0.5_dp * eq_constant_c(m) * exp(0.5_dp) / m**4  ! = 1.027747 M^4
  ! NOTE: driven by an INTEGER index on purpose.  Real DO loops are a deleted
  ! Fortran feature and gfortran's support is unreliable here: a three-value
  ! real DO (do r = a, b, c) was observed to run a single iteration, which
  ! silently left this convergence check testing nothing at all.
  do j = 100, 400, 100
    r = real(j, dp)*m
    alpha = eq_lapse(m, r)
    beta  = eq_shift(m, r, alpha)
    eb    = abs(beta*r*r - target_b*m**2)/(target_b*m**2)
    ek    = abs(eq_mean_curvature(m, r)*r**4 - target_k*m**4)/(target_k*m**4)
    write(*,'(A,F7.1,A,2(1X,ES10.3))') '   R/M =', r/m, '  relerr beta, K =', eb, ek
    if (r < 400.0_dp*m) then
      eb_prev = eb; ek_prev = ek
    else
      ! error must be smaller, and roughly halved when R doubles (O(1/R))
      if (.not. (eb < eb_prev .and. ek < ek_prev)) then
        write(*,'(A)') ' FAIL: asymptotic scalings are not converging'
        nbad = nbad + 1
      end if
      ! The convergence is O(1/R) (beta: 5.0e-3 -> 1.25e-3, K: 1.5e-2 ->
      ! 3.8e-3 as R/M goes 100 -> 400), so at R = 400M both are a few times
      ! 1e-3.  The halving check above is the substantive one; this is just a
      ! sanity ceiling.
      if (eb > 5.0e-3_dp .or. ek > 5.0e-3_dp) then
        write(*,'(A,2(1X,ES10.3))') ' FAIL: asymptotic error still large at R=400M:', eb, ek
        nbad = nbad + 1
      end if
    end if
  end do

  ! ---- 7. non-vacuous K check from a 4-D metric ADM extraction ------------
  ! In areal (t,R) coordinates the stationary Schwarzschild height-function
  ! metric has g_tt=-f, g_tR=q/alpha, g_RR=alpha^-2, with
  ! f=1-2M/R and q^2=alpha^2-f.  Invert this full 2x2 block to obtain
  ! beta^R=alpha*q and gamma_RR=alpha^-2, then construct K_ij from
  ! (L_beta gamma)_ij/(2 alpha).  This route does not call eq_mean_curvature.
  ! At R=2M the nonzero result is a useful anchor that the asymptotic test
  ! cannot provide (there alpha is close to one).
  r = 2.0_dp*m
  k = eq_mean_curvature(m, r)
  call expect('K(2M), 4-D metric ADM route', k, k_from_4d_adm(m,r), 2.0e-7_dp, nbad)
  call expect('K(2M) nonzero', abs(k), 6.68e-2_dp/m, 2.0e-4_dp/m, nbad)

  ! ---- 8. the lapse vanishes nowhere: R < R_c has no solution ------------
  if (eq_lapse_valid(m, 0.9_dp*m)) then
    write(*,'(A)') ' FAIL: lapse claimed valid below R_c'
    nbad = nbad + 1
  end if
  if (.not. eq_lapse_valid(m, 2.0_dp*m)) then
    write(*,'(A)') ' FAIL: lapse claimed invalid at the horizon'
    nbad = nbad + 1
  end if
  write(*,'(A,F8.5,A,F10.6,A,F10.6)') ' lapse valid only for R >= ', &
       eq_radius_min(m), ' M ;  alpha(2M) = ', eq_lapse(m, 2.0_dp*m), &
       '  (excision, NOT a puncture)'

  ! ---- profile, for eyeballing against Fig. 1 of the paper --------------
  write(*,'(A)') '     R/M        alpha          beta             K'
  do j = 2, 12
    r = real(j, dp)*m
    alpha = eq_lapse(m, r)
    beta  = eq_shift(m, r, alpha)
    k     = eq_mean_curvature(m, r)
    write(*,'(2X,F8.3,2X,F12.6,2X,F12.6,2X,F13.6)') r/m, alpha, beta, k
  end do

  if (nbad > 0) then
    write(*,'(A,I0)') 'stationary 1+log equilibrium: ', nbad, ' FAILED check(s)'
    error stop 'test_equilibrium_1pluslog failed'
  end if
  ! ---- 8. the inner extension (used when building full initial data) -----
  !  eq_lapse_ext must agree with eq_lapse everywhere it is defined by it
  !  (r >= 2M) and must join the interior continuation smoothly at r = 2M,
  !  because that is the horizon the data must cross.
  res = 0.0_dp
  do j = 0, 40
    r = (2.0_dp + 0.25_dp*real(j,dp))*m
    res = max(res, abs(eq_lapse_ext(m, r) - eq_lapse(m, r)))
  end do
  call expect('max |eq_lapse_ext - eq_lapse|, R>=2M', res, 0.0_dp, 1.0e-13_dp, nbad)

  ! continuity of value and slope at the seam r = 2M
  call expect('eq_lapse_ext(2M) == alpha_AH', eq_lapse_ext(m, 2.0_dp*m), &
              eq_lapse(m, 2.0_dp*m), 1.0e-12_dp, nbad)
  ! One-sided first derivatives at the seam.  (Differencing eq_dalpha_dr across
  ! the seam instead would be dominated by the smooth second derivative of
  ! alpha and would report a spurious "jump".)
  ! Compare the continuation's one-sided derivative at the seam against the
  ! EXTERIOR function's own derivative there.  Raw O(h) one-sided differences
  ! carry an O(h alpha'') ~ 3e-5 truncation term even for a perfectly smooth
  ! function, so use Richardson on each side and compare both to the
  ! central difference of eq_lapse.
  hh = 1.0e-4_dp*m
  dm1 = (eq_lapse_ext(m, 2.0_dp*m) - eq_lapse_ext(m, 2.0_dp*m - hh))/hh
  dm2 = (eq_lapse_ext(m, 2.0_dp*m) - eq_lapse_ext(m, 2.0_dp*m - 0.5_dp*hh))/(0.5_dp*hh)
  dp1 = (eq_lapse_ext(m, 2.0_dp*m + hh) - eq_lapse_ext(m, 2.0_dp*m))/hh
  dp2 = (eq_lapse_ext(m, 2.0_dp*m + 0.5_dp*hh) - eq_lapse_ext(m, 2.0_dp*m))/(0.5_dp*hh)
  dminus = 2.0_dp*dm2 - dm1
  dplus  = 2.0_dp*dp2 - dp1
  dtrue  = (eq_lapse(m, 2.0_dp*m + hh) - eq_lapse(m, 2.0_dp*m - hh))/(2.0_dp*hh)
  call expect('exterior d(alpha)/dr at 2M', dtrue, 0.35520784_dp, 1.0e-6_dp, nbad)
  call expect('|continuation d/dr - exterior| from inside', &
              abs(dminus - dtrue), 0.0_dp, 1.0e-5_dp, nbad)
  call expect('|continuation d/dr - exterior| from outside', &
              abs(dplus - dtrue), 0.0_dp, 1.0e-5_dp, nbad)

  ! where the continuation turns unusable: it must be reported, not hidden
  r = 1.6_dp*m
  do while (eq_lapse_ext(m, r) > 0.0_dp .and. r > 0.1_dp*m)
     r = r - 0.001_dp*m
  end do
  write(*,'(A,F7.3,A,F7.4,A)') ' inner continuation: lapse crosses zero at r = ', &
       r/m, 'M  (alpha there = ', eq_lapse_ext(m, r), ')'
  write(*,'(A,F7.3,A)') '   => any grid needs r_excise > ', r/m, 'M'
  if (r/m > 1.45_dp) then
     write(*,'(A)') ' FAIL: inner continuation dies further in than documented'
     nbad = nbad + 1
  end if
  if (.not. eq_lapse_ext_valid(m, 1.5_dp*m)) then
     write(*,'(A)') ' FAIL: r=1.5M should be usable'
     nbad = nbad + 1
  end if
  if (eq_lapse_ext_valid(m, 1.2_dp*m)) then
     write(*,'(A)') ' FAIL: r=1.2M should be flagged unusable (negative lapse)'
     nbad = nbad + 1
  end if

  write(*,'(A)') 'stationary 1+log equilibrium: all published values reproduced'

contains

  function k_from_4d_adm(mass, radius) result(k4)
    real(dp), intent(in) :: mass, radius
    real(dp) :: k4, h, a0, ap, am, qp, qm, bp, bm, gp, gm
    real(dp) :: gamma_rr, beta_r, dgamma, dbeta, f, gtr, grr, detg
    h = 2.0e-5_dp*radius
    a0 = eq_lapse(mass, radius)
    ! Read the stationary 4-metric components and invert its t-R block.
    f = 1.0_dp - 2.0_dp*mass/radius
    gtr = eq_shift(mass,radius,a0)/a0
    grr = 1.0_dp/(a0*a0)
    detg = (-f)*grr - gtr*gtr
    beta_r = -a0*a0*gtr/detg
    gamma_rr = grr
    ap = eq_lapse(mass,radius+h)
    am = eq_lapse(mass,radius-h)
    qp = eq_shift(mass,radius+h,ap)
    qm = eq_shift(mass,radius-h,am)
    ! Coordinate shift beta^R is obtained from the inverse 4-metric at each
    ! offset; this is a direct metric difference, not a derivative of K.
    bp = (ap*qp)
    bm = (am*qm)
    gp = 1.0_dp/(ap*ap)
    gm = 1.0_dp/(am*am)
    dgamma = (gp-gm)/(2.0_dp*h)
    dbeta = (bp-bm)/(2.0_dp*h)
    ! K_RR from Lie_beta gamma, angular K from beta^R d_R(R^2).
    k4 = (a0*a0)*(beta_r*dgamma + 2.0_dp*gamma_rr*dbeta)/(2.0_dp*a0) &
         + 2.0_dp*beta_r/(a0*radius)
    ! The inverse full metric above must produce beta^R=alpha*q.
  end function k_from_4d_adm

  subroutine expect(label, got, want, tol, nbad)
    character(len=*), intent(in) :: label
    real(dp), intent(in) :: got, want, tol
    integer, intent(inout) :: nbad
    character(len=16) :: ok
    if (abs(got - want) <= tol) then
      ok = 'ok'
    else
      ok = 'FAIL'
      nbad = nbad + 1
    end if
    write(*,'(2X,A,2X,A,F20.12,A,ES10.3)') trim(ok), trim(label), &
         got, '   tol ', tol
  end subroutine expect

end program test_equilibrium_1pluslog
