#!/usr/bin/env bash
#
# install_warpdrive_core.sh
# =========================
# Writes arrangements/WarpDrive/{WarpBase,WarpInitial,WarpTmunu} into an
# Einstein Toolkit checkout.
#
#   usage:  ./install_warpdrive_core.sh /path/to/Cactus
#
# Provenance discipline (see WarpDrive_Formulation.md Sec. 0):
#   [PAPER-ABSTRACT]  verified in the abstract of arXiv:2605.03653
#   [PAPER-SECONDARY] from a machine-generated summary -- NOT yet PDF-verified
#   [DERIVED]/[DERIVED-CAS]  derived here, SymPy-verified
#   [IMPL]            implementation choice, not from the paper
#   [LEGACY]          Alcubierre 1994 / Natario 2002, not the target paper
#
# NOTHING in these three thorns depends on [PAPER-SECONDARY] content.
#
set -euo pipefail

CACTUS="${1:-}"
[[ -n "$CACTUS" && -d "$CACTUS" ]] || { echo "usage: $0 /path/to/Cactus" >&2; exit 2; }
[[ -d "$CACTUS/arrangements" ]] || { echo "error: $CACTUS/arrangements not found" >&2; exit 2; }

ARR="$CACTUS/arrangements/WarpDrive"
mkdir -p "$ARR"/{WarpBase,WarpInitial,WarpTmunu}/src
echo "installing into $ARR"

# =============================================================================
# =============================  W A R P B A S E  =============================
# =============================================================================
# WarpBase is the SINGLE SOURCE OF TRUTH.  It owns V(t,x,y,z), every derivative
# of V that any closed form needs, the kinematic scalars, and the ANALYTIC
# matter variables.  No other thorn may re-derive these.

cat > "$ARR/WarpBase/interface.ccl" <<'EOF'
# interface.ccl -- WarpBase
# Owns the warp velocity field V and everything analytically derivable from it.

implements: WarpBase
inherits: grid

USES INCLUDE HEADER: cctk_Functions.h

public:

# ---------------------------------------------------------------- the field
# Model [DERIVED, one-component subcase of the Natario class under R1+R2+R3]:
#     ds^2 = -dt^2 + (dx - V dt)^2 + dy^2 + dz^2
#     alpha = 1,  beta^i = (-V,0,0),  gamma_ij = delta_ij
CCTK_REAL warp_v_group TYPE=GF TIMELEVELS=1 \
  TAGS='tensortypealias="scalar" prolongation="none" checkpoint="no"'
{
  warp_V
} "Coordinate velocity V(t,x,y,z).  beta^i = (-V,0,0)."

# ------------------------------------------------------- first derivatives
CCTK_REAL warp_dv1_group TYPE=GF TIMELEVELS=1 \
  TAGS='prolongation="none" checkpoint="no"'
{
  warp_dVt, warp_dVx, warp_dVy, warp_dVz
} "First derivatives of V"

# ------------------------------------------------------ second derivatives
# The COMPLETE symmetric set d_mu d_nu V.  With V and these ten numbers the
# entire 4-D Riemann tensor is exactly determined, because g_mu_nu is
# algebraic in V:  g_tt = -1+V^2, g_tx = -V, g_ij = delta_ij.
# This is why WarpAnalysis needs no finite differencing in analytic mode.
CCTK_REAL warp_dv2_group TYPE=GF TIMELEVELS=1 \
  TAGS='prolongation="none" checkpoint="no"'
{
  warp_dVtt, warp_dVtx, warp_dVty, warp_dVtz,
  warp_dVxx, warp_dVxy, warp_dVxz, warp_dVyy, warp_dVyz, warp_dVzz
} "Second derivatives of V (complete symmetric 4-D set)"

# ----------------------------------------------------- kinematic scalars
CCTK_REAL warp_kin_group TYPE=GF TIMELEVELS=1 \
  TAGS='prolongation="none" checkpoint="no"'
{
  warp_accel,                            # A = d_t V + V d_x V   (coordinate acceleration)
  warp_dAx, warp_dAy, warp_dAz,          # its gradient
  warp_omega2,                           # Omega^2 = (1/4)[(d_y V)^2 + (d_z V)^2]
  warp_theta,                            # theta = d_x V
  warp_rs                                # r_s = |x - x_s(t)|
} "Coordinate kinematics.  A appears ONLY in S_ij and R^(4) -- never in rho, S_i, K_ij."

# -------------------------------------------------- ANALYTIC matter [DERIVED-CAS]
# Corrected Milestone-2 table.  Two Phase-0 forms were WRONG and are NOT here:
#   E1  R = 0        was the 4-D scalar mislabelled; R^(4) = 2 d_x A + 2 Omega^2
#   E2  S = -rho     FALSE; S = 3 rho - d_x A/(4 pi)
CCTK_REAL warp_matter_group TYPE=GF TIMELEVELS=1 \
  TAGS='prolongation="none" checkpoint="no"'
{
  warp_rho,
  warp_Sx, warp_Sy, warp_Sz,
  warp_Sxx, warp_Sxy, warp_Sxz, warp_Syy, warp_Syz, warp_Szz,
  warp_Strace,
  warp_Ricci4
} "Analytic 3+1 matter variables and 4-D Ricci scalar [DERIVED-CAS]"
EOF

cat > "$ARR/WarpBase/param.ccl" <<'EOF'
# param.ccl -- WarpBase

restricted:

# ---------------------------------------------------------------- profile
KEYWORD profile_type "Shape function for the warp velocity profile" STEERABLE=never
{
  "alcubierre1994" :: "[LEGACY] Alcubierre (1994) tanh top-hat. NOT 'the paper's metric'."
  "gaussian"       :: "[IMPL] C-infinity Gaussian core, exp(-r^2/2R^2). wall_sigma unused."
  "zero"           :: "V == 0 identically.  Minkowski.  Tests T1/T2."
} "alcubierre1994"

CCTK_REAL warp_velocity "v_s, bubble coordinate velocity, units of c (G=c=1)" STEERABLE=never
{
  *:* :: "any real.  |v_s|>1 is permitted: this is a COORDINATE velocity."
} 0.0

CCTK_REAL bubble_radius "R, bubble radius in code units" STEERABLE=never
{
  (0:* :: "strictly positive"
} 2.0

CCTK_REAL wall_sigma "sigma, wall steepness (inverse length)" STEERABLE=never
{
  (0:* :: "strictly positive"
} 3.0

CCTK_REAL wall_thickness "if > 0, overrides wall_sigma with sigma = 1/wall_thickness" STEERABLE=never
{
  *:* :: "negative disables"
} -1.0

# NOTE on the bubble centre.  Milestone 2 recommended a half-grid-spacing
# offset because h = f'/r and q = h'/r were 0/0 at r_s = 0.  That is NO LONGER
# NECESSARY: warp_profile.F90 evaluates h and q in exactly regular form.  The
# default is therefore an honest 0, and origin_treatment="naive" exists purely
# so the old failure can be reproduced as a regression test.
CCTK_REAL bubble_center_x "x_s at t = initial_time" STEERABLE=never { *:* :: "" } 0.0
CCTK_REAL bubble_center_y "y_s at t = initial_time" STEERABLE=never { *:* :: "" } 0.0
CCTK_REAL bubble_center_z "z_s at t = initial_time" STEERABLE=never { *:* :: "" } 0.0
CCTK_REAL initial_time    "t_0"                     STEERABLE=never { *:* :: "" } 0.0

BOOLEAN translate_bubble "Rigid translation x_s(t) = x_s(t_0) + v_s (t-t_0).  This is paper Example 1 [PAPER-ABSTRACT: 'solution form assumed a priori']." STEERABLE=never
{
} "yes"

# ------------------------------------------------------------- conventions
# T0 IS BLOCKING.  Run verify_sign_convention.sh against your checkout.
# Do not guess.  A wrong sign gives an evolution that looks plausible and
# runs backwards in time.
KEYWORD k_sign_convention "Extrinsic-curvature sign convention of your ADMBase" STEERABLE=never
{
  "mtw_admbase" :: "K_ij = -(1/2a)(d_t g_ij - Lie_b g_ij).  K<0 expanding.  Toolkit default."
  "opposite"    :: "Opposite sign.  Only if T0 says so."
} "mtw_admbase"

# ------------------------------------------------------------- numerics
KEYWORD derivative_method "How derivatives of V are obtained" STEERABLE=never
{
  "analytic"          :: "[IMPL] exact chain rule.  Production setting."
  "finite_difference" :: "[IMPL] FD of warp_V.  Convergence instrument for T5/T6 ONLY."
} "analytic"

INT fd_order "Centred FD order when derivative_method = finite_difference"
{
  2:8:2 :: "even orders"
} 4

KEYWORD origin_treatment "Evaluation of h = f'/r and q = h'/r at the bubble centre" STEERABLE=never
{
  "regularized" :: "[IMPL] exactly regular closed forms.  Validated to 1e-14 at 7.2e5 points."
  "naive"       :: "[IMPL] f'/r and (r f'' - f')/r^3 verbatim.  RETURNS NaN AT r_s=0. Regression test only."
} "regularized"

BOOLEAN check_finite "Abort if any WarpBase output is not IEEE-finite" STEERABLE=always
{
} "yes"

BOOLEAN verify_identities "Check the free algebraic identities every fill (S_xx=3rho, R4=8pi(rho-S), S_yy+S_zz=-d_xA/4pi)" STEERABLE=always
{
} "yes"

CCTK_REAL identity_tolerance "Absolute tolerance for verify_identities" STEERABLE=always
{
  (0:* :: "positive"
} 1.0e-11

# ------------------------------------------------------------- provenance
BOOLEAN strict_provenance "Refuse to activate any routine flagged [PAPER-SECONDARY]" STEERABLE=never
{
} "yes"

# ------------------------------------------------------------- units
# Internally G = c = 1 ALWAYS.  These are output metadata only; no routine
# multiplies by them.  See README Sec. 'Units'.
CCTK_REAL length_unit_meters "SI length of one code length unit" STEERABLE=never
{
  (0:* :: "positive"
} 1.0
EOF

cat > "$ARR/WarpBase/schedule.ccl" <<'EOF'
# schedule.ccl -- WarpBase

STORAGE: warp_v_group warp_dv1_group warp_dv2_group warp_kin_group warp_matter_group

schedule WarpBase_ParamCheck AT CCTK_PARAMCHECK
{
  LANG: Fortran
  OPTIONS: global
} "Validate WarpDrive parameters; report conventions and provenance mode"

# V is analytic, so it is filled EVERYWHERE including ghost and boundary
# zones.  No SYNC is required and none is requested.
schedule WarpBase_Fill AT CCTK_BASEGRID AFTER SpatialCoordinates
{
  LANG: Fortran
  READS:  grid::coordinates(everywhere)
  WRITES: warp_v_group(everywhere) warp_dv1_group(everywhere)
  WRITES: warp_dv2_group(everywhere) warp_kin_group(everywhere)
  WRITES: warp_matter_group(everywhere)
} "Evaluate V, its derivatives, kinematics and analytic matter (base grid)"

schedule WarpBase_Fill AT CCTK_INITIAL BEFORE ADMBase_InitialData
{
  LANG: Fortran
  READS:  grid::coordinates(everywhere)
  WRITES: warp_v_group(everywhere) warp_dv1_group(everywhere)
  WRITES: warp_dv2_group(everywhere) warp_kin_group(everywhere)
  WRITES: warp_matter_group(everywhere)
} "Evaluate V before ADMBase initial data is set"

# Keeps the ANALYTIC reference solution current during evolution.  In
# Experiment B the evolved geometry drifts away from this; that difference is
# the measurement.
schedule WarpBase_Fill AT CCTK_POSTSTEP
{
  LANG: Fortran
  READS:  grid::coordinates(everywhere)
  WRITES: warp_v_group(everywhere) warp_dv1_group(everywhere)
  WRITES: warp_dv2_group(everywhere) warp_kin_group(everywhere)
  WRITES: warp_matter_group(everywhere)
} "Refresh the analytic reference solution at the new time"

schedule WarpBase_Fill AT CCTK_POST_RECOVER_VARIABLES
{
  LANG: Fortran
} "Refresh the analytic reference solution after recovery"
EOF

cat > "$ARR/WarpBase/configuration.ccl" <<'EOF'
# configuration.ccl -- WarpBase
# WarpBase exports Fortran modules (warp_profile, warp_matter) that WarpTmunu
# and WarpAnalysis USE.  Declaring the capability fixes build order and puts
# the .mod files on the include path.
PROVIDES WarpBase
{
}
EOF

cat > "$ARR/WarpBase/src/make.code.defn" <<'EOF'
# make.code.defn -- WarpBase
# Listed in Fortran module dependency order.
SRCS = warp_profile.F90 warp_matter.F90 warp_parameters.F90 warp_field.F90
EOF

# ----------------------------------------------------------- warp_profile.F90
# This module is DEPENDENCY-FREE apart from the CCTK_REAL typedef.  Its
# algorithm was validated in float64 against an 80-digit mpmath reference at
# 720,090 points (sigma in [0.25,200], R in [0.1,50], r in [0,160]):
#   worst relative error   f: 1.6e-15   h: 6.9e-15   q: 1.5e-14
#   non-finite values:     0
cat > "$ARR/WarpBase/src/warp_profile.F90" <<'EOF'
#include "cctk.h"

! =============================================================================
! warp_profile -- shape functions and their REGULARIZED radial derivatives
! =============================================================================
!
! Every profile returns the triple (f, h, q) where
!
!     f = f(r_s)
!     h = f'(r_s)/r_s
!     q = h'(r_s)/r_s
!
! because the chain rule for V = v_s f(r_s), r_s = |x - x_s|, is
!
!     d_i V       = v_s h d_i
!     d_i d_j V   = v_s [ q d_i d_j + h delta_ij ],      d_i = x_i - x_s,i
!
! and that form contains no explicit 1/r_s.  It is nevertheless a TRAP: h and
! q are themselves 0/0 at r_s = 0.  Milestone 2 patched this with a Taylor
! branch.  This module does better -- h and q are written in exactly regular
! closed form and the Taylor branch is gone.
!
! ALCUBIERRE 1994 [LEGACY].  The textbook form
!
!     f = [tanh(sigma(r+R)) - tanh(sigma(r-R))] / (2 tanh(sigma R))
!
! subtracts two numbers both tending to 1.  In float64 at sigma=3,R=2 that
! costs 4% relative accuracy at r=5 and 100% at r=12 -- exactly where an outer
! boundary sits.  Using tanh a - tanh b = sinh(a-b)/(cosh a cosh b) with
! a-b = 2 sigma R, and sinh(2u) = 2 sinh u cosh u, the whole thing collapses to
!
!     f(r) = cosh^2(sigma R) / [ cosh(sigma(r+R)) cosh(sigma(r-R)) ]      (P1)
!
! which has no subtraction at all.  Differentiating (P1),
!
!     h(r) = -2 sigma^2 cosh^2(sigma R) sinhc(2 sigma r) sech^2 a sech^2 b (P2)
!
!     q(r) = -8 sigma^4 cosh^2(sigma R) sech^2 a sech^2 b
!                 [ g1(2 sigma r) - sinhc^2(2 sigma r)/(cosh a cosh b) ]  (P3)
!
! with sinhc(u) = sinh(u)/u and g1(u) = (u cosh u - sinh u)/u^3, both entire
! and both O(1) at u = 0 (sinhc -> 1, g1 -> 1/3).  Nothing diverges, nothing
! cancels catastrophically, and r_s = 0 is an ordinary point.
!
! Sanity limits, verified symbolically and numerically:
!     h(0) = -2 sigma^2 sech^2(sigma R)
!     q(0) = (8 sigma^4/3) sech^2(sigma R) [2 - 3 tanh^2(sigma R)]
! q(0) has a genuine zero at sigma R = 1.146216; near it only the ABSOLUTE
! error is meaningful (measured 4e-17).
!
! All exponentials are evaluated through log_cosh / log_sinhc / log_g1 so that
! large sigma*r underflows gracefully to zero instead of overflowing.  An
! earlier draft that formed sinhc(u)**2 directly raised OverflowError.
! =============================================================================

module warp_profile
  implicit none
  private

  public :: WARP_SHAPE_ZERO, WARP_SHAPE_ALCUBIERRE, WARP_SHAPE_GAUSSIAN
  public :: warp_shape_id, warp_shape_eval, warp_shape_origin_limits

  integer, parameter :: WARP_SHAPE_ZERO       = 0
  integer, parameter :: WARP_SHAPE_ALCUBIERRE = 1
  integer, parameter :: WARP_SHAPE_GAUSSIAN   = 2

  CCTK_REAL, parameter :: ZERO  = 0.0d0
  CCTK_REAL, parameter :: ONE   = 1.0d0
  CCTK_REAL, parameter :: TWO   = 2.0d0
  CCTK_REAL, parameter :: THREE = 3.0d0
  CCTK_REAL, parameter :: FOUR  = 4.0d0
  CCTK_REAL, parameter :: EIGHT = 8.0d0
  CCTK_REAL, parameter :: HALF  = 0.5d0
  CCTK_REAL, parameter :: THIRD = ONE/THREE
  CCTK_REAL, parameter :: LOG2  = 0.69314718055994530942d0

contains

  ! ---------------------------------------------------------------- helpers

  !> log cosh(u), overflow-free.
  elemental function log_cosh(u) result(res)
    CCTK_REAL, intent(in) :: u
    CCTK_REAL :: res, a
    a = abs(u)
    if (a > ONE) then
      res = a + log(ONE + exp(-TWO*a)) - LOG2
    else
      res = log(cosh(a))
    end if
  end function log_cosh

  !> log[ sinh(u)/u ] for u >= 0.  Regular at u = 0 (value 0).
  elemental function log_sinhc(u) result(res)
    CCTK_REAL, intent(in) :: u
    CCTK_REAL :: res, u2
    u2 = u*u
    if (u2 < 1.0d-8) then
      res = u2/6.0d0*(ONE - u2/60.0d0)                 ! log(1 + u^2/6 + ...)
    else
      res = u + log(ONE - exp(-TWO*u)) - LOG2 - log(u)
    end if
  end function log_sinhc

  !> log[ (u cosh u - sinh u)/u^3 ] for u >= 0.  Tends to log(1/3).
  !! Uses u cosh u - sinh u = cosh u (u - tanh u), which does not cancel.
  elemental function log_g1(u) result(res)
    CCTK_REAL, intent(in) :: u
    CCTK_REAL :: res, u2
    if (u < 1.0d-3) then
      u2 = u*u
      res = log(THIRD) + log(ONE + u2/10.0d0*(ONE + u2/28.0d0))
    else
      res = log_cosh(u) + log(u - tanh(u)) - THREE*log(u)
    end if
  end function log_g1

  ! ------------------------------------------------------------ dispatch

  !> Map the profile_type keyword onto an integer id once, outside the loop.
  function warp_shape_id(name) result(id)
    character(len=*), intent(in) :: name
    integer :: id
    if      (name == "zero")           then; id = WARP_SHAPE_ZERO
    else if (name == "alcubierre1994") then; id = WARP_SHAPE_ALCUBIERRE
    else if (name == "gaussian")       then; id = WARP_SHAPE_GAUSSIAN
    else;                                    id = -1
    end if
  end function warp_shape_id

  ! ------------------------------------------------------------ evaluation

  !> Evaluate (f, h, q) at radius r.
  !! @param regular  .true. -> regularized forms (P1)-(P3), production.
  !!                 .false. -> naive forms; RETURNS NaN AT r = 0 on purpose,
  !!                 so the Milestone-2 failure stays reproducible as a test.
  pure subroutine warp_shape_eval(id, r, sigma, radius, regular, f, h, q)
    integer,   intent(in)  :: id
    CCTK_REAL, intent(in)  :: r, sigma, radius
    logical,   intent(in)  :: regular
    CCTK_REAL, intent(out) :: f, h, q

    CCTK_REAL :: a, b, lca, lcb, lcR, u, l1, l2, m, lpre
    CCTK_REAL :: fp, fpp, ta, tb, sa, sb

    select case (id)

    case (WARP_SHAPE_ZERO)
      f = ZERO; h = ZERO; q = ZERO

    case (WARP_SHAPE_ALCUBIERRE)
      a   = sigma*(r + radius)
      b   = sigma*(r - radius)
      lca = log_cosh(a)
      lcb = log_cosh(b)
      lcR = log_cosh(sigma*radius)

      ! (P1) -- exact, no subtraction, valid for every r including 0.
      f = exp(TWO*lcR - lca - lcb)

      if (regular) then
        u = TWO*sigma*r
        ! (P2)
        h = -exp(log(TWO*sigma*sigma) + TWO*lcR + log_sinhc(u)          &
                 - TWO*lca - TWO*lcb)
        ! (P3), with the bracket evaluated by scaled exponentials so that
        ! neither term ever overflows.
        l1   = log_g1(u)
        l2   = TWO*log_sinhc(u) - lca - lcb
        m    = max(l1, l2)
        lpre = THREE*LOG2 + FOUR*log(sigma) + TWO*lcR                    &
               - TWO*lca - TWO*lcb + m
        q = -exp(lpre)*(exp(l1 - m) - exp(l2 - m))
      else
        ! Naive branch, retained ONLY as a regression target.
        ta = tanh(a); tb = tanh(b)
        sa = ONE/cosh(a)**2; sb = ONE/cosh(b)**2
        fp  = sigma*(sa - sb)/(TWO*tanh(sigma*radius))
        fpp = sigma*sigma*(sb*tb - sa*ta)/tanh(sigma*radius)
        h = fp/r
        q = fpp/r**2 - fp/r**3
      end if

    case (WARP_SHAPE_GAUSSIAN)
      ! [IMPL] C-infinity alternative.  f = exp(-r^2/2R^2).  Note that
      ! wall_sigma is IGNORED here: the wall scale IS the radius.  Every
      ! derivative is exactly regular with no special-casing whatsoever,
      ! which makes this the cleanest profile for convergence tests.
      f = exp(-HALF*(r/radius)**2)
      h = -f/radius**2
      q =  f/radius**4

    case default
      f = ZERO; h = ZERO; q = ZERO

    end select
  end subroutine warp_shape_eval

  !> Closed-form h(0), q(0).  Used by WarpBase_ParamCheck to prove at startup
  !! that the regularized evaluator reproduces the analytic origin limits.
  pure subroutine warp_shape_origin_limits(id, sigma, radius, h0, q0)
    integer,   intent(in)  :: id
    CCTK_REAL, intent(in)  :: sigma, radius
    CCTK_REAL, intent(out) :: h0, q0
    CCTK_REAL :: sech2, t2
    select case (id)
    case (WARP_SHAPE_ALCUBIERRE)
      sech2 = ONE/cosh(sigma*radius)**2
      t2    = tanh(sigma*radius)**2
      h0 = -TWO*sigma**2*sech2
      q0 = (EIGHT*sigma**4*THIRD)*sech2*(TWO - THREE*t2)
    case (WARP_SHAPE_GAUSSIAN)
      h0 = -ONE/radius**2
      q0 =  ONE/radius**4
    case default
      h0 = ZERO; q0 = ZERO
    end select
  end subroutine warp_shape_origin_limits

end module warp_profile
EOF

# ------------------------------------------------------------ warp_matter.F90
cat > "$ARR/WarpBase/src/warp_matter.F90" <<'EOF'
#include "cctk.h"

! =============================================================================
! warp_matter -- the corrected Milestone-2 matter table [DERIVED-CAS]
! =============================================================================
!
! THE single definition of rho, S_i, S_ij, S and R^(4).  WarpTmunu and
! WarpAnalysis both call into here.  Nothing is duplicated (brief Sec. 20).
!
! Model: one-component subcase of the Natario class under R1+R2+R3, arbitrary
! V(t,x,y,z).  Re-derived from g_mu_nu with SymPy for arbitrary V; 15/15
! assertions pass at zero residual; independently confirmed through the ADM
! Hamiltonian and momentum constraints without touching the 4-D Christoffels.
!
!   rho    = -Omega^2/(8 pi) = -[(d_yV)^2 + (d_zV)^2]/(32 pi)
!   S_x    = -(d_yy V + d_zz V)/(16 pi)
!   S_y    =  d_xy V/(16 pi)
!   S_z    =  d_xz V/(16 pi)
!   S_xx   =  3 rho
!   S_xy   = (d_y A + d_xV d_yV)/(16 pi)
!   S_xz   = (d_z A + d_xV d_zV)/(16 pi)
!   S_yy   = -d_x A/(8 pi) + [(d_yV)^2 - (d_zV)^2]/(32 pi)
!   S_zz   = -d_x A/(8 pi) - [(d_yV)^2 - (d_zV)^2]/(32 pi)
!   S_yz   =  d_yV d_zV/(16 pi)
!   S      =  3 rho - d_x A/(4 pi)
!   R^(4)  =  2 d_x A + 2 Omega^2
!
! TWO PHASE-0 FORMS WERE WRONG AND ARE DELIBERATELY ABSENT:
!   E1  "R = 0"     -- that is the SPATIAL scalar R^(3), which vanishes only
!                      because gamma_ij = delta_ij (restriction R3).  The 4-D
!                      scalar is nonzero.  They must never share a name.
!   E2  "S = -rho"  -- p = -rho is the equation of state of a cosmological
!                      constant.  Shipping it would have manufactured a
!                      spurious "the warp fluid is dark-energy-like" result
!                      that also appeared to corroborate the paper's retention
!                      of Lambda [PAPER-SECONDARY].  warp_matter_regression()
!                      asserts that both wrong forms STAY wrong.
!
! Physics that falls straight out:
!   * rho <= 0 everywhere, strictly negative wherever d_yV or d_zV != 0.  The
!     energy density is PURELY transverse-gradient driven; a purely
!     longitudinal profile has rho = 0 identically.
!   * A enters ONLY S_ij and R^(4).  It is absent from rho, S_i and K_ij.  So
!     coordinate acceleration [PAPER-ABSTRACT] is precisely a statement about
!     which anisotropic stresses must be supplied externally.
!   * Not a perfect fluid: S_xx = 3 rho while S_yy + S_zz = -d_xA/(4 pi), and
!     S_i != 0 means heat flux in the Eulerian frame.
! =============================================================================

module warp_matter
  implicit none
  private
  public :: warp_matter_point, warp_matter_identities, warp_matter_regression

  CCTK_REAL, parameter :: PI       = 3.14159265358979323846d0
  CCTK_REAL, parameter :: INV_8PI  = 1.0d0/( 8.0d0*PI)
  CCTK_REAL, parameter :: INV_16PI = 1.0d0/(16.0d0*PI)
  CCTK_REAL, parameter :: INV_32PI = 1.0d0/(32.0d0*PI)
  CCTK_REAL, parameter :: INV_4PI  = 1.0d0/( 4.0d0*PI)

contains

  pure subroutine warp_matter_point(dVx, dVy, dVz,                        &
                                    dVxx, dVxy, dVxz, dVyy, dVyz, dVzz,   &
                                    dAx, dAy, dAz,                        &
                                    rho, Sx, Sy, Sz,                      &
                                    Sxx, Sxy, Sxz, Syy, Syz, Szz,         &
                                    Str, R4)
    CCTK_REAL, intent(in)  :: dVx, dVy, dVz
    CCTK_REAL, intent(in)  :: dVxx, dVxy, dVxz, dVyy, dVyz, dVzz
    CCTK_REAL, intent(in)  :: dAx, dAy, dAz
    CCTK_REAL, intent(out) :: rho, Sx, Sy, Sz
    CCTK_REAL, intent(out) :: Sxx, Sxy, Sxz, Syy, Syz, Szz
    CCTK_REAL, intent(out) :: Str, R4

    CCTK_REAL :: dVy2, dVz2, omega2

    dVy2   = dVy*dVy
    dVz2   = dVz*dVz
    omega2 = 0.25d0*(dVy2 + dVz2)

    rho = -omega2*INV_8PI

    Sx  = -(dVyy + dVzz)*INV_16PI
    Sy  =   dVxy*INV_16PI
    Sz  =   dVxz*INV_16PI

    Sxx = 3.0d0*rho
    Sxy = (dAy + dVx*dVy)*INV_16PI
    Sxz = (dAz + dVx*dVz)*INV_16PI
    Syy = -dAx*INV_8PI + (dVy2 - dVz2)*INV_32PI
    Szz = -dAx*INV_8PI - (dVy2 - dVz2)*INV_32PI
    Syz =  dVy*dVz*INV_16PI

    Str = 3.0d0*rho - dAx*INV_4PI
    R4  = 2.0d0*dAx + 2.0d0*omega2
  end subroutine warp_matter_point

  !> The three free algebraic identities.  Any correct implementation
  !! satisfies all of them to machine precision, so they cost nothing and
  !! catch almost every plausible coding error.
  pure subroutine warp_matter_identities(rho, Sxx, Syy, Szz, Str, R4, dAx, &
                                         e_trace, e_sxx, e_ricci)
    CCTK_REAL, intent(in)  :: rho, Sxx, Syy, Szz, Str, R4, dAx
    CCTK_REAL, intent(out) :: e_trace, e_sxx, e_ricci
    e_trace = (Syy + Szz) + dAx*INV_4PI            ! S_yy + S_zz = -d_xA/(4pi)
    e_sxx   = Sxx - 3.0d0*rho                      ! S_xx = 3 rho
    e_ricci = R4 - 8.0d0*PI*(rho - Str)            ! R^(4) = 8pi(rho - S)
  end subroutine warp_matter_identities

  !> Guard against the two Phase-0 errors creeping back.  Returns .true. if
  !! the WRONG forms would be satisfied, i.e. if something regressed.
  pure function warp_matter_regression(rho, Str, R4, tol) result(bad)
    CCTK_REAL, intent(in) :: rho, Str, R4, tol
    logical :: bad
    bad = .false.
    ! E2: S = -rho must NOT hold (unless everything is trivially zero).
    if (abs(rho) > tol .and. abs(Str + rho) < tol) bad = .true.
    ! E1: R^(4) = 0 must NOT hold where the geometry is curved.
    if (abs(rho) > tol .and. abs(R4) < tol) bad = .true.
  end function warp_matter_regression

end module warp_matter
EOF

# -------------------------------------------------------- warp_parameters.F90
cat > "$ARR/WarpBase/src/warp_parameters.F90" <<'EOF'
#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

! =============================================================================
! WarpBase_ParamCheck -- fail loudly at CCTK_PARAMCHECK, never mid-evolution.
! =============================================================================
subroutine WarpBase_ParamCheck(CCTK_ARGUMENTS)
  use warp_profile
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  integer   :: id
  CCTK_REAL :: sig, h0, q0, hR, qR, fR, err
  character(len=512) :: msg

  id = warp_shape_id(profile_type)
  if (id < 0) then
    call CCTK_PARAMWARN("WarpBase::profile_type is not recognised")
  end if

  if (wall_thickness > 0.0d0) then
    sig = 1.0d0/wall_thickness
  else
    sig = wall_sigma
  end if
  if (sig <= 0.0d0) call CCTK_PARAMWARN("WarpBase: effective sigma must be > 0")
  if (bubble_radius <= 0.0d0) call CCTK_PARAMWARN("WarpBase: bubble_radius must be > 0")

  ! ------------------------------------------------- resolution sanity [IMPL]
  ! The wall must be resolved.  sigma is an inverse length, so the wall
  ! occupies roughly 1/sigma; fewer than ~8 points across it makes every
  ! convergence statement meaningless.
  if (CCTK_MyProc(cctkGH) == 0) then
    write(msg,'(A,ES12.5,A,ES12.5)')                                        &
      "WarpBase: effective sigma = ", sig, ", wall scale 1/sigma = ", 1.0d0/sig
    call CCTK_INFO(msg)
    write(msg,'(A,A,A,ES12.5)')                                             &
      "WarpBase: profile = ", trim(profile_type), ", v_s = ", warp_velocity
    call CCTK_INFO(msg)
    write(msg,'(A,A)') "WarpBase: k_sign_convention = ", trim(k_sign_convention)
    call CCTK_INFO(msg)
    call CCTK_INFO("WarpBase: T0 IS BLOCKING -- confirm the above against "// &
                   "your checkout with verify_sign_convention.sh")
    if (CCTK_EQUALS(profile_type, "alcubierre1994")) then
      call CCTK_INFO("WarpBase: profile_type=alcubierre1994 is [LEGACY] "//   &
                     "Alcubierre 1994.  It is paper Example 1, NOT 'the "//   &
                     "paper's metric'.")
    end if
    if (strict_provenance /= 0) then
      call CCTK_INFO("WarpBase: strict_provenance=yes -- [PAPER-SECONDARY] "//&
                     "routines are disabled.  No routine in WarpBase, "//     &
                     "WarpInitial or WarpTmunu needs them.")
    else
      call CCTK_WARN(CCTK_WARN_ALERT,                                         &
        "WarpBase: strict_provenance=no.  Unverified paper content may be "// &
        "active.  Do not report numbers from this run.")
    end if
  end if

  ! ------------------------------- prove the regularized evaluator at r_s = 0
  ! Cheap, and it converts "I believe the limits are right" into a startup
  ! assertion.  h(0) and q(0) have independent closed forms.
  if (CCTK_EQUALS(origin_treatment, "regularized")) then
    call warp_shape_eval(id, 0.0d0, sig, bubble_radius, .true., fR, hR, qR)
    call warp_shape_origin_limits(id, sig, bubble_radius, h0, q0)
    err = abs(hR - h0) + abs(qR - q0)
    if (err > 1.0d-8*(1.0d0 + abs(h0) + abs(q0))) then
      write(msg,'(A,ES12.5)') "WarpBase: origin-limit self-test FAILED, err = ", err
      call CCTK_PARAMWARN(msg)
    else if (CCTK_MyProc(cctkGH) == 0) then
      write(msg,'(A,ES9.2)')                                                  &
        "WarpBase: origin-limit self-test passed, residual = ", err
      call CCTK_INFO(msg)
    end if
  else
    call CCTK_WARN(CCTK_WARN_ALERT,                                           &
      "WarpBase: origin_treatment=naive will produce NaN at r_s=0.  "//       &
      "This setting exists only to reproduce the Milestone-2 failure.")
  end if
end subroutine WarpBase_ParamCheck
EOF

# ------------------------------------------------------------- warp_field.F90
cat > "$ARR/WarpBase/src/warp_field.F90" <<'EOF'
#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

! =============================================================================
! WarpBase_Fill -- evaluate V, all its derivatives, kinematics, matter.
! =============================================================================
!
! Chain rule for V = v_s f(r_s), r_s = |x - x_s(t)|, d_i = x_i - x_s,i  [DERIVED]:
!
!     d_i V     = v_s h d_i
!     d_i d_j V = v_s [ q d_i d_j + h delta_ij ]
!
! Time dependence enters ONLY through x_s(t).  For rigid translation
! (translate_bubble = yes, paper Example 1) x_s = x_s0 + v_s (t - t_0), so
! d(d_x)/dt = -v_s and therefore, exactly,
!
!     d_t V       = -v_s d_x V
!     d_t d_i V   = -v_s d_x d_i V
!     d_t d_t V   =  v_s^2 d_x d_x V
!
! Consequence worth noting: A = d_tV + V d_xV = (V - v_s) d_xV, so A vanishes
! at the bubble centre (V = v_s) and in the far field, but NOT in the wall.
! That is precisely what distinguishes Example 1 (accelerated) from Example 2
! (inertial, A = 0 everywhere).
! =============================================================================
subroutine WarpBase_Fill(CCTK_ARGUMENTS)
  use warp_profile
  use warp_matter
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  integer   :: i, j, k, id
  logical   :: rigid, regular
  CCTK_REAL :: sig, vs, xs, ys, zs, dxs, dys, dzs, rs, f, h, q
  CCTK_REAL :: Vl, Vt, Vx, Vy, Vz
  CCTK_REAL :: Vtt, Vtx, Vty, Vtz, Vxx, Vxy, Vxz, Vyy, Vyz, Vzz
  CCTK_REAL :: Al, Ax, Ay, Az
  CCTK_REAL :: rho, Sx, Sy, Sz, Sxx, Sxy, Sxz, Syy, Syz, Szz, Str, R4
  CCTK_REAL :: e1, e2, e3, worst
  integer   :: nbad
  character(len=256) :: msg

  id      = warp_shape_id(profile_type)
  regular = CCTK_EQUALS(origin_treatment, "regularized")
  rigid   = (translate_bubble /= 0)
  vs      = warp_velocity

  if (wall_thickness > 0.0d0) then
    sig = 1.0d0/wall_thickness
  else
    sig = wall_sigma
  end if

  ! Bubble centre at the current Cactus time.
  xs = bubble_center_x
  ys = bubble_center_y
  zs = bubble_center_z
  if (rigid) xs = xs + vs*(cctk_time - initial_time)

  nbad  = 0
  worst = 0.0d0

  !$OMP PARALLEL DO COLLAPSE(3) DEFAULT(NONE)                                &
  !$OMP   PRIVATE(i,j,k,dxs,dys,dzs,rs,f,h,q,Vl,Vt,Vx,Vy,Vz,                 &
  !$OMP           Vtt,Vtx,Vty,Vtz,Vxx,Vxy,Vxz,Vyy,Vyz,Vzz,Al,Ax,Ay,Az,       &
  !$OMP           rho,Sx,Sy,Sz,Sxx,Sxy,Sxz,Syy,Syz,Szz,Str,R4,e1,e2,e3)      &
  !$OMP   SHARED(cctk_lsh,x,y,z,xs,ys,zs,id,sig,vs,rigid,regular,            &
  !$OMP          bubble_radius,warp_V,warp_dVt,warp_dVx,warp_dVy,warp_dVz,   &
  !$OMP          warp_dVtt,warp_dVtx,warp_dVty,warp_dVtz,                    &
  !$OMP          warp_dVxx,warp_dVxy,warp_dVxz,warp_dVyy,warp_dVyz,warp_dVzz,&
  !$OMP          warp_accel,warp_dAx,warp_dAy,warp_dAz,                      &
  !$OMP          warp_omega2,warp_theta,warp_rs,                             &
  !$OMP          warp_rho,warp_Sx,warp_Sy,warp_Sz,                           &
  !$OMP          warp_Sxx,warp_Sxy,warp_Sxz,warp_Syy,warp_Syz,warp_Szz,      &
  !$OMP          warp_Strace,warp_Ricci4)                                    &
  !$OMP   REDUCTION(max:worst)
  do k = 1, cctk_lsh(3)
    do j = 1, cctk_lsh(2)
      do i = 1, cctk_lsh(1)

        dxs = x(i,j,k) - xs
        dys = y(i,j,k) - ys
        dzs = z(i,j,k) - zs
        rs  = sqrt(dxs*dxs + dys*dys + dzs*dzs)

        call warp_shape_eval(id, rs, sig, bubble_radius, regular, f, h, q)

        ! ------------------------------------------------- field and space
        Vl  = vs*f
        Vx  = vs*h*dxs
        Vy  = vs*h*dys
        Vz  = vs*h*dzs
        Vxx = vs*(q*dxs*dxs + h)
        Vyy = vs*(q*dys*dys + h)
        Vzz = vs*(q*dzs*dzs + h)
        Vxy = vs*q*dxs*dys
        Vxz = vs*q*dxs*dzs
        Vyz = vs*q*dys*dzs

        ! -------------------------------------------------- time (exact)
        if (rigid) then
          Vt  = -vs*Vx
          Vtx = -vs*Vxx
          Vty = -vs*Vxy
          Vtz = -vs*Vxz
          Vtt =  vs*vs*Vxx
        else
          Vt  = 0.0d0; Vtx = 0.0d0; Vty = 0.0d0; Vtz = 0.0d0; Vtt = 0.0d0
        end if

        ! ------------------------------------------------- kinematics
        Al = Vt  + Vl*Vx
        Ax = Vtx + Vx*Vx + Vl*Vxx
        Ay = Vty + Vy*Vx + Vl*Vxy
        Az = Vtz + Vz*Vx + Vl*Vxz

        ! -------------------------------------------------- store field
        warp_V(i,j,k)    = Vl
        warp_dVt(i,j,k)  = Vt
        warp_dVx(i,j,k)  = Vx
        warp_dVy(i,j,k)  = Vy
        warp_dVz(i,j,k)  = Vz
        warp_dVtt(i,j,k) = Vtt
        warp_dVtx(i,j,k) = Vtx
        warp_dVty(i,j,k) = Vty
        warp_dVtz(i,j,k) = Vtz
        warp_dVxx(i,j,k) = Vxx
        warp_dVxy(i,j,k) = Vxy
        warp_dVxz(i,j,k) = Vxz
        warp_dVyy(i,j,k) = Vyy
        warp_dVyz(i,j,k) = Vyz
        warp_dVzz(i,j,k) = Vzz

        warp_accel(i,j,k)  = Al
        warp_dAx(i,j,k)    = Ax
        warp_dAy(i,j,k)    = Ay
        warp_dAz(i,j,k)    = Az
        warp_omega2(i,j,k) = 0.25d0*(Vy*Vy + Vz*Vz)
        warp_theta(i,j,k)  = Vx
        warp_rs(i,j,k)     = rs

        ! -------------------------------------------------- analytic matter
        call warp_matter_point(Vx, Vy, Vz, Vxx, Vxy, Vxz, Vyy, Vyz, Vzz,     &
                               Ax, Ay, Az,                                   &
                               rho, Sx, Sy, Sz,                              &
                               Sxx, Sxy, Sxz, Syy, Syz, Szz, Str, R4)

        warp_rho(i,j,k)    = rho
        warp_Sx(i,j,k)     = Sx
        warp_Sy(i,j,k)     = Sy
        warp_Sz(i,j,k)     = Sz
        warp_Sxx(i,j,k)    = Sxx
        warp_Sxy(i,j,k)    = Sxy
        warp_Sxz(i,j,k)    = Sxz
        warp_Syy(i,j,k)    = Syy
        warp_Syz(i,j,k)    = Syz
        warp_Szz(i,j,k)    = Szz
        warp_Strace(i,j,k) = Str
        warp_Ricci4(i,j,k) = R4

        ! ----------------------------------------- free regression tests
        call warp_matter_identities(rho, Sxx, Syy, Szz, Str, R4, Ax, e1, e2, e3)
        worst = max(worst, abs(e1), abs(e2), abs(e3))

      end do
    end do
  end do
  !$OMP END PARALLEL DO

  ! ------------------------------------------------------------ identities
  if (verify_identities /= 0) then
    if (worst > identity_tolerance) then
      write(msg,'(A,ES12.5,A,ES12.5)')                                       &
        "WarpBase: matter identity residual ", worst, " exceeds tolerance ",  &
        identity_tolerance
      call CCTK_WARN(CCTK_WARN_ABORT, msg)
    end if
  end if

  ! -------------------------------------------------------- finiteness
  ! Milestone 2 found NaN at the bubble centre from h = f'/r = 0/0.  The
  ! regularized evaluator removes it, but the guard stays: a silent NaN that
  ! only appears on one grid point is the worst possible failure mode.
  if (check_finite /= 0) then
    nbad = 0
    !$OMP PARALLEL DO COLLAPSE(3) REDUCTION(+:nbad)
    do k = 1, cctk_lsh(3)
      do j = 1, cctk_lsh(2)
        do i = 1, cctk_lsh(1)
          if (.not. ieee_is_finite(warp_V(i,j,k))    .or.                    &
              .not. ieee_is_finite(warp_dVx(i,j,k))  .or.                    &
              .not. ieee_is_finite(warp_dVy(i,j,k))  .or.                    &
              .not. ieee_is_finite(warp_dVz(i,j,k))  .or.                    &
              .not. ieee_is_finite(warp_dVxx(i,j,k)) .or.                    &
              .not. ieee_is_finite(warp_dAx(i,j,k))  .or.                    &
              .not. ieee_is_finite(warp_rho(i,j,k))) nbad = nbad + 1
        end do
      end do
    end do
    !$OMP END PARALLEL DO
    if (nbad > 0) then
      write(msg,'(A,I0,A)') "WarpBase: ", nbad,                              &
        " grid points hold non-finite values.  If origin_treatment=naive "// &
        "this is expected at r_s=0."
      call CCTK_WARN(CCTK_WARN_ABORT, msg)
    end if
  end if
end subroutine WarpBase_Fill
EOF

# =============================================================================
# ===========================  W A R P I N I T I A L  =========================
# =============================================================================

cat > "$ARR/WarpInitial/interface.ccl" <<'EOF'
# interface.ccl -- WarpInitial
# Fills ADMBase.  Owns no grid functions of its own: it must not become a
# second place where geometry is defined.
implements: WarpInitial
inherits: ADMBase grid WarpBase
EOF

cat > "$ARR/WarpInitial/param.ccl" <<'EOF'
# param.ccl -- WarpInitial

shares: ADMBase

EXTENDS KEYWORD initial_data
{
  "warpdrive" :: "[DERIVED] gamma_ij = delta_ij, K_ij from the one-component Natario subcase"
}

EXTENDS KEYWORD initial_lapse
{
  "warpdrive" :: "[DERIVED] alpha = 1 (restriction R2).  This is GEODESIC SLICING."
}

EXTENDS KEYWORD initial_shift
{
  "warpdrive" :: "[DERIVED] beta^i = (-V,0,0) (restriction R2)"
}

EXTENDS KEYWORD initial_dtlapse
{
  "warpdrive" :: "[DERIVED] d_t alpha = 0"
}

EXTENDS KEYWORD initial_dtshift
{
  "warpdrive" :: "[DERIVED] d_t beta^i = (-d_t V, 0, 0)"
}

private:

BOOLEAN warn_about_geodesic_slicing "Emit the alpha=1 focusing warning at startup" STEERABLE=never
{
} "yes"
EOF

cat > "$ARR/WarpInitial/schedule.ccl" <<'EOF'
# schedule.ccl -- WarpInitial

if (CCTK_EQUALS(initial_data, "warpdrive"))
{
  schedule WarpInitial_Metric IN ADMBase_InitialData
  {
    LANG: Fortran
    READS:  WarpBase::warp_v_group(everywhere) WarpBase::warp_dv1_group(everywhere)
    WRITES: ADMBase::metric(everywhere) ADMBase::curv(everywhere)
  } "Set gamma_ij = delta_ij and K_ij [DERIVED]"
}

if (CCTK_EQUALS(initial_lapse, "warpdrive") || CCTK_EQUALS(initial_shift, "warpdrive"))
{
  schedule WarpInitial_Gauge IN ADMBase_InitialGauge
  {
    LANG: Fortran
    READS:  WarpBase::warp_v_group(everywhere)
    WRITES: ADMBase::lapse(everywhere) ADMBase::shift(everywhere)
  } "Set alpha = 1 and beta^i = (-V,0,0) [DERIVED]"
}

if (CCTK_EQUALS(initial_dtlapse, "warpdrive"))
{
  schedule WarpInitial_DtLapse IN ADMBase_InitialGauge AFTER WarpInitial_Gauge
  {
    LANG: Fortran
    WRITES: ADMBase::dtlapse(everywhere)
  } "Set d_t alpha = 0"
}

if (CCTK_EQUALS(initial_dtshift, "warpdrive"))
{
  schedule WarpInitial_DtShift IN ADMBase_InitialGauge AFTER WarpInitial_Gauge
  {
    LANG: Fortran
    READS:  WarpBase::warp_dv1_group(everywhere)
    WRITES: ADMBase::dtshift(everywhere)
  } "Set d_t beta^i = (-d_t V,0,0)"
}

schedule WarpInitial_Announce AT CCTK_PARAMCHECK
{
  LANG: Fortran
  OPTIONS: global
} "Announce the gauge and its known pathology"
EOF

cat > "$ARR/WarpInitial/configuration.ccl" <<'EOF'
# configuration.ccl -- WarpInitial
# Needs WarpBase's Fortran modules, hence the build-order requirement.
REQUIRES WarpBase
EOF

cat > "$ARR/WarpInitial/src/make.code.defn" <<'EOF'
# make.code.defn -- WarpInitial
SRCS = warp_geometry.F90 warp_extrinsic.F90 warp_initial.F90
EOF

cat > "$ARR/WarpInitial/src/warp_geometry.F90" <<'EOF'
#include "cctk.h"

! =============================================================================
! warp_geometry -- pointwise (alpha, beta^i, gamma_ij)  [DERIVED]
! =============================================================================
!
! Starting from the one-component Natario-class subcase under R1+R2+R3,
!
!     ds^2 = -dt^2 + (dx - V dt)^2 + dy^2 + dz^2
!
! and matching to the ADM form
!
!     ds^2 = -alpha^2 dt^2 + gamma_ij (dx^i + beta^i dt)(dx^j + beta^j dt)
!
! term by term:
!     gamma_ij = delta_ij                       (restriction R3, flat slices)
!     beta^i   = (-V, 0, 0)                     (restriction R2, N^i = -V^i)
!     alpha    = 1                              (restriction R2, N = 1)
!
! Cross-check on g_tt:  -alpha^2 + gamma_ij beta^i beta^j = -1 + V^2, which is
! the (t,t) component of the line element above.  Consistent.
!
! IMPORTANT.  alpha = 1 is geodesic slicing.  It is a known focusing gauge:
! coordinate observers are freely falling and their worldlines converge.  The
! paper separately predicts a PHYSICAL caustic instability for Example 2
! [PAPER-SECONDARY].  On a plot these look the same.  They are discriminated
! by curvature invariants (physical) versus det(gamma) collapse at fixed
! invariants (gauge), and by repeating the run in 1+log slicing.
! =============================================================================

module warp_geometry
  implicit none
  private
  public :: warp_adm_gauge, warp_adm_metric

contains

  !> alpha and beta^i at a point.
  pure subroutine warp_adm_gauge(V, alp, betax, betay, betaz)
    CCTK_REAL, intent(in)  :: V
    CCTK_REAL, intent(out) :: alp, betax, betay, betaz
    alp   = 1.0d0
    betax = -V
    betay = 0.0d0
    betaz = 0.0d0
  end subroutine warp_adm_gauge

  !> gamma_ij at a point.  Flat by R3 -- but written out explicitly rather
  !! than assumed, so that a future R1-Warp variant (which DROPS R3) has one
  !! obvious place to change.
  pure subroutine warp_adm_metric(gxx_, gxy_, gxz_, gyy_, gyz_, gzz_)
    CCTK_REAL, intent(out) :: gxx_, gxy_, gxz_, gyy_, gyz_, gzz_
    gxx_ = 1.0d0; gyy_ = 1.0d0; gzz_ = 1.0d0
    gxy_ = 0.0d0; gxz_ = 0.0d0; gyz_ = 0.0d0
  end subroutine warp_adm_metric

end module warp_geometry
EOF

cat > "$ARR/WarpInitial/src/warp_extrinsic.F90" <<'EOF'
#include "cctk.h"

! =============================================================================
! warp_extrinsic -- pointwise K_ij  [DERIVED-CAS]
! =============================================================================
!
!     K_ij = -(1/(2 alpha)) ( d_t gamma_ij - Lie_beta gamma_ij )        [MTW/BS]
!
! With alpha = 1, gamma_ij = delta_ij (so d_t gamma_ij = 0) and beta^i =
! (-V,0,0), the Lie derivative reduces to Lie_beta gamma_ij = d_i beta_j +
! d_j beta_i, giving
!
!     K_xx = -d_x V
!     K_xy = -(1/2) d_y V
!     K_xz = -(1/2) d_z V
!     K_yy = K_yz = K_zz = 0        IDENTICALLY, for every profile
!
! and trace K = -theta = -d_x V.
!
! Three of six components vanish profile-independently.  That is the cheapest
! structural test in the whole suite (T4a) and it is free.
!
! THE SIGN IS NOT ASSUMED.  It is flipped here and ONLY here, from the
! k_sign_convention parameter, whose value must come from running
! verify_sign_convention.sh against the actual checkout.  Never flip a sign in
! an analysis thorn: that hides the error instead of fixing it.
! =============================================================================

module warp_extrinsic
  implicit none
  private
  public :: warp_adm_curv

contains

  pure subroutine warp_adm_curv(dVx, dVy, dVz, ksign,                    &
                                kxx_, kxy_, kxz_, kyy_, kyz_, kzz_)
    CCTK_REAL, intent(in)  :: dVx, dVy, dVz
    CCTK_REAL, intent(in)  :: ksign          !< +1 mtw_admbase, -1 opposite
    CCTK_REAL, intent(out) :: kxx_, kxy_, kxz_, kyy_, kyz_, kzz_
    kxx_ = ksign*(-dVx)
    kxy_ = ksign*(-0.5d0*dVy)
    kxz_ = ksign*(-0.5d0*dVz)
    kyy_ = 0.0d0
    kyz_ = 0.0d0
    kzz_ = 0.0d0
  end subroutine warp_adm_curv

end module warp_extrinsic
EOF

cat > "$ARR/WarpInitial/src/warp_initial.F90" <<'EOF'
#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

! =============================================================================
! WarpInitial -- scheduled routines.  Thin: all equations live in the modules.
! =============================================================================

subroutine WarpInitial_Metric(CCTK_ARGUMENTS)
  use warp_geometry
  use warp_extrinsic
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS
  integer   :: i, j, k
  CCTK_REAL :: ksign

  ksign = 1.0d0
  if (CCTK_EQUALS(k_sign_convention, "opposite")) ksign = -1.0d0

  !$OMP PARALLEL DO COLLAPSE(3) PRIVATE(i,j,k)
  do k = 1, cctk_lsh(3)
    do j = 1, cctk_lsh(2)
      do i = 1, cctk_lsh(1)
        call warp_adm_metric(gxx(i,j,k), gxy(i,j,k), gxz(i,j,k),          &
                             gyy(i,j,k), gyz(i,j,k), gzz(i,j,k))
        call warp_adm_curv(warp_dVx(i,j,k), warp_dVy(i,j,k),              &
                           warp_dVz(i,j,k), ksign,                        &
                           kxx(i,j,k), kxy(i,j,k), kxz(i,j,k),            &
                           kyy(i,j,k), kyz(i,j,k), kzz(i,j,k))
      end do
    end do
  end do
  !$OMP END PARALLEL DO
end subroutine WarpInitial_Metric


subroutine WarpInitial_Gauge(CCTK_ARGUMENTS)
  use warp_geometry
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS
  integer :: i, j, k

  !$OMP PARALLEL DO COLLAPSE(3) PRIVATE(i,j,k)
  do k = 1, cctk_lsh(3)
    do j = 1, cctk_lsh(2)
      do i = 1, cctk_lsh(1)
        call warp_adm_gauge(warp_V(i,j,k), alp(i,j,k),                    &
                            betax(i,j,k), betay(i,j,k), betaz(i,j,k))
      end do
    end do
  end do
  !$OMP END PARALLEL DO
end subroutine WarpInitial_Gauge


subroutine WarpInitial_DtLapse(CCTK_ARGUMENTS)
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS
  dtalp = 0.0d0                          ! alpha = 1 for all time under R2
end subroutine WarpInitial_DtLapse


subroutine WarpInitial_DtShift(CCTK_ARGUMENTS)
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS
  integer :: i, j, k

  !$OMP PARALLEL DO COLLAPSE(3) PRIVATE(i,j,k)
  do k = 1, cctk_lsh(3)
    do j = 1, cctk_lsh(2)
      do i = 1, cctk_lsh(1)
        dtbetax(i,j,k) = -warp_dVt(i,j,k)
        dtbetay(i,j,k) = 0.0d0
        dtbetaz(i,j,k) = 0.0d0
      end do
    end do
  end do
  !$OMP END PARALLEL DO
end subroutine WarpInitial_DtShift


subroutine WarpInitial_Announce(CCTK_ARGUMENTS)
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS
  if (CCTK_MyProc(cctkGH) /= 0) return
  if (warn_about_geodesic_slicing == 0) return
  if (.not. CCTK_EQUALS(initial_lapse, "warpdrive")) return

  call CCTK_INFO("WarpInitial: alpha = 1 is GEODESIC SLICING (restriction R2).")
  call CCTK_INFO("WarpInitial: geodesic slicing focuses; coordinate observers "// &
                 "converge and det(gamma) can collapse for purely GAUGE reasons.")
  call CCTK_INFO("WarpInitial: the paper separately predicts a PHYSICAL caustic "//&
                 "instability [PAPER-SECONDARY].  These two look identical on a plot.")
  call CCTK_INFO("WarpInitial: discriminate with curvature INVARIANTS, and by "//  &
                 "repeating the run in 1+log slicing, BEFORE claiming an instability.")
end subroutine WarpInitial_Announce
EOF

# =============================================================================
# ============================  W A R P T M U N U  ============================
# =============================================================================
# Without this thorn, Experiment B is INVALID.  The data has rho != 0, so it
# satisfies the constraints only WITH matter present.  Handing it to vacuum
# McLachlan produces a finite, resolution-INDEPENDENT constraint violation
# that never converges away -- and reporting that as "the warp solution is
# unstable" would be a serious error.

cat > "$ARR/WarpTmunu/interface.ccl" <<'EOF'
# interface.ccl -- WarpTmunu
implements: WarpTmunu
inherits: ADMBase TmunuBase WarpBase grid
EOF

cat > "$ARR/WarpTmunu/param.ccl" <<'EOF'
# param.ccl -- WarpTmunu

restricted:

BOOLEAN add_to_tmunu "Add the analytic warp stress-energy to TmunuBase" STEERABLE=always
{
} "yes"

# [IMPL] Deliberate mis-sourcing knob.  Set to 0 to reproduce the INVALID
# vacuum-evolution experiment, or to 1.01 to measure how sensitively the
# constraints respond to a 1% error in T_mu_nu.  Both are useful controls;
# neither is a physics run.
CCTK_REAL tmunu_scale "Multiply the warp T_mu_nu by this before adding" STEERABLE=always
{
  *:* :: "1.0 is the only physically correct value"
} 1.0
EOF

cat > "$ARR/WarpTmunu/schedule.ccl" <<'EOF'
# schedule.ccl -- WarpTmunu

if (add_to_tmunu)
{
  # AddToTmunu is TmunuBase's group; members ADD to eT..., they never assign.
  # TmunuBase has already zeroed the components at this point.
  schedule WarpTmunu_Add IN AddToTmunu
  {
    LANG: Fortran
    READS:  ADMBase::lapse(everywhere) ADMBase::shift(everywhere)
    READS:  WarpBase::warp_matter_group(everywhere)
    WRITES: TmunuBase::stress_energy_scalar(everywhere)
    WRITES: TmunuBase::stress_energy_vector(everywhere)
    WRITES: TmunuBase::stress_energy_tensor(everywhere)
  } "Add the analytic warp stress-energy tensor to TmunuBase"
}

schedule WarpTmunu_Announce AT CCTK_PARAMCHECK
{
  LANG: Fortran
  OPTIONS: global
} "Warn if the stress-energy is being mis-sourced on purpose"
EOF

cat > "$ARR/WarpTmunu/configuration.ccl" <<'EOF'
# configuration.ccl -- WarpTmunu
REQUIRES WarpBase
EOF

cat > "$ARR/WarpTmunu/src/make.code.defn" <<'EOF'
# make.code.defn -- WarpTmunu
SRCS = warp_tmunu.F90
EOF

cat > "$ARR/WarpTmunu/src/warp_tmunu.F90" <<'EOF'
#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

! =============================================================================
! WarpTmunu -- push the analytic (rho, S_i, S_ij) into TmunuBase as T_mu_nu.
! =============================================================================
!
! TmunuBase stores the COVARIANT 4-D components eTtt, eTti, eTij.  The inverse
! of the 3+1 projection is  [DERIVED, standard 3+1 identity]
!
!     T_mu_nu = rho n_mu n_nu + n_mu S_nu + S_mu n_nu + S_mu_nu
!
! with n_mu = (-alpha, 0, 0, 0).  Because S_mu and S_mu_nu are purely spatial
! (S_mu n^mu = 0 with n^mu = (1/alpha)(1, -beta^i)) their time components are
! not independent:
!
!     S_t    = beta^i S_i
!     S_ti   = beta^j S_ji
!     S_tt   = beta^i beta^j S_ij
!
! Substituting gives the components TmunuBase wants:
!
!     T_tt = alpha^2 rho - 2 alpha beta^i S_i + beta^i beta^j S_ij
!     T_ti = -alpha S_i + beta^j S_ji
!     T_ij = S_ij
!
! For this model alpha = 1 and beta^i = (-V,0,0), so T_tt = rho + 2 V S_x +
! V^2 S_xx.  The general form is coded anyway: in Experiment B the gauge is
! allowed to evolve, and hard-coding alpha = 1 there would be silently wrong.
! =============================================================================
subroutine WarpTmunu_Add(CCTK_ARGUMENTS)
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  integer   :: i, j, k
  CCTK_REAL :: a, bx, by, bz, s
  CCTK_REAL :: rho_, Sx_, Sy_, Sz_
  CCTK_REAL :: Sxx_, Sxy_, Sxz_, Syy_, Syz_, Szz_
  CCTK_REAL :: bS, bbS, Ttx_, Tty_, Ttz_

  s = tmunu_scale

  !$OMP PARALLEL DO COLLAPSE(3) DEFAULT(NONE)                                &
  !$OMP   PRIVATE(i,j,k,a,bx,by,bz,rho_,Sx_,Sy_,Sz_,                         &
  !$OMP           Sxx_,Sxy_,Sxz_,Syy_,Syz_,Szz_,bS,bbS,Ttx_,Tty_,Ttz_)       &
  !$OMP   SHARED(cctk_lsh,s,alp,betax,betay,betaz,                           &
  !$OMP          warp_rho,warp_Sx,warp_Sy,warp_Sz,                           &
  !$OMP          warp_Sxx,warp_Sxy,warp_Sxz,warp_Syy,warp_Syz,warp_Szz,      &
  !$OMP          eTtt,eTtx,eTty,eTtz,eTxx,eTxy,eTxz,eTyy,eTyz,eTzz)
  do k = 1, cctk_lsh(3)
    do j = 1, cctk_lsh(2)
      do i = 1, cctk_lsh(1)

        a  = alp(i,j,k)
        bx = betax(i,j,k); by = betay(i,j,k); bz = betaz(i,j,k)

        rho_ = s*warp_rho(i,j,k)
        Sx_  = s*warp_Sx(i,j,k);  Sy_  = s*warp_Sy(i,j,k);  Sz_  = s*warp_Sz(i,j,k)
        Sxx_ = s*warp_Sxx(i,j,k); Sxy_ = s*warp_Sxy(i,j,k); Sxz_ = s*warp_Sxz(i,j,k)
        Syy_ = s*warp_Syy(i,j,k); Syz_ = s*warp_Syz(i,j,k); Szz_ = s*warp_Szz(i,j,k)

        bS  = bx*Sx_ + by*Sy_ + bz*Sz_
        bbS =      bx*bx*Sxx_ +     by*by*Syy_ +     bz*bz*Szz_               &
             + 2.0d0*bx*by*Sxy_ + 2.0d0*bx*bz*Sxz_ + 2.0d0*by*bz*Syz_

        Ttx_ = -a*Sx_ + (bx*Sxx_ + by*Sxy_ + bz*Sxz_)
        Tty_ = -a*Sy_ + (bx*Sxy_ + by*Syy_ + bz*Syz_)
        Ttz_ = -a*Sz_ + (bx*Sxz_ + by*Syz_ + bz*Szz_)

        ! ADD.  TmunuBase zeroed these; other matter thorns may also add.
        eTtt(i,j,k) = eTtt(i,j,k) + a*a*rho_ - 2.0d0*a*bS + bbS
        eTtx(i,j,k) = eTtx(i,j,k) + Ttx_
        eTty(i,j,k) = eTty(i,j,k) + Tty_
        eTtz(i,j,k) = eTtz(i,j,k) + Ttz_
        eTxx(i,j,k) = eTxx(i,j,k) + Sxx_
        eTxy(i,j,k) = eTxy(i,j,k) + Sxy_
        eTxz(i,j,k) = eTxz(i,j,k) + Sxz_
        eTyy(i,j,k) = eTyy(i,j,k) + Syy_
        eTyz(i,j,k) = eTyz(i,j,k) + Syz_
        eTzz(i,j,k) = eTzz(i,j,k) + Szz_

      end do
    end do
  end do
  !$OMP END PARALLEL DO
end subroutine WarpTmunu_Add


subroutine WarpTmunu_Announce(CCTK_ARGUMENTS)
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS
  character(len=256) :: msg
  if (CCTK_MyProc(cctkGH) /= 0) return

  if (add_to_tmunu == 0) then
    call CCTK_WARN(CCTK_WARN_ALERT,                                          &
      "WarpTmunu: add_to_tmunu=no.  The initial data has rho != 0, so a "//  &
      "VACUUM evolution violates the Hamiltonian constraint by "//           &
      "construction.  That violation will NOT converge away under "//        &
      "refinement.  Do not interpret it as an instability.")
  else if (abs(tmunu_scale - 1.0d0) > 1.0d-14) then
    write(msg,'(A,ES12.5,A)') "WarpTmunu: tmunu_scale = ", tmunu_scale,      &
      " != 1.  This is a deliberately mis-sourced control run, not physics."
    call CCTK_WARN(CCTK_WARN_ALERT, msg)
  end if
end subroutine WarpTmunu_Announce
EOF

echo
echo "done."
echo "  WarpBase     $(ls "$ARR/WarpBase/src" | wc -l) sources"
echo "  WarpInitial  $(ls "$ARR/WarpInitial/src" | wc -l) sources"
echo "  WarpTmunu    $(ls "$ARR/WarpTmunu/src" | wc -l) sources"
echo
echo "next:  ./install_warpdrive_analysis.sh $CACTUS"
echo "       ./install_warpdrive_runtime.sh  $CACTUS"
echo
echo "BEFORE building: run verify_sign_convention.sh $CACTUS  (test T0, BLOCKING)"

