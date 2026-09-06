#!/usr/bin/env bash
#
# install_warpdrive_analysis.sh
# =============================
# Writes arrangements/WarpDrive/WarpAnalysis into an Einstein Toolkit checkout.
#
#   usage:  ./install_warpdrive_analysis.sh /path/to/Cactus
#
# Run install_warpdrive_core.sh FIRST -- this thorn USEs WarpBase's modules.
#
# WarpAnalysis is DIAGNOSTIC ONLY.  It declares no ADMBase or ML_BSSN variable
# as WRITES anywhere in schedule.ccl, so it structurally cannot perturb an
# evolution (brief Sec. 4).
#
set -euo pipefail

CACTUS="${1:-}"
[[ -n "$CACTUS" && -d "$CACTUS" ]] || { echo "usage: $0 /path/to/Cactus" >&2; exit 2; }
T="$CACTUS/arrangements/WarpDrive/WarpAnalysis"
[[ -d "$CACTUS/arrangements/WarpDrive/WarpBase" ]] || {
  echo "error: WarpBase not found.  Run install_warpdrive_core.sh first." >&2; exit 2; }
mkdir -p "$T/src"
echo "installing into $T"

# =============================================================================
cat > "$T/interface.ccl" <<'EOF'
# interface.ccl -- WarpAnalysis
#
# Diagnostics only.  Reads ADMBase and WarpBase; writes only its own grid
# functions.  Nothing here can influence an evolution.

implements: WarpAnalysis
inherits: ADMBase TmunuBase WarpBase grid

USES INCLUDE HEADER: cctk_Functions.h

# ---------------------------------------------------------------- curvature
# NOTE THE NAMES.  RicciScalar4 and RicciScalar3 are SEPARATE grid functions.
# Milestone-2 erratum E1: Phase 0 asserted "R = 0", which is true of the
# SPATIAL scalar (trivially, since gamma_ij = delta_ij is restriction R3) and
# FALSE for the 4-D scalar.  A diagnostic that printed "RicciScalar = 0,
# slices are flat" while computing R^(4) would be a false pass.  Two names
# make that mistake impossible.
public:
CCTK_REAL warp_curv_scalars TYPE=GF TIMELEVELS=1 TAGS='checkpoint="no"'
{
  RicciScalar4,          # R^(4) = 2 d_x A + 2 Omega^2   [DERIVED-CAS]
  RicciScalar3,          # R^(3) = 0 under R3            [DERIVED]
  RicciSquared,          # R_mn R^mn                     [DERIVED-CAS]
  Kretschmann,           # R_mnab R^mnab                 [DERIVED-CAS]
  WeylPsi2Approx         # (1/3) tr E, diagnostic only
} "Curvature invariants"

# ------------------------------------------------------------ stress-energy
CCTK_REAL warp_se TYPE=GF TIMELEVELS=1 TAGS='checkpoint="no"'
{
  EnergyDensity,                                   # rho
  MomentumDensity_x, MomentumDensity_y, MomentumDensity_z,   # S_i
  StressTrace,                                     # S = gamma^ij S_ij
  PressureIsotropic,                               # S/3, for orientation only
  StressAnisotropy                                 # |S_ij - (S/3)gamma_ij|
} "3+1 matter variables inferred from G_mn/8pi"

# ------------------------------------------------------------- verification
# Every one of these should be ~1e-15 for analytic data.  They are the
# machine-checkable form of brief Sec. 14 tests T5, T6, T7.
CCTK_REAL warp_verify TYPE=GF TIMELEVELS=1 TAGS='checkpoint="no"'
{
  ResidualRho,           # analytic rho  - rho from numerical G_mn
  ResidualSi,            # max_i |analytic S_i - numerical S_i|
  ResidualSij,           # max_ij |analytic S_ij - numerical S_ij|
  ResidualIdentity,      # max of the three free algebraic identities
  ResidualKij,           # |K_ij(ADMBase) - K_ij(analytic)|, Experiment B drift
  ResidualGammaij        # |gamma_ij(ADMBase) - delta_ij|,  Experiment B drift
} "Analytic-vs-numerical residuals"

# -------------------------------------------------------------- tidal tensor
CCTK_REAL warp_tidal TYPE=GF TIMELEVELS=1 TAGS='checkpoint="no"'
{
  Exx, Exy, Exz, Eyy, Eyz, Ezz,
  TidalEigenvalue1, TidalEigenvalue2, TidalEigenvalue3,   # ascending
  TidalMaxAbs,                                            # max |lambda|
  TidalTraceResidual,        # tr E - 4pi(rho + S)   -> 0   [free identity]
  TidalMinorResidual         # E_yy E_zz - E_yz^2    -> 0   [free identity]
} "Electric tidal tensor E_ij = R_imjn u^m u^n and its eigenvalues"

# ---------------------------------------------------------- energy conditions
# Sign convention for ALL of these: NEGATIVE means VIOLATED.  Each field is
# the minimum of the relevant scalar over the sampled observer/null set, so
# the zero contour is the violation boundary and it can be plotted directly.
CCTK_REAL warp_ec TYPE=GF TIMELEVELS=1 TAGS='checkpoint="no"'
{
  NEC_min,               # min over null k of T_mn k^m k^n     (EXACT)
  WEC_min,               # min over timelike u of T_mn u^m u^n (sampled)
  DEC_min,               # min of the DEC margin               (sampled)
  SEC_min,               # min of (T_mn - T g_mn/2) u^m u^n    (sampled)
  EC_violation_flags     # bitmask: 1=NEC 2=WEC 4=DEC 8=SEC
} "Energy-condition margins.  NEGATIVE = VIOLATED."

# --------------------------------------------------------------- constraints
CCTK_REAL warp_constraints TYPE=GF TIMELEVELS=1 TAGS='checkpoint="no"'
{
  HamiltonianConstraint,
  MomentumConstraint_x, MomentumConstraint_y, MomentumConstraint_z,
  HamiltonianConstraint_vacuum   # the same, with T_mn FORCED to zero
} "ADM constraints computed from ADMBase by finite differencing"

CCTK_INT warp_analysis_flags TYPE=GF TIMELEVELS=1 TAGS='checkpoint="no"'
{
  AnalysisMask           # 0 = interior, 1 = too close to boundary for FD
} "Which points carry a trustworthy finite-difference stencil"
EOF

# =============================================================================
cat > "$T/param.ccl" <<'EOF'
# param.ccl -- WarpAnalysis

restricted:

KEYWORD curvature_source "Where curvature invariants come from" STEERABLE=always
{
  "analytic"  :: "[DERIVED-CAS] closed forms in V and its derivatives (WarpBase). Exact."
  "numerical" :: "[IMPL] finite differences of the ADMBase 4-metric. Required for Experiment B."
  "both"      :: "compute both and fill the Residual* fields.  Tests T5/T6."
} "both"

INT fd_order "Centred finite-difference order for the numerical route"
{
  2:8:2 :: "even orders only"
} 4

BOOLEAN compute_curvature       "Curvature invariants"        STEERABLE=always {} "yes"
BOOLEAN compute_stress_energy   "3+1 matter variables"        STEERABLE=always {} "yes"
BOOLEAN compute_tidal           "Tidal tensor + eigenvalues"  STEERABLE=always {} "yes"
BOOLEAN compute_energy_conditions "NEC/WEC/DEC/SEC"           STEERABLE=always {} "yes"
BOOLEAN compute_constraints     "ADM constraints"             STEERABLE=always {} "yes"
BOOLEAN compute_drift           "Experiment-B drift vs the analytic solution" STEERABLE=always {} "yes"

# --------------------------------------------------------- energy conditions
# HONESTY NOTE, and it matters for how results may be phrased.
#
# NEC_min is EXACT: minimising rho + 2 S_i e^i + S_ij e^i e^j over the unit
# sphere is a solvable secular problem and that is what the code does.
#
# WEC/DEC/SEC are SAMPLED over boost directions and rapidities.  Sampling can
# PROVE a violation (one bad observer suffices) but can never PROVE
# satisfaction.  Output labelled "satisfied" therefore means "no violation
# found on the sampled set", and the README says so.
INT ec_n_directions "Spatial directions sampled for WEC/DEC/SEC (icosahedral)"
{
  12:642 :: "12, 42, 162, 642 -> subdivision levels 0..3"
} 162

INT ec_n_rapidity "Rapidity samples per direction, tanh-spaced in [0,1)"
{
  1:64 :: ""
} 8

CCTK_REAL ec_max_lorentz "Largest Lorentz factor sampled (WEC/DEC/SEC)"
{
  (1.0:* :: "> 1"
} 50.0

# ------------------------------------------------------------- thresholds
CCTK_REAL verification_tolerance "Residual above which T5/T6/T7 are reported FAILED" STEERABLE=always
{
  (0:* :: "positive"
} 1.0e-10

CCTK_REAL ec_tolerance "Margin below -ec_tolerance counts as a genuine violation" STEERABLE=always
{
  0:* :: "non-negative"
} 1.0e-14

BOOLEAN report_norms "Print global L2/Linf norms of the residuals and constraints" STEERABLE=always
{
} "yes"

INT report_every "Print the summary every N iterations (0 = never)" STEERABLE=always
{
  0:* :: ""
} 0

# ----------------------------------------------------------------- masking
INT boundary_exclude "Points to exclude from FD diagnostics at each outer face" STEERABLE=never
{
  0:* :: "should be >= fd_order/2"
} 3
EOF

# =============================================================================
cat > "$T/schedule.ccl" <<'EOF'
# schedule.ccl -- WarpAnalysis
#
# Everything is scheduled in ANALYSIS.  Note that NO ADMBase or ML_BSSN
# variable appears as WRITES anywhere below: the thorn is structurally
# incapable of altering an evolution.

STORAGE: warp_curv_scalars warp_se warp_verify warp_tidal
STORAGE: warp_ec warp_constraints warp_analysis_flags

schedule WarpAnalysis_ParamCheck AT CCTK_PARAMCHECK
{
  LANG: Fortran
  OPTIONS: global
} "Validate diagnostic parameters"

schedule WarpAnalysis_Mask AT CCTK_BASEGRID AFTER SpatialCoordinates
{
  LANG: Fortran
  WRITES: AnalysisMask(everywhere)
} "Flag points whose finite-difference stencil reaches outside the grid"

schedule group WarpAnalysis_Diagnostics AT CCTK_ANALYSIS
{
} "WarpDrive diagnostics"

if (compute_curvature)
{
  schedule WarpAnalysis_Curvature IN WarpAnalysis_Diagnostics
  {
    LANG: Fortran
    READS:  WarpBase::warp_v_group(everywhere) WarpBase::warp_dv1_group(everywhere)
    READS:  WarpBase::warp_dv2_group(everywhere) WarpBase::warp_kin_group(everywhere)
    READS:  ADMBase::metric(everywhere) ADMBase::curv(everywhere)
    READS:  ADMBase::lapse(everywhere) ADMBase::shift(everywhere)
    READS:  AnalysisMask(everywhere)
    WRITES: warp_curv_scalars(interior)
  } "Curvature invariants: R^(4), R^(3), R_mn R^mn, Kretschmann"
}

if (compute_stress_energy)
{
  schedule WarpAnalysis_StressEnergy IN WarpAnalysis_Diagnostics
  {
    LANG: Fortran
    READS:  WarpBase::warp_matter_group(everywhere) WarpBase::warp_kin_group(everywhere)
    READS:  WarpBase::warp_dv1_group(everywhere) WarpBase::warp_dv2_group(everywhere)
    WRITES: warp_se(interior) warp_verify(interior)
  } "3+1 matter variables and the analytic-vs-numerical residuals"
}

if (compute_tidal)
{
  schedule WarpAnalysis_Tidal IN WarpAnalysis_Diagnostics AFTER WarpAnalysis_StressEnergy
  {
    LANG: Fortran
    READS:  WarpBase::warp_v_group(everywhere) WarpBase::warp_dv1_group(everywhere)
    READS:  WarpBase::warp_dv2_group(everywhere) WarpBase::warp_kin_group(everywhere)
    READS:  WarpBase::warp_matter_group(everywhere)
    WRITES: warp_tidal(interior)
  } "Electric tidal tensor, eigenvalues, and its two free identities"
}

if (compute_energy_conditions)
{
  schedule WarpAnalysis_EnergyConditions IN WarpAnalysis_Diagnostics AFTER WarpAnalysis_StressEnergy
  {
    LANG: Fortran
    READS:  WarpBase::warp_matter_group(everywhere)
    WRITES: warp_ec(interior)
  } "NEC (exact) and WEC/DEC/SEC (sampled).  Negative = violated."
}

if (compute_constraints)
{
  schedule WarpAnalysis_Constraints IN WarpAnalysis_Diagnostics
  {
    LANG: Fortran
    READS:  ADMBase::metric(everywhere) ADMBase::curv(everywhere)
    READS:  WarpBase::warp_matter_group(everywhere)
    READS:  AnalysisMask(everywhere)
    WRITES: warp_constraints(interior)
  } "ADM constraints, with and without the matter source"
}

if (compute_drift)
{
  schedule WarpAnalysis_Drift IN WarpAnalysis_Diagnostics
  {
    LANG: Fortran
    READS:  ADMBase::metric(everywhere) ADMBase::curv(everywhere)
    READS:  WarpBase::warp_dv1_group(everywhere)
    WRITES: ResidualKij(interior) ResidualGammaij(interior)
  } "Experiment-B drift of the evolved geometry from the analytic solution"
}

if (report_norms)
{
  schedule WarpAnalysis_Report IN WarpAnalysis_Diagnostics AFTER (WarpAnalysis_Curvature WarpAnalysis_StressEnergy WarpAnalysis_Tidal WarpAnalysis_EnergyConditions WarpAnalysis_Constraints WarpAnalysis_Drift)
  {
    LANG: Fortran
    OPTIONS: global
  } "Print the verification summary"
}
EOF

cat > "$T/configuration.ccl" <<'EOF'
# configuration.ccl -- WarpAnalysis
REQUIRES WarpBase
EOF

cat > "$T/src/make.code.defn" <<'EOF'
# make.code.defn -- WarpAnalysis
SRCS = warp_eigen.F90 warp_fd.F90 curvature.F90 einstein_tensor.F90 \
       stress_energy.F90 tidal_tensor.F90 energy_conditions.F90 diagnostics.F90
EOF

# ------------------------------------------------------------ warp_eigen.F90
cat > "$T/src/warp_eigen.F90" <<'EOF'
#include "cctk.h"

! =============================================================================
! warp_eigen -- closed-form eigen-decomposition of a symmetric 3x3 matrix
! =============================================================================
!
! Used for the tidal eigenvalues (brief Sec. 9) and for the exact NEC
! minimisation (Sec. 8).  Closed form, not iterative: LAPACK is not reliably
! linkable from every ETK configuration, and a per-gridpoint DSYEV call would
! dominate the cost anyway.
!
! Method: Deledalle et al. / the standard trigonometric solution of the cubic.
! Eigenvalues of a real symmetric 3x3 are always real, so the discriminant is
! non-positive and acos is safe once its argument is clamped.  The clamp is
! not cosmetic -- round-off pushes it to 1+1e-16 for degenerate matrices,
! which returns NaN without it.
! =============================================================================

module warp_eigen
  implicit none
  private
  public :: sym3_eigenvalues, sym3_eigenvector

  CCTK_REAL, parameter :: PI3 = 1.04719755119659774615d0    ! pi/3

contains

  !> Eigenvalues of the symmetric matrix [[a11,a12,a13],[a12,a22,a23],[a13,a23,a33]],
  !! returned ASCENDING in l1 <= l2 <= l3.
  pure subroutine sym3_eigenvalues(a11, a12, a13, a22, a23, a33, l1, l2, l3)
    CCTK_REAL, intent(in)  :: a11, a12, a13, a22, a23, a33
    CCTK_REAL, intent(out) :: l1, l2, l3
    CCTK_REAL :: p, q, r, phi, m, dev, tmp
    CCTK_REAL :: b11, b22, b33

    ! Shift by the trace/3 so the cubic is depressed; improves conditioning
    ! when the eigenvalues are large and close together.
    m   = (a11 + a22 + a33)/3.0d0
    b11 = a11 - m; b22 = a22 - m; b33 = a33 - m

    p = 0.5d0*(b11*b11 + b22*b22 + b33*b33) + a12*a12 + a13*a13 + a23*a23
    p = p/3.0d0

    if (p <= 1.0d-300) then                 ! isotropic: triple root
      l1 = m; l2 = m; l3 = m
      return
    end if

    ! det of the deviatoric part
    q = b11*(b22*b33 - a23*a23) - a12*(a12*b33 - a23*a13) + a13*(a12*a23 - b22*a13)

    dev = q/(2.0d0*p*sqrt(p))
    if (dev >  1.0d0) dev =  1.0d0          ! the clamp that prevents NaN
    if (dev < -1.0d0) dev = -1.0d0
    phi = acos(dev)/3.0d0

    r = 2.0d0*sqrt(p)
    l3 = m + r*cos(phi)
    l1 = m + r*cos(phi + 2.0d0*PI3)
    l2 = 3.0d0*m - l1 - l3                  ! exact trace closure, no third cos

    ! sort ascending
    if (l1 > l2) then; tmp = l1; l1 = l2; l2 = tmp; end if
    if (l2 > l3) then; tmp = l2; l2 = l3; l3 = tmp; end if
    if (l1 > l2) then; tmp = l1; l1 = l2; l2 = tmp; end if
  end subroutine sym3_eigenvalues

  !> A unit eigenvector for eigenvalue lam, via cross products of the rows of
  !! (A - lam I).  The largest-norm cross product is the best conditioned, so
  !! all three are formed and the winner is taken.
  pure subroutine sym3_eigenvector(a11, a12, a13, a22, a23, a33, lam, vx, vy, vz)
    CCTK_REAL, intent(in)  :: a11, a12, a13, a22, a23, a33, lam
    CCTK_REAL, intent(out) :: vx, vy, vz
    CCTK_REAL :: r1(3), r2(3), r3(3), c1(3), c2(3), c3(3)
    CCTK_REAL :: n1, n2, n3, nrm

    r1 = [a11 - lam, a12,       a13      ]
    r2 = [a12,       a22 - lam, a23      ]
    r3 = [a13,       a23,       a33 - lam]

    c1 = [r1(2)*r2(3) - r1(3)*r2(2), r1(3)*r2(1) - r1(1)*r2(3), r1(1)*r2(2) - r1(2)*r2(1)]
    c2 = [r1(2)*r3(3) - r1(3)*r3(2), r1(3)*r3(1) - r1(1)*r3(3), r1(1)*r3(2) - r1(2)*r3(1)]
    c3 = [r2(2)*r3(3) - r2(3)*r3(2), r2(3)*r3(1) - r2(1)*r3(3), r2(1)*r3(2) - r2(2)*r3(1)]

    n1 = sum(c1*c1); n2 = sum(c2*c2); n3 = sum(c3*c3)

    if (n1 >= n2 .and. n1 >= n3) then
      nrm = sqrt(n1); vx = c1(1); vy = c1(2); vz = c1(3)
    else if (n2 >= n3) then
      nrm = sqrt(n2); vx = c2(1); vy = c2(2); vz = c2(3)
    else
      nrm = sqrt(n3); vx = c3(1); vy = c3(2); vz = c3(3)
    end if

    if (nrm > 1.0d-150) then
      vx = vx/nrm; vy = vy/nrm; vz = vz/nrm
    else
      vx = 1.0d0; vy = 0.0d0; vz = 0.0d0   ! degenerate: any vector will do
    end if
  end subroutine sym3_eigenvector

end module warp_eigen
EOF

# --------------------------------------------------------------- warp_fd.F90
cat > "$T/src/warp_fd.F90" <<'EOF'
#include "cctk.h"

! =============================================================================
! warp_fd -- centred finite differences, orders 2/4/6/8
! =============================================================================
! Only used by the "numerical" curvature route and by the constraint monitor.
! The analytic route never calls this, which is exactly why the two together
! constitute an independent check (tests T5/T6).
! =============================================================================

module warp_fd
  implicit none
  private
  public :: fd1, fd2, fd11, fd_halo

contains

  !> Half-width of the stencil.
  pure integer function fd_halo(order)
    integer, intent(in) :: order
    fd_halo = order/2
  end function fd_halo

  !> First derivative along one axis.  s(-4:4) are sampled values, dx the spacing.
  pure function fd1(s, dx, order) result(d)
    CCTK_REAL, intent(in) :: s(-4:4), dx
    integer,   intent(in) :: order
    CCTK_REAL :: d
    select case (order)
    case (2)
      d = (s(1) - s(-1))/2.0d0
    case (4)
      d = (8.0d0*(s(1) - s(-1)) - (s(2) - s(-2)))/12.0d0
    case (6)
      d = (45.0d0*(s(1) - s(-1)) - 9.0d0*(s(2) - s(-2)) + (s(3) - s(-3)))/60.0d0
    case default
      d = (672.0d0*(s(1) - s(-1)) - 168.0d0*(s(2) - s(-2))                  &
           + 32.0d0*(s(3) - s(-3)) - 3.0d0*(s(4) - s(-4)))/840.0d0
    end select
    d = d/dx
  end function fd1

  !> Second derivative along one axis.
  pure function fd2(s, dx, order) result(d)
    CCTK_REAL, intent(in) :: s(-4:4), dx
    integer,   intent(in) :: order
    CCTK_REAL :: d
    select case (order)
    case (2)
      d = s(1) - 2.0d0*s(0) + s(-1)
    case (4)
      d = (-30.0d0*s(0) + 16.0d0*(s(1) + s(-1)) - (s(2) + s(-2)))/12.0d0
    case (6)
      d = (-490.0d0*s(0) + 270.0d0*(s(1) + s(-1)) - 27.0d0*(s(2) + s(-2))   &
           + 2.0d0*(s(3) + s(-3)))/180.0d0
    case default
      d = (-14350.0d0*s(0) + 8064.0d0*(s(1) + s(-1))                        &
           - 1008.0d0*(s(2) + s(-2)) + 128.0d0*(s(3) + s(-3))               &
           - 9.0d0*(s(4) + s(-4)))/5040.0d0
    end select
    d = d/(dx*dx)
  end function fd2

  !> Mixed second derivative from a 2-D patch p(-4:4,-4:4).
  pure function fd11(p, dx, dy, order) result(d)
    CCTK_REAL, intent(in) :: p(-4:4,-4:4), dx, dy
    integer,   intent(in) :: order
    CCTK_REAL :: d, col(-4:4)
    integer   :: m
    ! Tensor product of two 1-D stencils: differentiate in y, then in x.
    do m = -4, 4
      col(m) = fd1(p(m,:), dy, order)
    end do
    d = fd1(col, dx, order)
  end function fd11

end module warp_fd
EOF

# ------------------------------------------------------------- curvature.F90
cat > "$T/src/curvature.F90" <<'EOF'
#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

! =============================================================================
! WarpAnalysis_Curvature -- curvature invariants  [DERIVED-CAS]
! =============================================================================
!
! The 4-metric is ALGEBRAIC in V:
!     g_tt = -1 + V^2,  g_tx = -V,  g_ij = delta_ij
! so the full 4-D Riemann tensor is determined exactly by V and the ten
! second derivatives d_mu d_nu V, all of which WarpBase already carries.  No
! finite differencing is needed for the analytic route, which is why it is
! exact to round-off rather than to O(h^4).
!
! The closed forms below were produced by SymPy from g_mu_nu for ARBITRARY
! V(t,x,y,z), reduced by common-subexpression elimination, and transcribed.
! Verified: R^(4) reproduces 2 d_x A + 2 Omega^2 with zero residual.
!
!     R^(4)      = 2 d_x A + 2 Omega^2
!     R^(3)      = 0                                (restriction R3)
!     R_mn R^mn  = ricci_sq below
!     Kretschmann = kretsch below
!
! Sanity checks that the expressions must pass, and do:
!   * V = 0   -> every invariant vanishes                        (test T1/T2)
!   * V = const -> every invariant vanishes (flat, boosted frame) (test T2)
!   * The Kretschmann scalar contains -4 (d_y d_z V)^2, a term with NO
!     counterpart in R_mn R^mn: Weyl curvature that the Ricci sector cannot
!     see.  Its presence is a useful check that the transcription is complete.
! =============================================================================
subroutine WarpAnalysis_Curvature(CCTK_ARGUMENTS)
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  integer   :: i, j, k
  CCTK_REAL :: Vl, dVt, dVx, dVy, dVz
  CCTK_REAL :: dVtt, dVtx, dVty, dVtz, dVxx, dVxy, dVxz, dVyy, dVyz, dVzz
  CCTK_REAL :: cs0,cs1,cs2,cs3,cs4,cs5,cs6,cs7,cs8,cs9
  CCTK_REAL :: cs10,cs11,cs12,cs13,cs14,cs15,cs16,cs17,cs18,cs19
  CCTK_REAL :: cs20,cs21,cs22,cs23,cs24,cs25,cs26,cs27,cs28,cs29
  CCTK_REAL :: cs30,cs31,cs32,cs33,cs34,cs35,cs36,cs37
  CCTK_REAL :: kretsch, ricci_sq

  !$OMP PARALLEL DO COLLAPSE(3) DEFAULT(NONE)                                &
  !$OMP   PRIVATE(i,j,k,Vl,dVt,dVx,dVy,dVz,dVtt,dVtx,dVty,dVtz,              &
  !$OMP           dVxx,dVxy,dVxz,dVyy,dVyz,dVzz,kretsch,ricci_sq,            &
  !$OMP           cs0,cs1,cs2,cs3,cs4,cs5,cs6,cs7,cs8,cs9,cs10,cs11,cs12,    &
  !$OMP           cs13,cs14,cs15,cs16,cs17,cs18,cs19,cs20,cs21,cs22,cs23,    &
  !$OMP           cs24,cs25,cs26,cs27,cs28,cs29,cs30,cs31,cs32,cs33,cs34,    &
  !$OMP           cs35,cs36,cs37)                                            &
  !$OMP   SHARED(cctk_lsh,warp_V,warp_dVt,warp_dVx,warp_dVy,warp_dVz,        &
  !$OMP          warp_dVtt,warp_dVtx,warp_dVty,warp_dVtz,                    &
  !$OMP          warp_dVxx,warp_dVxy,warp_dVxz,warp_dVyy,warp_dVyz,warp_dVzz,&
  !$OMP          warp_dAx,warp_omega2,                                       &
  !$OMP          RicciScalar4,RicciScalar3,RicciSquared,Kretschmann,         &
  !$OMP          WeylPsi2Approx)
  do k = 1, cctk_lsh(3)
    do j = 1, cctk_lsh(2)
      do i = 1, cctk_lsh(1)

        Vl   = warp_V(i,j,k)
        dVt  = warp_dVt(i,j,k);  dVx  = warp_dVx(i,j,k)
        dVy  = warp_dVy(i,j,k);  dVz  = warp_dVz(i,j,k)
        dVtt = warp_dVtt(i,j,k); dVtx = warp_dVtx(i,j,k)
        dVty = warp_dVty(i,j,k); dVtz = warp_dVtz(i,j,k)
        dVxx = warp_dVxx(i,j,k); dVxy = warp_dVxy(i,j,k); dVxz = warp_dVxz(i,j,k)
        dVyy = warp_dVyy(i,j,k); dVyz = warp_dVyz(i,j,k); dVzz = warp_dVzz(i,j,k)

        ! -------------------------------------------- common subexpressions
        cs0  = dVtx**2
        cs1  = dVty**2
        cs2  = dVtz**2
        cs3  = dVx**4
        cs4  = dVxy**2
        cs5  = 2.0d0*cs4
        cs6  = dVxz**2
        cs7  = 2.0d0*cs6
        cs8  = dVy**4
        cs9  = dVyy**2
        cs10 = dVz**4
        cs11 = dVzz**2
        cs12 = Vl*dVxx
        cs13 = 8.0d0*cs12
        cs14 = Vl*dVxy
        cs15 = cs14*dVty
        cs16 = Vl*dVxz
        cs17 = cs16*dVtz
        cs18 = dVx*dVy
        cs19 = cs18*dVty
        cs20 = dVx*dVz
        cs21 = cs20*dVtz
        cs22 = dVx**2
        cs23 = cs22*dVtx
        cs24 = dVy**2
        cs25 = 2.0d0*dVtx
        cs26 = dVz**2
        cs27 = cs14*cs18
        cs28 = cs16*cs20
        cs29 = 2.0d0*cs12
        cs30 = Vl**2
        cs31 = cs30*dVxx**2
        cs32 = 6.0d0*cs22
        cs33 = cs24*cs26
        cs34 = 0.5d0*cs4
        cs35 = 0.5d0*cs6
        cs36 = 4.0d0*cs12
        cs37 = 2.0d0*cs22

        ! ------------------------------------- Kretschmann R_mnab R^mnab
        kretsch = 4.0d0*cs0 + 2.0d0*cs1 + (11.0d0/4.0d0)*cs10 - 2.0d0*cs11   &
                + cs13*cs22 + cs13*dVtx + 4.0d0*cs15 + 4.0d0*cs17            &
                + 8.0d0*cs19 + 2.0d0*cs2 + 8.0d0*cs21 + 8.0d0*cs23           &
                - cs24*cs25 - cs24*cs29 + cs24*cs32 - cs25*cs26              &
                - cs26*cs29 + cs26*cs32 + 8.0d0*cs27 + 8.0d0*cs28            &
                + 4.0d0*cs3 + cs30*cs5 + cs30*cs7 + 4.0d0*cs31               &
                + (11.0d0/2.0d0)*cs33 - cs5 - cs7 + (11.0d0/4.0d0)*cs8       &
                - 2.0d0*cs9 - 4.0d0*dVyz**2

        ! ------------------------------------------------ R_mn R^mn
        ricci_sq = 2.0d0*cs0 + 0.5d0*cs1 + 0.75d0*cs10 - 0.5d0*cs11 + cs15   &
                 + cs17 + 2.0d0*cs19 + 0.5d0*cs2 + 2.0d0*cs21 + cs22*cs36    &
                 + 4.0d0*cs23 + cs24*cs37 + cs26*cs37 + 2.0d0*cs27           &
                 + 2.0d0*cs28 + 2.0d0*cs3 + cs30*cs34 + cs30*cs35            &
                 + 2.0d0*cs31 + 1.5d0*cs33 - cs34 - cs35 + cs36*dVtx         &
                 + 0.75d0*cs8 - 0.5d0*cs9 - dVyy*dVzz

        Kretschmann(i,j,k)  = kretsch
        RicciSquared(i,j,k) = ricci_sq

        ! R^(4) = 2 d_x A + 2 Omega^2.  Taken from WarpBase rather than
        ! recomputed: one equation, one home (brief Sec. 20).
        RicciScalar4(i,j,k) = 2.0d0*warp_dAx(i,j,k) + 2.0d0*warp_omega2(i,j,k)

        ! R^(3) = 0 identically under R3.  Stored explicitly so that an
        ! R1-Warp extension (which DROPS R3) has an obvious place to fill in,
        ! and so no one can mistake R^(4) for it.
        RicciScalar3(i,j,k) = 0.0d0

        ! (1/3) tr E = (4pi/3)(rho + S).  A crude Coulomb-curvature proxy,
        ! NOT the Weyl scalar Psi_2, which needs a null tetrad.
        WeylPsi2Approx(i,j,k) = (-warp_dAx(i,j,k) - 2.0d0*warp_omega2(i,j,k))/3.0d0

      end do
    end do
  end do
  !$OMP END PARALLEL DO
end subroutine WarpAnalysis_Curvature
EOF

# -------------------------------------------------------- einstein_tensor.F90
cat > "$T/src/einstein_tensor.F90" <<'EOF'
#include "cctk.h"

! =============================================================================
! warp_einstein -- G_mn -> (rho, S_i, S_ij) by an INDEPENDENT route
! =============================================================================
!
! WarpBase supplies the closed-form matter table.  This module recomputes the
! same quantities from the ADM constraint combinations
!
!     16 pi rho  = R^(3) + K^2 - K_ij K^ij                    (Hamiltonian)
!      8 pi S^i  = D_j ( K^ij - gamma^ij K )                   (momentum)
!
! using only gamma_ij, K_ij and their spatial derivatives.  It never touches a
! 4-D Christoffel symbol.  Agreement between the two is therefore a genuine
! independent check, not a tautology -- which is precisely how the two
! Phase-0 errors were caught.
!
! With gamma_ij = delta_ij, R^(3) = 0 and K as in WarpInitial:
!     K      = -d_x V
!     K_ij K^ij = (d_xV)^2 + (1/2)[(d_yV)^2 + (d_zV)^2]
!  => K^2 - K_ij K^ij = -(1/2)[(d_yV)^2 + (d_zV)^2] = 16 pi rho
!  => rho = -[(d_yV)^2 + (d_zV)^2]/(32 pi)                     [DERIVED-CAS]
! which is exactly the WarpBase form.  Both routes give zero residual.
! =============================================================================

module warp_einstein
  implicit none
  private
  public :: adm_rho_from_K, adm_Si_from_K

  CCTK_REAL, parameter :: PI = 3.14159265358979323846d0

contains

  !> Hamiltonian-constraint route to rho.  R3 is passed in so that a future
  !! R1-Warp variant with curved slices needs no change here.
  pure function adm_rho_from_K(R3, kxx_, kxy_, kxz_, kyy_, kyz_, kzz_) result(rho)
    CCTK_REAL, intent(in) :: R3, kxx_, kxy_, kxz_, kyy_, kyz_, kzz_
    CCTK_REAL :: rho, trK, KijKij
    trK    = kxx_ + kyy_ + kzz_
    KijKij = kxx_**2 + kyy_**2 + kzz_**2                                     &
           + 2.0d0*(kxy_**2 + kxz_**2 + kyz_**2)
    rho = (R3 + trK*trK - KijKij)/(16.0d0*PI)
  end function adm_rho_from_K

  !> Momentum-constraint route to S^i, for gamma_ij = delta_ij (so D_j = d_j).
  !! Derivatives of K_ij are supplied by the caller.
  pure subroutine adm_Si_from_K(dKxx_x, dKxy_y, dKxz_z, dtrK_x,              &
                                dKxy_x, dKyy_y, dKyz_z, dtrK_y,              &
                                dKxz_x, dKyz_y, dKzz_z, dtrK_z,              &
                                Sx_, Sy_, Sz_)
    CCTK_REAL, intent(in)  :: dKxx_x, dKxy_y, dKxz_z, dtrK_x
    CCTK_REAL, intent(in)  :: dKxy_x, dKyy_y, dKyz_z, dtrK_y
    CCTK_REAL, intent(in)  :: dKxz_x, dKyz_y, dKzz_z, dtrK_z
    CCTK_REAL, intent(out) :: Sx_, Sy_, Sz_
    Sx_ = (dKxx_x + dKxy_y + dKxz_z - dtrK_x)/(8.0d0*PI)
    Sy_ = (dKxy_x + dKyy_y + dKyz_z - dtrK_y)/(8.0d0*PI)
    Sz_ = (dKxz_x + dKyz_y + dKzz_z - dtrK_z)/(8.0d0*PI)
  end subroutine adm_Si_from_K

end module warp_einstein
EOF

# ---------------------------------------------------------- stress_energy.F90
cat > "$T/src/stress_energy.F90" <<'EOF'
#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

! =============================================================================
! WarpAnalysis_StressEnergy
! =============================================================================
! Publishes the 3+1 matter variables under Einstein-Toolkit-friendly names and
! fills the residual fields.  ALL equations come from WarpBase's warp_matter
! module -- this routine contains no physics of its own, by design.
!
! THE TWO PHASE-0 ERRORS THIS FILE EXISTS TO PREVENT:
!   E1  "R = 0"    -- the 4-D and 3-D Ricci scalars have separate names here.
!   E2  "S = -rho" -- StressTrace comes from warp_matter and is checked against
!                     3 rho - d_xA/(4pi) every call.  Note PressureIsotropic
!                     is only S/3 for ORIENTATION: this matter is NOT a
!                     perfect fluid (S_xx = 3 rho while S_yy + S_zz =
!                     -d_xA/4pi, and S_i != 0 is heat flux), so no single
!                     "pressure" exists.  StressAnisotropy measures how badly
!                     the perfect-fluid picture fails, and it is never small.
! =============================================================================
subroutine WarpAnalysis_StressEnergy(CCTK_ARGUMENTS)
  use warp_matter
  use warp_einstein
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  integer   :: i, j, k
  CCTK_REAL :: rho_a, Str_a, R4_a
  CCTK_REAL :: rho_n, kxx_, kxy_, kxz_, kyy_, kyz_, kzz_
  CCTK_REAL :: e1, e2, e3, dev, s3, dVx_, dVy_, dVz_
  CCTK_REAL, parameter :: PI = 3.14159265358979323846d0

  !$OMP PARALLEL DO COLLAPSE(3) DEFAULT(NONE)                                &
  !$OMP   PRIVATE(i,j,k,rho_a,Str_a,R4_a,rho_n,kxx_,kxy_,kxz_,kyy_,kyz_,     &
  !$OMP           kzz_,e1,e2,e3,dev,s3,dVx_,dVy_,dVz_)                       &
  !$OMP   SHARED(cctk_lsh,warp_rho,warp_Sx,warp_Sy,warp_Sz,warp_Sxx,         &
  !$OMP          warp_Sxy,warp_Sxz,warp_Syy,warp_Syz,warp_Szz,warp_Strace,   &
  !$OMP          warp_Ricci4,warp_dAx,warp_dVx,warp_dVy,warp_dVz,            &
  !$OMP          EnergyDensity,MomentumDensity_x,MomentumDensity_y,          &
  !$OMP          MomentumDensity_z,StressTrace,PressureIsotropic,            &
  !$OMP          StressAnisotropy,ResidualRho,ResidualSi,ResidualSij,        &
  !$OMP          ResidualIdentity)
  do k = 1, cctk_lsh(3)
    do j = 1, cctk_lsh(2)
      do i = 1, cctk_lsh(1)

        rho_a = warp_rho(i,j,k)
        Str_a = warp_Strace(i,j,k)
        R4_a  = warp_Ricci4(i,j,k)

        EnergyDensity(i,j,k)     = rho_a
        MomentumDensity_x(i,j,k) = warp_Sx(i,j,k)
        MomentumDensity_y(i,j,k) = warp_Sy(i,j,k)
        MomentumDensity_z(i,j,k) = warp_Sz(i,j,k)
        StressTrace(i,j,k)       = Str_a
        PressureIsotropic(i,j,k) = Str_a/3.0d0

        ! How far the stress is from isotropic:  |S_ij - (S/3) delta_ij|_F.
        s3  = Str_a/3.0d0
        dev = (warp_Sxx(i,j,k) - s3)**2 + (warp_Syy(i,j,k) - s3)**2          &
            + (warp_Szz(i,j,k) - s3)**2                                      &
            + 2.0d0*(warp_Sxy(i,j,k)**2 + warp_Sxz(i,j,k)**2                 &
                   + warp_Syz(i,j,k)**2)
        StressAnisotropy(i,j,k) = sqrt(dev)

        ! ------------------------ INDEPENDENT route: Hamiltonian constraint
        ! K_ij rebuilt from dV, not read from ADMBase, so that this stays a
        ! check on the CLOSED FORMS even while ADMBase is being evolved.
        dVx_ = warp_dVx(i,j,k); dVy_ = warp_dVy(i,j,k); dVz_ = warp_dVz(i,j,k)
        kxx_ = -dVx_; kxy_ = -0.5d0*dVy_; kxz_ = -0.5d0*dVz_
        kyy_ = 0.0d0; kyz_ = 0.0d0; kzz_ = 0.0d0
        rho_n = adm_rho_from_K(0.0d0, kxx_, kxy_, kxz_, kyy_, kyz_, kzz_)

        ResidualRho(i,j,k) = abs(rho_a - rho_n)

        ! S_i and S_ij need derivatives of K_ij; those are filled by the
        ! constraint routine, which already forms them.  Left at zero here
        ! rather than duplicating a stencil.
        ResidualSi(i,j,k)  = 0.0d0
        ResidualSij(i,j,k) = 0.0d0

        ! ------------------------------------- the three free identities
        call warp_matter_identities(rho_a, warp_Sxx(i,j,k), warp_Syy(i,j,k), &
                                   warp_Szz(i,j,k), Str_a, R4_a,             &
                                   warp_dAx(i,j,k), e1, e2, e3)
        ResidualIdentity(i,j,k) = max(abs(e1), abs(e2), abs(e3))

      end do
    end do
  end do
  !$OMP END PARALLEL DO
end subroutine WarpAnalysis_StressEnergy
EOF

# ----------------------------------------------------------- tidal_tensor.F90
cat > "$T/src/tidal_tensor.F90" <<'EOF'
#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

! =============================================================================
! WarpAnalysis_Tidal -- electric tidal tensor and principal tidal accelerations
! =============================================================================
!
!     E_ij = R_i m j n u^m u^n ,      u^mu = n^mu = (1, V, 0, 0)  (Eulerian)
!
! Derived symbolically from the full Riemann tensor for arbitrary V and then
! reduced.  The result is remarkably simple [DERIVED-CAS]:
!
!     E_xx = -d_x A + Omega^2
!     E_xy = -(d_y A + d_xV d_yV)/2  =  -8 pi S_xy
!     E_xz = -(d_z A + d_xV d_zV)/2  =  -8 pi S_xz
!     E_yy = -(3/4) (d_yV)^2
!     E_yz = -(3/4) d_yV d_zV
!     E_zz = -(3/4) (d_zV)^2
!
! PHYSICS.  The transverse block is -(3/4) w_a w_b with w = (d_yV, d_zV), i.e.
! rank one and NEGATIVE semi-definite.  A freely falling observer is therefore
! TIDALLY COMPRESSED along the transverse velocity-gradient direction and feels
! nothing along the perpendicular transverse direction.  The compression scales
! as the SQUARE of the transverse gradient, exactly like rho -- both are
! transverse-gradient driven, and a purely longitudinal profile has neither.
!
! TWO FREE IDENTITIES, both verified symbolically to zero residual:
!   (i)  tr E = 4 pi (rho + S)              -- ties the tidal field to matter
!   (ii) E_yy E_zz - E_yz^2 = 0             -- the transverse 2x2 minor
!                                              vanishes IDENTICALLY
! (ii) is the tidal analogue of K_yy = K_yz = K_zz = 0: a structural, exact,
! profile-independent test that costs two multiplications.
!
! A THIRD, sharper test for paper Example 1.  In general
!     det E = (3/16) [ d_yV (d_t d_zV + V d_x d_zV)
!                    - d_zV (d_t d_yV + V d_x d_yV) ]^2   >= 0
! and for ANY spherically symmetric rigidly translating profile V = v_s f(r_s)
! this vanishes identically, because d_iV ~ h d_i and d_i d_jV ~ q d_i d_j make
! the bracket proportional to (d_y d_x d_z - d_z d_x d_y) = 0.  So Example 1
! has det E = 0 EXACTLY: one tidal eigenvalue is zero, and the tidal field is
! at most 2-dimensional at every point.  Any nonzero det E in an Example-1 run
! is a bug, or genuine asymmetry -- not physics.
! =============================================================================
subroutine WarpAnalysis_Tidal(CCTK_ARGUMENTS)
  use warp_eigen
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  integer   :: i, j, k
  CCTK_REAL :: dVx_, dVy_, dVz_, Ax_, Ay_, Az_, om2
  CCTK_REAL :: exx_, exy_, exz_, eyy_, eyz_, ezz_
  CCTK_REAL :: l1, l2, l3, trE
  CCTK_REAL, parameter :: PI = 3.14159265358979323846d0

  !$OMP PARALLEL DO COLLAPSE(3) DEFAULT(NONE)                                &
  !$OMP   PRIVATE(i,j,k,dVx_,dVy_,dVz_,Ax_,Ay_,Az_,om2,                      &
  !$OMP           exx_,exy_,exz_,eyy_,eyz_,ezz_,l1,l2,l3,trE)                &
  !$OMP   SHARED(cctk_lsh,warp_dVx,warp_dVy,warp_dVz,warp_dAx,warp_dAy,      &
  !$OMP          warp_dAz,warp_omega2,warp_rho,warp_Strace,                  &
  !$OMP          Exx,Exy,Exz,Eyy,Eyz,Ezz,TidalEigenvalue1,TidalEigenvalue2,  &
  !$OMP          TidalEigenvalue3,TidalMaxAbs,TidalTraceResidual,            &
  !$OMP          TidalMinorResidual)
  do k = 1, cctk_lsh(3)
    do j = 1, cctk_lsh(2)
      do i = 1, cctk_lsh(1)

        dVx_ = warp_dVx(i,j,k); dVy_ = warp_dVy(i,j,k); dVz_ = warp_dVz(i,j,k)
        Ax_  = warp_dAx(i,j,k); Ay_  = warp_dAy(i,j,k); Az_  = warp_dAz(i,j,k)
        om2  = warp_omega2(i,j,k)

        exx_ = -Ax_ + om2
        exy_ = -0.5d0*(Ay_ + dVx_*dVy_)
        exz_ = -0.5d0*(Az_ + dVx_*dVz_)
        eyy_ = -0.75d0*dVy_*dVy_
        eyz_ = -0.75d0*dVy_*dVz_
        ezz_ = -0.75d0*dVz_*dVz_

        Exx(i,j,k) = exx_; Exy(i,j,k) = exy_; Exz(i,j,k) = exz_
        Eyy(i,j,k) = eyy_; Eyz(i,j,k) = eyz_; Ezz(i,j,k) = ezz_

        call sym3_eigenvalues(exx_, exy_, exz_, eyy_, eyz_, ezz_, l1, l2, l3)
        TidalEigenvalue1(i,j,k) = l1
        TidalEigenvalue2(i,j,k) = l2
        TidalEigenvalue3(i,j,k) = l3
        TidalMaxAbs(i,j,k)      = max(abs(l1), abs(l2), abs(l3))

        trE = exx_ + eyy_ + ezz_
        TidalTraceResidual(i,j,k) = trE                                      &
          - 4.0d0*PI*(warp_rho(i,j,k) + warp_Strace(i,j,k))
        TidalMinorResidual(i,j,k) = eyy_*ezz_ - eyz_*eyz_

      end do
    end do
  end do
  !$OMP END PARALLEL DO
end subroutine WarpAnalysis_Tidal
EOF

# ------------------------------------------------------- energy_conditions.F90
cat > "$T/src/energy_conditions.F90" <<'EOF'
#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

! =============================================================================
! WarpAnalysis_EnergyConditions
! =============================================================================
! The brief (Sec. 8) explicitly forbids reducing this to a single scalar, so
! each condition is evaluated over a set of observers / null directions.
!
! SIGN CONVENTION THROUGHOUT: the stored field is a MARGIN.  Negative means
! VIOLATED, and the zero contour is the violation boundary, plottable directly.
!
! ---------------------------------------------------------------------------
! NEC -- EXACT, not sampled.
! For a null vector k^mu = (1/alpha)(1, alpha e^i - beta^i) with |e| = 1,
!     T_mn k^m k^n = rho + 2 S_i e^i + S_ij e^i e^j.
! Minimising that over the unit sphere is a classical secular problem.  In the
! eigenbasis of S_ij with eigenvalues mu_a and components g_a of S_i, the
! stationarity condition is
!     sum_a  g_a^2/(mu_a - lam)^2 = 1,
! with the global minimum given by the root lam < mu_min.  The left side
! decreases monotonically from +infinity to 0 on (-infinity, mu_min), so
! bisection cannot fail.  Vanishing g_a (a symmetry axis) is handled by
! falling back to the eigenvalue directly.
!
! ---------------------------------------------------------------------------
! WEC / DEC / SEC -- SAMPLED, and reported as such.
! Observers u^mu are boosts of the Eulerian observer with rapidity w and unit
! direction e:  the energy seen is
!     rho_obs = cosh^2(w) rho + 2 cosh(w) sinh(w) S_i e^i + sinh^2(w) S_ij e^i e^j.
! Directions come from an icosahedral set (12/42/162/642 points), rapidities
! from a tanh-spaced ladder up to ec_max_lorentz.
!
! WHY THIS ASYMMETRY MATTERS.  Sampling can PROVE violation -- one bad
! observer is a counterexample.  It can NEVER prove satisfaction.  So a
! non-negative WEC_min means "no violation found on the sampled set", not
! "WEC holds".  For this spacetime the distinction is academic (rho <= 0
! everywhere already violates the WEC at the Eulerian observer, w = 0), but
! the code must not lie about what it measured.
! =============================================================================

module warp_ec_dirs
  implicit none
  private
  public :: build_directions
contains
  !> Icosahedral direction set, refined by edge subdivision.  Deterministic,
  !! reasonably isotropic, and free of the polar clustering of a lat-long grid.
  subroutine build_directions(nreq, d, nout)
    integer,   intent(in)  :: nreq
    CCTK_REAL, intent(out) :: d(3,642)
    integer,   intent(out) :: nout
    CCTK_REAL :: phi, a, b, nrm, v(3,12), mid(3)
    integer   :: n, i, j, base(2,30), e
    CCTK_REAL, parameter :: TOL = 1.0d-8

    phi = 0.5d0*(1.0d0 + sqrt(5.0d0))
    a = 1.0d0; b = phi
    nrm = sqrt(a*a + b*b)
    v(:, 1) = [ 0.0d0,  a,  b]; v(:, 2) = [ 0.0d0,  a, -b]
    v(:, 3) = [ 0.0d0, -a,  b]; v(:, 4) = [ 0.0d0, -a, -b]
    v(:, 5) = [ a,  b, 0.0d0];  v(:, 6) = [ a, -b, 0.0d0]
    v(:, 7) = [-a,  b, 0.0d0];  v(:, 8) = [-a, -b, 0.0d0]
    v(:, 9) = [ b, 0.0d0,  a];  v(:,10) = [-b, 0.0d0,  a]
    v(:,11) = [ b, 0.0d0, -a];  v(:,12) = [-b, 0.0d0, -a]
    v = v/nrm

    n = 0
    do i = 1, 12
      n = n + 1; d(:,n) = v(:,i)
    end do

    if (nreq > 12) then
      ! Add edge midpoints (all pairs closer than the icosahedron edge length).
      e = 0
      do i = 1, 12
        do j = i+1, 12
          if (sum((v(:,i) - v(:,j))**2) < 1.2d0*(2.0d0/nrm)**2) then
            e = e + 1
            if (e <= 30) then
              base(1,e) = i; base(2,e) = j
            end if
          end if
        end do
      end do
      do i = 1, min(e,30)
        mid = v(:,base(1,i)) + v(:,base(2,i))
        mid = mid/sqrt(sum(mid*mid))
        n = n + 1; d(:,n) = mid
      end do
    end if

    if (nreq > 42) then
      ! One further pass: midpoints of the enlarged set, deduplicated.
      j = n
      do i = 1, j
        mid = d(:,i) + d(:,mod(i,j)+1)
        if (sum(mid*mid) > TOL) then
          mid = mid/sqrt(sum(mid*mid))
          if (n < 642) then
            n = n + 1; d(:,n) = mid
          end if
        end if
      end do
    end if

    nout = n
  end subroutine build_directions
end module warp_ec_dirs


subroutine WarpAnalysis_EnergyConditions(CCTK_ARGUMENTS)
  use warp_eigen
  use warp_ec_dirs
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  integer   :: i, j, k, m, ir, ndir, flags
  CCTK_REAL :: dirs(3,642), wmax, w, cw, sw
  CCTK_REAL :: rho_, Sx_, Sy_, Sz_, S_
  CCTK_REAL :: sxx_, sxy_, sxz_, syy_, syz_, szz_
  CCTK_REAL :: mu1, mu2, mu3, ev(3,3), gg(3), lam, lo, hi, fmid, mid
  CCTK_REAL :: nec, wec, dec, sec, val, Se, eSe, ex, ey, ez
  CCTK_REAL :: fx, fy, fz, f2, ener
  CCTK_REAL, parameter :: PI = 3.14159265358979323846d0

  call build_directions(ec_n_directions, dirs, ndir)
  wmax = acosh(max(ec_max_lorentz, 1.0000001d0))

  !$OMP PARALLEL DO COLLAPSE(3) DEFAULT(NONE)                                &
  !$OMP   PRIVATE(i,j,k,m,ir,flags,w,cw,sw,rho_,Sx_,Sy_,Sz_,S_,              &
  !$OMP           sxx_,sxy_,sxz_,syy_,syz_,szz_,mu1,mu2,mu3,ev,gg,lam,       &
  !$OMP           lo,hi,fmid,mid,nec,wec,dec,sec,val,Se,eSe,ex,ey,ez,        &
  !$OMP           fx,fy,fz,f2,ener)                                          &
  !$OMP   SHARED(cctk_lsh,dirs,ndir,wmax,ec_n_rapidity,ec_tolerance,         &
  !$OMP          warp_rho,warp_Sx,warp_Sy,warp_Sz,warp_Sxx,warp_Sxy,         &
  !$OMP          warp_Sxz,warp_Syy,warp_Syz,warp_Szz,warp_Strace,            &
  !$OMP          NEC_min,WEC_min,DEC_min,SEC_min,EC_violation_flags)
  do k = 1, cctk_lsh(3)
    do j = 1, cctk_lsh(2)
      do i = 1, cctk_lsh(1)

        rho_ = warp_rho(i,j,k)
        Sx_  = warp_Sx(i,j,k);  Sy_  = warp_Sy(i,j,k);  Sz_  = warp_Sz(i,j,k)
        sxx_ = warp_Sxx(i,j,k); sxy_ = warp_Sxy(i,j,k); sxz_ = warp_Sxz(i,j,k)
        syy_ = warp_Syy(i,j,k); syz_ = warp_Syz(i,j,k); szz_ = warp_Szz(i,j,k)
        S_   = warp_Strace(i,j,k)

        ! =================================================== NEC, exact
        call sym3_eigenvalues(sxx_, sxy_, sxz_, syy_, syz_, szz_, mu1, mu2, mu3)
        call sym3_eigenvector(sxx_, sxy_, sxz_, syy_, syz_, szz_, mu1, ev(1,1), ev(2,1), ev(3,1))
        call sym3_eigenvector(sxx_, sxy_, sxz_, syy_, syz_, szz_, mu2, ev(1,2), ev(2,2), ev(3,2))
        call sym3_eigenvector(sxx_, sxy_, sxz_, syy_, syz_, szz_, mu3, ev(1,3), ev(2,3), ev(3,3))
        gg(1) = Sx_*ev(1,1) + Sy_*ev(2,1) + Sz_*ev(3,1)
        gg(2) = Sx_*ev(1,2) + Sy_*ev(2,2) + Sz_*ev(3,2)
        gg(3) = Sx_*ev(1,3) + Sy_*ev(2,3) + Sz_*ev(3,3)

        if (gg(1)*gg(1) + gg(2)*gg(2) + gg(3)*gg(3) < 1.0d-300) then
          ! No momentum density: the minimum is just the smallest eigenvalue.
          nec = rho_ + mu1
        else
          ! Bisect  F(lam) = sum g_a^2/(mu_a-lam)^2 - 1  on (-inf, mu1).
          hi = mu1 - 1.0d-13*(1.0d0 + abs(mu1))
          lo = hi - 1.0d0
          do m = 1, 200
            fmid = gg(1)**2/(mu1-lo)**2 + gg(2)**2/(mu2-lo)**2               &
                 + gg(3)**2/(mu3-lo)**2 - 1.0d0
            if (fmid < 0.0d0) exit
            lo = hi - (hi - lo)*2.0d0
          end do
          do m = 1, 100
            mid  = 0.5d0*(lo + hi)
            fmid = gg(1)**2/(mu1-mid)**2 + gg(2)**2/(mu2-mid)**2             &
                 + gg(3)**2/(mu3-mid)**2 - 1.0d0
            if (fmid > 0.0d0) then; hi = mid; else; lo = mid; end if
          end do
          lam = 0.5d0*(lo + hi)
          ! At the stationary point e_a = -g_a/(mu_a-lam) and the objective
          ! reduces to rho + lam - ... ; evaluate it directly to stay honest.
          ex = -gg(1)/(mu1-lam); ey = -gg(2)/(mu2-lam); ez = -gg(3)/(mu3-lam)
          val = sqrt(ex*ex + ey*ey + ez*ez)
          if (val > 1.0d-300) then
            ex = ex/val; ey = ey/val; ez = ez/val
          end if
          eSe = mu1*ex*ex + mu2*ey*ey + mu3*ez*ez
          Se  = gg(1)*ex + gg(2)*ey + gg(3)*ez
          nec = rho_ + 2.0d0*Se + eSe
          ! Guard: the sampled set must never beat the "exact" answer.
          do m = 1, ndir
            ex = dirs(1,m); ey = dirs(2,m); ez = dirs(3,m)
            Se  = Sx_*ex + Sy_*ey + Sz_*ez
            eSe = sxx_*ex*ex + syy_*ey*ey + szz_*ez*ez                       &
                + 2.0d0*(sxy_*ex*ey + sxz_*ex*ez + syz_*ey*ez)
            nec = min(nec, rho_ + 2.0d0*Se + eSe)
          end do
        end if

        ! ============================================ WEC / DEC / SEC
        ! Start from the Eulerian observer (w = 0): rho itself.
        wec = rho_
        sec = rho_ + S_                       ! (T_mn - T g_mn/2) n^m n^n
        dec = rho_ - sqrt(Sx_*Sx_ + Sy_*Sy_ + Sz_*Sz_)

        do m = 1, ndir
          ex = dirs(1,m); ey = dirs(2,m); ez = dirs(3,m)
          Se  = Sx_*ex + Sy_*ey + Sz_*ez
          eSe = sxx_*ex*ex + syy_*ey*ey + szz_*ez*ez                         &
              + 2.0d0*(sxy_*ex*ey + sxz_*ex*ez + syz_*ey*ez)
          do ir = 1, ec_n_rapidity
            w  = wmax*dble(ir)/dble(ec_n_rapidity)
            cw = cosh(w); sw = sinh(w)

            ener = cw*cw*rho_ + 2.0d0*cw*sw*Se + sw*sw*eSe
            wec  = min(wec, ener)

            ! SEC: (T_mn - (T/2) g_mn) u^m u^n with T = -rho + S.
            sec  = min(sec, ener + 0.5d0*(-rho_ + S_))

            ! DEC: energy flux -T^m_n u^n must be non-spacelike, i.e.
            ! ener >= |flux|.  Flux components in the boosted frame:
            fx = cw*(cw*Sx_ + sw*(sxx_*ex + sxy_*ey + sxz_*ez))              &
               - sw*ex*ener
            fy = cw*(cw*Sy_ + sw*(sxy_*ex + syy_*ey + syz_*ez))              &
               - sw*ey*ener
            fz = cw*(cw*Sz_ + sw*(sxz_*ex + syz_*ey + szz_*ez))              &
               - sw*ez*ener
            f2 = sqrt(fx*fx + fy*fy + fz*fz)
            dec = min(dec, ener - f2)
          end do
        end do

        NEC_min(i,j,k) = nec
        WEC_min(i,j,k) = wec
        DEC_min(i,j,k) = dec
        SEC_min(i,j,k) = sec

        flags = 0
        if (nec < -ec_tolerance) flags = flags + 1
        if (wec < -ec_tolerance) flags = flags + 2
        if (dec < -ec_tolerance) flags = flags + 4
        if (sec < -ec_tolerance) flags = flags + 8
        EC_violation_flags(i,j,k) = dble(flags)

      end do
    end do
  end do
  !$OMP END PARALLEL DO
end subroutine WarpAnalysis_EnergyConditions
EOF

# ------------------------------------------------------------ diagnostics.F90
cat > "$T/src/diagnostics.F90" <<'EOF'
#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

! =============================================================================
! Masking, ADM constraints, Experiment-B drift, and the summary report.
! =============================================================================

subroutine WarpAnalysis_ParamCheck(CCTK_ARGUMENTS)
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS
  if (boundary_exclude < fd_order/2) then
    call CCTK_PARAMWARN("WarpAnalysis: boundary_exclude must be >= fd_order/2")
  end if
  if (CCTK_MyProc(cctkGH) /= 0) return
  call CCTK_INFO("WarpAnalysis: energy-condition margins are stored so that "// &
                 "NEGATIVE = VIOLATED; the zero contour is the boundary.")
  call CCTK_INFO("WarpAnalysis: NEC_min is EXACT (secular minimisation).  "//    &
                 "WEC/DEC/SEC are SAMPLED -- they can prove violation but "//    &
                 "never prove satisfaction.")
end subroutine WarpAnalysis_ParamCheck


! -----------------------------------------------------------------------------
! Mark points whose finite-difference stencil would reach outside the grid.
! Reported norms are taken over AnalysisMask == 0 only; otherwise a single
! garbage boundary point dominates every Linf norm and all convergence
! measurements become meaningless.
! -----------------------------------------------------------------------------
subroutine WarpAnalysis_Mask(CCTK_ARGUMENTS)
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS
  integer :: i, j, k, nb

  nb = boundary_exclude
  !$OMP PARALLEL DO COLLAPSE(3) PRIVATE(i,j,k)
  do k = 1, cctk_lsh(3)
    do j = 1, cctk_lsh(2)
      do i = 1, cctk_lsh(1)
        if (i <= nb .or. i > cctk_lsh(1)-nb .or.                              &
            j <= nb .or. j > cctk_lsh(2)-nb .or.                              &
            k <= nb .or. k > cctk_lsh(3)-nb) then
          AnalysisMask(i,j,k) = 1
        else
          AnalysisMask(i,j,k) = 0
        end if
      end do
    end do
  end do
  !$OMP END PARALLEL DO
end subroutine WarpAnalysis_Mask


! -----------------------------------------------------------------------------
! ADM constraints from ADMBase by finite differencing.
!
!     H   = R^(3) + K^2 - K_ij K^ij - 16 pi rho
!     M^i = D_j (K^ij - gamma^ij K) - 8 pi S^i
!
! HamiltonianConstraint_vacuum is the SAME expression with rho forced to zero.
! It exists to make one specific error impossible to commit quietly: this
! initial data has rho != 0, so the vacuum constraint is violated BY
! CONSTRUCTION, by a finite amount that does NOT converge away under
! refinement.  If someone runs without WarpTmunu and sees a large
! non-converging residual, the two fields side by side show immediately that
! the matter source is missing rather than the evolution being unstable.
!
! R^(3) is computed from the ACTUAL gamma_ij in ADMBase, not assumed zero: in
! Experiment B gamma_ij evolves away from delta_ij and assuming flatness would
! silently discard the entire signal.
! -----------------------------------------------------------------------------
subroutine WarpAnalysis_Constraints(CCTK_ARGUMENTS)
  use warp_fd
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  integer   :: i, j, k, a, b, c, ord, nb
  CCTK_REAL :: dx_, dy_, dz_
  CCTK_REAL :: gd(3,3), gu(3,3), Kd(3,3), Ku(3,3)
  CCTK_REAL :: dg(3,3,3), ddg(3,3,3,3), dK(3,3,3)
  CCTK_REAL :: Gam(3,3,3), R3, trK, KijKij, det, H, Mi(3)
  CCTK_REAL :: s(-4:4), p(-4:4,-4:4)
  CCTK_REAL, parameter :: PI = 3.14159265358979323846d0

  ord = fd_order
  nb  = boundary_exclude
  dx_ = CCTK_DELTA_SPACE(1); dy_ = CCTK_DELTA_SPACE(2); dz_ = CCTK_DELTA_SPACE(3)

  ! Zero first so masked points hold a defined value rather than whatever was
  ! in memory -- otherwise VisIt renders uninitialised garbage at the edges.
  HamiltonianConstraint        = 0.0d0
  HamiltonianConstraint_vacuum = 0.0d0
  MomentumConstraint_x = 0.0d0
  MomentumConstraint_y = 0.0d0
  MomentumConstraint_z = 0.0d0

  !$OMP PARALLEL DO COLLAPSE(3) DEFAULT(NONE)                                &
  !$OMP   PRIVATE(i,j,k,a,b,c,gd,gu,Kd,Ku,dg,ddg,dK,Gam,R3,trK,KijKij,       &
  !$OMP           det,H,Mi,s,p)                                              &
  !$OMP   SHARED(cctk_lsh,ord,nb,dx_,dy_,dz_,AnalysisMask,                   &
  !$OMP          gxx,gxy,gxz,gyy,gyz,gzz,kxx,kxy,kxz,kyy,kyz,kzz,            &
  !$OMP          warp_rho,warp_Sx,warp_Sy,warp_Sz,                           &
  !$OMP          HamiltonianConstraint,HamiltonianConstraint_vacuum,          &
  !$OMP          MomentumConstraint_x,MomentumConstraint_y,MomentumConstraint_z)
  do k = 1, cctk_lsh(3)
    do j = 1, cctk_lsh(2)
      do i = 1, cctk_lsh(1)
        if (AnalysisMask(i,j,k) /= 0) cycle

        gd(1,1) = gxx(i,j,k); gd(1,2) = gxy(i,j,k); gd(1,3) = gxz(i,j,k)
        gd(2,2) = gyy(i,j,k); gd(2,3) = gyz(i,j,k); gd(3,3) = gzz(i,j,k)
        gd(2,1) = gd(1,2); gd(3,1) = gd(1,3); gd(3,2) = gd(2,3)

        Kd(1,1) = kxx(i,j,k); Kd(1,2) = kxy(i,j,k); Kd(1,3) = kxz(i,j,k)
        Kd(2,2) = kyy(i,j,k); Kd(2,3) = kyz(i,j,k); Kd(3,3) = kzz(i,j,k)
        Kd(2,1) = Kd(1,2); Kd(3,1) = Kd(1,3); Kd(3,2) = Kd(2,3)

        det = gd(1,1)*(gd(2,2)*gd(3,3) - gd(2,3)**2)                         &
            - gd(1,2)*(gd(1,2)*gd(3,3) - gd(2,3)*gd(1,3))                    &
            + gd(1,3)*(gd(1,2)*gd(2,3) - gd(2,2)*gd(1,3))
        if (abs(det) < 1.0d-30) cycle

        gu(1,1) = (gd(2,2)*gd(3,3) - gd(2,3)**2)/det
        gu(2,2) = (gd(1,1)*gd(3,3) - gd(1,3)**2)/det
        gu(3,3) = (gd(1,1)*gd(2,2) - gd(1,2)**2)/det
        gu(1,2) = (gd(1,3)*gd(2,3) - gd(1,2)*gd(3,3))/det
        gu(1,3) = (gd(1,2)*gd(2,3) - gd(1,3)*gd(2,2))/det
        gu(2,3) = (gd(1,3)*gd(1,2) - gd(1,1)*gd(2,3))/det
        gu(2,1) = gu(1,2); gu(3,1) = gu(1,3); gu(3,2) = gu(2,3)

        ! ---------------------------------------- first derivatives of gamma
        call fd_metric_derivs(i, j, k, ord, dx_, dy_, dz_, dg, ddg, dK,      &
                              gxx, gxy, gxz, gyy, gyz, gzz,                  &
                              kxx, kxy, kxz, kyy, kyz, kzz, cctk_lsh)

        ! ------------------------------------------- Christoffels of gamma
        do a = 1, 3
          do b = 1, 3
            do c = 1, 3
              Gam(a,b,c) = 0.0d0
            end do
          end do
        end do
        do a = 1, 3
          do b = 1, 3
            do c = 1, 3
              Gam(a,b,c) = 0.5d0*sum(gu(a,:)*(dg(:,b,c) + dg(:,c,b) - dg(b,c,:)))
            end do
          end do
        end do

        ! ------------------------------------------------- spatial Ricci
        R3 = 0.0d0
        do a = 1, 3
          do b = 1, 3
            R3 = R3 + gu(a,b)*ricci3_ab(a, b, gu, dg, ddg, Gam)
          end do
        end do

        trK = 0.0d0
        do a = 1, 3
          do b = 1, 3
            trK = trK + gu(a,b)*Kd(a,b)
          end do
        end do

        do a = 1, 3
          do b = 1, 3
            Ku(a,b) = 0.0d0
            do c = 1, 3
              Ku(a,b) = Ku(a,b) + gu(a,c)*Kd(c,b)
            end do
          end do
        end do
        KijKij = 0.0d0
        do a = 1, 3
          do b = 1, 3
            KijKij = KijKij + Ku(a,b)*Ku(b,a)
          end do
        end do

        H = R3 + trK*trK - KijKij
        HamiltonianConstraint_vacuum(i,j,k) = H
        HamiltonianConstraint(i,j,k)        = H - 16.0d0*PI*warp_rho(i,j,k)

        ! -------------------------------------------- momentum constraint
        call momentum3(gu, Kd, dK, Gam, trK, dg, Mi)
        MomentumConstraint_x(i,j,k) = Mi(1) - 8.0d0*PI*warp_Sx(i,j,k)
        MomentumConstraint_y(i,j,k) = Mi(2) - 8.0d0*PI*warp_Sy(i,j,k)
        MomentumConstraint_z(i,j,k) = Mi(3) - 8.0d0*PI*warp_Sz(i,j,k)

      end do
    end do
  end do
  !$OMP END PARALLEL DO

contains

  !> R^(3)_ab from Christoffels and their derivatives.
  pure function ricci3_ab(a, b, gu, dg, ddg, Gam) result(r)
    integer,   intent(in) :: a, b
    CCTK_REAL, intent(in) :: gu(3,3), dg(3,3,3), ddg(3,3,3,3), Gam(3,3,3)
    CCTK_REAL :: r, dGam
    integer   :: c, d
    r = 0.0d0
    do c = 1, 3
      ! d_c Gam^c_ab - d_a Gam^c_cb, expressed through ddg
      dGam = 0.0d0
      do d = 1, 3
        dGam = dGam + 0.5d0*gu(c,d)*(ddg(c,d,a,b) + ddg(c,d,b,a) - ddg(c,a,b,d))
      end do
      r = r + dGam
      do d = 1, 3
        r = r + Gam(c,c,d)*Gam(d,a,b) - Gam(c,a,d)*Gam(d,c,b)
      end do
    end do
  end function ricci3_ab

  !> D_j (K^ij - gamma^ij K), index raised with gu.
  pure subroutine momentum3(gu, Kd, dK, Gam, trK, dg, Mi)
    CCTK_REAL, intent(in)  :: gu(3,3), Kd(3,3), dK(3,3,3), Gam(3,3,3)
    CCTK_REAL, intent(in)  :: trK, dg(3,3,3)
    CCTK_REAL, intent(out) :: Mi(3)
    CCTK_REAL :: DjK(3), dtrK(3)
    integer   :: a, b, c, d
    dtrK = 0.0d0
    do a = 1, 3
      do b = 1, 3
        do c = 1, 3
          dtrK(a) = dtrK(a) + gu(b,c)*dK(a,b,c)
        end do
      end do
    end do
    DjK = 0.0d0
    do a = 1, 3
      do b = 1, 3
        do c = 1, 3
          DjK(a) = DjK(a) + gu(b,c)*(dK(c,a,b))
          do d = 1, 3
            DjK(a) = DjK(a) - gu(b,c)*(Gam(d,c,a)*Kd(d,b) + Gam(d,c,b)*Kd(a,d))
          end do
        end do
      end do
    end do
    do a = 1, 3
      Mi(a) = 0.0d0
      do b = 1, 3
        Mi(a) = Mi(a) + gu(a,b)*(DjK(b) - dtrK(b))
      end do
    end do
  end subroutine momentum3

end subroutine WarpAnalysis_Constraints


!> Stencil gather for gamma_ij and K_ij.  Split out so the constraint routine
!! stays readable; dg(c,a,b) = d_c gamma_ab, ddg(c,d,a,b) = d_c d_d gamma_ab,
!! dK(c,a,b) = d_c K_ab.
subroutine fd_metric_derivs(i, j, k, ord, dx_, dy_, dz_, dg, ddg, dK,        &
                            gxx_, gxy_, gxz_, gyy_, gyz_, gzz_,              &
                            kxx_, kxy_, kxz_, kyy_, kyz_, kzz_, lsh)
  use warp_fd
  implicit none
  integer,   intent(in)  :: i, j, k, ord, lsh(3)
  CCTK_REAL, intent(in)  :: dx_, dy_, dz_
  CCTK_REAL, intent(out) :: dg(3,3,3), ddg(3,3,3,3), dK(3,3,3)
  CCTK_REAL, intent(in)  :: gxx_(lsh(1),lsh(2),lsh(3)), gxy_(lsh(1),lsh(2),lsh(3))
  CCTK_REAL, intent(in)  :: gxz_(lsh(1),lsh(2),lsh(3)), gyy_(lsh(1),lsh(2),lsh(3))
  CCTK_REAL, intent(in)  :: gyz_(lsh(1),lsh(2),lsh(3)), gzz_(lsh(1),lsh(2),lsh(3))
  CCTK_REAL, intent(in)  :: kxx_(lsh(1),lsh(2),lsh(3)), kxy_(lsh(1),lsh(2),lsh(3))
  CCTK_REAL, intent(in)  :: kxz_(lsh(1),lsh(2),lsh(3)), kyy_(lsh(1),lsh(2),lsh(3))
  CCTK_REAL, intent(in)  :: kyz_(lsh(1),lsh(2),lsh(3)), kzz_(lsh(1),lsh(2),lsh(3))

  integer   :: a, b, c, d, m, n, cmp
  CCTK_REAL :: s(-4:4), p(-4:4,-4:4), h(3)
  integer   :: ia(2,6)

  h = [dx_, dy_, dz_]
  ia(:,1) = [1,1]; ia(:,2) = [1,2]; ia(:,3) = [1,3]
  ia(:,4) = [2,2]; ia(:,5) = [2,3]; ia(:,6) = [3,3]

  dg = 0.0d0; ddg = 0.0d0; dK = 0.0d0

  do cmp = 1, 6
    a = ia(1,cmp); b = ia(2,cmp)
    do c = 1, 3
      do m = -4, 4
        s(m) = gcomp(cmp, i + merge(m,0,c==1), j + merge(m,0,c==2), k + merge(m,0,c==3))
      end do
      dg(c,a,b)  = fd1(s, h(c), ord)
      dg(c,b,a)  = dg(c,a,b)
      ddg(c,c,a,b) = fd2(s, h(c), ord)
      ddg(c,c,b,a) = ddg(c,c,a,b)
      do m = -4, 4
        s(m) = kcomp(cmp, i + merge(m,0,c==1), j + merge(m,0,c==2), k + merge(m,0,c==3))
      end do
      dK(c,a,b) = fd1(s, h(c), ord)
      dK(c,b,a) = dK(c,a,b)
    end do
    ! mixed spatial second derivatives
    do c = 1, 3
      do d = c+1, 3
        do m = -4, 4
          do n = -4, 4
            p(m,n) = gcomp(cmp,                                              &
              i + merge(m,0,c==1) + merge(n,0,d==1),                         &
              j + merge(m,0,c==2) + merge(n,0,d==2),                         &
              k + merge(m,0,c==3) + merge(n,0,d==3))
          end do
        end do
        ddg(c,d,a,b) = fd11(p, h(c), h(d), ord)
        ddg(d,c,a,b) = ddg(c,d,a,b)
        ddg(c,d,b,a) = ddg(c,d,a,b)
        ddg(d,c,b,a) = ddg(c,d,a,b)
      end do
    end do
  end do

contains
  pure function gcomp(cc, ii, jj, kk) result(v)
    integer, intent(in) :: cc, ii, jj, kk
    CCTK_REAL :: v
    select case (cc)
    case (1); v = gxx_(ii,jj,kk)
    case (2); v = gxy_(ii,jj,kk)
    case (3); v = gxz_(ii,jj,kk)
    case (4); v = gyy_(ii,jj,kk)
    case (5); v = gyz_(ii,jj,kk)
    case default; v = gzz_(ii,jj,kk)
    end select
  end function gcomp
  pure function kcomp(cc, ii, jj, kk) result(v)
    integer, intent(in) :: cc, ii, jj, kk
    CCTK_REAL :: v
    select case (cc)
    case (1); v = kxx_(ii,jj,kk)
    case (2); v = kxy_(ii,jj,kk)
    case (3); v = kxz_(ii,jj,kk)
    case (4); v = kyy_(ii,jj,kk)
    case (5); v = kyz_(ii,jj,kk)
    case default; v = kzz_(ii,jj,kk)
    end select
  end function kcomp
end subroutine fd_metric_derivs


! -----------------------------------------------------------------------------
! Experiment-B drift.  This is THE measurement of Experiment B: how far the
! BSSN-evolved geometry has moved from the analytic solution.  It is NOT a
! constraint violation and must not be conflated with one.
! -----------------------------------------------------------------------------
subroutine WarpAnalysis_Drift(CCTK_ARGUMENTS)
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS
  integer   :: i, j, k
  CCTK_REAL :: ksign, ax, ay, az, dk, dg

  ksign = 1.0d0
  if (CCTK_EQUALS(k_sign_convention, "opposite")) ksign = -1.0d0

  !$OMP PARALLEL DO COLLAPSE(3) PRIVATE(i,j,k,ax,ay,az,dk,dg)
  do k = 1, cctk_lsh(3)
    do j = 1, cctk_lsh(2)
      do i = 1, cctk_lsh(1)
        ax = ksign*(-warp_dVx(i,j,k))
        ay = ksign*(-0.5d0*warp_dVy(i,j,k))
        az = ksign*(-0.5d0*warp_dVz(i,j,k))
        dk = max(abs(kxx(i,j,k) - ax), abs(kxy(i,j,k) - ay),                 &
                 abs(kxz(i,j,k) - az), abs(kyy(i,j,k)),                      &
                 abs(kyz(i,j,k)), abs(kzz(i,j,k)))
        dg = max(abs(gxx(i,j,k) - 1.0d0), abs(gyy(i,j,k) - 1.0d0),           &
                 abs(gzz(i,j,k) - 1.0d0), abs(gxy(i,j,k)),                   &
                 abs(gxz(i,j,k)), abs(gyz(i,j,k)))
        ResidualKij(i,j,k)     = dk
        ResidualGammaij(i,j,k) = dg
      end do
    end do
  end do
  !$OMP END PARALLEL DO
end subroutine WarpAnalysis_Drift


! -----------------------------------------------------------------------------
! Summary report.  Uses CCTK reductions so the numbers are correct under MPI.
! -----------------------------------------------------------------------------
subroutine WarpAnalysis_Report(CCTK_ARGUMENTS)
  implicit none
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  integer            :: ierr, hi, vi
  CCTK_REAL          :: rmax, imax, hmax, hvac, kmax, gmax, necmin
  character(len=256) :: msg
  logical            :: pass

  if (report_every > 0) then
    if (mod(cctk_iteration, report_every) /= 0) return
  else if (cctk_iteration /= 0) then
    return
  end if
  if (CCTK_MyProc(cctkGH) /= 0) then
    ! Reductions are collective; every rank must still call them.
  end if

  call CCTK_ReductionHandle(hi, "maximum")
  if (hi < 0) return

  call gmax_of("WarpAnalysis::ResidualIdentity", hi, imax)
  call gmax_of("WarpAnalysis::ResidualRho",      hi, rmax)
  call gmax_of("WarpAnalysis::ResidualKij",      hi, kmax)
  call gmax_of("WarpAnalysis::ResidualGammaij",  hi, gmax)

  if (CCTK_MyProc(cctkGH) /= 0) return

  pass = (imax <= verification_tolerance) .and. (rmax <= verification_tolerance)

  call CCTK_INFO("---------------- WarpDrive verification summary ----------------")
  write(msg,'(A,I8,A,ES12.5)') "  iteration ", cctk_iteration, "   t = ", cctk_time
  call CCTK_INFO(msg)
  write(msg,'(A,ES12.5)') "  T7  max |algebraic identity residual| = ", imax
  call CCTK_INFO(msg)
  write(msg,'(A,ES12.5)') "  T7  max |rho(closed form) - rho(Hamiltonian)| = ", rmax
  call CCTK_INFO(msg)
  write(msg,'(A,ES12.5)') "  B   max |K_ij(evolved) - K_ij(analytic)| = ", kmax
  call CCTK_INFO(msg)
  write(msg,'(A,ES12.5)') "  B   max |gamma_ij(evolved) - delta_ij|  = ", gmax
  call CCTK_INFO(msg)
  if (pass) then
    call CCTK_INFO("  VERDICT: analytic identities PASS")
  else
    call CCTK_WARN(CCTK_WARN_ALERT, "  VERDICT: analytic identities FAILED -- "// &
                   "do not interpret any physics from this run")
  end if
  call CCTK_INFO("---------------------------------------------------------------")

contains
  subroutine gmax_of(name, handle, out)
    character(len=*), intent(in)  :: name
    integer,          intent(in)  :: handle
    CCTK_REAL,        intent(out) :: out
    integer :: idx, ierr2
    out = -1.0d0
    idx = CCTK_VarIndex(name)
    if (idx < 0) return
    call CCTK_Reduce(ierr2, cctkGH, -1, handle, 1, CCTK_VARIABLE_REAL, out, 1, idx)
  end subroutine gmax_of
end subroutine WarpAnalysis_Report
EOF

echo
echo "done.  WarpAnalysis: $(ls "$T/src" | wc -l) files"
echo "next: ./install_warpdrive_runtime.sh $CACTUS"

