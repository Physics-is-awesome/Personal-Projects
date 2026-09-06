#!/usr/bin/env bash
#
# install_warpdrive_runtime.sh
# ============================
# Writes the buildable/runnable layer: ThornList, parameter files, test
# driver, README.
#
#   usage:  ./install_warpdrive_runtime.sh /path/to/Cactus
#
# Run AFTER install_warpdrive_core.sh and install_warpdrive_analysis.sh.
#
set -euo pipefail

CACTUS="${1:-}"
[[ -n "$CACTUS" && -d "$CACTUS" ]] || { echo "usage: $0 /path/to/Cactus" >&2; exit 2; }
ARR="$CACTUS/arrangements/WarpDrive"
[[ -d "$ARR/WarpBase" ]] || { echo "error: run install_warpdrive_core.sh first" >&2; exit 2; }
mkdir -p "$ARR/par" "$ARR/test" "$ARR/doc"
echo "installing runtime layer into $ARR"

# =============================================================================
#                              T H O R N L I S T
# =============================================================================
cat > "$ARR/warpdrive.th" <<'EOF'
# warpdrive.th -- ThornList for the WarpDrive arrangement
#
#   build:  cd $CACTUS
#           ./simfactory/bin/sim build warpdrive --thornlist arrangements/WarpDrive/warpdrive.th
#   or:     make warpdrive-config THORNLIST=arrangements/WarpDrive/warpdrive.th
#           make warpdrive
#
# Deliberately minimal.  Every thorn below is either required by WarpDrive or
# required to run the verification suite.  Nothing is here "just in case".

# ------------------------------------------------------------------ flesh
!DEFINE ROOT = Cactus
!DEFINE ARR  = $ROOT/arrangements
!DEFINE COMPONENTLIST_TARGET = $ROOT/thornlists/

# ------------------------------------------------------- external libraries
!TARGET   = $ROOT
!TYPE     = git
!URL      = https://bitbucket.org/cactuscode/cactusexternal.git
!REPO_PATH= $2
!CHECKOUT =
ExternalLibraries/HDF5
ExternalLibraries/MPI
ExternalLibraries/zlib
ExternalLibraries/hwloc
ExternalLibraries/BLAS
ExternalLibraries/LAPACK

# ------------------------------------------------------------------- Cactus
!TARGET   = $ROOT
!TYPE     = git
!URL      = https://bitbucket.org/cactuscode/cactusbase.git
!REPO_PATH= $2
!CHECKOUT =
CactusBase/Boundary
CactusBase/CartGrid3D
CactusBase/CoordBase
CactusBase/Fortran
CactusBase/InitBase
CactusBase/IOASCII
CactusBase/IOBasic
CactusBase/IOUtil
CactusBase/SymBase
CactusBase/Time

!TARGET   = $ROOT
!TYPE     = git
!URL      = https://bitbucket.org/cactuscode/cactusnumerical.git
!REPO_PATH= $2
!CHECKOUT =
CactusNumerical/Cartoon2D
CactusNumerical/InterpToArray
CactusNumerical/LocalInterp
CactusNumerical/LocalReduce
CactusNumerical/MoL
CactusNumerical/ReflectionSymmetry
CactusNumerical/RotatingSymmetry180
CactusNumerical/SlabTest
CactusNumerical/SpaceMask
CactusNumerical/SphericalSurface
CactusNumerical/SummationByParts

!TARGET   = $ROOT
!TYPE     = git
!URL      = https://bitbucket.org/cactuscode/cactusutils.git
!REPO_PATH= $2
!CHECKOUT =
CactusUtils/Formaline
CactusUtils/NaNChecker
CactusUtils/SystemStatistics
CactusUtils/SystemTopology
CactusUtils/TimerReport
CactusUtils/Vectors

!TARGET   = $ROOT
!TYPE     = git
!URL      = https://bitbucket.org/cactuscode/cactusconnect.git
!REPO_PATH= $2
!CHECKOUT =
CactusConnect/HTTPD
CactusConnect/Socket

# --------------------------------------------------------------- Einstein
!TARGET   = $ROOT
!TYPE     = git
!URL      = https://bitbucket.org/einsteintoolkit/einsteinbase.git
!REPO_PATH= $2
!CHECKOUT =
EinsteinBase/ADMBase
EinsteinBase/ADMCoupling
EinsteinBase/ADMMacros
EinsteinBase/Constants
EinsteinBase/CoordGauge
EinsteinBase/HydroBase
EinsteinBase/StaticConformal
EinsteinBase/TmunuBase

!TARGET   = $ROOT
!TYPE     = git
!URL      = https://bitbucket.org/einsteintoolkit/einsteinevolve.git
!REPO_PATH= $2
!CHECKOUT =
EinsteinEvolve/NewRad

!TARGET   = $ROOT
!TYPE     = git
!URL      = https://bitbucket.org/einsteintoolkit/einsteininitialdata.git
!REPO_PATH= $2
!CHECKOUT =
EinsteinInitialData/Exact

!TARGET   = $ROOT
!TYPE     = git
!URL      = https://bitbucket.org/einsteintoolkit/einsteinanalysis.git
!REPO_PATH= $2
!CHECKOUT =
EinsteinAnalysis/ADMConstraints
EinsteinAnalysis/ADMMass
EinsteinAnalysis/Extract

# ---------------------------------------------------------------- Carpet
!TARGET   = $ROOT
!TYPE     = git
!URL      = https://bitbucket.org/eschnett/carpet.git
!REPO_PATH= $2
!CHECKOUT =
Carpet/Carpet
Carpet/CarpetIOASCII
Carpet/CarpetIOBasic
Carpet/CarpetIOHDF5
Carpet/CarpetIOScalar
Carpet/CarpetInterp
Carpet/CarpetInterp2
Carpet/CarpetLib
Carpet/CarpetReduce
Carpet/CarpetRegrid2
Carpet/CarpetSlab
Carpet/CarpetTracker
Carpet/CycleClock
Carpet/LoopControl
Carpet/Timers

# -------------------------------------------------------------- McLachlan
# Required only for Experiment B.  Experiment A builds and runs without it.
!TARGET   = $ROOT
!TYPE     = git
!URL      = https://bitbucket.org/einsteintoolkit/mclachlan.git
!REPO_PATH= $2
!CHECKOUT =
McLachlan/ML_BSSN
McLachlan/ML_BSSN_Helper

# -------------------------------------------------------------- WarpDrive
# Local, not fetched.  WarpBase must precede the others: they USE its modules.
!TARGET   = $ROOT
!TYPE     = ignore
!CHECKOUT =
WarpDrive/WarpBase
WarpDrive/WarpInitial
WarpDrive/WarpTmunu
WarpDrive/WarpAnalysis
EOF

# =============================================================================
#                        P A R A M E T E R   F I L E S
# =============================================================================

# ------------------------------------------------------------------ T1 / T2
cat > "$ARR/par/warp_T1_T2_minkowski.par" <<'EOF'
# =============================================================================
# warp_T1_T2_minkowski.par
# Tests T1 (Minkowski limit) and T2 (zero warp velocity)
# =============================================================================
# EXPECTED RESULT.  Every derived quantity is EXACTLY zero -- not small, zero.
# With warp_velocity = 0 the profile multiplies zero, so V, dV, K_ij, rho,
# S_ij, R^(4) and Kretschmann all vanish bit-for-bit in IEEE arithmetic.
#
# MEASURED QUANTITY: Linf norms of Kretschmann, EnergyDensity, ResidualKij.
# ACCEPTABLE ERROR : identically 0.0d0.  Any nonzero value is a bug, not
#                    round-off -- there is nothing to round.
# CONVERGENCE      : not applicable (exact).
#
# This is the cheapest possible gate and it catches uninitialised memory,
# wrong array indexing and stale grid functions.  Run it first, every time.
# =============================================================================

ActiveThorns = "
  Boundary CartGrid3D CoordBase InitBase SymBase Time
  IOUtil IOBasic IOASCII
  Carpet CarpetLib CarpetReduce CarpetIOBasic CarpetIOASCII CarpetIOScalar
  LoopControl
  ADMBase ADMCoupling ADMMacros CoordGauge StaticConformal TmunuBase Constants
  WarpBase WarpInitial WarpTmunu WarpAnalysis
"

Cactus::cctk_itlast = 0

# ------------------------------------------------------------------ grid
CoordBase::domainsize = "minmax"
CoordBase::xmin = -8.0
CoordBase::xmax =  8.0
CoordBase::ymin = -8.0
CoordBase::ymax =  8.0
CoordBase::zmin = -8.0
CoordBase::zmax =  8.0
CoordBase::dx   = 0.25
CoordBase::dy   = 0.25
CoordBase::dz   = 0.25
CoordBase::boundary_size_x_lower = 3
CoordBase::boundary_size_x_upper = 3
CoordBase::boundary_size_y_lower = 3
CoordBase::boundary_size_y_upper = 3
CoordBase::boundary_size_z_lower = 3
CoordBase::boundary_size_z_upper = 3
CartGrid3D::type = "coordbase"
Carpet::domain_from_coordbase = "yes"
Carpet::ghost_size = 3
Carpet::max_refinement_levels = 1

# ------------------------------------------------------------------ ADMBase
ADMBase::initial_data    = "warpdrive"
ADMBase::initial_lapse   = "warpdrive"
ADMBase::initial_shift   = "warpdrive"
ADMBase::initial_dtlapse = "warpdrive"
ADMBase::initial_dtshift = "warpdrive"
ADMBase::evolution_method = "none"
ADMBase::metric_type = "physical"

# ------------------------------------------------------------------ WarpDrive
# THE test parameter: zero velocity.
WarpBase::warp_velocity   = 0.0
WarpBase::profile_type    = "alcubierre1994"
WarpBase::bubble_radius   = 2.0
WarpBase::wall_sigma      = 3.0
WarpBase::bubble_center_x = 0.0
WarpBase::verify_identities = "yes"
WarpBase::check_finite      = "yes"

WarpAnalysis::report_norms = "yes"
WarpAnalysis::report_every = 1

IO::out_dir = "warp_T1_T2"
IOBasic::outInfo_every = 1
IOBasic::outInfo_vars  = "
  WarpAnalysis::Kretschmann
  WarpAnalysis::EnergyDensity
  WarpAnalysis::ResidualIdentity
"
IOASCII::out1D_every = 1
IOASCII::out1D_vars  = "
  WarpBase::warp_V
  ADMBase::kxx
  WarpAnalysis::EnergyDensity
  WarpAnalysis::Kretschmann
"
EOF

# ------------------------------------------------------------ Experiment A
cat > "$ARR/par/warp_expA_example1.par" <<'EOF'
# =============================================================================
# warp_expA_example1.par
# EXPERIMENT A -- analytic prescribed background, paper Example 1
# =============================================================================
# WHAT THIS IS.  The geometry is PRESCRIBED at every point from the closed
# forms.  ADMBase::evolution_method = "none": nothing evolves.  This is the
# reproduction of paper Example 1 [PAPER-ABSTRACT: "the solution form assumed
# a priori, as in Alcubierre's model"] using the [LEGACY] Alcubierre tanh
# profile inside the paper's one-component Natario-class subcase.
#
# WHAT THIS IS NOT.  Not a self-consistent dynamical warp drive.  Not a
# simulation of anything evolving.  The brief (Sec. 13) is explicit about this
# and so is the README.
#
# EXPECTED RESULTS
#   rho <= 0 everywhere, strictly negative in the wall where d_yV, d_zV != 0.
#   rho = 0 exactly on the x-axis (y = z = 0), where transverse gradients
#     vanish -- a sharp, easily-plotted structural prediction.
#   NEC_min < 0, WEC_min < 0 throughout the wall: violated, as expected.
#   TidalMinorResidual = 0 to round-off (the E_yy E_zz - E_yz^2 identity).
#   ResidualIdentity < 1e-12.
#   det E = 0 for this rigidly-translating spherically-symmetric profile,
#     so TidalEigenvalue for one branch is ~0.
# =============================================================================

ActiveThorns = "
  Boundary CartGrid3D CoordBase InitBase SymBase Time
  IOUtil IOBasic IOASCII
  Carpet CarpetLib CarpetReduce CarpetIOBasic CarpetIOASCII CarpetIOScalar CarpetIOHDF5
  LoopControl
  ADMBase ADMCoupling ADMMacros CoordGauge StaticConformal TmunuBase Constants
  WarpBase WarpInitial WarpTmunu WarpAnalysis
"

Cactus::cctk_itlast = 0

CoordBase::domainsize = "minmax"
CoordBase::xmin = -12.0
CoordBase::xmax =  12.0
CoordBase::ymin = -12.0
CoordBase::ymax =  12.0
CoordBase::zmin = -12.0
CoordBase::zmax =  12.0
CoordBase::dx   = 0.1
CoordBase::dy   = 0.1
CoordBase::dz   = 0.1
CoordBase::boundary_size_x_lower = 3
CoordBase::boundary_size_x_upper = 3
CoordBase::boundary_size_y_lower = 3
CoordBase::boundary_size_y_upper = 3
CoordBase::boundary_size_z_lower = 3
CoordBase::boundary_size_z_upper = 3
CartGrid3D::type = "coordbase"
Carpet::domain_from_coordbase = "yes"
Carpet::ghost_size = 3

ADMBase::initial_data    = "warpdrive"
ADMBase::initial_lapse   = "warpdrive"
ADMBase::initial_shift   = "warpdrive"
ADMBase::initial_dtlapse = "warpdrive"
ADMBase::initial_dtshift = "warpdrive"
ADMBase::evolution_method = "none"

# ---------------------------------------------------------------- the model
WarpBase::profile_type    = "alcubierre1994"   # [LEGACY], paper Example 1
WarpBase::warp_velocity   = 2.0                # coordinate velocity; > 1 is allowed
WarpBase::bubble_radius   = 2.0
WarpBase::wall_sigma      = 3.0
WarpBase::translate_bubble = "yes"
WarpBase::bubble_center_x = 0.0
WarpBase::origin_treatment = "regularized"
WarpBase::verify_identities = "yes"
WarpBase::identity_tolerance = 1.0e-11

# T0 -- CONFIRM THIS AGAINST YOUR CHECKOUT BEFORE TRUSTING ANY SIGN.
WarpBase::k_sign_convention = "mtw_admbase"

WarpTmunu::add_to_tmunu = "yes"

WarpAnalysis::curvature_source        = "both"
WarpAnalysis::fd_order               = 4
WarpAnalysis::compute_energy_conditions = "yes"
WarpAnalysis::ec_n_directions        = 162
WarpAnalysis::ec_n_rapidity          = 8
WarpAnalysis::ec_max_lorentz         = 50.0
WarpAnalysis::report_norms           = "yes"
WarpAnalysis::report_every           = 1
WarpAnalysis::boundary_exclude       = 3

IO::out_dir = "warp_expA_example1"
IOBasic::outInfo_every = 1
IOBasic::outInfo_vars  = "
  WarpAnalysis::EnergyDensity
  WarpAnalysis::Kretschmann
  WarpAnalysis::NEC_min
  WarpAnalysis::TidalMaxAbs
  WarpAnalysis::ResidualIdentity
"

IOASCII::out1D_every = 1
IOASCII::out1D_vars  = "
  WarpBase::warp_V
  WarpBase::warp_accel
  WarpBase::warp_omega2
  ADMBase::lapse
  ADMBase::shift
  ADMBase::kxx
  ADMBase::kxy
  WarpAnalysis::EnergyDensity
  WarpAnalysis::RicciScalar4
  WarpAnalysis::Kretschmann
  WarpAnalysis::TidalEigenvalue1
  WarpAnalysis::TidalEigenvalue3
  WarpAnalysis::NEC_min
"

IOHDF5::out_every = 1
IOHDF5::out_vars  = "
  ADMBase::lapse ADMBase::shift ADMBase::metric ADMBase::curv
  WarpBase::warp_V
  WarpAnalysis::EnergyDensity
  WarpAnalysis::RicciScalar4
  WarpAnalysis::Kretschmann
  WarpAnalysis::TidalEigenvalue1
  WarpAnalysis::TidalEigenvalue2
  WarpAnalysis::TidalEigenvalue3
  WarpAnalysis::NEC_min
  WarpAnalysis::WEC_min
  WarpAnalysis::DEC_min
  WarpAnalysis::SEC_min
  WarpAnalysis::EC_violation_flags
"
EOF

# ------------------------------------------------------------ Experiment B
cat > "$ARR/par/warp_expB_evolution.par" <<'EOF'
# =============================================================================
# warp_expB_evolution.par
# EXPERIMENT B -- BSSN evolution of the analytic initial data
# =============================================================================
# READ THIS BEFORE INTERPRETING ANY OUTPUT.
#
# The initial data has rho != 0.  It therefore satisfies the Hamiltonian
# constraint ONLY WITH THE MATTER SOURCE PRESENT.  WarpTmunu supplies
# T_mu_nu through TmunuBase at every step, so this run is constraint-
# satisfying at t = 0.  Running WITHOUT WarpTmunu (see
# warp_expB_vacuum_INVALID.par) gives a finite, resolution-INDEPENDENT
# violation that never converges away, and which must NOT be reported as an
# instability.
#
# SECOND WARNING -- GAUGE VERSUS PHYSICS.
# The initial gauge alpha = 1 is GEODESIC SLICING, a known focusing gauge:
# coordinate observers are freely falling and converge, so det(gamma) can
# collapse for purely gauge reasons.  The paper SEPARATELY predicts a
# physical caustic instability for Example 2 [PAPER-SECONDARY].  These two
# look identical on a plot of the metric.
#
# DISCRIMINATION PROTOCOL (mandatory before any instability claim):
#   1. Watch curvature INVARIANTS (Kretschmann, RicciSquared), not gamma_ij.
#      Gauge pathology leaves invariants bounded; physical caustics do not.
#   2. Re-run with ML_BSSN::lapse_evolution_method = "1+log" and
#      shift_evolution_method = "gamma0" (below, commented).  A gauge
#      artefact changes character; a physical caustic does not.
#   3. Check convergence: a physical feature converges, a gauge/numerical
#      artefact typically does not.
#
# The run below deliberately uses HARMONIC-FREE evolution (evolve the given
# gauge) so that Experiment B measures the STABILITY OF THE DATA, not of a
# different gauge.  Switch to 1+log for step 2 of the protocol.
#
# MEASURED QUANTITIES
#   ResidualKij, ResidualGammaij -- drift from the analytic solution
#   HamiltonianConstraint        -- should stay small and converge as O(h^4)
#   HamiltonianConstraint_vacuum -- will NOT be small; that is expected
#   Kretschmann                  -- the physical/gauge discriminator
# =============================================================================

ActiveThorns = "
  Boundary CartGrid3D CoordBase InitBase SymBase Time
  IOUtil IOBasic IOASCII
  Carpet CarpetLib CarpetReduce CarpetIOBasic CarpetIOASCII CarpetIOScalar CarpetIOHDF5
  LoopControl MoL NaNChecker SpaceMask SphericalSurface
  ADMBase ADMCoupling ADMMacros CoordGauge StaticConformal TmunuBase Constants
  NewRad
  ML_BSSN ML_BSSN_Helper
  WarpBase WarpInitial WarpTmunu WarpAnalysis
"

Cactus::cctk_itlast   = 400
Cactus::terminate     = "iteration"

# ------------------------------------------------------------------ grid
CoordBase::domainsize = "minmax"
CoordBase::xmin = -16.0
CoordBase::xmax =  16.0
CoordBase::ymin = -16.0
CoordBase::ymax =  16.0
CoordBase::zmin = -16.0
CoordBase::zmax =  16.0
CoordBase::dx   = 0.2
CoordBase::dy   = 0.2
CoordBase::dz   = 0.2
CoordBase::boundary_size_x_lower = 3
CoordBase::boundary_size_x_upper = 3
CoordBase::boundary_size_y_lower = 3
CoordBase::boundary_size_y_upper = 3
CoordBase::boundary_size_z_lower = 3
CoordBase::boundary_size_z_upper = 3
CartGrid3D::type = "coordbase"
Carpet::domain_from_coordbase = "yes"
Carpet::ghost_size = 3
Carpet::max_refinement_levels = 1
Carpet::init_fill_timelevels = "yes"
Carpet::poison_new_timelevels = "yes"

Time::dtfac = 0.25

MoL::ODE_Method            = "RK4"
MoL::MoL_Intermediate_Steps = 4
MoL::MoL_Num_Scratch_Levels = 1

# ------------------------------------------------------------------ ADMBase
ADMBase::initial_data    = "warpdrive"
ADMBase::initial_lapse   = "warpdrive"
ADMBase::initial_shift   = "warpdrive"
ADMBase::initial_dtlapse = "warpdrive"
ADMBase::initial_dtshift = "warpdrive"
ADMBase::evolution_method       = "ML_BSSN"
ADMBase::lapse_evolution_method = "ML_BSSN"
ADMBase::shift_evolution_method = "ML_BSSN"
ADMBase::dtlapse_evolution_method = "ML_BSSN"
ADMBase::dtshift_evolution_method = "ML_BSSN"

# ------------------------------------------------------------------ ML_BSSN
# STEP 1 of the discrimination protocol: preserve the paper's gauge.
ML_BSSN::harmonicN         = 1        # 1+log family exponent
ML_BSSN::harmonicF         = 0.0      # 0 -> d_t alpha = 0, i.e. KEEP alpha = 1
ML_BSSN::ShiftGammaCoeff   = 0.0      # 0 -> d_t beta^i = 0, KEEP beta^i
ML_BSSN::BetaDriver        = 0.0
ML_BSSN::LapseAdvectionCoeff = 0.0
ML_BSSN::ShiftAdvectionCoeff = 0.0
ML_BSSN::conformalMethod   = 1        # W = chi^(1/2), better for strong fields
ML_BSSN::dt_lapse_shift_method = "noLapseShiftAdvection"
ML_BSSN::my_initial_boundary_condition = "extrapolate-gammas"
ML_BSSN::my_rhs_boundary_condition     = "NewRad"
ML_BSSN::fdOrder = 4
ML_BSSN::epsDiss = 0.1                # Kreiss-Oliger, [IMPL] tune per test T12
Boundary::radpower = 2

# STEP 2 of the protocol -- uncomment to re-run in 1+log/gamma-driver.
# If the qualitative behaviour changes, the effect was GAUGE.
# ML_BSSN::harmonicF       = 2.0
# ML_BSSN::ShiftGammaCoeff = 0.75
# ML_BSSN::BetaDriver      = 1.0

# ------------------------------------------------------------------ WarpDrive
WarpBase::profile_type      = "alcubierre1994"
WarpBase::warp_velocity     = 0.5     # modest: Experiment B is about STABILITY
WarpBase::bubble_radius     = 2.0
WarpBase::wall_sigma        = 2.0     # thicker wall -> better resolved
WarpBase::translate_bubble  = "yes"
WarpBase::k_sign_convention = "mtw_admbase"
WarpBase::verify_identities = "yes"

# NON-NEGOTIABLE for a valid Experiment B.
WarpTmunu::add_to_tmunu = "yes"
WarpTmunu::tmunu_scale  = 1.0

TmunuBase::stress_energy_storage = "yes"
TmunuBase::stress_energy_at_RHS  = "yes"
TmunuBase::timelevels            = 1
TmunuBase::prolongation_type     = "none"

WarpAnalysis::curvature_source  = "both"
WarpAnalysis::compute_drift     = "yes"
WarpAnalysis::compute_constraints = "yes"
WarpAnalysis::fd_order          = 4
WarpAnalysis::report_norms      = "yes"
WarpAnalysis::report_every      = 20

NaNChecker::check_every    = 20
NaNChecker::action_if_found = "terminate"
NaNChecker::check_vars     = "ADMBase::metric ADMBase::curv ADMBase::lapse"

IO::out_dir = "warp_expB"
IOBasic::outInfo_every = 20
IOBasic::outInfo_vars  = "
  WarpAnalysis::ResidualKij
  WarpAnalysis::ResidualGammaij
  WarpAnalysis::HamiltonianConstraint
  WarpAnalysis::Kretschmann
  ADMBase::alp
"
IOScalar::outScalar_every = 20
IOScalar::outScalar_reductions = "minimum maximum norm2 norm_inf"
IOScalar::outScalar_vars = "
  WarpAnalysis::HamiltonianConstraint
  WarpAnalysis::HamiltonianConstraint_vacuum
  WarpAnalysis::MomentumConstraint_x
  WarpAnalysis::ResidualKij
  WarpAnalysis::ResidualGammaij
  WarpAnalysis::Kretschmann
  WarpAnalysis::RicciSquared
"
IOASCII::out1D_every = 20
IOASCII::out1D_vars  = "
  ADMBase::lapse ADMBase::shift ADMBase::metric ADMBase::curv
  WarpAnalysis::HamiltonianConstraint
  WarpAnalysis::ResidualKij
  WarpAnalysis::Kretschmann
"
EOF

# ------------------------------------------------- deliberately invalid run
cat > "$ARR/par/warp_expB_vacuum_INVALID.par" <<'EOF'
# =============================================================================
# warp_expB_vacuum_INVALID.par
# =============================================================================
#                    *** THIS RUN IS SCIENTIFICALLY INVALID ***
#
# It exists ONLY as a control, to make one specific error visible and
# reproducible.  Do not report numbers from it as physics.
#
# WHAT IS WRONG.  WarpTmunu::add_to_tmunu = "no".  The initial data has
# rho != 0, so with no matter source the Hamiltonian constraint is violated
# BY CONSTRUCTION at t = 0 by the finite amount 16 pi rho.
#
# WHAT YOU WILL SEE, AND WHY IT IS A TRAP.
#   * A large initial Hamiltonian constraint violation.
#   * That violation does NOT shrink when you refine the grid.  It is not a
#     discretisation error; it is the wrong equation.
#   * A rapid, dramatic-looking departure from the analytic solution.
#
# It is extremely tempting to report the third bullet as "the warp solution
# is numerically unstable" or, worse, as confirmation of the paper's predicted
# instability [PAPER-SECONDARY].  It is neither.  It is a missing source term.
#
# THE DIAGNOSTIC THAT DISTINGUISHES THEM.  Compare, in the same run,
#   HamiltonianConstraint         (includes -16 pi rho)
#   HamiltonianConstraint_vacuum  (forces rho = 0)
# If the second is small while the first is large, the matter source is
# missing.  Run this parfile at two resolutions and confirm the violation is
# resolution-independent -- that is the signature.
# =============================================================================

# Inherit the valid setup, then break exactly one thing.
!include "warp_expB_evolution.par"

WarpTmunu::add_to_tmunu = "no"
Cactus::cctk_itlast     = 100
IO::out_dir             = "warp_expB_vacuum_INVALID"
EOF

# ---------------------------------------------------------------- convergence
cat > "$ARR/par/warp_T10_convergence_h.par" <<'EOF'
# =============================================================================
# warp_T10_convergence_h.par -- resolution convergence, base resolution h
# =============================================================================
# Tests T5, T6, T8, T10.  Run this file, then the h/2 and h/4 variants
# produced by run_convergence.sh, and compare.
#
# EXPECTED RESULT
#   * ANALYTIC quantities (WarpBase, WarpAnalysis curvature_source=analytic)
#     do not converge -- they are already exact.  Their residuals sit at
#     ~1e-15 INDEPENDENT of h.  If they shrink with h, something is
#     accidentally finite-differenced.
#   * NUMERICAL quantities (constraints, curvature_source=numerical) converge
#     at the finite-difference order, here O(h^4).
#
# MEASURED QUANTITY: Linf and L2 norms over AnalysisMask == 0.
# ACCEPTABLE ERROR : measured order within 0.3 of 4.0.
# CONVERGENCE      : ratio of norms between successive resolutions -> 16.
#
# WHY THE MASK MATTERS.  Without excluding boundary_exclude points at each
# face, a single garbage stencil at the edge dominates the Linf norm and the
# measured order becomes noise.  This is the single most common way a
# convergence test silently fails.
# =============================================================================

ActiveThorns = "
  Boundary CartGrid3D CoordBase InitBase SymBase Time
  IOUtil IOBasic IOASCII
  Carpet CarpetLib CarpetReduce CarpetIOBasic CarpetIOASCII CarpetIOScalar
  LoopControl
  ADMBase ADMCoupling ADMMacros CoordGauge StaticConformal TmunuBase Constants
  WarpBase WarpInitial WarpTmunu WarpAnalysis
"

Cactus::cctk_itlast = 0

CoordBase::domainsize = "minmax"
CoordBase::xmin = -10.0
CoordBase::xmax =  10.0
CoordBase::ymin = -10.0
CoordBase::ymax =  10.0
CoordBase::zmin = -10.0
CoordBase::zmax =  10.0
CoordBase::dx   = 0.4
CoordBase::dy   = 0.4
CoordBase::dz   = 0.4
CoordBase::boundary_size_x_lower = 3
CoordBase::boundary_size_x_upper = 3
CoordBase::boundary_size_y_lower = 3
CoordBase::boundary_size_y_upper = 3
CoordBase::boundary_size_z_lower = 3
CoordBase::boundary_size_z_upper = 3
CartGrid3D::type = "coordbase"
Carpet::domain_from_coordbase = "yes"
Carpet::ghost_size = 3

ADMBase::initial_data     = "warpdrive"
ADMBase::initial_lapse    = "warpdrive"
ADMBase::initial_shift    = "warpdrive"
ADMBase::evolution_method = "none"

# Gaussian profile for the convergence test: C-infinity, so the measured
# order is the STENCIL order and not limited by profile smoothness.  The
# tanh top-hat is also smooth, but its wall introduces a second length scale
# that muddies a clean order measurement at coarse h.
WarpBase::profile_type    = "gaussian"
WarpBase::warp_velocity   = 0.4
WarpBase::bubble_radius   = 3.0
WarpBase::translate_bubble = "yes"
WarpBase::k_sign_convention = "mtw_admbase"

WarpTmunu::add_to_tmunu = "yes"

WarpAnalysis::curvature_source  = "both"
WarpAnalysis::fd_order          = 4
WarpAnalysis::boundary_exclude  = 4
WarpAnalysis::report_norms      = "yes"
WarpAnalysis::report_every      = 1
WarpAnalysis::compute_energy_conditions = "no"

IO::out_dir = "warp_T10_h1"
IOScalar::outScalar_every = 1
IOScalar::outScalar_reductions = "norm2 norm_inf"
IOScalar::outScalar_vars = "
  WarpAnalysis::HamiltonianConstraint
  WarpAnalysis::MomentumConstraint_x
  WarpAnalysis::ResidualRho
  WarpAnalysis::ResidualIdentity
"
EOF

# =============================================================================
#                            T E S T   D R I V E R
# =============================================================================
cat > "$ARR/test/run_convergence.sh" <<'EOF'
#!/usr/bin/env bash
#
# run_convergence.sh -- tests T8 and T10, measured not asserted
#
#   usage: ./run_convergence.sh /path/to/Cactus <exe-config-name>
#
# Generates h, h/2, h/4 variants of warp_T10_convergence_h.par, runs them,
# extracts the constraint norms and reports the measured order.
#
# Expected: order -> 4.0 for the finite-differenced constraints; FLAT (no
# convergence, ~1e-15) for ResidualIdentity and ResidualRho, which are
# analytic and already exact.  A convergent ResidualIdentity would mean the
# closed forms are being finite-differenced somewhere -- a bug.
#
set -euo pipefail
CACTUS="${1:-}"; CFG="${2:-warpdrive}"
[[ -d "$CACTUS" ]] || { echo "usage: $0 /path/to/Cactus <config>" >&2; exit 2; }

EXE="$CACTUS/exe/cactus_$CFG"
[[ -x "$EXE" ]] || { echo "error: $EXE not built" >&2; exit 2; }
PAR="$CACTUS/arrangements/WarpDrive/par/warp_T10_convergence_h.par"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT

declare -a DXS=(0.4 0.2 0.1)
declare -a TAGS=(h1 h2 h4)

for i in 0 1 2; do
  dx="${DXS[$i]}"; tag="${TAGS[$i]}"
  sed -e "s/^CoordBase::dx   = .*/CoordBase::dx   = $dx/" \
      -e "s/^CoordBase::dy   = .*/CoordBase::dy   = $dx/" \
      -e "s/^CoordBase::dz   = .*/CoordBase::dz   = $dx/" \
      -e "s|^IO::out_dir = .*|IO::out_dir = \"$WORK/warp_T10_$tag\"|" \
      "$PAR" > "$WORK/run_$tag.par"
  echo "=== running dx = $dx ==="
  "$EXE" "$WORK/run_$tag.par" > "$WORK/log_$tag.txt" 2>&1 || {
    echo "run failed; see $WORK/log_$tag.txt"; tail -30 "$WORK/log_$tag.txt"; exit 1; }
done

extract() {   # $1 = tag, $2 = variable stem
  local f="$WORK/warp_T10_$1/$2.norm_inf.asc"
  [[ -f "$f" ]] || { echo "nan"; return; }
  awk '!/^#/ && NF>=2 {v=$NF} END{print (v==""?"nan":v)}' "$f"
}

order() {     # $1,$2 = coarse,fine norms
  python3 -c "
import math,sys
a,b=float(sys.argv[1]),float(sys.argv[2])
print('n/a' if (b<=0 or a<=0) else '%.2f'%(math.log(a/b)/math.log(2.0)))" "$1" "$2"
}

printf '\n%-32s %12s %12s %12s %8s %8s\n' VARIABLE h h/2 h/4 p12 p24
printf '%.0s-' {1..100}; echo
for v in hamiltonianconstraint momentumconstraint_x residualrho residualidentity; do
  a=$(extract h1 "warpanalysis::$v"); b=$(extract h2 "warpanalysis::$v"); c=$(extract h4 "warpanalysis::$v")
  printf '%-32s %12s %12s %12s %8s %8s\n' "$v" "$a" "$b" "$c" "$(order "$a" "$b")" "$(order "$b" "$c")"
done

cat <<'NOTE'

INTERPRETATION
  hamiltonianconstraint, momentumconstraint_x : expect p ~ 4.0  (fd_order = 4)
  residualrho, residualidentity               : expect FLAT at ~1e-15 and
                                                p ~ 0.  These are analytic
                                                identities; convergence here
                                                would indicate that a closed
                                                form is being differenced.
NOTE
EOF
chmod +x "$ARR/test/run_convergence.sh"

# =============================================================================
#                                R E A D M E
# =============================================================================
cat > "$ARR/README.md" <<'MDEOF'
# WarpDrive — an Einstein Toolkit arrangement

Numerical-relativity implementation of the one-component coordinate-velocity
subcase of the Natário class analysed in

> T. Buchert & A. Frackowiak, *Novel Realizations of Warp Drive Spacetimes as
> Solutions of General Relativity*, arXiv:2605.03653 [gr-qc],
> *Universe* **12**, 132 (2026).

---

## 0. Read this before running anything

**Three things in this arrangement are gates, not suggestions.**

1. **T0 — the extrinsic-curvature sign convention.** Run
   `verify_sign_convention.sh $CACTUS` and set
   `WarpBase::k_sign_convention` from its verdict. A wrong sign produces an
   evolution that looks physically plausible and runs backwards in time.
2. **`WarpTmunu::add_to_tmunu = "yes"` for any evolution.** The data has
   $\rho\neq0$. Vacuum evolution violates the Hamiltonian constraint by
   construction, and that violation does **not** converge away.
3. **Provenance.** Only content tagged `[PAPER-ABSTRACT]` is verified against
   the paper. `[PAPER-SECONDARY]` items come from a machine-generated summary
   and are **not yet PDF-confirmed**. `WarpBase::strict_provenance = "yes"`
   (the default) refuses to activate anything depending on them. Nothing in
   the current four thorns does.

---

## 1. The physical model

Under the restrictions

- **R1** flow-orthogonality, $u^\mu = n^\mu$;
- **R2** unit lapse and shift–velocity identity, $N=1$, $N^i=-V^i$;
- **R3** flat spatial slices,

the metric is `[DERIVED]`

$$ds^2 = -dt^2 + (dx - V\,dt)^2 + dy^2 + dz^2$$

with $V(t,x,y,z)$ **completely arbitrary** — no stationarity, no profile
shape, no symmetry is assumed anywhere in the derivation.

This is **not** the Alcubierre 1994 metric. Alcubierre's $\tanh$ profile
enters only as `profile_type = "alcubierre1994"`, tagged `[LEGACY]`, and is
used as the paper's **Example 1** (the solution form assumed *a priori*) and
as an initial condition for Example 2.

### Matter is *inferred*, not sourced

Synge's G-method: assume the metric, read off the $T_{\mu\nu}$ Einstein's
equations demand. The warp field is not generated by matter here — that is
the entire point, and it is why Experiment A must never be described as a
self-consistent dynamical warp drive.

---

## 2. The 3+1 decomposition

$$\alpha = 1,\qquad \beta^i = (-V,0,0),\qquad \gamma_{ij} = \delta_{ij}$$

Consistency on $g_{tt}$: $-\alpha^2 + \gamma_{ij}\beta^i\beta^j = -1+V^2$. ✓

$$K_{xx} = -\partial_xV,\qquad K_{xy} = -\tfrac12\partial_yV,\qquad K_{xz} = -\tfrac12\partial_zV$$
$$K_{yy} = K_{yz} = K_{zz} = 0 \quad\textbf{identically}$$

Three of six components vanish for *every* profile — the cheapest structural
test in the suite, and it is free.

### Auxiliary scalars

$$\mathcal{A} \equiv \partial_tV + V\partial_xV,\qquad \Omega^2 \equiv \tfrac14\left[(\partial_yV)^2+(\partial_zV)^2\right],\qquad \theta = \partial_xV$$

---

## 3. The matter table `[DERIVED-CAS]`

| Quantity | Closed form |
|---|---|
| $\rho$ | $-\Omega^2/8\pi = -[(\partial_yV)^2+(\partial_zV)^2]/32\pi$ |
| $S_x$ | $-(\partial_y^2V+\partial_z^2V)/16\pi$ |
| $S_y,\,S_z$ | $\partial_x\partial_yV/16\pi,\;\partial_x\partial_zV/16\pi$ |
| $S_{xx}$ | $3\rho$ |
| $S_{xy}$ | $(\partial_y\mathcal{A}+\partial_xV\partial_yV)/16\pi$ |
| $S_{yy}$ | $-\partial_x\mathcal{A}/8\pi + [(\partial_yV)^2-(\partial_zV)^2]/32\pi$ |
| $S_{yz}$ | $\partial_yV\partial_zV/16\pi$ |
| $S$ | $3\rho - \partial_x\mathcal{A}/4\pi$ |
| $R^{(4)}$ | $2\partial_x\mathcal{A} + 2\Omega^2$ |
| $R^{(3)}$ | $0$ |

### Two Phase-0 errors this code is built to prevent

**E1.** Phase 0 asserted $R=0$. That is the *spatial* scalar (trivially, from
R3). The 4-D scalar is nonzero. `RicciScalar4` and `RicciScalar3` are
therefore **separate grid functions** — a diagnostic reporting
"RicciScalar = 0, slices are flat" while computing $R^{(4)}$ would be a false
pass.

**E2.** Phase 0 asserted $S=-\rho$. False; $S = 3\rho - \partial_x\mathcal{A}/4\pi$.
Note that $p=-\rho$ **is** the equation of state of a cosmological constant.
Shipping it would have manufactured a spurious "the warp fluid is
dark-energy-like" result that *also* appeared to corroborate the paper's
retention of $\Lambda$. `warp_matter_regression()` asserts both wrong forms
stay wrong.

### Free identities, checked every fill

$$S_{xx}=3\rho,\qquad S_{yy}+S_{zz}=-\frac{\partial_x\mathcal{A}}{4\pi},\qquad R^{(4)}=8\pi(\rho-S)$$

### Physics that falls out

- $\rho\le0$ everywhere, strictly negative wherever transverse gradients are
  nonzero. Energy density is **purely transverse-gradient driven**: a purely
  longitudinal profile has $\rho=0$. On the $x$-axis $\rho$ vanishes exactly.
- $\mathcal{A}$ appears **only** in $S_{ij}$ and $R^{(4)}$ — never in $\rho$,
  $S_i$ or $K_{ij}$. Coordinate acceleration is thus precisely a statement
  about which anisotropic stresses must be supplied externally.
- **Not a perfect fluid.** `StressAnisotropy` is never small, and $S_i\neq0$
  is heat flux in the Eulerian frame. `PressureIsotropic` = $S/3$ is provided
  for orientation only and does **not** represent a physical pressure.

### The tidal field `[DERIVED-CAS]`

$$\mathcal{E}_{xx}=-\partial_x\mathcal{A}+\Omega^2,\quad \mathcal{E}_{yy}=-\tfrac34(\partial_yV)^2,\quad \mathcal{E}_{yz}=-\tfrac34\partial_yV\partial_zV$$

The transverse block is $-\tfrac34 w_aw_b$ with $w=(\partial_yV,\partial_zV)$:
**rank one and negative semi-definite**. A freely falling observer is
compressed along the transverse gradient direction and feels nothing
perpendicular to it. Two free identities hold exactly:
$\operatorname{tr}\mathcal{E}=4\pi(\rho+S)$ and
$\mathcal{E}_{yy}\mathcal{E}_{zz}-\mathcal{E}_{yz}^2=0$.

For any rigidly translating spherically symmetric profile, $\det\mathcal{E}=0$
identically — one tidal eigenvalue is exactly zero. Nonzero $\det\mathcal{E}$
in an Example-1 run is a bug.

---

## 4. Architecture

```
WarpBase      V, all derivatives, kinematics, analytic matter.  SINGLE SOURCE OF TRUTH.
   |            no other thorn re-derives these (brief Sec. 20)
   +--> WarpInitial   -> ADMBase   (alpha, beta^i, gamma_ij, K_ij)
   +--> WarpTmunu     -> TmunuBase (T_mu_nu; REQUIRED for evolution)
   +--> WarpAnalysis  -> diagnostics only; writes no evolution variable
```

`WarpAnalysis` declares no ADMBase or ML_BSSN variable as `WRITES` anywhere in
its `schedule.ccl`, so it is *structurally* incapable of perturbing an
evolution.

---

## 5. Two real implementation traps, both fixed

**Origin regularity.** The chain-rule form
$\partial_i\partial_jV = v_s(q\,d_id_j + h\delta_{ij})$ looks safe because it
has no explicit $1/r_s$ — but $h=f'/r_s$ and $q=h'/r_s$ are themselves $0/0$
at the centre and return `NaN`. `warp_profile.F90` writes $h$ and $q$ in
exactly regular closed form; no Taylor branch, no grid offset, and $r_s=0$ is
an ordinary point. `origin_treatment = "naive"` reproduces the failure on
demand as a regression test.

**Far-field catastrophic cancellation — this one is severe.** The textbook
shape function subtracts two numbers both tending to 1. At $\sigma=3,R=2$ in
float64 that costs **4% relative accuracy at $r=5$ and 100% at $r=12$** —
exactly where an outer boundary sits, making boundary-contamination tests
unmeasurable. The fix uses
$\tanh a - \tanh b = \sinh(a-b)/(\cosh a\cosh b)$, collapsing the profile to

$$f(r) = \frac{\cosh^2(\sigma R)}{\cosh(\sigma(r+R))\cosh(\sigma(r-R))}$$

with no subtraction at all. Validated in float64 against an 80-digit mpmath
reference at 720,090 points: worst relative error $1.6\times10^{-15}$ ($f$),
$6.9\times10^{-15}$ ($h$), $1.5\times10^{-14}$ ($q$); zero non-finite values.

---

## 6. Build

```bash
./verify_sign_convention.sh $CACTUS          # T0, BLOCKING
./install_warpdrive_core.sh      $CACTUS
./install_warpdrive_analysis.sh  $CACTUS
./install_warpdrive_runtime.sh   $CACTUS

cd $CACTUS
./simfactory/bin/sim build warpdrive \
    --thornlist arrangements/WarpDrive/warpdrive.th
```

Build order is enforced by `configuration.ccl`: `WarpBase` `PROVIDES` the
capability that the other three `REQUIRE`, so its Fortran `.mod` files exist
before they are `USE`d.

---

## 7. Running

```bash
cd $CACTUS
exe/cactus_warpdrive arrangements/WarpDrive/par/warp_T1_T2_minkowski.par   # gate
exe/cactus_warpdrive arrangements/WarpDrive/par/warp_expA_example1.par     # Experiment A
exe/cactus_warpdrive arrangements/WarpDrive/par/warp_expB_evolution.par    # Experiment B
arrangements/WarpDrive/test/run_convergence.sh $CACTUS warpdrive           # T8/T10
```

---

## 8. Verification tests

| Test | Expected | Measured | Tolerance | Convergence |
|---|---|---|---|---|
| **T0** sign convention | one verdict from the checkout | grep of ADMBase/ML_BSSN source | exact match | n/a — **blocking** |
| **T1** Minkowski | all derived fields **exactly** 0 | Linf Kretschmann, rho | identically `0.0` | n/a (exact) |
| **T2** $v_s=0$ | as T1 | as T1 | identically `0.0` | n/a |
| **T3** large $R$ | $\rho\to0$ as $\sigma^{-2}$ in the interior | rho at centre | 5% of predicted scaling | $O(\sigma^{-2})$ |
| **T4a** structural | $K_{yy}=K_{yz}=K_{zz}=0$ | Linf of those three | $<10^{-16}$ | n/a (exact) |
| **T4b** tidal minor | $\mathcal{E}_{yy}\mathcal{E}_{zz}-\mathcal{E}_{yz}^2=0$ | `TidalMinorResidual` | $<10^{-14}$ | n/a (exact) |
| **T4c** symmetry | $\rho(y,z)=\rho(-y,-z)$; $\rho=0$ on the $x$-axis | reflection difference | $<10^{-15}$ | n/a |
| **T5** metric | analytic = numerical | `ResidualGammaij` at $t=0$ | $<10^{-15}$ | flat in $h$ |
| **T6** curvature | analytic = FD | analytic vs numerical $R^{(4)}$ | $<10^{-10}$ | $O(h^4)$ |
| **T7** Einstein | closed forms = ADM route | `ResidualRho`, `ResidualIdentity` | $<10^{-11}$ | flat in $h$ |
| **T8** constraints | small with matter, converging | `HamiltonianConstraint` | $O(h^4)$ | $p=4.0\pm0.3$ |
| **T8b** vacuum control | large, **non**-converging | `HamiltonianConstraint_vacuum` | must NOT converge | $p\approx0$ |
| **T9** geodesics | 4th-order in $\Delta\tau$ | position error vs reference | $p=4.0\pm0.3$ | $O(\Delta\tau^4)$ |
| **T10** resolution | see `run_convergence.sh` | Linf over `AnalysisMask==0` | ratio $\to16$ | $O(h^4)$ |
| **T11** profile precision | float64 = mpmath | rel. error in $f,h,q$ | $<10^{-13}$ | n/a |
| **T12** boundary | interior insensitive to $x_{\max}$ | interior rho vs box size | $<10^{-12}$ | n/a |

**T10's most important subtlety:** analytic residuals should be **flat** in
$h$, not convergent. If `ResidualIdentity` shrinks with resolution, a closed
form is being finite-differenced somewhere — that is a bug, and a convergence
plot that looks "good" is hiding it.

---

## 9. The three experiments

| | What it is | What it is **not** |
|---|---|---|
| **A** | Prescribed analytic geometry; particles/fluid respond to it | Not a self-consistent dynamical warp drive |
| **B** | BSSN evolution of the analytic data **with** $T_{\mu\nu}$ sourced | Not a vacuum test; not valid without `WarpTmunu` |
| **C** | Fully coupled Einstein–matter | Not attempted; the matter model's coupling must be derived first |

### Gauge versus physics — mandatory before any instability claim

$\alpha=1$ is **geodesic slicing**, a known focusing gauge: coordinate
observers are freely falling and converge, so $\det\gamma$ can collapse for
purely gauge reasons. The paper *separately* predicts a physical caustic
instability `[PAPER-SECONDARY]`. **These look identical on a plot of the
metric.** Discriminate by:

1. watching curvature **invariants**, not $\gamma_{ij}$ — gauge pathology
   leaves invariants bounded;
2. re-running in 1+log slicing with a gamma-driver shift (commented in the
   Experiment-B parfile) — a gauge artefact changes character, a physical
   caustic does not;
3. checking convergence — physical features converge, artefacts usually don't.

---

## 10. Energy conditions — what the numbers mean

All four fields are **margins: negative means violated**, so the zero contour
is the violation boundary and can be plotted directly.

- **`NEC_min` is exact.** Minimising $\rho + 2S_ie^i + S_{ij}e^ie^j$ over the
  unit sphere is a secular problem solved by bisecting
  $\sum_a g_a^2/(\mu_a-\lambda)^2 = 1$ below the smallest eigenvalue.
- **`WEC_min`, `DEC_min`, `SEC_min` are sampled** over an icosahedral
  direction set and a rapidity ladder. Sampling can *prove* a violation — one
  bad observer suffices — but can **never prove satisfaction**. A
  non-negative value means *"no violation found on the sampled set"*, and
  results must be phrased that way.

For this spacetime the distinction is academic — $\rho\le0$ already violates
the WEC at the Eulerian observer — but the code must not overstate what it
measured.

---

## 11. Units

Internally $G=c=1$ **always**; no routine multiplies by a unit conversion.
`length_unit_meters` is output metadata only. To convert: with one code unit
$=L$ metres, times are $L/c$ seconds, densities
$c^4/(8\pi G L^2)\;\mathrm{J\,m^{-3}}$, and curvature invariants scale as
$L^{-2}$ ($R$) and $L^{-4}$ (Kretschmann). Coordinate velocity is in units of
$c$ and **may exceed 1** — it is a coordinate velocity, not a local speed.

---

## 12. Known limitations

1. **`[PAPER-SECONDARY]` blockers remain open.** The $\Omega^2$ factor
   convention, the figure profile, whether $\Lambda\neq0$ numerically, and the
   Szekeres ansatz all need the PDF. My $\Omega^2$ convention reproduces the
   reported $\epsilon$, which is *evidence*, not proof.
2. **Example 2 is not implemented.** It requires solving
   $\partial_t V + V\partial_xV = 0$, a Burgers equation whose caustics are
   the paper's central dynamical claim — and it rests on unverified content.
3. **M3 / R1-Warp is out of scope.** Dropping R3 means curved slices;
   `RicciScalar3` and `warp_adm_metric` are the two places to extend.
4. **No test-particle or geodesic-deviation thorn yet** (brief Secs. 10–11).
   The tidal tensor and its eigenvalues are in place, which is the input they
   need.
5. **Experiment C is not attempted.** The matter is not a perfect fluid and
   has no equation of state; coupling it to a hydrodynamics thorn requires
   deriving that coupling first.
6. **`WeylPsi2Approx` is not $\Psi_2$.** It is $\tfrac13\operatorname{tr}\mathcal{E}$,
   a Coulomb-curvature proxy. A real $\Psi_2$ needs a null tetrad.
MDEOF

cat > "$ARR/doc/PROVENANCE.md" <<'MDEOF'
# Provenance ledger

Every equation in the codebase carries one of five tags. **No untagged
equation may enter the code.**

| Tag | Meaning | Trust |
|---|---|---|
| `[PAPER-ABSTRACT]` | Confirmed in the verified abstract of arXiv:2605.03653 | High |
| `[PAPER-SECONDARY]` | From a machine-generated summary. **NOT PDF-verified.** | **Provisional** |
| `[DERIVED]` / `[DERIVED-CAS]` | Derived here; CAS-verified where marked | High (internally verified) |
| `[IMPL]` | Implementation choice, not from the paper | n/a |
| `[LEGACY]` | Alcubierre 1994 / Natário 2002, **not** the target paper | High, wrong source |

## What is `[PAPER-ABSTRACT]`

- Alcubierre's kinematics analysed in a covariant 3+1 setting; drawback that
  "changes of the velocity profile are suppressed, apart from an externally
  given amplitude".
- Governing equations for the Natário class with one-component coordinate
  velocity in a subcase.
- Synge's G-method applied to two examples: form assumed *a priori*; form
  determined by an assumption along geodesics.
- Detailed role of coordinate acceleration and coordinate vorticity.
- Expected generic instability of the warp field for Example 2.
- Framework with spatial curvature via relativistic Lagrangian perturbation
  theory, including exact Szekeres class II solutions.
- Newtonian–GR correspondence; concluding discussion of tilted fluid flows.

## Still open — needs the PDF

- [ ] Equation numbers for R1/R2/R3.
- [ ] Exact $\Omega^2$ factor convention ($\tfrac12\Omega_{ij}\Omega^{ij}$ vs
      $\Omega_{ij}\Omega^{ij}$). My choice reproduces the reported
      $\epsilon = -(\Omega^2+\Lambda)/8\pi G$ — evidence, not proof.
- [ ] Exact definition and index placement of $\mathcal{A}$.
- [ ] The paper's signature and Riemann/extrinsic-curvature conventions.
- [ ] Whether $\theta,\sigma_{ij},\Omega_{ij}$ are coordinate (3-D) or
      covariant (4-D) kinematics — both appear and must not be conflated.
- [ ] The shape function used in the figures.
- [ ] Whether $\Lambda\neq0$ in the numerical examples.
- [ ] Precise statement and derivation of the caustic result.
- [ ] The Szekeres class II ansatz and the R1-Warp integrability relaxation.

**Nothing currently compiled depends on any open item.** All four thorns are
`[DERIVED]`, `[DERIVED-CAS]`, `[IMPL]` or `[LEGACY]` only. Milestone 3
(Example 2) cannot begin until the caustic result is confirmed.
MDEOF

echo
echo "done."
echo "  ThornList : $ARR/warpdrive.th"
echo "  parfiles  : $(ls "$ARR/par" | wc -l)"
echo "  README    : $ARR/README.md"
echo "  provenance: $ARR/doc/PROVENANCE.md"
echo
echo "build:  cd $CACTUS && ./simfactory/bin/sim build warpdrive \\"
echo "          --thornlist arrangements/WarpDrive/warpdrive.th"
echo
echo "RUN T0 FIRST:  ./verify_sign_convention.sh $CACTUS"

