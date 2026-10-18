# Build and run the axisymmetric vacuum Kerr BSSN example.
#
# Usage:
#   make            # production binary
#   make check      # parity + production build + full fast test suite (gate)
#   make run        # run with parameter_files/vacuum_kerr.par
#   make run PARFILE=path/to/another.par
#   make run RUN_ENV='GR_NSTEPS=700 GR_HORIZON_EVERY=0 GR_HZ_ABORT=0' RUN_DIR=/tmp
#                   # pass runtime overrides and keep generated output in /tmp
#
# Slower physics diagnostics (not part of `make check`):
#   make test-constraint-conv   # Hamiltonian-constraint convergence on EXACT
#                               # Kerr-Schild data; confirms 2nd-order accuracy
#   make test-horizon-drift     # validates horizon_mod against exact Kerr and
#                               # attributes horizon migration to solver vs data
#
# Long-run health checks (environment variables, driver):
#   GR_DIAG_SHIFT=1      print max|beta| / max|Gbar| in the inner ring
#   GR_EXCISION_SAT=...  turn the level-set characteristic SAT on/off
#   GR_HZ_EVERY=<steps>  horizon-find interval (0 disables)

.DEFAULT_GOAL := all
#
# KNOWN LIMITATION: the puncture gauge is not active, so the found horizon
# drifts steadily inward and the run eventually fails near r_excise. The driver
# detects this early and says so. See docs below / gamma_driver_mode in
# init/param.F90. `make run` is therefore NOT yet valid for long simulations.
#
# `make check` is EXPECTED TO FAIL at test-eq-schwarzschild-data until this is
# fixed. That test measures the driver residual on EXACT stationary 1+log
# equilibrium data; a gauge that is a genuine fixed point must leave only a
# discretisation-level residual. The default (gamma_driver_mode 0) leaves
# ~3.1e-2 versus ~9.4e-4 for the mode-4 candidate -- ratio ~33 -- because its
# driver equation has no target profile, so the shift is anchored to nothing.
# Measured consequence: horizon migrates r_min 1.60 -> 1.44, horizon finder
# loses the surface by step 4000, det(gtilde) = -0.85 at the excision boundary
# at step 6848. Do NOT relax that gate to get a green build; change the default
# gauge. See helper/test_eq_schwarzschild_data.F90 for the gate and its notes.
#
# CAREFUL: do NOT treat a green gate here as licence to change the default.
# gamma_driver_mode 4 passes that gate (it is the exact fixed point for
# SPHERICAL equilibrium data) but that has NOT been shown to fix the Kerr run.
# The fixture sets bh_spin=0; production runs a=0.8 Kerr-Schild, which is not a
# 1+log slice at any spin, and the tree has no stationary Kerr data at all.
#
# Resolution evidence (mode 0, apparent-horizon r_min drift from 1.5999,
# compared at equal PHYSICAL time -- not equal step count):
#         t       48x96      96x192
#      0.4466     0.0429      0.0395
#      0.8886     0.1277      0.1274
#      1.3328     0.1558      0.2364
#      1.7770     0.1640      0.3452
# The two resolutions agree early and diverge later, and 48x96 STALLS at r~1.44
# while 96x192 keeps marching. The stall is a discretization artifact: the coarse
# grid appears to survive only because it cannot follow the true horizon motion.
#
# But r_min is NOT a trustworthy stationarity measure here. The gauge-invariant
# horizon quantities converge under refinement (exact: area 40.212, M_irr
# 0.8943, J 0.8), while r_min diverges:
#                       48x96 area   96x192 area
#          r_excise=0.7   -14.9%        -0.41%
#          r_excise=1.0   +14.5%        -2.15%
# The r_excise sensitivity of the area collapses from a 29% spread at 48x96 to
# 1.8% at 96x192 (~17x under 2x refinement). So the inner boundary is
# RESOLUTION-LIMITED, not missing a characteristic condition: at adequate
# resolution the excision treatment converges. What survives is genuine slice
# evolution (Kerr-Schild is not a 1+log slice), which equilibrium data removes.
#
# Consequences:
#   * The default 48x96 grid is under-resolved; the physics it produces is not
#     trustworthy regardless of gauge. Use >= 96x192 for any physics claim.
#   * The driver aborts `make run` on r_min drift, which is the single
#     worst-behaved quantity in the code. Do not treat that abort as a verdict
#     on the physics; compare the invariants at equal physical time.
#   * The controlling blocker is the missing stationary Kerr data
#     (LONG_TERM_GAUGE_DRIFT_REPORT.md section 9, Phase 3).

# NOTE: make predefines FC=f77, so a plain `FC ?= gfortran` never takes effect
# and every recipe silently built with f77.  Only override the built-in default,
# so an explicit `make FC=...` still wins.
ifeq ($(origin FC),default)
FC      := gfortran
endif
FFLAGS  ?= -O2 -Wall -Wextra -fcheck=all -fbacktrace

TARGET  := bin/vacuum_kerr
MODDIR  := build/mod
PARFILE ?= parameter_files/vacuum_kerr.par
RUN_ENV ?=
RUN_DIR ?= .

HDF5_SHOW  := $(shell h5fc -show)
HDF5_FLAGS := $(wordlist 2,$(words $(HDF5_SHOW)),$(HDF5_SHOW))

FFLAGS  += $(filter -I%,$(HDF5_FLAGS))
LDLIBS  += $(filter-out -I%,$(HDF5_FLAGS))
# Keep this list in module-dependency order. Add any new module source before
# source files that USE it.
#
# Every non-driver module that a `make test` target compiles must also appear
# here, so the production binary is built from exactly the sources the test
# suite validates. `make check` enforces that.
SOURCES := \
	helper/kinds_mod.F90 \
	helper/fo_sbp.F90 \
	helper/fo_sat.F90 \
	init/param.F90 \
	init/grid.F90 \
	init/vars.F90 \
	init/fd_weights.F90 \
	helper/cartoon.F90 \
	init/fo_vars.F90 \
	helper/fo_constraints.F90 \
	helper/fo_constraint_damping.F90 \
	init/fo_complete_vars.F90 \
	helper/excision.F90 \
	helper/fo_curved_sat.F90 \
	helper/fo_normal_system.F90 \
	helper/excision_sat.F90 \
	helper/stencil.F90 \
	helper/geometry.F90 \
	helper/equilibrium_1pluslog.F90 \
	helper/eq_schwarzschild_data.F90 \
	helper/eq_kerr_trumpet.F90 \
	helper/eq_kerr_bssn_data.F90 \
	init/init_data.F90 \
	init/stencils.F90 \
	helper/constraints.F90 \
	helper/fd_excision.F90 \
	integrators/sommerfeld.F90 \
	integrators/ko_diss.F90 \
	integrators/bssn_rhs.F90 \
	integrators/fo_rhs.F90 \
	integrators/fo_complete_rhs.F90 \
	helper/horizon.F90 \
	integrators/time_step.F90 \
	helper/output_test.F90 \
	drivers/vacuum_kerr.F90

SOURCES_NO_DRIVER := $(filter-out drivers/vacuum_kerr.F90,$(SOURCES))

TESTS := test-excision-sat test-fo-sbp-sat test-fo-rhs test-fo-normal-system \
	test-cartoon \
	test-fo-curved-sat test-fo-validation test-fo-complete-layout \
	test-fo-complete-state test-fo-complete-rhs test-fo-layout test-fo-convergence \
	test-horizon-drift test-constraint-conv test-momentum-conv \
	test-equilibrium-1pluslog test-gamma-driver-rhs test-eq-schwarzschild-data \
	test-k-from-gauge

.PHONY: all run clean check check-parity diagnose-k-from-gauge diagnose-stencil-derivs diagnose-eq-kerr-data test-horizon-drift test-constraint-conv test-momentum-conv test-equilibrium-1pluslog test-gamma-driver-rhs test-eq-schwarzschild-data probe-momentum-terms diagnose-ks-stationarity diagnose-momentum-point diagnose-inner-boundary $(TESTS)

# Build-only: report targets that optionally take a parameter file.
bin/diagnose_stationary_ks_rhs: $(SOURCES_NO_DRIVER) helper/diagnose_stationary_ks_rhs.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(SOURCES_NO_DRIVER) \
		helper/diagnose_stationary_ks_rhs.F90 -o bin/diagnose_stationary_ks_rhs $(LDLIBS)

diagnose-ks-stationarity: bin/diagnose_stationary_ks_rhs
	./bin/diagnose_stationary_ks_rhs

# Build-only: report targets a compile-time error instead of stopping the build.
bin/diagnose_momentum_point: $(SOURCES_NO_DRIVER) helper/diagnose_momentum_point.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(SOURCES_NO_DRIVER) \
		helper/diagnose_momentum_point.F90 -o bin/diagnose_momentum_point $(LDLIBS)

diagnose-momentum-point: bin/diagnose_momentum_point
	./bin/diagnose_momentum_point parameter_files/vacuum_kerr.par \
		0.0628272251309 -1.535248

# test-cartoon now exercises the cartoon y-DERIVATIVE through the production
# stencil, so it needs the full module set rather than just cartoon.F90.
test-cartoon: $(SOURCES_NO_DRIVER) helper/test_cartoon.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(SOURCES_NO_DRIVER) \
		helper/test_cartoon.F90 -o bin/test_cartoon $(LDLIBS)
	./bin/test_cartoon

all: $(TARGET)

$(TARGET): $(SOURCES) | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(SOURCES) -o $@ $(LDLIBS)

bin $(MODDIR):
	mkdir -p $@

run: $(TARGET)
	cd $(RUN_DIR) && env $(RUN_ENV) $(abspath $(TARGET)) $(abspath $(PARFILE))

clean:
	rm -rf bin build
	# Stale .mod files in the tree root shadow $(MODDIR): gfortran searches the
	# current directory BEFORE any -I path, so an old params_mod.mod there wins
	# and silently hides newly added parameters.
	rm -f ./*.mod ./*.o

test-excision-sat: helper/kinds_mod.F90 init/param.F90 init/grid.F90 init/vars.F90 \
		helper/cartoon.F90 init/fo_complete_vars.F90 helper/excision.F90 \
		helper/fo_curved_sat.F90 helper/fo_normal_system.F90 helper/excision_sat.F90 \
		helper/test_excision_sat.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) helper/kinds_mod.F90 init/param.F90 \
		init/grid.F90 init/vars.F90 helper/cartoon.F90 init/fo_complete_vars.F90 \
		helper/excision.F90 helper/fo_curved_sat.F90 helper/fo_normal_system.F90 \
		helper/excision_sat.F90 helper/test_excision_sat.F90 -o bin/test_excision_sat
	./bin/test_excision_sat

test: $(TESTS)

test-gamma-driver-rhs: $(SOURCES_NO_DRIVER) helper/test_gamma_driver_rhs.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(SOURCES_NO_DRIVER) \
		helper/test_gamma_driver_rhs.F90 -o bin/test_gamma_driver_rhs $(LDLIBS)
	./bin/test_gamma_driver_rhs

test-eq-schwarzschild-data: $(SOURCES_NO_DRIVER) helper/test_eq_schwarzschild_data.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(SOURCES_NO_DRIVER) \
		helper/test_eq_schwarzschild_data.F90 -o bin/test_eq_schwarzschild_data $(LDLIBS)
	./bin/test_eq_schwarzschild_data

# `check` is the gate: the production binary must build from a source set that
# a superset-check covers, and the whole test suite must pass.
check: check-parity $(TARGET) test
	@echo "check: production build and full test suite OK"

# Every .F90 a test target compiles must be in SOURCES (the driver is
# obviously excluded). This is what keeps production from drifting away from
# what the tests actually exercise.
check-parity:
	@missing=0; \
	srcs=' $(SOURCES) '; \
	for f in $$(grep -ho '[a-z_]*/[a-z_]*\.F90' Makefile | sort -u); do \
	  case "$$f" in drivers/*|helper/test_*|helper/diagnose_*|init/test_*|helper/probe_*) continue;; esac; \
	  case "$$srcs" in \
	    *" $$f "*) ;; \
	    *) echo "check-parity: $$f is built by a test but missing from SOURCES"; \
	       missing=1;; \
	  esac; \
	done; \
	if [ $$missing -ne 0 ]; then \
	  echo "check-parity: FAILED"; exit 1; \
	fi; \
	echo "check-parity: production SOURCES covers every tested module"

test-fo-complete-rhs: helper/kinds_mod.F90 init/param.F90 init/grid.F90 init/vars.F90 \
		helper/cartoon.F90 init/fo_complete_vars.F90 helper/excision.F90 \
		init/fd_weights.F90 helper/stencil.F90 helper/geometry.F90 init/init_data.F90 init/stencils.F90 \
		integrators/bssn_rhs.F90 integrators/fo_complete_rhs.F90 \
		helper/test_fo_complete_rhs.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) \
		helper/kinds_mod.F90 init/param.F90 init/grid.F90 init/vars.F90 \
		helper/cartoon.F90 init/fo_complete_vars.F90 helper/excision.F90 \
		init/fd_weights.F90 helper/stencil.F90 helper/geometry.F90 init/init_data.F90 init/stencils.F90 \
		integrators/bssn_rhs.F90 integrators/fo_complete_rhs.F90 \
		helper/test_fo_complete_rhs.F90 -o bin/test_fo_complete_rhs
	./bin/test_fo_complete_rhs

test-fo-normal-system: helper/kinds_mod.F90 init/param.F90 init/grid.F90 init/vars.F90 \
		helper/cartoon.F90 init/fo_complete_vars.F90 helper/fo_normal_system.F90 \
		helper/test_fo_normal_system.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) helper/kinds_mod.F90 init/param.F90 \
		init/grid.F90 init/vars.F90 helper/cartoon.F90 init/fo_complete_vars.F90 \
		helper/fo_normal_system.F90 helper/test_fo_normal_system.F90 -o bin/test_fo_normal_system
	./bin/test_fo_normal_system

test-fo-curved-sat: helper/kinds_mod.F90 helper/fo_curved_sat.F90 helper/test_fo_curved_sat.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) helper/kinds_mod.F90 helper/fo_curved_sat.F90 \
		helper/test_fo_curved_sat.F90 -o bin/test_fo_curved_sat
	./bin/test_fo_curved_sat

test-fo-validation: helper/kinds_mod.F90 init/param.F90 init/grid.F90 init/vars.F90 \
		helper/cartoon.F90 init/fo_complete_vars.F90 helper/fo_curved_sat.F90 \
		helper/fo_normal_system.F90 helper/test_fo_validation.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) helper/kinds_mod.F90 init/param.F90 \
		init/grid.F90 init/vars.F90 helper/cartoon.F90 init/fo_complete_vars.F90 \
		helper/fo_curved_sat.F90 helper/fo_normal_system.F90 helper/test_fo_validation.F90 \
		-o bin/test_fo_validation
	./bin/test_fo_validation

test-fo-rhs: helper/kinds_mod.F90 init/param.F90 init/grid.F90 init/vars.F90 init/fo_vars.F90 \
		init/fd_weights.F90 helper/cartoon.F90 helper/excision.F90 helper/stencil.F90 \
		helper/geometry.F90 init/init_data.F90 init/stencils.F90 integrators/bssn_rhs.F90 \
		integrators/fo_rhs.F90 helper/test_fo_rhs.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) \
		helper/kinds_mod.F90 init/param.F90 init/grid.F90 init/vars.F90 init/fo_vars.F90 \
		init/fd_weights.F90 helper/cartoon.F90 helper/excision.F90 helper/stencil.F90 \
		helper/geometry.F90 init/init_data.F90 init/stencils.F90 integrators/bssn_rhs.F90 \
		integrators/fo_rhs.F90 helper/test_fo_rhs.F90 -o bin/test_fo_rhs
	./bin/test_fo_rhs

test-fo-layout: init/fo_vars.F90 helper/fo_constraints.F90 helper/test_fo_layout.F90 \
		helper/kinds_mod.F90 init/param.F90 init/grid.F90 init/vars.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) \
		helper/kinds_mod.F90 init/param.F90 init/grid.F90 init/vars.F90 \
		init/fo_vars.F90 helper/fo_constraints.F90 helper/test_fo_layout.F90 \
		-o bin/test_fo_layout
	./bin/test_fo_layout

test-fo-complete-layout: helper/kinds_mod.F90 init/param.F90 init/grid.F90 init/vars.F90 \
		helper/cartoon.F90 init/fo_complete_vars.F90 \
		helper/test_fo_complete_layout.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) helper/kinds_mod.F90 init/param.F90 \
		init/grid.F90 init/vars.F90 helper/cartoon.F90 init/fo_complete_vars.F90 \
		helper/test_fo_complete_layout.F90 \
		-o bin/test_fo_complete_layout
	./bin/test_fo_complete_layout

test-fo-complete-state: helper/kinds_mod.F90 init/param.F90 init/grid.F90 init/vars.F90 \
		helper/cartoon.F90 init/fo_complete_vars.F90 helper/test_fo_complete_state.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) \
		helper/kinds_mod.F90 init/param.F90 init/grid.F90 init/vars.F90 \
		helper/cartoon.F90 init/fo_complete_vars.F90 helper/test_fo_complete_state.F90 \
		-o bin/test_fo_complete_state
	./bin/test_fo_complete_state

test-fo-sbp-sat: helper/kinds_mod.F90 helper/fo_sbp.F90 helper/fo_sat.F90 \
		helper/test_fo_sbp_sat.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) helper/kinds_mod.F90 \
		helper/fo_sbp.F90 helper/fo_sat.F90 helper/test_fo_sbp_sat.F90 \
		-o bin/test_fo_sbp_sat
	./bin/test_fo_sbp_sat

test-fo-convergence: helper/kinds_mod.F90 helper/fo_sbp.F90 helper/test_fo_convergence.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) helper/kinds_mod.F90 helper/fo_sbp.F90 \
		helper/test_fo_convergence.F90 -o bin/test_fo_convergence
	./bin/test_fo_convergence

# Validates horizon_mod against exact Kerr AND attributes any horizon migration
# to the solver vs the evolved data.  Links the same production SOURCES as the
# vacuum_kerr binary (it drives rk4_step directly), so it cannot drift away from
# what production actually does.
test-horizon-drift: $(SOURCES) helper/test_horizon_drift.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(filter-out drivers/vacuum_kerr.F90,$(SOURCES)) \
		helper/test_horizon_drift.F90 -o bin/test_horizon_drift $(LDLIBS)
	./bin/test_horizon_drift $(PARFILE)

# Validates the stationary 1+log ("trumpet") equilibrium lapse/shift of
# Baumgarte et al., PRD 78 044015 (2008), arXiv:0810.0006, against the
# numbers published in that paper (C/M^2, alpha_AH = 0.376179, alpha_c, R_c,
# the two regularity conditions, alpha^2-beta^2 = 1-2M/R, and the
# asymptotic scalings).  Exterior lapse/shift only -- K_ij is NOT claimed.
test-equilibrium-1pluslog: $(SOURCES) helper/test_equilibrium_1pluslog.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(filter-out drivers/vacuum_kerr.F90,$(SOURCES)) \
		helper/test_equilibrium_1pluslog.F90 -o bin/test_equilibrium_1pluslog $(LDLIBS)
	./bin/test_equilibrium_1pluslog

# Validates constraints_mod::momentum_constraint (Brown et al. PRD 85 084004,
# eq. 2.19b) against exact Kerr-Schild data under 2x refinement.
test-momentum-conv: $(SOURCES) helper/test_momentum_conv.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(filter-out drivers/vacuum_kerr.F90,$(SOURCES)) \
		helper/test_momentum_conv.F90 -o bin/test_momentum_conv $(LDLIBS)
	./bin/test_momentum_conv $(PARFILE)

# Diagnostic: prints each candidate piece of the conformal/ADM momentum
# constraint on exact Kerr-Schild data at two resolutions.  The correct
# combination must tend to ZERO (exact data satisfies every constraint).
# FINDING SO FAR: DIV, C1, C2, C3 are all non-zero and FLAT under refinement,
# for a=0.8 AND a=0, so none of them (nor any +-1 combination) is the momentum
# constraint as currently written.  The momentum-constraint expression is NOT
# yet established -- see the notes in helper/constraints.F90.
probe-momentum-terms: $(SOURCES) helper/probe_momentum_terms.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(filter-out drivers/vacuum_kerr.F90,$(SOURCES)) \
		helper/probe_momentum_terms.F90 -o bin/probe_momentum_terms $(LDLIBS)
	./bin/probe_momentum_terms $(PARFILE)

# Measures the Hamiltonian-constraint residual on EXACT Kerr-Schild data under
# 2x refinement.  Kerr-Schild satisfies the constraints analytically, so this is
# pure truncation error; a flat (order ~0) result means a bug, not a resolution
# limit.
test-constraint-conv: $(SOURCES) helper/test_constraint_conv.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(filter-out drivers/vacuum_kerr.F90,$(SOURCES)) \
		helper/test_constraint_conv.F90 -o bin/test_constraint_conv $(LDLIBS)
	./bin/test_constraint_conv $(PARFILE)


# Build-only: WHERE the momentum-constraint residual lives.
# Answers, by measurement, the four hypotheses that had been floated for
# test-momentum-conv's failure.  All four are REJECTED:
#   H-axis   rejected.  rms with x < 4 dx removed changes the residual by
#            6%, 2%, 0.4% at 96/192/384 -- i.e. not at all.  The plateau
#            survives in rms_away: 5.39e-4 -> 5.34e-4 -> 5.83e-4.
#   H-polar  rejected, and BACKWARDS.  The polar bin (|z|/r in 0.8..1.0)
#            converges (ord -0.02); the EQUATORIAL bin (0.0..0.2) is worst
#            and grows (1.92e-4 -> 3.84e-4 -> 5.82e-4, ord -0.60).
#            Theory D's "polar horizon" localisation does not hold in r=[3,5].
#   H-inner-boundary rejected.  On a far annulus [6,8] the order is still -0.07.
#   H-excision rejected.  extrapolate_excised touches 0 cells in the annulus
#            (measured: max|du| over the annulus is exactly 0).
# What IS established:
#   * The residual is dx-INDEPENDENT (order ~0), at EVERY radius in [3,5] and
#     in [6,8], decaying smoothly as ~r^-3.5 from 1.17e-3 at r=3 to 2.08e-4
#     at r=5.  It is a smooth global field, not a localised defect.
#   * ALL SEVEN terms of M are individually dx-independent (ord 0.00), each
#     O(1e-2), cancelling to 5.7e-4 at every resolution.
#   * The Hamiltonian constraint converges at order 2 on the SAME data with
#     the SAME stencils (1.81e-4 -> 4.54e-5 -> 1.13e-5 -> 2.82e-6).
#   H-fixed-step-in-K REJECTED.  kerr_schild_adm builds K_ij with a HARDCODED
#            h = 1.0e-4 that does not scale with dx -- dx-independent by
#            construction, and K feeds pieces 2, 3, 5.  Tested directly by
#            setting h = dx/4 and rebuilding: 5.7412e-4 / 5.4435e-4 / 5.8533e-4
#            versus 5.7335e-4 / 5.4401e-4 / 5.8524e-4.  Unchanged to 4 digits.
#            (It is still an inconsistency worth fixing on its own merits --
#            every other operator in the code scales with dx -- but it is not
#            the cause of the plateau.)
#
# WHY IT MUST BE ALGEBRAIC, not discretisation: over dx = 0.0311 -> 0.0039 the
# residual goes 5.90e-4 -> 6.41e-4 (order -0.06).  A 4th-order stencil at
# dx = 0.0039 has truncation ~1e-9 relative, against a residual of 6e-4.  No
# discretisation error of any order can produce that, so the residual does not
# come from the stencil at all.
#
# ============================ ROUTE C (in progress) ===================
# Built: helper/eq_kerr_bssn_data.F90 (Kerr trumpet -> BSSN, K from the
# validated Lie form K_ij = L_beta gamma_ij / (2 alpha), derivatives at a FIXED
# step 1e-4 so the residual separates data/gauge from truncation);
# slicing_mode 3 in integrators/bssn_rhs.F90 (tabulated f(alpha));
# helper/diagnose_kerr_bssn.F90 (RHS + both constraints, Schwarzschild
# fixture on the same stencils as a scale reference).
#
# FRAME-CONVERSION BUG FOUND AND FIXED in helper/eq_kerr_trumpet.F90.
#   ktr_kerr_spatial returns the metric in the COORDINATE basis (r,theta,phi):
#   at large r it gives (1, r^2, r^2 sin^2 th), which is exactly flat space in
#   spherical coordinates, so g_phi_phi legitimately carries sin^2(th) and
#   det(g) -> 0 on the axis.  BOTH Cartesian assembly routines rotated that with
#   T = E g E^T, i.e. treated it as an ORTHONORMAL tensor.  The correct
#   relation is g_cart = J^{-T} g_coord J^{-1} with J_ia = dx^i/dq^a; verified
#   on flat space, where J g J^T gives g_xx = cos^2 th + r^2 sin^2 th (!= 1)
#   and only J^{-T} g J^{-1} returns 1.  Equivalent, and what was implemented:
#   convert to the orthonormal frame first, h_ab = g_ab/(s_a s_b) with
#   s = (1, r, r sin th), then rotate.  Added ktr_kerr_spatial_orth, which
#   cancels the sin^2(th) analytically so there is no cancellation on the axis.
#   THE SHIFT HAD THE SAME DEFECT: beta = matmul(E,w) omitted the r and
#   r sin(th) scale factors, so the shift was wrong by those factors too.
#   Effect of the fix:
#     det(gamma) along the equator: 84 -> 1695 (diverging, wrong)
#                                  21.96 -> 1.497 (approaching flat, correct)
#     det(gamma) vs polar angle at r=4: now FLAT (1.88..1.98 for |ct|<=0.8)
#     degenerate-metric rejections: 1196 -> 0.  Only the 160 genuine trumpet
#     cells remain (r in [0.06, 1.41]), as they should.
#     max|H| in the r in [3,5] annulus: 2.40e3 -> 2.82
#     phi at a sample point: 0.75 -> 0.0205, and gamma-tilde_xx 0.063 -> 1.167
#
# VALIDATED AFTER THE FRAME FIX (helper/diagnose_kerr_bssn.F90 probes):
#   * shift: |beta|/(2M/r) is constant to ~3% over r = 4..18 (2.79 -> 2.87),
#     i.e. the required 1/r far-field falloff.  So the shift IS in the right
#     basis once the scale factors are applied; the "shift may be in the
#     coordinate basis too" hypothesis is REFUTED -- the same patch fixed it.
#   * metric: at the equator gamma_cart = diag(h11, h33, h22) with
#     h11 = (r/(r-R0))^2, so gamma_xx -> 1 + 2 R0/r, i.e. an ADM mass of 2M,
#     which is exactly right for the ABLE-shifted DBM coordinate.
#     gamma_yy -> 1.002, gamma_xy = 0, and gamma_zz == 1 identically there
#     because rho^2 == r^2 at ct = 0 (NOT a bug).
#   So metric and shift are now verified in the far field, det(gamma) -> 1, and
#   the trumpet is the only rejected region.
#
# STILL NOT WORKING.  max|H| = 2.82 against Schwarzschild 2.69e-3 on identical
# stencils -- still ~1000x too large, so the Kerr data is not yet a valid
# solution and the f(alpha) table cannot be trusted until this is resolved.
# The data at the worst point is now genuine (not the placeholder), so this is
# a real residual, not an artefact.  Candidate causes, none yet tested:
#   * the SHIFT is now ruled out (validated above).  That leaves the LAPSE, or
#     the family not actually satisfying the vacuum constraint equations.
#     Nothing in the tree has ever checked that the DBM family obeys
#     R_ij = 0; Part A checked structure and asymptotics only.
#   * NEXT TEST, in order: (a) plot H against r -- if H -> 0 at large r and
#     grows inward, the lapse is the suspect; (b) if H is flat in r, the family
#     is not a vacuum solution as coded and route C is dead, which must be
#     established BEFORE the f(alpha) table or any horizon-drift run is
#     trusted.  Apply the constraint to the RAW spatial metric before conformal
#     flattening so a normalisation error cannot masquerade as curvature.
#
# HISTORY (kept deliberately).  First measurement gave
# max|H| = 2.40e3 against Schwarzschild 2.69e-3.  Tracing it:
#   * state_at rejected 160 cells as "outside the closed-form domain" -- that is
#     the genuine trumpet, r in [0.06, 1.41], and is expected.
#   * and 1196 cells because det(gamma) <= 0, spread over r in [1.41, 12.02],
#     with the worst at (x,z) = (0.758, 12.0) -- i.e. the POLAR AXIS at large r.
# A polar-angle probe at fixed r=4 shows det(gamma) falling as varpi^2:
#       ct=0.0 -> 482,  0.2 -> 463,  0.4 -> 404,  0.6 -> 306,  0.8 -> 165,
# and going non-positive by ct ~ 0.998 (varpi ~ 0.13).
# CONCLUSION: the metric returned by ktr_kerr_xyz is DEGENERATE ON THE AXIS.
# det(E g E^T) = det(g) for the orthonormal frame E, so if det(g) -> 0 as
# varpi -> 0 then g is a COORDINATE-BASIS metric carrying the varpi^2 factors
# of the azimuthal direction, and the Cartesian metric it is supposed to return
# is not being formed.  Consequences: det(gamma-tilde) = 1 cannot be imposed
# on-axis, and every near-axis cell is silently replaced by the flat
# placeholder -- which is exactly what the constraint is being evaluated on.
# NEXT: fix ktr_kerr_spatial / ktr_kerr_xyz in helper/eq_kerr_trumpet.F90 so
# the returned metric is the Cartesian one and det -> 1 (or at least stays
# positive and bounded) on the axis.  Until then no route-C constraint or RHS
# number is meaningful, and none should be quoted.
# NOTE the two failure modes must stay separate in the instrumentation: a
# domain rejection and a degenerate determinant are different bugs, and
# collapsing them into one boolean is what hid this at first.
#
# ------------------------- GAUGE GATE (still red, deferred) -----------
# Checked whether the missing-e^{4phi} Christoffel bug contaminated this gate.
# IT DOES NOT: helper/geometry.F90 builds Gam from gamma-tilde directly, with
# no e^{4phi} factor anywhere -- it is the CONFORMAL connection Gamma-tilde,
# which is a different object from the PHYSICAL connection that was wrong in
# momentum_constraint.  So the gate numbers below are uncontaminated and stand.
# Where the mode-0 defect lives (measured on the Schwarzschild fixture):
#   mode 4 residual = RHS_Gam            = 9.392e-4
#   mode 0 residual = RHS_Gam - lbeta_gam = 3.063e-2
#   => lbeta_gam ~ 3.06e-2, about 32x RHS_Gam.  So the mode-0 residual is
#   dominated ENTIRELY by its material-derivative conversion, not by RHS_Gam.
# mode 0 converts with  lbeta_gam = beta.d(beta) + Lambda-tilde div(beta)
# mode 3 converts with  beta.d(Lambda-tilde)
# On a stationary solution d_perp(Lambda-tilde) must equal RHS_Gam, and only
# the mode-3 form is structurally that quantity.  NEXT STEP, not yet done:
# expose RHS_Gam and both conversion forms from compute_rhs_point as an opt-in
# diagnostic output and measure which one equals RHS_Gam on this fixture.
# Do NOT flip the default to mode 4 to make the gate green: per the gate's own
# notes, mode 4 passes only because B=0 is its exact fixed point on a
# SPHERICAL fixture, while production starts from Kerr-Schild.
#
# ============================ RESOLVED ==============================
# test-momentum-conv NOW PASSES at order 2.  Two real bugs in
# helper/constraints.F90, both found by measurement, not by inspection:
#
# BUG 1 (fixed): d_dir(Atilde^{ij}) needs the WHOLE d_dir(gtilde^{ij})
# tensor, because it reads dgtinv(dir,a,c) and dgtinv(dir,b,d) for ALL c,d.
# The old code fused that with the dgtinv construction in one loop nest, so
# entries with c > b had not been written yet for the current dir.  dgtinv is
# a subroutine-scope local, never re-zeroed inside the cell loop, so those
# reads returned values left over from the PREVIOUS GRID CELL.  Only piece 1
# (d_j Atilde^{ij}) consumes the off-diagonal entries, and it was the only
# wrong piece -- pieces 2..7 reproduced bit-exactly.  Proof: zeroing dgtinv
# before the loop moved piece 1 from -3.403e-2 to -5.010e-2 against an
# independent replication's -5.053e-2.  Fixed by splitting into two passes.
#
# BUG 2 (fixed, this was the plateau): in the physical Christoffel,
#   gamma_ij = e^{4 phi} gtilde_ij   =>   d_dir gamma_ij
#                                          = e^{4 phi} [ 4 (d_dir phi) gtilde_ij
#                                                        + d_dir gtilde_ij ]
# BOTH terms carry e^{4 phi}.  The code had
#   dgamma = 4 (d_dir phi) gtilde + e^{4 phi} d_dir gtilde
# dropping the e^{4 phi} from the first term.  Because e^{4 phi} ~ 0.96 that
# is only a few percent, which is why it looked like a plausible small effect
# rather than a gross error -- and why it was invisible to every per-term
# scan, since S^{ij} and d_j S^{ij} never touch gamma and were correct.
#   d_jS  coded vs ADM agreed to 1e-12;  max|Gam_coded - Gam_ADM| = 1.1e-3.
# After the fix: 4.5e-12, and |M| coded 2.7470e-8 == |M| ADM 2.7469e-8.
# Result: rms|M| 4.975e-5 -> 1.231e-5 -> 3.058e-6, order 2.01/2.01.
#
# HOW IT WAS FOUND -- the method that worked, for the record.  Three earlier
# attempts failed and are recorded below as REJECTED, because the negative
# results are what narrowed the search.  The thing that actually worked was a
# CONTROLLED COMPARISON, not another scan:
#   1. Establish dx-independence (over 8x in dx).  Excludes discretisation.
#   2. Build an exact-derivative reference that shares no algebra with the
#      coded form (the ADM form D_j(K^{ij} - gamma^{ij} K)), and validate it
#      against the code FIRST via a replication control that must be bit-exact.
#   3. Split both forms the same way and diff term by term.
# The control is what makes step 2 trustworthy; without it the reference's
# numbers would have been meaningless (it caught two transcription errors).
#
# FOUND AND FIXED A REAL BUG (helper/constraints.F90, momentum_constraint).
#   d_dir(Atilde^{ij}) needs the WHOLE d_dir(gtilde^{ij}) tensor, because it
#   reads dgtinv(dir,a,c) and dgtinv(dir,b,d) for ALL c,d.  The old code fused
#   that into one loop nest with the dgtinv construction, so entries with
#   c > b had not yet been written for the current dir.  dgtinv is a
#   subroutine-scope local, never re-zeroed inside the cell loop, so those
#   reads returned values left over from the PREVIOUS GRID CELL.  Only piece 1
#   (d_j Atilde^{ij}) consumes the off-diagonal entries, and it is the only
#   term that was wrong -- pieces 2..7 reproduced bit-exactly.
#   Proof: zeroing dgtinv before the loop moved piece 1 from -3.403e-2 to
#   -5.010e-2, against an independent replication's -5.053e-2.  The fix splits
#   the nest into two passes.  It makes the code deterministic and correct; it
#   does NOT by itself make the residual converge.
#
# WHERE THE RESIDUAL REALLY IS (helper/diagnose_momentum_exact.F90).
#   That program replicates the constraint expression and feeds it EXACT
#   derivatives (fixed step 1e-5, independent of dx) rebuilt from the
#   Kerr-Schild closed form.  Its control -- run first, feeding the code's own
#   stencil derivatives and requiring a bit-exact match -- now PASSES at
#   0.000e+00 for all seven pieces, so the numbers it prints are trustworthy.
#   Result at r = 4.31 and 4.55:
#       |M| stencil = 6.930e-4 / 4.492e-4
#       |M| exact   = 7.421e-4 / 4.779e-4
#   With discretisation removed the residual is UNCHANGED.  That settles it:
#   the momentum-constraint residual is ALGEBRAIC, ~2% of the individual term
#   magnitudes, not a discretisation error.  This is consistent with the
#   dx-scaling above and with the Hamiltonian converging on the same data.
#   Still not localised to a specific algebraic term.
#   conformal_factor_mode = 1 tested and is WORSE (5.06e-3 vs 6.47e-4), so the
#   det(gamma-tilde) = 1 convention of mode 0 is the right one.
#
# NOT YET DONE: identifying WHICH term.  helper/diagnose_momentum_exact.F90
# replicates the constraint expression and feeds it EXACT derivatives (fixed
# step 1e-5, independent of dx) built from the Kerr-Schild closed form, which
# is the experiment that separates algebraic from numerical.  Its control --
# run first, feeding the code's own stencil derivatives and requiring a match
# to 1e-12 -- is what makes it trustworthy, and it currently FAILS: pieces
# 2, 3, 4, 5, 6, 7 reproduce momentum_constraint exactly, but piece 1 (the only
# term using d_j A-tilde^{ij}) does not, and zeroing the A-tilde derivatives
# does not reconcile it either.  So the exact-derivative |M| that program prints
# is a transcription artefact and must NOT be quoted as a result until the
# control passes.
#
# The last two together are a genuine contradiction that is NOT yet resolved:
# if the data and stencils are sound (Hamiltonian proves it), then the dx-
# independence of every M term cannot be a discretisation error.  The next
# experiment that would settle it is to evaluate M from ANALYTIC derivatives
# of the Kerr-Schild solution and check whether it vanishes to roundoff.
# NOTE: the earlier "ord" column printed here took log2 of raw SUMS, which
# grow 4x per refinement from cell count alone; it now uses the rms.

# MEASURED: the cartoon y-derivative is limited by the 2-POINT CENTRAL
# DIFFERENCE in stencil.F90, not by the reconstruction.  Control: feeding the
# ANALYTIC off-plane values into the same difference reproduces the stencil's
# d_y error to ~1% (1.77e-4/2.66e-4/3.17e-4/3.93e-4 vs 1.80e-4/2.66e-4/3.17e-4/
# 3.93e-4), while the per-ghost interpolation error is only ~1e-7.
#   => d1y = (yp-ym)/(2dy) is 2nd order while d_x, d_z are 4th order.  That is
#      the whole reason for the ~250x larger error constant on the y path.
# The reconstruction itself supports a 4th-order y stencil (its error is
# ~1e-7..3e-6 out to y/dx = 4, i.e. it contributes ~1e-5 to a derivative vs
# the 3e-4 the 2-point difference contributes).  Worth ~30x if implemented.
#
# CORRELATION LIMITS -- do not conflate the two:
#   test-momentum-conv FAILS at 96x192 because rms|M| does not converge
#   (5.73e-4 -> 5.44e-4 -> 5.85e-4, observed order -0.11).  A 2nd-order
#   truncation error CONVERGES, so the y-stencil order CANNOT explain that
#   plateau; it is a separate mechanism (report theory D, polar-horizon
#   contamination, is the live candidate).  Raising the default grid from
#   48x96 to 96x192 merely made the pre-existing plateau visible -- at
#   48/96/192 the same test passes.  Left red deliberately.

# Build-only: verification + family scan for the analytic Kerr trumpet data.
#   Part A  structural checks (trumpet, asymptotics, positivity)      PASS
#   Part B  K-from-gauge vs analytic, term by term                    PASS
#   Part C  R0 scan: no 1+log or maximal member in the family
#   Part D  f = (beta.dalpha)/(alpha^2 K) is exact for a=0: (1-alpha)/(3 alpha)
#   Part E  f is a function of alpha ALONE (needs the full 3-D K)
#   Part G  the alpha-dependence of c is real, not an h artefact  -> TABULATE
#   Part H  emits the converged f(alpha) table
# So the family IS usable, but under a tabulated Bona-Masso gauge, not under
# 1+log.  No numerical stationary-gauge solve is required.
# See the MEASURED RESULT block in helper/eq_kerr_trumpet.F90 for the traps.
bin/test_eq_kerr_data: helper/kinds_mod.F90 helper/eq_kerr_trumpet.F90 \
		helper/test_eq_kerr_data.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) helper/kinds_mod.F90 \
		helper/eq_kerr_trumpet.F90 helper/test_eq_kerr_data.F90 \
		-o bin/test_eq_kerr_data

diagnose-eq-kerr-data: bin/test_eq_kerr_data
	./bin/test_eq_kerr_data

# Build-only: per-operator convergence probe for the production stencil.
# Feeds an analytic spherically symmetric field (the structure of the areal
# Schwarzschild/Kerr metric) through compute_stencil on the y=0 plane and
# compares every derivative against the analytic value at three resolutions.
# One resolution per process: setup_grid allocates xg/zg without freeing.
bin/test_stencil_derivs: helper/kinds_mod.F90 init/param.F90 init/grid.F90 \
		init/vars.F90 init/fo_vars.F90 init/fd_weights.F90 helper/cartoon.F90 \
		helper/excision.F90 helper/stencil.F90 helper/geometry.F90 \
		init/init_data.F90 init/stencils.F90 helper/test_stencil_derivs.F90 \
		| bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) helper/kinds_mod.F90 init/param.F90 \
		init/grid.F90 init/vars.F90 init/fo_vars.F90 init/fd_weights.F90 \
		helper/cartoon.F90 helper/excision.F90 helper/stencil.F90 \
		helper/geometry.F90 init/init_data.F90 init/stencils.F90 \
		helper/test_stencil_derivs.F90 -o bin/test_stencil_derivs

# FINDING: every stencil operator CONVERGES.  Observed order, 96x192 -> 192x384:
#     d_x, d_z, d2x, d2z          : order ~4.0   (matches the 4th-order claim)
#     d_y, d2y, d2xy, d2xz, d2yz  : order ~1.7-2.0
# The y path is less accurate in absolute terms (d_y error on T_xy is 5.2e-4
# against 2.1e-6 for d_x on T_xx), so anything carrying a y index is limited
# to ~2nd order.  That is a resolution limit, not a defect.
#
# CORRECTION, and a warning: two earlier claims of a cartoon y-derivative
# defect were BOTH bugs in the probes, not in the stencil.
#   1. "d_y of beta_y has order ~0 and returns ~0" -- wrong component
#      bookkeeping in the probe.
#   2. "K reconstructed from the gauge fails in the y slots and plateaus under
#      refinement" -- the index accessors gtvar/atld_c/slotix keyed on 3*a+b and
#      handled only the upper triangle, so (2,1), (3,1) and (3,2) silently fell
#      through to a default and were scored against the WRONG tensor component.
# With the lower triangle added, test_k_from_gauge converges at order ~2 and
# PASSES.  Do not resurrect either claim.
bin/diagnose_momentum_map: $(SOURCES_NO_DRIVER) helper/diagnose_momentum_map.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(SOURCES_NO_DRIVER) \
		helper/diagnose_momentum_map.F90 -o $@ $(LDLIBS)

diagnose-momentum-map: bin/diagnose_momentum_map
	./bin/diagnose_momentum_map

diagnose-stencil-derivs: bin/test_stencil_derivs
	for n in 48 96 192; do \
	  GR_NX=$$n GR_NZ=$$((2*n)) ./bin/test_stencil_derivs; echo; \
	done

# Gate: K reconstructed from (alpha,beta,gamma) must reproduce the stored K of
# the exact stationary 1+log Schwarzschild trumpet AND converge (~2nd order).
# This is the machinery any analytic Kerr equilibrium data set depends on, so it
# is a production gate rather than a diagnostic.  Three resolutions, because the
# order matters as much as the value: a wrong-but-small error is still a defect.
test-k-from-gauge: $(SOURCES_NO_DRIVER) helper/test_k_from_gauge.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(SOURCES_NO_DRIVER) \
		helper/test_k_from_gauge.F90 -o bin/test_k_from_gauge $(LDLIBS)
	for n in 48 96 192; do \
	  GR_NX=$$n GR_NZ=$$((2*n)) ./bin/test_k_from_gauge || exit 1; \
	done

# Build-only: Work Package B inner-boundary instrumentation.
bin/diagnose_inner_boundary: $(SOURCES_NO_DRIVER) helper/diagnose_inner_boundary.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(SOURCES_NO_DRIVER) \
		helper/diagnose_inner_boundary.F90 -o bin/diagnose_inner_boundary $(LDLIBS)

diagnose-inner-boundary: bin/diagnose_inner_boundary
	./bin/diagnose_inner_boundary parameter_files/vacuum_kerr.par

# Build-only: ghost-fill accuracy vs exact Kerr-Schild (MARGIN_FINDINGS follow-up).
bin/diagnose_ghost_accuracy: $(SOURCES_NO_DRIVER) helper/diagnose_ghost_accuracy.F90 | bin $(MODDIR)
	$(FC) $(FFLAGS) -J$(MODDIR) -I$(MODDIR) $(SOURCES_NO_DRIVER) \
		helper/diagnose_ghost_accuracy.F90 -o bin/diagnose_ghost_accuracy $(LDLIBS)

diagnose-ghost-accuracy: bin/diagnose_ghost_accuracy
	./bin/diagnose_ghost_accuracy parameter_files/vacuum_kerr.par
