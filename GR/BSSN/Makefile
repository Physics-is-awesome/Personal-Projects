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
	test-equilibrium-1pluslog test-gamma-driver-rhs test-eq-schwarzschild-data

.PHONY: all run clean check check-parity test-horizon-drift test-constraint-conv test-momentum-conv test-equilibrium-1pluslog test-gamma-driver-rhs test-eq-schwarzschild-data probe-momentum-terms diagnose-ks-stationarity diagnose-momentum-point diagnose-inner-boundary $(TESTS)

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
