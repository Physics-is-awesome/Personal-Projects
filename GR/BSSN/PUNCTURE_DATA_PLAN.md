# Plan: Puncture initial data and gauge for moving-puncture evolution

## Goal

Replace Kerr-Schild (and route-C trumpet) initial data with **puncture data**
that is regular at r=0 and compatible with the 1+log + Gamma-driver gauge, so
the interior evolves stably without excision.

## Why the current data fails

- Kerr-Schild: alpha never vanishes; 1+log slicing collapses it dynamically,
  producing strong interior gradients and det(gtilde)<0 by ~step 24.
- Route-C trumpet: only valid for R > R0 (bh_mass); the interior is a
  non-existent trumpet throat, so evolving it injects O(1) constraint violation.

## Approach (incremental)

### Step A — Conformally flat puncture (Schwarzschild, spin 0) [simplest]

1. Set conformal metric gamma-tilde_ij = delta_ij (flat).
2. Solve the Hamiltonian constraint for psi: psi = 1 + M/(2r) (Bowen-York
   single puncture, analytic for Schwarzschild).
   - phi = ln psi.
   - alpha = psi^{-2} (puncture lapse) or 1 (then let gauge collapse).
   - K_ij = 0, beta = 0, Gtilde = 0.
3. This is analytic and regular-ish (psi ~ 1/r at origin, but BSSN variables
   stay finite).  No solver needed.

### Step B — Add spin (Kerr) via Bowen-York extrinsic curvature

1. Compute the Bowen-York A_ij for given mass M and spin S.
2. Solve the (linear) momentum constraint (analytic for Bowen-York).
3. Hamiltonian constraint becomes nonlinear in psi: solve
   (D^2 psi) + (1/8) psi^{-7} A_ij A^ij = 0 with psi -> 1+M/(2r).
   - Requires a nonlinear elliptic solver (Newton + multigrid or FFT).
   - Start with a simple iterative (Jacobi/SOR) solver on the existing grid;
     acceptable for a first working run.

### Step C — Gauge and driver tuning

1. Use slicing_mode = 0 (1+log) and gamma_driver_mode = 1 or 4.
2. Initialise Gbar from the data (compute_conformal_connections already does
   this for mode 1).
3. Tune gauge_eta (~2/M) and puncture_sign (-1).
4. Remove puncture_lapse_mode (data already has vanishing lapse).

### Step D — Validation

1. t=0 constraints finite over the whole domain including origin.
2. Run >= 1000 steps without NaN.
3. Horizon invariants (area, M_irr, J) within 1% over 5000 steps.
4. Convergence test in L2 excluding the puncture point.

## Implementation order

1. Add `set_puncture_id` to init/init_data.F90 (Step A, analytic Schwarzschild).
2. Wire `initial_data_mode = 2` to it; update param defaults and validation.
3. Test build + smoke run; expect much longer survival.
4. Add Bowen-York A_ij (Step B) for spinning case.
5. Add iterative Hamiltonian solver.
6. Tune gauge and validate (Step C/D).

## Files to touch

- init/init_data.F90 (new subroutine set_puncture_id)
- init/param.F90 (new mode 2, remove trumpet r_excise constraint)
- drivers/vacuum_kerr.F90 (select new mode)
- helper/constraints.F90 (ensure origin not NaN)

## Risk

- The elliptic solver (Step B) is the hard part; Step A alone gives a
  non-spinning black hole, sufficient to validate the pipeline.
- Bowen-York data has a "junk radiation" burst; long-run stationarity will
  be approximate until better data (e.g. quasi-equilibrium) is used.