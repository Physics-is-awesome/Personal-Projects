# Plan: Convert BSSN code from excision to moving puncture

## Goal

Replace the frozen-excision inner boundary with a **moving puncture**
evolution: no excised region, no ghost fill, no inner boundary operator.
The black-hole singularity is handled by the gauge (1+log lapse collapsing
to zero + Gamma-driver shift) and the puncture is allowed to advect.  The
entire grid, including the origin/axis, becomes evolved physical domain.

This is a *structural* change, not a parameter tweak: excision is woven into
initial data, RHS, RK stages, diagnostics, horizon finder, and the Makefile
test suite.  The plan below is ordered so each phase leaves a buildable,
runnable code with a measurable regression target.

## What "moving puncture" implies for this codebase

1. **No excision mask.** `excision_mod`, `excision_sat_mod`, `fd_excision_mod`,
   and all `is_excised`/`extrapolate_excised` calls disappear.  Every grid
   point is evolved.
2. **No ghost fill.** `rk4_step` no longer calls `extrapolate_excised` between
   stages; `u` is a single consistent state.
3. **No `r_excise` / `excision_order`.** Remove from `params_mod` and `.par`
   files, or keep as deprecated no-ops temporarily for compatibility.
4. **Puncture-compatible initial data.** Prefer route-C trumpet (DBM) data
   (`initial_data_mode = 1`) with `conformal_factor_mode` consistent; Kerr-Schild
   data has no puncture and will not collapse correctly.  Make trumpet the
   default.
5. **Puncture gauge.** `slicing_mode = 0` (1+log, f=2/alpha) and a
   non-frozen Gamma-driver (`gamma_driver_mode = 3` or `4`), with
   `puncture_lapse_mode` producing a lapse that vanishes at the puncture.
6. **Origin/axis handling.** Cartoon axisymmetry at x=0 must provide regular
   one-sided derivatives; no excision hole means the origin is on the grid and
   must be evolved with regularity conditions.
7. **Horizon finder.** Already interpolates the evolved metric; remove the
   excision-buffer rejection or redefine it against a tiny radius around the
   puncture.
8. **Diagnostics.** Remove inside-horizon shells and mask-based NaN checks;
   report over the whole domain with a small-radius exclusion only for the
   puncture point.

## Current excision touchpoints (inventory)

- `helper/excision.F90`, `helper/excision_sat.F90`, `helper/fd_excision.F90`
- `integrators/time_step.F90`: `extrapolate_excised` (4 calls/stage), `is_excised`
  (projection), `apply_legacy_excision_sat`, `lapse_floor` clamp.
- `integrators/bssn_rhs.F90`, `integrators/fo_rhs.F90`, `integrators/ko_diss.F90`,
  `integrators/sommerfeld.F90`: `is_excised` gating and safe-stencil checks.
- `init/param.F90`: `r_excise`, `excision_order`, `excision_sat` (+ validation).
- `drivers/vacuum_kerr.F90`: heavy use of `is_excised`, `extrapolate_excised`,
  `set_excision_background`, `excision_characteristic_check`, horizon-set-excise,
  shell stats keyed to `r_excise`, ring diagnostics.
- `helper/stencil.F90`, `helper/cartoon.F90`, `helper/horizon.F90`,
  `helper/constraints.F90`: mask arguments and excision-aware stencils.
- `helper/*diagnose*.F90`, `helper/test_*.F90`: many assume an excised interior.
- `parameter_files/*.par`: `r_excise`, `excision_order`, `excision_sat`.
- `Makefile`: builds `excision*.F90`, tests `test_excision_sat`.

## Phased plan

### Phase 0 — Branch and baseline (no code changes)

1. Create branch `moving-puncture`.
2. Run the current green gate (`make` + acceptance checks) and archive
   `run_info.txt` / diagnostics as the "excision baseline" for comparison.
3. Document which tests are excision-specific so they can be retired, not
   silently broken.

**Exit criterion:** clean build + recorded baseline numbers.

### Phase 1 — Decouple RHS from the mask (lowest risk, reversible)

1. In `bssn_rhs.F90` / `fo_rhs.F90`: replace `if (is_excised(i,k)) cycle` with
   unconditional evolution; guard only the true domain boundaries (i=1 axis,
   i=Nx outer, k=1,Nz outer).
2. In `stencil.F90`: remove `stencil_is_safe_*` gating by excision status;
   keep boundary/axis safety.  Derivatives near the origin use the existing
   Cartoon regularity, not excision.
3. In `ko_diss.F90`: apply KO everywhere (or with a tiny-radius taper at the
   puncture) instead of skipping excised points.
4. Keep `extrapolate_excised` as a no-op shim temporarily so callers compile.

**Exit criterion:** code compiles; a short run does not crash; NaN check
passes outside a small puncture radius.

### Phase 2 — Remove ghost fill from the time integrator

1. Delete the four `extrapolate_excised(u/utmp)` calls in `rk4_step`.
2. Remove `apply_legacy_excision_sat` calls (or make them no-ops).
3. In `project_bssn_constraints`: remove `is_excised` skip; instead skip only
   the single puncture point (r < epsilon) or apply a lapse floor there.
   Keep the lapse floor clamp but allow alpha -> 0 (raise floor handling: do
   not clamp above a tiny epsilon that would break puncture gauge; consider
   removing the floor entirely once stable).
4. Remove `set_excision_background` usage.

**Exit criterion:** RK4 runs without ghost fill; constraints project over the
whole domain.

### Phase 3 — Initial data and gauge defaults

1. Switch defaults: `initial_data_mode = 1` (trumpet), `slicing_mode = 0`
   (1+log), `gamma_driver_mode = 4` (or 3), `puncture_lapse_mode = 1`,
   `conformal_factor_mode` validated against trumpet.
2. Remove route-C's `r_excise > bh_mass` constraint (no excision); instead
   require the grid to cover the trumpet interior down to the puncture.
3. Ensure `compute_conformal_connections` and boundary fill work with no
   excised interior (they currently may assume a mask).
4. Add a puncture-position parameter (default origin) and offset data
   routines; keep `bh_center_*` working.

**Exit criterion:** t=0 constraints finite over the whole domain including
origin; no NaN.

### Phase 4 — Driver, diagnostics, horizon finder

1. `drivers/vacuum_kerr.F90`:
   - Remove `extrapolate_excised`, `set_excision_background`,
     `excision_characteristic_check`, `kerr_radius_squared`-based mask tests.
   - Replace ring/inside-horizon diagnostics with whole-domain stats plus a
     small-radius exclusion for the puncture.
   - Keep outer Sommerfeld and KO.
   - Remove `horizon_set_excision` excision coupling or pass radius 0.
2. `helper/horizon.F90`: remove excision-buffer rejection (`reject_limit`)
   or set it to a tiny value; rely on the found surface not crossing the
   puncture.
3. Remove `excision_characteristic_check` callers; characteristic info now
   only at outer boundary.

**Exit criterion:** driver runs N steps, writes HDF5, horizon finder reports
invariants.

### Phase 5 — Retire excision modules and params

1. Delete `helper/excision.F90`, `helper/excision_sat.F90`,
   `helper/fd_excision.F90` (or move to `archive/`).
2. Remove from `Makefile` and module lists.
3. Remove `r_excise`, `excision_order`, `excision_sat` from `params_mod`,
   `read_params`, overrides, and `.par` files (or keep parsed-and-ignored for
   one release).
4. Update `docs/bsssn-kerr-axi.tex` chapter 4 (boundaries) and file tour.

**Exit criterion:** clean build with no excision symbols; grep for `excision`
returns only historical docs/run_archive.

### Phase 6 — Tests and acceptance

Retire excision-specific tests (`test_excision_sat`, `test_fo_*` excision
variants) and add/adjust:
- Convergence test in L2 over the whole domain (excluding puncture point).
- Stationarity: horizon area, M_irr, J drift < tolerance for >= 1000 steps.
- Lapse collapses to ~0 at puncture and stays positive elsewhere.
- No NaN/Inf anywhere except possibly the single puncture point.
- Long-run (>= 10000 steps) stability at fiducial resolution.

Acceptance gate: horizon invariants within 1% over 5000 steps at Nx=96, Nz=192
with 1+log + Gamma-driver, no crashes.

## Key risks

- **Origin regularity.** Without an excision hole, the axis x=0 becomes a
  true coordinate singularity location; Cartoon regularity must hold for all
  evolved fields, including conformal connections.  Existing code may rely on
  the hole to avoid the origin.
- **Puncture gauge tuning.** 1+log + Gamma-driver has free parameters
  (eta, damping); wrong choices cause gauge drift or blow-up.  Start from
  known moving-puncture parameters (eta ~ 2/M, driver factor 3/4).
- **Constraint projection at the puncture.** det(gtilde) may be singular at
  r=0; projection must skip or regularize the puncture point.
- **KO dissipation at the puncture.** Standard KO can be unstable at a
  puncture; may need a taper or higher-order dissipation.
- **Horizon finder.** Must handle a moving puncture (coordinate drift) and
  not reject surfaces near the origin.
- **Performance.** Evolving the interior adds ~ (r_excise/dx)^2 points per
  slice; minor.

## Suggested execution order for minimal breakage

Phase 1 -> Phase 2 -> (run smoke test) -> Phase 3 -> Phase 4 -> Phase 5 ->
Phase 6.  Do not delete modules until Phase 4 runs are stable; keep shims.

## Files most likely to change (non-exhaustive)

- `integrators/time_step.F90`
- `integrators/bssn_rhs.F90`, `integrators/fo_rhs.F90`
- `integrators/ko_diss.F90`
- `init/param.F90`, `init/init_data.F90`, `init/vars.F90`
- `drivers/vacuum_kerr.F90`
- `helper/stencil.F90`, `helper/cartoon.F90`, `helper/constraints.F90`
- `helper/horizon.F90`
- `helper/excision*.F90`, `helper/fd_excision.F90` (delete in Phase 5)
- `parameter_files/*.par`
- `Makefile`
- `docs/bsssn-kerr-axi.tex` (chapter 4, file tour)

## Out of scope (for this plan)

- Adding AMR or higher-order (beyond 4th) stencils.
- Changing the outer boundary treatment (Sommerfeld stays).
- Replacing BSSN with CCZ4/Z4c.