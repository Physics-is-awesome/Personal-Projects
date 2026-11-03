# Long-Term Kerr Evolution: Continuation Plan

**Date:** 2026-11-03  
**Scope:** `GR/BSSN` axisymmetric vacuum Kerr solver (BSSN + Cartoon + DBM route-C data/gauge).  
**Supersedes for execution:** `LONG_TERM_FIX_PLAN.md`, and the unchecked action lists in `LONG_TERM_GAUGE_DRIFT_REPORT.md` §9/§13. Those documents remain the evidence record; this file is the ordered work plan.  
**Status:** the solver is stable to `t/M ≈ 2.66` at 192×384, but it is **not** stationary and is **not** yet a long-term solution.

---

## 0. Definition of done

A run counts as a long-term result only when all of the following hold:

1. The same validated configuration runs at three resolutions to the **same physical time** `T/M`.
2. The apparent horizon is found throughout, and its area, irreducible mass, and spin converge with resolution at equal time.
3. Hamiltonian and all three momentum constraints are bounded and converge with resolution in the exterior, horizon shell, and near-excision band.
4. No non-positive conformal determinant, NaN, or invalid metric occurs.
5. The gauge/data pair is stationary to discretisation error, or any residual drift is quantified and bounded rather than hidden.
6. The invariant gate remains honest: the existing 5% `M_irr`/area abort is not relaxed. The programme target is <1% drift at the accepted final time.
7. The result is reproducible from a clean commit, fixed parameter file, and archived logs/output.

**Provisional duration target:** `T/M = 100`, with staged checkpoints at `T/M = 10` and `T/M = 30`. The historical 14,000-step failure is a regression point, not the acceptance target. Fix the final `T/M` before starting the acceptance campaign; do not change it after a run starts.

A run that merely survives longer, or improves one constraint component, is an experiment, not a fix.

---

## 1. Current state (evidence snapshot)

### 1.1 Live production configuration

From `parameter_files/vacuum_kerr.par`:

| Setting | Value |
|---|---|
| Grid | `96×192`, `xmax=zmax=12M` |
| Spin / mass | `a=0.8M`, `M=1` |
| Initial data | `initial_data_mode=1` (Dennison–Baumgarte–Montero route-C trumpet) |
| Slicing | `slicing_mode=3` (tabulated scalar Bona–Masso `f(alpha)`) |
| Shift | `gamma_driver_mode=4` (non-advective Gamma-driver, `B=0` fixed point) |
| Excision | `r_excise=1.5M`, `excision_order=3`, radius-only mask |
| Boundary | Sommerfeld outer, extrapolation inner, SAT disabled |
| Dissipation | `ko_eps=0.2` |
| Domain | `12M` |

`vacuum_kerr_192.par` is the same physics at `192×384`.

### 1.2 Verified results

- **Initial data constraints improve under refinement.** Route-C DBM Hamiltonian/momentum residuals and the `K`, `Atilde`, `phi` RHS decrease at 96×192 → 192×384 → 384×768 (`baseline/STEPS_1_3_CURRENT_STATUS.md` §3). The DBM momentum point comparison also converges (`STEPS_1_3_CURRENT_STATUS.md` §2).
- **The route-C data/gauge pair is close to stationary, but not exact.** In `[3M,5M]`, the 192×384 residuals are approximately `max|H|=6.16e-4`, `max|M|=5.90e-5`, `max|RHS_K|=4.29e-5`, `max|RHS_A|=6.95e-4`, `max|RHS_phi|=2.62e-6`, but `max|RHS_alpha|=4.36e-4` and it **plateaus** at 384×768 (`4.34e-4`). This is a gauge-law mismatch, not a stencil-convergence failure.
- **The high-resolution horizon-finder coverage bug has a fix.** Offsetting the finder mask inward while keeping the evolution excision at `1.5M` restores the initial horizon at 384×768 (`dA/A_Kerr ≈ -9e-6` in `baseline/LAPSE_HORIZON_DIAGNOSTIC.md`).
- **A matched 192×384 run reaches `t=2.6586` (12,000 steps) with the horizon found throughout.** Exterior `max|H|` / RMS are `8.2846e-2` / `2.2411e-3`; the horizon area drifts from `-0.004%` to `-0.643%` relative to Kerr. This drift is real: independent re-solves at steps 0/4000/8000/12000 converge and reproduce it (`STEPS_1_3_CURRENT_STATUS.md` §3, report §13.10).
- **The 96×192 matched baseline reaches `t=2.6655` without a determinant abort**, with exterior `max|H|=3.0323e-1`, RMS `8.6009e-3`, `max|M|=6.2315e-2`. It loses the finder late in the run. The status document records loss after step 4100; `LAPSE_HORIZON_DIAGNOSTIC.md` records an earlier rejection at step 1900 for a different finder-mask variant. Reconcile this from archived logs before quoting either number.
- **Evolved momentum is not an axis-only defect.** At 192×384 and `t≈2.66`, the near-axis and same-radius off-axis norms are `7.02e-3` and `1.17e-2`; the maximum is immediately outside excision (`r_KS≈1.515`, `|M|=2.27e-2`). At 96×192 the inner-band maximum is `1.29e-1`. The bulk `r_KS=3..8` residual converges, but the outermost band does not (`1.56e-2`/`2.00e-3` at 192×384 versus `9.41e-3`/`1.49e-3` at 96×192).
- **A scalar `f(alpha)` cannot fit all radii.** The dense equatorial table gives weighted lapse residual RMS `5.53e-2`; a binned fit gives `2.28e-2`, but the smooth candidate was rejected because the full `[3M,5M]` lapse RHS rose to `5.19e-3`/`2.34e-3` at both 192×384 and 384×768, against `4.36e-4`/`1.03e-4` for the current table. Shell fits trade `1.55..3M` against `5..8M` (report §13.11).
- **A spatial lapse-coefficient experiment is already scaffolded but unvalidated.** `slicing_mode=4` uses `initialize_kerr_f_cache()`/`get_kerr_f_cache()` in `helper/eq_kerr_bssn_data.F90` to precompute the required `f(x,z)` for the DBM state. It is selected only by an environment/parameter override and has no recorded acceptance run. This is the most direct continuation of the §13.11 recommendation.

### 1.3 Current gate status

At the time of this examination:

- The production binary builds, but **`make check` is red at `check-parity`**: `helper/fo_constraint_fields.F90` is compiled by test/diagnostic targets but is missing from `SOURCES`. The module is used only by `test-fo-constraint-fields`, `test-fo-constraint-principal`, and `diagnose_route_c_constraint_jacobian`; production does not `use` it.
- **`make -k test` has two further build failures**, both caused by the same drift: `integrators/bssn_rhs.F90` now uses `get_kerr_f_cache` from `eq_kerr_bssn_data_mod`, but the explicit source lists for `test-fo-rhs` and `test-fo-complete-rhs` omit `helper/eq_kerr_bssn_data.F90` (and its dependency `helper/eq_kerr_trumpet.F90`).
  - `test-fo-rhs` fails to compile: `Error: Symbol ‘get_kerr_f_cache’ referenced at (1) not found in module ‘eq_kerr_bssn_data_mod’`.
  - `test-fo-complete-rhs` compiles against a stale module, then fails to link: `undefined reference to __eq_kerr_bssn_data_mod_MOD_get_kerr_f_cache`.
  - Either way the failure is a missing dependency in the `Makefile`, not a physics failure.
- The rest of `make -k test` passes, including `test-horizon-drift` (~8 min at 96×192), the constraint/momentum convergence targets, the gauge-driver RHS checks, and `test-gauge-f-continuity`. The two link failures above are therefore the complete current test-gate breakage, not a symptom of a wider suite failure.
- `make check` must be green again before any long run is treated as a baseline. Fix by (a) adding `helper/fo_constraint_fields.F90` to `SOURCES` in module order (it has no production callers but the parity rule deliberately requires every tested non-driver module to be in the production source set), and (b) adding `helper/eq_kerr_trumpet.F90` and `helper/eq_kerr_bssn_data.F90` to the explicit source lists for `test-fo-rhs` and `test-fo-complete-rhs`. Do not bypass the gate silently or leave a stale `.mod` as the only reason a target links.

### 1.4 Documentation is not a reliable current baseline

The live parameter file and the newest status files have moved ahead of the reference documents. In particular:

- `docs/bsssn-kerr-axi.tex` still says production is Kerr–Schild data + route-C gauge at `r_excise=0.8`, and still states that route-C data cannot be initial data. That is no longer the live configuration.
- `LONG_TERM_EVOLUTION_FIX.md` still describes the older Kerr–Schild + `r_excise=0.8` production configuration.
- `LONG_TERM_FIX_PLAN.md` is an unfinished checklist.
- `MOMENTUM_FINDINGS.md` concludes that `constraints.F90` should be rewritten; later work fixed the two momentum-assembly bugs and validated the DBM initial residual. Mark the earlier conclusion as historical.
- `STEPS_1_3_CURRENT_STATUS.md` is the best current status, but its horizon-loss step and initial-area numbers need reconciliation with `LAPSE_HORIZON_DIAGNOSTIC.md` and the archived output.

Documentation consolidation is part of Phase 0, not an afterthought: otherwise the next run will again be configured from a stale statement.

---

## 2. Working rules

1. **Preserve the honest gates.** Do not relax the 5% invariant abort, hide horizon failures, or clamp the determinant to manufacture a pass.
2. **Compare equal physical time, not equal step count.** `t` is accumulated by `t=t+dt`; a smaller step is a stability diagnostic, not a physical result.
3. **Change one thing at a time.** Do not change gauge, initial data, resolution, excision radius, boundary operator, and dissipation in the same experiment.
4. **Every operator change needs a controlled comparison.** A new stencil or boundary condition must be compared against the existing one on the same frozen state and same grid before it is promoted.
5. **A result is not accepted from one resolution.** Report convergence or state explicitly that convergence is unavailable.
6. **Do not tune the target values.** Exact Kerr area/mass/spin are outputs to compare against, not shooting parameters.
7. **Keep experimental modes reachable.** Legacy/control configurations stay available under explicit parameters even after production defaults change.

---

## 3. Work plan

### Phase 0 — Freeze a reproducible baseline (1–2 days)

**Goal:** make the current state measurable before changing physics.

- [ ] **P0.1 Commit or branch the current tree.** The worktree contains thousands of uncommitted lines across many files. Create a dedicated branch, commit the current BSSN work only, and record the exact state. Do not mix unrelated changes from the enclosing repository.
- [ ] **P0.2 Archive the exact diff and configuration.** Save `git status --short`, `git diff`, the two production `.par` files, compiler version/flags, HDF5 version, `make` log, and the current `make check` output under a run archive outside `output/`.
- [ ] **P0.3 Fix the red build gate.** Resolve both known failures:
  - add `helper/fo_constraint_fields.F90` to `SOURCES` so `check-parity` stops reporting it (or document a test-only exemption inside the gate itself; do not edit the check to skip it silently);
  - add `helper/eq_kerr_trumpet.F90` and `helper/eq_kerr_bssn_data.F90` to the explicit source lists of `test-fo-rhs` and `test-fo-complete-rhs`, matching the new `get_kerr_f_cache` dependency in `integrators/bssn_rhs.F90`.
  
  Then run `make clean && make -j4 && make check` from a clean tree. The gate must pass before a baseline is declared; a pass that depends on a stale `build/mod` is not a pass.
- [ ] **P0.4 Separate run directories.** Never reuse an output directory across configurations. The current `output/` mixes runs from different dates and is not a self-consistent baseline (`STEPS_1_3_CURRENT_STATUS.md` §1). Move or delete generated artifacts; keep archived logs in a dedicated run archive.
- [ ] **P0.5 Add run metadata.** Write a small `run_info.txt` (or HDF5 attributes) at startup containing: git hash, full parameter file, environment overrides, compiler/flags, HDF5 version, grid, start time. This is cheap and prevents another “which configuration produced this?” ambiguity.
- [ ] **P0.6 Add a baseline regression target.** A `make` target that runs the 96×192 6000-step baseline with `GR_HZ_ABORT=0` and checks the documented final exterior `H`/`M` values and the absence of determinant failures. Keep tolerances explicit; do not make it a long acceptance run.
- [ ] **P0.7 Reconcile the status documents.** Update `docs/bsssn-kerr-axi.tex` and the older root-level fix documents to point at the live route-C configuration, or mark them superseded. Resolve the 1900-vs-4100 horizon-loss discrepancy from logs.
- [ ] **P0.8 Reconcile the horizon-finder numbers.** Re-run the zero-step horizon check at 96×192, 192×384, and 384×768 with the current finder-mask offset and record the initial `dA/A_Kerr` in one table.

**Gate 0:** clean build, `make check` green, baseline reproduced in a fresh directory, run metadata archived, and status documents consistent with the live configuration.

---

### Phase 1 — Close the stationarity gate (Gate A) (1–2 weeks)

**Goal:** turn the DBM route-C data + gauge from “close” into a demonstrated fixed point of the production RHS, or establish that a different data/gauge pair is required.

#### P1.1 Validate the spatial `f(x,z)` experiment (`slicing_mode=4`)

The code already has the mechanism; it needs measurement, not another prototype.

- [ ] Run `make diagnose-kerr-bssn` with `slicing_mode=3` and `4` at 96×192, 192×384, and 384×768. Record per-field max/RMS RHS, not only `RHS_alpha`.
- [ ] Verify the cached `f(x,z)` is finite, positive, and continuous across the axis and the excision boundary. Check where the cache falls back to `bm_f_of_alpha`; document any discontinuity.
- [ ] Evaluate the full RHS at `t=0` with the cache and with the scalar table. Gate A requires every evolved field’s residual to decrease under refinement. A reduction in `RHS_alpha` alone is insufficient.
- [ ] If the full RHS converges, run matched short evolutions (e.g. 1000–2000 steps at 96×192 and 192×384) and compare horizon drift, constraints, and determinant minima against `slicing_mode=3`.
- [ ] If the cache introduces a seam or a non-convergent residual, do not promote it. Record the failed comparison and move to P1.2.

#### P1.2 Decide between the two viable gauge/data formulations

The report (§13.11) identifies two routes. Make the decision with measured RHS, not with a preference:

- **Route A1 (spatial coefficient):** keep the DBM data and use a smooth spatially dependent lapse coefficient/source, i.e. the `slicing_mode=4` family. This is already implemented as a cell-centred cache; if it works, it is the smaller change.
- **Route A2 (scalar-law-compatible data):** construct a Kerr stationary data set whose required `f` is a single scalar function of `alpha`, so `slicing_mode=3` is exact. This is a research/data-generation task, not a table-fitting task. Timebox it.
- Required deliverable for A2: an initializer (or analytic construction) plus a three-resolution full-RHS table. If no such member of the family exists, document the no-go and retain A1.
- Do **not** accept a global scalar fit merely because its weighted aggregate score improves; the smooth candidate already failed the full discrete RHS while improving the aggregate.

#### P1.3 Verify the shift-driver equilibrium

- [ ] On the chosen data/gauge pair, measure `RHS_beta`, `RHS_B`, and `RHS_Gammatilde` by region at three resolutions. `gamma_driver_mode=4` has the `B=0` fixed point; confirm `RHS_Gam→0` rather than assuming it.
- [ ] Check whether the initial `B` (set to zero in the route-C initializer) is consistent with the chosen stationary state. If `RHS_Gam` is nonzero, `B` will not stay at zero.
- [ ] Only after stationarity is established, study `gauge_eta` sensitivity. Do not tune `eta` to hide a nonzero stationary residual.

#### P1.4 Horizon and invariant validation

- [ ] Confirm the finder mask offset and the evolution mask are still separate at all resolutions.
- [ ] At `t=0` and after a fixed physical interval, independently re-solve the horizon (existing `diagnose-horizon-snapshot`) and compare area, mass, and spin.
- [ ] Require the 192×384 and 384×768 runs to find the horizon throughout the tested interval. A finder loss with a regular local metric is a finder issue; a geometry change with a converged finder is a physics issue. Keep them separate.

**Gate A:** all 24 production RHS components converge toward zero for the chosen stationary data/gauge pair (or the non-convergent component is localized and explained); constraints and horizon invariants converge at `t=0`; the shift driver has a demonstrated fixed point; and the initial horizon is recovered at all three resolutions. If Gate A fails after the timebox, enter Plan B1 in §7.

---

### Phase 2 — Localize and repair the boundary operators (2–4 weeks)

**Goal:** determine whether the remaining secular error is inner-boundary, outer-boundary, or bulk, and repair only what the controlled comparison identifies.

#### P2.1 Frozen-state stencil isolation (the next planned measurement)

- [ ] Use saved states from the 96×192, 192×384, and 384×768 runs at early and late equal physical times.
- [ ] For the near-excision band (`r_KS ≈ 1.5–1.6`) and the outer band (`r_KS > 12`), compare the production constraint/derivative assembly against:
  - an interior/high-order derivative on the same frozen state;
  - a one-sided alternative at the mask edge;
  - the same operator with the cartoon `y` path replaced by an analytic/interpolated control where possible.
- [ ] Report all seven momentum pieces and the x/y/z raised-`Atilde` divergence pieces, plus the Hamiltonian, by region.
- [ ] Change a boundary operator only if the error follows that operator across resolutions. The current evidence localizes the near-mask error to the raised-`Atilde` divergence, but does not yet distinguish data conversion, metric raising, cartoon `y`, or one-sided `x/z`.
- [ ] Extend the existing snapshot diagnostic if needed: a `diagnose_stationarity_snapshot` that reloads any `state_*.h5` and evaluates the full RHS by region would make the secular growth directly measurable.

#### P2.2 Inner boundary: complete the characteristic treatment

- [ ] Write down the normal principal part of the actual production second-order BSSN + gauge system at a curved Kerr excision surface. The existing inner diagnostic covers only the lapse-gauge family; that is not the full spectrum.
- [ ] Use the existing FO/SBP modules (`helper/fo_normal_system.F90`, `helper/fo_curved_sat.F90`, `helper/fo_sbp.F90`, `helper/excision_sat.F90`) as research infrastructure, but do not transplant a characteristic map from a different first-order formulation without matching the reduction.
- [ ] Build manufactured incoming/outgoing tests with nonzero amplitudes. A zero-field pass is not evidence.
- [ ] Implement a complete SAT/penalty or a justified excision condition for all incoming modes, with a documented sign convention and discrete energy/positivity argument.
- [ ] Keep the current reduced SAT disabled. Its own test now declares `exact_excision_characteristics=.false.`; the longer matched run showed it redistributes error rather than controlling it (`baseline/ROUTE_C_INTERIOR.md` §SAT).
- [ ] Revisit moving or cylindrical excision only if the fixed mask is shown to be the limiting factor after Gate A. A moving mask must preserve stencil integrity and never cross outside the horizon.

#### P2.3 Outer boundary: replace or validate the Sommerfeld closure

The outer band is now a measured non-convergent region in the evolved momentum residual, and older domain sweeps show a real outer-boundary error floor.

- [ ] Reproduce the outer-domain sweep with the **current DBM configuration**, not the old frozen Kerr–Schild configuration: 12M, 18M, 24M at fixed `dx`, equal physical time, same inner setup.
- [ ] Measure the exterior and outermost-band `H`, `Mx/My/Mz`, and the first-reflection time.
- [ ] Replace the current single-layer, all-fields Sommerfeld prescription with a characteristic or constraint-preserving outer boundary if the sweep confirms dominance. Keep the legacy condition under an explicit control parameter.
- [ ] Do not use the empirical sponge as a fix. The recorded sweep shows it increases the residual; it is at best a diagnostic.
- [ ] If a larger domain is used as a stopgap, keep `dx` fixed and report the cost; do not claim convergence from a larger box alone.

#### P2.4 Cartoon / `y`-derivative accuracy

The `y` path is second order while `x/z` are fourth order. This is a known resolution limit, not the current failure.

- [ ] After P2.1 identifies whether the `y` path contributes to the near-mask momentum error, consider a fourth-order `y` difference using the existing reconstruction.
- [ ] Validate with `helper/test_stencil_derivs.F90` and a manufactured axisymmetric field; do not change it speculatively.

**Gate B/C:** the controlled comparison either fixes the dominant localized error or proves it is not in the tested operator; the inner and outer boundary maps pass manufactured and convergence tests; no reduced or ad hoc penalty is called exact; the stationary data remain stationary to discretization error. If the second-order formulation cannot support a tractable boundary treatment, enter Plan B4.

---

### Phase 3 — Long-term acceptance campaign (2–4 weeks)

**Goal:** demonstrate the accepted physical duration at three resolutions with the same validated configuration.

- [ ] Freeze the configuration after Gate A/B: initial data, gauge, excision radius in `M`, boundary method, dissipation, domain, CFL.
- [ ] Run 96×192, 192×384, and 384×768 to `T/M = 10`, then 30, then 100 (or the agreed final target). Use separate directories and archive all logs.
- [ ] At each diagnostic time record:
  - horizon found flag, area, `M_irr`, `J`, `M_H`, shape extrema, coordinate location;
  - exterior, horizon-shell, near-excision, and outermost-band `H`, `Mx`, `My`, `Mz` max/RMS;
  - minimum `det(gtilde)`, minimum metric eigenvalue, and coordinates;
  - minimum lapse and maxima of `|K|`, `|Atilde|`, `|beta|`, `|B|` in a fixed near-hole shell;
  - each field’s RHS norm and the incoming characteristic amplitudes;
  - any projection correction that is not at roundoff.
- [ ] Compare at equal `t/M`, not equal step. Report observed convergence orders for horizon invariants and constraints.
- [ ] If a run fails, preserve the state and log; rerun only with a controlled change. Do not restart from a later state to hide the failure.
- [ ] Add checkpoint/restart only if it can reproduce the RK4 state exactly. A restart that changes the trajectory is a new experiment.

**Gate D:** no determinant/eigenvalue loss; horizon found throughout; constraints and invariants bounded and convergent at equal time; stationary case shows no unexplained secular drift; clean-build reproducibility.

---

### Phase 4 — Productionization and maintenance (1 week)

- [ ] Promote only the validated configuration to `parameter_files/vacuum_kerr.par`; keep the legacy baseline under a separate explicit parameter file.
- [ ] Update parameter comments to state the data/gauge pairing requirement and the valid spin range. `slicing_mode=3`'s `f(alpha)` table is measured at `a=0.8`; intermediate spins are an unvalidated blend (see `integrators/bssn_rhs.F90`).
- [ ] Add a `make` regression target for the accepted long run (or a shortened equal-time proxy) with explicit tolerances.
- [ ] Update `docs/bsssn-kerr-axi.tex`, `docs/README.md`, and the root-level status documents so a new maintainer sees the live configuration first and the historical investigations second.
- [ ] Tag or commit the accepted state; archive the reference logs and the exact parameter files.

---

## 4. Immediate next actions (ordered)

1. **Make the gate green.** Fix the `fo_constraint_fields` parity failure and the missing `eq_kerr_bssn_data.F90` dependency in `test-fo-rhs`/`test-fo-complete-rhs`, then run `make clean && make -j4 && make check`. Record the result.
2. **Archive the current tree and configuration.** Commit/branch, save the diff and parameter files, and empty/rehome the mixed `output/` directory.
3. **Reconcile the status documents.** Correct the `docs/bsssn-kerr-axi.tex` production description and mark the superseded plans. Resolve the horizon-loss-step discrepancy from logs.
4. **Run the spatial-`f` experiment.** Use the existing `slicing_mode=4` cache:
   ```sh
   GR_SLICING_MODE=3 make diagnose-kerr-bssn
   GR_SLICING_MODE=4 make diagnose-kerr-bssn
   GR_SLICING_MODE=4 GR_NX=192 GR_NZ=384 make diagnose-kerr-bssn
   GR_SLICING_MODE=4 GR_NX=384 GR_NZ=768 make diagnose-kerr-bssn
   ```
   Record per-field RHS and the `f(x,z)` seam/fallback behaviour. Do not promote it on `RHS_alpha` alone.
5. **Run the matched short evolution for the better gauge candidate.** If the full RHS converges, compare `slicing_mode=3` and `4` at 96×192 and 192×384 for 1000–2000 steps, with horizon and determinant monitoring.
6. **Start the frozen-state stencil isolation.** Use the existing 192×384 state at `t≈2.66` and the 96×192 state at the same time; focus on `r_KS≈1.5–1.6` and `r_KS>12`. Report the seven momentum pieces and the x/y/z raised-`Atilde` divergence.
7. **Only then choose the boundary repair.** If the stencil isolation identifies the inner or outer operator, fix that operator; if not, proceed to the complete characteristic treatment.

---

## 5. Acceptance gates

| Gate | Question | Pass criterion | If it fails |
|---|---|---|---|
| 0 | Is the baseline reproducible? | Clean build, `make check` green, 96×192 baseline reproduced, metadata archived | Fix build/docs first; do not run physics |
| A | Are the data and gauge a fixed point? | All 24 RHS components converge; constraints and initial horizon converge at 3 resolutions; shift fixed point demonstrated | Plan B1: isolate data conversion, gauge equation, or driver |
| B | Is the inner boundary controlled? | Full characteristic/manufactured tests pass; no reduced SAT; no unexamined mask-edge growth | Plan B4: compatible SBP-SAT or different excision strategy |
| C | Is the discrete bulk/boundary error convergent? | Constraints and geometric diagnostics converge; first growing mode attributable or bounded | Isolate operator; do not tune KO |
| D | Does it run long term? | `T/M` target achieved at 3 resolutions, horizon found, invariants bounded/convergent, reproducible | Report the localized failure; do not relax the gate |

**Region definitions for all comparisons:** inside horizon `[r_excise, r_ah)`, horizon shell `[r_ah, r_ah+w)`, exterior `[r_ah+w, ∞)`, near-excision band `[r_excise, r_excise+3dx]`, outermost band `r_KS > 12M`. Keep these fixed across experiments; changing the shell definition mid-campaign invalidates the comparison.

---

## 6. Experiment record template

Every run gets one row, including failed runs:

| ID | Git | Par file | Data/gauge | Grid | `r_excise` | BC | KO | Steps | `t/M` | Horizon found | `dA/A_Kerr` | `max/rms H` | `max/rms M` | `min det` | Decision |
|---|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---|

Rules:

- The parameter file is copied into the run archive; environment overrides are recorded verbatim.
- “Horizon found” is a coverage statement, not a claim that the geometry is correct.
- A run that fails before the checkpoint still gets a row and a preserved log.
- No row is deleted because it is inconvenient.

---

## 7. Decision points and fallback

### If Gate A fails

1. Compare, at the same point, the closed-form DBM state, the stored BSSN state, the production stencil derivatives, and the production RHS. The DBM point comparison already shows stored-vs-analytic agreement to `~1e-13`; use that as the control.
2. Reduce spin: `a=0` must reproduce the established Schwarzschild equilibrium; then small spin; then `a=0.8`. A discontinuity at `a→0` points to the spin-dependent geometry or transformation.
3. If the full RHS is nonzero for claimed stationary data, fix the data conversion or gauge equation before touching the boundary.
4. If no member of the DBM family admits a scalar law, retain the spatial-coefficient route or document a no-go; do not fit the table to the failing run.

### If the boundary work stalls

- **Plan B4:** move the near-boundary evolution to an SBP-SAT discretization compatible with the production equations, or adopt a validated first-order formulation. The existing FO modules are a start, but a standalone FO test is not sufficient; the production executable must use the same system and pass `check-parity`.
- Do not call the reduced 3×3 SAT exact, and do not enable it as a production default.

### If the outer boundary dominates

- Prefer a characteristic/constraint-preserving outer boundary over a larger box or stronger sponge.
- A larger domain is acceptable only as a documented cost/stopgap, with fixed `dx` and equal-time comparison.

### If a run is bounded but not stationary

- Record the drift and its convergence. A bounded non-stationary evolution may be a valid engineering result for some purposes, but it does not satisfy Gate D’s stationarity claim. State the distinction explicitly.

---

## 8. Estimated effort

| Phase | Effort | Critical dependency |
|---|---|---|
| Phase 0 | 1–2 days | none |
| Phase 1 | 1–2 weeks | Phase 0; existing `slicing_mode=4` scaffold |
| Phase 2 | 2–4 weeks | Gate A; frozen-state diagnostics |
| Phase 3 | 2–4 weeks | Gates B/C; compute budget for 3 resolutions |
| Phase 4 | ~1 week | Gate D |

The estimates assume the existing diagnostics and test infrastructure are reused. They do not include a full first-order/SBP port; if Plan B4 is required, add a separate multi-week estimate after the boundary gate fails.

---

## 9. What not to do

- Do not relax the 5% invariant abort or hide horizon-finder failures.
- Do not claim stationarity from a run that only survives longer.
- Do not tune `f(alpha)`, `eta`, `ko_eps`, `r_excise`, or the grid to make one diagnostic pass.
- Do not quote a single-resolution RHS or constraint number as convergence.
- Do not enable the reduced SAT, or describe extrapolation as a characteristic boundary condition.
- Do not mix Kerr–Schild data with the route-C gauge (or vice versa) in a production result.
- Do not begin the 50,000-step/100M acceptance run before Gate A and the boundary comparison are closed.

---

## 10. Primary references

- `LONG_TERM_GAUGE_DRIFT_REPORT.md` — architecture, evidence, work packages A–D, Plan B.
- `baseline/STEPS_1_3_CURRENT_STATUS.md` — the most current numerical status (Nov 3).
- `baseline/LAPSE_HORIZON_DIAGNOSTIC.md` — lapse-RHS plateau and high-resolution horizon fix.
- `baseline/ROUTE_C_INTERIOR.md` — route-C data/gauge consistency, SAT rejection, outer-boundary isolation.
- `baseline/CONSTRAINT_SHELLS.md`, `baseline/NEAR_MASK_DIAGNOSTIC.md` — region definitions and the “inside-horizon junk” trap.
- `baseline/EXCISION_ORDER_DECISION.md` — fill-order trade-off (order 3 production, order 4 not uniformly better).
- `docs/bsssn-kerr-axi.tex` — full code reference; update before treating its production statements as current.
- `parameter_files/vacuum_kerr.par`, `parameter_files/vacuum_kerr_192.par` — live configurations.
