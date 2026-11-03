# Long-Term Kerr Evolution Fix Plan (Detailed, Executable)

> **SUPERSEDED (2026-11-03):** This is a historical checklist. Execution is
> now governed by `LONG_TERM_CONTINUATION_PLAN.md`. The live configuration is
> route-C DBM data + route-C gauge at `r_excise=1.5`, not the older
> Kerr–Schild setup described below. Keep this file as evidence, not as the
> current plan.

**Date:** 2026-10-24  
**Goal:** Achieve stable, convergent long-term evolution (>>14000 steps) of spinning Kerr in BSSN/Cartoon with the invariant horizon drift gate remaining honest (no tolerance relaxation).  
**Current baseline:** Axis leaks removed; radius-only excision restored; runs stable to 6000 steps at 96x192 with zero axis-conformal failures. Drift is gauge-driven (crosses 5% later) — this is the target behavior to fix via better initial data/gauge/boundary, not masking.


## 1. Mathematical Foundations

### 1.1 BSSN (relevant)
- Conformal decomposition: $\gamma_{ij}=e^{4\phi}\tilde\gamma_{ij}$, $\tilde\gamma=\det\tilde\gamma_{ij}=1$, $K=\gamma^{ij}K_{ij}$, $\tilde A_{ij}=e^{-4\phi}(K_{ij}-\frac{1}{3}\gamma_{ij}K)$ trace-free.
- Evolution: $\partial_t \phi$ from $-\alpha K/6 +$ advection/divergence; $\partial_t \tilde\gamma_{ij}$ traceless part from $-2\alpha\tilde A_{ij} + \mathcal{L}_\beta\tilde\gamma_{ij}$; $\partial_t K, \partial_t \tilde A_{ij}$ involve Ricci, lapse Hessian, $A^2, K^2$; $\partial_t \tilde\Gamma^i = -2(\tilde A^{ij}\partial_j\alpha - 6\tilde A^{ij}\partial_j\phi - \frac{2}{3}\tilde\gamma^{ij}\partial_j K) + \ldots + \mathcal{L}_\beta \tilde\Gamma^i$.
- Constraints: Hamiltonian $\mathcal{H}=R + \frac{2}{3}K^2 - \tilde A_{ij}\tilde A^{ij}=0$, momentum $\mathcal{M}_i = \tilde\gamma^{jk}D_j \tilde A_{ki} - \frac{2}{3}\partial_i K + 6 \tilde A_{ki}\partial^k \phi = 0$.

### 1.2 Gauge
- Bona–Masso: $\partial_t \alpha - \beta^i\partial_i \alpha = -\alpha^2 f(\alpha) K$, $f(\alpha)$ controls slicing. Route C uses tabulated $f(\alpha)$ for stationary Kerr (regenerated in codebase).
- Gamma-driver (shifting): $\partial_t \beta^i - \beta^j\partial_j \beta^i = \frac{3}{4}B^i$, $\partial_t B^i - \beta^j\partial_j B^i = \partial_t \tilde\Gamma^i - \beta^j\partial_j \tilde\Gamma^i - \eta B^i$ (mode 4, non-advective form) aims for stationary shift; mode 0 is legacy (toward zero) — mismatch with KS initial data causes transient.
- Stationarity requires consistent initial slice + gauge equations + driver equilibrium.

### 1.3 Excision / Characteristics
Excision is not a BC alone; need all incoming characteristic modes controlled. Current extrapolation fills stencils but doesn't enforce incoming mode constraints. Moving horizon relative to fixed excision is problematic. Kerr radius $r_K(\rho,z,a)$ is correct for geometry.

### 1.4 Horizon Invariants
For stationary BH, $A, M_{irr}=\sqrt{A/(16\pi)}, J, M_H=\sqrt{M_{irr}^2 + J^2/(4M_{irr^2})}$ are gauge-invariant. Drift indicates non-stationarity (gauge evolution / loss of control). Gate must remain honest.


## 2. Root Causes of ~14000-Step Failure

1. **Gauge/initial data mismatch (primary):** Kerr–Schild initial data + Route C gauge not in equilibrium. Large transient.
2. **No puncture anchoring / driver equilibrium:** Mode 4 better than 0 but may not reach equilibrium for KS initial slice; coordinate drift.
3. **Fixed excision:** Horizon moves in coordinates as gauge settles; excision becomes poorly placed, fill quality degrades.
4. **Incomplete boundary control:** Extrapolation doesn't control all incoming characteristics near excision.
5. **Constraint growth:** Near-hole momentum/hamiltonian grow, metric becomes ill-conditioned.
6. **Axis/mask correctness:** Now fixed (was causing earlier crashes) — not the long-term issue.

## 3. Detailed Fix Plan

### Phase 0: Baseline & Instrumentation (1-2 days)
**Goal:** Reproducible measurements before changes.

- [ ] **P0.1 Verify clean state:** `git diff` clean on modified files or stash; rebuild from clean. Ensure no axis band-aids remain.
- [ ] **P0.2 Baseline run:** 96x192, KS+routeC (mode4), `GR_NSTEPS=6000 GR_HZ_ABORT=0` — record horizon.dat, constraints, near-mask.
- [ ] **P0.3 Add diagnostics:** Instrument to log min $det\tilde\gamma$, min lapse, max $|K|, |\tilde A|, |\beta|, |\tilde B|$, momentum constraints by component, near excision ring. Use existing `diagnose_*` tools.
- [ ] **P0.4 Checkpoint harness:** Script to run at fixed physical times across resolutions.

**Acceptance:** Baseline documented, diagnostics available.

### Phase 1: Stationary Kerr Initial Data (Critical, 1-2 weeks)
**Goal:** Minimize gauge transient via equilibrium slice.

- [ ] **P1.1 Literature/spec:** Choose stationary Kerr trumpet family (Dennison–Baumgarte–Montero or equivalent). Document conventions (time coordinate, spin, lapse/shift/metric/extrinsic curvature).
- [ ] **P1.2 Reference implementation:** Python/SymPy or independent code to compute 4-metric, decompose to ADM ($\gamma_{ij},K_{ij},\alpha,\beta^i$) at arbitrary points. Validate constraints to high precision.
- [ ] **P1.3 Transform to Cartesian cartoon:** Compute all 24 BSSN fields, verify $\det\tilde\gamma=1$, $\tilde A$ trace-free, axis regularity. Test against independent checks.
- [ ] **P1.4 Stationarity test:** Evaluate RHS on exact data with production RHS, no dissipation/BCs/projection first. All fields must have RHS ~ discretization error, convergent in resolution.
- [ ] **P1.5 Integrate into code:** Add `init_data_kerr_trumpet` or similar, selectable via parameter. Verify at t=0 constraints match.

**Resources:** `helper/eq_kerr_trumpet.F90`, `helper/eq_schwarzschild_data.F90`, `helper/test_equilibrium_1pluslog.F90` for patterns.  
**Acceptance:** Gate A from report — constraints satisfied, RHS convergent to zero on equilibrium data.

### Phase 2: Gauge Optimization (3-5 days)
**Goal:** Match gauge to initial data.

- [ ] **P2.1 Test gauge modes:** Try mode 4 vs alternatives; tune $\eta$ in gamma-driver.
- [ ] **P2.2 Lapse evolution:** Test slicing modes; ensure $f(\alpha)$ matches stationary slicing.
- [ ] **P2.3 Driver equilibrium:** Ensure $\partial_t \tilde\Gamma^i$ matches equilibrium so $B^i$ reaches fixed point.
- [ ] **P2.4 Measure transient:** Compare horizon motion, constraint growth with KS vs trumpet initial data.

**Acceptance:** Reduced horizon coordinate drift, smaller initial constraint spikes.

### Phase 3: Excision & Boundary Treatment (1-2 weeks)
**Goal:** Proper control of incoming modes.

- [ ] **P3.1 Characteristic analysis:** Derive principal part of production BSSN+gauge on curved boundary (Kerr normal). Compute eigenvalues/eigenvectors for all incoming/outgoing fields.
- [ ] **P3.2 Complete SAT:** Extend `excision_sat.F90` to penalize all incoming characteristics with correct coefficients (energy-stable). Currently reduced/experimental.
- [ ] **P3.3 Manufactured tests:** Inject incoming waves, verify damping; outgoing should not be penalized.
- [ ] **P3.4 Moving excision (if needed):** Track apparent horizon, move excision surface or ensure safe separation. Implement horizon tracking for excision if drift large.
- [ ] **P3.5 Test:** Enable SAT, verify no new instabilities.

**Resources:** `helper/fo_normal_system.F90`, `helper/excision_sat.F90`, `helper/test_excision_sat.F90`, SBP infrastructure in `helper/fo_sbp.F90`.  
**Acceptance:** Gate B — complete boundary map passes manufactured tests.

### Phase 4: Resolution & Convergence (3-5 days)
**Goal:** Ensure numerical convergence.

- [ ] **P4.1 Resolution study:** Run 48x96, 96x192, 192x384 to same physical time. Compare constraints, horizon invariants.
- [ ] **P4.2 Convergence rates:** Measure Hamiltonian/momentum convergence (expect ~2nd order near boundaries/stencils, higher in bulk).
- [ ] **P4.3 CFL study:** Halve dt to check temporal convergence.
- [ ] **P4.4 Near-axis/mask:** Verify axis treatment convergent.

**Acceptance:** Gate C — convergence observed, errors decreasing.

### Phase 5: Long-Term Validation (1 week)
**Goal:** Demonstrate >>14000 step stability.

- [ ] **P5.1 Moderate long run:** 20000 steps at 96x192 with validated config.
- [ ] **P5.2 Full long run:** 50000+ steps or until t/M large (target >>100M). Monitor all diagnostics.
- [ ] **P5.3 Invariant check:** Horizon invariants must remain within tight bounds (ideally < 1% drift) if truly stationary, or drift must be understood. Gate remains honest.
- [ ] **P5.4 Reproducibility:** Clean build, fixed config, documented results.

**Acceptance:** Gate D — stable, convergent, horizon found throughout, constraints bounded.


## 4. Immediate Action Items (Can do now)

1. **Purge any remaining band-aids:** Ensure no `i<=2/i<=4` excision exemptions or projection clamps remain. (Done.)
2. **Create test harness:** Add Makefile targets for quick tests.
3. **Run instrumented baseline:** Collect detailed diagnostics on current fixed code.
4. **Start Phase 1.1-1.2:** Begin stationary Kerr data generation in parallel (Python) — doesn't require changing Fortran yet.

## 5. Resources in Codebase

| File/Dir | Purpose |
|---|---|
| `helper/eq_kerr_trumpet.F90` | Kerr trumpet data (needs verification/update) |
| `helper/eq_schwarzschild_data.F90` | Schwarzschild equilibrium reference |
| `helper/excision.F90` | Excision/mask/fill (fixed) |
| `integrators/bssn_rhs.F90` | RHS + gauge modes |
| `integrators/time_step.F90` | RK4 + projection (fixed) |
| `drivers/vacuum_kerr.F90` | Driver + horizon/invariant guard |
| `helper/horizon.F90` | Horizon finder + invariants |
| `helper/excision_sat.F90` | Inner boundary SAT (experimental) |
| `helper/fo_normal_system.F90` | Characteristic analysis |
| `parameter_files/vacuum_kerr.par` | Config |
| `LONG_TERM_GAUGE_DRIFT_REPORT.md` | Detailed background |
| `LONG_TERM_EVOLUTION_FIX.md` | Fix summary |

## 6. Makefile Targets

Add to `GR/BSSN/Makefile`:
