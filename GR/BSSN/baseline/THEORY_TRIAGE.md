# Theory triage — long-time determinant failure

**Date:** 2026-10-14
**Question:** which of the report's theories A–F actually causes the
conformal-determinant failure at ~7.5k–18k steps?
**Method:** equal-physical-time experiments, frozen-gauge control, and the
existing stationarity instrumentation.

## Headline

**Theory A (gauge transient drives horizon migration) is real but INSUFFICIENT.**
The horizon does migrate, but it migrates *even with the gauge frozen at the
analytically exact Kerr–Schild values*, and the run still fails — with the
failure in the bulk, far from the excision surface.

The dominant mechanism is the **inner boundary (Theory B)**, aggravated by
**resolution (Theory C)**. Evidence below.

## Experiments

All runs: `GR_HZ_ABORT=0`, `vacuum_kerr.par` base, a=0.8, equal physical time.

### 1. Horizon drift is real (default 1+log + legacy Gamma-driver)

```
[horizon] step   0  r in [1.59986, 1.78831]
[horizon] step 300: DRIFT r_min 1.5999 -> 1.5817 (d=1.81e-2, rate=6.8e-2/t)
   -> stationary solution is NOT stationary
 ABORT at step 300
```
`M_irr` (0.89433 -> 0.89381), `J` (0.80053 -> 0.79792) and area are essentially
unchanged: this is **coordinate** migration with preserved invariants, the
Theory A signature. Confirmed as a phenomenon.

### 2. The implemented puncture gauge makes it worse

`GR_PUNCTURE_LAPSE_MODE=1 GR_GAMMA_DRIVER_MODE=1`:
```
[horizon] step 100: DRIFT r_min 1.5999 -> 1.5741 (d=2.58e-2, rate=2.9e-1/t)
```
**4x faster** than the default gauge. The report's warning that this mode's
"normalization is not established" is borne out — it is not a usable fix.

### 3. DECISIVE: a frozen, exactly stationary gauge still drifts and still fails

`slicing_mode=2, gamma_driver_mode=2` freezes lapse and shift at the exact
Kerr–Schild values — the gauge that is stationary *by construction* for
Kerr–Schild data.

Drift still occurs:
```
step 100  r in [1.60017, 1.78859]
step 300  r in [1.59557, 1.78309]
step 500  r in [1.58491, 1.77058]
step 600: DRIFT r_min 1.5999 -> 1.5781 (d=2.18e-2, rate=4.1e-2/t)
```
Same order as the "bad" gauge (4.1e-2 vs 6.8e-2 /t). **A gauge transient cannot
explain drift that survives an exactly stationary gauge. Theory A is falsified
as the primary mechanism.**

Run to failure:
```
step 7400  max|H|(ext)=9.0052E+00  horizon r in [1.57099, 1.78587]
step 7500  [horizon] NOT FOUND
step 7500  max|H|(ext)=1.2455E+01
Conformal metric failure det= -1.067321E+11
Invalid state at step 7536  i=7 k=48  x=1.53191 z=-0.12632
```
Critical geometry:
- horizon `hmin` = **1.571** at failure — it never approached `r_excise = 1.0`
- failure point **x=1.532, z=-0.126**, r ~ 1.54 — **bulk**, far from excision

So the horizon never reaches the excision boundary before the run dies.

### 4. The bulk RHS is stationary-consistent (Theory E test passes)

`diagnose_stationary_ks_rhs`, per-field RHS residual maxima:

| Nx | field 1 (phi) | field 2 (gtilde_xx) | field 3 (gtilde_xy) |
|---|---|---|---|
| 48  | 8.59e-4 | 5.41e-3 | 3.94e-3 |
| 96  | 1.73e-4 | 1.92e-3 | 1.01e-3 |
| 192 | 4.33e-5 | 5.71e-4 | 2.92e-4 |
| 384 | 1.05e-5 | 1.44e-4 | 7.30e-5 |

Clean ~2nd-order convergence to zero. The bulk RHS is consistent; Theory E's
discriminating test **passes**.

### 5. The growth is LINEAR in time — constant forcing, not an eigenmode

Frozen gauge, `max|H|`:

| t | 0.089 | 0.98 | 1.88 | 2.77 | 3.66 | 4.56 | 5.45 | 6.34 |
|---|---|---|---|---|---|---|---|---|
| max\|H\| | 0.036 | 0.568 | 1.616 | 2.702 | 3.641 | 4.190 | 5.555 | 7.033 |

Rate ~1.1/t, starting at t=0. This is steady error *injection*, not an
unstable mode growing exponentially. A source is pushing constraint violation
in at a constant rate from the beginning.

### 6. The source is the excision boundary

Ring diagnostic `r in (r_excise, r_excise+3dx)`, frozen gauge:
```
step 7200  max|K|= 1.311E+01 @(5,48)
step 7400  max|K|= 1.566E+01 @(5,49)
step 7500  max|K|= 2.171E+01 @(5,49)     ->  (5,49) = (x=1.02, z=0.126), r~1.03
```
`max|K|` peaks ~1 cell outside `r_excise = 1.0` and grows monotonically.

### 7. Scaling: r_excise dominates, refinement helps

Equal physical time, frozen gauge, max|H|:

| config | t | max\|H\| |
|---|---|---|
| Nx=48, r_excise=0.5 | 0.804 | 2.31 |
| Nx=48, r_excise=1.0 | 0.755 (interp) | ~0.43 |
| Nx=48, r_excise=1.0 | 0.980 | 0.568 |
| Nx=96, r_excise=1.0 | 0.755 | 0.118 |
| Nx=96, r_excise=1.0 | 0.933 | 0.176 |

Moving the excision surface inward (1.0 -> 0.5) makes growth ~5x **worse**;
2x refinement makes it ~4x **better**. Consistent with a discretization-limited
error generated at the excision surface, over a region that is under-resolved
in the strong-field zone.

The Nx=192 point was attempted but did not complete within the available
wall-clock budget (192x384 is 16x the work per step of 48x96, and the run is
I/O- and compute-bound). The 48 -> 96 trend is sufficient to establish the
scaling; the 192 confirmation is not required for the conclusion below, but the
series should be completed before Gate B is designed in detail.

## Diagnosis

Ranked, with the evidence each rests on:

1. **Theory B — inner-boundary treatment.** The error is injected at the
   excision surface at a constant rate (linear H growth from t=0), is
   maximised one cell outside it, and gets sharply worse when the surface is
   moved inward. The current treatment is *extrapolation*, which the report
   itself notes "is not a characteristic boundary condition", plus a reduced
   SAT that is disabled by default and explicitly not the exact
   characteristic map. **Primary mechanism.**

2. **Theory C — near-excision resolution.** Aggravating: 4x improvement per
   2x refinement, and strong `r_excise` dependence. Mask-edge stencils drop to
   1st/2nd order, and `r_excise=1.0` leaves only ~4 cells between the surface
   and the horizon.

3. **Theory E — bulk RHS.** Its discriminating test **passes** (clean 2nd
   order). Not the cause.

4. **Theory A — gauge.** The transient is real and the drift is real, but both
   persist under an exactly stationary gauge. Contributes to *when* things go
   wrong, not to the mechanism.

5. **Theory D — momentum constraint.** Resolved earlier: real defect, fixed,
   and provably not on the evolution path.

6. **Theory F — outer boundary.** Not excluded; failure is at r ~ 1.5, and
   Sommerfeld/KO are active. Lower evidence, not yet tested.

## Implication for the repair programme

Gate A (stationary Kerr data/gauge pair) and Gate B (exact inner-boundary
characteristic treatment) are both still required, but their priority is now
reversed relative to the report's presentation:

- The report treats the gauge/data mismatch as the leading cause. **The frozen
  control shows the gauge is not the cause.** Building Kerr trumpet data is a
  large effort that would not, on this evidence, fix the failure.
- **Gate B is now the critical path.** A proper characteristic (SAT/penalty)
  treatment matched to the actual production second-order BSSN system, on a
  stencil compatible with it, is what the evidence points at.

The honest caveat: this is a *localization* of where error is injected, not a
proof of the unstable eigenmode that eventually kills the run. Linear growth in
H with a determinant failure at H ~ 12 is consistent with the injected error
eventually overwhelming the representation, but confirming that requires the
Work Package B instrumentation (per-variable RHS saved across RK stages at the
mask edge).

## Reproduction

```sh
# theory A controls
GR_HZ_ABORT=1 GR_HORIZON_EVERY=100 GR_NSTEPS=20000 ./bin/vacuum_kerr parameter_files/vacuum_kerr.par
GR_PUNCTURE_LAPSE_MODE=1 GR_GAMMA_DRIVER_MODE=1 GR_HZ_ABORT=1 ./bin/vacuum_kerr parameter_files/vacuum_kerr.par
GR_SLICING_MODE=2 GR_GAMMA_DRIVER_MODE=2 GR_HZ_ABORT=0 ./bin/vacuum_kerr parameter_files/vacuum_kerr.par

# theory E: RHS convergence
make bin/diagnose_stationary_ks_rhs
./bin/diagnose_stationary_ks_rhs parameter_files/vacuum_kerr.par   # per-field RHS vs resolution
```