# Work Package B instrumentation — inner boundary

**Date:** 2026-10-14
**Instrument:** `helper/diagnose_inner_boundary.F90` (`make diagnose-inner-boundary`)
**Purpose:** localize the boundary source flagged by `THEORY_TRIAGE.md`.

## Method

At t=0, from the same exact Kerr–Schild initial data, build the state twice:

- `u_prod` — production path: analytic exterior, then `extrapolate_excised()`
  overwrites the interior.
- `u_ana` — analytic everywhere, no extension.

Evaluate the **same point RHS** (`compute_rhs_point`) at the same exterior points
with both. The interior is a pure *input* to the point RHS, so

```
err = rhs(u_prod) - rhs(u_ana)
```

is exactly the error the extension injects. The instrument also reports, per
variable, the analytic-state RHS in the ring split by distance from the axis,
and runs a resolution ladder to measure convergence order.

## Results

### 1. The extension injects NO error at t=0 — exactly zero

```
sanity: filled (GHOST) cells= 34    max|up-ua| inside mask= 0.0000E+00
err|ring| (fill-injected) = 0.0000E+00   for all 24 variables, all resolutions
```

This is **not** a result that the fill is perfect. The fill is

```
u(i,k) = background(i,k) + <one-sided polynomial extrapolation of (u - background)>
```

(`helper/excision.F90:extrapolate_direction`). At t=0 the perturbation
`u - background` is identically zero, so the extension returns
`background` exactly, bit for bit. **A static t=0 test is structurally blind to
this failure mode** — a lesson worth recording, because it looks like a clean
pass.

### 2. At evolved time the fill is well behaved (no runaway)

Frozen gauge, `r_excise=1.0`, Nx=48, perturbation scan:

| step | t | max\|pert\| | max\|background\| | ratio |
|---|---|---|---|---|
| 0 | 0.00 | 0.0 | 4.64 | 0.00 |
| 300 | 0.27 | 0.785 | 4.64 | 0.17 |
| 600 | 0.54 | 1.329 | 4.64 | 0.29 |
| 900 | 0.80 | 1.505 | 4.64 | 0.32 |
| 1200 | 1.07 | 1.376 | 4.64 | 0.30 |

The perturbation grows and then **saturates** around 30% of the background. The
O(1e4) amplification that `excised.F90`'s own comment warns about does **not**
materialise at these settings (chains are short: only 34 masked cells).

### 3. The stale background is NOT a cause

`set_excision_background` is called once, before the time loop, so the interior
stays anchored to the t=0 data for the whole run. That looked like a prime
suspect. A/B test with a new opt-in probe (`GR_REFRESH_BG=1`, off by default)
that re-anchors every step:

| t | frozen background | refreshed each step |
|---|---|---|
| 0.268 | ~0.065 | 0.060 |
| 0.536 | ~0.175 | 0.174 |
| 0.804 | ~0.378 | 0.386 |
| 0.893 | 0.474 | ~0.479 |

**Indistinguishable.** Ruled out.

### 4. The ring residual DOES converge — at reduced order

With the gauge frozen (see confound below), analytic-state RHS in the ring:

| Nx | err_fill | err_ana (analytic) |
|---|---|---|
| 48 | 0.00000E+00 | 9.094E-01 |
| 96 | 0.00000E+00 | 9.232E-02 |
| 192 | 0.00000E+00 | 2.448E-02 |

observed order **3.30**, then **1.91**.

## A confound I got wrong, and the correction

My first reading of this instrument reported that the ring residual was
"~1.6 and does NOT converge (order -0.00, 0.01)". **That was wrong.** The
largest ring values were variables 18 (lapse) and 22–24 (shift) — whose RHS is
*legitimately* O(1) because Kerr–Schild data are not stationary under the
default 1+log gauge. The gauge was contaminating a discretisation measurement.

With `slicing_mode = 2, gamma_driver_mode = 2` set inside the instrument, the
residual converges at ~2nd order as shown above. Recorded here because the
error nearly caused a wrong conclusion in the other direction.

## Conclusion

The inner boundary is **not** injecting a non-convergent or runaway error:

- extension error at evolved time: bounded, saturating, ~30% of background;
- stale background anchor: no measurable effect;
- ring RHS residual: converging at ~2nd order.

The observed `max|H|` growth is *linear in time from t=0*, and the equal-time
resolution scaling measured in `THEORY_TRIAGE.md` (0.568 at Nx=48 vs 0.118 at
Nx=96, ~4x per 2x refinement) is consistent with **a ~2nd-order boundary-localised
truncation error accumulating every step until it reaches O(1)**, at which point
the algebraic projection can no longer keep `gtilde` positive.

That reframes the problem: this is an **accuracy** problem at the inner
boundary, not an instability and not a runaway fill. Higher resolution buys a
factor ~4 in survival time; it does not fix anything.

The fix remains what Gate B says — a boundary treatment accurate enough that the
accumulated error stays small over the required physical duration (a characteristic
SAT or equivalent, matched to the production second-order BSSN system). But the
evidence now says the bar is "get the boundary to 2nd-order-plus accuracy",
**not** "control an unstable incoming mode".

## Reproduction

```sh
make bin/diagnose_inner_boundary
./bin/diagnose_inner_boundary parameter_files/vacuum_kerr.par          # static + ladder
./bin/diagnose_inner_boundary parameter_files/vacuum_kerr.par 1200     # + perturbation scan

# stale-background A/B probe (off by default)
GR_SLICING_MODE=2 GR_GAMMA_DRIVER_MODE=2 ./bin/vacuum_kerr parameter_files/vacuum_kerr.par
GR_REFRESH_BG=1 GR_SLICING_MODE=2 GR_GAMMA_DRIVER_MODE=2 ./bin/vacuum_kerr parameter_files/vacuum_kerr.par
```

## State of the tree

- `GR_REFRESH_BG` probe added to `drivers/vacuum_kerr.F90`, **default off**.
  700-step baseline unchanged: `max|H|=1.3162E-01 rms|H|=3.8587E-03 alpha=0.910348`.
- `make check` passes.
- No evolution equation, gauge equation, or boundary parameter was changed.