# `f(alpha)` for the route-C slicing: four defects, one of them live

**Date:** 2026-10-30
**Files:** `integrators/bssn_rhs.F90` (`bm_f_of_alpha`, `bm_f_tab`, `bm_f_closed`),
`helper/test_eq_kerr_data.F90` (Part H = the table generator),
`helper/test_gauge_f_continuity.F90` (the new gate)

## What was believed

`tab_f(1) = 1.45346533` at `alpha = 0.37628835` looked like a bad table entry:
it sits 14.0% below the a = 0 closed form `(1-alpha)/alpha = 1.65754` at the
same alpha, while every later node sits above it.

## What is actually true

`tab_f(1)` is **correct**. Regenerating the table at `R = 1.05 ... 6.2`
(Part H) shows the a = 0.8 sequence passing smoothly through that point:

```
R      alpha        f          alpha*f
1.600  0.30343304   1.83213266  0.5559
1.700  0.34143425   1.62041861  0.5533
1.800  0.37628835   1.45346533  0.5469   <- the "outlier"
2.000  0.43758753   1.20348374  0.5266
2.200  0.48931866   1.02296282  0.5006
```

The 14% step came from the *fallback*, not the table: below `tab_a(1)` the code
substituted the **a = 0** closed form. So the table was not wrong; it was
truncated at `R = 1.8` (a choice tuned for a = 0, whose horizon is at R = 2),
while the a = 0.8 family reaches smaller alpha.

**Correction (2026-10-31).** The earlier statement that the evolved run reaches
`alpha_min = 0.363 < 0.3763`, making the truncation live in the operating range,
predates the `r_KS` fix and is not reproduced. With the correct Kerr-Schild
radius, at `t = 1.33` and `r_excise = 1.0` the minimum is `0.4055`, i.e.
**above** the old table bottom `0.3763`. On present evidence the old truncation
was not yet exercised by `t = 1.33`; it would only be reached by a longer run.
The truncation and the below-table fallback are still defects worth fixing, but
the "crossed twice per timestep" claim is withdrawn until a run is shown to
cross `0.3763`.

The a = 0 column of Part H reproduces `(1-alpha)/alpha` to machine precision at
every R, which is the independent validation that the generator, the DBM
metrics and the K reconstruction are all sound.

## The four defects

| # | defect | consequence |
|---|---|---|
| 1 | table started at `R = 1.8` (`alpha = 0.3763`) | nothing below that alpha was tabulated; whether the current run reaches it is unverified (see correction above) |
| 2 | below the table, the **a = 0** closed form was used | 14.0% step in `f`, i.e. in `d_t alpha`; O(1) source, resolution-independent |
| 3 | blend was `(1-w)*table + w*closed` with `w = a/0.8` | at `a = 0` it returned the a = 0.8 table; at `a -> 0.8` it returned the a = 0 curve, so `f` jumped 14% in the **spin** direction at exactly `a = 0.8` |
| 4 | `a <= 0` returned `bm_f_tab` | a = 0 used the a = 0.8 table instead of the exact closed form |

For the production run at `a = 0.8` the defect the evolution actually feels is
**defect 3**: the old blend returned the a = 0 closed form everywhere (see the
note on the A/B below), so the A/B measures that substitution, not the table
truncation. Defect 3 also means that scanning spin at fixed alpha would have
shown a discontinuity the alpha scan cannot see; the new test checks both
directions.

## Fix

- Table extended to 23 nodes, `R = 1.05 ... 6.2`, `alpha in [0.030, 0.830]`.
- Below the first node: `f = f1*alpha1/alpha`, the table's own `1/alpha` tail.
  Continuous at the node by construction, correct trumpet divergence.
- Above the last node: `f = c_hi*(1-alpha)/alpha` with
  `c_hi = alpha_N f_N/(1-alpha_N) = 1.0618`. Holding `f` flat (the old
  behaviour) left a constant source in the far field, where at `alpha = 0.99`
  the held value is ~20x too large.
- Blend: `f = (1-w)*closed + w*table`, `w = a/0.8`.
- `a <= 0` returns the closed form.

## Gate

`make test-gauge-f-continuity` scans both directions and distinguishes a jump
from a steep-but-smooth region by the stride-dependence of the difference:

```
smooth f:  d(h/4)/d(h) ~ 0.25      jump:  d(h/4)/d(h) ~ 1.00
```

Measured after the fix: ratio 0.250 at every sample for a = 0, 0.4, 0.8, and at
the spin joint `a = 0.8`. Before the fix it reported 0.204 at alpha = 0.3763
with ratio 1.0.

`f(0.99) = 1.01e-2 (a=0) .. 1.07e-2 (a=0.8)`, i.e. the far field falls like
`(1-alpha)/alpha` as it must.

## Measured effect: matched A/B, old f vs new f

96x192, `a = 0.8`, `r_excise = 1.0`, `excision_sat = false`, 3000 steps
(`t = 1.3328`), identical binary except for `bm_f_*`:

| metric at t = 1.3328 | old f | new f | change |
|---|---|---|---|
| `max|H|(ext)` | 1.8923e-01 | 1.4399e-01 | **-24%** |
| `rms|H|(ext)` | 5.3175e-03 | 5.1512e-03 | -3% |
| `max|M|(ext)` | 1.3511e-02 | 1.3708e-02 | +1.5% |
| horizon-shell `max|H|` | 8.8631e-01 | 8.8434e-01 | -0.2% |
| area / A_Kerr - 1 | +5.75e-04 | +1.65e-03 | **2.9x worse** |
| `J/M^2` | 0.797537 | 0.797523 | -0.002% |
| `hmin` | 1.55919 | 1.56015 | +0.06% |
| `alpha_min` (evolved domain, `r_KS`) | 0.40471 | 0.40552 | +0.2% |

Reading: the discontinuity fix is right on the constraint (24% lower exterior
`max|H|`) and neutral on `J`, `hmin` and the gauge collapse, but the horizon
area error gets 2.9x worse. That is not an argument to keep the jump — the old
larger `f` near the collapse happened to hold the area closer, which is a
cancellation, not physics. `alpha_min` itself is almost unchanged (0.4047 vs
0.4055, +0.2%), which says the collapse is not driven by `f` at all; see
`GAUGE_COLLAPSE.md`.

**Which defect this A/B actually measures.** At `a = 0.8` the old blend
`(1-w)*table + w*closed` with `w = a/0.8` evaluates to `w = 1`, so the old code
returned the **a = 0 closed form** `(1-alpha)/alpha` at every alpha, for the
whole run. The A/B therefore measures "closed form vs a = 0.8 table" (defect 3
plus the far-field tail), not the table truncation. The 24% exterior
improvement should be attributed to defect 3, not to extending the table.

## Caveat that matters more than the table

The production configuration pairs **Kerr-Schild initial data**
(`initial_data_mode = 0`, the default) with a slicing function derived from the
**Dennison-Baumgarte-Montero trumpet** family. Those are different slices of
Kerr; only the DBM pair (data + f) is stationary. Fixing `f` removes a real
defect but cannot make KS data a fixed point of a DBM slicing condition.

This is why `diagnose_stationary_ks_rhs` (which freezes the gauge) still shows a
near-horizon `|d_t K|` residual: the mismatch is in the data/gauge pair, not
only in `f`. Route C is only meaningful with `initial_data_mode = 1`, and that
path is currently broken — not by `max|H| = 5.89e2` (that number was wrong), but
by a coordinate degeneracy in the data chart at `r_E = 2a|cos th|`, which at
`a = 0.8` coincides with the horizon `r_+ = 1.6`. See `ROUTE_C_INTERIOR.md`.
