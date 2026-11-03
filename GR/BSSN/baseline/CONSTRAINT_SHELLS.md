# Constraint shells keyed to the horizon

**Date:** 2026-10-30
**Supersedes:** `NEAR_MASK_DIAGNOSTIC.md` (its "near-mask" band straddled the horizon)

## Problem with the old windows

Both windows were keyed to `r_excise`:

| window | old definition | what it actually contained |
|---|---|---|
| exterior | `r > r_excise + diag_margin` (default 2.0 for `r_excise=1.0`) | excluded `r_+ < r < 2.0` — the only shell where inner-boundary error can reach the physical domain |
| near-mask | `r_excise < r < r_excise + w` | included `r_excise < r < r_+` — inside the horizon, where the constraint has no physical meaning |

The apparent horizon is at `r_+ = 1.6` for `M=1, a=0.8`. So the "near-mask"
maximum previously reported at `worst r = 1.0078` was **inside-horizon junk**,
not a boundary-operator error. `NEAR_MASK_DIAGNOSTIC.md` inherits that
misattribution; `INNER_BOUNDARY_FINDINGS.md` measures a different quantity (the
error the fill injects, which is exactly zero) and is unaffected.

## New diagnostic

`drivers/vacuum_kerr.F90` now reports three shells keyed to the *apparent
horizon* `r_ah` (from the finder, analytic `r_+` as fallback), with
`w = GR_NEAR_MASK_W` cells (default 2):

```
inside-horizon  [r_excise, r_ah)      reported, never gated
horizon shell   [r_ah, r_ah + w)      reported, this is the boundary gate
exterior        [r_ah + w, inf)       reported, this is the accuracy claim
```

## Measurement: `r_excise` does not affect the physical domain

96×192, `a=0.8`, `excision_sat=false`, `t = 0.0889` (step 200), `w = 2Δ`:

| `r_excise` | exterior `max|H|` | horizon shell `max|H|` | inside-horizon `max|H|` |
|---|---|---|---|
| 0.8 | 9.4692e-03 | 1.4588e-02 | **1.3223e+00** |
| 1.0 | 9.4765e-03 | 1.4545e-02 | **1.0792e-01** |

Read this table carefully:

- **Exterior and horizon shell agree to 3 digits.** The choice of `r_excise`
  changes nothing in the physical domain at early times.
- The 12× difference lives entirely in the inside-horizon junk volume.
- Every earlier "`r_excise=1.0` is N× more accurate than 0.8" claim, including
  the step-1 resolution ladder in `NEAR_MASK_DIAGNOSTIC.md`, was measuring
  that junk volume.
- The horizon shell is only ~1.5× the exterior, with its worst point at
  `r = 1.719` (mid-shell, not at the shell edge). At this resolution the mask
  edge is **not** leaking into the physical domain.

## Consequences

1. `r_excise` should be chosen for **runway**, not accuracy: the mask sits
   `r_ah - r_excise` away from a horizon that contracts at 0.03–0.04 M per M.
   `r=0.8` buys ~19 M, `r=1.0` buys ~14 M. Parameterize as a fraction of the
   *found* `r_ah` (see item 5 of the long-term plan) rather than an absolute
   number.
2. The inner-boundary operator is a **robustness** problem (junk growing from
   1.3 toward NaN inside the horizon), not the dominant early-time error. The
   dominant early-time error is truncation: exterior `max|H| ~ 9.5e-3` at
   96×192.
3. The inside-horizon shell must still be watched: it starts at 1.55 and is
   the raw material for a NaN. `(not gated)` means "do not fail on it", not
   "do not log it".

## Reproduce

```
GR_NSTEPS=200 GR_HORIZON_EVERY=100 GR_HZ_ABORT=0 GR_OUT_EVERY=200 \
  ./bin/vacuum_kerr parameter_files/vacuum_kerr.par
```

with `r_excise` set to 0.8 and 1.0, `excision_sat = false`.
