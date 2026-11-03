# The lapse minimum relaxes inside the horizon — and it is not the boundary

**Date:** 2026-10-31 (supersedes the 2026-10-30 version of this file)
**Measurement:** `alpha_min` over the evolved domain, defined by the exact
Kerr-Schild radius `r_KS` (`kerr_radius_squared`), 96x192 and 192x384,
`a = 0.8`, `excision_sat = false`, `slicing_mode = 3`, `cfl = 0.02`
(verified identical to `cfl = 0.005`), 750 / 1500 steps to `t = 1.33`.

## Retraction: the previous numbers used the wrong radius

The 2026-10-30 version reported `alpha_min: 0.49122 -> 0.22320` and located the
point at `r = 1.012`. That used the **Euclidean** radius `sqrt(x^2+z^2)`, which
is not the coordinate the driver excises on. The driver uses `r_KS`, and for
`a = 0.8` at fixed `x,z` the two differ by a factor of ~1.24. Redone with
`kerr_radius_squared`, the minimum is at `r_KS = 1.007` and the curve is
`0.58286 -> 0.40552`. **The old 0.491 / 0.223 values, and everything derived
from them, are wrong.**

## Data: r_excise and resolution ladder

`alpha_min(t)` at `t = 0, 0.267, 0.533, 0.800, 1.066, 1.333`:

| config | t=0 | 0.267 | 0.533 | 0.800 | 1.066 | 1.333 |
|---|---|---|---|---|---|---|
| 96x192, r_exc = 1.0 | 0.58286 | 0.52477 | 0.48242 | 0.45067 | 0.42594 | 0.40552 |
| 192x384, r_exc = 1.0 | 0.58464 | 0.52707 | 0.48492 | 0.45319 | 0.42802 | 0.40705 |
| 96x192, r_exc = 1.3 | 0.63005 | 0.59435 | 0.56548 | 0.54182 | 0.52227 | 0.50580 |
| 192x384, r_exc = 1.3 | 0.63021 | 0.59450 | 0.56561 | 0.54196 | 0.52232 | 0.50579 |

Per-interval changes at 96x192, `r_exc = 1.0`: -0.0581, -0.0424, -0.0318,
-0.0247, -0.0204. Monotone and decelerating, but the ratio of successive
increments *rises* (0.73, 0.75, 0.78, 0.83), i.e. slower than geometric, so
there is no evidence yet of an imminent collapse to zero.

## Test 1: resolution — it is converged, not truncation

At `t = 1.333`:

| r_excise | 96x192 | 192x384 | difference |
|---|---|---|---|
| 1.0 | 0.40552 | 0.40705 | +0.38% |
| 1.3 | 0.50580 | 0.50579 | -0.002% |

The whole curve is converged to ~0.5%. Refining the grid does not change it, so
it is **not a truncation artefact**. The initial value also matches
(0.58286 vs 0.58464, and 0.63005 vs 0.63021), so the initial data is converged
too.

## Test 2: mask position — it is not boundary leakage

The decisive test is whether `alpha` at a **fixed physical point** depends on
where the mask sits. Compare `r_excise = 1.0` (point is in the evolved region)
against `r_excise = 1.3` (the same point is the mask edge):

| point | 96x192, r=1.0 | 96x192, r=1.3 |
|---|---|---|
| (1.516, -0.188), `r_KS = 1.306` | 0.50565 | 0.50580 |

| point | 192x384, r=1.0 | 192x384, r=1.3 |
|---|---|---|
| (1.508, -0.219), `r_KS = 1.304` | 0.50550 | 0.50579 |

Agreement is 3e-4 (0.03%) and 3e-4 (0.06%). Moving the excision boundary by
`0.3 M` does **not** change the lapse anywhere it can be compared. The inner
boundary is not leaking.

Note that `alpha_min` always sits at the first cell outside the mask simply
because `alpha` increases monotonically with `r_KS` in Kerr-Schild data; the
minimum being at the smallest radius is not evidence of anything by itself.
The 2026-10-30 file treated it as evidence; that was a mistake.

## What it actually is

The Kerr-Schild slice is not a fixed point of the DBM slicing condition, so
`alpha` evolves. That evolution is largest where `alpha` is smallest, which is
just inside the horizon, and it is reproduced identically at every resolution
and every mask position tested. It is a gauge evolution of the slice, resolved
and converged — not a defect in the boundary operator.

The minimum sits at `r_KS = 1.007` while the horizon is at `r_+ ~ 1.56`, i.e.
`0.55 M` inside. Nothing inside the horizon propagates out on the
characteristic cone, so this cannot directly contaminate the exterior.

## What is still open

- **It is bounded.** A 6000-step run (96x192, `r_excise = 0.8`, SAT off) gives
  `alpha_min = 0.34770, 0.26285, 0.23678, 0.22869, 0.22777, 0.22884` at
  `t = 0.89, 1.78, 2.67, 3.55, 4.44, 5.33`. It saturates at **~0.228**; there is
  no collapse to zero. The earlier concern that this was a time bomb is
  resolved: it is a bounded gauge adjustment inside the horizon.
- **It is not what kills the run.** The same run aborts at step 3500
  (`t = 6.22`) on the horizon invariant check: `dM_irr/M_irr = 3.6e-2`,
  `dA/A = 7.1e-2` (limit 5e-2), with the found horizon moved from 1.600 to
  1.273 and `alpha_min = 0.229`. The lapse is not collapsing; the spacetime is
  drifting. See `EXCISION_SAT_CRASH.md` for what that run does establish.
- **Is it connected to the step-1387 crash?** **No.** A 2x2 over
  `excision_sat` and `r_excise` shows the crash happens only with the legacy
  excision SAT on, at either mask radius, and the crash cell follows the mask
  edge. `alpha_min` at the crash is still `~0.38`, i.e. the lapse has not
  collapsed. See `EXCISION_SAT_CRASH.md`.

## Consequence for the plan

`alpha_min` is a gauge quantity inside the horizon. It is converged, independent
of `r_excise`, and decelerating. It should be monitored but it is not, on this
evidence, the dominant error in the physical domain and it is not a boundary
bug. Item 4 (inner boundary operator) remains a robustness item; the earlier
framing of it as "the boundary condition failing" was based on the wrong radius
and on a location that is trivially the minimum.

The exterior constraint convergence over the same window is a separate and more
troubling result: refining 96x192 -> 192x384 at `r_exc = 1.0`, `t = 1.33`,
`max|H|(ext)` goes `1.44e-1 -> 1.19e-1` and `rms|H|(ext)` `5.15e-3 -> 3.53e-3`,
i.e. observed order ~0.3-0.5, not 2. At `r_exc = 1.3` it is
`7.09e-1 -> 4.19e-1` and `1.82e-2 -> 7.36e-3` (order ~0.8-1.3). The exterior
error is also 5x larger with the mask at 1.3 than at 1.0, which is the opposite
of the old "closer to the horizon is better" reading. Both point at the
data/gauge mismatch (item 3) rather than the inner boundary.
