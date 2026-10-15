# excision_order sweep — the fill order is not the binding constraint

**Date:** 2026-10-14
**Instrument:** `GR_EXCISION_ORDER` env override (existing), no code change.
**Question:** if the failure is accumulated boundary truncation error, does a
more accurate interior extension extend survival?

## Setup

Frozen gauge (`GR_SLICING_MODE=2 GR_GAMMA_DRIVER_MODE=2`), Nx=48, `r_excise=1.0`,
`GR_HZ_ABORT=0`, identical otherwise. Only `excision_order` varies.

## Early-time error — higher order works as intended

`max|H|` (exterior), well before the nonlinear phase:

| t | order 2 | order 3 | order 4 |
|---|---|---|---|
| 0.0893 | 3.637e-2 | 3.617e-2 | 3.594e-2 |
| 0.3573 | 8.685e-2 | 7.784e-2 | 6.909e-2 |
| 0.6252 | 2.350e-1 | 1.879e-1 | 1.466e-1 |
| 0.8932 | 4.737e-1 | 3.445e-1 | **2.414e-1** |

Monotonic and clean: order 4 has **~2x less** error than order 2 at t=0.89.
The extension does what it claims.

## Lifetime — non-monotonic, and not improved

| order | fail step | fail t | fail r | fail point |
|---|---|---|---|---|
| 2 | 7536 | 6.731 | 1.315 | (1.532, -0.126) |
| 3 | **8081** | 7.218 | **1.008** | (1.277, -0.126) |
| 4 | **7152** | 6.388 | 1.312 | (0.766, -1.137) |

A 2x reduction in early error buys **no** net lifetime. Order 3 is worst-best-
worst: best lifetime, but it fails *at the excision surface itself*
(r = 1.008 vs r_excise = 1.0), whereas orders 2 and 4 fail out at r ~ 1.31.

## Conclusion

**`excision_order` is not a lever.** The fill polynomial order controls the
*early* injected error cleanly and in the expected direction, but the quantity
that actually kills the run is insensitive to it, and order 4 is actively
harmful.

This rules out "improve the interior extension accuracy" as a repair path.

## What this redirects

The fill order determines the *values* placed inside the mask. It does **not**
determine:

- which derivative branch `compute_stencil` selects at the mask edge (4th
  order vs 2nd vs one-sided),
- the cartoon y-derivative geometry (`F(s)/s` rather than `F(x)/x`, i.e.
  relative error `x/sqrt(x^2+dy^2)` — fixed at fixed `x/dx`, so it does not
  converge),
- the base resolution.

All three are independent of the fill polynomial, and all three are consistent
with what has now been measured:

| measurement | result |
|---|---|
| fill error at evolved time | bounded, saturating ~30% of background |
| stale background anchor | no measurable effect (A/B, `GR_REFRESH_BG`) |
| fill polynomial order | no effect on lifetime |
| ring RHS residual | converges, ~2nd order (3.30, 1.91) |

Taken together: **the error that eventually kills the run does not live in the
interior extension.** It enters through the exterior discretization adjacent to
the mask — the degraded stencil branch and/or the cartoon y-derivative — and
accumulates until the algebraic projection can no longer hold `gtilde` positive.

## Implications

- Gate B as originally posed (a characteristic SAT to "control an unstable
  incoming mode") is **not** supported by this evidence. There is no unstable
  mode here that the fill can control.
- If anything is wanted at the inner boundary, the direction is
  **discretization, not boundary condition**: an SBP-compatible stencil near the
  mask (the report's Plan B4), or a corrected cartoon y-derivative.
- The cheapest remaining lever is still plain resolution, which the equal-time
  scaling suggests buys ~4x survival per 2x refinement.

## State

No production code changed. Env-var overrides only. 700-step baseline
unchanged: `max|H|=1.3162E-01 rms|H|=3.8587E-03 alpha=0.910348`.

## Reproduction

```sh
for o in 2 3 4; do
  GR_EXCISION_ORDER=$o GR_HZ_ABORT=0 GR_HORIZON_EVERY=0 GR_NSTEPS=12000 \
  GR_SLICING_MODE=2 GR_GAMMA_DRIVER_MODE=2 \
  ./bin/vacuum_kerr parameter_files/vacuum_kerr.par 2>&1 | grep "Invalid state"
done
```