# The diagnostic margin was hiding the problem

> **CORRECTION (2026-10-17).** Partly wrong; see `DRIVER_FILTER_BUG.md`.
>
> The figures `max|H| = 1.78` and `H = 1.1764 at r ~ 1.09` below were measured
> through a driver filter that selected points by the **cylindrical** radius
> `sqrt(x^2+z^2)` instead of the **Kerr** radius. At margin=0.05 the reported
> worst point had `r_KS = 0.83` -- *inside* the excision region, a filled ghost
> point where H is meaningless. Those numbers are **withdrawn**.
>
> The qualitative claim **survives**, verified separately: with the filter
> corrected, `max|H|` really does rise from 4.06e-3 at `r_KS >= 3` to 5.25e-1
> at `r_KS >= 1.05`, a ~129x gradient concentrated within 1M of the mask.
>
> Also withdrawn: the `excision_order` null in this file. Measured correctly,
> higher fill order *reduces* the near-mask violation (5.25e-1 / 3.54e-1 /
> 2.71e-1 for order 2 / 3 / 4). The "still doesn't help" conclusion was an
> artifact of the broken filter.

**Date:** 2026-07-16
**Status:** this supersedes the attribution in `THEORY_TRIAGE.md`,
`INNER_BOUNDARY_FINDINGS.md`, `OUTER_BOUNDARY_FINDINGS.md`,
`EXCISION_ORDER_SWEEP.md` and `CARTOON_YDERIV_FINDINGS.md`.

## The finding

`drivers/vacuum_kerr.F90` computes `max|H|(ext)` only over
`r > r_excise + diag_margin`, with `diag_margin = 1.0` by default. That
excludes the band `1.0 < r < 2.0` — which is exactly where the constraint
violation peaks.

Equal physical time (t = 0.36), frozen gauge, Nx=48, varying only the margin:

| margin | monitored from | max\|H\| |
|---|---|---|
| 2.0 | 3.0M | 4.06e-3 |
| 1.0 (default) | 2.0M | 8.69e-2 |
| 0.5 | 1.5M | 3.19e-1 |
| 0.2 | 1.2M | **1.78** |
| 0.05 | 1.05M | **1.78** |

A factor of **~400** between the reported and the true peak. The true peak sits
at **r = 1.089**, i.e. 0.089 M outside the excision surface.

## It is present at t = 0, in the exact initial data

The x=0 profile of H at **t = 0**, before any evolution:

```
z = -0.3789   H =  1.1764E+00     <- near the hole
z = -3.1579   H = -2.08e-03
z = -5.9368   H = -1.46e-04
z = -8.7158   H = -2.45e-05
z =-11.4947   H =  5.55e-03
```

Kerr–Schild is an exact vacuum solution, so the continuum constraint is zero
everywhere. An O(1) violation at r ~ 1.1 is therefore **pure discretisation
error**, present in the initial state — not something that accumulates during
the run. It decays by ~4 orders of magnitude over ~10 M.

The "linear growth of max|H|" documented throughout the report is this
fixed near-hole error slowly propagating outward, not a growing instability.

## Consequences for every earlier conclusion

All the earlier work measured `max|H|(ext)` at the default margin, i.e. the
far-field *tail* of a near-mask peak. Re-testing the two most promising levers
against the correct metric:

| lever | margin=1.0 | margin=0.05 |
|---|---|---|
| `excision_order` 2 / 3 / 4 | 8.69e-2 / — / — | 1.779 / 1.897 / 2.078 |
| `GR_REFRESH_BG` 0 / 1 | 0.474 / ~0.479 | **bit-identical** |

Both nulls **survive**: fill order still doesn't help (order 2 is best), and
the stale background is bit-identical. Those conclusions stand.

What does *not* stand is the localisation. The error is not "at r ~ 2.05"
(the inner edge of the default monitored band — an artefact of where the
monitor stops), and the outer-boundary 2nd-order convergence was measured in
the far field and is a minor, resolution-dependent effect.

## What the problem actually is

An O(1) Hamiltonian-constraint violation at the excision margin, present in
the initial discretisation, decaying outward over ~10 M.

The remaining suspects are all things that could produce a large local error at
`r ~ 1.1` specifically:

1. **Stencil degradation at the mask edge.** At `dx = 0.255`, a point at
   r = 1.089 is ~0.4 cells from the `r_excise = 1.0` surface. Its 4th-order
   stencils are unavailable; `stencil.F90` falls back to 2nd-order centred or
   one-sided. The Riemann-invariance and det(gtilde)=1 conditions are then
   enforced only to the degraded order.
2. **The projection fighting the RHS near the mask.**
   `project_bssn_constraints` rescales gtilde to det = 1 and removes
   tr(Atilde) at *every RK stage*. Near the mask the degraded stencil produces
   a det != 1 state, and the projection then makes a large correction four
   times per step. This is the classic way an algebraic projection manufactures
   its own constraint violation, and it has never been measured.
3. **The `H(z)` peak at z ~ -0.38 sits near the polar horizon**, not on the
   equator, suggesting the Kerr-radius mask geometry interacts with the
   Cartesian grid in a way that is worst near the polar direction.

## Next step

Measure the **magnitude of the projection correction** per stage near the mask
(`|det(gtilde) - 1|` before projection, and `|scale - 1|` applied). That is the
one candidate above that is both untested and cheap, and if it is large it is a
self-sustaining error source: projection → det=1 → next stage sees a metric it
cannot integrate consistently → larger det error.

This is the single most informative next measurement, and it is a small
diagnostic rather than a production change.

## Reproduction

```sh
GR_DIAG_MARGIN=0.05 GR_SLICING_MODE=2 GR_GAMMA_DRIVER_MODE=2 GR_NSTEPS=1 \
  ./bin/vacuum_kerr <par>     # see the t=0 H(z) profile and worst point
```