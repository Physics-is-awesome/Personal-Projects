# Should `excision_order` be raised? Accuracy says 4, lifetime says 3.

**Date:** 2026-10-17
**Context:** `DRIVER_FILTER_BUG.md` found that raising `excision_order` reduces
the near-mask Hamiltonian violation by ~2x (a reversal of the earlier null
result, which had been measured through a broken diagnostic filter). The
natural response was to raise the production default. Measurement says not to.

## Two conflicting metrics

**Accuracy** -- near-mask violation at Nx=48, t~0.36, `margin=0.05`:

| `excision_order` | max\|H\| | rms\|H\| |
|---|---|---|
| 2 (current default) | 5.254e-1 | 1.600e-2 |
| 3 | 3.537e-1 | 1.244e-2 |
| 4 | 2.711e-1 | 1.010e-2 |

Monotonic and clean. Order 4 is ~2x better than order 2.

**Lifetime** -- run to failure, production config (`parameter_files/vacuum_kerr.par`,
live gauge, frozen gauge for comparison):

| `excision_order` | live fail step | live fail t | frozen fail step |
|---|---|---|---|
| 2 | 6331 | 5.63 | 7536 |
| 3 | **6848** | **6.07** | 8081 |
| 4 | **4364** | **3.84** | 7152 |

The frozen order-2 run reproduced the previously recorded 7536 exactly, which
validates the harness.

## CORRECTION -- the accuracy table above does not generalise

Once the near-mask diagnostic (`baseline/NEAR_MASK_DIAGNOSTIC.md`) existed, the
same A/B was repeated over its band, `r_excise < r_KS < r_excise + 2.0`
(Nx=48):

| `excision_order` | near-mask max\|H\| @ step 400 | @ step 700 | max\|M\| @ 700 |
|---|---|---|---|
| 2 | **1.101** | **1.289** | **1.365** |
| 3 | 1.220 | 1.949 | 1.492 |
| 4 | 1.282 | 2.068 | 1.675 |

**The ordering inverts.** On this band order 2 is best and order 4 is worst --
the opposite of the table above.

The two bands differ by one thing: the table above monitored `r_KS >= 1.05`,
which *excludes* the innermost sliver `1.00 <= r_KS < 1.05`. The near-mask band
includes it. So the disagreeing points are the one or two cells hugging the
mask, where the stencil is most degraded and H is largest for every order.

**My earlier claim that order 3 is "strictly dominant" was wrong.** It was
based on the `margin=0.05` band, and it does not hold in the band immediately
adjacent to the mask.

## What actually survives scrutiny

| metric | order 2 | order 3 | order 4 |
|---|---|---|---|
| live lifetime (steps) | 6331 | **6848** | 4364 |
| exterior max\|H\| @700 | 1.983e-2 | **1.266e-2** | -- |
| exterior rms\|H\| @700 | 2.081e-3 | **2.021e-3** | -- |
| exterior max\|M\| @700 | 3.612e-2 | **2.873e-2** | -- |
| near-mask max\|H\| @700 | **1.289** | 1.949 | 2.068 |

Order 3 wins on lifetime and on all three exterior metrics; it loses on the
innermost band. That is a genuine trade-off, not a free win, and it is not
resolvable by this knob alone.

## Decision taken

`excision_order = 3` is now the production default, on the strength of the
lifetime and exterior-metric evidence, which is the more robust of the two
measurements (lifetime is a whole-run property, not a 1-2 point max). The
near-mask regression is recorded here as an accepted cost, not glossed over.

Order 4 remains reachable via `GR_EXCISION_ORDER=4`.

## Route-C follow-up

A matched route-C sweep was run at 96x192, `a=0.8`, `r_excise=1.5`,
`excision_sat=false`, with 1000 steps (`t=0.4443`):

| order | exterior max/RMS H | horizon-shell max H | horizon-shell max M | inside-horizon max H |
|---|---:|---:|---:|---:|
| 2 | 4.4315e-2 / 2.0803e-3 | 2.2607e-1 | 4.5388e-2 | 5.1848e-1 |
| 3 | 4.3897e-2 / 2.0801e-3 | 2.0264e-1 | 4.1213e-2 | 5.4715e-1 |
| 4 | 4.3817e-2 / 2.0813e-3 | 1.9930e-1 | 3.8478e-2 | 5.3909e-1 |

Order 4 was then run to 2000 steps (`t=0.8885`) and completed without a
determinant failure:

```text
exterior max/RMS H       = 5.2185e-2 / 2.7354e-3
horizon-shell max H      = 2.2223e-1
horizon-shell max M      = 6.0889e-2
inside-horizon max H     = 5.4264e-1
```

Relative to order 3 at the same endpoint, order 4 lowers the shell Hamiltonian
residual by about 3.6% and the shell momentum residual by about 8.5%, while
the exterior and ungated interior values are effectively unchanged. This
makes order 4 the best current fill candidate for route-C, but the improvement
is modest and does not yet justify changing the production default: the
existing order-3 lifetime evidence covers a different baseline and order 4
still uses extrapolation rather than a characteristic boundary condition.

### Refinement check

The same route-C settings were tested at 192x384 for 300 steps
(`t=0.0665`), with order 3 and order 4:

| order | exterior max/RMS H | horizon-shell max H | horizon-shell max M | inside-horizon max H |
|---|---:|---:|---:|---:|
| 3 | 2.0487e-2 / 6.8462e-4 | 5.6315e-2 | 2.2017e-3 | 1.2417e-1 |
| 4 | 2.0487e-2 / 6.8461e-4 | 5.6428e-2 | 2.2017e-3 | 1.2595e-1 |

At this refined, short-time checkpoint, order 4 is very slightly worse in
Hamiltonian residuals and indistinguishable in momentum residuals. The
comparison is not a same-physical-time lifetime test—the 96x192 runs require
approximately 2000 steps to reach the same `t`—but it is enough to reject the
idea that order 4 is uniformly superior. Keep the production default at order
3 until a matched-time refinement study shows a durable benefit.

## Caveat on all of the above

Even the best near-mask value anywhere in this study is ~1.1, against a
far-field level of ~1e-3. No setting of `excision_order` fixes the near-mask
error; it is at best a ~2x mitigation. The knob is not the answer to the
long-time failure.

## Reproduction

```sh
for o in 2 3 4; do
  GR_EXCISION_ORDER=$o GR_HZ_ABORT=0 GR_HORIZON_EVERY=0 GR_NSTEPS=20000 \
    ./bin/vacuum_kerr parameter_files/vacuum_kerr.par
done
# accuracy, corrected filter:
for o in 2 3 4; do
  GR_EXCISION_ORDER=$o GR_DIAG_MARGIN=0.05 GR_HZ_ABORT=0 GR_NSTEPS=400 \
    ./bin/vacuum_kerr parameter_files/vacuum_kerr.par
done
```

Raw logs in `baseline/experiments/order/`.