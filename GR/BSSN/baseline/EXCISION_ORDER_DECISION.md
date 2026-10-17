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