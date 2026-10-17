# Late-time (failure) check on the outer-boundary attribution

**Date:** 2026-10-15
**Question:** the outer-boundary dominance was measured at t≈0.5. Does it hold
at the times where the run actually dies (t≈6.7)?

## Experiment

Hold `dx` fixed (12M/Nx=48: dx=0.25532; 18M/Nx=72: dx=0.25352 — 0.7% apart),
enlarge only the domain, frozen gauge, `r_excise=1.0`, run to failure.

| domain | Nx | fail step | fail t | fail r_KS |
|---|---|---|---|---|
| 12M | 48 | 7536 | 6.73 | 1.315 |
| **18M** | **72** | **10495** | **9.34** | 1.015 |

## Results

1. **Enlarging the domain extends survival by ~39%** (t 6.73 -> 9.34) at fixed
   `dx`. So the outer boundary materially limits long-time survival, consistent
   with the early-time finding.

2. **But the run still fails, and the failure moves inward.** With the bigger
   domain the failure occurs at `r_KS = 1.015`, essentially at the excision
   surface (`r_excise = 1.0`), rather than at `r ~ 1.3`. Enlarging the domain
   does not remove the failure; it relocates it from the near-hole region to the
   inner boundary.

## Interpretation

The outer boundary is the dominant error source **early** (t<1, and it limits
survival), but it is **not the sole cause** of the terminal failure. The
late-time failure is still anchored at the inner excision boundary. So:

- Outer boundary: real, dominant early, contributes to survival time. Removing
  or fixing it buys time but does not eliminate the failure.
- Inner boundary: not the early-time bottleneck, but is where the run
  ultimately dies once the outer error is reduced enough.

Both are real. Neither alone explains the whole picture, and fixing either one
alone would not deliver a stable long run. The correct target is both, with the
outer boundary first (it is the larger early-time lever and cheaper to address
via domain size).

## Incomplete

The per-step worst-point radius trajectory (where `max|H|` sits as the run
evolves) was not captured in this session — the run logs were filtered to the
failure line only. Re-running with unfiltered logging would localize whether the
error migrates from r~12 inward to r~1 over time. This is worth doing but is a
long run; the failure-location shift above already establishes the key point.

## Reproduction

```sh
# reference (fails t=6.73)
GR_SLICING_MODE=2 GR_GAMMA_DRIVER_MODE=2 GR_HZ_ABORT=0 GR_NSTEPS=11195 \
  ./bin/vacuum_kerr baseline/experiments/latetime/dom12.par
# larger domain, same dx (fails t=9.34)
GR_SLICING_MODE=2 GR_GAMMA_DRIVER_MODE=2 GR_HZ_ABORT=0 GR_NSTEPS=11235 \
  ./bin/vacuum_kerr baseline/experiments/latetime/dom18.par
```