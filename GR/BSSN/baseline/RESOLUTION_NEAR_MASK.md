# Resolution vs. the near-mask momentum residual

**Date:** 2026-10-17
**Instrument:** the near-mask band added in `NEAR_MASK_DIAGNOSTIC.md`
**Status:** the resolution→lifetime question is still open; the run was killed
before failing. Two other findings came out of the partial data.

## Setup

Identical configuration, resolution the only difference:

| | Nx | Nz | dx | dt | out\_every |
|---|---|---|---|---|---|
| reference | 48 | 96 | 0.2553 | 8.93e-4 | 100 |
| refined | 96 | 192 | 0.1263 | 4.44e-4 | 500 |

`xmax = zmax = 12`, `r_excise = 1.0`, `excision_order = 3`, live gauge.
Compared at **equal physical time**, not equal step number.

## Finding 1: the near-mask momentum residual is catastrophically
ill-conditioned at production resolution

Near-mask `max|M|`, equal $t$:

| $t$ | Nx=48 | Nx=96 | improvement |
|---|---|---|---|
| 0.22 | 1.613 | 0.217 | 7.4x |
| 1.55 | 28.85 | 2.347 | 12.3x |
| 2.67 | 3.505 | 2.835 | 1.2x |
| 3.11 | 13.73 | 5.262 | 2.6x |
| **3.33** | **1.243e+08** | 6.817 | 1.8e+07 x |
| 3.55 | 2.312e+04 | 8.274 | 2.8e+03 x |
| 3.78 | 2.157e+02 | 9.463 | 22.8x |
| 4.22 | 2.828e+02 | 16.52 | 17.1x |
| **4.44** | **1.052e+07** | 28.17 | 3.7e+05 x |

At $N_x = 48$ the momentum constraint develops **isolated spikes reaching
$10^{8}$** at individual grid points, alternating with moderate values of
order $10^{2}$. The excursion at $t = 3.33$ is $1.24\times10^{8}$ — eight
orders of magnitude above the exterior residual ($4.9\times10^{-2}$) and seven
orders above the near-mask Hamiltonian.

At $N_x = 96$ these spikes do not occur; the near-mask `max|M|` stays below
$2.9\times10^{1}$ throughout the same interval.

This is a genuine resolution effect and it is exactly the fill/stencil
inconsistency signature that the near-mask band was added to look for. It is
the most violent single behaviour measured in this project that is *not* the
conformal-determinant failure.

Caveat, and it is important: the run aborts on a non-positive conformal
determinant, **not** on the momentum residual. So these spikes are a symptom
of a severely ill-conditioned near-mask region, not a demonstrated cause of the
abort. They may be an indicator rather than a mechanism.

## Finding 2: refinement does not monotonically improve the exterior constraints

Exterior ($r_{\mathrm{KS}} \geq 2.0$), equal $t$:

| $t$ | Nx=48 rms\|H\| | Nx=96 rms\|H\| | Nx=48 max\|H\| | Nx=96 max\|H\| |
|---|---|---|---|---|
| 0.22 | 9.65e-4 | **3.54e-3** | 1.20e-2 | **3.34e-2** |
| 1.78 | **3.58e-3** | 2.19e-3 | **7.67e-2** | 5.52e-2 |
| 2.67 | **4.88e-3** | 2.88e-3 | **9.93e-2** | 9.02e-2 |
| 3.33 | **4.23e-3** | 3.63e-3 | **8.10e-2** | 1.13e-1 |
| 4.44 | **3.62e-3** | 5.11e-3 | **4.83e-2** | 1.37e-1 |

Refinement is **worse at early times** (3.7x worse in rms at $t = 0.22$) and
**worse again at late times**, with a window in between where it is better.
This reproduces the non-monotonic resolution behaviour noted early in the
project, now measured with the corrected diagnostics and at equal physical time.

The practical consequence: there is no power law relating resolution to
constraint accuracy over this range, so no resolution budget can be fitted and
"just refine it" is not a supported remedy.

## Still open

The refined run was killed by the harness timeout at step 10500 ($t = 4.66$)
with no failure recorded. **Its lifetime is unknown.** So whether refinement
buys survival time remains unanswered — and given Finding 2 it is not obvious
that it would.

## Reproduction

```sh
# reference (fails t=6.07)
GR_HZ_ABORT=0 GR_HORIZON_EVERY=0 GR_NSTEPS=20000 \
  ./bin/vacuum_kerr parameter_files/vacuum_kerr.par

# refined -- needs roughly 2.5 hours; use stdbuf -oL
stdbuf -oL env GR_HZ_ABORT=0 GR_HORIZON_EVERY=0 GR_NSTEPS=40000 \
  ./bin/vacuum_kerr baseline/experiments/reslife/n96.par
```

Logs: `baseline/experiments/order/live_o3.log` (Nx=48) and
`baseline/experiments/reslife/n96.log` (Nx=96).