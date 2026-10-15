# Outer boundary is the dominant error source (Theory F confirmed)

**Date:** 2026-10-15
**Supersedes:** the "boundary truncation error" attribution in
`INNER_BOUNDARY_FINDINGS.md` and `THEORY_TRIAGE.md`.

## The resolution anomaly that started this

Equal-time `rms|H|` at t=0.5, frozen gauge, `r_excise=1.0`, only Nx varying:

| Nx | 48 | 96 | 120 | 144 | 192 |
|---|---|---|---|---|---|
| rms\|H\| | 3.64e-3 | **3.52e-3** | 6.10e-3 | 9.30e-3 | 1.72e-2 |

A **minimum at Nx=96** — the scheme gets worse under refinement. No power law
exists, so no resolution budget can be fitted from this direction.

## Ruled out along the way

- **Kreiss-Oliger dissipation** (`ko_eps`): setting `ko_eps=0` barely changes
  anything and slightly *increases* the error.

  | Nx | rms\|H\| ko=0.2 | rms\|H\| ko=0.0 |
  |---|---|---|
  | 96 | 3.516e-3 | 3.604e-3 |
  | 120 | 6.098e-3 | 6.743e-3 |
  | 144 | 9.304e-3 | 1.101e-2 |

- **Mask-relative resolution**: cells from `r_excise=1.0` to the horizon grow
  monotonically (2, 4, 5, 7, 9), which cannot produce a minimum at 96.

## The cause: the OUTER boundary

Holding `dx` fixed at 0.100840 and enlarging only the domain, at t≈0.5:

| xmax | Nx | Nz | rms\|H\| | ratio |
|---|---|---|---|---|
| 12M | 120 | 240 | 6.098e-3 | — |
| 18M | 180 | 360 | 2.006e-3 | 0.329 |
| 24M | 240 | 480 | 9.292e-4 | 0.463 |

**Observed order vs domain size: 2.74, then 2.67 — clean second-order
convergence in L.** A 3x reduction in `rms|H|` from enlarging the domain alone,
with nothing else changed.

## Interpretation

This inverts the attribution. At the *production* configuration (12M domain) the
constraint violation is dominated by the **outer** boundary treatment, not the
excision mask. The apparent "excision is the problem" reading came from:

- failures clustering near the hole (where the excursion is largest and where
  the error has had longest to grow inward-facing),
- strong `r_excise` sensitivity of *when* the run dies,

but the actual error floor at fixed resolution is set by the outer boundary.

The Nx=96 minimum is then explained: refining at fixed 12M domain resolves the
inner region better while leaving the outer-boundary error untouched, so the
inner gains are masked until the outer error dominates — and the outer error
itself grows as the grid point count rises within a fixed domain, because the
Sommerfeld boundary is applied to a finer, larger array at the same physical
radius.

## Implications

- **The report's Theory F ("outer boundary and dissipation interaction — lower
  evidence, not excluded") is the correct one.** It was ranked last and is in
  fact dominant.
- The excision mask, fill order, stale background, and cartoon y-derivative were
  all measured and are minor. That work stands, but it was not the bottleneck.
- **Resolution alone cannot fix this**, because refining within a fixed domain
  makes it worse. The lever is the *outer* boundary condition and/or the domain
  size.
- Gate B (inner-boundary characteristic treatment) is **not** the critical path.
  The critical path is the outer boundary.

## Next concrete step

The outer boundary is handled by `integrators/sommerfeld.F90`. It should be
audited the same way the inner boundary was: sweep the outer radius at fixed
inner resolution, and check whether the residual is a propagating incoming mode
from r=12M. A cheap first probe is to vary `ko_eps` **together with** the domain
(now that `ko_eps` is known to be irrelevant on its own) and to test whether
the convergence order in L persists when the Sommerfeld damping width is
changed.

## Is the 1/r falloff assumption the defect?  NO

`sommerfeld.F90` applies `rhs = -c*dfdr - (f-f0)/r` to **all 24 variables**,
i.e. it assumes `u - f0 ~ 1/r` universally. For exact Kerr–Schild data the true
powers are much steeper: gtilde → 1 (no 1/r term), φ, α−1, β ~ 1/r³, and
K, Atilde, Γ̃, B ~ 1/r⁴.

Implemented as `GR_SOMMERFELD_DECAY=1` (per-variable power n, default off) and
A/B tested at fixed dx=0.1008, 12M domain, t=0.5:

| | rms\|H\| | max\|H\| |
|---|---|---|
| legacy (n=1 everywhere) | 6.098e-3 | 1.004e-1 |
| per-variable (n=3/4, n=0 for gtilde) | **5.190e-2** | **5.274e-1** |

**8.5x worse.** The asymptotically correct condition is dramatically worse than
the incorrect uniform one.

Interpretation: `-(f-f0)/r` is not acting as an asymptotic-correctness device —
it is acting as **damping**, and the correct powers weaken it (n=0 removes it
entirely for the gtilde diagonal, which is where the asymptotic answer says no
1/r term belongs). That lets boundary-generated error propagate inward. The
legacy n=1 is a better *compromise* than the correct one.

So the defect is **not** the falloff exponent. It is the structure of the
condition: a single-layer, all-fields, first-order outgoing prescription applied
independently to each of the 24 variables. That is not a characteristic
boundary condition — a coupled system has 24 characteristic fields, and
prescribing `u_t` for each independently reflects incoming modes. The 2nd-order
convergence in L is that reflection, decaying as the domain grows.

## Outer-radius sweep is dx-independent

Same 12M → 18M comparison at a second, coarser dx (~0.202):

| xmax | rms\|H\| dx≈0.20 | rms\|H\| dx≈0.10 |
|---|---|---|
| 12M | 1.184e-3 | 6.098e-3 |
| 18M | **5.274e-4** | 2.006e-3 |

~2.2x reduction in both cases. The outer-boundary dominance is not a
resolution artefact.

## Caveat

Measured at t≈0.5, one spin (0.8), one `r_excise` (1.0), frozen gauge. The
*early-time* error floor is outer-boundary dominated; whether this stays true at
the times where the run actually fails (t≈6.7) is not yet established and should
be checked before acting on it.

## Reproduction

```sh
# outer-domain scaling at fixed dx
for cfg in "12 120 240" "18 180 360" "24 240 480"; do
  set -- $cfg
  sed -e "s/^nx   = 48/nx   = $2/" -e "s/^nz   = 96/nz   = $3/" \
      -e "s/^xmax = 12.0/xmax = $1.0/" -e "s/^zmax = 12.0/zmax = $1.0/" \
      -e "s/^out_every = 100/out_every = 1000000/" \
      parameter_files/vacuum_kerr.par > /tmp/dom.par
  GR_HZ_ABORT=0 GR_HORIZON_EVERY=0 GR_NSTEPS=1403 \
  GR_SLICING_MODE=2 GR_GAMMA_DRIVER_MODE=2 ./bin/vacuum_kerr /tmp/dom.par
done
```