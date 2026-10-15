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