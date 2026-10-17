# The driver's diagnostic filter used the wrong radius (and MARGIN_FINDINGS was partly an artifact of it)

**Date:** 2026-10-17
**Supersedes parts of:** `MARGIN_FINDINGS.md`
**Related:** `PROJECTION_AND_GHOST_FINDINGS.md`

## The defect

`drivers/vacuum_kerr.F90` filtered its constraint diagnostics with

```fortran
if (sqrt(xg(i)**2 + zg(k)**2) < r_excise + diag_margin) cycle
```

That is the **cylindrical** radius rho = sqrt(x^2+z^2). The excision mask
itself is defined by the **Kerr** radius:

```fortran
! excision_mod, is_excised
is_excised = (kerr_radius_squared(xg(i), zg(k)) < r_excise**2)
```

For `bh_spin != 0` these disagree whenever `z != 0`. Ten sites in the driver
used rho; all ten now use `sqrt(kerr_radius_squared(x,z))`.

## Why this matters, and how it was found

`PROJECTION_AND_GHOST_FINDINGS.md` left an unexplained 13x disagreement between
the driver's t=0 `max|H|` and the standalone diagnostic. A byte-for-byte
replica of the driver's scan (now PART 5 of `diagnose_ghost_accuracy`)
reproduces the driver to every digit:

| Nx | driver | replica | worst-point rho | worst-point r_KS | status |
|---|---|---|---|---|---|
| 48 | 8.2028E-01 | 8.2028E-01 | 1.0893 | **0.8255** | 2 = GHOST |
| 96 | 1.8057E+00 | 1.8057E+00 | 1.1386 | **0.8125** | 2 = GHOST |
| 192 | 5.0928E-01 | 5.0928E-01 | 1.0685 | **0.7092** | 2 = GHOST |

At every resolution the driver's worst point had **r_KS < r_excise** -- it was
inside the excision region, a filled ghost point. H there is meaningless.

**The "non-convergence" of `max|H|` (0.820 / 1.806 / 0.509) was this bug.**
It was measuring H on ghost points, which has no reason to converge. It has
been withdrawn.

## Consequences for MARGIN_FINDINGS.md

`MARGIN_FINDINGS.md` reported, at margin=0.05, `max|H| = 1.18 at r ~ 1.09`
and a "~400x" hidden peak. That number came from the driver's unfiltered
`x=0` z-profile, which samples `r_KS ~ 0.38` -- deep inside the mask. **That
figure is wrong and is withdrawn.**

However the *qualitative* claim survives, for a different and now-verified
reason. With the filter corrected, the margin sweep at Nx=48, t~0.36 is:

| margin | monitors from r_KS | max\|H\| | rms\|H\| |
|---|---|---|---|
| 2.0 | 3.0 | 4.06e-3 | 3.51e-4 |
| 1.0 (default) | 2.0 | 1.17e-2 | 7.10e-4 |
| 0.5 | 1.5 | 1.53e-1 | 4.09e-3 |
| 0.2 | 1.2 | 3.19e-1 | 1.09e-2 |
| 0.05 | 1.05 | 5.25e-1 | 1.60e-2 |

There is a real ~129x error gradient concentrated within 1M of the mask. The
default margin genuinely does hide it.

## Effect on the recorded baseline

Correcting the filter changes what is *reported* (not what is evolved):

| | old (rho >= 2.0) | new (r_KS >= 2.0) |
|---|---|---|
| t=0 max\|H\| | 1.316e-1 (at r_KS = 1.884) | 1.860e-2 (at r_KS = 2.158) |
| step 700 max\|H\| | 1.3162E-01 | 1.9825E-02 |
| step 700 rms\|H\| | 3.8587E-03 | 2.0813E-03 |

The old default-margin max was being set by a point at r_KS = 1.88 -- inside
the contaminated near-hole shell. `alpha(probe) = 0.910348` is **identical**
before and after, confirming the evolution is untouched.

The step-700 baseline of record must be restated as
`max|H|(ext) = 1.9825E-02  rms|H|(ext) = 2.0813E-03  max|M|(ext) = 3.6121E-02`.

## REVERSAL: fill order does help

The earlier `excision_order` null was also measured through the broken filter
and is **withdrawn**. Measured correctly at margin=0.05:

| `excision_order` | max\|H\| | rms\|H\| |
|---|---|---|
| 2 (legacy) | 5.254e-1 | 1.600e-2 |
| 3 | 3.537e-1 | 1.244e-2 |
| 4 | **2.711e-1** | **1.010e-2** |

Monotonically better with higher order, a ~2x reduction. My previous
statement that "fill order still doesn't help (order 2 is best)" was an
artifact and was wrong.

Caveat: this does **not** yet translate into longer runs. The lifetime sweep
(7536 / 8081 / 7152 steps for order 2 / 3 / 4) was measured by the actual
`error stop` on a non-positive conformal determinant, which is independent of
the diagnostic filter, so that result stands -- and it is non-monotonic.

## Still stands

- The `GR_REFRESH_BG` null is unchanged: bit-identical with the corrected
  filter (5.2543E-01 either way). Consistent with the ghost fill being exact.
- `PROJECTION_AND_GHOST_FINDINGS.md`: the projection is negligible and the
  ghost fill is exact. Neither result depended on the filter.

## Open

- Max|H| over *physical* points only (r_KS > r_excise) is 7.77e-1 / 1.42e-1 /
  6.21e-2 for Nx = 48 / 96 / 192. It decreases, but at orders ~2.4 then ~1.2,
  not cleanly second order, and the worst point is always within ~1 cell of
  the mask. A real near-mask error remains, and it is not yet explained.

## Reproduction

```sh
make bin/diagnose_ghost_accuracy
for N in 48 96 192; do GR_DIAG_MARGIN=0.05 \
  ./bin/diagnose_ghost_accuracy baseline/experiments/ghost/n$N.par; done
```