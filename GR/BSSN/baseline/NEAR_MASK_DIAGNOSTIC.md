# Near-mask constraint diagnostic

**Date:** 2026-10-17
**Motivated by:** `DRIVER_FILTER_BUG.md`

## Problem

The exterior statistics `max|H|(ext)` / `max|M|(ext)` deliberately exclude
`r_KS < r_excise + diag_margin` (default 1.0M). That is the right choice for a
smoothness monitor, but it makes the driver structurally **blind** to the first
~4 cells outside the excision surface -- which is exactly where the largest
constraint violation in the domain lives.

Measured at the production configuration (Nx=48, r_excise=1.0, t=0.0018):

```
 step     2  t=  0.0018  max|H|(ext)=  1.8549E-02  rms|H|(ext)=  1.2120E-03
          max|M|(ext)=  2.4680E-02  alpha(probe)=  0.923257
    near-mask r_KS <   3.0000E+00  n=   192  max|H|=  7.8223E-01
         max|M|=  1.5999E+00  worst r=  1.0078E+00
```

The near-mask band carries **42x** the Hamiltonian violation and **65x** the
momentum violation of the exterior band. Both were previously invisible.

The near-mask `max|H| = 7.8223E-01` matches the independent
`diagnose_ghost_accuracy` measurement (7.7743E-01) to three digits, so the two
instruments agree.

## What was added

`drivers/vacuum_kerr.F90` now also reports, on every output step, the band

```
r_excise < r_KS < r_excise + near_mask_w
```

with `near_mask_w` defaulting to 2.0M and overridable via `GR_NEAR_MASK_W`.
Setting `GR_NEAR_MASK_W=0` silences it.

Reported: `n` (points in band), `max|H|`, `max|M|`, and the Kerr radius of the
worst-`|H|` point, so the failure location is visible without a separate run.

## Cost

The band is a strict subset of the region already scanned for the exterior
statistics, so it adds no `compute_stencil` work -- only a second pass over the
same `Hgrid`/`Mgrid` arrays that have already been computed. No effect on the
evolution.

## Why `max|M|` is reported too

The momentum residual in the band is 1.60 against 2.47e-2 outside. Since the
band is where the fill injects its error into the stencil, a large `max|M|`
there is the signature of a fill/stencil inconsistency rather than a gauge
problem, which is a useful discriminator between the two failure families the
report tracks.

## Reproduction

```sh
./bin/vacuum_kerr parameter_files/vacuum_kerr.par          # band on (default)
GR_NEAR_MASK_W=0 ./bin/vacuum_kerr parameter_files/vacuum_kerr.par   # off
```