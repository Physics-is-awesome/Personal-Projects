# Current work on long-term Kerr steps 1–3

**Date:** 2026-11-02  
**Source state:** current uncommitted working tree; production baseline was built from this tree before the diagnostic-only edits below.  
**Parameters:** `parameter_files/vacuum_kerr.par`, unless stated otherwise.

This records a fresh baseline and the current validation status. It supersedes
older descriptions of the default data/gauge pairing where they conflict with
the live parameter file. The production configuration is now DBM Kerr trumpet
data (`initial_data_mode=1`), route-C slicing (`slicing_mode=3`), Gamma-driver
mode 4, `r_excise=1.5M`, `a=0.8M`, and a `12M` outer domain. SAT is disabled.

## 1. Reproducible baseline

The 96×192 production run used:

```sh
make run PARFILE=parameter_files/vacuum_kerr.par \
  RUN_DIR=/tmp/kerr_steps123_baseline \
  RUN_ENV='GR_NSTEPS=6000 GR_OUT_EVERY=500 GR_HORIZON_EVERY=100 GR_HZ_ABORT=0 GR_NEAR_MASK_W=0'
```

It completed 6,000 steps at `t/M=2.6655`, without a determinant abort. The
apparent horizon was found through step 4,100 and then not found from step
4,200 through step 6,000. At step 6,000, exterior `max|H|=3.0323e-1`,
`rms|H|=8.6009e-3`, and `max|M|=6.2315e-2`. The found area started at
`43.74535` (`+8.79%` relative to exact Kerr) and had already drifted to about
`43.35` before the finder lost the surface. The lapse stayed finite.

The same temporary run directory was then reused for zero-step initial-data
checks at higher resolutions. Its `output/` directory now contains files from
different runs and is not a self-consistent baseline archive; use the command
above to reproduce the 96×192 run in a fresh output directory.

Zero-step production initializations at 192×384 and 384×768 found:

| Grid | Exterior `max|H|` | Exterior `rms|H|` | Horizon shell `max|H|` | Initial horizon |
|---|---:|---:|---:|---|
| 96×192 | `1.9503e-1` | `5.4810e-3` | not separately reported | found, area `43.74535` (`+8.79%`) |
| 192×384 | `2.4431e-2` | `1.9746e-3` | `5.4409e-2` | found, `dA/A_Kerr=+1.33%` |
| 384×768 | `4.9007e-2` | `2.6455e-3` | `1.4365e-2` | not found (`ierr=2`) |

The outer-domain maximum is not monotone under refinement, so it should not be
used as the sole convergence measure. Near-horizon shells and fixed-radius
annuli are more informative. The 384×768 horizon-finder failure is a real
acceptance failure that needs its own investigation; do not infer convergence
from the 96/192 area improvement alone.

The chart change in the current source uses the standard Kerr radius and full
Cartesian Jacobian. At spin 0.8, points rejected as inside the closed-form
trumpet domain are all interior to the `r_excise=1.5M` mask and are radially
projected for stencil filling; no flat placeholders were used in the observed
96/192/384 initialization diagnostics. This differs from the older
`ROUTE_C_INTERIOR.md` diagnosis of an exterior chart singularity.

## 2. Momentum-constraint cross-check

`make test-momentum-conv` passes in the Kerr-Schild annulus `[3M,5M]` at
96×192, 192×384, and 384×768. The measured RMS norms are
`4.975e-5`, `1.231e-5`, and `3.058e-6`; the observed order is about 2.01.

The point diagnostic had two reproducibility/reference bugs, now corrected in
`helper/diagnose_momentum_point.F90` and its Makefile target:

1. Its hard-coded 192-grid x coordinate was halfway between grid points on
   the 96×192 grid, so nearest-point selection chose the axis. A negative x
   argument now selects the first positive x point, and the target parameter
   file and coordinates are configurable.
2. The independent ADM calculation reconstructed the central `K_ij` with the
   physical metric in a conformal formula, multiplying by `exp(4 phi)` twice.
   It now uses `gtilde_ij` in that trace term, matching its already-correct
   neighboring-point reconstruction.

At the near-axis target and same-radius off-axis control, stored-vs-analytic
Kerr-Schild BSSN fields agree to roundoff. At the near-axis point the BSSN and
separate physical ADM momentum vectors both decrease under refinement:

| Grid | `|M|` BSSN | `|M|` ADM-form |
|---|---:|---:|
| 96×192 | `1.61e-4` | `1.12e-3` |
| 192×384 | `5.62e-5` | `3.38e-4` |
| 384×768 | `1.07e-5` | `7.79e-5` |

The independent assembly is still noisier than the production constraint at
this point, but both show a decreasing residual. The earlier `~1e-1` discrepancy
and the later `~2e-2` ADM residual came from obsolete assembly code and the
buggy central ADM reconstruction; they are not evidence of a remaining
momentum-constraint defect. The remaining limitations are the localized
finite-difference accuracy near the polar horizon and the fact that the
pointwise diagnostic is a report, while the convergence test’s hard gate is
the annular RMS.

Reproduce the corrected point comparison with:

```sh
make diagnose-momentum-point \
  MOMENTUM_PAR=parameter_files/vacuum_kerr_192.par \
  MOMENTUM_X=-1.0 MOMENTUM_Z=-1.535248
env GR_NX=384 GR_NZ=768 make diagnose-momentum-point \
  MOMENTUM_PAR=parameter_files/vacuum_kerr_192.par \
  MOMENTUM_X=-1.0 MOMENTUM_Z=-1.535248
```

The route-C DBM initial state was then checked separately with
`make diagnose-dbm-momentum-point`. At the first positive-x point nearest
`z=-1.535248M`, stored fields match `state_at` to `2.3e-14` or better on all
three grids. The production BSSN momentum vector decreases from `8.05e-4` to
`1.79e-4` to `2.81e-5` across 96×192, 192×384, and 384×768. An independent
physical ADM divergence assembled from the closed-form point data is
`4.6e-8`, `2.8e-8`, and `2.0e-7`, respectively; its fine-grid value is limited
by nested fixed-step finite-difference noise. Thus the DBM point residual is a
discretization error that decreases under refinement, not evidence of a bad
data conversion or an algebraic constraint mismatch. The seven production
pieces also sum exactly to the reported vector. This route-C result should not
be conflated with the older large Kerr-Schild near-band peak in report §13.7;
that historical case used a different initializer and diagnostic path.

## 3. Stationary Kerr data/gauge validation

`make diagnose-eq-kerr-data` passes its structural Part A checks. The production
RHS and constraints were measured on route-C data in the fixed Euclidean-radius
annulus `[3M,5M]`, with matching gauge modes 3/4:

| Grid | `max|H|` | `max|M|` | `max|RHS_K|` | `max|RHS_A|` | `max|RHS_alpha|` | `max|RHS_phi|` |
|---|---:|---:|---:|---:|---:|---:|
| 96×192 | `2.274e-3` | `2.255e-4` | `1.648e-4` | `2.386e-3` | `4.307e-4` | `1.031e-5` |
| 192×384 | `6.155e-4` | `5.899e-5` | `4.289e-5` | `6.945e-4` | `4.356e-4` | `2.622e-6` |
| 384×768 | `1.598e-4` | `1.498e-5` | `1.076e-5` | `3.805e-4` | `4.340e-4` | `6.611e-7` |

The Hamiltonian, momentum, `K`, `Atilde`, and `phi` residuals decrease with
refinement. The lapse RHS plateaus near `4.34e-4` instead of converging to
zero. Therefore the current `f(alpha)` table plus DBM data is a useful
near-stationary candidate, not yet a validated exact fixed point. This plateau
and the high-resolution horizon-finder failure block acceptance of Gate A.

The lapse and horizon follow-up is recorded in
`baseline/LAPSE_HORIZON_DIAGNOSTIC.md`. The same-point gauge comparison
attributes the lapse RHS plateau to the mismatch between the pointwise DBM
slicing relation and the tabulated function; it does not yet split table
interpolation error from off-equatorial spread. At 384×768 the horizon finder
was rejecting a marginal candidate because `r_excise=1.5` and its coverage
buffer reached beyond the surface. With `r_excise=1.2`, the initial horizon is
recovered with the expected area and expansion residual.

Completed in the follow-up: the dense equatorial comparison now isolates
interpolation error from off-equatorial spread (`baseline/LAPSE_HORIZON_DIAGNOSTIC.md`),
and the table is retained because angular spread is larger than the table error.
The high-resolution horizon recovery is fixed by offsetting only the finder
mask; the evolution excision remains at `r_excise=1.5`. A matched 6,000-step
96×192 run completed. The first equal-physical-time 12,000-step 192×384 attempt
stopped after the step-400 checkpoint without a retained exit log. A tracked
retry in `/tmp/kerr_routec_192_maskfix_retry` completed successfully. The
higher resolution retains a converged horizon through `t=2.6586`; the coarse
finder is rejected from `t=0.8885` onward. At matched final time the exterior
Hamiltonian max/RMS improve by factors 3.66/3.84, but the fine-grid horizon
area is still 0.643% below Kerr. This points to a resolved coarse finder
coverage issue plus remaining gauge/continuum drift, not full long-term
stationarity.

The next checks have now been completed on the 192x384 checkpoints at steps
0, 4,000, 8,000, and 12,000. Independent horizon re-solves converge at every
checkpoint (fit residual below `2.1e-12`, fit-order area spread about
`0.04%`), while the area moves from `-0.004%` to `-0.64%` relative to Kerr.
This confirms a real evolved geometry drift rather than a finder failure.
Physical-cell lapse minima rise from `0.2651` to `0.2766`, and physical-cell
`phi` minima rise from `0.01032` to `0.01114`; maxima change much less. The
same run's momentum error grows near excision and the outer boundary, while
the 3–8M bulk converges. The evolution therefore remains bounded through
`t≈2.66M` at this resolution, but it is not stationary.

The all-angle fit probe spans `R=1.55..8M`, `|cos(theta)|=0..1`, and
`alpha=0.25..0.85`. Its piecewise scalar fit reduces the aggregate
volume-weighted continuum lapse-residual RMS from `5.53e-2` to `2.28e-2` at
96 bins. However, a smooth 96-bin profile checked with the production RHS
fails the radial acceptance check: in `[3M,5M]` the lapse RHS max/RMS is
`5.19e-3` / `2.34e-3` at both 192x384 and 384x768, versus
`4.34e-4` / `1.03e-4` for the current table. It was rejected and no
experimental production mode was retained. This shows that the aggregate
weighted score hides a substantial exterior degradation.

A fit restricted to `[3M,5M]` improves its sampled weighted residual RMS by
only about 11% (`4.57e-3` to `4.09e-3`) and has not been shown to improve the
full discrete RHS. Radial bins expose the tradeoff directly: the global fit
lowers lapse-residual RMS from `1.23e-1` to `5.00e-2` in `1.55..3M`, and from
`4.71e-3` to `4.09e-3` in `3..5M`, but raises it from `1.25e-3` to `3.07e-3`
in `5..8M`. A fit restricted independently to each shell has nearly the same
tradeoff. More resolution in a single scalar `f(alpha)` fit will not remove
this radial conflict.

Next, compare constructing Kerr data compatible with one scalar Bona-Masso
law against a smooth spatially dependent lapse coefficient/source for the
DBM background. Require a convergent full RHS plus bounded horizon and
constraint measures before a long run. Do not begin 50,000-step acceptance
runs or reduce the PDE excision radius based on the failed `1.2` and `1.3`
probes.

Do not call the current route-C data stationary until the lapse RHS plateau is
explained and the initial horizon is reliably recovered. Do not tune the
long-time evolution using the previous Kerr-Schild-only explanations without
rechecking them against this matched DBM configuration.
## 4. Evolved momentum residual localization (2026-11-03)

`helper/diagnose_momentum_snapshot.F90` re-evaluates the production momentum
constraint on saved BSSN states. Pass the run's step-zero HDF5 state as the
third argument so excised cells use the same frozen background as the run:

```sh
make diagnose-momentum-snapshot PARFILE=parameter_files/vacuum_kerr_192.par \
  SNAPSHOT=/path/to/output/state_012000.h5 BACKGROUND=/path/to/output/state_000000.h5
```

The diagnostic reports Kerr-radius bands, angular bins near excision, the
near-axis point and same-radius off-axis control, seven additive momentum
pieces, and the x/y/z parts of the raised-A divergence.

The evolved residual is not an axis-only failure. At 192x384 and `t≈2.66`,
the near-axis and same-radius off-axis norms are `7.02e-3` and `1.17e-2`;
both have grown substantially from their initial values. The maximum lies
just outside excision (`r_KS≈1.515`, `|M|=2.27e-2`). At 96x192 and the same
time, the maximum in that band is `1.29e-1`, so the inner-edge error improves
under refinement but remains the dominant local defect. Angular maxima shift
between bins and resolutions, which does not support a polar-axis-specific
cause.

The bulk `r_KS=3..8` maximum and RMS decrease under refinement. At the outer
edge (`r_KS>12`), however, the 192x384 maximum/RMS (`1.56e-2` / `2.00e-3`)
exceed the 96x192 values (`9.41e-3` / `1.49e-3`). The raised-A divergence is
the largest contribution near excision; at the outermost maxima it is mostly
the x and z directional pieces. This localizes the next work to the inner
mask-adjacent stencil and outer-boundary closure, not a change to the
continuum BSSN equations.

The momentum snapshot study above localizes a secondary numerical concern to
the mask-adjacent and outermost regions. After the gauge-law experiment, add a
frozen-state stencil isolation comparison for the raised-A divergence in those
two regions across 96x192, 192x384, and 384x768. Do not change the boundary
operator unless the controlled comparison identifies its stencil as the
source.
