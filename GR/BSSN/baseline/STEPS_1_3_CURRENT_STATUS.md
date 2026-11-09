# Current work on long-term Kerr steps 1–3

**Date:** 2026-11-05

**Source state:** branch `bssn-long-term-phase0`; the physics/run source is commit
`59d6cddc`. Follow-up commits add status updates, the baseline regression
script/target, the Phase 2 sweep record, and diagnostic-only one-sided
instrumentation; they do not change the evolution path. The combined
`make clean && make -j4 && make check` gate is green on `b1a97b03`
(`run_archive/2026-11-04_phase0_post_fix/make_clean_check_final_20261107.log`,
`MAKE_CLEAN_CHECK_EXIT:0`) and again on the high-order diagnostic commit
`2e4b3243` (`make_clean_check_final_20261107_highorder.log`,
`MAKE_CLEAN_CHECK_EXIT:0`). `make baseline-regression` passes end-to-end on
the final diagnostic tree (`baseline_regression_final_20261107.log`,
`BASELINE_REGRESSION_EXIT:0`), with the same numbers as the earlier run. The
current branch HEAD is `24c2ce9e`, a user commit whose only BSSN change is
`nsteps` `100000 -> 42000` in `vacuum_kerr.par`; it does not affect the
6000-step regression, which overrides the step count.

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

It completed 6,000 steps at `t/M=2.6655`, without a determinant abort. With
`GR_NEAR_MASK_W=0` the apparent horizon was found through step 4,100 and then
not found from step 4,200 through step 6,000. At step 6,000 the archived
horizon-shell diagnostic is `max|H|=3.0323e-1` and `max|M|=6.2315e-2`; an
older note labeled the `3.0323e-1` value "exterior", which is not comparable
to exterior values from other runs. The found area started at `43.74535`
(`+8.79%` relative to exact Kerr) and had already drifted to about `43.35`
before the finder lost the surface. The lapse stayed finite.

**Final-tree baseline (2026-11-04).** The 6,000-step run on the final tree
with the default near-mask width and the outer-`Gtilde` fill is archived in
`run_archive/baseline_gtbc_96x192_6000/`. It exits 0 at `t/M=2.6655` with
exterior `max|H|=4.6565e-2`, `rms|H|=1.8230e-3`, `max|M|=2.7067e-2`, and
`alpha_min=0.28477`. The horizon shell has `max|H|=3.0323e-1` and
`max|M|=6.2315e-2`; the finder rejects the candidate from step 1,900 onward
because it enters the coverage buffer (`r_KS,min=1.52240` at step 6,000
against limit `1.56316`). `baseline/check_baseline_regression.sh` passes on
this log.

**Horizon-loss reconciliation.** `LAPSE_HORIZON_DIAGNOSTIC.md` quotes loss at
step 1,900, not 4,100. The two numbers belong to different finder variants:
the 4,100 result predates the inward-offset finder fix; after that fix the
matched run rejects the candidate when it enters the coverage buffer at step
1,900 (`t=0.844`). The 2026-11-04 baseline archived in
`run_archive/baseline_96x192_6000` also rejects at step 1,900 with the default
near-mask width, and at step 6,000 its best surface has `r_KS,min=1.52240`
against a limit of `1.56316`. The difference is the finder coverage rule, not
the near-mask width. Quote the finder configuration with each number; they are
not contradictory measurements of the same run.

The same temporary run directory was then reused for zero-step initial-data
checks at higher resolutions. Its `output/` directory now contains files from
different runs and is not a self-consistent baseline archive; use the command
above to reproduce the 96×192 run in a fresh output directory.

Zero-step production initializations with the current default
`GR_NEAR_MASK_W=2` and the inward-offset finder mask (`run_archive/horizon_initial_*`):

| Grid | Exterior `max|H|` | Exterior `rms|H|` | Horizon shell `max|H|` | Initial horizon |
|---|---:|---:|---:|---|
| 96×192 | `4.1542e-2` | `2.0286e-3` | `1.9503e-1` | found, `dA/A_Kerr=+9.69e-4` |
| 192×384 | `2.4431e-2` | `1.9746e-3` | `5.4409e-2` | found, `dA/A_Kerr=-1.95e-4` |
| 384×768 | `4.9007e-2` | `2.6455e-3` | `1.4365e-2` | found, `dA/A_Kerr=-9.11e-6` |

The outer-domain maximum is not monotone under refinement, so it should not be
used as the sole convergence measure. Near-horizon shells and fixed-radius
annuli are more informative. The earlier 384×768 finder failure (`ierr=2`) was
caused by the mask reaching beyond the surface; the inward-offset finder mask
recovers the initial horizon at all three resolutions.

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

## 5. Outer-boundary Gtilde consistency (2026-11-04)

The initializer's conformal-connection loop covers only `i=1..Nx-1`,
`k=2..Nz-1`, so it leaves the three outer boundary rows at exactly zero.
On the 192x384 grid the adjacent `Gamx` is `1.978e-2`; the RHS differentiates
across that jump and produced a spurious `Axx` RHS of `-1.0067e-1` on the last
interior column. A one-step run confirmed that this RHS was applied. The
full-RHS band at `r=12..20` carried `max|Axx RHS|=1.06e-1` at `t=0`.

`fill_conformal_connection_boundary()` now linearly extrapolates
`Gtx,Gty,Gtz` from the two nearest interior points onto the outer x and z
rows. It is called once after `compute_conformal_connections()`. After the
fix, `Gamx` on the boundary is `1.956e-2`, the last-column `Axx` RHS is
`-1.77e-5`, and the outer band's `max|Axx RHS|` is `1.68e-5`. The
near-excision band is unchanged (`max|Axx RHS|=4.89e-2`), so this is an
outer-boundary repair, not a cure for the inner mask-adjacent defect.

A matched 96x192 2000-step run with the fill completed at `t/M=0.8885` and
exit 0. Against the archived pre-fix mode-3 run it gives exterior
`max|H|=5.1304e-2` (was `5.2185e-2`), `rms|H|=2.6679e-3` (was `2.7275e-3`),
`max|M|=1.5632e-2` (unchanged), horizon-shell `max|H|=2.2779e-1` (was
`2.3054e-1`), and `alpha_min=0.27740` (unchanged). The fill is retained.

The frozen-state band maximum `max|Axx RHS|` with the fill at `t=0` is:

| band (`r_KS`) | 96x192 | 192x384 | 384x768 | observed order |
|---|---:|---:|---:|---:|
| `1.5–1.6` | `2.69e-2` | `1.47e-2` | `4.61e-3` | ~1–1.7 |
| `1.6–1.8` | `1.23e-1` | `2.85e-2` | `6.64e-3` | ~2.1 |
| `1.8–2.0` | `5.92e-2` | `1.79e-2` | `4.17e-3` | ~1.8–2.1 |
| `8–12` | `3.55e-5` | `1.77e-5` | `8.81e-6` | ~1.0 |
| `12–20` | `3.22e-5` | `1.68e-5` | `8.58e-6` | ~1.0 |

The near-mask bulk residual is resolution-limited and roughly second order;
the outermost band is only first order after the consistency fix. That outer
band is still the target of the P2.3 boundary work.

This is an interim consistency closure, not a characteristic boundary
condition. The outer Sommerfeld layer still has to be replaced or validated
by the domain sweep in Phase 2 before it can be called a boundary treatment.

A 192x384 2000-step run with the fill (`run_archive/phase2_gtbc_192x384_2000`)
also exits 0. At `t/M=0.4431` the horizon is found throughout
(`dA/A_Kerr=-1.88e-3`), with exterior `max|H|=1.0257e-1`, `rms|H|=3.2252e-3`,
`max|M|=1.1794e-2`, horizon-shell `max|H|=7.83e-2`, `max|M|=1.09e-2`, and
`alpha_min=0.26742`. The exterior `max|H|` is larger than the 96x192 value at
comparable time (`6.04e-2` at step 1000). That exterior statistic excludes the
horizon shell but includes both the near-horizon and outermost regions, so this
run alone does not identify which band dominates. Treat it as a
boundary-validation result, not an acceptance result.

## 6. Phase 2 controlled comparisons (2026-11-07)

### 6.1 One-sided mask control

`diagnose_momentum_snapshot` now recomputes the momentum constraint on the
same frozen state with filled `GHOST` points treated as `EXCISED`. This forces
strictly one-sided physical data at the mask edge. It is a diagnostic control,
not a proposed boundary condition. Production and one-sided near-mask maxima:

| state | production `max|M|` | one-sided `max|M|` |
|---|---:|---:|
| 96x192, `t=0` | `7.16e-3` | `1.33e-1` |
| 96x192, `t/M=0.889` | `1.73e-1` | `8.16e-2` |
| 96x192, `t/M=2.66` | `1.29e-1` | `1.76e-1` |
| 192x384, `t=0` | `7.62e-4` | `3.79e-2` |
| 192x384, `t/M=0.443` | `2.55e-2` | `2.15e-2` |
| 192x384, `t/M=2.66` | `2.27e-2` | `4.35e-2` |

The control is better only in the intermediate snapshots and is much worse at
`t=0` and at late time. No consistent improvement follows, so the
ghost-filled centered branch is not the standalone cause and the alternative
is not promoted. The late 192x384 near-mask residual survives both branches.

### 6.2 Outer-domain sweep

The current DBM configuration was run at fixed `dx` to `t/M≈2.66` with 12M,
18M, and 24M outer domains. Full tables and commands are in
`run_archive/2026-11-04_phase0_post_fix/PHASE2_OUTER_SWEEP.md`.

| domain | grid | `max|H|(ext)` | `rms|H|(ext)` | `max|M|(ext)` | `alpha_min` |
|---|---:|---:|---:|---:|---:|
| 12M | 96x192 | `4.6565e-2` | `1.8230e-3` | `2.7067e-2` | `0.28477` |
| 18M | 144x288 | `5.0496e-2` | `1.1086e-3` | `2.7072e-2` | `0.28217` |
| 24M | 191x384 | `4.7296e-2` | `8.2314e-4` | `2.7146e-2` | `0.28455` |

Moving the boundary outward cleans the `r=8–12` band (`2.98e-3` at 12M
versus `1.04e-5` and `9.82e-6` at 18M and 24M) and lowers the exterior RMS,
but leaves the near-horizon maximum and shell metrics essentially unchanged.
The boundary layer is predominantly `x/z`, decreases roughly as
`1/r_boundary`, and is `1e-3` in `|M|` by `t/M≈2.66`. It is not a reflection:
the earliest causal round trip at 12M is `t/M≈20.8`, beyond the run end.

The outer boundary is therefore not the dominant source of the exterior
maximum. Retain the single-layer Sommerfeld closure as an interim condition,
do not call it characteristic, and do not treat a larger domain as a
convergence result. The near-horizon/mask residual remains the primary open
problem.

## 7. Phase 2 high-order and cartoon-y controls (2026-11-07)

`compute_stencil` now accepts diagnostic-only optional flags for a
fourth-order one-sided x/z branch and a fourth-order cartoon-y ghost pair.
Production calls omit both, so the evolution path is unchanged. The snapshot
diagnostic selects them with `GR_DIAG_HIGH_ORDER=1` (x/z), `2` (y), or `3`
(both); `GR_DIAG_ONESIDE=1` combines the control with the strictly one-sided
mask. Logs are in `run_archive/phase2_high_order/`; the interpretation is in
`run_archive/2026-11-04_phase0_post_fix/PHASE2_HIGH_ORDER.md`.

### 7.1 Fourth-order cartoon y

| state | production `max|M|` | high-order y `max|M|` | band `1.5–1.55` | band `1.55–1.6` | band `1.6–1.8` |
|---|---:|---:|---:|---:|---:|
| 96x192, `t=0` | `7.163e-3` | `6.557e-3` | `7.163e-3 → 6.557e-3` | `4.549e-3 → 3.866e-3` | `2.304e-3 → 1.652e-3` |
| 96x192, `t/M=2.66` | `1.293e-1` | `1.266e-1` | `1.293e-1 → 1.266e-1` | `7.044e-2 → 6.961e-2` | `6.232e-2 → 6.298e-2` |
| 192x384, `t=0` | `7.619e-4` | `3.186e-4` | `7.619e-4 → 3.186e-4` | `4.567e-4 → 2.006e-4` | `5.045e-4 → 1.731e-4` |
| 192x384, `t/M=2.66` | `2.269e-2` | `2.295e-2` | `2.269e-2 → 2.295e-2` | `2.052e-2 → 2.076e-2` | `1.781e-2 → 1.802e-2` |

The wider cartoon pair improves the initial-data residual, especially away
from the mask at 192x384, but not the late-time residual: it is about 2%
better at 96x192 late and about 1% worse at 192x384 late. The Hamiltonian
maximum is not improved either (for example the 192x384 `t=0` near band goes
from `1.217e-1` to `1.301e-1`). The cartoon y truncation is therefore a real
piece of the `t=0` defect, not the dominant late-time residual.

### 7.2 Fourth-order one-sided x/z

Under the production ghost-filled mask the x/z control is a no-op: the filled
`GHOST` layer supplies the first inward neighbour, so the genuinely one-sided
branch is never selected at the reported bands and the momentum/H table is
unchanged. The order test therefore has to use the strictly one-sided mask:

| state | one-sided 2nd-order `max|M|` | + high-order x/z | + high-order x/z and y |
|---|---:|---:|---:|
| 96x192, `t=0` | `1.334e-1` | `4.860e-2` | `4.796e-2` |
| 96x192, `t/M=2.66` | `1.761e-1` | `1.830e-1` | `1.805e-1` |
| 192x384, `t=0` | `3.793e-2` | `1.457e-2` | `1.432e-2` |
| 192x384, `t/M=2.66` | `4.353e-2` | `3.155e-2` | `3.185e-2` |

The one-sided order is a large part of the `t=0` discrepancy (2.6–2.7x
improvement), but it does not fix late time consistently and remains worse
than the production ghost-filled branch in every state. No one-sided or
high-order branch is promoted. The next target is the near-mask data/seam and
inner-boundary physics, not a higher-order stencil.

## 8. Near-mask `f(x,z)` cache and axis audit (2026-11-07)

The mode-4 `f(x,z)` cache was audited at 96x192, 192x384, and 384x768. Every
physical cell is valid, there are no missing values and no flat fallbacks. The
worst adjacent `|df|` is `5.6668e-1`, `3.2479e-1`, and `1.7985e-1`
respectively, at points just outside `r_excise=1.5M` with `r_KS≈1.51–1.65`.
The jump decreases roughly first order with `dx`; this is a steep near-mask
profile, not a missing-data hole or a fixed grid seam. Mode 4 remains
unpromoted and is not read by production (`slicing_mode=3`).

The polar-angle `|M|` profile peaks at the equator and falls toward the axis
(96x192 `ct=0..0.1` `2.255e-4` vs `ct=0.9..1.0` `8.314e-5`; 192x384
`5.899e-5` vs `2.166e-5`), and all four production near-mask maxima are
off-axis (`x=0.63–1.64`). There is no axis-specific defect to promote.
Details: `run_archive/2026-11-04_phase0_post_fix/PHASE2_SEAM_AXIS.md`; logs in
`run_archive/phase2_seam/`.
