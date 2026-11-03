# Route C: the data chart degenerates at the horizon for a = 0.8

**Date:** 2026-10-31
**Files:** `helper/eq_kerr_trumpet.F90` (`ktr_kerr_xyz`),
`helper/eq_kerr_bssn_data.F90` (`set_eq_kerr_trumpet_id`),
`helper/excision.F90` (`kerr_radius_squared`, `extrapolate_direction`),
`drivers/vacuum_kerr.F90`

## Summary

Route C (`initial_data_mode = 1`, modes 3/4) is the only self-consistent
(data + gauge) pair in the tree. It is **clean at `t = 0` for `a <= 0.5`** and
fails progressively as `a` rises, catastrophically at the production spin
`a = 0.8`.

The cause is a **coordinate degeneracy in the data module, not the fill**:

- `ktr_kerr_xyz` inverts `r_E = r^2 / sqrt(r^2 - a^2 cos^2 th)`. That map has a
  minimum Euclidean radius `r_E = 2a|cos(th)|`, attained at `r = sqrt(2) a|cos(th)|`.
  Above the minimum there are two branches; the module takes the larger root, so
  its smallest representable `r` is `sqrt(2) a|cos(th)|`, and the trumpet
  `r = R0` is never reached.
- At the minimum the Jacobian `dr_E/dr` vanishes, so the Cartesian metric
  (`Minv(1,1) = 1/a11`) diverges. `ktr_kerr_xyz` only rejects `a11 == 0`
  exactly; cells one step away are accepted with enormous `gamma`.
- For `a = 0.8`, the axis value is `2a = 1.6`, and `r_+ = 1 + sqrt(1 - 0.64) = 1.6`.
  **The horizon sits exactly on the degeneracy.** No mask can separate them.

This supersedes the earlier reading that the projection fill was the root cause.
The fill is a symptom: with a sane chart the same fill gives `max|H| = 0.40`
inside the horizon (see `a = 0.5` below).

## What is stationary

`bin/diagnose_kerr_bssn` (a = 0.8, R0 = 1.0, dx = 0.126) over its fixed
annulus `r in [3,5]`:

| quantity | value |
|---|---|
| `max|H|` | 8.60e-3 |
| `max|M|` | 7.49e-3 |
| `max|d_t Lambda|` | 9.14e-3 |
| `max|d_t K|` | 4.67e-3 |
| `max|d_t Atilde|` | 7.71e-3 |
| `max|d_t alpha|` | 5.65e-4 |
| `max|d_t phi|` | 1.06e-5 |

The bulk is a solution to truncation level, and the gauge pairing is confirmed.
Everything that fails is near the chart degeneracy.

## The decisive test: spin scan at t = 0

Driver `t = 0` diagnostics, 96x192, `r_excise = 1.55`, same data and gauge:

| `a` | chart boundary `2a` | `r_+` | exterior `max|H|` | horizon shell | inside (ungated) |
|---|---|---|---|---|---|
| 0.4 | 0.8 | 1.917 | **2.76e-2** | 6.22e-2 | 2.60e-1 |
| 0.5 | 1.0 | 1.866 | **4.30e-2** | 9.48e-2 | 4.03e-1 |
| 0.6 | 1.2 | 1.800 | 6.41e-2 | 1.36e-1 | 1.59e3 |
| 0.7 | 1.4 | 1.714 | 7.52e-2 | 2.07e0 | 1.06e15 (aborts at step 1) |
| 0.8 | 1.6 | 1.600 | **5.65e0** | 3.95e15 | 1.05e36 |

For reference, Kerr-Schild data at `t = 0` has exterior `max|H| = 1.12e-2`.
`a <= 0.5` is bounded at `O(5e-2)` and resolution-stable: at `a = 0.5` the
exterior value is `4.30e-2` at 96x192 and `6.06e-2` at 192x384, i.e. the same
order rather than a growing sequence. It is an order of magnitude above the KS
truncation residual, but it does not diverge.

The trend is exactly what the boundary argument predicts: as `2a` approaches
`r_+`, the degeneracy moves out to the horizon and the data becomes unusable.
`a = 0.8` is the worst case in the whole family because `2a = r_+` there.

## Fixed-radius profiles (why it looks like a horizon singularity)

`diagnose_kerr_bssn` temporarily re-binned to `r in [1.6, 4]`, at fixed physical
radii, `r_excise = 0.8`:

`R0 = 1.0`:

| r-bin | 96x192 `max|H|` | 192x384 `max|H|` |
|---|---|---|
| 1.60–1.94 | 8.588e+04 | 1.230e+09 |
| 1.94–2.29 | 8.978e-01 | 1.099e-01 |
| 2.29–2.63 | 4.866e-02 | 4.599e-02 |
| 2.97–3.31 | 8.869e-03 | 8.810e-03 |
| 3.66–4.00 | 2.386e-03 | 2.093e-03 |

`R0 = 0.8`:

| r-bin | 96x192 `max|H|` | 192x384 `max|H|` |
|---|---|---|
| 1.60–1.94 | 1.402e+03 | 1.057e+08 |
| 1.94–2.29 | 5.809e-01 | 1.109e-01 |
| 2.29–2.63 | 4.463e-02 | 4.313e-02 |

Everything at `r >= 1.94` converges; the innermost bin diverges by four orders
per factor of two in resolution. That bin is the one that crosses the chart
boundary `r_E = 2a|cos th|`, where `dr_E/dr -> 0`. It is not truncation error.

Lowering `R0` from 1.0 to 0.8 reduces the value (the grid's smallest `r` is
`sqrt(2)a = 1.131`, so a smaller `R0` means a larger `r - R0` and a milder
`g_RR = rho^2/(r-R0)^2`), but it cannot remove the degeneracy, which depends on
`a` and `theta` only. `R0 = 0.6` is below `a` and fails with
`ERROR STOP Excision: unable to preserve positive ghost metric`.

## Why `r_excise` cannot fix it

Measured `t = 0` at `a = 0.8`, original projection threshold:

| `r_excise` | exterior `max|H|` | horizon-shell `max|H|` | shell-worst r |
|---|---|---|---|
| 0.8 | 5.6453e0 | 3.9521e15 | 1.6349 |
| 1.3 | 5.6453e0 | 3.9521e15 | 1.6349 |
| 1.45 | 5.6453e0 | 3.9521e15 | 1.6349 |
| 1.55 | 5.6453e0 | 3.9521e15 | 1.6349 |

Identical to five digits. The degeneracy is at `r_KS = 1.6` (axis), which is
outside every admissible mask (`r_excise < r_+ = 1.6`). The excision fill is
perturbation-based and reproduces the frozen background at `t = 0`, so moving
the mask only changes *which* cells carry the same degenerate values.

## Fill experiments (second-order, and not the root cause)

| change | inside-horizon `max|H|` | horizon shell | verdict |
|---|---|---|---|
| baseline (`alpha > 1e-6`) | 1.05e36 | 3.95e15 | reference |
| flat interior | 0 | 8.79e4 | C0 seam, still O(1e5) |
| projection at `alpha > 0.1` | 4.85e8 (axis) | 8.59e4 | 4.6e4x better, still bad |
| direct extrapolation of `u` | 2e29 | non-positive det | unstable (`15^12`) |

These reduce the *fill* contribution but leave the exterior untouched, because
the exterior value at `r_KS ~ 1.85` is produced by the degenerate closed form,
not by the fill. The `alpha > 0.1` change is still a genuine improvement to the
fill and is worth keeping if the chart is fixed; it does not unblock route C on
its own.

## What to do next

Two independent paths, and they answer different questions:

1. **Prove the machinery at `a = 0.5`.** The `t = 0` state there is bounded and
   resolution-stable (`4.3e-2`/`6.1e-2` exterior at 96x192/192x384), the chart
   boundary is at `r_E = 1.0`, well inside `r_+ = 1.866`, and the mask covers it.
   A long run at `a = 0.5` tests whether modes 3/4 plus the tabulated `f(alpha)`
   are truly stationary *without* the `a = 0.8` chart defect. That is the
   cleanest route to a `T/M >= 100` demonstration in this tree.
2. **Fix the chart for `a = 0.8`.** Either invert the same relation but take the
   *inner* branch (which reaches `R0` and has `alpha = 0` there), or evaluate the
   DBM metric using the standard Kerr-Schild radius
   (`kerr_radius_squared`) so the data and the excision/horizon logic share one
   coordinate.

   The second is the physically consistent choice. Evaluated with the standard
   KS radius at `a = 0.8` the lapse is already correct:

   ```
   r = 1.0 (trumpet)  alpha = 0.0000
   r = 1.4            alpha = 0.2481
   r = 1.6 (horizon)  alpha = 0.3354
   r = 2.0            alpha = 0.4642
   ```

   vanishing exactly at the trumpet, positive and small at the horizon, and the
   trumpet is safely inside `r_+`. That is the behaviour the current chart only
   produces on the discarded inner branch. The change touches `ktr_kerr_xyz`
   (radius and Jacobian `dr_E/dR`, `dr_E/dtheta`) and needs its own matched A/B
   plus a re-run of the `t = 0` spin table.

Do not tune `r_excise` or the projection further until the chart is fixed.

## Equilibrium-ring gate result

The production RHS was subsequently evaluated after the actual route-C state
construction, excision background setup, ghost fill, and finite-difference
stencils.  The annulus test at `r in [3,5]` is not sufficient:

| spin | `r_excise` | region | max RHS |
|---:|---:|---|---:|
| 0.5 | 0.8 | excision to horizon | `5.81e7` |
| 0.8 | 0.8 | excision to horizon | `1.59e5` |
| 0.8 | 1.3 | excision to horizon | `2.81e2` |
| 0.8 | 1.3 | horizon shell | `2.81` |

The first two failures are explained by the mask lying inside the DBM trumpet
surface (`R0 = M = 1`): points between the mask and `R0` were treated as
evolved even though the chart does not exist there.  The code now rejects this
configuration when `initial_data_mode = 1`; route-C requires
`r_excise > bh_mass`.

Moving the mask outside the trumpet removes the catastrophic `10^5`--`10^8`
residual.  With the complete production initialization, including
`compute_conformal_connections`, the remaining near-mask residual converges
under refinement when the mask is kept sufficiently close to (but inside) the
horizon:

| spin | `r_excise` | grid | near max RHS | near RMS RHS |
|---:|---:|---:|---:|---:|
| 0.8 | 1.45 | 96x192 | `1.42` | `2.67e-1` |
| 0.8 | 1.45 | 192x384 | `7.19e-2` | `3.36e-2` |
| 0.8 | 1.50 | 96x192 | `1.96e-1` | `7.75e-2` |
| 0.8 | 1.50 | 192x384 | `4.89e-2` | `2.30e-2` |

The horizon-shell and outer-bulk maxima at 192x384 are `2.03e-2` and
`1.31e-2`, respectively, for `r_excise=1.50`.  This is evidence that the
route-C state can provide a small, convergent production RHS, but only with a
mask placement that leaves a narrow sub-horizon buffer and adequate
resolution.  The invalid `r_excise <= R0` configuration remains rejected.

The next boundary step can therefore use route-C data as the equilibrium
target, but characteristic penalties must first be tested at `a=0.8`,
`r_excise=1.50`, and 192x384 (or a matched-resolution manufactured test).

## Reduced SAT status

The existing inner penalty is now explicitly marked non-exact.  It only
contains reduced lapse, gravity, and shift 3x3 blocks; it is not the complete
57-variable characteristic transform of the production second-order BSSN
system.  `test-excision-sat` verifies the reduced behavior and verifies that a
zero-speed lapse offset is not incorrectly damped.

`make diagnose-route-c-sat` applies this reduced penalty to the route-C
equilibrium state without advancing it.  At `a=0.8`, `r_excise=1.5`, `R0=1`,
the equilibrium target produces:

```text
exact characteristic map advertised: F
SAT contribution max/RMS:             0.0 / 0.0
combined RHS max/RMS:                 1.96e-1 / 7.70e-3
```

This is the expected no-forcing result when the target equals the background;
it does not validate stability for perturbations.  The reduced SAT remains
disabled in production until a manufactured incoming-mode test and a complete
production characteristic map are available.

The manufactured mode checks are now part of `test-excision-sat`.  They cover
the lapse, gravity, and shift reduced blocks, verify that each positive-speed
mode is damped toward its target, and verify that negative- and zero-speed
modes are untouched.  The generic characteristic helper was aligned with the
same convention: the excision normal points toward increasing radius, so
positive speed is the incoming-to-the-evolved-domain family.

These tests validate the reduced algebra and sign convention only.  The
apparent route-C versus Kerr--Schild normal mismatch is not present: the
route-C map and the excision mask both use the same standard Kerr radius
defined by the oblate-spheroidal Cartesian inversion.  This removes the need
for a separate route-C normal helper, but does not establish stability of the
production second-order discretization.

## Short production-path comparison

The actual RK4 driver was run at 96x192, `a=0.8`, `r_excise=1.5`,
`GR_INITIAL_DATA_MODE=1`, and 1000 steps (`t=0.4443`) with identical settings
except for `GR_EXCISION_SAT`.  Both runs completed without determinant or
lapse-floor failure:

| setting | exterior max/RMS H | horizon-shell max H | horizon-shell max M | inside-horizon max H |
|---|---:|---:|---:|---:|
| SAT disabled | 4.3897e-2 / 2.0801e-3 | 2.0264e-1 | 4.1213e-2 | 5.4715e-1 |
| reduced SAT enabled | 4.1425e-2 / 2.0717e-3 | 1.8866e-1 | 4.1142e-2 | 6.0995e-1 |

The reduced penalty therefore slightly improves the exterior and
horizon-shell Hamiltonian residual in this short run, leaves the shell
momentum residual essentially unchanged, and worsens the ungated
inside-horizon Hamiltonian residual.  This is evidence that the SAT is
non-catastrophic in this route-C configuration, not evidence that it is
stable or constraint-preserving over long times.  Production remains
`excision_sat=false`.

The matched run was extended to 2000 steps (`t=0.8885`) with diagnostics
every 500 steps:

| setting | exterior max/RMS H | horizon-shell max H | horizon-shell max M | inside-horizon max H |
|---|---:|---:|---:|---:|
| SAT disabled | 5.2185e-2 / 2.7275e-3 | 2.3054e-1 | 6.6512e-2 | 5.3989e-1 |
| reduced SAT enabled | 5.2185e-2 / 2.7422e-3 | 1.5877e-1 | 7.6240e-2 | 1.1781 |

The longer result rejects the reduced SAT as a production repair.  It lowers
the horizon-shell Hamiltonian residual by about 31%, but the interior
Hamiltonian residual more than doubles and the shell momentum residual is
higher.  The state remains finite and the conformal determinant does not
fail in this interval, so this is not an immediate crash mechanism; it is a
clear error-redistribution mechanism.  A penalty that damps only the reduced
incoming blocks cannot control the omitted characteristic and constraint
sectors.

## Production gauge/data consistency

The original production parameter file selected Kerr-Schild initial data
(`initial_data_mode=0`) while also selecting the route-C tabulated Bona--Masso
slicing and non-advective Gamma driver (`slicing_mode=3`,
`gamma_driver_mode=4`). Those gauge modes are the stationary compatibility
pair for the DBM route-C trumpet data, not for Kerr-Schild data.

A matched 96x192, 1000-step comparison at `r_excise=0.8` showed the effect:

| Kerr-Schild data and gauge | exterior max/RMS H | horizon-shell max H | inside-horizon max H |
|---|---:|---:|---:|
| compatible frozen gauge (modes 2/2) | 3.5498e-2 / 2.7469e-3 | 1.8216e-2 | 4.6065e-1 |
| route-C gauge (modes 3/4) | 3.9106e-2 / 2.6221e-3 | 1.5184e-2 | 6.4227 |

Both runs reached `t=0.4443` without a hard crash, and their exterior
residuals are similar. The order-of-magnitude interior difference is why the
mismatch can be missed by the usual exterior diagnostics.

The production parameter file now selects a self-consistent route-C baseline:
`initial_data_mode=1`, `r_excise=1.5`, `slicing_mode=3`, and
`gamma_driver_mode=4`. The Kerr-Schild configuration remains available as an
explicit control by overriding the initial-data and gauge modes together.

## Outer-boundary isolation

The corrected route-C baseline was compared with an enlarged outer domain while
holding the grid spacing and inner setup approximately fixed:

| domain/grid | t | exterior max/RMS H | horizon-shell max H | horizon-shell max M | inside-horizon max H |
|---|---:|---:|---:|---:|---:|
| 12M, 96x192 | 0.3110 | 4.0824e-2 / 1.6778e-3 | 2.0045e-1 | 3.0430e-2 | 5.5987e-1 |
| 18M, 144x288 | 0.3104 | 4.2352e-2 / 9.5859e-4 | 2.0799e-1 | 3.1779e-2 | 5.9065e-1 |

Moving the outer boundary from 12M to 18M approximately halves the exterior
RMS Hamiltonian residual, but barely changes the horizon-shell or
inside-horizon residuals at this time. The outer Sommerfeld boundary is
therefore a real far-field error source, but it is not the dominant early
failure mechanism. The remaining secular growth is localized at the inner
boundary and in the uncontrolled constraint sectors, so further outer-domain
or sponge tuning is not the highest-value repair.

## Excision placement test

The route-C mask position was then varied at 96x192 for 700 steps
(`t=0.3110`), with the matching gauge and SAT disabled:

| `r_excise` | horizon-shell max H | horizon-shell max M | inside-horizon max H | inside-horizon max M |
|---:|---:|---:|---:|---:|
| 1.50 | 2.0045e-1 | 3.0430e-2 | 5.5987e-1 | 8.2377e-2 |
| 1.55 | 1.9123e-1 | 2.8566e-2 | 4.3490e-1 | 4.2823e-2 |
| 1.58 | 1.8690e-1 | 2.5841e-2 | 1.5855e-1 | 2.2872e-2 |
| 1.59 | 1.8690e-1 | 2.5841e-2 | 1.5855e-1 | 2.2872e-2 |

This is the strongest structural signal so far. Moving the mask closer to the
outer horizon sharply reduces the constraint residual in the thin
inside-horizon band and modestly improves the horizon shell. The `1.58` and
`1.59` values have only 8 and 6 sampled interior points respectively, so their
inside-band maxima are not a standalone acceptance criterion; nevertheless,
the monotonic trend from `1.50` to `1.58` is consistent with excluding the
worst projected/trumpet-buffer cells rather than merely increasing accuracy.

Do not change the production mask to `1.58` solely from this short sweep.
First perform a matched-time run at a refined grid and verify that the mask
still leaves a valid stencil layer and that horizon invariants remain stable.

### Refined placement check

The requested matched-time check was run at 192x384 for 300 steps
(`t=0.0665`) with horizon monitoring:

| `r_excise` | horizon-shell max H | horizon-shell max M | inside-horizon max H | inside-horizon max M |
|---:|---:|---:|---:|---:|
| 1.50 | 5.6315e-2 | 2.2017e-3 | 1.2417e-1 | 2.7425e-3 |
| 1.58 | 5.4492e-2 | 2.2026e-3 | 6.1343e-2 | 2.4213e-3 |

The tighter mask again reduces the inside-band Hamiltonian residual by about
50%, while leaving the horizon shell nearly unchanged. However, it is not a
safe production placement: the horizon finder reports
`rejected surface inside excision buffer` at both step 0 and step 300
(`r_min` about 1.3266, buffer limit about 1.6114). The `r_excise=1.50` run
also reports a horizon-finder rejection at this resolution, but its initial
surface is only marginally rejected (`r_min` about 1.4809); neither run
provides a valid horizon-invariant measurement at this grid.

This rules out promoting `r_excise=1.58`. The lower residual is real but is
obtained by removing most of the sub-horizon diagnostic band and leaving the
horizon finder without a valid stencil margin. Keep `r_excise=1.50` as the
conservative route-C baseline until the horizon finder and excision geometry
are redesigned together.

## Status

The route-C chart is now usable at `a=0.8` when the excision surface is placed
outside the trumpet radius (`r_excise=1.5` for `R0=1`) and the matching gauge
pair is used. The remaining long-time issue is the non-characteristic outer
boundary plus the unresolved full inner characteristic/constraint treatment;
the reduced SAT is rejected for production.
### Horizon/excision geometry contract

The excision mask is defined by the Kerr radius `r_KS`, not by the Euclidean
surface radius used by the Legendre representation.  The horizon finder now
computes `rmin_ks` by sampling the fitted surface and the driver compares that
quantity with `r_excise` when deciding whether a converged candidate enters the
excision buffer.  Comparing `hmin` directly with `r_excise` was incorrect for
spinning holes and could reject a geometrically valid surface (or accept an
invalid one) because the two radii are different coordinates.

Candidate selection also applies this contract before ranking roots.  A
converged candidate must clear `r_excise + 0.5 max(dx,dz)` in `r_KS`; valid
candidates are ranked by largest `rmin_ks`, with expansion residual as the
tie-breaker.  If no marginal candidate has valid coverage, the finder returns
`NOT FOUND` rather than returning an inner ghost-contaminated root.  The
initial-state smoke test now reports a best rejected coverage of about `1.68M`
while the prior selected root was about `1.35M`, showing that the remaining
problem is the marginal sign/expansion solve for the outer candidate, not
candidate ranking or coordinate comparison.

The marginality test was subsequently made multi-scale.  It now probes
inward/outward rescalings of 2%, 5%, 10%, 20%, and 30%, accepting the first
resolved sign change with negative expansion inside and positive expansion
outside.  This avoids treating a single displacement as authoritative when it
is either smaller than the finite-difference/interpolation footprint or large
enough to cross another expansion root.  The route-C initial state now finds
the outer candidate with `r_KS,min=1.67842`; the 100-step smoke test retains
it with only small invariant drift.

If a later step reports a marginal probe with `Theta(in)<0` and
`Theta(out)>0` but `r_KS,min` below the coverage limit, that is an explicitly
diagnosed marginal surface inside the excision mask, not a failed sign test.
Such a root remains rejected; accepting it would make the horizon invariants
depend on ghost-filled data and conceal an actual inner-boundary failure.

### Long-time failure diagnosis

The step-4200 loss is not a marginality or candidate-ranking failure.  The
surviving root has the correct trapped-surface sign pattern
(`Theta(in)<0`, `Theta(out)>0`) but is an inner root at approximately
`r_KS,min=1.35`, inside the `r_excise=1.5` mask.  The coverage-valid outer
surface remains present through approximately step 4000, then disappears.

Moving the mask outward to `r_excise=1.58` does not remove the failure: the
outer surface remains smooth through step 4000 and is again lost at step 4200.
It reduces the available contaminated interior band and delays neither the
underlying transition nor the constraint source sufficiently to be a fix.

The production decomposition at the worst horizon-shell point identifies the
dominant momentum residual as terms 1 and 2:

1. `e^-4phi d_j Atilde^(ij)`;
2. `-(2/3) e^-4phi K d_j gammatilde^(ij)`.

At `t=0.8885`, representative values were approximately
`(-1.65e-1, +8.16e-2)` for these terms, while the gauge-only warning reported
82 incoming lapse-gauge points with maximum speed about `2.24e-1`.  Thus the
incoming gauge family is a warning, but the measured long-time constraint
source is primarily the uncontrolled geometric/constraint sector at the
excision interface.  The reduced SAT covers only three incomplete blocks and
previously redistributed error into the interior; it is not a valid repair.

The true fix is a production characteristic/constraint-preserving inner
boundary treatment for the complete second-order BSSN principal system,
including the `Atilde`/`gammatilde` constraint sector and compatible gauge
conditions.  Until that principal-symbol map and manufactured incoming-mode
tests exist, changing the mask radius, accepting the inner horizon root, or
enabling the reduced SAT would only hide or redistribute the failure.

The next diagnostic stage now reports the published first-order mode inventory
at the worst shell point.  The initial route-C run has 15 positive-speed gauge
modes and 30 positive-speed non-gauge modes entering the evolved exterior.
This is a diagnostic based on the published mode speeds, not an assertion that
the current production RHS has a valid 57-field characteristic map.  It
quantifies why the existing three reduced SAT blocks cannot be promoted: most
incoming sectors, including the constraint-carrying geometric sector, remain
uncontrolled.

### Derived principal-symbol couplings

The normal-symbol scaffold now includes only couplings that can be read
directly from `rhs_point()`:

- `phi`: normal transport plus the `div(beta)/6` term;
- `gammatilde_ij`: normal transport, both metric--shift Lie terms, and the
  `-(2/3) gammatilde_ij div(beta)` term;
- `K`: the existing lapse-gradient coupling;
- `Gammatilde^i`: transport, the `-2 Atilde^{ij} d_j alpha` term, the
  `-(4/3) alpha gammatilde^{ij} d_j K` term, the
  `12 alpha Atilde^{ij} d_j phi` term, and the principal shift-driver
  derivative terms;
- the differentiated lapse--`K` pair already present in the complete
  first-order matrix.

The conformal-Ricci second-derivative block in `Atilde_t` and the
Gamma-driver auxiliary second-derivative block remain explicitly labelled
`unsupported-ricci` and `unsupported-driver`.  This is intentional: the
production RHS computes these terms through the second-order geometry helper,
but the differentiated first-order equations needed to close their normal
symbol have not been implemented.  No production SAT uses this partial
matrix.

Manufactured tests now verify the derived transport/Lie couplings and retain
the existing incoming/outgoing reduced-block tests.  They also assert that
the unresolved `Atilde` and driver sectors remain unsupported, preventing a
future boundary penalty from silently treating this partial matrix as a full
characteristic decomposition.

The first production-RHS gate is automated as
`make -j1 test-fo-symbol-consistency`.  It linearizes `compute_rhs_point()` on
a flat background with nonzero constant shift and verifies scalar transport
in both coordinate-normal directions.  The measured responses agree with the
normal matrix at `0.20` in x and `0.13` in z.  It now repeats the check at
three perturbation amplitudes (`1e-5`, `1e-7`, `1e-9`) and two wavelengths.
The same executable passes at `GR_NX=32,GR_NZ=64` and
`GR_NX=96,GR_NZ=192`.  This gate intentionally does not promote the
unresolved metric/Ricci, connection, driver, or constraint sectors; production
SAT remains disabled.

An initial manufactured conformal-Ricci wave exposed a mismatch between the
continuum trace-free Ricci estimate and the production Cartoon/stencil path.
The discrepancy persists after forcing the perturbation value to zero at the
sample point, so it is not merely a zero-order coefficient effect.  The
likely remaining source is the Cartoon tensor rotation contribution to the
transverse second derivatives.  That trial remains diagnostic only and was
removed from the pass/fail gate rather than weakening the tolerance or
marking the sector supported.  The Ricci block therefore remains unresolved.

The production stencil probe is available as
`make -j1 test-stencil-derivs`.  Its tensor decomposition confirms that the
Cartoon reconstruction is accurate, while the current `d2y` operator is
second-order.  For x>0 on the symmetry plane, the rotation identities needed
by the Ricci derivation are:

```text
d2y S   = d_x S / x
d2y Vx  = d_x Vx / x - Vx / x^2
d2y Vz  = d_x Vz / x
d2y Txx = d_x Txx / x + 2 (Tyy - Txx) / x^2
d2y Tyy = d_x Tyy / x + 2 (Txx - Tyy) / x^2
d_y Txy = (Txx - Tyy) / x
d_y Tyz = Txz / x
```

These are geometric rotation couplings, not independent normal derivatives.
They must be included as lower-order Cartoon-plane terms when validating a
production Ricci response; the unresolved characteristic principal block is
still not enabled.

A trial implementation of the continuum Ricci principal rows was rejected:
the production `Atilde` response did not match even after subtracting a
baseline with the same local metric perturbation.  This confirms that the
remaining test must separate the full Cartoon-plane operator, including
rotation-induced lower-order terms and the production connection-function
reduction, before any Ricci rows are marked supported.

The symbol test now includes a production-geometry decomposition.  It
perturbs the conformal metric, evaluates `compute_conformal_geometry()` on
the same production stencil, and verifies that the measured `Atilde_xx` RHS
change matches the exact trace-free `alpha Rtilde_ij` contribution:

```text
production geometry measured/expected = -2.605e-9 / -2.605e-9
```

This isolates the geometry helper and confirms that the remaining mismatch is
specifically in translating the full production operator into a normal
principal symbol, rather than in the Ricci calculation itself.  The Ricci
characteristic rows remain unsupported until that translation is completed.

The decomposition was subsequently expanded to all six conformal-metric
components and all three conformal-connection components, using sinusoidal
perturbations in both the x and z directions.  Every measured `Atilde_xx`
response agrees with the trace-free production-geometry response to the
finite-difference roundoff level.  Representative connection responses are:

```text
direction x: Gamma-x  -1.947e-8 / -1.947e-8
direction z: Gamma-z  -1.478e-8 / -1.478e-8
```

The zero responses for the parity-incompatible transverse connection
components are also reproduced.  Thus no individual metric or
connection-function input currently exposes an inconsistency between
`compute_rhs_point()` and `compute_conformal_geometry()`.  The unresolved
characteristic problem is the coupled first-order reduction: it must evolve
the metric, connection, and Ricci/second-derivative variables together,
rather than adding an isolated Ricci row to the partial normal matrix.

An independent connection-reduction gate now checks the analytic
trace-free `Gamma` derivative contribution to `Atilde_xx`.  It includes the
Cartoon vector identity `d_y Gamma^y = Gamma^x/x` for an x-directed vector
perturbation.  The measured and predicted responses agree:

```text
Gamma-x, x direction: -1.947e-8 / -1.947e-8
Gamma-z, z direction: -1.478e-8 / -1.478e-8
```

This confirms that the connection-function reduction is not a missing
standalone coefficient; its correct normal response depends on the
symmetry-plane rotation terms.  The remaining work is to formulate the
coupled metric/connection/constraint characteristic block with those terms
present.

The first manufactured momentum-constraint gate has also been added.  On a
flat conformal background with isolated sinusoidal `Atilde` perturbations it
checks the principal divergence responses:

```text
Axx/x -> Mx:  5.7264e-7 / 5.0144e-7
Azz/z -> Mz:  7.0712e-7 / 7.0715e-7
```

An unrelated component test, `Ayy/x -> Mx`, produces only
`-7.12e-8`, quantifying the finite-difference/Cartoon leakage floor for this
setup.  The manufactured test passes with a `2e-7` tolerance.  This validates
the diagnostic momentum-divergence operator, but does not yet provide the
constraint characteristic eigenfields needed by an excision SAT.

The gate now covers off-diagonal channels as well:

```text
Axy/x -> My:  6.4386e-7 / 5.0144e-7
Axz/z -> Mx:  7.0712e-7 / 7.0715e-7
```

Together with the diagonal tests, this verifies the normal divergence
response across the independent symmetric `Atilde` components.  The
constraint-sector SAT remains deferred because the current first-order state
does not yet carry independent constraint fields; adding a penalty directly
to these diagnostic residuals would not define a closed characteristic
subsystem.

The complete first-order scaffold confirms this limitation.  Its 96 fields
are the 24 evolved BSSN/gauge fields plus x/y/z reduction slots only; no
momentum-constraint or Gamma-constraint variables are evolved independently.
The complete-state reduction test passes with zero reduction norm.  The
complete RHS now differentiates the Cartoon y direction explicitly, but the
constraint fields remain diagnostic combinations rather than independently
evolved state variables.

The first part of that prerequisite is now implemented.  The complete-state
RHS differentiates the legacy RHS in x and z as before, and differentiates it
in y using `get_neighbor()` from the Cartoon module.  This applies the
vector/tensor rotations to the RHS before taking the centered y difference,
rather than treating every derivative slot as a scalar.  The complete RHS
test now verifies that the y slot equals the independently reconstructed
Cartoon difference and reports `FO_STATUS_OK`; the intentional reduction-slot
defect check remains active.  The complete-state reduction test still passes
with zero norms.

Explicit diagnostic constraint combinations are now implemented in
`fo_constraint_fields.F90`:

* `fo_gamma_constraint` computes
  `C_Gamma^i = Gamma-tilde^i + partial_j gamma-tilde^{ij}`, including the
  inverse-metric derivative from the reduction slots.
* `fo_momentum_constraint_principal` computes the principal derivative
  combination of `d_j Atilde^{ij}`, `d_j K`, and `d_j phi`, including metric
  raising/lowering derivatives.

The manufactured cancellation test passes for both combinations.  These are
diagnostic fields, not yet characteristic fields: the current RHS still does
not provide a closed evolution equation for `C_Gamma` and `C_M`.  In
particular, assigning them the published BSSN speeds without deriving their
coupled evolution would be unsupported.  Therefore no SAT has been added or
enabled; the next gated step is to differentiate the production Gamma and
momentum equations and measure the resulting constraint-to-constraint
principal matrix.

That matrix gate is now implemented in
`test_fo_constraint_principal.F90`.  It differentiates the complete
production RHS and applies the Frechet derivative of the explicit constraint
operator, subtracting the stationary Cartoon background RHS.  For a flat
background with shift `beta^n`, the measured matrix is:

```text
partial_t C_Gamma^i = beta^n partial_n C_Gamma^i + 2 alpha C_M^i
partial_t C_M^i     = beta^n partial_n C_M^i
```

The x- and z-normal manufactured tests pass for all three components,
including off-diagonal `Atilde` channels.  The `2 alpha C_M` coupling is the
expected contribution from
`partial_t gamma-tilde_ij = -2 alpha Atilde_ij`; it is not unexplained
cross-sector leakage.

The principal characteristic basis for this measured block is the identity
basis
`(C_Gamma^x,C_Gamma^y,C_Gamma^z,C_M^x,C_M^y,C_M^z)`, with all six normal
speeds `-beta^n` under the `partial_t + A^n partial_n` convention.  The
implementation reports `complete=.false.` because this is only the
flat/principal constraint block; curvature, gauge, and full Cartoon
lower-order couplings have not been closed.

Consequently no SAT is added yet.  An opt-in penalty based on this block alone
would omit the unresolved geometric and gauge incoming modes and repeat the
earlier reduced-SAT failure mode.

### Fix determination

The validated tests now distinguish the fix from an attractive but
insufficient workaround.  The constraint block is closed at principal order,
but it is not the full incoming subsystem at the excision surface.  The
long-time failure therefore cannot be fixed by damping `C_Gamma` and `C_M`
alone: the measured `2 alpha C_M` coupling would merely transfer the error
between diagnostic combinations while the incoming metric/Ricci and gauge
modes remain uncontrolled.

The production fix must be a coupled boundary operator with these properties:

1. Construct the normal principal matrix for the complete metric,
   `Atilde`, connection, lapse, shift-driver, and constraint variables on the
   local Kerr background.
2. Include the Cartoon rotation terms in the normal and transverse rows.
3. Use the full left/right characteristic transform, not the current
   reduced 3-by-3 blocks.
4. Penalize only modes whose characteristic speed points into the evolved
   exterior, using a target supplied by the stationary route-C trumpet data.
5. Validate zero penalty on outgoing manufactured modes and damping of every
   incoming mode on a curved Kerr-radius interface before enabling it.

Until that operator is implemented, the safe production configuration remains
`excision_sat=false`.  Enabling the existing legacy or reduced SAT would not
be the fix; earlier runs already demonstrated that it redistributes the
Hamiltonian error into momentum and interior shells.

## Full constraint combination status

The first-order diagnostics now expose both levels of the momentum constraint:

- `fo_momentum_constraint_principal` contains the differentiated
  `Atilde`, metric, `K`, and `phi` terms used for principal-symbol tests.
- `fo_momentum_constraint_full` adds the physical-Christoffel contractions
  present in `helper/constraints.F90`.

The full combination is validated on a flat manufactured state and is
available for the curved-background Jacobian work.  It is diagnostic only;
it does not alter the production RHS or enable a boundary penalty.  The
constraint characteristic transform remains explicitly incomplete until the
full metric/Ricci/gauge block has been differentiated and validated.
