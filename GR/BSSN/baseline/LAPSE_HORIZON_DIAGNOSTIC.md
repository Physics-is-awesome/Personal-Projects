# Route-C lapse residual and high-resolution horizon search

**Configuration:** `a=0.8`, `R0=1`, DBM route-C data, annulus `3 <= r <= 5`,
384x768 grid.

## Lapse residual

`diagnose-kerr-bssn` now evaluates the exact DBM lapse, shift and trace `K` at
the grid sample, then measures

```text
R_alpha = beta^i d_i(alpha) - alpha^2 f_table(alpha) K.
```

It uses the same fixed-step analytic derivatives as the data constructor. The
results are:

| Grid | max `|R_alpha|` | RMS `R_alpha` | max `|f_table-f_required|` |
|---|---:|---:|---:|
| 96x192 | `4.3075e-4` | `1.0095e-4` | `1.7110e-2` |
| 192x384 | `4.3564e-4` | `1.0320e-4` | `1.7436e-2` |
| 384x768 | `4.3399e-4` | `1.0304e-4` | `1.7385e-2` |

The production finite-difference RHS maximum follows the same plateau, while
the Hamiltonian, momentum, `K`, `Atilde`, and `phi` residuals decrease under
grid refinement. This isolates the plateau to the tabulated gauge relation
rather than the production spatial stencil.

The first pointwise comparison included both interpolation error in the
equatorial table and off-equatorial spread. A cubic interpolation trial
slightly increased the measured maximum (`4.60e-4`) and was discarded; the
production table remains unchanged.

The follow-on probe now performs that split directly: for 117 samples across
`R=2..6` and `|cos(theta)|=0..1`, it inverts the equatorial lapse to evaluate
the exact equatorial DBM relation at each sample's `alpha`. The largest table
versus dense-equatorial difference is `8.98e-2` (RMS `2.03e-2`); the largest
off-equatorial difference from that same relation is `1.57e-1` (RMS
`2.74e-2`). At `R=3` on the equator, `f_required=0.66776`, the production table
returns `0.65846`, and the dense reference returns `0.66776`. The denser
reference therefore identifies meaningful interpolation error, while the
larger angular spread means a denser one-dimensional table alone will not
eliminate the full mismatch. The probe is available as
`make diagnose-f-alpha-relation`.

## Horizon search

At `r_excise=1.5`, the 384x768 finder converges to a marginal candidate but
rejects it: best candidate `r_KS,min=1.30`, versus the required clearance
`r_excise + 0.5 dx = 1.5157`. The failure was therefore caused by the mask and
its interpolation buffer reaching past the marginal surface, not by Newton
non-convergence. The centered Kerr surface fit now uses even Legendre modes
while still loading both grid hemispheres.

The final fix keeps the evolution excision at `1.5` and offsets the separate
horizon mask inward by two grid spacings. Since the horizon module itself
extends that mask outward by 1.5 cells, the effective mask ends half a cell
inside the evolution excision instead of covering the physical horizon. With
this adjustment and the even-mode surface fit, initial horizons are found at
96x192 (`dA/A_Kerr=9.69e-4`), 192x384 (`-1.95e-4`), and 384x768
(`-9.11e-6`). The production parameter files retain `r_excise=1.5`.

Short probes at smaller evolution excision radii were not viable: at `1.2` the
96x192 run failed the conformal determinant at step 469; at `1.3` it failed at
step 709 after large constraint growth. The original `1.5` evolution boundary
is therefore retained.

The matched 6,000-step 96x192 run with the final finder fix completes to
`t=2.6655` without determinant failure. The horizon moves inside the coverage
buffer by step 1,900 (`t=0.844`), so the finder correctly rejects it thereafter;
at step 6,000 the best surface has `r_KS,min=1.5224` against a limit of
`1.5632`. Exterior `max|H|=3.0323e-1`, `rms|H|=8.6009e-3`, and
`max|M|=6.2315e-2`, essentially unchanged from the prior 96x192 run. The fix
resolves initial high-resolution recovery, not the longer-term horizon drift.

The first full 192x384 attempt exposed a separate latent bounds bug: when
`GR_NEAR_MASK_W=0`, an empty horizon shell left its representative indices
uninitialized, and the later diagnostic sample indexed the state array with
those stale values. Initializing the horizon and exterior shell indices to
`-1` makes the empty-shell path explicit. A full-HDF5 probe then passed the
previous step-1000 failure point with a valid horizon (`dA/A_Kerr=-0.112%` at
step 1000), confirming the earlier apparent HDF5 crash was downstream of
this invalid access.

A matched 12,000-step 192x384 run is now underway, using the same physical
duration and output cadence as the completed 6,000-step 96x192 run. It uses a
separate output directory under `/tmp/kerr_routec_192_maskfix_final`. This
run will show whether the later horizon coverage rejection and constraint
growth converge with resolution; until it finishes, those comparisons remain
open.

The first attempt's artifacts end at step 400 (`t=0.08862`) and contain no
exit log; its termination cause is unknown. A tracked retry is running in
`/tmp/kerr_routec_192_maskfix_retry` so subsequent failures can be captured
from the live process session.

At step 200 (`t=0.04431`), both attempts have the same checkpoint: the finder
converges with area `40.19669` (`-0.039%` relative to Kerr), `r_min=1.59963`,
and `r_max=1.78834`. The tracked retry completed all 12,000 steps to
`t=2.65857`, with the finder converged at every 200-step sample. At the final
sample the area is `39.95390` (`-0.643%` relative to Kerr), `M_H=0.996686`,
and the surface spans `r_min=1.60210` to `r_max=1.78562`.

At the matched final time, the 96x192 run reaches `t=2.66553` but has already
lost the finder at step 2,000 (`t=0.8885`) because its candidate enters the
coverage buffer. Its exterior Hamiltonian max/RMS are `3.0323e-1` and
`8.6009e-3`; the 192x384 run has `8.2846e-2` and `2.2411e-3`. The reductions
are factors of 3.66 and 3.84, respectively, consistent with roughly second
order refinement over this interval. The finer run therefore resolves the
high-resolution finder coverage failure and improves the exterior Hamiltonian
residual, but it does not eliminate horizon area drift. The latter remains a
long-term accuracy failure that needs gauge and constraint investigation.

The HDF5 scalar `alpha_min/max` and `phi_min/max` include excised grid points,
so their large late-time values cannot be interpreted as exterior gauge
growth without a masked reduction. The driver prints a separate lapse minimum
outside the excision, but that console stream was not retained for this run.

An independent snapshot re-solve was added as `make diagnose-horizon-snapshot`
(pass `SNAPSHOT=...` and `PARFILE=...`). It reloads the final state and repeats
the solve at Legendre orders 8, 10, 12, 16, 20, and 24 with fresh initial
guesses. Every order finds a marginal surface with maximum expansion residual
below `2.1e-12`. The resulting areas range from `39.94017` to `39.95515`, a
0.038% spread, while every fit remains 0.640%–0.677% below the Kerr area.
Thus the late-time area loss is much larger than fit-order sensitivity and is
present in the evolved geometry, rather than being caused by the order-8
surface representation.
