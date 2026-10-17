# Projection and ghost-fill measurements at the excision margin

**Date:** 2026-07-16
**Follows:** `MARGIN_FINDINGS.md` (which showed the default diagnostic margin
hides a ~20-400x larger constraint violation in the band 1.0 < r < 2.0).
**Tooling added:** `helper/diagnose_ghost_accuracy.F90`
(`make diagnose-ghost-accuracy`), env-gated `GR_PROJ_DIAG` inside
`project_bssn_constraints`.

## Question

Two untested candidates from `MARGIN_FINDINGS.md`, both acting near the mask:

1. Does the algebraic projection manufacture its own constraint violation by
   rescaling gtilde to det=1 at every RK stage?
2. Is the excised-region ghost fill inaccurate enough to explain the O(1)
   Hamiltonian violation at the innermost physical points?

## Result 1 -- the projection is NOT the cause (falsified)

`project_bssn_constraints` rescales gtilde by `det^(-1/3)` and removes
`tr(Atilde)` four times per step (three stage projections plus one on the
final state). Measured with `GR_PROJ_DIAG`, frozen gauge, Nx=48, r_excise=1.0:

| stage | worst \|det(gtilde)-1\| | \|scale-1\| | \|tr(Atilde)\| |
|---|---|---|---|
| call 1 | 9.82e-6 | 3.28e-6 | 4.29e-5 |
| call 4 | 1.97e-5 | 6.55e-6 | 8.57e-5 |
| call 1201 (step 300) | 2.25e-5 | -- | -- |

The correction is 4-5 orders of magnitude smaller than the H ~ 1 it would have
to explain, and it grows only linearly (9.8e-6 -> 2.3e-5 over 400 steps) at
the same relative rate as H itself. It is a *consequence* of the developing
error, not a driver, and it never approaches O(1).

**Falsified.** The `GR_PROJ_DIAG` diagnostic is left in place but off by
default; with the default unset the output is bit-identical (verified by diff).

## Result 2 -- the ghost fill is EXACT (falsified)

`diagnose_ghost_accuracy` builds the analytic Kerr-Schild BSSN state at every
ghost point's own (x, 0, z) using the production conversion formula and
compares against what `extrapolate_excised` actually stored:

```
worst ghost fill error anywhere = 4.4409E-16
```

Machine precision, at every depth inside the mask. This is expected in
hindsight: `set_excision_background` anchors the background to the t=0 exact
Kerr-Schild data, and for a stationary exact solution that *is* the exact
value at all later times.

Replacing the ghosts with analytic values changes H not at all (bit-identical,
8.7779E-01 either way).

**Falsified.** This also explains why the earlier `excision_order` and
`GR_REFRESH_BG` nulls were genuine: there was nothing for them to fix.

## Result 3 -- where the error actually lives

Decomposing H = R + (2/3)K^2 - A^2. The K and A terms are pointwise algebraic
and carry no discretisation error, so the whole error must be in
R = e^(-4phi) gtilde^ij (Rt_ij + Rphi_ij).

Max|.| per radial bin, bins of one cell outward from the mask, t=0:

| dx | bin0 \|Rt\| | bin0 \|Rphi\| | bin0 H |
|---|---|---|---|
| 0.2553 | 2.689e-1 | 1.992e-1 | 7.774e-1 |
| 0.1263 | 5.606e-1 | 2.049e-1 | 1.142e-1 |
| 0.0628 | 6.283e-1 | 2.256e-1 | 6.205e-2 |

Individual derivative errors at bin0 all converge (worst is `d2y` on
gtilde_xx: 9.3e-2 -> 3.6e-2 -> 1.1e-2), yet the assembled H falls much more
slowly (0.777 -> 0.114 -> 0.062, i.e. orders 2.8 then 0.9). So the assembly
amplifies derivative error well beyond what the derivatives alone suggest.

## Two bugs I introduced and had to fix

Both were in the new diagnostic, not the production code, and both initially
produced confident nonsense:

1. **Forward difference dressed up as a second derivative.** I built the y
   second-derivative reference as `(f(+h)-f(0))/h^2` without the `-h` point.
   That is a forward difference scaling as `f'/h`, and it reported a spurious
   `|d2y err| ~ 1.0e+03` that looked exactly like a smoking gun. With a proper
   central difference the same quantity is ~1e-2 and converges normally.
2. **Missing `compute_conformal_connections`.** The diagnostic called
   `set_kerr_schild_id` but not the separate connection pass, so IGTX..IGTZ
   were identically zero. Every H I reported from that binary was computed
   with `Rt`'s term3 silently dropped. Adding the call changed bin0 H at
   Nx=96 from 4.93e-1 to 1.14e-1.

A third measurement (PART 5, H recomputed with fully analytic derivatives) was
also invalid -- the analytic state left IGTX zero, dropping term3 again -- and
has been removed rather than kept in a broken state.

## Resolution of the two open items (added 2026-10-17)

1. **The driver/diagnostic discrepancy** was a filter defect in the driver, not a
   difference in the physics. `drivers/vacuum_kerr.F90` selected diagnostic
   points with the *cylindrical* radius `sqrt(x^2+z^2)` while the excision mask
   uses the *Kerr* radius; the driver's reported worst point had `r_KS ~ 0.83`,
   inside the mask. Fully diagnosed and fixed in `DRIVER_FILTER_BUG.md`.

2. **Gamma-tilde converges, so it is not the culprit.** Measured against the
   same formula evaluated on the exact analytic conformal metric
   (`Gamma^k = -(dx gt^{k1} + dy gt^{k2} + dz gt^{k3})`), worst of the three
   components, in bins outward from the mask:

   | dr/dx | Nx=48 | Nx=96 | Nx=192 |
   |---|---|---|---|
   | 0 | 9.071e-2 | 3.051e-2 | 7.680e-3 |
   | 1 | 4.396e-2 | 2.008e-2 | 5.812e-3 |
   | 2 | 2.444e-2 | 1.379e-2 | 5.531e-3 |

   Orders ~1.57 then ~1.99 -- it converges, at the second order expected from
   `compute_conformal_connections`' second-order central differences. Its
   magnitude at bin 0 (9.07e-2 at Nx=48) sits *within* the range spanned by its
   neighbours (`d2x` of gtilde_xx is 1.80e-1 there, `d2y` is 9.30e-2). So
   Gamma-tilde is lower-order than the surrounding fourth-order stencil, as
   suspected on structural grounds, but it is not an outlier and does not
   explain a non-converging error.

   Note this measurement required an analytic reference built from the
   production formula itself. The earlier attempt returned all zeros because
   the analytic state left IGTX identically zero.

## Revised picture

The near-mask Hamiltonian violation is real and is what kills the run, but it is
**not** explained by the projection, the ghost fill, or Gamma-tilde -- all three
now measured and cleared. Every derivative quantity examined converges at
~second order; the assembled H is larger than its inputs by roughly 5-8x
(consistent with the curvature contraction's 1/gamma^2 and Gamma-Gamma terms),
and its apparent non-convergence (orders 2.45 then 1.19) may be an artifact of
comparing maxima attained at different physical points -- the worst point sits
at dr/dx = 0.03, 1.10 and 0.79 for Nx = 48, 96, 192 respectively.

That leaves an open question of the productive kind: if the error is
resolution-controlled, refinement should buy lifetime directly. Measured in
`baseline/experiments/reslife/`.

## Unresolved (superseded)

1. ~~**The diagnostic does not reproduce the driver.**~~ Resolved -- driver
   filter defect, see above.
   `max|H|` at margin=0.05 is 0.820 / 1.806 / 0.509 for Nx=48/96/192 and does
   not converge. This diagnostic reports bin1 max 1.42e-1 at Nx=96 where the
   driver reports 1.806 at the same radius -- a factor of ~13. Both use the same
   `hamiltonian_constraint` on the same initial state. The discrepancy is
   unexplained and is the most important open item, because one of the two is
   measuring the wrong thing.
2. **Gamma-tilde is on a separate, never-validated path.**
   `compute_conformal_connections` builds IGTX..IGTZ from **second-order**
   central differences of gtilde^{-1} via `get_neighbor`, with no excision-mask
   guard on its read range. The curvature operator, meanwhile, uses the
   **fourth-order** `compute_stencil`. So the state field feeding `Rt`'s term3
   is two orders less accurate than the fields around it, by construction, and
   its error has never been measured against anything. This is now the
   highest-value remaining suspect: it is the one state field that is both
   demonstrably on a different (lower-order) path and unvalidated.

## Bottom line

The projection and the ghost fill are both cleared -- they are not the cause,
and both measurements are negative results worth keeping. The error is a
real, resolution-independent constraint violation confined to the first one to
two cells outside the excision surface, present in the initial discretisation.
The leading remaining suspect is the conformal connection, which is computed
on a separate second-order path from everything else in the curvature
operator.