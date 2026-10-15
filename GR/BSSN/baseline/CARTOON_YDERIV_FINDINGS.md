# Cartoon y-derivative accuracy — measured, gated, not the bottleneck

**Date:** 2026-10-14
**Instrument:** extended `helper/test_cartoon.F90` (now a real gated test)
**Hypothesis tested:** the cartoon y-derivative's `x/sqrt(x^2+dy^2)` geometric
error (0.707 relative at `x = dx`) is a major, fixable contributor to the
long-time failure.

## Result: the geometric error largely cancels

The manufactured-field test (axisymmetric scalar + polar vector + symmetric
tensor with known closed-form `d_y`) measures the production `d1y` against the
exact chain-rule derivative:

```
   i   x/dx   rel.err(scalar)  rel.err(vector)  rel.err(tensor d_y T^{yz})
   2     2.0      0.00000E+00      0.00000E+00      1.29876E-02
   4     4.0      0.00000E+00      0.00000E+00      5.09449E-03
   8     8.0      0.00000E+00      0.00000E+00      2.22630E-03
  12    12.0      0.00000E+00      0.00000E+00      1.41973E-03
```

- Scalar `d_y` is **exact** (the operator is antisymmetric in the ghost pair).
- Vector `d_y V^x` is **exact** (this is the axis-singular component; the stored
  `V^y = 0` on the plane makes the surviving piece exact).
- Tensor `d_y T^{yz}` has ~1.3e-2 relative error at `x=dx`, falling to ~1.4e-3
  by `x=12dx`.

The predicted 0.707 factor is **absent**. Reason: a regular axisymmetric tensor
has `T^{xz} ~ x` near the axis (odd under `x -> -x`), and `d_y T^{yz} = T^{xz}/x`
is therefore finite and the `s = sqrt(x^2+dy^2)` substitution reproduces
`T^{xz}(x)/x` to leading order. The leading geometric error cancels for smooth
fields — the same cancellation seen for Kerr's `A^{xz}` earlier.

The residual error is `O(1/x)`: `rel.err * (x/dx)` is ~constant (0.026 -> 0.017).

## Sizing against the actual ring residual

- Cartoon `d1y` error near the axis: **~1e-2 relative**, and it is largest only
  within a few cells of the axis.
- Ring RHS residual it would have to explain: **~1.4e-1** at Nx=48, dominated by
  points at **large** `x` (measured in `INNER_BOUNDARY_FINDINGS.md`), where the
  cartoon error is ~2e-3.

So the cartoon y-derivative accounts for roughly **1-2%** of the ring error — a
real but minor contributor, and it decays *toward* the region that dominates.
It is **not** the bottleneck and fixing it would not materially change
survival.

## What was changed

`helper/test_cartoon.F90` now exercises the cartoon y-DERIVATIVE (the old test
only checked value reconstruction) and **gates** it: it fails if the tensor
error at `x=dx` exceeds 5e-2 or does not decrease away from the axis. This
converts a silent gap (derivatives were untested) into a real regression guard.
The Makefile `test-cartoon` target was widened to build against the full
production module set (it previously compiled only a handful of modules, so it
could not have caught this class of bug at all).

No production stencil, RHS, or boundary code was modified.

## Status of the overall investigation

This closes the last "genuinely non-convergent mechanism" on the short list:

| candidate | verdict |
|---|---|
| momentum constraint | real defect, **fixed**; not on the evolution path |
| gauge / horizon migration | real symptom, **not the cause** (frozen gauge drifts and fails) |
| interior fill order | **no effect** on lifetime |
| stale background anchor | **no effect** |
| cartoon y-derivative | **~1-2% of ring error**; gated, not the bottleneck |

Every measurement points the same way: the failure is **second-order
truncation error near the excision mask accumulating until the algebraic
projection can no longer keep `gtilde` positive**, and the practical lever is
resolution (or an SBP-consistent near-boundary discretization), not a boundary
condition fix.

## Reproduction

```sh
make test-cartoon
./bin/test_cartoon
```