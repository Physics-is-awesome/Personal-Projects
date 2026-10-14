# Momentum-constraint investigation — findings

**Date:** 2026-10-13
**Scope:** the §13.7 / §6D lead in `LONG_TERM_GAUGE_DRIFT_REPORT.md`.
**Status:** defect localized to a single assembly. Not yet fixed.

## Summary

The near-axis momentum-constraint residual reported as `M_z ≈ -1.25e-1` at
`(x,z) = (0.0628272251309, -1.535248)M` is **not** an axis-parity or cartoon
derivative problem. The initial data are exact, and the residual is produced by
the seven-piece BSSN assembly in `helper/constraints.F90:momentum_constraint`,
which does not agree with an independent ADM-form evaluation of the same state
by a margin far larger than the discretization error.

## Reproduce

```sh
make bin/diagnose_momentum_point
./bin/diagnose_momentum_point parameter_files/vacuum_kerr_192.par \
      0.0628272251309 -1.535248
```

## Step 1 — baseline established (DONE)

`make check` was **failing** on this machine before any change of mine:
`test_horizon_drift` T4 failed. Cause was a real latent bug, not the
environment:

`helper/horizon.F90:expansion` declared `ok` as `intent(inout)` but only ever
wrote `.false.` — never `.true.` on the success path. Callers therefore had to
pre-seed it. `marginal()` (horizon.F90:636) did; `check_trapped()` in
`helper/test_horizon_drift.F90` did not and read uninitialised stack.

Fixed by making `ok` a true `intent(out)` set at function entry, accumulating
the flag explicitly in `residual()` (which loops), and seeding `ok` in the
test. T4 now returns `Theta = -9.38e-3` inside / `+9.69e-3` outside — the
correct sign change for a genuinely trapped surface.

`make check` now passes. The 700-step baseline reproduces exactly:
`max|H|=1.3162E-01 rms|H|=3.8587E-03 max|M|=5.6664E-02 alpha=0.910348`.

## Step 2 — initial data are exact (NOT the defect)

New diagnostic `helper/diagnose_momentum_point.F90` compares the stored BSSN
state against the analytic state built from `kerr_schild_adm()` at the same
point. At the report point:

```
max |A-B| over all gtilde and Atilde components = 1.11e-16
```

So `set_kerr_schild_id` converts `(gamma,K) -> (phi,gtilde,K,Atilde)` correctly.
§13.7's four candidate stages: the **data stage is ruled out**.

## Step 3 — the residual comes from the constraint assembly

At the report point, on the *same state with the same production stencils*:

| assembly | `M_x` | `M_y` | `M_z` |
|---|---|---|---|
| `constraints.F90` seven-piece BSSN | -1.368e-02 | +6.755e-03 | **-1.246e-01** |
| independent ADM form (new routine) | +5.769e-04 | -3.273e-04 | **-1.979e-02** |

The ADM form `M^i = D_j(K^{ij} - gamma^{ij}K)` uses the **physical** metric and
the **physical** connection, reconstructed from the sampled state through the
production cartoon stencil. It shares no code with `constraints.F90`.

The two disagree by ~6x in `M_z` at a point where both should be near zero.
The remaining nonzero ADM value is ordinary truncation error (this point lies
just inside the polar horizon at `r_KS = 1.536 < r+ = 1.6`, where the field
varies on the grid scale).

## Ruled out along the way

- **Cartoon y-derivative** (`stencil.F90:231`). The operator is geometrically
  inexact — it returns `F(s)/s` with `s = sqrt(x^2+dy^2)` where exactness needs
  `F(x)/x`, a relative error of `x/sqrt(x^2+dy^2)` = 0.707 at `x = dx`. But for
  Kerr `A^{xz} ∝ x` near the axis, so the error cancels: measured code/exact
  ratio is 1.0035 at `i=2`, 0.9987 at `i=12`, and converging. Not the defect,
  though it is a real accuracy limitation of the operator worth noting.
- **Radial interpolation stencil** (`cartoon.F90:interp_radial`). Verified
  third-order accurate and exact on grid points. Correct as written.
- **Initial-data conversion.** Exact to 1.1e-16 (Step 2).

## Caveat on the report's own independent script

`/tmp/independent_ks_momentum.py` uses `phi = +ln(1+2H)/12`. The correct BSSN
mode-0 value is `phi = ln(det gamma)/12 = -ln(1+2H)/6` — wrong sign and half
magnitude. Its **ADM momentum** check is still valid (it never uses BSSN
fields, and correctly returns ~1e-41). Its **seven-piece BSSN values** are not
a valid comparison and should not be quoted.

## Next step

Rewrite `constraints.F90:momentum_constraint` to assemble the ADM form
(`D_j(K^{ij} - gamma^{ij}K)`, physical connection), then re-check that it
converges under refinement and that the stored-vs-analytic state check still
holds. The seven-piece decomposition should be kept only as a diagnostic, and
its coefficients re-derived from scratch rather than taken from Brown et al.
2012 eq. 2.19b, whose form does not match this code's `det(gtilde)=1`
convention.

## Does the momentum defect reach the evolution?  NO.

This is the question that decides whether the defect is a stability blocker or
a validation blocker. Answer: **it cannot affect the evolution at all.**

### Structural evidence

`constraints_mod` is imported by exactly six files, none of them on the
evolution path:

- `drivers/vacuum_kerr.F90` — reporting only; calls
  `hamiltonian_constraint(u, Hgrid, ...)` and `momentum_constraint(u, Mgrid)`
  with `u` `intent(in)`, writing to fresh diagnostic arrays.
- `helper/diagnose_*.F90`, `helper/test_*.F90` — diagnostics and tests.

`integrators/time_step.F90` (the RK4 driver) does **not** import
`constraints_mod`. Its imports are `kinds_mod`, `vars_mod`, `params_mod`,
`grid_mod`, `excision_mod`, `bssn_rhs_mod`, `dissipation_mod`,
`sommerfeld_mod`, `excision_sat_mod`.

`integrators/bssn_rhs.F90` contains **zero** references to constraints,
constraint damping, `H`, or `Mgrid`.

`helper/fo_constraint_damping.F90` exists in the tree and is compiled into the
production binary, but has **no callers** anywhere — dead code.

`time_step.F90:project_bssn_constraints` is misnamed. Despite the name it
enforces only *algebraic* BSSN conditions and reads no differential constraint:
lapse floor, `det(gtilde) = 1` rescale, and `tr(Atilde) = 0`. It does not
project H or M^i. `elliptic_hamiltonian_correction` (the only routine that
would feed H back into the state) likewise has **no callers**.

### Empirical proof

Patched `momentum_constraint` to return `1.234567E+00` everywhere, rebuilt
`vacuum_kerr`, and reran the 700-step baseline:

```
corrupted: max|H|(ext)=1.3162E-01  rms|H|(ext)=3.8587E-03  max|M|(ext)=2.1383E+00  alpha=0.910348
baseline:  max|H|(ext)=1.3162E-01  rms|H|(ext)=3.8587E-03  max|M|(ext)=5.6664E-02  alpha=0.910348
```

Every evolution quantity — `max|H|`, `rms|H|`, `alpha(probe)` — is
**bit-identical**. Only the reported `max|M|` changed, which is precisely the
quantity that routine produces. The state trajectory is provably independent
of it. (`constraints.F90` was restored from backup and re-verified by sha256
and by the 700-step baseline.)

### Consequence

The momentum-constraint defect **cannot** cause the long-time conformal
determinant failure. Theories A–C in the report (gauge/data mismatch, inner
boundary, near-excision resolution) remain the live candidates for the actual
failure; D (this defect) is now confined to the validation track.

Its real cost is that it undermines the equilibrium-Kerr route: Gate A in §13.2
requires trusting constraint output, and `test-momentum-conv` is currently
passing while measuring something that is not the momentum constraint.

## Step 4 — FIXED: momentum_constraint rewritten in ADM form (DONE)

`helper/constraints.F90:momentum_constraint` now evaluates

```
M^i = D_j S^{ij} ,   S^{ij} = K^{ij} - gamma^{ij} K
                   = e^{-4phi} ( Atilde^{ij} - (2/3) gamma-tilde^{ij} K )
```

contracted with the **physical** connection

```
Gamma^k_{ij} = gamma^{km} (1/2)( d_i gamma_{mj} + d_j gamma_{mi} - d_m gamma_{ij} )
d_m gamma_ij = 4 d_m phi gamma_ij + e^{4phi} d_m gamma-tilde_ij
```

Differentiating `S^{ij} = e^{-4phi} Q^{ij}` once yields exactly seven pieces,
which replace the old mislabelled decomposition:

| piece | term |
|---|---|
| 1 | `e^{-4phi} d_j Atilde^{ij}` |
| 2 | `-(2/3) e^{-4phi} K d_j gamma-tilde^{ij}` |
| 3 | `-(2/3) e^{-4phi} gamma-tilde^{ij} d_j K` |
| 4 | `-4 e^{-4phi} d_j phi Atilde^{ij}` |
| 5 | `+(8/3) e^{-4phi} d_j phi gamma-tilde^{ij} K` |
| 6 | `Gamma^i_{jk} S^{kj}` |
| 7 | `Gamma^j_{jk} S^{ik}` |

### Effect

Near-band momentum max (`diagnose_stationary_ks_rhs`, fixed radial band):

| Nx | before | after |
|---|---|---|
| 48  | 1.109e-01 | 4.12e-02 |
| 96  | 1.200e-01 | 3.17e-02 |
| 192 | 1.250e-01 | 2.09e-02 |
| 384 | — | 1.67e-02 |

The previous **non-convergence** (0.111, 0.120, 0.125 — increasing) is gone;
the residual now falls monotonically. `M3` RMS is monotone at every
resolution: 1.20e-2, 9.48e-3, 7.59e-3, 6.81e-3.

`test_momentum_conv` (annulus r in [3,5]M) now gives max|M| = 7.14e-3, 4.96e-3,
3.69e-3 and rms|M| = 1.05e-3, 5.73e-4, 5.44e-4 — monotone.

### test_momentum_conv could never fail

It printed an "observed order" and then always returned success. That is why
it passed while measuring the wrong expression. It now asserts that rms|M|
strictly decreases under each refinement step, and `error stop`s otherwise.
Verified this gate is real: flipping the sign of one piece makes it FAIL.

Observed order is still modest (0.5, 0.4 on max|M|) rather than 4th order.
The bulk annulus residual is dominated by points whose stencil order drops
near the excision mask, and by the axis-adjacent layer; the RMS converges
better than the max. This is a refinement of the claim, not a new defect, but
it is **not** yet the clean 4th-order convergence the report's Gate C asks
for, and should not be reported as such.

### Also corrected

- `test_horizon_drift` / `horizon.F90` `ok` contract (Step 1).
- Stale piece labels in `diagnose_stationary_ks_rhs.F90` and
  `diagnose_momentum_point.F90`.
- `diagnose_stationary_ks_rhs` now accepts a parameter file argument, and the
  Makefile has build-only targets, so convergence runs at other resolutions
  are possible (previously the binary was hard-wired to the 48x96 default and
  silently ignored arguments — a stale-binary trap I hit myself).
- `diagnose_momentum_point` now refuses to run on an unreadable parameter file
  instead of silently falling back to built-in defaults.

## Deliberately not done

No production evolution equation or gauge parameter was changed. The 700-step
evolution baseline is bit-identical (`max|H|`, `rms|H|`, `alpha`); only the
reported `max|M|` moves, from 5.6664E-02 to 4.8257E-02.

The long-time determinant failure remains unaddressed. Per Step 3, this
momentum defect was never on the causal path to it; theories A–C in the report
(gauge/data mismatch, inner-boundary treatment, near-excision resolution)
remain the live candidates.