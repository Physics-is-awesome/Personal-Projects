# Production normal principal matrix — second-order block

**Date:** 2026-11-09
**Branch:** `bssn-long-term-phase0`
**Code:** `helper/fo_production_normal.F90`,
`helper/test_fo_production_normal.F90`
**Gate:** `make test-fo-production-normal` (now part of `make check`)
**Artifact:** `run_archive/2026-11-04_phase0_post_fix/production_normal_matrix_20261109.dat`
**Logs:** `run_archive/2026-11-04_phase0_post_fix/production_normal_matrix_20261109.log`,
`.../make_clean_check_final_20261109.log` (`MAKE_CLEAN_CHECK_EXIT:0`)

## What was added

`fo_production_normal_mod` returns the analytic coefficient of the **highest
normal derivative** in the actual production 24-field BSSN + gauge RHS.  The
convention is

```text
delta(RHS_v) = ... + C2(v,w) d_n^2 delta(u_w) + ...
```

so this is a second-order principal matrix, not a first-order characteristic
matrix.  It contains:

| output row | nonzero second-normal columns | coefficient |
|---|---|---|
| `K` | `alpha` | `-e^{-4 phi} gtilde^{nn}` |
| `Atilde_ij` | `alpha`, `phi`, `gt_ab` | `e^{-4 phi}` times the trace-free `-D_iD_j alpha + alpha(Rtilde_ij + Rphi_ij)` block |
| `Gamma^i` | `beta^c` | `gtilde^{nn} delta^i_c + (1/3)(gtilde^{ia} n_a)n_c` |
| `B^i` (driver modes 0/3/4) | `beta^c` | same as `Gamma^i` |
| all other rows | none | zero |

The `Atilde` entries are

```text
C2(A_ij, alpha) = e^{-4 phi} [ -n_i n_j + (1/3) g_ij gtilde^{nn} ]
C2(A_ij, phi)   = e^{-4 phi} alpha [ -2 n_i n_j + (2/3) g_ij gtilde^{nn} ]
C2(A_ij, g_ab)  = e^{-4 phi} alpha [ -0.5 gtilde^{nn} delta_ij,ab
                                      + (1/6) gtilde^{nn} g_ij
                                        * m_ab gtilde^{ab} ],
```

where `m_ab = 1` for `a=b` and `2` for `a != b`.  The multiplicity is
required because `trace_X = sum(gtildeinv * X)` counts both `(a,b)` and
`(b,a)` for an off-diagonal conformal-metric component.  Omitting it produced
the observed factor-two error in the off-diagonal `Atilde <- gt_xz` and
`Atilde <- gt_yz` columns.

## Numerical validation

The test initializes the route-C Kerr trumpet at `a=0.8`, sets the production
excision background, and picks a central-stencil point at
`(x,z)=(0.5053,-1.9476)`, `r_KS=2.0034`, a few cells outside `r_excise=1.5`.

For each production field it applies two probes at the sample point:

- linear: `delta(u_w)=a n.(x-x0)`; at `x0` the value and second derivative
  vanish and the gradient is `a n`, so the response isolates the first-normal
  block;
- quadratic: `delta(u_w)=a [n.(x-x0)]^2`; at `x0` the value and gradient
  vanish and the Hessian is `2a n n`, so the response isolates `C2`.

The second probe has no wavelength and does not rely on a fitted dispersion
relation.  The measured `C2` is compared with the analytic matrix entry by
entry.

## Results

| probe | max relative `C2` error | max relative `A1` error (derived rows) | status |
|---|---:|---:|---|
| z normal `(0,0,1)` | `1.2e-9` | `1.8e-10` | clean `m=0` plane wave; gate |
| radial normal `(0.2184,0,-0.9759)` | `1.45e-3` | `1.16e-2` | C2 gate; A1 diagnostic |
| x normal `(1,0,0)` | `1.90e-2` | `2.99e-2` | diagnostic only |

All 24x24 `C2` entries pass the `3e-3` tolerance for the z and radial normals.
All derived `A1` rows (`phi`, conformal metric, lapse, shift) pass the same
tolerance for the z normal (exact `m=0` first derivative).  `A1` probes with
`n_x != 0` are contaminated by Cartoon reconstruction of the linear profile:
the test therefore gates `A1` only on the z normal, while `C2` is gated on z
and radial.  The x-normal `C2` discrepancy is the same transverse
reconstruction limitation documented previously.

## Analytic `A1` rows (partial)

`production_normal_first_matrix` now provides the first-normal block for rows
whose production RHS has no background Christoffel/connection perturbation:

| row | `A1` terms |
|---|---|
| `phi` | advection `beta^n d_n phi`, divergence `(1/6) n_i d_n beta^i` |
| `gt_ij` | advection `beta^n d_n gt_ij`, Lie terms `gt_ik n_j + gt_kj n_i - (2/3)gt_ij n_k` for `beta^k` |
| `alpha` (modes 0/1/3/4) | advection `beta^n d_n alpha` |
| `beta^i` (modes 0/4) | no first-normal derivative in the beta RHS; mode 3 adds advection |

Rows left open (`K`, `Atilde`, `Gamma`, `B`) require `DDa`, `Rphi`, `Rtilde`,
Gamma-driver, and constraint-coupling first-derivative contributions,
including background Christoffel perturbations.

## What this does and does not establish

Established:

- the highest-normal-derivative block of the actual production RHS is known
  analytically and matches the production point RHS on a curved Kerr state;
- the first-normal block is analytically closed and gated for `phi`,
  conformal metric, lapse, and shift (z-normal exact, m=0);
- the metric/Ricci trace-free projection and off-diagonal component
  multiplicity are validated;
- the nonzero `K <- alpha`, `Atilde <- (alpha, phi, gt_ab)`, and
  `Gamma/B <- beta` second-normal channels are identified.

Not established:

- analytic `A1` for `K`, `Atilde`, `Gamma`, and `B` (transport, Lie,
  Ricci/connection, gauge, Gamma-driver, and constraint-coupling terms);
- the full left/right characteristic transform and incoming/outgoing tests;
- no SAT or penalty is enabled, and the production path is unchanged;
- x/radial first-normal probes are not clean 3D Cartesian checks in the
  current Cartoon reduction.

The next P2.2 artifact is analytic closure and validation of the remaining
`A1` rows on the curved Kerr background, followed by the characteristic
transform and manufactured incoming/outgoing tests.  The reduced SAT must stay
disabled until that full coupled block is validated.
