# The production crash is the legacy excision SAT, not the excision radius

**Date:** 2026-10-31
**Measurement:** 96x192, `a = 0.8`, `slicing_mode = 3`, `gamma_driver_mode = 4`,
`cfl = 0.02` (verified to reproduce `cfl = 0.005`), 750 steps (`t = 1.3328`).
Only `excision_sat` and `r_excise` differ between the four runs.

## The 2x2

| run | `excision_sat` | `r_excise` | outcome | crash step | crash `t` | crash cell |
|---|---|---|---|---|---|---|
| A | **true** | 0.8 | error stop | 347 | 0.617 | `i=10 k=94`, `r_KS=0.91` |
| B | false | 0.8 | survives | — | 1.333 | — |
| C | **true** | 1.0 | error stop | 402 | 0.715 | `i=11 k=95`, `r_KS=1.01` |
| D | false | 1.0 | survives | — | 1.333 | — |

Both SAT-on runs die; neither SAT-off run does. The crash cell tracks the mask
edge (`i=10` at `r_exc = 0.8`, `i=11` at `r_exc = 1.0`), not a fixed physical
location.

Run A is an exact reproduction of the production failure: production runs
`cfl = 0.005` and fails at step 1387, i.e. `t = 0.6163`, at the **same cell**
`i=10 k=94`; run A fails at step 347 with `cfl = 0.02`, i.e. `t = 0.617`, at the
same cell. The failure mode is identical:
`det(gamma) < 0` in `project_bssn_constraints`. `|det-1|` in the innermost
`r_KS` bin `[0.8, 1.3)` sits flat at `~2e-6` for most of the run, then goes
`9.0e-4 -> 2.8e-2` in the last two projection reports before the stop, i.e. the
loss of positivity is sudden, not gradual.

## Surviving branch at `t = 1.333`

| metric | B (r_exc 0.8) | D (r_exc 1.0) |
|---|---|---|
| `max|H|(ext)` | 8.71e-02 | 1.44e-01 |
| `rms|H|(ext)` | 3.74e-03 | 5.15e-03 |
| `max|M|(ext)` | 2.52e-02 | 1.37e-02 |
| `alpha(probe)` | 0.924556 | 0.924556 |
| `alpha_min` | 0.29601 | 0.40551 |

Moving the mask inward to 0.8 improves the Hamiltonian residual by 40% but
worsens the momentum residual by 84%. Neither is a stationarity claim; both are
still growing secularly.

## Why the SAT fails

`apply_legacy_excision_sat` penalises the evolved state toward the **background
state captured at `t = 0`**,

```
rhs += -tau * lambda * (w - w_target) / h_normal     (incoming modes only)
```

with `u_target` read from `background_state`. For a genuinely stationary
configuration that is a legitimate boundary condition. For the production pair
(Kerr-Schild data with DBM-derived `f`) the background is *not* the solution the
evolution is moving toward, so the penalty is a persistent source that grows as
the evolved state separates from the initial data. The loss of positivity then
shows up first at the mask edge, which is exactly what the 2x2 measures.

The module header already states this:

> the production RHS has no complete first-order principal symbol or full
> characteristic transform. This penalty is experimental and is NOT an exact
> characteristic boundary treatment for the production system.

## What turning it off buys — and does not

A 6000-step follow-up (96x192, `r_excise = 0.8`, SAT **off**, `cfl = 0.02`,
`t` up to 10.7 requested):

| t | `max|H|(ext)` | `rms|H|(ext)` | `max|M|(ext)` | `alpha_min` | found `r_+` |
|---|---|---|---|---|---|
| 0.89 | 6.63e-2 | 3.48e-3 | 1.04e-2 | 0.34770 | 1.600 |
| 1.78 | 1.92e-1 | 6.64e-3 | 3.26e-2 | 0.26285 | — |
| 2.67 | 5.50e-1 | 1.29e-2 | 8.49e-2 | 0.23678 | — |
| 3.55 | 7.05e-1 | 1.49e-2 | 9.30e-2 | 0.22869 | — |
| 4.44 | 6.47e-1 | 1.43e-2 | 2.05e-1 | 0.22777 | 1.333 |
| 5.33 | 7.18e-1 | 1.59e-2 | 2.34e-1 | 0.22884 | 1.273 |

Two things are established:

1. **`alpha_min` saturates at ~0.228** and stops moving after `t ~ 3.5`. The
   lapse relaxation is bounded and is not the failure.
2. **The spacetime is not stationary.** `max|H|(ext)` reaches `0.7` (from `1e-2`
   at `t = 0`), `M_irr` grows `0.8944 -> 0.9046`, `J` falls `0.7994 -> 0.7867`,
   and the found horizon migrates `1.600 -> 1.273`. At step 3500 (`t = 6.22`)
   the driver aborts on `dA/A = 7.1e-2 > 5e-2` and states: *"the evolved black
   hole is NOT stationary. The spacetime itself is changing, which is a genuine
   failure."*

So switching the SAT off is necessary and correct, but it converts an
`O(1)`-time crash into an `O(6)`-time physical drift. It does not make
`T/M >= 100` reachable. That requires the matched route-C configuration, which
is blocked by a chart degeneracy that coincides with the horizon at
`a = 0.8` (`ROUTE_C_INTERIOR.md`).

## Consequence

- `excision_sat = true` in the working-tree `parameter_files/vacuum_kerr.par`
  is the direct cause of the `test-horizon-drift` failure and of the step-1387
  crash. The committed value is `false`.
- Turning it off is not a cosmetic workaround: it is the difference between a
  run that dies at `t/M = 0.62` and one that reaches `t/M = 1.33`.
- This does **not** make the run good. The surviving branch still has a
  secularly growing exterior constraint and no stationarity argument. It is the
  floor to build on, not a result.

The SAT cannot be repaired by rescaling `tau`: the target state is wrong, not
the penalty size. A usable SAT needs either (a) a target computed from the
current solution (characteristic projection without a frozen background), or
(b) the matched route-C data, whose background *is* the solution — but that
path is blocked by the same chart degeneracy
(`ROUTE_C_INTERIOR.md`).
