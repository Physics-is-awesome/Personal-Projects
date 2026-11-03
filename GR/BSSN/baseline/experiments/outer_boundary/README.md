# Outer-boundary sweep

Run from `GR/BSSN`:

```sh
make bin/vacuum_kerr
baseline/experiments/outer_boundary/sweep.sh
```

The sweep compares 12M and 18M domains at nearly fixed grid spacing, with the
empirical outer sponge disabled and enabled. It uses frozen Kerr-Schild gauge
controls and stops at approximately `t = 0.5`. The final `step` line from each
run reports the exterior Hamiltonian and momentum residuals at equal physical
time.

The sponge is an experiment, not a characteristic boundary condition. A useful
result is a reduction that persists across both domain sizes without changing
the near-hole configuration.

The initial sweep did not meet that criterion. At `t ~= 0.5`, the disabled
baseline had RMS Hamiltonian residuals `1.08e-3` (12M) and `6.81e-4` (18M).
The tested sponge settings increased them:

| Domain | Width | Strength | RMS `H` |
|---|---:|---:|---:|
| 12M | 0 | 0 | `1.08e-3` |
| 12M | 2M | 0.05 | `3.08e-3` |
| 12M | 4M | 0.10 | `9.85e-3` |
| 18M | 0 | 0 | `6.81e-4` |
| 18M | 2M | 0.05 | `1.64e-3` |
| 18M | 4M | 0.10 | `4.63e-3` |

The sponge should therefore remain disabled in production. The result supports
replacing the scalar Sommerfeld prescription with a characteristic,
constraint-preserving boundary treatment rather than adding stronger damping.
