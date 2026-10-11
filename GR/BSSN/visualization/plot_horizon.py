#!/usr/bin/env python3

#!/usr/bin/env python3
"""Plot the apparent-horizon diagnostics stored in output/diagnostics.h5.

usage:  python3 plot_horizon.py [output_dir] [--M 1.0 --a 0.5]

Prints a table of (step, time, area, M_irr, J, M_H) and saves horizon.png.
If --M and --a are given (the Kerr mass and spin you started from), the plots
also show the exact stationary-Kerr values, since area, J and M_H of a Kerr
hole do not depend on the slicing -- their drift is an accuracy monitor.
Rows where the solve failed (hz_found = 0) are masked and show up as gaps.
The image is written into the selected output directory, so it cannot
silently display a plot from a different run.
"""
import sys, argparse
import numpy as np
import h5py

p = argparse.ArgumentParser()
p.add_argument("outdir", nargs="?", default="output")
p.add_argument("--M", type=float, default=None)
p.add_argument("--a", type=float, default=None)
args = p.parse_args()

with h5py.File(f"{args.outdir}/diagnostics.h5", "r") as f:
    ds = f["diagnostics"]
    cols = ds.attrs["columns"]
    cols = (cols.decode() if isinstance(cols, bytes) else cols).split()
    data = ds[:]                       # h5py shape: (nrows, ncols)
if "hz_area" not in cols:
    sys.exit("no hz_* columns: run with GR_HORIZON_EVERY=<n> set")
c = lambda name: data[:, cols.index(name)]
t, step = c("time"), c("step")
area, mirr, J, MH = c("hz_area"), c("hz_m_irr"), c("hz_j_spin"), c("hz_m_H")
rmin, rmax, found = c("hz_rmin"), c("hz_rmax"), c("hz_found")
valid = (found >= 0.5) & np.isfinite(area) & np.isfinite(mirr) & np.isfinite(J) & np.isfinite(MH)
invalid_rows = np.flatnonzero(~valid)

print(f"{'step':>7} {'time':>9} {'found':>5} {'area':>12} {'M_irr':>10} {'J':>10} {'M_H':>10} {'r_min':>8}")
for i in range(len(t)):
    if valid[i]:
        print(f"{int(step[i]):7d} {t[i]:9.4f} {int(found[i]):5d} {area[i]:12.6f} "
              f"{mirr[i]:10.6f} {J[i]:10.6f} {MH[i]:10.6f} {rmin[i]:8.4f}")
    else:
        print(f"{int(step[i]):7d} {t[i]:9.4f} {int(found[i]):5d} "
              f"{'--':>12} {'--':>10} {'--':>10} {'--':>10} {'--':>8}  (horizon solve failed/rejected)")

exact = None
if args.M is not None and args.a is not None:
    M, a = args.M, args.a
    rp = M + np.sqrt(max(M * M - a * a, 0.0))
    exact = dict(area=8 * np.pi * M * rp, J=a * M, MH=M,
                 mirr=np.sqrt(8 * np.pi * M * rp / (16 * np.pi)))
    print(f"\nexact Kerr: area={exact['area']:.6f}  M_irr={exact['mirr']:.6f}  "
          f"J={exact['J']:.6f}  M_H={exact['MH']:.6f}")
    valid_rows = np.flatnonzero(valid)
    if valid_rows.size:
        j = valid_rows[-1]
        print(f"last valid: step={int(step[j])} dA/A={area[j]/exact['area']-1:+.2e}  "
              f"dJ={J[j]-exact['J']:+.2e} dM_H={MH[j]-exact['MH']:+.2e}")
    else:
        print("last valid: none")

try:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
except ImportError:
    sys.exit("(matplotlib not installed: table printed, no plot saved)")

fig, ax = plt.subplots(2, 2, figsize=(10, 7), sharex=True)
panels = [(ax[0, 0], area, "horizon area A", exact and exact["area"]),
          (ax[0, 1], mirr, "irreducible mass M_irr", exact and exact["mirr"]),
          (ax[1, 0], J, "spin J", exact and exact["J"]),
          (ax[1, 1], MH, "Christodoulou mass M_H", exact and exact["MH"])]
for a_, y, title, ex in panels:
    y_plot = np.where(valid, y, np.nan)
    a_.plot(t, y_plot, "o-", ms=3, label="numerical")
    if ex is not None:
        a_.axhline(ex, color="k", ls="--", lw=1, label="exact Kerr")
        a_.legend()
    if invalid_rows.size:
        first_bad = invalid_rows[0]
        a_.axvline(t[first_bad], color="tab:red", ls=":", lw=1,
                   label="first rejected solve" if a_ is ax[0, 0] else None)
    a_.set_title(title); a_.grid(alpha=0.3)
ax[1, 0].set_xlabel("t"); ax[1, 1].set_xlabel("t")
fig.suptitle("Apparent horizon (blank = solve failed)")
fig.tight_layout()
plot_path = f"{args.outdir}/horizon.png"
fig.savefig(plot_path, dpi=130)
print(f"\nsaved {plot_path}")
