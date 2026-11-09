#!/usr/bin/env python3
"""Check that an archived run is usable as an acceptance result.

This deliberately does NOT treat "the driver reached nsteps" as success.
It requires the horizon to stay found through the requested time and checks
the invariant drift and lapse range recorded in the run directory.

Usage:
    python3 baseline/check_acceptance_run.py RUN_DIR --target-t 10
    python3 baseline/check_acceptance_run.py RUN_DIR --target-t 30 --max-h 0.1

RUN_DIR must contain horizon.dat and diagnostics.h5.  The script uses h5dump
(the same tool the Fortran build already requires) and only Python's standard
library.  Exit status is 0 only on PASS.

This checker is for the Phase 3 acceptance campaign.  A regression baseline
such as the 6000-step 96x192 run intentionally continues after the finder
rejects the surface and is not expected to pass the horizon requirement.
"""

import argparse
import math
import re
import subprocess
import sys
from pathlib import Path

# diagnostics.h5 columns (see helper/output_test.F90 and the file attribute)
DIAG_COLS = [
    "step", "time", "dt", "alpha_min", "alpha_max", "phi_min", "phi_max",
    "ham_max", "ham_rms", "n_nan_metric", "hz_found", "hz_area", "hz_m_irr",
    "hz_j_spin", "hz_m_h", "hz_rmin", "hz_rmax",
]


def parse_horizon(path):
    rows = []
    for line in path.read_text().splitlines():
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        p = line.split()
        if len(p) < 11:
            continue
        try:
            rows.append({
                "step": int(p[0]),
                "time": float(p[1]),
                "found": p[2] == "T",
                "area": float(p[5]),
                "m_irr": float(p[6]),
                "j_spin": float(p[7]),
                "m_h": float(p[8]),
                "hmin": float(p[9]),
                "hmax": float(p[10]),
            })
        except ValueError:
            continue
    return rows


def parse_diagnostics(path):
    """Read the single /diagnostics dataset through h5dump."""
    try:
        out = subprocess.run(
            ["h5dump", "-d", "/diagnostics", str(path)],
            check=True, capture_output=True, text=True,
        ).stdout
    except FileNotFoundError:
        raise RuntimeError("h5dump not found; it is required to read diagnostics.h5")
    except subprocess.CalledProcessError as exc:
        raise RuntimeError(f"h5dump failed on {path}: {exc.stderr.strip()}")

    body = out.split("DATA {", 1)[1]
    # The columns/layout attributes follow the dataset; stop before them.
    body = re.split(r"\n\s*}\s*\n\s*ATTRIBUTE", body, maxsplit=1)[0]
    parts = re.split(r"\((\d+),(\d+)\):", body)
    rows = {}
    for k in range(1, len(parts), 3):
        i, j = int(parts[k]), int(parts[k + 1])
        vals = re.findall(
            r"(?i)(?:nan|[+-]?inf(?:inity)?|[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?)",
            parts[k + 2],
        )
        row = rows.setdefault(i, {})
        for m, value in enumerate(vals):
            try:
                row[j + m] = float(value)
            except ValueError:
                row[j + m] = float("nan")

    ncols = len(DIAG_COLS)
    out_rows = []
    for i in sorted(rows):
        r = rows[i]
        out_rows.append([r.get(c, float("nan")) for c in range(ncols)])
    return out_rows


def rel_drift(value, reference):
    if reference == 0.0 or not math.isfinite(reference):
        return float("nan")
    return (value - reference) / abs(reference)


def fmt(value, width=12):
    if value is None or not math.isfinite(value):
        return "nan".rjust(width)
    return f"{value: .6e}".rjust(width)


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("run_dir", type=Path)
    ap.add_argument("--target-t", type=float, default=10.0,
                    help="required final t/M (default: 10)")
    ap.add_argument("--drift-tol", type=float, default=0.01,
                    help="maximum relative invariant drift (default: 1%%)")
    ap.add_argument("--max-h", type=float, default=None,
                    help="optional maximum allowed max|H| (default: report only)")
    args = ap.parse_args()

    hz_path = args.run_dir / "horizon.dat"
    diag_path = args.run_dir / "diagnostics.h5"
    if not hz_path.is_file():
        print(f"FAIL: missing {hz_path}")
        return 2
    if not diag_path.is_file():
        print(f"FAIL: missing {diag_path}")
        return 2

    hz = parse_horizon(hz_path)
    diag = parse_diagnostics(diag_path)
    if not hz or not diag:
        print("FAIL: no horizon or diagnostics rows")
        return 2

    failures = []
    warnings = []

    t_final = max(r[1] for r in diag)
    found = [r for r in hz if r["found"]]
    found_window = [r for r in found if r["time"] <= args.target_t + 1.0e-12]
    lost = [r for r in hz if not r["found"] and r["time"] <= args.target_t + 1.0e-12]
    t_last_hz = found[-1]["time"] if found else float("nan")

    print(f"run:            {args.run_dir}")
    print(f"t/M final:      {t_final:.6f}  (target {args.target_t:.6f})")
    print(f"horizon checks: {len(hz)} total, {len(found)} found, last at t/M={t_last_hz:.6f}")

    if t_final < args.target_t:
        failures.append(f"final time {t_final:.6f} < target {args.target_t:.6f}")
    if not found:
        failures.append("horizon was never found")
    if lost:
        first = lost[0]
        failures.append(
            f"horizon not found at t/M={first['time']:.6f} (step {first['step']}), "
            f"before target {args.target_t:.6f}"
        )

    if not found_window:
        failures.append(
            f"no found horizon at or before target t/M={args.target_t:.6f}"
        )
    else:
        ref = found_window[0]
        last = found_window[-1]
        print("\ninvariant drift at last found surface within target (t/M=%.6f):"
              % last["time"])
        print("  invariant       first          last       relative")
        for key, label in [
            ("area", "area"),
            ("m_irr", "M_irr"),
            ("j_spin", "J"),
            ("m_h", "M_H"),
        ]:
            d = rel_drift(last[key], ref[key])
            print(f"  {label:10s} {fmt(ref[key])} {fmt(last[key])} {100.0*d: .4f}%")
            if math.isfinite(d) and abs(d) > args.drift_tol:
                failures.append(
                    f"{label} drift {100.0*d:+.4f}% exceeds {100.0*args.drift_tol:.2f}%"
                )

        # Where did the first >tolerance crossing occur?
        for key, label in [("area", "area"), ("m_irr", "M_irr")]:
            crossing = next(
                (r for r in found_window
                 if math.isfinite(rel_drift(r[key], ref[key]))
                 and abs(rel_drift(r[key], ref[key])) > args.drift_tol),
                None,
            )
            if crossing is not None:
                print(f"  first {label} drift > {100.0*args.drift_tol:.2f}% at "
                      f"t/M={crossing['time']:.6f}")

    # Diagnostics checks, restricted to the target interval.
    window = [r for r in diag if r[1] <= args.target_t + 1.0e-12]
    if not window:
        window = diag
    amin = min(r[3] for r in window)
    phimin = min(r[5] for r in window)
    hmax = max(r[7] for r in window)
    nan_count = sum(r[9] for r in window)
    print("\nevolution diagnostics through target:")
    print(f"  min alpha       {amin: .6e}")
    print(f"  min phi         {phimin: .6e}")
    print(f"  max |H|         {hmax: .6e}")
    print(f"  metric NaN count {nan_count:.0f}")

    if amin <= 0.0:
        failures.append(f"lapse reached non-positive value {amin:.6e}")
    if nan_count > 0:
        failures.append(f"{nan_count:.0f} diagnostics rows report metric NaNs")
    if phimin < -100.0:
        failures.append(f"conformal factor reached implausible value {phimin:.6e}")
    if args.max_h is not None and hmax > args.max_h:
        failures.append(f"max|H|={hmax:.6e} exceeds {args.max_h:.6e}")
    if args.max_h is None:
        warnings.append("no --max-h supplied; constraint magnitude was reported only")

    for warning in warnings:
        print(f"WARN: {warning}")
    if failures:
        print("\nFAIL")
        for failure in failures:
            print(f"  - {failure}")
        return 1

    print("\nPASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())
