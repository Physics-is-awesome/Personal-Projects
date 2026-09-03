#!/usr/bin/env python3
"""
Build solar_system.pvd from frames_manifest.csv, written incrementally by
the Fortran program during a run. Decoupled from the simulation itself so
it works regardless of how many frames a given run ends up producing
(main.F90's run length is wall-clock-budget-driven, not a fixed step
count, so the total frame count isn't known in advance).

Usage:
    python3 make_pvd.py
    (run any time after the simulation has produced at least one frame --
    safe to run while the simulation is still going, to preview progress)
"""
import csv

rows = []
with open("frames_manifest.csv") as f:
    reader = csv.DictReader(f)
    for row in reader:
        rows.append((float(row["sim_time_years"]), row["filename"]))

with open("solar_system.pvd", "w") as f:
    f.write('<?xml version="1.0"?>\n')
    f.write('<VTKFile type="Collection" version="0.1">\n')
    f.write('  <Collection>\n')
    for t, fname in rows:
        f.write(f'    <DataSet timestep="{t:.6f}" file="{fname}"/>\n')
    f.write('  </Collection>\n')
    f.write('</VTKFile>\n')

print(f"wrote solar_system.pvd with {len(rows)} frames")
