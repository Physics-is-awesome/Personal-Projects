#!/usr/bin/env python3
"""
Plot all efficiency/performance markers from performance_log.csv (written by
main.F90 during a WHCKL run).

Usage:
    python3 plot_efficiency.py [path/to/performance_log.csv]

Produces:
    efficiency_dashboard.png  -- one multi-panel figure with all markers
    (also displays interactively if run in an environment with a display)
"""
import sys
import csv
import matplotlib
matplotlib.use("Agg")   # safe for headless runs; drop this line if you want
                         # an interactive window and have a display available
import matplotlib.pyplot as plt

def load_log(path):
    step, sim_time, wall, cpu, steps_per_sec, dE = ([] for _ in range(6))

    with open(path) as f:
        reader = csv.DictReader(f)

        for row in reader:
            step.append(int(row["step"]))

            try:
                t = float(row["sim_time_years"])
            except (ValueError, OverflowError):
                t = sim_time[-1] + 45000.0

            sim_time.append(t)

            wall.append(float(row["wall_seconds"]))
            cpu.append(float(row["cpu_seconds"]))
            steps_per_sec.append(float(row["steps_per_sec"]))
            dE.append(float(row["dE_over_E0"]))

    return step, sim_time, wall, cpu, steps_per_sec, dE

def main():
    path = sys.argv[1] if len(sys.argv) > 1 else "performance_log.csv"
    step, sim_time, wall, cpu, steps_per_sec, dE = load_log(path)

    # Derived markers, computed from the raw columns
    cpu_utilization = [c / w if w > 0 else float("nan") for c, w in zip(cpu, wall)]
    sim_years_per_wall_second = [s / w if w > 0 else float("nan") for s, w in zip(sim_time, wall)]
    abs_dE = [abs(x) for x in dE]

    fig, axes = plt.subplots(3, 2, figsize=(13, 12))
    fig.suptitle(f"WHCKL efficiency markers  ({path})", fontsize=14)

    # 1. Throughput (steps/sec) over the run
    ax = axes[0, 0]
    ax.plot(wall, steps_per_sec, marker='o', markersize=3)
    ax.set_xlabel("wall-clock time (s)")
    ax.set_ylabel("steps / sec")
    ax.set_title("Throughput")
    ax.grid(True, alpha=0.3)

    # 2. CPU utilization (cpu_time / wall_time) over the run
    ax = axes[0, 1]
    ax.plot(wall, cpu_utilization, marker='o', markersize=3, color='tab:orange')
    ax.axhline(1.0, color='gray', linestyle='--', linewidth=1, label='ideal (=1.0)')
    ax.set_xlabel("wall-clock time (s)")
    ax.set_ylabel("CPU time / wall time")
    ax.set_title("CPU utilization")
    ax.set_ylim(0, 1.15)
    ax.legend()
    ax.grid(True, alpha=0.3)

    # 3. Wall-clock time vs CPU time (should sit on the diagonal if efficient)
    ax = axes[1, 0]
    ax.plot(wall, cpu, marker='o', markersize=3, color='tab:green', label='actual')
    lims = [0, max(wall)]
    ax.plot(lims, lims, color='gray', linestyle='--', linewidth=1, label='wall = cpu (ideal)')
    ax.set_xlabel("wall-clock time (s)")
    ax.set_ylabel("CPU time (s)")
    ax.set_title("Wall-clock vs CPU time")
    ax.legend()
    ax.grid(True, alpha=0.3)

    # 4. Simulated years per wall-clock second (time-compression ratio)
    ax = axes[1, 1]
    ax.plot(wall, sim_years_per_wall_second, marker='o', markersize=3, color='tab:purple')
    ax.set_xlabel("wall-clock time (s)")
    ax.set_ylabel("simulated years / wall-clock second")
    ax.set_title("Simulation speed (time compression)")
    ax.grid(True, alpha=0.3)

    # 5. Cumulative steps vs wall-clock time (linear = steady throughput)
    ax = axes[2, 0]
    ax.plot(wall, step, marker='o', markersize=3, color='tab:red')
    ax.set_xlabel("wall-clock time (s)")
    ax.set_ylabel("cumulative steps")
    ax.set_title("Steps completed over time")
    ax.grid(True, alpha=0.3)

    # 6. Energy conservation (accuracy, not raw speed, but the other key
    #    "is this run actually good" marker logged alongside performance)
    ax = axes[2, 1]
    ax.plot(sim_time, abs_dE, marker='o', markersize=3, color='tab:brown')
    ax.set_xlabel("simulated time (years)")
    ax.set_ylabel("|dE / E0|")
    ax.set_yscale('log')
    ax.set_title("Energy conservation")
    ax.grid(True, alpha=0.3)

    plt.tight_layout(rect=[0, 0, 1, 0.96])
    out_path = "efficiency_dashboard.png"
    plt.savefig(out_path, dpi=150)
    print(f"wrote {out_path}")

    # Print a short numeric summary too, not just the plot
    print()
    print("=== summary ===")
    print(f"rows loaded            : {len(step)}")
    print(f"final throughput       : {steps_per_sec[-1]:,.1f} steps/s")
    print(f"mean throughput        : {sum(steps_per_sec)/len(steps_per_sec):,.1f} steps/s")
    print(f"final CPU utilization  : {cpu_utilization[-1]:.4f}")
    print(f"final wall-clock time  : {wall[-1]:,.2f} s")
    print(f"final simulated time   : {sim_time[-1]:,.2f} years")
    print(f"final |dE/E0|          : {abs_dE[-1]:.4e}")
    print(f"max |dE/E0| seen       : {max(abs_dE):.4e}")


if __name__ == "__main__":
    main()
