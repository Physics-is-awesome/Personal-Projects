#!/usr/bin/env python3

"""
Demo script for the variational RMHD integrator (Kraus, Tassi & Grasso,
arXiv:1511.09314v2).

Runs two of the paper's numerical experiments at modest resolution:

  1. Orszag-Tang vortex (Section 4.1): 64x64, ht=0.01, to t~0.8.
     Produces a conservation-error plot (analogous to the paper's
     Figure 4) and a current-density snapshot (analogous to Figure 3).

  2. Current sheet (Section 4.2 / 4.3): 128x64, ht=0.01, run once with
     de=0 (ideal -- ce should show NO reconnection: psi contour
     topology preserved) and once with de=0.2 (electron inertia --
     reconnection / island formation should occur), each for a modest
     number of steps. Produces psi-contour snapshots for both cases
     side by side, and a conservation-error plot for the de=0.2 run
     (which is the more demanding one, matching the paper's Figure 10).

Resolution and run length are kept modest here so the whole script
finishes in a couple of minutes on a laptop; see the note in
integrator.py about the (unimplemented) Appendix-A preconditioner if
you want to push to the paper's full 1024x512 current-sheet resolution.

Output: PNG files written to the current directory (or OUTPUT_DIR if
you edit it below).
"""

import os
import numpy as np
import matplotlib.pyplot as plt

from integrator import RMHDIntegrator
from initial_conditions import make_grid, orszag_tang_vortex, current_sheet

OUTPUT_DIR = "."


def plot_conservation(hist, title, fname):
    t = np.array(hist["t"])
    E = np.array(hist["E"])
    CMH = np.array(hist["C_MH"])
    CL2 = np.array(hist["C_L2"])
    CCH = np.array(hist["C_CH"])

    E0, CL20, CCH0 = E[0], CL2[0], CCH[0]

    def rel(x, x0):
        return (x - x0) / x0 if x0 != 0 else (x - x0)

    fig, axes = plt.subplots(4, 1, figsize=(7, 9), sharex=True)
    axes[0].plot(t, rel(E, E0))
    axes[0].set_ylabel(r"$(E(t)-E(0))/E(0)$")
    axes[1].plot(t, CMH)
    axes[1].set_ylabel(r"$C_{MH}(t)$")
    axes[2].plot(t, rel(CL2, CL20))
    axes[2].set_ylabel(r"$(C_{L^2}(t)-C_{L^2}(0))/C_{L^2}(0)$")
    axes[3].plot(t, rel(CCH, CCH0) if CCH0 != 0 else CCH)
    axes[3].set_ylabel(r"$C_{CH}(t)-C_{CH}(0)$" if CCH0 == 0
                        else r"$(C_{CH}(t)-C_{CH}(0))/C_{CH}(0)$")
    axes[3].set_xlabel("t")
    fig.suptitle(title)
    fig.tight_layout()
    path = os.path.join(OUTPUT_DIR, fname)
    fig.savefig(path, dpi=140)
    plt.close(fig)
    print(f"wrote {path}")


def run_orszag_tang():
    print("\n=== Orszag-Tang vortex (Section 4.1) ===")
    nx, ny = 64, 64
    X, Y, hx, hy = make_grid(nx, ny, 0, 2 * np.pi, 0, 2 * np.pi)
    phi0, psi0 = orszag_tang_vortex(X, Y)

    integ = RMHDIntegrator(nx, ny, 2 * np.pi, 2 * np.pi, ht=0.01, de=0.0)
    integ.initialize(phi0, psi0)

    n_steps = 80  # t = 0.8, matching the paper's under-resolution cutoff
    hist = integ.run(n_steps, diagnostics_every=1, progress_every=20)

    plot_conservation(
        hist,
        "Orszag-Tang vortex: conservation errors (cf. paper Fig. 4)",
        "orszag_tang_conservation.png",
    )

    # snapshot of current density j, analogous to paper's Figure 3
    fig, ax = plt.subplots(figsize=(5, 4.5))
    im = ax.imshow(integ.j.T, origin="lower",
                    extent=[0, 2 * np.pi, 0, 2 * np.pi], cmap="viridis")
    ax.set_title(f"Current density j at t={integ.t:.2f}")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    fig.colorbar(im, ax=ax)
    fig.tight_layout()
    path = os.path.join(OUTPUT_DIR, "orszag_tang_current_density.png")
    fig.savefig(path, dpi=140)
    plt.close(fig)
    print(f"wrote {path}")

    E0 = hist["E"][0]
    print(f"final relative energy error: {(hist['E'][-1]-E0)/E0:.3e}")
    print(f"Newton iterations per step (min/median/max): "
          f"{np.min(hist['newton_iters'][1:])}/"
          f"{np.median(hist['newton_iters'][1:])}/"
          f"{np.max(hist['newton_iters'][1:])}")


def run_current_sheet():
    print("\n=== Current sheet: ideal vs. electron-inertia (Section 4.2/4.3) ===")
    nx, ny = 128, 64
    X, Y, hx, hy = make_grid(nx, ny, -np.pi, np.pi, -np.pi, np.pi)
    phi0, psi0 = current_sheet(X, Y, psi0=1.29, phi0_amp=1e-3, n_modes=22)

    n_steps = 400  # t = 4.0; modest compared to the paper's t up to 100

    results = {}
    for label, de in [("ideal (de=0)", 0.0), ("electron inertia (de=0.2)", 0.2)]:
        print(f"\n-- running {label} --")
        integ = RMHDIntegrator(nx, ny, 2 * np.pi, 2 * np.pi, ht=0.01, de=de)
        integ.initialize(phi0, psi0)
        hist = integ.run(n_steps, diagnostics_every=5, progress_every=100)
        results[label] = (integ, hist)
        E0 = hist["E"][0]
        print(f"  final relative energy error: {(hist['E'][-1]-E0)/E0:.3e}")

    # side-by-side psi contour comparison
    fig, axes = plt.subplots(1, 2, figsize=(10, 4.5))
    for ax, (label, (integ, hist)) in zip(axes, results.items()):
        ax.contour(integ.X.T, integ.Y.T, integ.psi.T, levels=25, linewidths=0.7)
        ax.set_title(f"{label}\npsi at t={integ.t:.2f}")
        ax.set_xlabel("x")
        ax.set_ylabel("y")
        ax.set_aspect("equal")
    fig.tight_layout()
    path = os.path.join(OUTPUT_DIR, "current_sheet_psi_contours.png")
    fig.savefig(path, dpi=140)
    plt.close(fig)
    print(f"wrote {path}")

    # conservation plot for the more demanding (electron-inertia) run
    _, hist_inertia = results["electron inertia (de=0.2)"]
    plot_conservation(
        hist_inertia,
        "Current sheet with electron inertia: conservation errors (cf. Fig. 10)",
        "current_sheet_inertia_conservation.png",
    )


if __name__ == "__main__":
    run_orszag_tang()
    run_current_sheet()
    print("\nDone.")
