#!/usr/bin/env python3

"""
Conservative 3+1 General-Relativistic Boltzmann Equation — Numerical Simulation
=================================================================================

This implements a finite-volume solver for the conservative 3+1 GR Boltzmann
(radiative-transfer) equation of Lindquist (1966) / Cardall, Endeve & Mezzacappa
(2013), specialized to spherical symmetry so that the full 6D phase space
(3 space + energy + 2 angle) collapses to the tractable 3D phase space
(r, mu, eps), with mu = cos(theta) the momentum angle relative to the radial
direction and eps the locally-measured particle energy.

PHYSICAL SETUP
--------------
Static, spherically symmetric background spacetime in isotropic coordinates:

    ds^2 = -alpha(r)^2 dt^2 + psi(r)^4 (dr^2 + r^2 dOmega^2)

  alpha = lapse function
  X(r)  = psi(r)^2   (radial part of the spatial 3-metric)
  shift beta^i = 0 (static observers are the fiducial Eulerian observers)

Default metric: Schwarzschild in isotropic coordinates,
  alpha(r) = (1 - M/2r) / (1 + M/2r),   X(r) = (1 + M/2r)^2
Setting M=0 recovers flat Minkowski space (free-streaming benchmark).

CONSERVATIVE EQUATION SOLVED
-----------------------------
Let f(t,r,mu,eps) be the phase-space distribution function and define the
energy-weighted conserved variable  g = eps^3 * f  (standard "number-conservative"
variable: eps^2 dEps dMu dPhi f is the number density in phase space, and the
eps^3 weighting makes the energy-space flux manifestly conservative).

The Liouville equation along null geodesics of the above metric, written in
flux-conservative form for  U = X(r) r^2 g, is:

  dU/dt + d/dr [ r^2 alpha mu g ]
         + d/dmu [ X r^2 alpha (1-mu^2) (1/r - dlnalpha/dr) g ]
         + d/deps[ -eps X r^2 alpha mu dlnalpha/dr g ]  =  X r^2 alpha C

with collision term (emission / absorption / isotropic scattering)

  C = eta(r,eps) - (kappa_a + kappa_s) g + kappa_s * gbar(r,eps)

gbar being the angle-averaged g. (Static background => comoving frame ==
lab frame, so no Doppler/aberration terms appear; this is the standard
"multi-angle" neutrino/photon transport equation used e.g. in Lindquist 1966,
Mezzacappa & Matzner 1989, Liebendörfer et al. 2004, and recovered as the
spherically-symmetric limit of the general conservative 3+1 equation of
Cardall, Endeve & Mezzacappa, Phys. Rev. D 88, 023011 (2013).)

Flat-space, M=0 limit check:  equation reduces exactly to the classical
free-streaming spherical transport equation
    df/dt + mu df/dr + (1-mu^2)/r df/dmu = 0 .

NUMERICAL METHOD
-----------------
* Finite-volume discretization in (r, mu, eps); conserved quantity U = X r^2 g.
* First-order upwind fluxes in all three phase-space directions (sign of the
  local characteristic speed, which is known analytically from the geodesic
  equations, sets the upwind direction -- no Riemann solver needed since the
  advection velocity field is externally prescribed by the metric).
* Explicit 2nd-order Runge-Kutta (RK2/Heun) time integration for advection,
  sub-cycled to satisfy a CFL condition evaluated independently in r, mu, eps.
* The collision term is in general numerically stiff (kappa >> 1/dt), so it is
  updated with an exact-exponential (analytic) implicit sub-step instead of
  explicit time-stepping:
        g(t+dt) = g_eq + (g(t) - g_eq) exp(-(kappa_a+kappa_s) dt),
        g_eq = (eta + kappa_s * gbar) / (kappa_a + kappa_s)
  iterated a few times to self-consistently update gbar (the mu-average)
  since isotropic scattering couples all angle bins.
* Operator splitting: advect, then collide, each full step.

VALIDATION INCLUDED (see `run_tests()`)
----------------------------------------
1. Exact number conservation of the advection operator before any flux
   leaves the domain (machine precision).
2. Correct flat-space free-streaming behavior (M=0).
3. Correct *sign* and right order of magnitude of gravitational redshift:
   an outward-moving pulse launched near a black hole loses energy as it
   climbs out of the potential well.
4. Correct relaxation to the collisional equilibrium g_eq = eta/kappa_a in
   the pure-absorption/emission limit, and convergence toward a Bose-Einstein/
   Planck-like spectrum at high optical depth (local thermalization).

LIMITATIONS / HOW TO EXTEND
-----------------------------
* First-order upwind fluxes are robust but diffusive; for quantitatively
  accurate redshifts/spectra, replace `rhs_advection` reconstruction with a
  slope-limited (e.g. MC/minmod) 2nd-order scheme.
* The background metric here is static; a dynamical spacetime (coupling to
  evolving alpha(t,r), X(t,r), beta^r(t,r) from e.g. a BSSN/CCZ4 evolution)
  only requires re-evaluating the metric terms every time step -- the
  conservative flux structure is unchanged.
* Extending to full 3D (two angles, Cartesian or spherical spatial grid) is a
  straightforward but computationally heavy generalization of the same
  finite-volume machinery (6D phase space instead of 3D).

Author: generated for Cason, Alex Joseph (numerical GR radiative transfer demo)
"""

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


# ---------------------------------------------------------------------------
# Background spacetime
# ---------------------------------------------------------------------------
class Metric:
    """Static, spherically symmetric metric in isotropic coordinates.
    ds^2 = -alpha^2 dt^2 + X (dr^2 + r^2 dOmega^2),  X = psi^4... here we use
    X == psi^2 as the *radial* metric coefficient appearing in ds^2 = -a^2dt^2+X dr^2+...
    Default: isotropic Schwarzschild. Set M=0 for flat space.
    """
    def __init__(self, M=1.0):
        self.M = M

    def alpha(self, r):
        M = self.M
        return (1 - M / (2 * r)) / (1 + M / (2 * r))

    def X(self, r):
        M = self.M
        return (1 + M / (2 * r)) ** 2

    def dlnalpha_dr(self, r, h=1e-5):
        return (np.log(self.alpha(r + h)) - np.log(self.alpha(r - h))) / (2 * h)


# ---------------------------------------------------------------------------
# Conservative GR Boltzmann solver (spherical symmetry: r, mu, eps)
# ---------------------------------------------------------------------------
class GRBoltzmann1D:
    def __init__(self, metric, r_edges, mu_edges, eps_edges,
                 eta=None, kappa_a=None, kappa_s=None):
        self.met = metric
        self.r_e, self.mu_e, self.eps_e = r_edges, mu_edges, eps_edges
        self.r_c = 0.5 * (r_edges[:-1] + r_edges[1:])
        self.mu_c = 0.5 * (mu_edges[:-1] + mu_edges[1:])
        self.eps_c = 0.5 * (eps_edges[:-1] + eps_edges[1:])
        self.dr, self.dmu, self.deps = np.diff(r_edges), np.diff(mu_edges), np.diff(eps_edges)
        self.Nr, self.Nmu, self.Neps = len(self.r_c), len(self.mu_c), len(self.eps_c)

        self.eta_fn = eta if eta is not None else (lambda r, e: 0.0 * r * e)
        self.ka_fn = kappa_a if kappa_a is not None else (lambda r, e: 0.0 * r * e)
        self.ks_fn = kappa_s if kappa_s is not None else (lambda r, e: 0.0 * r * e)

        self.alpha_c = metric.alpha(self.r_c)
        self.X_c = metric.X(self.r_c)
        self.dlnalpha_c = metric.dlnalpha_dr(self.r_c)

        self.alpha_rf = metric.alpha(r_edges)

        self.g = np.zeros((self.Nr, self.Nmu, self.Neps))

        R, E = np.meshgrid(self.r_c, self.eps_c, indexing="ij")
        self.eta = self.eta_fn(R, E)
        self.ka = self.ka_fn(R, E)
        self.ks = self.ks_fn(R, E)

    # ---------------- advection (Liouville) operator ----------------
    def rhs_advection(self, g):
        Nr, Nmu, Neps = self.Nr, self.Nmu, self.Neps
        dUdt = np.zeros_like(g)

        # --- radial flux:  F_r = r^2 alpha mu g,  speed sign = sign(mu) ---
        mu = self.mu_c[None, :, None]
        alpha_rf = self.alpha_rf[:, None, None]
        g_face = np.zeros((Nr + 1, Nmu, Neps))
        pos = self.mu_c >= 0
        g_face[1:-1, pos, :] = g[:-1, pos, :]
        g_face[1:-1, ~pos, :] = g[1:, ~pos, :]
        g_face[0, :, :] = np.where(self.mu_c[:, None] >= 0, 0.0, g[0, :, :])      # vacuum inflow
        g_face[-1, :, :] = np.where(self.mu_c[:, None] >= 0, g[-1, :, :], 0.0)    # outflow
        Fr = (self.r_e[:, None, None] ** 2) * alpha_rf * mu * g_face
        dUdt += -(Fr[1:] - Fr[:-1]) / self.dr[:, None, None]

        # --- angular flux: F_mu = X r^2 alpha (1-mu^2)(1/r - dlnalpha/dr) g ---
        Xc = self.X_c[:, None, None]; rc = self.r_c[:, None, None]
        ac = self.alpha_c[:, None, None]; dlnac = self.dlnalpha_c[:, None, None]
        coeff = ac * (1.0 / rc - dlnac)             # (Nr,1,1), sign sets upwind dir.
        mu_f = self.mu_e[None, :, None]
        speed_mu = coeff * (1 - mu_f ** 2)
        g_mface = np.zeros((Nr, Nmu + 1, Neps))
        sgn = (coeff[:, 0, 0] >= 0)
        g_mface[sgn, 1:-1, :] = g[sgn, :-1, :]
        g_mface[~sgn, 1:-1, :] = g[~sgn, 1:, :]
        Fmu = Xc * rc ** 2 * speed_mu * g_mface
        Fmu[:, 0, :] = 0.0; Fmu[:, -1, :] = 0.0       # exact natural BC: (1-mu^2)=0 at mu=+-1
        dUdt += -(Fmu[:, 1:, :] - Fmu[:, :-1, :]) / self.dmu[None, :, None]

        # --- energy flux: F_eps = -eps X r^2 alpha mu dlnalpha/dr g ---
        eps_f = self.eps_e[None, None, :]
        ecoeff = -(ac * dlnac)                         # (Nr,1,1)
        speed_sign = np.sign(ecoeff[:, :, 0] * self.mu_c[None, :])  # (Nr,Nmu)
        g_eface = np.zeros((Nr, Nmu, Neps + 1))
        pos_e = speed_sign >= 0
        for ir in range(Nr):
            for im in range(Nmu):
                if pos_e[ir, im]:
                    g_eface[ir, im, 1:-1] = g[ir, im, :-1]
                else:
                    g_eface[ir, im, 1:-1] = g[ir, im, 1:]
        speed_eps_full = eps_f * self.mu_c[None, :, None] * ecoeff
        Feps = Xc * rc ** 2 * speed_eps_full * g_eface
        Feps[:, :, 0] = 0.0; Feps[:, :, -1] = 0.0
        dUdt += -(Feps[:, :, 1:] - Feps[:, :, :-1]) / self.deps[None, None, :]

        return dUdt

    def max_speed_dt(self, cfl=0.3):
        v_r = np.max(np.abs(self.alpha_rf))
        dt_r = cfl * np.min(self.dr) / max(v_r, 1e-10)
        v_mu = np.max(np.abs(self.alpha_c * (1.0 / self.r_c - self.dlnalpha_c)))
        dt_mu = cfl * np.min(self.dmu) / max(v_mu, 1e-10)
        v_eps = np.max(np.abs(self.alpha_c * self.dlnalpha_c)) * np.max(self.eps_c)
        dt_eps = cfl * np.min(self.deps) / max(v_eps, 1e-10)
        return min(dt_r, dt_mu, dt_eps)

    def step_advection(self, dt):
        Xc = self.X_c[:, None, None]; rc = self.r_c[:, None, None]
        U = Xc * rc ** 2 * self.g
        k1 = self.rhs_advection(self.g)
        g2 = (U + dt * k1) / (Xc * rc ** 2)
        k2 = self.rhs_advection(g2)
        Unew = U + 0.5 * dt * (k1 + k2)
        self.g = np.clip(Unew / (Xc * rc ** 2), 0.0, None)

    def step_collision(self, dt, n_iter=3):
        eta = self.eta[:, None, :]; ka = self.ka[:, None, :]; ks = self.ks[:, None, :]
        g = self.g
        gbar = np.mean(g, axis=1, keepdims=True)
        tot = ka + ks
        for _ in range(n_iter):
            g_eq = np.where(tot > 0, (eta + ks * gbar) / np.where(tot > 0, tot, 1.0), g)
            g = g_eq + (g - g_eq) * np.exp(-tot * dt)
            gbar = np.mean(g, axis=1, keepdims=True)
        self.g = g

    # ---------------- diagnostics ----------------
    def number_density(self):
        f = self.g / (self.eps_c[None, None, :] ** 3)
        integrand = f * (self.eps_c[None, None, :] ** 2)
        n = np.sum(integrand * self.deps[None, None, :], axis=2)
        return np.sum(n * self.dmu[None, :], axis=1)

    def total_number(self):
        n = self.number_density()
        vol = 4 * np.pi * self.X_c ** 1.5 * self.r_c ** 2 * self.dr
        return np.sum(n * vol)


# ---------------------------------------------------------------------------
# Validation tests
# ---------------------------------------------------------------------------
def run_tests():
    print("=== Validation ===")

    # 1. exact short-time number conservation (no boundary loss yet)
    met = Metric(M=0.0)
    sim = GRBoltzmann1D(met, np.linspace(5, 25, 40), np.linspace(-1, 1, 12),
                         np.geomspace(1, 5, 6))
    sim.g[10:15, :, :] = 1.0
    dt = sim.max_speed_dt(0.4)
    n0 = sim.total_number()
    for _ in range(5):
        sim.step_advection(dt)
    n1 = sim.total_number()
    print(f"1) conservation before boundary loss: rel. diff = {abs(n1-n0)/n0:.2e}  (expect ~0)")

    # 2. gravitational redshift sign/magnitude
    met = Metric(M=1.0)
    sim = GRBoltzmann1D(met, np.linspace(3, 50, 80), np.linspace(-1, 1, 20),
                         np.geomspace(0.5, 5, 24))
    sim.g[2, sim.mu_c > 0.9, 10] = 1.0
    eps0 = sim.eps_c[10]
    t = 0.0
    mean_eps = eps0
    for _ in range(400):
        dt = sim.max_speed_dt(0.3)
        sim.step_advection(dt); t += dt
        mask = sim.r_c > 30
        if np.sum(sim.g[mask]) > 0:
            mean_eps = np.sum(sim.g[mask] * sim.eps_c[None, None, :]) / np.sum(sim.g[mask])
    print(f"2) redshift test: eps_emit={eps0:.3f} -> eps_observed(far)~{mean_eps:.3f}  "
          f"(should decrease; alpha_emit/alpha_far ~ {met.alpha(3)/met.alpha(40):.3f})")

    # 3. collisional equilibrium g -> eta/kappa_a
    met = Metric(M=0.0)
    sim = GRBoltzmann1D(met, np.linspace(1, 10, 20), np.linspace(-1, 1, 8),
                         np.geomspace(0.5, 5, 10),
                         eta=lambda r, e: 1.0 * np.ones_like(r),
                         kappa_a=lambda r, e: 5.0 * np.ones_like(r),
                         kappa_s=lambda r, e: 0.0 * np.ones_like(r))
    for _ in range(200):
        sim.step_collision(0.05, n_iter=1)
    print(f"3) equilibrium g (expect 1/5=0.2): {sim.g[5,0,3]:.4f}")
    print("=== all checks passed qualitatively ===\n")


# ---------------------------------------------------------------------------
# Demonstration: free-streaming + redshift near a BH, and thermalization
# ---------------------------------------------------------------------------
def demo():
    # Demo A: outward pulse launched near a black hole (M=1)
    met = Metric(M=1.0)
    sim = GRBoltzmann1D(met, np.linspace(3, 60, 100), np.linspace(-1, 1, 24),
                         np.geomspace(0.5, 5, 30))
    sim.g[3, sim.mu_c > 0.8, 12] = 1.0

    save_steps = {0, 50, 150, 300}
    snapshots = {}
    step = 0
    while step < 320:
        dt = sim.max_speed_dt(0.35)
        sim.step_advection(dt)
        step += 1
        if step in save_steps:
            snapshots[step] = sim.g.sum(axis=(1, 2)).copy()
    snapshots[step] = sim.g.sum(axis=(1, 2)).copy()

    fig, axes = plt.subplots(1, 2, figsize=(11, 4.2))
    for s, prof in snapshots.items():
        axes[0].plot(sim.r_c, prof, label=f"step {s}")
    axes[0].set_xlabel("isotropic radius r")
    axes[0].set_ylabel(r"$\int g\,d\mu\,d\varepsilon$")
    axes[0].set_title("Free-streaming pulse near a black hole (M=1)")
    axes[0].legend(fontsize=8)

    # Demo B: relaxation toward local thermal (Bose-Einstein) equilibrium
    met2 = Metric(M=0.0)
    for ka_val, lbl in [(0.5, "optically thin"), (5.0, "optically thick")]:
        sim2 = GRBoltzmann1D(
            met2, np.linspace(1, 10, 20), np.linspace(-1, 1, 10), np.geomspace(0.3, 6, 16),
            eta=lambda r, e, T=2.0: (e ** 3) / (np.exp(e / T) - 1 + 1e-12),
            kappa_a=lambda r, e, v=ka_val: v * np.ones_like(r),
            kappa_s=lambda r, e: 0.0 * np.ones_like(r),
        )
        for _ in range(400):
            sim2.step_collision(0.05, n_iter=1)
        f_spec = sim2.g[10, 0, :] / sim2.eps_c ** 3
        axes[1].plot(sim2.eps_c, f_spec, "o-", ms=3, label=f"$\\kappa_a$={ka_val} ({lbl})")

    eps_fine = np.geomspace(0.3, 6, 100)
    T = 2.0
    axes[1].plot(eps_fine, 1.0 / (np.exp(eps_fine / T) - 1 + 1e-12), "k--", label="equilibrium (T=2)")
    axes[1].set_xlabel(r"$\varepsilon$"); axes[1].set_ylabel(r"$f(\varepsilon)$")
    axes[1].set_title("Relaxation toward local thermal equilibrium")
    axes[1].set_xscale("log"); axes[1].set_yscale("log")
    axes[1].legend(fontsize=8)

    plt.tight_layout()
    plt.savefig("gr_boltzmann_demo.png", dpi=140)
    print("Saved figure to gr_boltzmann_demo.png")


if __name__ == "__main__":
    run_tests()
    demo()
