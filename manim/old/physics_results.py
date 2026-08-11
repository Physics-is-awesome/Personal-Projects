"""
physics_results.py

Pure computation, no manim dependency, fully testable on its own.
Reproduces the derivations from the write-up: horizon integral, the static
Schwarzschild-de Sitter mass, the LTB curvature at the boundary shell, and a
Monte Carlo uncertainty pass over Planck-era parameters. The manim scenes
import from this module rather than hard-coding numbers, so the animation's
on-screen figures are always whatever this script actually computes.
"""

import numpy as np
from scipy.integrate import quad

# ---- constants (SI) ----
c = 2.99792458e8
G = 6.67430e-11
MPC = 3.0856775814913673e22
GYR = 3.15576e16  # seconds

# ---- Planck 2018 parameters ----
H0_KMS_MPC = 67.36
SIG_H0 = 0.54
OM0 = 0.3153
SIG_OM0 = 0.0073
ODE0 = 1.0 - OM0
OR0 = 9.24e-5

# ---- SH0ES value, for the tension comparison ----
H0_SHOES = 73.04


def _E(a, om0, ode0, or0=OR0):
    return np.sqrt(or0 * a**-4 + om0 * a**-3 + ode0)


def horizon_radius(H0_kms_mpc, om0, or0=OR0):
    """Comoving particle horizon (radius of the observable universe), meters."""
    H0 = H0_kms_mpc * 1000 / MPC
    ode0 = 1.0 - om0
    val, _ = quad(lambda a: 1.0 / (a**2 * _E(a, om0, ode0, or0)), 1e-9, 1.0, limit=400)
    return (c / H0) * val


def critical_mass(H0_kms_mpc, om0, R_obs=None, or0=OR0):
    """Static Schwarzschild-de Sitter mass: M such that R_obs is the balance radius."""
    H0 = H0_kms_mpc * 1000 / MPC
    ode0 = 1.0 - om0
    if R_obs is None:
        R_obs = horizon_radius(H0_kms_mpc, om0, or0)
    Lambda = 3 * H0**2 * ode0 / c**2
    M = Lambda * c**2 * R_obs**3 / (3 * G)
    return M, R_obs, Lambda


def background_matter_mass(om0, R_obs, H0_kms_mpc=H0_KMS_MPC):
    """Ordinary matter already inside the comoving patch of radius R_obs (time-independent)."""
    H0 = H0_kms_mpc * 1000 / MPC
    rho_crit0 = 3 * H0**2 / (8 * np.pi * G)
    return om0 * rho_crit0 * (4 / 3) * np.pi * R_obs**3


def ltb_curvature(M_crit, R_obs):
    """Exact closed form: E(r_obs) = -3GM/(2R_obs)."""
    return -1.5 * G * M_crit / R_obs


def f_of_R(R, M, E_curv, Lambda):
    return 2 * G * M / R + (Lambda * c**2 / 3) * R**2 + 2 * E_curv


def freeze_out_time(R_target, M, E_curv, Lambda):
    """t(R) = integral_0^R dR'/sqrt(f(R')), seconds."""
    integrand = lambda Rp: 1.0 / np.sqrt(f_of_R(Rp, M, E_curv, Lambda))
    val, _ = quad(integrand, 0, R_target, limit=400)
    return val


def monte_carlo(n=8000, seed=42):
    rng = np.random.default_rng(seed)
    H0s = rng.normal(H0_KMS_MPC, SIG_H0, n)
    Oms = np.clip(rng.normal(OM0, SIG_OM0, n), 0.1, 0.6)
    Es = np.empty(n)
    for i in range(n):
        M, R, _ = critical_mass(H0s[i], Oms[i])
        Es[i] = M * c**2
    return Es


def headline_numbers():
    """Everything the video needs, computed once."""
    M_crit, R_obs, Lambda = critical_mass(H0_KMS_MPC, OM0)
    E_total = M_crit * c**2
    M_bg = background_matter_mass(OM0, R_obs)
    E_added = (M_crit - M_bg) * c**2
    E_curv = ltb_curvature(M_crit, R_obs)

    M_shoes, R_shoes, _ = critical_mass(H0_SHOES, OM0)
    E_shoes = M_shoes * c**2

    Es_mc = monte_carlo()
    median = np.median(Es_mc)
    lo68, hi68 = np.percentile(Es_mc, [16, 84])

    t_90 = freeze_out_time(0.90 * R_obs, M_crit, E_curv, Lambda) / GYR
    t_99 = freeze_out_time(0.99 * R_obs, M_crit, E_curv, Lambda) / GYR

    return dict(
        R_obs_m=R_obs, R_obs_gly=R_obs / (c * GYR),
        M_crit=M_crit, E_total=E_total,
        M_bg=M_bg, E_added=E_added,
        E_curv=E_curv, Lambda=Lambda,
        E_shoes=E_shoes,
        E_median=median, E_lo68=lo68, E_hi68=hi68,
        t_90_gyr=t_90, t_99_gyr=t_99,
    )


if __name__ == "__main__":
    r = headline_numbers()
    for k, v in r.items():
        print(f"{k:12s} = {v:.5e}")
