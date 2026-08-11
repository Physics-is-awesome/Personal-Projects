#!/usr/bin/env python3

import numpy as np
from scipy.integrate import quad
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

c = 2.99792458e8
G = 6.67430e-11
Mpc = 3.0856775814913673e22
Or0 = 9.24e-5  # fixed; subdominant, negligible effect on final uncertainty

def compute_E(H0_kmsMpc, Om0):
    H0 = H0_kmsMpc*1000/Mpc
    Ode0 = 1.0 - Om0  # flat universe assumption
    def E_of_a(a):
        return np.sqrt(Or0*a**-4 + Om0*a**-3 + Ode0)
    val, _ = quad(lambda a: 1.0/(a**2*E_of_a(a)), 1e-9, 1.0, limit=200)
    R_obs = (c/H0)*val
    Lambda = 3*H0**2*Ode0/c**2
    M = Lambda*c**2*R_obs**3/(3*G)
    return M*c**2, R_obs, M

# ---- central (Planck 2018) values ----
H0_planck, sig_H0_planck = 67.36, 0.54
Om0_planck, sig_Om0_planck = 0.3153, 0.0073

E0, R0, M0 = compute_E(H0_planck, Om0_planck)
print(f"Central estimate (Planck 2018 best-fit): E = {E0:.4e} J\n")

# ---- Monte Carlo over Planck-internal parameter uncertainties ----
np.random.seed(42)
N = 8000
H0_samples = np.random.normal(H0_planck, sig_H0_planck, N)
Om0_samples = np.clip(np.random.normal(Om0_planck, sig_Om0_planck, N), 0.1, 0.6)

E_samples = np.array([compute_E(h, om)[0] for h, om in zip(H0_samples, Om0_samples)])

median = np.median(E_samples)
lo68, hi68 = np.percentile(E_samples, [16, 84])
lo95, hi95 = np.percentile(E_samples, [2.5, 97.5])

print("Monte Carlo over Planck-internal uncertainties (H0, Omega_m):")
print(f"  median E        = {median:.4e} J")
print(f"  68% CI          = [{lo68:.4e}, {hi68:.4e}] J")
print(f"  95% CI          = [{lo95:.4e}, {hi95:.4e}] J")
print(f"  relative spread (68%): +{100*(hi68-median)/median:.2f}% / -{100*(median-lo68)/median:.2f}%\n")

# ---- H0 tension: Planck vs SH0ES, systematic (not statistical) comparison ----
H0_shoes, sig_H0_shoes = 73.04, 1.04
E_planck, _, _ = compute_E(H0_planck, Om0_planck)
E_shoes, _, _  = compute_E(H0_shoes, Om0_planck)   # Om0 held fixed to isolate the H0 effect

print("H0 tension check (Omega_m held fixed at Planck value):")
print(f"  Planck  (H0={H0_planck} km/s/Mpc): E = {E_planck:.4e} J")
print(f"  SH0ES   (H0={H0_shoes} km/s/Mpc): E = {E_shoes:.4e} J")
print(f"  ratio SH0ES/Planck = {E_shoes/E_planck:.4f}  ({100*(E_shoes/E_planck-1):.1f}%)")

# ---- plot ----
fig, ax = plt.subplots(figsize=(8,5))
ax.hist(E_samples, bins=80, color='#4C72B0', edgecolor='white', linewidth=0.3)
ax.axvline(E0, color='k', linestyle='--', linewidth=1.2, label=f'Planck central: {E0:.2e} J')
ax.axvline(E_shoes, color='#C44E52', linestyle='--', linewidth=1.2, label=f'SH0ES H0 value: {E_shoes:.2e} J')
ax.set_xlabel('Energy needed (J)')
ax.set_ylabel('Monte Carlo samples')
ax.set_title('Distribution of required energy under Planck parameter uncertainties\n(8000 samples; H0 ~ N(67.36, 0.54), Ωm ~ N(0.3153, 0.0073))')
ax.legend(fontsize=9)
ax.ticklabel_format(axis='x', style='sci', scilimits=(0,0))
plt.tight_layout()
plt.savefig('/home/Pictures/mc_energy_distribution.png', dpi=140)
print("\nSaved plot.")
