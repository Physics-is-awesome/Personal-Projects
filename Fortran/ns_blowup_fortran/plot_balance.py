#!/usr/bin/env python3
"""
plot_balance.py [run_dir] [h]  ->  balance.png

Size of every term of the physical theta-momentum equation (from balance.dat):
    d_t u_theta = NL + VISC + f_theta
The proof's flow has individually huge terms that cancel to leave a smooth force; this shows how the
toy flow of this code behaves.  Needs numpy + matplotlib.
"""
import sys
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.ticker import FixedLocator, FixedFormatter, NullLocator

run = sys.argv[1] if len(sys.argv) > 1 else '.'
H = float(sys.argv[2]) if len(sys.argv) > 2 else 0.01
d = np.loadtxt(f'{run}/balance.dat')
d = d[(d[:, 1] <= 0.5) & (d[:, 1] >= 0.02 - 1e-9)]          # after the force is fully on, up to tau_end
tau = d[:, 1]
names = ['dt_uth', 'nonlin', 'visc', 'force', 'int_sum', 'adv', 'str']
mx = {n: d[:, 2 + i] for i, n in enumerate(names)}
l2 = {n: d[:, 9 + i] for i, n in enumerate(names)}


def slope(y, tmax=0.15):
    m = tau < tmax
    return np.polyfit(np.log(tau[m]), np.log(y[m]), 1)[0]


TICKS = [0.5, 0.2, 0.1, 0.05, 0.02]


def fix(a):
    a.set_xlim(0.5, 0.02)
    a.xaxis.set_major_locator(FixedLocator(TICKS))
    a.xaxis.set_major_formatter(FixedFormatter([str(x) for x in TICKS]))
    a.xaxis.set_minor_locator(NullLocator())
    a.set_xlabel(r'$\tau = 1-t$')


lab = {'dt_uth': r'$|\partial_t u_\theta|$ (acceleration)', 'nonlin': r'$|(u\cdot\nabla u)_\theta|$ (nonlinear)',
       'visc': r'$|\nu(\Delta-r^{-2})u_\theta|$ (viscous)', 'force': r'$|f_\theta|$ (force)'}
col = {'dt_uth': 'C0', 'nonlin': 'C1', 'visc': 'C2', 'force': 'C3'}

fig, ax = plt.subplots(2, 2, figsize=(13, 9))

a = ax[0, 0]
for n in ('dt_uth', 'nonlin', 'visc', 'force'):
    a.loglog(tau, mx[n], col[n], lw=1.8 if n == 'force' else 1.2, label=f'{lab[n]}  slope {slope(mx[n]):+.2f}')
a.set_ylabel('max over the grid'); a.legend(fontsize=8)
a.set_title('terms of the theta-momentum equation (max-norm)', fontsize=10)

a = ax[0, 1]
for n in ('dt_uth', 'nonlin', 'visc'):
    a.semilogx(tau, mx[n] * tau**(1.5 + H), col[n], label=lab[n])
a.semilogx(tau, mx['force'] * tau**(1.5 + H), col['force'], lw=1.8, label=lab['force'])
a.set_ylabel(r'term $\times\ \tau^{3/2+h}$'); a.legend(fontsize=8)
a.set_title(r'compensated: flat = grows like $\tau^{-3/2-h}$ (the scaling of $\partial_t u_\theta$)', fontsize=10)

a = ax[1, 0]
rho_m = mx['force'] / np.maximum.reduce([mx['dt_uth'], mx['nonlin'], mx['visc']])
rho_2 = l2['force'] / np.maximum.reduce([l2['dt_uth'], l2['nonlin'], l2['visc']])
a.semilogx(tau, rho_m, 'C3', label='max-norm')
a.semilogx(tau, rho_2, 'C3--', label='$L^2$ norm')
a.axhline(1, color='gray', lw=.6)
a.set_ylim(0, max(1.1, 1.1 * rho_m.max()))
a.set_ylabel(r'$|f_\theta|\,/\,\max(|\partial_t u_\theta|,|NL|,|VISC|)$'); a.legend(fontsize=8)
a.set_title('is the force small compared with the individual terms?', fontsize=10)

a = ax[1, 1]
r_nl_m = mx['nonlin'] / np.maximum(mx['adv'], mx['str'])
r_nl_2 = l2['nonlin'] / np.maximum(l2['adv'], l2['str'])
r_sum_m = mx['int_sum'] / np.maximum(mx['nonlin'], mx['visc'])
a.semilogx(tau, r_nl_m, 'C1', label=r'$|NL|\,/\,\max(|{\rm adv}|,|{\rm geom}|)$  (max)')
a.semilogx(tau, r_nl_2, 'C1--', label=r'same, $L^2$')
a.semilogx(tau, r_sum_m, 'C4', label=r'$|NL+{\rm VISC}|\,/\,\max(|NL|,|{\rm VISC}|)$  (max)')
a.axhline(1, color='gray', lw=.6)
a.set_ylabel('cancellation ratio'); a.legend(fontsize=8)
a.set_title('cancellation among the other terms (ratio near 1 = no cancellation)', fontsize=10)

for a in ax.ravel():
    fix(a)
fig.suptitle('Term balance of the toy collapsing vortex (this code), 120x240 grid', fontsize=11)
fig.tight_layout()
fig.savefig(f'{run}/balance.png', dpi=130)

print(f'wrote {run}/balance.png')
print('slopes over tau<0.15:  ' + '  '.join(f'{n} {slope(mx[n]):+.3f}' for n in names))
for t in (0.5, 0.2, 0.1, 0.05, 0.02):
    i = np.argmin(abs(tau - t))
    print(f'tau={tau[i]:.3f}  force/terms max {rho_m[i]:.3f}  L2 {rho_2[i]:.3f} | NL/adv,geom {r_nl_m[i]:.3f}'
          f' | (NL+V)/(NL,V) {r_sum_m[i]:.3f}')
