#!/usr/bin/env python3
"""
plot_forceoff.py [base_dir] [off_dir ...]  ->  forceoff.png   (and forceoff_snaps.png if snapshots exist)

Compares the forced run (run_base) with runs where the force is smoothly removed at tau_off = 0.30, 0.10, 0.04
and the simulation continues past t = 1.  Needs numpy + matplotlib.
"""
import glob
import re
import sys
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

args = sys.argv[1:] or ['run_base', 'run_off_0.30', 'run_off_0.10', 'run_off_0.04']
base, offs = args[0], args[1:]
H = 0.01


def load(run):
    return np.loadtxt(f'{run}/history.dat')


def toff(run):
    m = re.search(r'in\.nml', '')
    for ln in open(f'{run}/in.nml'):
        mm = re.search(r'tau_off\s*=\s*([-0-9.eE+]+)', ln)
        if mm:
            return float(mm.group(1))
    return None


b = load(base)
colors = ['C1', 'C2', 'C3', 'C4', 'C5']
fig, ax = plt.subplots(2, 2, figsize=(13, 9))

# amplitude of the forced run's blow-up law  max|u_theta| = A tau^(-1/2-h), fitted on tau<0.15
m = (b[:, 1] < 0.15) & (b[:, 1] > 0.02 - 1e-9)
A = np.exp(np.mean(np.log(b[m, 2]) + (0.5 + H) * np.log(b[m, 1])))
tt = np.linspace(0.45, 0.9995, 400)

for k, (a, key, ttl) in enumerate(((ax[0, 0], 2, r'peak swirl $\max|u_\theta|$'),
                                   (ax[0, 1], 3, r'peak axial speed $\max|u_z|$'))):
    a.semilogy(b[:, 0], b[:, key], 'C0', lw=2, label='force kept on (to $\\tau=0.02$)')
    if key == 2:
        a.semilogy(tt, A * (1 - tt)**(-0.5 - H), 'k:', lw=1, label=r'$A\,\tau^{-1/2-h}$ continued to $t=1$')
    for c, r in zip(colors, offs):
        d = load(r)
        to = toff(r)
        a.semilogy(d[:, 0], np.maximum(d[:, key], 1e-6), c, label=f'force off at $\\tau={to:.2f}$')
        a.axvline(1 - to, color=c, lw=.6, ls='--')
    a.axvline(1.0, color='k', lw=.8)
    a.set_xlim(0.3, 1.3); a.set_xlabel('t'); a.set_title(ttl, fontsize=10)
    if k == 0:
        a.legend(fontsize=8, loc='lower left')
        a.set_ylim(1e-2, 3e2)
    else:
        a.set_ylim(1e-3, 3e2)

a = ax[1, 0]
a.semilogy(b[:, 0], b[:, 7], 'C0', lw=2, label='forced')
for c, r in zip(colors, offs):
    d = load(r)
    a.semilogy(d[:, 0], d[:, 7], c)
    a.axvline(1 - toff(r), color=c, lw=.6, ls='--')
a.axvline(1.0, color='k', lw=.8)
a.set_xlim(0.3, 1.3); a.set_ylim(1e-4, 1e2); a.set_xlabel('t'); a.set_title('kinetic energy', fontsize=10)

a = ax[1, 1]
a.plot(b[:, 0], b[:, 5], 'C0', lw=2, label='forced')
for c, r in zip(colors, offs):
    d = load(r)
    a.plot(d[:, 0], d[:, 5], c)
    a.axvline(1 - toff(r), color=c, lw=.6, ls='--')
a.axvline(1.0, color='k', lw=.8)
a.set_xlim(0.3, 1.3); a.set_ylim(0, 1.6); a.set_xlabel('t')
a.set_title(r'core radius $\ell_r$ (swirl-weighted rms): collapse vs viscous spreading', fontsize=10)

fig.suptitle('What happens when the force is removed (dashed lines = removal time, black line = t = 1)', fontsize=11)
fig.tight_layout()
fig.savefig('forceoff.png', dpi=130)

# summary numbers
print('wrote forceoff.png;  fitted A =', round(A, 3))
for r in offs:
    d = load(r)
    to = toff(r)
    i = np.argmax(d[:, 2])
    j = np.argmin(abs(d[:, 1] - to))
    print(f'tau_off={to:.2f}: peak max|u_th| = {d[i, 2]:.2f} at t={d[i, 0]:.3f} (value at removal {d[j, 2]:.2f});'
          f' at t=1: {d[np.argmin(abs(d[:, 0] - 1.0)), 2]:.3f}; at t={d[-1, 0]:.2f}: {d[-1, 2]:.4f};'
          f' energy at removal {d[j, 7]:.2f} -> end {d[-1, 7]:.4f}')
