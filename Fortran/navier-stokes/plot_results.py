#!/usr/bin/env python3
"""plot_results.py [run_dir]  ->  scaling.png, snapshots.png  (needs numpy + matplotlib)"""
import glob
import sys
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

run = sys.argv[1] if len(sys.argv) > 1 else '.'
H = float(sys.argv[2]) if len(sys.argv) > 2 else 0.01      # must match hh used in the run
d = np.loadtxt(f'{run}/history.dat')
d = d[d[:, 1] <= 0.5]                                       # after the force has fully switched on (t1=0.5)
tau = d[:, 1]
col = dict(umax_th=2, umax_z=3, umax_r=4, R=5, Z=6, E=7, fth=8, f2=9, eu=10, ep=11)


def slope(y, tmax=0.15):
    m = tau < tmax
    return np.polyfit(np.log(tau[m]), np.log(y[m]), 1)[0]


from matplotlib.ticker import FixedLocator, FixedFormatter, NullLocator
TICKS = [0.5, 0.2, 0.1, 0.05, 0.02]


def fixticks(a):
    a.set_xlim(0.5, 0.02)
    a.xaxis.set_major_locator(FixedLocator(TICKS))
    a.xaxis.set_major_formatter(FixedFormatter([str(x) for x in TICKS]))
    a.xaxis.set_minor_locator(NullLocator())
    a.set_xlabel(r'$\tau = 1-t$')


fig, ax = plt.subplots(2, 3, figsize=(15, 8.5))
a = ax[0, 0]
for key, lab, c in (('umax_th', r'$\max|u_\theta|$', 'C0'), ('umax_z', r'$\max|u_z|$', 'C1'),
                    ('umax_r', r'$\max|u_r|$', 'C2')):
    y = d[:, col[key]]
    a.loglog(tau, y, c, label=f'{lab}   slope {slope(y):+.3f}')
a.loglog(tau, d[0, 2] * (tau / tau[0])**(-0.5 - H), 'k--', lw=.8, label=r'$\tau^{-1/2-h}$')
a.set_ylabel('velocity'); a.legend(fontsize=8)
a.set_title('velocity growth (paper: $\\tau^{-1/2-h}$, $\\tau^{-1/2}$)')

a = ax[0, 1]
a.loglog(tau, d[:, col['R']], label=f'$\\ell_r$ (rms radius)  slope {slope(d[:, col["R"]]):+.3f}')
a.loglog(tau, d[:, col['Z']], label=f'$\\ell_z$ (rms height)  slope {slope(d[:, col["Z"]]):+.3f}')
a.legend(fontsize=8)
a.set_title(f'core scales (paper: 1/2 and 1/2-h, h={H})')

a = ax[0, 2]
a.loglog(tau, d[:, col['E']])

a.set_title(f'kinetic energy, slope {slope(d[:, col["E"]]):+.3f} (paper: 1/2-3h = {0.5 - 3 * H:+.2f})')

a = ax[1, 0]
a.loglog(tau, d[:, col['fth']], label=f'$\\max|f_\\theta|$ slope {slope(d[:, col["fth"]]):+.3f}')
a.loglog(tau, d[:, col['f2']], label=f'$\\max|(\\nabla\\times f)_\\theta|$ slope {slope(d[:, col["f2"]]):+.3f}')
a.legend(fontsize=8)
a.set_title(f'force of this toy construction: expected slopes {-1.5 - H:.2f}, {-2 - H:.2f}\n(singular; the real proof makes the force smooth)', fontsize=10)

a = ax[1, 1]
a.loglog(tau, d[:, col['eu']], label=r'rel. $L^2$ error $u_\theta$')
a.loglog(tau, d[:, col['ep']], label=r'rel. $L^2$ error $\psi_1$')
a.legend(fontsize=8)
a.set_title('numerical vs exact manufactured solution')

a = ax[1, 2]
a.semilogx(tau, d[:, col['Z']] / d[:, col['R']])

a.set_title(r'aspect ratio $\ell_z/\ell_r \propto \tau^{-h}$')
for a in ax.ravel():
    fixticks(a)
fig.tight_layout()
fig.savefig(f'{run}/scaling.png', dpi=130)

# ------------------------------------------------------------------ snapshots
files = sorted(glob.glob(f'{run}/snap_*.dat'))
if files:
    n = len(files)
    fig, ax = plt.subplots(2, n, figsize=(2.9 * n, 6.6), squeeze=False)
    for k, f in enumerate(files):
        with open(f) as fh:
            tau_k = float(fh.readline().split('tau=')[1].split()[0])
        s = np.loadtxt(f)
        r, zz = s[:, 0], s[:, 1]
        nr = len(np.unique(r)); nz = len(np.unique(zz))
        R = r.reshape(nz, nr); Z = zz.reshape(nz, nr)
        uth = s[:, 3].reshape(nz, nr)
        ur_ = s[:, 2].reshape(nz, nr); uz_ = s[:, 4].reshape(nz, nr)
        # top: physical space, colour scale shared by all panels -> the vortex gets stronger and thinner
        a = ax[0, k]
        a.pcolormesh(R, Z, uth, shading='nearest', cmap='viridis', vmin=0, vmax=26)
        a.set_xlim(0, 1.0); a.set_ylim(-1.0, 1.0); a.set_aspect('equal')
        a.set_title(f'$\\tau$={tau_k:.3f}\nphysical $(r,z)$', fontsize=9)
        # bottom: similarity variables, velocities rescaled by their blow-up rates -> profile stays put
        a = ax[1, k]
        X, Y = R / np.sqrt(tau_k), Z / tau_k**(0.5 - H)
        a.pcolormesh(X, Y, uth * tau_k**(0.5 + H), shading='nearest', cmap='viridis', vmin=0, vmax=3.6)
        st = 3
        a.quiver(X[::st, ::st], Y[::st, ::st], (ur_ * np.sqrt(tau_k))[::st, ::st],
                 (uz_ * tau_k**(0.5 + H))[::st, ::st], color='w', width=0.006, scale=25, alpha=.9)
        a.set_xlim(0, 2.5); a.set_ylim(-2.5, 2.5); a.set_aspect('equal')
        a.set_title('$(r/\\tau^{1/2},\\,z/\\tau^{1/2-h})$', fontsize=9)
    fig.suptitle(r'Top: swirl $u_\theta$ in physical space (same colour scale). Bottom: $u_\theta\,\tau^{1/2+h}$ in '
                 'similarity variables with rescaled meridional flow (arrows): the shape stays fixed', fontsize=10)
    fig.tight_layout()
    fig.savefig(f'{run}/snapshots.png', dpi=110)
print('wrote', f'{run}/scaling.png', f'{run}/snapshots.png' if files else '')
