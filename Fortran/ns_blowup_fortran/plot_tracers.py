#!/usr/bin/env python3
"""
plot_tracers.py [run_dir] [h]  ->  tracers.png
Passive tracer particles advected by the toy collapsing vortex (from tracers.bin).
 (a) meridional (r,z) paths     (b) the same in similarity variables (r/sqrt(tau), z/tau^(1/2-h))
 (c) 3D paths                   (d) accumulated azimuthal turns  theta/2pi  vs time
Needs numpy + matplotlib.
"""
import sys
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

run = sys.argv[1] if len(sys.argv) > 1 else '.'
H = float(sys.argv[2]) if len(sys.argv) > 2 else 0.01


def load_tracers(path):
    with open(path, 'rb') as f:
        n = int(np.fromfile(f, np.int32, 1)[0])
        raw = np.fromfile(f, np.float64)
    nrec = raw.size // (1 + 4 * n)
    raw = raw[:nrec * (1 + 4 * n)].reshape(nrec, 1 + 4 * n)
    t = raw[:, 0]
    a = raw[:, 1:].reshape(nrec, n, 4)           # (record, particle, [r,z,theta,alive])
    return t, a[..., 0], a[..., 1], a[..., 2], a[..., 3]


t, r, z, th, alive = load_tracers(f'{run}/tracers.bin')
tau = 1 - t
n = r.shape[1]
r0 = r[0]
print(f'{n} tracers, {len(t)} records, t = {t[0]:.3f} .. {t[-1]:.4f}')
froze = alive[-1] < 0.5
print(f'frozen (left the domain edge window) by the end: {froze.sum()} / {n}')
# which tracers stay in the core?  (similarity radius at the end)
rho_end = r[-1] / np.sqrt(tau[-1])
print(f'similarity radius at the end: min {rho_end.min():.2f}  median {np.median(rho_end):.2f}  max {rho_end.max():.2f}')
print(f'turns theta/2pi accumulated: median {np.median((th[-1]-th[0])/2/np.pi):.1f}, '
      f'max {np.max((th[-1]-th[0])/2/np.pi):.1f}')

cmap = plt.get_cmap('plasma')
c = cmap((r0 - r0.min()) / (r0.max() - r0.min() + 1e-12) * 0.9)

fig = plt.figure(figsize=(14, 10), layout='constrained')
a = fig.add_subplot(2, 2, 1)
for k in range(n):
    a.plot(r[:, k], z[:, k], color=c[k], lw=.6)
    a.plot(r[0, k], z[0, k], '.', color=c[k], ms=3)
a.set_xlim(0, 1.3); a.set_ylim(-1.1, 1.1)
a.set_xlabel('r'); a.set_ylabel('z')
a.set_title(f'meridional paths, t = {t[0]:.2f} to {t[-1]:.3f} (dots = release)', fontsize=10)

a = fig.add_subplot(2, 2, 2)
for k in range(n):
    a.plot(r[:, k] / np.sqrt(tau), z[:, k] / tau**(0.5 - H), color=c[k], lw=.6)
a.set_xlim(0, 4); a.set_ylim(-4, 4)
a.set_xlabel(r'$r/\tau^{1/2}$'); a.set_ylabel(r'$z/\tau^{1/2-h}$')
a.set_title('same paths in similarity variables', fontsize=10)

ax3 = fig.add_subplot(2, 2, 3, projection='3d')
for k in range(0, n, 2):
    ax3.plot(r[:, k] * np.cos(th[:, k]), r[:, k] * np.sin(th[:, k]), z[:, k], color=c[k], lw=.5)
ax3.set_xlim(-1.1, 1.1); ax3.set_ylim(-1.1, 1.1); ax3.set_zlim(-1.1, 1.1)
ax3.set_box_aspect((1, 1, 1))
ax3.set_xlabel('x'); ax3.set_ylabel('y'); ax3.set_zlabel('z')
ax3.set_title('3D paths (every second tracer)', fontsize=10)

a = fig.add_subplot(2, 2, 4)
for k in range(n):
    a.plot(t, (th[:, k] - th[0, k]) / 2 / np.pi, color=c[k], lw=.6)
a.set_xlabel('t'); a.set_ylabel(r'turns around the axis, $\Delta\theta/2\pi$')
a.set_title('azimuthal turns accumulated', fontsize=10)

sm = plt.cm.ScalarMappable(cmap=cmap, norm=plt.Normalize(r0.min(), r0.max()))
fig.colorbar(sm, ax=fig.axes, shrink=.5, pad=.03, location='right', label='release radius')
fig.suptitle('Tracer particles in the toy blow-up flow', fontsize=11)
fig.savefig(f'{run}/tracers.png', dpi=130)
print(f'wrote {run}/tracers.png')
