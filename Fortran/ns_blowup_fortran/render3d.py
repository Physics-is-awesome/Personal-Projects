#!/usr/bin/env python3
"""
render3d.py [run_dir] [h] [--still-only] [--step K]
    -> render3d_stills.png   (3 times x 2 views)
    -> render3d.mp4          (all frames; use --step K to render every K-th frame)

3D isosurface of the swirl u_theta of the axisymmetric toy flow, revolved around the z-axis, with a quarter cut out
so the interior is visible.  Surface colour = axial velocity u_z (rescaled by its blow-up rate).  White comets = tracer
particles (tracers.bin, if present).
Top / left view : physical coordinates, fixed window.
Bottom / right  : similarity coordinates (x/sqrt(tau), z/tau^(1/2-h)), fixed window.
Input: frames.bin (+ tracers.bin) written by ns_axi_blowup.  Needs numpy, scipy, scikit-image, matplotlib, ffmpeg.
"""
import os
import subprocess
import sys
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
from matplotlib.colors import LightSource, Normalize
from scipy.ndimage import map_coordinates
from skimage.measure import marching_cubes

argv = [a for a in sys.argv[1:] if not a.startswith('--')]
run = argv[0] if argv else '.'
H = float(argv[1]) if len(argv) > 1 else 0.01
still_only = '--still-only' in sys.argv
step = 1
if '--step' in sys.argv:
    step = int(sys.argv[sys.argv.index('--step') + 1])

LEVEL = 0.45          # isosurface at this fraction of the frame's max u_theta
N = 64                # volume resolution per side
WIN_PHYS = 1.2        # physical window half-width (x,y,z)
WIN_SIM = 2.6         # similarity window half-width


# ------------------------------------------------------------------ data
def load_frames(path):
    with open(path, 'rb') as f:
        nr, nz = (int(v) for v in np.fromfile(f, np.int32, 2))
        rmax, zmax = (float(v) for v in np.fromfile(f, np.float64, 2))
        ts, fr = [], []
        while True:
            t = np.fromfile(f, np.float64, 1)
            if t.size == 0:
                break
            a = np.fromfile(f, np.float32, 4 * nr * nz)
            if a.size < 4 * nr * nz:
                break
            ts.append(t[0])
            fr.append(a.reshape(4, nz, nr))      # u_theta, u_r, u_z, omega_theta
    return nr, nz, rmax, zmax, np.array(ts), np.array(fr)


def load_tracers(path):
    if not os.path.exists(path):
        return None
    with open(path, 'rb') as f:
        n = int(np.fromfile(f, np.int32, 1)[0])
        raw = np.fromfile(f, np.float64)
    nrec = raw.size // (1 + 4 * n)
    raw = raw[:nrec * (1 + 4 * n)].reshape(nrec, 1 + 4 * n)
    return raw[:, 0], raw[:, 1:].reshape(nrec, n, 4)


nr, nz, rmax, zmax, T, F = load_frames(f'{run}/frames.bin')
TR = load_tracers(f'{run}/tracers.bin')
dr, dz = rmax / nr, 2 * zmax / nz
print(f'{len(T)} frames  t = {T[0]:.3f}..{T[-1]:.4f}   grid {nr}x{nz}   tracers: {None if TR is None else TR[1].shape[1]}')
# colour limit: u_z * tau^(1/2+h) (should be O(1) and roughly constant if the flow is self-similar)
tau_all = 1 - T
zs = np.array([np.abs(F[i, 2]).max() * tau_all[i]**(0.5 + H) for i in range(len(T))])
CLIM = float(np.percentile(zs, 90))
print(f'max|u_z| tau^(1/2+h): {zs.min():.2f} .. {zs.max():.2f}; colour limit {CLIM:.2f}')


def sample(field2d, X, Y, Z, sign_z=False):
    """revolve a (nz,nr) field to the Cartesian points; field2d[j,i] at r=(i+.5)dr, z=-zmax+(j+.5)dz"""
    R = np.sqrt(X**2 + Y**2)
    ii = R / dr - 0.5
    jj = (Z + zmax) / dz - 0.5
    return map_coordinates(field2d, [jj, ii], order=1, mode='nearest')


def surface(k, mode):
    """vertices (in plotted coordinates), faces, face colours (rescaled u_z), for frame k"""
    tau = 1 - T[k]
    sx = 1.0 if mode == 'phys' else 1 / np.sqrt(tau)         # plotted -> physical scale factors
    sz = 1.0 if mode == 'phys' else 1 / tau**(0.5 - H)
    W = WIN_PHYS if mode == 'phys' else WIN_SIM
    g = np.linspace(-W, W, N)
    Xp, Yp, Zp = np.meshgrid(g, g, g, indexing='ij')
    X, Y, Z = Xp / sx, Yp / sx, Zp / sz                      # physical points
    uth = sample(F[k, 0], X, Y, Z)
    lev = LEVEL * F[k, 0].max()
    # carve out the quarter x>0, y<0 so the interior is visible
    cut = (Xp > 0) & (Yp < 0)
    uth = np.where(cut, 0.0, uth)
    # pad so the surface is closed at the window edge
    uth = np.pad(uth, 1, constant_values=0.0)
    try:
        v, f, nrm, _ = marching_cubes(uth, lev, spacing=(g[1] - g[0],) * 3)
    except (ValueError, RuntimeError):
        return None
    v = v - (g[1] - g[0]) - W
    X, Y, Z = v[:, 0] / sx, v[:, 1] / sx, v[:, 2] / sz
    uz = sample(F[k, 2], X, Y, Z) * tau**(0.5 + H) / CLIM
    return v, f, uz


def draw(ax, k, mode, trail=25):
    tau = 1 - T[k]
    W = WIN_PHYS if mode == 'phys' else WIN_SIM
    out = surface(k, mode)
    if out is not None:
        v, f, uz = out
        tri = v[f]
        # Lambertian shading with a fixed light
        n = np.cross(tri[:, 1] - tri[:, 0], tri[:, 2] - tri[:, 0])
        n /= np.linalg.norm(n, axis=1, keepdims=True) + 1e-30
        light = np.array([0.4, -0.5, 0.75]); light /= np.linalg.norm(light)
        shade = 0.45 + 0.55 * np.abs(n @ light)
        cval = uz[f].mean(axis=1)
        rgba = plt.get_cmap('coolwarm')(Normalize(-1, 1)(np.clip(cval, -1, 1)))
        rgba[:, :3] *= shade[:, None]
        pc = Poly3DCollection(tri, facecolors=rgba, edgecolor='none', linewidth=0, antialiased=False)
        ax.add_collection3d(pc)
    if TR is not None:
        kk = int(np.argmin(abs(TR[0] - T[k])))
        a = TR[1]
        for p in range(a.shape[1]):
            k0 = max(0, kk - trail)
            r_, z_, th_ = a[k0:kk + 1, p, 0], a[k0:kk + 1, p, 1], a[k0:kk + 1, p, 2]
            sx = 1.0 if mode == 'phys' else 1 / np.sqrt(tau)
            sz = 1.0 if mode == 'phys' else 1 / tau**(0.5 - H)
            x, y, z = r_ * np.cos(th_) * sx, r_ * np.sin(th_) * sx, z_ * sz
            if abs(x[-1]) > W or abs(y[-1]) > W or abs(z[-1]) > W:
                continue
            ax.plot(x, y, z, color='k', lw=0.7, alpha=0.8)
            ax.plot(x[-1:], y[-1:], z[-1:], 'o', color='gold', mec='k', mew=0.3, ms=2)
    ax.set_xlim(-W, W); ax.set_ylim(-W, W); ax.set_zlim(-W, W)
    ax.set_box_aspect((1, 1, 1), zoom=1.55)
    ax.view_init(elev=20, azim=-55)
    ax.set_axis_off()
    # frame box
    for s in ((-W, W),):
        pass


def label(ax, k, mode):
    tau = 1 - T[k]
    if mode == 'phys':
        ax.text2D(0.02, 0.95, f'physical space, window ±{WIN_PHYS}\n$\\tau$={tau:.3f}   max$|u_\\theta|$={F[k,0].max():.1f}',
                  transform=ax.transAxes, fontsize=9, va='top', bbox=dict(fc='w', ec='none', alpha=.8))
    else:
        ax.text2D(0.02, 0.95, f'similarity coordinates $(x,y)/\\tau^{{1/2}}$, $z/\\tau^{{1/2-h}}$\nwindow ±{WIN_SIM}',
                  transform=ax.transAxes, fontsize=9, va='top', bbox=dict(fc='w', ec='none', alpha=.8))


# ------------------------------------------------------------------ stills
want = [0.7, 0.9, 0.97]
ks = [int(np.argmin(abs(T - t))) for t in want]
fig = plt.figure(figsize=(14, 9.2))
for c, k in enumerate(ks):
    for rrow, mode in enumerate(('phys', 'sim')):
        ax = fig.add_subplot(2, 3, 3 * rrow + c + 1, projection='3d')
        draw(ax, k, mode)
        label(ax, k, mode)
sm = plt.cm.ScalarMappable(cmap='coolwarm', norm=Normalize(-CLIM, CLIM))
fig.subplots_adjust(left=0, right=0.9, top=0.94, bottom=0, wspace=0, hspace=0)
cb = fig.colorbar(sm, ax=fig.axes, shrink=.5, pad=.01)
cb.set_label(r'axial velocity $u_z\,\tau^{1/2+h}$ on the surface')
fig.suptitle(r'Isosurface $u_\theta = 0.45\,\max u_\theta$ of the toy blow-up flow (quarter cut away)', fontsize=11)
fig.savefig(f'{run}/render3d_stills.png', dpi=110)
print(f'wrote {run}/render3d_stills.png')
plt.close(fig)
if still_only:
    sys.exit()

# ------------------------------------------------------------------ movie
fdir = f'{run}/_frames'
os.makedirs(fdir, exist_ok=True)
idx = list(range(0, len(T), step))
if idx[-1] != len(T) - 1:
    idx.append(len(T) - 1)
fig = plt.figure(figsize=(12.8, 7.2), dpi=100)
for n, k in enumerate(idx):
    fig.clf()
    a1 = fig.add_subplot(1, 2, 1, projection='3d')
    a2 = fig.add_subplot(1, 2, 2, projection='3d')
    draw(a1, k, 'phys'); label(a1, k, 'phys')
    draw(a2, k, 'sim'); label(a2, k, 'sim')
    fig.subplots_adjust(left=0, right=1, bottom=0, top=1, wspace=0)
    fig.text(0.5, 0.04, 'toy manufactured flow with the paper\'s scalings, not the proof\'s solution; colour = $u_z$',
             ha='center', fontsize=8)
    fig.savefig(f'{fdir}/f_{n:04d}.png')
    if n % 10 == 0:
        print('frame', n, '/', len(idx), flush=True)
subprocess.run(['ffmpeg', '-y', '-loglevel', 'error', '-framerate', '24', '-i', f'{fdir}/f_%04d.png',
                '-c:v', 'libx264', '-pix_fmt', 'yuv420p', '-crf', '20', f'{run}/render3d.mp4'], check=True)
print(f'wrote {run}/render3d.mp4')
