#!/usr/bin/env python3
"""compare_res.py -> resolution.png and printed numbers.
Compares runs at 80x160, 120x240, 160x320 (dirs c80_*, run_*, c160_*): force-off histories and tracer results."""
import os
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

RES = [('80x160', 'c80_'), ('120x240', None), ('160x320', 'c160_')]


def d(prefix, name):
    return f'run_{name}' if prefix is None else f'{prefix}{name}'


def hist(prefix, name):
    p = f'{d(prefix, name)}/history.dat'
    return np.loadtxt(p) if os.path.exists(p) else None


def tracers(prefix):
    p = f'{d(prefix, "base")}/tracers.bin'
    if not os.path.exists(p):
        return None
    with open(p, 'rb') as f:
        n = int(np.fromfile(f, np.int32, 1)[0]); raw = np.fromfile(f, np.float64)
    nrec = raw.size // (1 + 4 * n)
    raw = raw[:nrec * (1 + 4 * n)].reshape(nrec, 1 + 4 * n)
    return raw[:, 0], raw[:, 1:].reshape(nrec, n, 4)


col = ['C0', 'C1', 'C2']
fig, ax = plt.subplots(2, 3, figsize=(15, 8.5))
print('--- force-off runs ---')
for k, (name, to) in enumerate((('off_0.10', 0.10), ('off_0.04', 0.04))):
    for (lab, pre), c in zip(RES, col):
        h = hist(pre, name)
        if h is None:
            continue
        ax[k, 0].semilogy(h[:, 0], np.maximum(h[:, 2], 1e-6), c, label=lab)
        ax[k, 1].semilogy(h[:, 0], np.maximum(h[:, 3], 1e-6), c, label=lab)
        i = np.argmax(h[:, 2])
        g = lambda t, j: h[np.argmin(abs(h[:, 0] - t)), j]
        print(f'tau_off={to:.2f} {lab:8s}: peak|u_th| {h[i,2]:.3f} (t={h[i,0]:.3f})  |u_th|(t=1) {g(1.0,2):.4f}  '
              f'|u_th|(1.3) {g(1.3,2):.5f}  |u_z|(1.3) {g(1.3,3):.5f}  E(1.3) {g(1.3,7):.6f}  core(1.3) {g(1.3,5):.4f}')
    for j in (0, 1):
        ax[k, j].set_xlim(0.8, 1.3); ax[k, j].axvline(1 - to, color='gray', lw=.6, ls='--')
        ax[k, j].set_title(f'force off at tau={to:.2f}: ' + (r'$\max|u_\theta|$' if j == 0 else r'$\max|u_z|$'), fontsize=10)
        ax[k, j].legend(fontsize=8)
ax[0, 0].set_ylim(1e-2, 30); ax[1, 0].set_ylim(1e-2, 30)
ax[0, 1].set_ylim(1e-3, 30); ax[1, 1].set_ylim(1e-3, 30)

print('--- tracers (run_base-type runs) ---')
T = {lab: tracers(pre) for lab, pre in RES}
ref = T['160x320'] if T['160x320'] is not None else T['120x240']
reflab = '160x320' if T['160x320'] is not None else '120x240'
for (lab, pre), c in zip(RES, col):
    if T[lab] is None:
        continue
    t, a = T[lab]
    turns = (a[:, :, 2] - a[0, :, 2]) / 2 / np.pi
    eq = np.abs(a[0, :, 1]) < 1e-9
    ax[0, 2].plot(t, turns[:, eq][:, :7].T if False else turns[:, eq][:, 3], c, label=lab)   # release r ~ 0.6, 4th radius
    ax[1, 2].plot(t, turns[:, eq].max(axis=1), c, label=lab)
    print(f'{lab:8s}: equatorial turns at t={t[-1]:.2f}: min {turns[-1, eq].min():.2f} max {turns[-1, eq].max():.2f} '
          f'median {np.median(turns[-1, eq]):.2f};  rho_end (non-eq) median {np.median(a[-1, ~eq, 0] / np.sqrt(1 - t[-1])):.2f}')
    tr, ar = ref
    if lab != reflab and len(tr) == len(t):
        dpos = np.hypot(a[-1, :, 0] - ar[-1, :, 0], a[-1, :, 1] - ar[-1, :, 1])
        print(f'   vs {reflab}: final (r,z) difference: median {np.median(dpos):.4f}, 90th pct {np.percentile(dpos, 90):.4f},'
              f' max {dpos.max():.4f};  equatorial turns diff max {np.abs(turns[-1, eq] - ((ar[-1, :, 2] - ar[0, :, 2]) / 2 / np.pi)[eq]).max():.3f}')
ax[0, 2].set_title('turns of one equatorial tracer (release r=0.6)', fontsize=10)
ax[1, 2].set_title('max turns over equatorial tracers', fontsize=10)
for a_ in (ax[0, 2], ax[1, 2]):
    a_.legend(fontsize=8); a_.set_xlabel('t')
for a_ in ax[:, :2].ravel():
    a_.set_xlabel('t')
fig.suptitle('Resolution check: force-off decay and tracer results', fontsize=11)
fig.tight_layout()
fig.savefig('resolution.png', dpi=120)
print('wrote resolution.png')
