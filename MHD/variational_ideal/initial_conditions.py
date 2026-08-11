#!/usr/bin/env python3

"""
Initial conditions for the test problems of Section 4 of the paper.
"""

import numpy as np


def make_grid(nx, ny, x0, x1, y0, y1):
    """Grid of nx*ny points covering [x0,x1) x [y0,y1) with periodic
    spacing hx = (x1-x0)/nx, hy = (y1-y0)/ny (endpoint excluded, as is
    standard for periodic domains)."""
    hx = (x1 - x0) / nx
    hy = (y1 - y0) / ny
    x = x0 + np.arange(nx) * hx
    y = y0 + np.arange(ny) * hy
    X, Y = np.meshgrid(x, y, indexing="ij")
    return X, Y, hx, hy


def orszag_tang_vortex(X, Y):
    """
    Section 4.1: domain [0,2pi) x [0,2pi), 64x64, ht=0.01 in the paper.

        phi = 2 cos(x) - 2 sin(y)
        psi = 2 cos(x) - cos(2y)
    """
    phi0 = 2.0 * np.cos(X) - 2.0 * np.sin(Y)
    psi0 = 2.0 * np.cos(X) - np.cos(2.0 * Y)
    return phi0, psi0


def _truncate_fourier_x(profile_1d, n_modes):
    """
    Project a 1D array onto a truncated Fourier series retaining only
    wavenumbers |k| <= n_modes (periodises / smooths a profile that is
    not itself exactly periodic on the box, e.g. Eq. 88's sech^2 flux
    function), matching the paper's Section 4.2 remark that psi in
    Eq. (88) is expanded in a Fourier series truncated to 22 modes to
    satisfy periodicity in x.
    """
    n = profile_1d.shape[0]
    F = np.fft.fft(profile_1d)
    k = np.fft.fftfreq(n) * n
    mask = np.abs(k) <= n_modes
    return np.real(np.fft.ifft(F * mask))


def current_sheet(X, Y, psi0=1.29, phi0_amp=1e-3, n_modes=22):
    """
    Section 4.2: domain [-pi,pi) x [-pi,pi), 1024x512 in the paper
    (nx x ny here is whatever the grid was built with), ht=0.01.

        phi = phi0 * (cos(x+y) - cos(x-y))
        psi = psi0 / cosh(x)^2,  Fourier-truncated to n_modes in x
              to enforce periodicity (paper's remark in Section 4.2).

    This is also the initial condition used for the electron-inertia /
    collisionless reconnection run in Section 4.3 (there with de > 0).
    """
    phi = phi0_amp * (np.cos(X + Y) - np.cos(X - Y))

    x_1d = X[:, 0]
    profile_1d = psi0 / np.cosh(x_1d) ** 2
    profile_trunc = _truncate_fourier_x(profile_1d, n_modes)
    psi = np.broadcast_to(profile_trunc[:, None], X.shape).copy()
    return phi, psi
