#!/usr/bin/env python3

"""
Discrete differential operators for the variational RMHD integrator of
Kraus, Tassi & Grasso, "Variational Integrators for Reduced
Magnetohydrodynamics" (arXiv:1511.09314v2).

All functions operate on 2D numpy arrays f[i, j] on a doubly-periodic
rectangular grid, i along x (axis 0), j along y (axis 1).

NOTE ON THE ARAKAWA BRACKET FORMULA
------------------------------------
Eq. (81b)-(81c) of the paper, as they appear in the extracted PDF text,
give two of the three sub-Jacobians (the "A+x" divergence form and the
"Ax+" rotation form) with a sign that does *not* satisfy the discrete
conservation identities

    sum_{i,j} f_{i,j} * A_{i,j}(f, g) = 0        (for any f, g)
    sum_{i,j} g_{i,j} * A_{i,j}(f, g) = 0

which are the entire reason Arakawa's scheme is used here: they are what
makes the discrete energy/Casimir conservation proofs in Section 3.2 of
the paper go through. This was checked numerically: with the literal
sign from the extracted text, the two terms do not even converge to the
continuum bracket {f,g} = f_x g_y - f_y g_x under grid refinement (the
error saturates at a finite value instead of shrinking), while flipping
the sign restores both 2nd-order convergence and the conservation
identities above to machine precision. This is consistent with a
sign/ordering artifact of PDF-to-text extraction (very easy to introduce
when flattening multi-term subtracted expressions), not a different
intended scheme -- the classic Arakawa (1966) Jacobian is unambiguous
and is what is implemented here, re-derived from the two equivalent
divergence forms of the bracket,

    {f,g} = d/dx (f g_y) - d/dy (f g_x)     ("+x" / divergence form)
    {f,g} = d/dy (g f_x) - d/dx (g f_y)     ("x+" / rotation form)

and centred finite differences of each. This has been verified against:
(1) a plain nested-loop reference implementation (agreement to machine
    precision), (2) 2nd-order convergence to several analytic bracket
    test cases, (3) antisymmetry A(f,g) = -A(g,f) to machine precision,
    (4) the conservation identities above to machine precision.
"""

import numpy as np


def _shift(f, di, dj):
    """f shifted by (di, dj) grid points with periodic wrap-around,
    i.e. returns the array h with h[i,j] = f[i+di, j+dj] (mod nx, ny)."""
    return np.roll(np.roll(f, -di, axis=0), -dj, axis=1)


def arakawa(f, g, hx, hy):
    """
    Arakawa's (1966) discretisation of the canonical Poisson bracket
    {f, g} = f_x g_y - f_y g_x, combining three second-order-accurate
    finite-difference forms with equal weight 1/3 (Eq. 54-55 of the
    paper, alpha=beta=gamma=1/3):

        A(f,g) = 1/3 [ A_pp(f,g) + A_px(f,g) + A_xp(f,g) ]

    See the module docstring for a note on a sign correction relative
    to the raw extracted text of Eq. (81b)-(81c).
    """
    fp0 = _shift(f, 1, 0)     # f_{i+1,j}
    fm0 = _shift(f, -1, 0)    # f_{i-1,j}
    f0p = _shift(f, 0, 1)     # f_{i,j+1}
    f0m = _shift(f, 0, -1)    # f_{i,j-1}
    fpp = _shift(f, 1, 1)     # f_{i+1,j+1}
    fpm = _shift(f, 1, -1)    # f_{i+1,j-1}
    fmp = _shift(f, -1, 1)    # f_{i-1,j+1}
    fmm = _shift(f, -1, -1)   # f_{i-1,j-1}

    gp0 = _shift(g, 1, 0)
    gm0 = _shift(g, -1, 0)
    g0p = _shift(g, 0, 1)
    g0m = _shift(g, 0, -1)
    gpp = _shift(g, 1, 1)
    gpm = _shift(g, 1, -1)
    gmp = _shift(g, -1, 1)
    gmm = _shift(g, -1, -1)

    denom = 4.0 * hx * hy

    # "direct" form: centred difference of both factors (Eq. 81a)
    Jpp = ((fp0 - fm0) * (g0p - g0m) - (f0p - f0m) * (gp0 - gm0)) / denom

    # "divergence" form: d/dx(f g_y) - d/dy(f g_x)
    Jpx = (fp0 * (gpp - gpm) - fm0 * (gmp - gmm)
           - f0p * (gpp - gmp) + f0m * (gpm - gmm)) / denom

    # "rotation" form: d/dy(g f_x) - d/dx(g f_y)
    Jxp = (fmp * (gm0 - g0p) - fpp * (gp0 - g0p)
           + fpm * (gp0 - g0m) - fmm * (gm0 - g0m)) / denom

    return (Jpp + Jpx + Jxp) / 3.0


def discrete_laplacian(f, hx, hy):
    """
    Standard centred second-order finite-difference Laplacian, Eq. (84):

        Delta_x f_{i,j} = (f_{i-1,j} - 2 f_{i,j} + f_{i+1,j}) / hx^2
        Delta_y f_{i,j} = (f_{i,j-1} - 2 f_{i,j} + f_{i,j+1}) / hy^2
    """
    fp0 = _shift(f, 1, 0)
    fm0 = _shift(f, -1, 0)
    f0p = _shift(f, 0, 1)
    f0m = _shift(f, 0, -1)
    return (fm0 - 2.0 * f + fp0) / hx ** 2 + (f0m - 2.0 * f + f0p) / hy ** 2


class SpectralSolvers:
    """
    Exact inversion (via FFT) of the linear elliptic operators that appear
    in the scheme, using the EXACT eigenvalues of the discrete 5-point
    stencil defined in `discrete_laplacian` above (not the continuum
    Laplacian). Because the domain is doubly periodic, discrete Fourier
    modes are exact eigenvectors of this stencil, so these solves are
    exact linear-algebra inversions of the very same discrete operator
    used everywhere else in the code -- not an additional approximation
    on top of the finite-difference scheme.
    """

    def __init__(self, nx, ny, hx, hy):
        self.nx, self.ny = nx, ny
        self.hx, self.hy = hx, hy
        kx = 2.0 * np.pi * np.fft.fftfreq(nx)
        ky = 2.0 * np.pi * np.fft.fftfreq(ny)
        Kx, Ky = np.meshgrid(kx, ky, indexing="ij")
        # eigenvalue of (-Delta) for the 5-point stencil, Eq. (84)
        self.minus_lap_eig = (2.0 * (1.0 - np.cos(Kx)) / hx ** 2
                               + 2.0 * (1.0 - np.cos(Ky)) / hy ** 2)

    def poisson_solve(self, rhs):
        """
        Solve  -Delta phi = rhs  (periodic), i.e. Eq. (82)/(86c) inverted.
        The constant (k=0) mode of phi is not determined by this equation
        (only gradients of phi enter the physics), so it is fixed to zero.
        """
        rhs_hat = np.fft.fft2(rhs)
        eig = self.minus_lap_eig.copy()
        eig[0, 0] = 1.0  # placeholder; k=0 mode is overwritten below
        phi_hat = rhs_hat / eig
        phi_hat[0, 0] = 0.0
        return np.real(np.fft.ifft2(phi_hat))

    def helmholtz_solve(self, rhs, de):
        """
        Solve  (1 - de^2 Delta) psi = rhs  (periodic); this is the
        relation psi-bar = psi + de^2 j = (1 - de^2 Delta) psi from
        Section 2.3, inverted for psi given psi-bar = rhs.
        Eigenvalue of (1 - de^2 Delta) is (1 + de^2 * minus_lap_eig),
        always >= 1, so there is no singular mode / gauge freedom here.
        Reduces to the identity map when de = 0.
        """
        if de == 0.0:
            return rhs.copy()
        rhs_hat = np.fft.fft2(rhs)
        eig = 1.0 + de ** 2 * self.minus_lap_eig
        psi_hat = rhs_hat / eig
        return np.real(np.fft.ifft2(psi_hat))
