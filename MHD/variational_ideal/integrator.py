#!/usr/bin/env python3

"""
Variational integrator for reduced MHD (RMHD), following
Kraus, Tassi & Grasso, "Variational Integrators for Reduced
Magnetohydrodynamics", arXiv:1511.09314v2 (referred to as "the paper"
below). Section/equation numbers refer to that paper.

MODEL
-----
Ideal RMHD (Eq. 1), or RMHD with electron inertia (Eq. 43), unified by
writing the second evolution equation in terms of the generalised
potential

    chi = psi + de^2 * j             (Eq. before Eq. 43; chi = psi-bar)

which reduces to chi = psi when de = 0 (ideal case). The model is then

    omega_t + {phi, omega} + {j, psi} = 0,      -Delta phi = omega
    chi_t   + {phi, chi}              = 0,      -Delta psi = j,
                                                 chi = (1 - de^2 Delta) psi

DISCRETISATION
--------------
Space: Arakawa's Jacobian for the Poisson bracket (Eq. 80-81, corrected
per the note in operators.py) and the standard 5-point Laplacian
(Eq. 84). Time: the paper's variational/Hamilton-Pontryagin construction
(Sec. 3.1) reduces, after eliminating the internal-stage and momentum
variables, to a Crank-Nicolson-type one-step method, Eq. (86):

    (omega^{n+1} - omega^n)/ht
        + 1/4 [A(phi^{n+1},omega^{n+1}) + A(phi^n,omega^{n+1})
               + A(phi^{n+1},omega^n)   + A(phi^n,omega^n)]
        + 1/4 [A(j^{n+1},psi^{n+1}) + A(j^n,psi^{n+1})
               + A(j^{n+1},psi^n)   + A(j^n,psi^n)] = 0

    (chi^{n+1} - chi^n)/ht
        + 1/4 [A(phi^{n+1},chi^{n+1}) + A(phi^n,chi^{n+1})
               + A(phi^{n+1},chi^n)   + A(phi^n,chi^n)] = 0

    omega^{n+1} = -Delta phi^{n+1},   j^{n+1} = -Delta psi^{n+1},
    chi^{n+1}   = (1 - de^2 Delta) psi^{n+1}

Because the Arakawa bracket A(.,.) is *bilinear*, the 1/4-weighted sum
of the four combinations above is algebraically identical to evaluating
A once at the time-averaged ("theta = 1/2") fields, e.g.

    1/4 [A(a1,b1)+A(a0,b1)+A(a1,b0)+A(a0,b0)] = A((a1+a0)/2, (b1+b0)/2)

This is used below purely as an algebraic simplification (it changes
nothing about the equations being solved -- it is exactly Eq. 86, just
written more efficiently), and matches the "phi^{n+1/2}" notation the
paper itself uses for the same quantity in Eq. (78) and Eq. (85).

SOLVING THE NONLINEAR SYSTEM
-----------------------------
phi and j are linear (Poisson/Helmholtz-type) functions of the unknowns
omega, chi, so the scheme is reduced here to a 2-field nonlinear system
in (omega^{n+1}, chi^{n+1}) with phi^{n+1}, psi^{n+1}, j^{n+1} recovered
by exact linear solves at every Newton iterate. This is an algebraically
exact reformulation of the paper's 4-field system (90a)-(90d): eliminating
a linear constraint by direct substitution does not change the solution.
It is solved by Newton's method; each Newton step's linear system is
solved with GMRES using a matrix-free Jacobian-vector product built from
the exact Jacobian (Eq. 95), generalised here to include the psi<->chi
Helmholtz map for the electron-inertia case. The convergence criterion
matches Eq. (94) of the paper.

WHAT IS NOT REPRODUCED EXACTLY
-------------------------------
The paper's Appendix A physics-based preconditioner (following Chacon
et al. 2002) for accelerating the Newton-GMRES solve is NOT implemented.
Newton-GMRES here runs unpreconditioned (or with a trivial identity
preconditioner). This changes only the *number of linear-solver
iterations needed per Newton step*, i.e. wall-clock cost -- it has no
effect on the equations being solved, on the converged solution at each
timestep (Newton's method converges to the same root regardless of how
the linear correction is solved), or on the conservation properties
verified below. At the resolutions used in Section 4.1-4.2 of the paper
(up to 1024x512) an unpreconditioned solve in pure Python/NumPy would be
slow; this implementation is intended for small-to-moderate grids
(demonstrated up to a few hundred points per side) where it runs in
seconds to minutes. See the docstring of RMHDIntegrator.solve_step for
more detail.
"""

import numpy as np
from scipy.sparse.linalg import LinearOperator, gmres

from operators import arakawa, discrete_laplacian, SpectralSolvers


class RMHDIntegrator:
    def __init__(self, nx, ny, Lx, Ly, ht, de=0.0,
                 abs_tol=5e-16, rel_tol=1e-10, max_newton=30,
                 gmres_rtol=1e-12, gmres_atol=0.0,
                 gmres_restart=60, gmres_maxiter=300, verbose=False):
        self.nx, self.ny = nx, ny
        self.Lx, self.Ly = Lx, Ly
        self.hx, self.hy = Lx / nx, Ly / ny
        self.ht = ht
        self.de = de

        # Newton tolerance, Eq. (94): ||F(phi_m)||_2 < n*eps_abs + eps_rel*||F(phi_0)||_2
        self.abs_tol = abs_tol
        self.rel_tol = rel_tol
        self.n_dof = nx * ny
        self.max_newton = max_newton

        self.gmres_rtol = gmres_rtol
        self.gmres_atol = gmres_atol
        self.gmres_restart = gmres_restart
        self.gmres_maxiter = gmres_maxiter
        self.verbose = verbose

        self.solvers = SpectralSolvers(nx, ny, self.hx, self.hy)

        x = np.arange(nx) * self.hx
        y = np.arange(ny) * self.hy
        self.X, self.Y = np.meshgrid(x, y, indexing="ij")

        self.t = 0.0
        self.step_count = 0
        self.newton_iters_last = None
        self.gmres_iters_last = None

    # ------------------------------------------------------------------
    # initialisation
    # ------------------------------------------------------------------
    def initialize(self, phi0, psi0):
        """
        Set the initial state from prescribed phi and psi fields
        (as in Sections 4.1-4.2 of the paper: phi and psi are given
        analytically, omega and j follow from the discrete Laplacian).
        """
        self.phi = phi0.copy()
        self.psi = psi0.copy()
        self.omega = -discrete_laplacian(self.phi, self.hx, self.hy)
        self.j = -discrete_laplacian(self.psi, self.hx, self.hy)
        self.chi = self.psi + self.de ** 2 * self.j
        self.t = 0.0
        self.step_count = 0

    # ------------------------------------------------------------------
    # residual and Jacobian-vector product for the Newton solve
    # ------------------------------------------------------------------
    def _fields_from_state(self, omega1, chi1):
        phi1 = self.solvers.poisson_solve(omega1)
        psi1 = self.solvers.helmholtz_solve(chi1, self.de)
        j1 = -discrete_laplacian(psi1, self.hx, self.hy)
        return phi1, psi1, j1

    def _residual(self, omega1, chi1):
        phi1, psi1, j1 = self._fields_from_state(omega1, chi1)

        phi_h = 0.5 * (phi1 + self.phi)
        omega_h = 0.5 * (omega1 + self.omega)
        psi_h = 0.5 * (psi1 + self.psi)
        j_h = 0.5 * (j1 + self.j)
        chi_h = 0.5 * (chi1 + self.chi)

        F_omega = ((omega1 - self.omega) / self.ht
                   + arakawa(phi_h, omega_h, self.hx, self.hy)
                   + arakawa(j_h, psi_h, self.hx, self.hy))
        F_chi = ((chi1 - self.chi) / self.ht
                 + arakawa(phi_h, chi_h, self.hx, self.hy))
        return F_omega, F_chi, phi1, psi1, j1

    def _jacobian_action(self, omega1, chi1, phi1, psi1, j1,
                          domega, dchi):
        """
        Action of the exact Jacobian of (F_omega, F_chi) with respect to
        (omega1, chi1) on the direction (domega, dchi). Derived directly
        from bilinearity of the Arakawa bracket (equivalent to Eq. 95 of
        the paper after eliminating phi, j via their linear defining
        relations, and generalised to include the psi<->chi Helmholtz
        map needed for de > 0).
        """
        dphi = self.solvers.poisson_solve(domega)
        dpsi = self.solvers.helmholtz_solve(dchi, self.de)
        dj = -discrete_laplacian(dpsi, self.hx, self.hy)

        phi_h = 0.5 * (phi1 + self.phi)
        omega_h = 0.5 * (omega1 + self.omega)
        psi_h = 0.5 * (psi1 + self.psi)
        j_h = 0.5 * (j1 + self.j)
        chi_h = 0.5 * (chi1 + self.chi)

        dphi_h = 0.5 * dphi
        domega_h = 0.5 * domega
        dpsi_h = 0.5 * dpsi
        dj_h = 0.5 * dj
        dchi_h = 0.5 * dchi

        dF_omega = (domega / self.ht
                    + arakawa(phi_h, domega_h, self.hx, self.hy)
                    + arakawa(dphi_h, omega_h, self.hx, self.hy)
                    + arakawa(j_h, dpsi_h, self.hx, self.hy)
                    + arakawa(dj_h, psi_h, self.hx, self.hy))
        dF_chi = (dchi / self.ht
                  + arakawa(phi_h, dchi_h, self.hx, self.hy)
                  + arakawa(dphi_h, chi_h, self.hx, self.hy))
        return dF_omega, dF_chi

    # ------------------------------------------------------------------
    # one implicit timestep
    # ------------------------------------------------------------------
    def solve_step(self):
        """
        Advance the state by one timestep ht, solving the nonlinear
        system F(omega^{n+1}, chi^{n+1}) = 0 by Newton's method. Each
        Newton correction is obtained from GMRES applied to a
        matrix-free LinearOperator built from `_jacobian_action`
        (no preconditioner -- see module docstring).
        """
        n = self.n_dof
        N = 2 * n
        shape2d = (self.nx, self.ny)

        omega1 = self.omega.copy()
        chi1 = self.chi.copy()

        F_omega, F_chi, phi1, psi1, j1 = self._residual(omega1, chi1)
        Fvec = np.concatenate([F_omega.ravel(), F_chi.ravel()])
        F0_norm = np.linalg.norm(Fvec)
        tol = n * self.abs_tol + self.rel_tol * F0_norm

        newton_it = 0
        while np.linalg.norm(Fvec) >= tol and newton_it < self.max_newton:
            def matvec(v, omega1=omega1, chi1=chi1,
                       phi1=phi1, psi1=psi1, j1=j1):
                domega = v[:n].reshape(shape2d)
                dchi = v[n:].reshape(shape2d)
                dFo, dFc = self._jacobian_action(
                    omega1, chi1, phi1, psi1, j1, domega, dchi)
                return np.concatenate([dFo.ravel(), dFc.ravel()])

            J = LinearOperator((N, N), matvec=matvec, dtype=float)
            rhs = -Fvec
            dx, info = gmres(J, rhs, rtol=self.gmres_rtol,
                              atol=self.gmres_atol,
                              restart=self.gmres_restart,
                              maxiter=self.gmres_maxiter)
            if info != 0 and self.verbose:
                print(f"  [gmres] did not fully converge, info={info}")

            domega = dx[:n].reshape(shape2d)
            dchi = dx[n:].reshape(shape2d)
            omega1 = omega1 + domega
            chi1 = chi1 + dchi

            F_omega, F_chi, phi1, psi1, j1 = self._residual(omega1, chi1)
            Fvec = np.concatenate([F_omega.ravel(), F_chi.ravel()])
            newton_it += 1

            if self.verbose:
                print(f"  [newton {newton_it}] ||F|| = {np.linalg.norm(Fvec):.3e}"
                      f"  (tol = {tol:.3e})")

        self.newton_iters_last = newton_it
        if newton_it >= self.max_newton and np.linalg.norm(Fvec) >= tol:
            print(f"WARNING: Newton solve did not converge at t={self.t:.4f} "
                  f"after {newton_it} iterations (||F||={np.linalg.norm(Fvec):.3e}, "
                  f"tol={tol:.3e})")

        # accept the step
        self.omega, self.chi = omega1, chi1
        self.phi, self.psi, self.j = phi1, psi1, j1
        self.t += self.ht
        self.step_count += 1

    def run(self, n_steps, diagnostics_every=1, progress_every=None):
        """Advance n_steps timesteps, returning a dict of diagnostic
        time series (see `diagnostics`)."""
        hist = {"t": [], "E": [], "C_MH": [], "C_L2": [], "C_CH": [],
                "newton_iters": []}

        def record():
            E, CMH, CL2, CCH = self.diagnostics()
            hist["t"].append(self.t)
            hist["E"].append(E)
            hist["C_MH"].append(CMH)
            hist["C_L2"].append(CL2)
            hist["C_CH"].append(CCH)
            hist["newton_iters"].append(self.newton_iters_last)

        record()  # initial state
        for k in range(n_steps):
            self.solve_step()
            if (k + 1) % diagnostics_every == 0:
                record()
            if progress_every and (k + 1) % progress_every == 0:
                print(f"step {k+1}/{n_steps}  t={self.t:.4f}  "
                      f"newton_iters={self.newton_iters_last}")
        return hist

    # ------------------------------------------------------------------
    # conserved quantities, Eq. (87a)-(87d) [ideal] / (44),(47)-(49) [inertia]
    # ------------------------------------------------------------------
    def diagnostics(self):
        hx, hy = self.hx, self.hy
        E = 0.5 * hx * hy * np.sum(self.phi * self.omega + self.chi * self.j)
        C_MH = hx * hy * np.sum(self.chi)
        C_L2 = hx * hy * np.sum(self.chi ** 2)
        C_CH = hx * hy * np.sum(self.omega * self.chi)
        return E, C_MH, C_L2, C_CH
