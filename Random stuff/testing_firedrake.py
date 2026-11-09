#!/usr/bin/env python3

#!/usr/bin/env python3
"""
Continuous-Galerkin (CG) compressible hydrodynamics (Euler equations) in Firedrake.

Model
-----
Conservative variables U = (rho, rho*u_1..rho*u_d, E), d = 1 or 2:

    dU/dt + div F(U) = div( nu grad U )

    F_rho   = rho u
    F_mom_i = rho u_i u + p e_i
    F_E     = (E + p) u
    p       = (gamma - 1) * (E - 0.5 rho |u|^2)          (ideal-gas EOS)

Discretisation
--------------
* Space : every component of U lives in the same continuous Lagrange space CG(p).
          Weak form (test function v, consistent mass matrix):

              (v, dU/dt) = (grad v, F(U)) - (grad v, nu grad U) - <v, F_wall . n>_wall

          The viscous term uses the natural boundary condition nu grad U . n = 0.
* Time  : explicit SSP-RK3 (Shu-Osher), consistent mass matrix solved with CG+Jacobi.
* Stabilisation : plain CG is not stable for shocks, so an entropy-viscosity
          nu (in the spirit of Guermond, Pasquetti & Popov, JCP 230 (2011)) is added:

              nu = min( nu_max , nu_E )
              nu_max = c_max * (h/p) * (|u| + c)                    first-order bound
              nu_E   = c_ent * (h/p)^2 * |D| / (max eta - min eta)  entropy residual
              D      = (eta^n - eta^{n-1}) / dt_prev + div(u eta^n)   (lagged BDF1)
              eta    = -rho * s,  s = ln(p / rho^gamma) / (gamma - 1)

          nu is frozen over each RK step (built from U^n and U^{n-1}); the first
          step uses nu_max because there is no U^{n-1} yet.  The same nu multiplies all
          conserved variables, so mass, momentum and energy remain conserved.
          NOTE: the normalisation uses the *range* of eta over the nodes, not
          ||eta - mean||_inf as in the paper.
* Boundaries : for each exterior-facet id the case must choose either
          - dirichlet_ids : strong Dirichlet, state frozen at its initial value, or
          - wall_ids      : weak slip wall, boundary flux (0, p n, 0).
          Any id in neither list is an error (it would silently be a zero-flux wall).

Test cases
----------
  sod        1D Sod shock tube on [0, 1], compared against the exact Riemann solution.
  explosion  2D cylindrical explosion (Toro) on [0, 2]^2, reflective walls.

Usage
-----
  python cg_euler.py --case sod --degree 2 --ncells 200
  python cg_euler.py --case explosion --degree 1 --ncells 100
  mpiexec -n 4 python cg_euler.py --case explosion        (VTK only; plots need 1 rank)

Everything experimental lives in `Config`; the full config is written to
<outdir>/config.json next to the results.
"""
from __future__ import annotations

import argparse
import dataclasses
import json
import os

import numpy as np
import ufl
import firedrake as fd
from mpi4py import MPI


# =============================================================================
# Configuration (no hidden defaults: every physical / numerical parameter here)
# =============================================================================
@dataclasses.dataclass(frozen=True)
class Config:
    case: str                 # "sod" or "explosion"
    dim: int                  # spatial dimension (1 or 2)
    degree: int               # CG polynomial degree
    ncells: int               # cells per direction on [0, length]^dim
    length: float             # edge length of the (hyper)cubic domain
    gamma: float              # ratio of specific heats
    t_end: float              # final time
    cfl: float                # dt = cfl * h_min / (degree^2 * max(|u| + c))
    c_max: float              # first-order viscosity coefficient
    c_ent: float              # entropy viscosity coefficient
    eta_ref_floor: float      # lower bound on the entropy normalisation (uniform states)
    ic_width_cells: float     # tanh smoothing half-width of the initial data, in cells
    quad_degree: int          # quadrature degree for all integrals (fixed explicitly)
    dirichlet_ids: tuple      # exterior facet ids with frozen (Dirichlet) state
    wall_ids: tuple           # exterior facet ids with weak slip wall
    conservation_tol: float   # max allowed relative drift of total mass and energy
    n_output: int             # write VTK every n_output steps (0 = no VTK)
    outdir: str
    ic_params: dict           # case-specific initial-condition parameters


def sod_config() -> Config:
    return Config(
        case="sod", dim=1, degree=2, ncells=200, length=1.0, gamma=1.4, t_end=0.2,
        cfl=0.25, c_max=0.5, c_ent=1.0, eta_ref_floor=1e-10, ic_width_cells=1.5,
        quad_degree=6, dirichlet_ids=(1, 2), wall_ids=(), conservation_tol=1e-8,
        n_output=0, outdir="output_sod",
        ic_params=dict(x0=0.5, rho_l=1.0, u_l=0.0, p_l=1.0,
                       rho_r=0.125, u_r=0.0, p_r=0.1),
    )


def explosion_config() -> Config:
    return Config(
        case="explosion", dim=2, degree=1, ncells=100, length=2.0, gamma=1.4, t_end=0.25,
        cfl=0.25, c_max=0.5, c_ent=1.0, eta_ref_floor=1e-10, ic_width_cells=1.5,
        quad_degree=3, dirichlet_ids=(), wall_ids=(1, 2, 3, 4), conservation_tol=1e-8,
        n_output=20, outdir="output_explosion",
        ic_params=dict(center=(1.0, 1.0), radius=0.4,
                       rho_in=1.0, p_in=1.0, rho_out=0.125, p_out=0.1),
    )


PRESETS = {"sod": sod_config, "explosion": explosion_config}


def validate_config(cfg: Config) -> None:
    assert cfg.case in PRESETS, f"unknown case {cfg.case!r}"
    assert cfg.dim in (1, 2), "only 1D and 2D are supported"
    assert cfg.degree >= 1 and cfg.ncells >= 1
    assert cfg.gamma > 1.0
    assert cfg.length > 0 and cfg.t_end > 0 and cfg.cfl > 0
    assert cfg.c_max >= 0 and cfg.c_ent >= 0 and cfg.eta_ref_floor > 0
    assert cfg.ic_width_cells > 0
    assert cfg.quad_degree >= 2 * cfg.degree, "quadrature too low for the mass matrix"
    assert not (set(cfg.dirichlet_ids) & set(cfg.wall_ids)), "facet id used twice"
    assert cfg.conservation_tol > 0 and cfg.n_output >= 0


# =============================================================================
# Pure-UFL physics (no Firedrake objects; can be unit-tested with plain UFL)
# =============================================================================
def primitives(U, dim, gamma):
    """Return (rho, m, u, p, E) as UFL expressions of the conservative vector U."""
    rho = U[0]
    m = ufl.as_vector([U[1 + i] for i in range(dim)])
    E = U[dim + 1]
    u = m / rho
    p = (gamma - 1.0) * (E - 0.5 * ufl.dot(m, m) / rho)
    return rho, m, u, p, E


def euler_flux(U, dim, gamma):
    """Flux tensor F[k, j] = flux of conserved component k in direction j; shape (dim+2, dim)."""
    rho, m, u, p, E = primitives(U, dim, gamma)
    rows = [[m[j] for j in range(dim)]]
    for i in range(dim):
        rows.append([m[i] * u[j] + (p if i == j else 0.0) for j in range(dim)])
    rows.append([(E + p) * u[j] for j in range(dim)])
    return ufl.as_matrix(rows)


def max_wave_speed(U, dim, gamma):
    """|u| + c, the largest characteristic speed."""
    rho, m, u, p, E = primitives(U, dim, gamma)
    return ufl.sqrt(ufl.dot(u, u)) + ufl.sqrt(gamma * p / rho)


def entropy(U, dim, gamma):
    """Mathematical entropy eta = -rho s, s = ln(p rho^-gamma)/(gamma-1) (convex in U)."""
    rho, m, u, p, E = primitives(U, dim, gamma)
    return -rho * ufl.ln(p / rho**gamma) / (gamma - 1.0)


def entropy_viscosity(U_n, U_nm1, dt_prev, ent_on, eta_ref, h, dim, degree, gamma,
                      c_max, c_ent):
    """nu = min(nu_max, nu_E); on the first step (ent_on = 0) nu = nu_max.

    U_n, U_nm1 : conservative states at t^n and t^{n-1}
    dt_prev    : t^n - t^{n-1}  (must be nonzero even when ent_on = 0)
    eta_ref    : entropy normalisation (range of eta over the domain)
    h          : cell diameter
    """
    rho, m, u, p, E = primitives(U_n, dim, gamma)
    h_eff = h / degree
    nu_max = c_max * h_eff * max_wave_speed(U_n, dim, gamma)
    eta_n = entropy(U_n, dim, gamma)
    eta_nm1 = entropy(U_nm1, dim, gamma)
    residual = (eta_n - eta_nm1) / dt_prev + ufl.div(u * eta_n)
    nu_ent = c_ent * h_eff**2 * abs(residual) / eta_ref
    return (1.0 - ent_on) * nu_max + ent_on * ufl.min_value(nu_max, nu_ent)


def weak_rhs(Us, v, n, nu, dim, gamma, wall_ids, quad_degree):
    """Right-hand side R(v; U) of  (v, dU/dt) = R."""
    dx = ufl.dx(degree=quad_degree)
    form = (ufl.inner(euler_flux(Us, dim, gamma), ufl.grad(v)) * dx
            - nu * ufl.inner(ufl.grad(Us), ufl.grad(v)) * dx)
    if wall_ids:
        _, _, _, p, _ = primitives(Us, dim, gamma)
        # slip-wall flux: no mass or energy flux, momentum flux = p n
        wall_flux = ufl.as_vector([0.0] + [p * n[i] for i in range(dim)] + [0.0])
        form = form - ufl.inner(wall_flux, v) * ufl.ds(tuple(wall_ids), degree=quad_degree)
    return form


def conservative_from_primitive(rho, vel, p, gamma):
    """UFL vector (rho, rho u, E) from primitives; vel is a list of UFL scalars."""
    kinetic = 0.5 * rho * sum(vi * vi for vi in vel)
    return ufl.as_vector([rho] + [rho * vi for vi in vel] + [p / (gamma - 1.0) + kinetic])


# =============================================================================
# Exact Riemann solver (Toro, "Riemann Solvers and Numerical Methods for Fluid
# Dynamics", ch. 4) -- numpy only, used to validate the Sod case
# =============================================================================
def exact_riemann(x, t, x0, left, right, gamma):
    """Exact solution (rho, u, p) at positions x and time t > 0.

    left, right : (rho, u, p) tuples.  Raises if a vacuum would be generated.
    """
    assert t > 0.0
    g = gamma
    g1, g2 = (g - 1) / (2 * g), (g + 1) / (2 * g)
    g3, g4, g5 = 2 * g / (g - 1), 2 / (g - 1), 2 / (g + 1)
    g6, g7 = (g - 1) / (g + 1), (g - 1) / 2
    rl, ul, pl = left
    rr, ur, pr = right
    cl, cr = np.sqrt(g * pl / rl), np.sqrt(g * pr / rr)
    if g4 * (cl + cr) <= ur - ul:
        raise ValueError("initial data generates a vacuum")

    def f_and_df(p, rk, pk, ck):
        if p > pk:  # shock
            a, b = g5 / rk, g6 * pk
            q = np.sqrt(a / (p + b))
            return (p - pk) * q, q * (1.0 - 0.5 * (p - pk) / (b + p))
        # rarefaction
        return (g4 * ck * ((p / pk) ** g1 - 1.0),
                (1.0 / (rk * ck)) * (p / pk) ** (-g2))

    # Starting guess for p* (Toro sec. 4.3.2): primitive-variable guess if it lies in
    # [pmin, pmax] and pmax/pmin <= 2, else two-rarefaction / two-shock approximation.
    p_pv = max(1e-8, 0.5 * (pl + pr) - 0.125 * (ur - ul) * (rl + rr) * (cl + cr))
    p_min, p_max = min(pl, pr), max(pl, pr)
    if p_max / p_min <= 2.0 and p_min <= p_pv <= p_max:
        p = p_pv
    elif p_pv < p_min:  # two-rarefaction approximation
        p = ((cl + cr - g7 * (ur - ul)) / (cl / pl ** g1 + cr / pr ** g1)) ** (1.0 / g1)
    else:  # two-shock approximation
        gl = np.sqrt((g5 / rl) / (g6 * pl + p_pv))
        gr = np.sqrt((g5 / rr) / (g6 * pr + p_pv))
        p = (gl * pl + gr * pr - (ur - ul)) / (gl + gr)
    assert p > 0.0, "Riemann starting guess is not positive"
    for _ in range(100):
        fl, dfl = f_and_df(p, rl, pl, cl)
        fr, dfr = f_and_df(p, rr, pr, cr)
        p_new = p - (fl + fr + (ur - ul)) / (dfl + dfr)
        if p_new <= 0.0:
            raise RuntimeError("Newton iteration produced non-positive pressure")
        change = 2.0 * abs(p_new - p) / (p_new + p)
        p = p_new
        if change < 1e-13:
            break
    else:
        raise RuntimeError("exact Riemann Newton iteration did not converge")
    fl, _ = f_and_df(p, rl, pl, cl)
    fr, _ = f_and_df(p, rr, pr, cr)
    p_star = p
    u_star = 0.5 * (ul + ur) + 0.5 * (fr - fl)

    x = np.asarray(x, dtype=float)
    rho_out = np.empty_like(x)
    u_out = np.empty_like(x)
    p_out = np.empty_like(x)
    for i, xi in enumerate(x):
        s = (xi - x0) / t
        if s <= u_star:  # left of the contact
            if p_star <= pl:  # left rarefaction
                if s <= ul - cl:
                    rho_out[i], u_out[i], p_out[i] = rl, ul, pl
                else:
                    c_star_l = cl * (p_star / pl) ** g1
                    if s > u_star - c_star_l:
                        rho_out[i] = rl * (p_star / pl) ** (1.0 / g)
                        u_out[i], p_out[i] = u_star, p_star
                    else:  # inside the fan
                        u_out[i] = g5 * (cl + g7 * ul + s)
                        c = g5 * (cl + g7 * (ul - s))
                        rho_out[i] = rl * (c / cl) ** g4
                        p_out[i] = pl * (c / cl) ** g3
            else:  # left shock
                pm = p_star / pl
                if s <= ul - cl * np.sqrt(g2 * pm + g1):
                    rho_out[i], u_out[i], p_out[i] = rl, ul, pl
                else:
                    rho_out[i] = rl * (pm + g6) / (pm * g6 + 1.0)
                    u_out[i], p_out[i] = u_star, p_star
        else:  # right of the contact
            if p_star > pr:  # right shock
                pm = p_star / pr
                if s >= ur + cr * np.sqrt(g2 * pm + g1):
                    rho_out[i], u_out[i], p_out[i] = rr, ur, pr
                else:
                    rho_out[i] = rr * (pm + g6) / (pm * g6 + 1.0)
                    u_out[i], p_out[i] = u_star, p_star
            else:  # right rarefaction
                if s >= ur + cr:
                    rho_out[i], u_out[i], p_out[i] = rr, ur, pr
                else:
                    c_star_r = cr * (p_star / pr) ** g1
                    if s <= u_star + c_star_r:
                        rho_out[i] = rr * (p_star / pr) ** (1.0 / g)
                        u_out[i], p_out[i] = u_star, p_star
                    else:  # inside the fan
                        u_out[i] = g5 * (-cr + g7 * ur + s)
                        c = g5 * (cr - g7 * (ur - s))
                        rho_out[i] = rr * (c / cr) ** g4
                        p_out[i] = pr * (c / cr) ** g3
    return rho_out, u_out, p_out


# =============================================================================
# Firedrake solver
# =============================================================================
def _reduce(comm, arr, local_op, mpi_op, empty_value):
    """Global min/max of a distributed nodal array (a rank may own no nodes)."""
    local = float(local_op(arr)) if arr.size else empty_value
    return comm.allreduce(local, op=mpi_op)


class CGEuler:
    """CG(p) Euler solver with entropy viscosity and SSP-RK3 time stepping."""

    def __init__(self, cfg: Config):
        validate_config(cfg)
        self.cfg = cfg
        d = cfg.dim
        self.nvar = d + 2

        # ---- mesh -------------------------------------------------------------
        if d == 1:
            self.mesh = fd.IntervalMesh(cfg.ncells, cfg.length)
        else:
            # "crossed" diagonals keep the triangulation symmetric for the radial test
            self.mesh = fd.RectangleMesh(cfg.ncells, cfg.ncells, cfg.length, cfg.length,
                                         diagonal="crossed")
        self.comm = self.mesh.comm
        markers = {int(mk) for mk in self.mesh.exterior_facets.unique_markers}
        assigned = set(cfg.dirichlet_ids) | set(cfg.wall_ids)
        assert markers == assigned, (
            f"exterior facet ids {sorted(markers)} must each be a Dirichlet or wall id; "
            f"got {sorted(assigned)}")

        # ---- spaces and fields --------------------------------------------------
        self.V = fd.VectorFunctionSpace(self.mesh, "CG", cfg.degree, dim=self.nvar)
        self.S = fd.FunctionSpace(self.mesh, "CG", cfg.degree)
        self.U = fd.Function(self.V, name="U")        # U^n   (frozen during a step)
        self.Uold = fd.Function(self.V)               # U^{n-1}
        self.Us = fd.Function(self.V)                 # RK stage state fed to the RHS
        self.k = fd.Function(self.V)                  # M^{-1} R(Us)
        self.U1 = fd.Function(self.V)
        self.U2 = fd.Function(self.V)
        self.Unew = fd.Function(self.V)

        # scalar diagnostic fields (nodal values)
        self.rho_fn = fd.Function(self.S, name="density")
        self.p_fn = fd.Function(self.S, name="pressure")
        self.u_fn = fd.Function(self.S, name="velocity_x")
        self.lam_fn = fd.Function(self.S)
        self.eta_fn = fd.Function(self.S)
        xs = fd.SpatialCoordinate(self.mesh)
        self.x_fn = fd.Function(self.S).interpolate(xs[0])
        self.y_fn = fd.Function(self.S).interpolate(xs[1]) if d == 2 else None

        # smallest cell diameter (for the time step)
        dg0 = fd.FunctionSpace(self.mesh, "DG", 0)
        h_dg = fd.project(fd.CellDiameter(self.mesh), dg0)
        self.h_min = _reduce(self.comm, h_dg.dat.data_ro, np.min, MPI.MIN, np.inf)
        assert np.isfinite(self.h_min) and self.h_min > 0

        # ---- time-dependent scalars used inside the forms -----------------------
        self.ent_on = fd.Constant(0.0)      # 0 on step 1 (nu = nu_max), then 1
        self.eta_ref = fd.Constant(1.0)
        self.dt_prev = fd.Constant(1.0)     # dummy until the first step has been taken

        # ---- initial condition ---------------------------------------------------
        self.U.interpolate(self._initial_state(xs))
        self.Uold.assign(self.U)

        # ---- weak forms and linear solver ----------------------------------------
        v = fd.TestFunction(self.V)
        n = fd.FacetNormal(self.mesh)
        h = fd.CellDiameter(self.mesh)
        nu = entropy_viscosity(self.U, self.Uold, self.dt_prev, self.ent_on, self.eta_ref,
                               h, d, cfg.degree, cfg.gamma, cfg.c_max, cfg.c_ent)
        rhs = weak_rhs(self.Us, v, n, nu, d, cfg.gamma, cfg.wall_ids, cfg.quad_degree)
        mass = ufl.inner(fd.TrialFunction(self.V), v) * ufl.dx(degree=cfg.quad_degree)
        # Dirichlet nodes keep their initial state, so dU/dt = 0 there
        bcs = []
        if cfg.dirichlet_ids:
            zero = fd.Constant(tuple(0.0 for _ in range(self.nvar)))
            bcs.append(fd.DirichletBC(self.V, zero, tuple(cfg.dirichlet_ids)))
        problem = fd.LinearVariationalProblem(mass, rhs, self.k, bcs=bcs)
        self.solver = fd.LinearVariationalSolver(
            problem, solver_parameters={"ksp_type": "cg", "pc_type": "jacobi",
                                        "ksp_rtol": 1e-12})

        # ---- output ---------------------------------------------------------------
        if self.comm.rank == 0:
            os.makedirs(cfg.outdir, exist_ok=True)
            with open(os.path.join(cfg.outdir, "config.json"), "w") as fh:
                json.dump(dataclasses.asdict(cfg), fh, indent=2)
        self.comm.barrier()
        self.vtk = (fd.VTKFile(os.path.join(cfg.outdir, "solution.pvd"))
                    if cfg.n_output > 0 else None)

    # ------------------------------------------------------------------ initial data
    def _initial_state(self, x):
        cfg, ic = self.cfg, self.cfg.ic_params
        width = cfg.ic_width_cells * cfg.length / cfg.ncells
        if cfg.case == "sod":
            s = 0.5 * (1.0 + ufl.tanh((x[0] - ic["x0"]) / width))  # 0 left, 1 right
            rho = ic["rho_l"] + (ic["rho_r"] - ic["rho_l"]) * s
            p = ic["p_l"] + (ic["p_r"] - ic["p_l"]) * s
            vel = [ic["u_l"] + (ic["u_r"] - ic["u_l"]) * s]
        elif cfg.case == "explosion":
            cx, cy = ic["center"]
            r = ufl.sqrt((x[0] - cx) ** 2 + (x[1] - cy) ** 2)
            s = 0.5 * (1.0 - ufl.tanh((r - ic["radius"]) / width))  # 1 inside, 0 outside
            rho = ic["rho_out"] + (ic["rho_in"] - ic["rho_out"]) * s
            p = ic["p_out"] + (ic["p_in"] - ic["p_out"]) * s
            vel = [0.0, 0.0]
        else:
            raise ValueError(cfg.case)
        return conservative_from_primitive(rho, vel, p, cfg.gamma)

    # --------------------------------------------------------------------- diagnostics
    def diagnostics(self):
        """Nodal checks on U^n; raises on non-finite or non-physical states."""
        cfg = self.cfg
        rho, m, u, p, E = primitives(self.U, cfg.dim, cfg.gamma)
        self.rho_fn.interpolate(rho)
        self.p_fn.interpolate(p)
        finite_local = bool(np.all(np.isfinite(self.rho_fn.dat.data_ro))
                            and np.all(np.isfinite(self.p_fn.dat.data_ro)))
        if not self.comm.allreduce(finite_local, op=MPI.LAND):
            raise FloatingPointError("non-finite density or pressure; reduce cfl or "
                                     "increase c_max / c_ent")
        rho_min = _reduce(self.comm, self.rho_fn.dat.data_ro, np.min, MPI.MIN, np.inf)
        p_min = _reduce(self.comm, self.p_fn.dat.data_ro, np.min, MPI.MIN, np.inf)
        if rho_min <= 0.0 or p_min <= 0.0:
            raise FloatingPointError(
                f"non-physical state: min density = {rho_min:.3e}, min pressure = {p_min:.3e}")
        self.lam_fn.interpolate(max_wave_speed(self.U, cfg.dim, cfg.gamma))
        self.eta_fn.interpolate(entropy(self.U, cfg.dim, cfg.gamma))
        self.u_fn.interpolate(u[0])
        eta_max = _reduce(self.comm, self.eta_fn.dat.data_ro, np.max, MPI.MAX, -np.inf)
        eta_min = _reduce(self.comm, self.eta_fn.dat.data_ro, np.min, MPI.MIN, np.inf)
        lam_max = _reduce(self.comm, self.lam_fn.dat.data_ro, np.max, MPI.MAX, -np.inf)
        return dict(rho_min=rho_min, p_min=p_min, lam_max=lam_max,
                    eta_range=eta_max - eta_min)

    def totals(self):
        """Domain integrals of every conserved variable."""
        return np.array([fd.assemble(self.U[i] * ufl.dx(degree=self.cfg.quad_degree))
                         for i in range(self.nvar)])

    # --------------------------------------------------------------------- time stepping
    def _L(self, state):
        """k <- M^{-1} R(state)."""
        self.Us.assign(state)
        self.solver.solve()

    def step(self, dt):
        """One SSP-RK3 step (Shu & Osher 1988); nu is frozen from (U^n, U^{n-1})."""
        U, k = self.U, self.k
        self._L(U)
        self.U1.assign(U + dt * k)
        self._L(self.U1)
        self.U2.assign(0.75 * U + 0.25 * (self.U1 + dt * k))
        self._L(self.U2)
        self.Unew.assign((1.0 / 3.0) * U + (2.0 / 3.0) * (self.U2 + dt * k))
        self.Uold.assign(U)
        self.U.assign(self.Unew)
        self.dt_prev.assign(dt)
        self.ent_on.assign(1.0)

    def run(self, log_every=50):
        cfg = self.cfg
        t, nstep = 0.0, 0
        totals0 = self.totals()
        while True:
            diag = self.diagnostics()
            finished = t >= cfg.t_end * (1.0 - 1e-12)
            if self.vtk is not None and (nstep % cfg.n_output == 0 or finished):
                self.vtk.write(self.rho_fn, self.p_fn, self.u_fn, time=t)
            if finished:
                break
            self.eta_ref.assign(max(diag["eta_range"], cfg.eta_ref_floor))
            dt = cfg.cfl * self.h_min / (cfg.degree ** 2 * diag["lam_max"])
            dt = min(dt, cfg.t_end - t)
            self.step(dt)
            t += dt
            nstep += 1
            if self.comm.rank == 0 and nstep % log_every == 0:
                print(f"step {nstep:6d}  t = {t:.5f}  dt = {dt:.3e}  "
                      f"min rho = {diag['rho_min']:.4f}  min p = {diag['p_min']:.4f}",
                      flush=True)

        totals1 = self.totals()
        drift = np.abs(totals1 - totals0) / np.maximum(np.abs(totals0), 1e-300)
        mass_drift, energy_drift = drift[0], drift[-1]
        if self.comm.rank == 0:
            print(f"finished: {nstep} steps, t = {t:.6f}")
            print(f"relative drift: mass = {mass_drift:.2e}, energy = {energy_drift:.2e}")
        if max(mass_drift, energy_drift) > cfg.conservation_tol:
            raise RuntimeError(
                f"conservation violated: mass drift {mass_drift:.2e}, energy drift "
                f"{energy_drift:.2e} > tol {cfg.conservation_tol:.1e}")
        return t


# =============================================================================
# Case post-processing
# =============================================================================
def postprocess_sod(sol: CGEuler, t):
    """L1 errors vs the exact Riemann solution, and a comparison plot."""
    cfg, ic = sol.cfg, sol.cfg.ic_params
    left = (ic["rho_l"], ic["u_l"], ic["p_l"])
    right = (ic["rho_r"], ic["u_r"], ic["p_r"])
    x = sol.x_fn.dat.data_ro
    rho_e, u_e, p_e = exact_riemann(x, t, ic["x0"], left, right, cfg.gamma)

    errors = {}
    for name, num, ex in (("density", sol.rho_fn, rho_e), ("velocity", sol.u_fn, u_e),
                          ("pressure", sol.p_fn, p_e)):
        ref = fd.Function(sol.S)
        ref.dat.data[:] = ex
        errors[name] = fd.assemble(abs(num - ref) * ufl.dx(degree=cfg.quad_degree))
    if sol.comm.rank == 0:
        print("L1 errors vs exact Riemann solution (domain length 1): "
              + ", ".join(f"{k} = {v:.4e}" for k, v in errors.items()))

    if sol.comm.size == 1:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
        order = np.argsort(x)
        fig, axes = plt.subplots(1, 3, figsize=(13, 3.6))
        for ax, name, num, ex in ((axes[0], "density", sol.rho_fn, rho_e),
                                  (axes[1], "velocity", sol.u_fn, u_e),
                                  (axes[2], "pressure", sol.p_fn, p_e)):
            ax.plot(x[order], ex[order], "k-", lw=1.0, label="exact")
            ax.plot(x[order], num.dat.data_ro[order], "r.", ms=3,
                    label=f"CG{cfg.degree}, {cfg.ncells} cells")
            ax.set_xlabel("x")
            ax.set_title(name)
        axes[0].legend()
        fig.tight_layout()
        fig.savefig(os.path.join(cfg.outdir, "sod.png"), dpi=150)
        plt.close(fig)
    return errors


def postprocess_explosion(sol: CGEuler, t):
    """Radial scatter plot (the flow should be axisymmetric about the centre)."""
    cfg, ic = sol.cfg, sol.cfg.ic_params
    if sol.comm.size != 1:
        if sol.comm.rank == 0:
            print("skipping radial plot (needs a single MPI rank); see the VTK output")
        return
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    cx, cy = ic["center"]
    r = np.hypot(sol.x_fn.dat.data_ro - cx, sol.y_fn.dat.data_ro - cy)
    fig, axes = plt.subplots(1, 2, figsize=(10, 3.8))
    for ax, name, fn in ((axes[0], "density", sol.rho_fn), (axes[1], "pressure", sol.p_fn)):
        ax.plot(r, fn.dat.data_ro, "r.", ms=1.5)
        ax.set_xlabel("r")
        ax.set_title(f"{name} at t = {t:.3f}")
    fig.tight_layout()
    fig.savefig(os.path.join(cfg.outdir, "explosion_radial.png"), dpi=150)
    plt.close(fig)


# =============================================================================
# Command line
# =============================================================================
def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--case", required=True, choices=sorted(PRESETS))
    ap.add_argument("--degree", type=int, help="override CG degree of the preset")
    ap.add_argument("--ncells", type=int, help="override cells per direction")
    ap.add_argument("--cfl", type=float, help="override CFL number")
    ap.add_argument("--c-ent", type=float, help="override entropy-viscosity coefficient")
    ap.add_argument("--c-max", type=float, help="override first-order viscosity coefficient")
    ap.add_argument("--outdir", help="override output directory")
    args = ap.parse_args()

    cfg = PRESETS[args.case]()
    overrides = {}
    if args.degree is not None:
        overrides["degree"] = args.degree
        overrides["quad_degree"] = 3 * args.degree
    for flag, field in (("ncells", "ncells"), ("cfl", "cfl"), ("c_ent", "c_ent"),
                        ("c_max", "c_max"), ("outdir", "outdir")):
        if getattr(args, flag) is not None:
            overrides[field] = getattr(args, flag)
    cfg = dataclasses.replace(cfg, **overrides)

    sol = CGEuler(cfg)
    t = sol.run()
    if cfg.case == "sod":
        postprocess_sod(sol, t)
    else:
        postprocess_explosion(sol, t)


if __name__ == "__main__":
    main()
