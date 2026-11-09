"""
Thermodynamically consistent discretization of the 1D Navier-Stokes-Fourier
(thermal-fluid) system via its metriplectic 4-bracket structure.

Mimics Barham, Morrison & Zaidni, "A thermodynamically consistent discretization
of 1D thermal-fluid models using their metriplectic 4-bracket structure",
Commun. Nonlinear Sci. Numer. Simul. 145 (2025) 108683 (arXiv:2410.11045).

Spatial discretization: P1 continuous Galerkin (Firedrake), eqs. (8)-(17).

Temporal discretization: Averaged Vector Field (AVF) discrete-gradient method.
  - The dynamical fields (rho, m, sigma) enter the bracket's *structure*
    coefficients evaluated at the midpoint (rho,m,sigma)_mid = (z^n+z^{n+1})/2.
  - The Hamiltonian's functional derivatives (eta, u, T) are replaced by the
    discrete gradient
        bar-D H(z^n, z^{n+1}) = int_0^1 DH( z^n + xi(z^{n+1}-z^n) ) dxi
    approximated by n-point Gauss-Legendre quadrature on [0,1] (the EOS is
    exponential, not polynomial, so the integral is evaluated numerically
    rather than in closed form; with n=5 points this is accurate to
    essentially machine precision for the smooth profiles used here).
  - This combination (midpoint structure + discrete-gradient functional
    derivative) is exactly what makes the scheme thermodynamically
    consistent: antisymmetry of the Poisson bracket gives EXACT discrete
    energy conservation (up to quadrature error in the discrete gradient,
    not time-truncation error), while positive semi-definiteness of the
    symmetric bracket gives guaranteed-sign discrete entropy production,
    independent of the step size dt.
  - Because AVF is a genuine quadrature-based discrete gradient (not a
    finite-stage Runge-Kutta/Gauss-Legendre collocation method) for this
    non-polynomial Hamiltonian, it is implemented here as a bespoke one-step
    nonlinear solve with Firedrake directly; Irksome's Butcher-tableau
    machinery does not express this scheme in general.

State variables (density coordinates): rho (mass density), m (momentum density),
sigma (entropy density), and the algebraically-defined dual fields
eta = delta H/delta rho, u = delta H/delta m, T = delta H/delta sigma, now
each representing the AVF discrete gradient rather than a pointwise evaluation.
"""

from firedrake import *
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# ----------------------------------------------------------------------
# Parameters (eq. 19): dry-air-like, low Reynolds number so dissipation is visible
# ----------------------------------------------------------------------
Re_inv = Constant(0.1)     # 1/Re
Pr     = Constant(0.71)    # Prandtl number
gamma  = Constant(1.4)     # heat capacity ratio

L  = 100.0                 # domain length, Omega = [0, L] periodic (T^1)
nx = 400                   # number of cells (paper uses dx=5e-4 -> ~2e5 cells;
                            # reduce resolution here for a tractable demo)

dt_val = 0.1                # paper: dt = 0.1 for the dissipative run
t_end  = 200.0               # paper: t in [0, 200]

NQ = 5                      # Gauss-Legendre points for the AVF line integral

# ----------------------------------------------------------------------
# Mesh and the degree-1 CG space V_h subset H^1(T^1), eq. (8)
# ----------------------------------------------------------------------
mesh = PeriodicIntervalMesh(nx, L)
x, = SpatialCoordinate(mesh)

V = FunctionSpace(mesh, "CG", 1)

# Mixed space for u_h = (rho_h, m_h, sigma_h, eta_h, u_h, T_h), eq. (13)
Z = V * V * V * V * V * V

w     = Function(Z, name="state_new")   # unknown state at t^{n+1}
w_old = Function(Z, name="state_old")   # known state at t^n

rho, m, sigma, eta, u, T = split(w)
rho0, m0, sigma0, eta0_, u0_, T0_ = split(w_old)

v = TestFunctions(Z)
phi_rho, phi_m, phi_sigma, phi_eta, phi_u, phi_T = v

# ----------------------------------------------------------------------
# Equation of state: ideal gas internal energy U(rho, s) = rho^(gamma-1) e^{(gamma-1)s}
# s = sigma/rho.  Functional derivatives of H = int(0.5 m^2/rho + rho U) dx, eq. (12):
#   H_m    = u    = m/rho
#   H_sigma= T    = dU/ds|_rho
#   H_rho  = eta  = -0.5 (m/rho)^2 + U + rho dU/drho - s dU/ds
# ----------------------------------------------------------------------
def H_derivs(rho_, m_, sigma_):
    """Pointwise Hamiltonian functional derivatives at a given (rho,m,sigma)."""
    s_ = sigma_ / rho_
    U_ = rho_ ** (gamma - 1) * exp((gamma - 1) * s_)
    dU1_ = (gamma - 1) * rho_ ** (gamma - 2) * exp((gamma - 1) * s_)   # dU/drho
    dU2_ = (gamma - 1) * rho_ ** (gamma - 1) * exp((gamma - 1) * s_)   # dU/ds = T
    H_rho_ = -0.5 * m_ ** 2 / rho_ ** 2 + U_ + rho_ * dU1_ - s_ * dU2_
    H_m_   = m_ / rho_
    H_sig_ = dU2_
    return H_rho_, H_m_, H_sig_

# ----------------------------------------------------------------------
# AVF discrete gradient: bar-D H(z^n,z^{n+1}) = int_0^1 DH(z^n+xi(z^{n+1}-z^n)) dxi,
# approximated with NQ-point Gauss-Legendre quadrature on [0,1].
# ----------------------------------------------------------------------
gl_nodes, gl_weights = np.polynomial.legendre.leggauss(NQ)   # on [-1,1]
xi_pts = 0.5 * (gl_nodes + 1.0)          # map to [0,1]
xi_wts = 0.5 * gl_weights

eta_avg = 0
u_avg   = 0
T_avg   = 0
for xi_k, w_k in zip(xi_pts, xi_wts):
    rho_xi   = rho0   + xi_k * (rho   - rho0)
    m_xi     = m0     + xi_k * (m     - m0)
    sigma_xi = sigma0 + xi_k * (sigma - sigma0)
    Hr, Hm, Hs = H_derivs(rho_xi, m_xi, sigma_xi)
    eta_avg = eta_avg + w_k * Hr
    u_avg   = u_avg   + w_k * Hm
    T_avg   = T_avg   + w_k * Hs

# Midpoint of the dynamical ("structure") variables -- NOT discrete-gradient
rho_mid   = 0.5 * (rho + rho0)
m_mid     = 0.5 * (m + m0)
sigma_mid = 0.5 * (sigma + sigma0)

# ----------------------------------------------------------------------
# Discrete Poisson bracket contribution, a(u_h; v_h) -- eq. (14).
# Structure coefficients (rho,m,sigma) at the midpoint; functional
# derivatives (eta,u,T) are the AVF-projected fields from the mixed space,
# enforced equal to eta_avg,u_avg,T_avg via c_form below.
# ----------------------------------------------------------------------
a_form = (
    m_mid * u.dx(0) * phi_m * dx - m_mid * u * phi_m.dx(0) * dx
    + rho_mid * eta.dx(0) * phi_m * dx - rho_mid * u * phi_rho.dx(0) * dx
    + sigma_mid * T.dx(0) * phi_m * dx - sigma_mid * u * phi_sigma.dx(0) * dx
)

# ----------------------------------------------------------------------
# Discrete metriplectic (dissipative) bracket contribution, b(u_h; v_h) -- eq. (15).
# Depends only on the functional derivatives (u, T), which are already the
# AVF discrete-gradient fields once c_form is satisfied.
# ----------------------------------------------------------------------
b_form = Re_inv * (
    u.dx(0) * phi_m.dx(0) * dx
    - (u.dx(0) ** 2 / T) * phi_sigma * dx
    + (1 / Pr) * (gamma / (gamma - 1)) * (
        (T.dx(0) / T) * phi_sigma.dx(0) * dx
        - (T.dx(0) ** 2 / T ** 2) * phi_sigma * dx
    )
)

# ----------------------------------------------------------------------
# Algebraic constraints: project the AVF discrete gradient onto V_h,
# c(u_h; v_h) -- eq. (16), now evaluated with the AVF-averaged expressions
# rather than a pointwise evaluation of H's functional derivatives.
# ----------------------------------------------------------------------
c_form = (
    (eta - eta_avg) * phi_eta * dx
    + (u - u_avg) * phi_u * dx
    + (T - T_avg) * phi_T * dx
)

# ----------------------------------------------------------------------
# Full one-step residual. Only rho, m, sigma carry a time derivative; the
# bracket is evaluated with midpoint structure + AVF discrete-gradient
# functional derivatives, which is what guarantees exact discrete energy
# conservation and sign-definite discrete entropy production:
#   H(z^{n+1}) - H(z^n) = bar-D H . (z^{n+1}-z^n)   (discrete-gradient identity)
#                        = dt * (bar-D H)^T J_mid (bar-D H) = 0  (antisymmetry)
# ----------------------------------------------------------------------
dt = Constant(dt_val)
F = (
    (rho - rho0) / dt * phi_rho * dx
    + (m - m0) / dt * phi_m * dx
    + (sigma - sigma0) / dt * phi_sigma * dx
    + a_form + b_form + c_form
)

# ----------------------------------------------------------------------
# Initial conditions (paper, Sec. 4)
# ----------------------------------------------------------------------
rho_ic, m_ic, sigma_ic, eta_ic, u_ic, T_ic = w_old.subfunctions

rho_ic.interpolate(Constant(1.0))
m_ic.interpolate(0.5 * sin(2 * pi * x / L))   # m_h(x,0) = (1/2) sin(2 pi x / L)
sigma_ic.interpolate(Constant(0.5))

u_ic.interpolate(m_ic / rho_ic)
s_ic = sigma_ic / rho_ic
T_ic.interpolate((gamma - 1) * rho_ic ** (gamma - 1) * exp((gamma - 1) * s_ic))
eta_ic.interpolate(
    -0.5 * (m_ic / rho_ic) ** 2
    + rho_ic ** (gamma - 1) * exp((gamma - 1) * s_ic)
    + rho_ic * (gamma - 1) * rho_ic ** (gamma - 2) * exp((gamma - 1) * s_ic)
    - s_ic * T_ic
)

w.assign(w_old)   # initial guess for the first nonlinear solve

# ----------------------------------------------------------------------
# Nonlinear solver (one bespoke AVF step per call; no Irksome, since AVF is
# not a finite-stage Runge-Kutta/collocation method for this non-polynomial H)
# ----------------------------------------------------------------------
solver_parameters = {
    "mat_type": "aij",
    "snes_type": "newtonls",
    "snes_rtol": 1e-12,
    "snes_atol": 1e-14,
    "ksp_type": "preonly",
    "pc_type": "lu",
    "pc_factor_mat_solver_type": "mumps",
}

problem = NonlinearVariationalProblem(F, w)
solver = NonlinearVariationalSolver(problem, solver_parameters=solver_parameters)

# ----------------------------------------------------------------------
# Diagnostics: discrete Hamiltonian (energy), entropy, and mass, eq. (9)
# ----------------------------------------------------------------------
def diagnostics(state):
    rho_, m_, sigma_, eta_, u_, T_ = state.subfunctions
    s_ = sigma_ / rho_
    U_ = rho_ ** (gamma - 1) * exp((gamma - 1) * s_)
    Hval = assemble((0.5 * m_ ** 2 / rho_ + rho_ * U_) * dx)
    Sval = assemble(sigma_ * dx)
    Mval = assemble(rho_ * dx)
    return Hval, Sval, Mval

H0, S0, M0 = diagnostics(w_old)
print(f"t={0.0:8.3f}  H={H0: .10e}  S={S0: .10e}  M={M0: .10e}")

# ----------------------------------------------------------------------
# Storage for diagnostics time series (eq. 18/19-type plots: relative
# energy/mass error, entropy growth) and field-profile snapshots
# (rho, u, T, s at several times, as in the paper's Figs. showing the
# viscous/thermal relaxation of the initial sinusoidal velocity perturbation).
# ----------------------------------------------------------------------
x_coords = Function(V).interpolate(x).dat.data_ro.copy()
sort_idx = np.argsort(x_coords)
x_sorted = x_coords[sort_idx]

t_hist = [0.0]
H_hist = [H0]
S_hist = [S0]
M_hist = [M0]

n_snaps = 6                       # number of field-profile snapshots to keep
snap_times_target = np.linspace(0.0, t_end, n_snaps)
snap_times = []
rho_snaps, u_snaps, T_snaps, s_snaps = [], [], [], []

def store_snapshot(state, t):
    rho_, m_, sigma_, eta_, u_, T_ = state.subfunctions
    s_ = Function(V).interpolate(sigma_ / rho_)
    snap_times.append(t)
    rho_snaps.append(rho_.dat.data_ro.copy()[sort_idx])
    u_snaps.append(u_.dat.data_ro.copy()[sort_idx])
    T_snaps.append(T_.dat.data_ro.copy()[sort_idx])
    s_snaps.append(s_.dat.data_ro.copy()[sort_idx])

store_snapshot(w_old, 0.0)
next_snap = 1

# ----------------------------------------------------------------------
# Time loop
# ----------------------------------------------------------------------
nsteps = int(round(t_end / dt_val))
tcur = 0.0
for step in range(nsteps):
    solver.solve()
    w_old.assign(w)        # accept step: z^n <- z^{n+1}
    tcur += dt_val

    if (step + 1) % 50 == 0:
        Hval, Sval, Mval = diagnostics(w_old)
        print(f"t={tcur:8.3f}  H={Hval: .10e}  S={Sval: .10e}  M={Mval: .10e}")
        t_hist.append(tcur)
        H_hist.append(Hval)
        S_hist.append(Sval)
        M_hist.append(Mval)

    if next_snap < n_snaps and tcur >= snap_times_target[next_snap] - 1e-9:
        store_snapshot(w_old, tcur)
        next_snap += 1

t_hist = np.array(t_hist)
H_hist = np.array(H_hist)
S_hist = np.array(S_hist)
M_hist = np.array(M_hist)

# ----------------------------------------------------------------------
# Plots mimicking the paper's diagnostics:
#   (1) relative energy error vs t          -- should be ~machine precision
#   (2) relative mass error vs t            -- should be ~machine precision
#   (3) entropy vs t                        -- should be monotonically non-decreasing
#   (4)-(7) field snapshots (rho, u, T, s) at several times -- shows the
#       viscous/thermal diffusion smoothing out the initial sinusoidal
#       velocity perturbation, as in the paper's Sec. 4 figures.
# ----------------------------------------------------------------------
fig, axes = plt.subplots(1, 3, figsize=(15, 4))

axes[0].plot(t_hist, np.abs(H_hist - H0) / np.abs(H0), "-o", ms=3)
axes[0].set_yscale("log")
axes[0].set_xlabel("t")
axes[0].set_ylabel(r"$|H^h(t) - H^h(0)| / |H^h(0)|$")
axes[0].set_title("Relative energy error (AVF)")
axes[0].grid(True, which="both", alpha=0.3)

axes[1].plot(t_hist, np.abs(M_hist - M0) / np.abs(M0), "-o", ms=3, color="tab:green")
axes[1].set_yscale("log")
axes[1].set_xlabel("t")
axes[1].set_ylabel(r"$|M^h(t) - M^h(0)| / |M^h(0)|$")
axes[1].set_title("Relative mass error")
axes[1].grid(True, which="both", alpha=0.3)

axes[2].plot(t_hist, S_hist, "-o", ms=3, color="tab:red")
axes[2].set_xlabel("t")
axes[2].set_ylabel(r"$S^h(t) = \int \sigma_h\,dx$")
axes[2].set_title("Discrete entropy (should be non-decreasing)")
axes[2].grid(True, alpha=0.3)

fig.tight_layout()
fig.savefig("diagnostics.png", dpi=150)

fig2, axes2 = plt.subplots(2, 2, figsize=(12, 9))
cmap = plt.cm.viridis
colors = [cmap(i / max(len(snap_times) - 1, 1)) for i in range(len(snap_times))]

for arr, ax, ylabel in zip(
    [rho_snaps, u_snaps, T_snaps, s_snaps],
    axes2.ravel(),
    [r"$\rho_h$", r"$u_h = m_h/\rho_h$", r"$T_h$", r"$s_h = \sigma_h/\rho_h$"],
):
    for k, (prof, t_k) in enumerate(zip(arr, snap_times)):
        ax.plot(x_sorted, prof, color=colors[k], label=f"t={t_k:.1f}")
    ax.set_xlabel("x")
    ax.set_ylabel(ylabel)
    ax.grid(True, alpha=0.3)

axes2[0, 0].legend(fontsize=8, loc="best")
fig2.suptitle("Field profiles: viscous/thermal relaxation of the initial perturbation")
fig2.tight_layout()
fig2.savefig("field_snapshots.png", dpi=150)

print("Saved plots: diagnostics.png, field_snapshots.png")
