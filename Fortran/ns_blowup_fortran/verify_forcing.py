#!/usr/bin/env python3
"""
verify_forcing.py -- independent check of the axisymmetric (Hou-Li) equations and of
the force in forcing_gen.f90 against the *Cartesian* 3D Navier-Stokes residual.

For the same exact flow we build u(x,y,z,t) in Cartesian components, form
    R = u_t + (u.grad)u - Lap(u)       (pressure drops out of f_theta and of curl f)
and compare   e_theta.R / r   with F1   and   e_theta.(curl R) / r   with F2.
"""
import random
import sympy as sp

x, y, z, t = sp.symbols('x y z t', real=True)
hh, p0, f0 = 0.01, 5.0, 8.0
half = sp.Rational(1, 2)

tau = 1 - t
a, b = sp.sqrt(tau), tau**(half - hh)
sw = sp.sin(t)**2 + t            # any smooth switch; the algebra does not depend on it
r2 = x**2 + y**2
env = sp.exp(-r2 / tau - z**2 / b**2)
psi1 = sw * tau**(-half - hh) * p0 * (z / b) * env
u1 = sw * tau**(-1 - hh) * f0 * env

# Cartesian velocity from u_r = -r psi1_z, u_theta = r u1, u_z = 2 psi1 + r psi1_r
psi1_z = sp.diff(psi1, z)
# r*psi1_r = x psi1_x + y psi1_y
ux = -x * psi1_z - y * u1
uy = -y * psi1_z + x * u1
uz = 2 * psi1 + x * sp.diff(psi1, x) + y * sp.diff(psi1, y)
U = sp.Matrix([ux, uy, uz])
X = (x, y, z)

div = sum(sp.diff(U[i], X[i]) for i in range(3))
R = sp.Matrix([sp.diff(U[i], t) + sum(U[j] * sp.diff(U[i], X[j]) for j in range(3))
               - sum(sp.diff(U[i], X[j], 2) for j in range(3)) for i in range(3)])
curlR = sp.Matrix([sp.diff(R[2], y) - sp.diff(R[1], z),
                   sp.diff(R[0], z) - sp.diff(R[2], x),
                   sp.diff(R[1], x) - sp.diff(R[0], y)])

fdiv = sp.lambdify((x, y, z, t), div, 'math')
fR = sp.lambdify((x, y, z, t), list(R), 'math')
fC = sp.lambdify((x, y, z, t), list(curlR), 'math')

# the generated force, evaluated through gfortran would be circular; instead re-derive
# F1,F2 from gen_forcing.py's sympy expressions
import importlib.util, sys, io, contextlib
spec = importlib.util.spec_from_file_location('gen', 'gen_forcing.py')
gen = importlib.util.module_from_spec(spec)
with contextlib.redirect_stdout(io.StringIO()):
    spec.loader.exec_module(gen)
# gen.F1/F2 contain S0 (=sw) and S1 (=sw')
tt = sp.Symbol('t')
subsd = {gen.hh: hh, gen.p0: p0, gen.f0: f0}
F1s = gen.F1.subs(subsd)
F2s = gen.F2.subs(subsd)
sw_t = sp.sin(gen.t)**2 + gen.t
F1n = sp.lambdify((gen.r, gen.z, gen.t), F1s.subs({gen.S0: sw_t, gen.S1: sp.diff(sw_t, gen.t)}), 'math')
F2n = sp.lambdify((gen.r, gen.z, gen.t), F2s.subs({gen.S0: sw_t, gen.S1: sp.diff(sw_t, gen.t)}), 'math')

random.seed(1)
worst1 = worst2 = worstdiv = 0.0
for _ in range(6):
    xv, yv = random.uniform(-0.6, 0.6), random.uniform(-0.6, 0.6)
    zv, tv = random.uniform(-0.5, 0.5), random.uniform(0.3, 0.85)
    rv = (xv**2 + yv**2) ** 0.5
    Rv = fR(xv, yv, zv, tv)
    Cv = fC(xv, yv, zv, tv)
    eth = (-yv / rv, xv / rv, 0.0)
    f_theta = sum(eth[i] * Rv[i] for i in range(3))
    curl_theta = sum(eth[i] * Cv[i] for i in range(3))
    a1, a2 = f_theta / rv, curl_theta / rv
    b1, b2 = F1n(rv, zv, tv), F2n(rv, zv, tv)
    worst1 = max(worst1, abs(a1 - b1) / (abs(a1) + 1e-30))
    worst2 = max(worst2, abs(a2 - b2) / (abs(a2) + 1e-30))
    worstdiv = max(worstdiv, abs(fdiv(xv, yv, zv, tv)))
print(f"max |div u|                 = {worstdiv:.2e}")
print(f"max rel err  F1 vs Cartesian = {worst1:.2e}")
print(f"max rel err  F2 vs Cartesian = {worst2:.2e}")
assert worstdiv < 1e-10 and worst1 < 1e-8 and worst2 < 1e-8
print("PASS")
