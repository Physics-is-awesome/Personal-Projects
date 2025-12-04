
"""
Soft-coded SymPy -> Fortran generator (argument-based, 3D arrays, no room for errors)

- Dynamically classifies symbols as scalars or arrays.
- Generates Fortran modules with assumed-shape arrays (:,:,:).
- Applies RHS element-wise with do concurrent loops.
- Avoids SymPy temporaries by emitting expression-only fcode and indexifying arrays.

"""

import os
import re
import sympy as sp
from sympy import fcode

# -----------------------------
# 1) Symbols, observables, basis
# -----------------------------
x, t = sp.symbols('x t')
gamma, Cv, Re, Pr = sp.symbols('gamma Cv Re Pr', positive=True)

# Observables (fields depending on x,t)
observables = {
    'm_h': sp.Function('m_h')(x, t),
    'rho_h': sp.Function('rho_h')(x, t),
    'sigma_h': sp.Function('sigma_h')(x, t),
}

# Basis/test functions (depend on x only)
basis_funcs = {
    'm_h': sp.Function('phi_m')(x),
    'rho_h': sp.Function('phi_rho')(x),
    'sigma_h': sp.Function('phi_sigma')(x),
}

# Additional fields used in equations
T_h = sp.Function('T_h')(x, t)
u_h = sp.Function('u_h')(x, t)
eta_h = sp.Function('eta_h')(x, t)

# Example thermodynamics (adaptable; generator handles changes)
s = sp.Symbol('s')
rho_sym = observables['rho_h']
sigma_sym = observables['sigma_h']
U_expr = Cv * sp.exp(s / Cv) * rho_sym**(gamma - 1) / (gamma - 1)
T_expr = sp.diff(U_expr, s).subs(s, sigma_sym / rho_sym)

# -----------------------------
# 2) L2 wrapper (weak form term)
# -----------------------------
class L2Linear(sp.Function):
    nargs = 2
    @classmethod
    def eval(cls, f, g):
        # Optional quick simplification
        if f == 0 or g == 0:
            return sp.S(0)

def L2(f, g):
    return L2Linear(f, g)

# -----------------------------
# 3) Full bracket (edit freely)
# -----------------------------
full_bracket = (
    -L2(observables['m_h']*sp.diff(u_h, x), basis_funcs['m_h'])
    + L2(observables['m_h']*u_h, sp.diff(basis_funcs['m_h'], x))
    -L2(observables['rho_h']*sp.diff(eta_h, x), basis_funcs['m_h'])
    + L2(observables['rho_h']*u_h, sp.diff(basis_funcs['rho_h'], x))
    -L2(observables['sigma_h']*sp.diff(T_h, x), basis_funcs['m_h'])
    + L2(observables['sigma_h']*u_h, sp.diff(basis_funcs['sigma_h'], x))
    -1/Re * L2(sp.diff(u_h, x), basis_funcs['m_h'])
    - L2((sp.diff(u_h, x))**2/T_h, basis_funcs['sigma_h'])
    + 1/Pr * (gamma/(gamma-1)) * (
        L2(sp.diff(T_h, x)/T_h, sp.diff(basis_funcs['sigma_h'], x))
        - L2((sp.diff(T_h, x))**2/T_h**2, basis_funcs['sigma_h'])
    )
)

# -----------------------------
# 4) Evolution equations from bracket
# -----------------------------
def remove_zero_terms(expr, zero_funcs):
    """Zero out terms containing basis functions not relevant to the observable."""
    if expr in zero_funcs:
        return sp.S(0)
    if expr.is_Atom:
        return expr
    return expr.func(*[remove_zero_terms(a, zero_funcs) for a in expr.args])

def derive_evolution(full_bracket, observables, basis_funcs):
    """Build d/dt f = RHS (weak form) for each observable f."""
    evol_eqs = {}
    for obs_name, obs_func in observables.items():
        # Only keep terms with basis matching this observable
        zero_funcs = [tf for key, tf in basis_funcs.items() if key != obs_name]
        rhs = remove_zero_terms(full_bracket
