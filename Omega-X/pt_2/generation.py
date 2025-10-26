import sympy as sp
from sympy import fcode

# -----------------------------
# 1. Symbols
# -----------------------------
x, t = sp.symbols('x t')
gamma, Cv, Re, Pr = sp.symbols('gamma Cv Re Pr', positive=True)

# Observables
observables = {
    'm_h': sp.Function('m_h')(x, t),
    'rho_h': sp.Function('rho_h')(x, t),
    'sigma_h': sp.Function('sigma_h')(x, t)
}

# Basis (test) functions for Galerkin/L2 projection
basis = {
    'm_h': sp.Function('phi_m')(x),
    'rho_h': sp.Function('phi_rho')(x),
    'sigma_h': sp.Function('phi_sigma')(x)
}

# Auxiliary fields
T_h = sp.Function('T_h')(x, t)
u_h = sp.Function('u_h')(x, t)
eta_h = sp.Function('eta_h')(x, t)

# -----------------------------
# 2. Internal energy and temperature
# -----------------------------
s = sp.Symbol('s')
rho_h = observables['rho_h']
sigma_h = observables['sigma_h']
U_expr = Cv * sp.exp(s / Cv) * rho_h**(gamma - 1) / (gamma - 1)
T_expr = sp.diff(U_expr, s).subs(s, sigma_h / rho_h)

# -----------------------------
# 3. L2 / Integral linear wrappers
# -----------------------------
class L2Linear(sp.Function):
    nargs = 2
    @classmethod
    def eval(cls, f, g):
        if f == 0 or g == 0:
            return sp.S(0)

class IntegralLinear(sp.Function):
    nargs = 1
    @classmethod
    def eval(cls, arg):
        if arg == 0:
            return sp.S(0)

def L2(f, g):
    return L2Linear(f, g)

def Integral(expr):
    return IntegralLinear(expr)

# -----------------------------
# 4. Full Poisson + Metriplectic bracket
# -----------------------------
full_bracket = (
    # Poisson
    -L2(observables['m_h']*sp.diff(u_h, x), basis['m_h'])
    + L2(observables['m_h']*u_h, sp.diff(basis['m_h'], x))
    -L2(observables['rho_h']*sp.diff(eta_h, x), basis['m_h'])
    + L2(observables['rho_h']*u_h, sp.diff(basis['rho_h'], x))
    -L2(observables['sigma_h']*sp.diff(T_h, x), basis['m_h'])
    + L2(observables['sigma_h']*u_h, sp.diff(basis['sigma_h'], x))
    # Metriplectic dissipation
    -1/Re * L2(sp.diff(u_h, x), basis['m_h'])
    - L2((sp.diff(u_h, x))**2/T_h, basis['sigma_h'])
    + 1/Pr * (gamma/(gamma-1)) * (
        L2(sp.diff(T_h, x)/T_h, sp.diff(basis['sigma_h'], x))
        - L2((sp.diff(T_h, x))**2/T_h**2, basis['sigma_h'])
    )
)

# -----------------------------
# 5. Zero removal helper
# -----------------------------
def remove_zero_terms(expr, zero_funcs):
    if expr in zero_funcs:
        return sp.S(0)
    if expr.is_Atom:
        return expr
    new_args = [remove_zero_terms(a, zero_funcs) for a in expr.args]
    return expr.func(*new_args)

# -----------------------------
# 6. Derive evolution equations
# -----------------------------
def derive_evolution(full_bracket, observables, basis):
    evol_eqs = {}
    for obs_name, obs_func in observables.items():
        rhs = full_bracket
        # Zero all other test functions
        zero_funcs = [tf for key, tf in basis.items() if key != obs_name]
        rhs_clean = remove_zero_terms(rhs, zero_funcs)
        rhs_clean = sp.expand(rhs_clean)
        rhs_clean = sp.cancel(rhs_clean)
        rhs_clean = sp.simplify(rhs_clean)
        # Time derivative
        obs_dot = sp.diff(obs_func, t)
        evol_eqs[obs_name] = sp.Eq(obs_dot, rhs_clean)
    return evol_eqs

evol_eqs = derive_evolution(full_bracket, observables, basis)

# -----------------------------
# 7. Code generation helpers
# -----------------------------
def replace_L2(expr):
    if expr.func == L2Linear:
        f, g = expr.args
        return f*g
    if expr.func == IntegralLinear:
        return expr.args[0]
    if expr.is_Atom:
        return expr
    new_args = [replace_L2(a) for a in expr.args]
    return expr.func(*new_args)

def replace_derivatives(expr):
    derivs = {}
    for d in expr.atoms(sp.Derivative):
        func = d.expr
        var = d.variables[0]
        derivs[d] = sp.Symbol(f'd{func.func.__name__}_d{var}')
    return expr.xreplace(derivs)

def replace_observables(expr):
    replacements = {obs_func: sp.Symbol(obs_name) for obs_name, obs_func in observables.items()}
    replacements.update({T_h: sp.Symbol('T_h'), u_h: sp.Symbol('u_h'), eta_h: sp.Symbol('eta_h')})
    return expr.xreplace(replacements)

def replace_basis(expr):
    basis_symbols = {
        basis['m_h']: sp.Symbol('phi_m_i'),
        basis['rho_h']: sp.Symbol('phi_rho_i'),
        basis['sigma_h']: sp.Symbol('phi_sigma_i')
    }
    return expr.xreplace(basis_symbols)

# -----------------------------
# 8. Generate Fortran code
# -----------------------------
print("Temperature expression T_expr:")
sp.pprint(T_expr)
print("\nGenerated Fortran code:\n")

for obs_name, eq in evol_eqs.items():
    rhs = eq.rhs
    rhs = replace_L2(rhs)
    rhs = replace_derivatives(rhs)
    rhs = replace_observables(rhs)
    rhs = replace_basis(rhs)
    rhs_fcode = fcode(rhs, assign_to=f'd{obs_name}_dt', source_format='free', standard=95)
    print(f"! Fortran code for {obs_name}")
    print(rhs_fcode)
    print()
