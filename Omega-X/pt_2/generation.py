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

# Test functions (variations)
test_funcs = {
    'phi_m': sp.Function('phi_m')(x),
    'phi_rho': sp.Function('phi_rho')(x),
    'phi_sigma': sp.Function('phi_sigma')(x)
}

# Auxiliary functions for dissipative 4-bracket
F = sp.Function('F')(x)
K = sp.Function('K')(x)
G = sp.Function('G')(x)
N = sp.Function('N')(x)
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
# 3. Linear symbolic wrappers
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
    def eval(cls, integrand):
        if integrand == 0:
            return sp.S(0)

def L2(f, g):
    return L2Linear(f, g)

def Integral(expr):
    return IntegralLinear(expr)

# -----------------------------
# 4. Full bracket (Poisson + Metriplectic)
# -----------------------------
full_bracket = (
    -L2(observables['m_h'] * sp.diff(u_h, x), test_funcs['phi_m'])
    + L2(observables['m_h'] * u_h, sp.diff(test_funcs['phi_m'], x))
    -L2(observables['rho_h'] * sp.diff(eta_h, x), test_funcs['phi_m'])
    + L2(observables['rho_h'] * u_h, sp.diff(test_funcs['phi_rho'], x))
    -L2(observables['sigma_h'] * sp.diff(T_h, x), test_funcs['phi_m'])
    + L2(observables['sigma_h'] * u_h, sp.diff(test_funcs['phi_sigma'], x))
    - 1/Re * L2(sp.diff(u_h, x), test_funcs['phi_m'])
    - L2((sp.diff(u_h, x))**2 / T_h, test_funcs['phi_sigma'])
    + (1/Pr) * (gamma/(gamma-1)) * (
        L2(sp.diff(T_h, x)/T_h, sp.diff(test_funcs['phi_sigma'], x))
        - L2((sp.diff(T_h, x))**2 / T_h**2, test_funcs['phi_sigma'])
    )
)

# -----------------------------
# 5. Remove zero terms recursively
# -----------------------------
def remove_zero_terms(expr, zero_funcs):
    """Remove terms depending on zeroed test functions only."""
    if expr in zero_funcs:
        return sp.S(0)
    if expr.is_Atom:
        return expr
    new_args = [remove_zero_terms(a, zero_funcs) for a in expr.args]
    # If multiplication or L2 has a zero argument, return 0
    if expr.func in [sp.Mul, L2Linear] and any(a == 0 for a in new_args):
        return sp.S(0)
    return expr.func(*new_args)

# -----------------------------
# 6. Derive evolution equations
# -----------------------------
def derive_evolution(full_bracket, observables, test_funcs):
    evol_eqs = {}
    obs_to_test = {
        'm_h': test_funcs['phi_m'],
        'rho_h': test_funcs['phi_rho'],
        'sigma_h': test_funcs['phi_sigma']
    }
    for obs_name, obs_func in observables.items():
        rhs = full_bracket
        zero_funcs = [tf for key, tf in obs_to_test.items() if key != obs_name]
        rhs_clean = remove_zero_terms(rhs, zero_funcs)
        rhs_clean = sp.expand(rhs_clean)
        rhs_clean = sp.cancel(rhs_clean)
        rhs_clean = sp.simplify(rhs_clean)
        obs_dot = sp.diff(obs_func, t)
        evol_eqs[obs_name] = sp.Eq(obs_dot, rhs_clean)
    return evol_eqs

evol_eqs = derive_evolution(full_bracket, observables, test_funcs)

# -----------------------------
# 7. Display
# -----------------------------
sp.init_printing()
print("Temperature expression T_expr:")
sp.pprint(T_expr)

for obs_name, eq in evol_eqs.items():
    print(f"\nEvolution equation for {obs_name}:")
    sp.pprint(eq)

# -----------------------------
# 8. Prepare Fortran code
# -----------------------------
def replace_L2(expr):
    if expr.func == L2Linear:
        f, g = expr.args
        return f * g
    if expr.func == IntegralLinear:
        return expr.args[0]
    if expr.is_Atom:
        return expr
    new_args = [replace_L2(a) for a in expr.args]
    return expr.func(*new_args)

def replace_functions_with_symbols(expr):
    replacements = {obs_func: sp.Symbol(obs_name) for obs_name, obs_func in observables.items()}
    replacements.update({u_h: sp.Symbol('u_h'), T_h: sp.Symbol('T_h'), eta_h: sp.Symbol('eta_h')})
    for tf in test_funcs.values():
        replacements[tf] = sp.Symbol(tf.func.__name__)
    return expr.xreplace(replacements)

def replace_derivatives(expr):
    derivs = {}
    for d in expr.atoms(sp.Derivative):
        func = d.expr
        var = d.variables[0]
        sym_name = f'd{func.func.__name__}_d{var}'
        derivs[d] = sp.Symbol(sym_name)
    return expr.xreplace(derivs)

print("\nGenerated Fortran code:\n")
for obs_name, eq in evol_eqs.items():
    rhs_clean = replace_L2(eq.rhs)
    rhs_final = replace_derivatives(replace_functions_with_symbols(rhs_clean))
    rhs_fcode = fcode(rhs_final, assign_to=f'd{obs_name}_dt', source_format='free', standard=95)
    print(f"! Fortran code for {obs_name}")
    print(rhs_fcode)
    print()
