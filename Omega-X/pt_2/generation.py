import sympy as sp
from sympy import fcode

# ============================================================
# 1. SYMBOLS AND OBSERVABLES
# ============================================================
x, t = sp.symbols('x t')
gamma, Cv, Re, Pr = sp.symbols('gamma Cv Re Pr', positive=True)

observables = {
    'm_h': sp.Function('m_h')(x, t),
    'rho_h': sp.Function('rho_h')(x, t),
    'sigma_h': sp.Function('sigma_h')(x, t)
}

test_funcs = {
    'm_h': sp.Function('phi_m')(x),
    'rho_h': sp.Function('phi_rho')(x),
    'sigma_h': sp.Function('phi_sigma')(x)
}

# Auxiliary / closure functions
F = sp.Function('F')(x)
K = sp.Function('K')(x)
G = sp.Function('G')(x)
N = sp.Function('N')(x)
T_h = sp.Function('T_h')(x, t)
u_h = sp.Function('u_h')(x, t)
eta_h = sp.Function('eta_h')(x, t)

# ============================================================
# 2. INTERNAL ENERGY AND TEMPERATURE RELATION
# ============================================================
s = sp.Symbol('s')
rho_h = observables['rho_h']
sigma_h = observables['sigma_h']
U_expr = Cv * sp.exp(s / Cv) * rho_h**(gamma - 1) / (gamma - 1)
T_expr = sp.diff(U_expr, s).subs(s, sigma_h / rho_h)

# ============================================================
# 3. LINEAR L2 AND INTEGRAL WRAPPERS
# ============================================================
class L2Linear(sp.Function):
    nargs = 2
    @classmethod
    def eval(cls, f, g):
        if g == 0 or f == 0:
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

# ============================================================
# 4. FULL BRACKET INPUT (POISSON + METRIPLECTIC)
# ============================================================
full_bracket = (
    # Poisson
    -L2(observables['m_h'] * sp.diff(u_h, x), test_funcs['m_h'])
    + L2(observables['m_h'] * u_h, sp.diff(test_funcs['m_h'], x))
    -L2(observables['rho_h'] * sp.diff(eta_h, x), test_funcs['m_h'])
    + L2(observables['rho_h'] * u_h, sp.diff(test_funcs['rho_h'], x))
    -L2(observables['sigma_h'] * sp.diff(T_h, x), test_funcs['m_h'])
    + L2(observables['sigma_h'] * u_h, sp.diff(test_funcs['sigma_h'], x))
    # Metriplectic (dissipative)
    - 1/Re * L2(sp.diff(u_h, x), test_funcs['m_h'])
    - L2(((sp.diff(u_h, x))**2) / T_h, test_funcs['sigma_h'])
    + 1/Pr * (gamma/(gamma - 1)) * (
        L2(sp.diff(T_h, x)/T_h, sp.diff(test_funcs['sigma_h'], x))
        - L2((sp.diff(T_h, x))**2 / T_h**2, test_funcs['sigma_h'])
    )
)

# ============================================================
# 5. ZERO REMOVAL
# ============================================================
def remove_zero_terms(expr, zero_funcs):
    if expr in zero_funcs:
        return sp.S(0)
    if expr.is_Atom:
        return expr
    new_args = [remove_zero_terms(a, zero_funcs) for a in expr.args]
    return expr.func(*new_args)

# ============================================================
# 6. EVOLUTION EQUATION DERIVATION
# ============================================================
def derive_evolution(full_bracket, observables, test_funcs):
    evol_eqs = {}
    for obs_name, obs_func in observables.items():
        rhs = full_bracket
        zero_funcs = [tf for key, tf in test_funcs.items() if key != obs_name]
        rhs_clean = remove_zero_terms(rhs, zero_funcs)
        rhs_clean = sp.simplify(sp.expand(rhs_clean))
        obs_dot = sp.diff(obs_func, t)
        evol_eqs[obs_name] = sp.Eq(obs_dot, rhs_clean)
    return evol_eqs

evol_eqs = derive_evolution(full_bracket, observables, test_funcs)

sp.init_printing()
print("Temperature expression T_expr:")
sp.pprint(T_expr)

# ============================================================
# 7. CODE GENERATION HELPERS
# ============================================================
def replace_L2(expr):
    if expr.func == L2Linear:
        f, g = expr.args
        return f * g
    if expr.func == IntegralLinear:
        (arg,) = expr.args
        return arg
    if expr.is_Atom:
        return expr
    new_args = [replace_L2(a) for a in expr.args]
    return expr.func(*new_args)

def replace_functions_with_symbols(expr, observables):
    replacements = {observables[k]: sp.Symbol(k) for k in observables}
    replacements.update({
        u_h: sp.Symbol('u_h'),
        T_h: sp.Symbol('T_h'),
        eta_h: sp.Symbol('eta_h')
    })
    return expr.xreplace(replacements)

def replace_derivatives(expr):
    derivs = {}
    for d in expr.atoms(sp.Derivative):
        func = d.expr
        vars_ = d.variables
        if len(vars_) == 1:
            var = vars_[0]
            func_name = func.func.__name__
            var_name = str(var)
            sym_name = f'd{func_name}_d{var_name}'
            derivs[d] = sp.Symbol(sym_name)
        else:
            derivs[d] = sp.Symbol("unsupported_deriv")
    return expr.xreplace(derivs)

# ============================================================
# 8. FORTRAN CODE GENERATION
# ============================================================
print("\nGenerated Fortran code:\n")
for obs_name, eq in evol_eqs.items():
    rhs_clean = replace_L2(eq.rhs)
    rhs_final = replace_derivatives(replace_functions_with_symbols(rhs_clean, observables))
    rhs_fcode = fcode(rhs_final, assign_to=f'd{obs_name}_dt', source_format='free', standard=95)
    print(f"! Fortran code for {obs_name}")
    print(rhs_fcode)
    print()
