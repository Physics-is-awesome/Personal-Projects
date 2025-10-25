import sympy as sp

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
    'm_h': sp.Function('phi_m')(x),
    'rho_h': sp.Function('phi_rho')(x),
    'sigma_h': sp.Function('phi_sigma')(x)
}

# Auxiliary functions for dissipative 4-bracket
F = sp.Function('F')(x)
K = sp.Function('K')(x)
G = sp.Function('G')(x)
N = sp.Function('N')(x)
T_h = sp.Function('T_h')(x, t)

# -----------------------------
# 2. Internal energy and temperature
# -----------------------------
s = sp.Symbol('s')
rho_h = observables['rho_h']
sigma_h = observables['sigma_h']
U_expr = Cv * sp.exp(s / Cv) * rho_h**(gamma - 1) / (gamma - 1)
T_expr = sp.diff(U_expr, s).subs(s, sigma_h / rho_h)

# -----------------------------
# 3. Linear symbolic wrappers for automatic zero propagation
# -----------------------------
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

# Redefine L2 and Integral to use linear wrappers
def L2(f, g):
    return L2Linear(f, g)

def Integral(expr):
    return IntegralLinear(expr)

# -----------------------------
# 4. Full bracket input
# -----------------------------
# Replace this with the full Poisson + metriplectic 4-bracket
full_bracket = (
    # Poisson part
    -L2(m_h * sp.diff(m_h, x), phi_m) + L2(m_h * m_h, sp.diff(phi_m, x))
    -L2(rho_h * sp.diff(sigma_h, x), phi_rho) + L2(rho_h * m_h, sp.diff(phi_rho, x))
    -L2(sigma_h * sp.diff(T_h, x), phi_sigma) + L2(sigma_h * m_h, sp.diff(phi_sigma, x))
    # Metriplectic dissipative part
    -1/Re * Integral(1/T_h * (K.diff(x)*F.diff(x) - F.diff(x)*K.diff(x)) *
                     (N.diff(x)*G.diff(x) - G.diff(x)*N.diff(x)))
    +1/( (Pr*(gamma-1)) ) * Integral(1/T_h * (K.diff(x)*F.diff(x) - F.diff(x)*K.diff(x)) *
                                     (N.diff(x)*G.diff(x) - G.diff(x)*N.diff(x)))
)

# -----------------------------
# 5. Recursive zero removal
# -----------------------------
def remove_zero_terms(expr, zero_funcs):
    """Recursively replace any expression depending on zero test functions with 0"""
    if expr in zero_funcs:
        return sp.S(0)
    if expr.is_Atom:
        return expr
    new_args = [remove_zero_terms(a, zero_funcs) for a in expr.args]
    return expr.func(*new_args)

# -----------------------------
# 6. Derive evolution equations
# -----------------------------
def derive_evolution(full_bracket, observables, test_funcs):
    evol_eqs = {}
    for obs_name, obs_func in observables.items():
        rhs = full_bracket
        # Zero all other test functions
        zero_funcs = [tf for key, tf in test_funcs.items() if key != obs_name]
        rhs_clean = remove_zero_terms(rhs, zero_funcs)
        rhs_clean = sp.expand(rhs_clean)
        rhs_clean = sp.cancel(rhs_clean)
        rhs_clean = sp.simplify(rhs_clean)
        # Time derivative
        obs_dot = sp.diff(obs_func, t)
        evol_eqs[obs_name] = sp.Eq(obs_dot, rhs_clean)
    return evol_eqs

# -----------------------------
# 7. Compute and display
# -----------------------------
evol_eqs = derive_evolution(full_bracket, observables, test_funcs)

sp.init_printing()
print("Temperature expression T_expr:")
sp.pprint(T_expr)

for obs_name, eq in evol_eqs.items():
    print(f"\nEvolution equation for {obs_name}:")
    sp.pprint(eq)
