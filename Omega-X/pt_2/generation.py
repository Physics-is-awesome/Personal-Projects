import sympy as sp
from sympy import fcode

# -----------------------------
# 1. Symbols and observables
# -----------------------------
x, t = sp.symbols('x t')                     # spatial and time symbols
gamma, Cv, Re, Pr = sp.symbols('gamma Cv Re Pr', positive=True)

# Observables as functions (symbolic in x and t)
m_h = sp.Function('m_h')(x, t)
rho_h = sp.Function('rho_h')(x, t)
sigma_h = sp.Function('sigma_h')(x, t)
u_h = sp.Function('u_h')(x, t)
T_h = sp.Function('T_h')(x, t)
eta_h = sp.Function('eta_h')(x, t)

observables = {
    'm_h': m_h,
    'rho_h': rho_h,
    'sigma_h': sigma_h,
    'u_h': u_h,
    'T_h': T_h,
    'eta_h': eta_h
}

# Test / basis functions: use plain Symbols so Fortran printing works
phi_m = sp.Symbol('phi_m')
phi_rho = sp.Symbol('phi_rho')
phi_sigma = sp.Symbol('phi_sigma')

test_funcs = {
    'm_h': phi_m,
    'rho_h': phi_rho,
    'sigma_h': phi_sigma
}

# Auxiliary functions used in dissipative 4-bracket (kept symbolic)
F = sp.Function('F')(x)
K = sp.Function('K')(x)
G = sp.Function('G')(x)
N = sp.Function('N')(x)

# -----------------------------
# 2. Internal energy and temperature (example EOS)
# -----------------------------
s = sp.Symbol('s')  # placeholder for entropy per mass during differentiation
U_expr = Cv * sp.exp(s / Cv) * rho_h**(gamma - 1) / (gamma - 1)
T_expr = sp.diff(U_expr, s).subs(s, sigma_h / rho_h)   # T = dU/ds | s = sigma/rho

# -----------------------------
# 3. Linear wrappers for L2 and Integral (help symbolic simplification)
# -----------------------------
class L2Linear(sp.Function):
    nargs = 2
    @classmethod
    def eval(cls, f, g):
        # if either argument is literally zero, return 0
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
# 4. Full Poisson + metriplectic weak-form bracket
#    (Replace/extend these lines with your full bracket)
# -----------------------------
full_bracket = (
    # Poisson part (example weak-form terms)
    - L2(observables['m_h'] * sp.diff(observables['u_h'], x), test_funcs['m_h']) \
    + L2(observables['m_h'] * observables['u_h'], sp.diff(test_funcs['m_h'], x)) \
    - L2(observables['rho_h'] * sp.diff(observables['eta_h'], x), test_funcs['m_h']) \
    + L2(observables['rho_h'] * observables['u_h'], sp.diff(test_funcs['rho_h'], x)) \
    - L2(observables['sigma_h'] * sp.diff(observables['T_h'], x), test_funcs['m_h']) \
    + L2(observables['sigma_h'] * observables['u_h'], sp.diff(test_funcs['sigma_h'], x))
    # Metriplectic/dissipative parts (example)
) + (
    - (1 / Re) * L2(sp.diff(observables['u_h'], x), test_funcs['m_h'])
    - L2((sp.diff(observables['u_h'], x)**2) / observables['T_h'], test_funcs['sigma_h'])
    + (1/Pr) * (gamma/(gamma - 1)) * (
        L2(sp.diff(observables['T_h'], x) / observables['T_h'], sp.diff(test_funcs['sigma_h'], x))
        - L2((sp.diff(observables['T_h'], x)**2) / observables['T_h']**2, test_funcs['sigma_h'])
    )
)

# (You should replace the above template with your exact bracket expressions
#  from the paper/Omega-X notes; this is the structural example.)

# -----------------------------
# 5. Remove zero-test-function dependencies
# -----------------------------
def remove_zero_terms(expr, zero_funcs):
    """
    Recursively replace any subtree equal to any zero_func with 0.
    This alone is not enough if a zero_func appears within a Function wrapper
    (e.g. L2(f, phi_rho)); we use L2Linear to help and later we will
    remove L2Linear wrappers before codegen.
    """
    # direct match
    if expr in zero_funcs:
        return sp.S(0)
    # atomic items: nothing to do
    if expr.is_Atom:
        return expr
    # otherwise recurse on args
    new_args = [remove_zero_terms(a, zero_funcs) for a in expr.args]
    return expr.func(*new_args)

# -----------------------------
# 6. Derive evolution equations for each observable
#    by zeroing out other test functions, expanding, simplifying
# -----------------------------
def derive_evolution(full_bracket, observables, test_funcs):
    evol_eqs = {}
    for obs_name, obs_func in observables.items():
        # skip auxiliary / non-evolved observables if needed (here we derive for keys in test_funcs)
        if obs_name not in test_funcs:
            # if you want evolution for u_h, T_h, etc., add to test_funcs and observables dict
            continue

        rhs = full_bracket
        # zero all test functions except the one paired with obs_name
        zero_map = {phi: 0 for key, phi in test_funcs.items() if key != obs_name}
        # substitute zeros first (will eliminate many terms)
        rhs_sub = rhs.subs(zero_map)
        # recursive removal of any nested occurrences
        zero_funcs = list(zero_map.keys())
        rhs_clean = remove_zero_terms(rhs_sub, zero_funcs)
        # further algebraic simplification/expansion
        rhs_clean = sp.simplify(sp.expand(rhs_clean))
        # time derivative of the observable
        obs_dot = sp.diff(observables[obs_name], t)
        evol_eqs[obs_name] = sp.Eq(obs_dot, rhs_clean)
    return evol_eqs

evol_eqs = derive_evolution(full_bracket, observables, test_funcs)

# show temperature and symbolic equations
sp.init_printing()
print("\n--- Temperature expression T_expr ---")
sp.pprint(T_expr)
print("\n--- Derived (symbolic) evolution equations ---")
for k, eq in evol_eqs.items():
    print(f"\nEvolution for {k}:")
    sp.pprint(eq)

# -----------------------------
# 7. Helpers to convert expressions to code-friendly form
#    - remove L2Linear/IntegralLinear wrappers
#    - replace Function(x,t) -> Symbol('name')
#    - replace Derivative(...) -> Symbol('d<name>_d<x/dt>')
# -----------------------------
def strip_wrappers(expr):
    """Replace L2Linear(f,g) -> f*g and IntegralLinear(arg) -> arg."""
    if expr.func == L2Linear:
        f, g = expr.args
        return strip_wrappers(f) * strip_wrappers(g)
    if expr.func == IntegralLinear:
        (arg,) = expr.args
        return strip_wrappers(arg)
    if expr.is_Atom:
        return expr
    return expr.func(*[strip_wrappers(a) for a in expr.args])

def function_to_symbol_map(observables_map):
    """Make mapping from Function(x,t) objects to simple Symbols for codegen."""
    mapping = {}
    for name, func in observables_map.items():
        mapping[func] = sp.Symbol(name)   # e.g. m_h(x,t) -> Symbol('m_h')
    return mapping

def replace_functions_with_symbols(expr, mapping):
    """Apply mapping to expr (use xreplace for atomic replacements)."""
    return expr.xreplace(mapping)

def replace_derivatives_with_symbols(expr):
    """
    Replace Derivative(f(x,t), var) with a named Symbol.
    Example: Derivative(m_h(x,t), x) -> d_m_h_dx
             Derivative(m_h(x,t), t) -> d_m_h_dt
    """
    deriv_map = {}
    for d in expr.atoms(sp.Derivative):
        # d.expr is the function, d.variables is tuple of variables
        func = d.expr                     # e.g. m_h(x,t) (AppliedUndef)
        vars_ = d.variables               # e.g. (x,) or (t,)
        # construct a name
        func_name = func.func.__name__ if hasattr(func, 'func') else str(func)
        # care: func may be like m_h(x,t); func.func.__name__ -> 'm_h'
        if len(vars_) == 1:
            var = vars_[0]
            var_name = 'x' if var == x else 't' if var == t else str(var)
            name = f'd{func_name}_d{var_name}'
            deriv_map[d] = sp.Symbol(name)
        else:
            # fallback: create generic name with concatenated var names
            var_names = '_'.join('x' if v == x else 't' if v == t else str(v) for v in vars_)
            name = f'd{func_name}_d{var_names}'
            deriv_map[d] = sp.Symbol(name)
    return expr.xreplace(deriv_map)

# -----------------------------
# 8. Prepare mapping and generate Fortran code
# -----------------------------
func_to_sym = function_to_symbol_map(observables)

print("\n--- Fortran-ready code strings ---\n")
for obs_name, eq in evol_eqs.items():
    # 1) strip the L2/Integral wrappers (turn them into normal algebra)
    stripped = strip_wrappers(eq.rhs)
    # 2) replace Function(x,t) objects with simple Symbols (m_h, rho_h, etc.)
    replaced = replace_functions_with_symbols(stripped, func_to_sym)
    # 3) replace derivatives with named symbols
    replaced = replace_derivatives_with_symbols(replaced)
    # 4) (optional) final simplification
    replaced = sp.simplify(replaced)
    # 5) produce Fortran code (assign_to name)
    assign_to = f'd{obs_name}_dt'   # e.g. dm_h_dt
    try:
        fortran_str = fcode(replaced, assign_to=assign_to, source_format='free', standard=95)
    except Exception as e:
        # fall back to non-strict fcode if minor issues, but we prefer explicit replacements above
        fortran_str = fcode(replaced, assign_to=assign_to, source_format='free', standard=95, strict=False)
    print(f"! Fortran for {obs_name}")
    print(fortran_str)
    print()
