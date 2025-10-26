import sympy as sp
from sympy import fcode, symbols, Function, Derivative

# -----------------------------
# 1. Symbols and parameters
# -----------------------------
x, t = sp.symbols('x t')
gamma, Cv, Re, Pr = sp.symbols('gamma Cv Re Pr', positive=True)

# Observables
observables = {
    'm_h': Function('m_h')(x, t),
    'rho_h': Function('rho_h')(x, t),
    'sigma_h': Function('sigma_h')(x, t),
    'u_h': Function('u_h')(x, t)  # new velocity observable
}

# Basis functions for Galerkin L2 projection
basis = {
    'm_h': Function('phi_m')(x),
    'rho_h': Function('phi_rho')(x),
    'sigma_h': Function('phi_sigma')(x),
    'u_h': Function('phi_u')(x)
}

# Auxiliary functions
T_h = Function('T_h')(x, t)
eta_h = Function('eta_h')(x, t)

# -----------------------------
# 2. Internal energy and temperature
# -----------------------------
s = sp.Symbol('s')
rho_h_sym = observables['rho_h']
sigma_h_sym = observables['sigma_h']
U_expr = Cv * sp.exp(s / Cv) * rho_h_sym**(gamma - 1) / (gamma - 1)
T_expr = sp.diff(U_expr, s).subs(s, sigma_h_sym / rho_h_sym)

# -----------------------------
# 3. L2 wrapper for projection
# -----------------------------
class L2Linear(sp.Function):
    nargs = 2
    @classmethod
    def eval(cls, f, g):
        # Automatically propagate zero
        if f == 0 or g == 0:
            return sp.S(0)
        return None

def L2(f, g):
    return L2Linear(f, g)

# -----------------------------
# 4. Full bracket system (example)
# -----------------------------
full_bracket = (
    # Poisson part (weak form)
    -L2(observables['m_h'] * Derivative(observables['u_h'], x), basis['m_h'])
    + L2(observables['m_h'] * observables['u_h'], Derivative(basis['m_h'], x))
    -L2(observables['rho_h'] * Derivative(eta_h, x), basis['m_h'])
    + L2(observables['rho_h'] * observables['u_h'], Derivative(basis['rho_h'], x))
    -L2(observables['sigma_h'] * Derivative(T_h, x), basis['m_h'])
    + L2(observables['sigma_h'] * observables['u_h'], Derivative(basis['sigma_h'], x))
    # Metriplectic dissipative part
    -1/Re * L2(Derivative(observables['u_h'], x), basis['m_h'])
    - L2((Derivative(observables['u_h'], x)**2)/T_h, basis['sigma_h'])
    + 1/Pr * (gamma/(gamma - 1)) * (
        L2(Derivative(T_h, x)/T_h, Derivative(basis['sigma_h'], x)) 
        - L2(Derivative(T_h, x)**2 / T_h**2, basis['sigma_h'])
    )
)

# -----------------------------
# 5. Derive evolution equations with L2 projection
# -----------------------------
def remove_zero_terms(expr, zero_basis):
    if expr in zero_basis:
        return sp.S(0)
    if expr.is_Atom:
        return expr
    new_args = [remove_zero_terms(a, zero_basis) for a in expr.args]
    return expr.func(*new_args)

def derive_evolution(full_bracket, observables, basis):
    evol_eqs = {}
    for obs_name, obs_func in observables.items():
        rhs = full_bracket
        # Zero all other basis functions
        zero_basis = [bf for key, bf in basis.items() if key != obs_name]
        rhs_clean = remove_zero_terms(rhs, zero_basis)
        rhs_clean = sp.expand(rhs_clean)
        rhs_clean = sp.simplify(rhs_clean)
        evol_eqs[obs_name] = sp.Eq(Derivative(obs_func, t), rhs_clean)
    return evol_eqs

evol_eqs = derive_evolution(full_bracket, observables, basis)

# -----------------------------
# 6. Replace L2 with actual product for Fortran code
# -----------------------------
def replace_L2(expr):
    if expr.func == L2Linear:
        f, g = expr.args
        return f * g
    if expr.is_Atom:
        return expr
    new_args = [replace_L2(a) for a in expr.args]
    return expr.func(*new_args)

def replace_derivatives(expr):
    derivs = {}
    for d in expr.atoms(Derivative):
        func = d.expr
        vars_ = d.variables
        if len(vars_) == 1:
            var = vars_[0]
            sym_name = f'd{func.func.__name__}_d{var}'
            derivs[d] = sp.Symbol(sym_name)
        else:
            derivs[d] = sp.Symbol("unsupported_deriv")
    return expr.xreplace(derivs)

def replace_functions(expr):
    replacements = {}
    for key, f in observables.items():
        replacements[f] = sp.Symbol(key)
    replacements.update({
        T_h: sp.Symbol('T_h'),
        eta_h: sp.Symbol('eta_h')
    })
    return expr.xreplace(replacements)

# -----------------------------
# 7. Generate Fortran code
# -----------------------------
print("\nGenerated Fortran code:\n")
for obs_name, eq in evol_eqs.items():
    rhs_clean = replace_L2(eq.rhs)
    rhs_clean = replace_derivatives(replace_functions(rhs_clean))
    rhs_fcode = fcode(rhs_clean, assign_to=f'd{obs_name}_dt', source_format='free', standard=95)
    print(f"! Fortran code for {obs_name}")
    print(rhs_fcode)
    print()
