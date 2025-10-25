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
T_h = sp.Function('T_h')(x, t)  # placeholder temperature

# -----------------------------
# 2. Internal energy and temperature
# -----------------------------
s = sp.Symbol('s')
rho_h = observables['rho_h']
sigma_h = observables['sigma_h']
U_expr = Cv * sp.exp(s / Cv) * rho_h**(gamma - 1) / (gamma - 1)
T_expr = sp.diff(U_expr, s).subs(s, sigma_h / rho_h)

# -----------------------------
# 3. L2 projection placeholder
# -----------------------------
def L2(f, g):
    return sp.Function('L2')(f, g)  # symbolic inner product placeholder

# -----------------------------
# 4. Full bracket input
# -----------------------------
# User inputs the full bracket (Poisson + metriplectic) symbolically
# Example placeholder: you can replace this with the actual bracket
full_bracket = (
    -L2(observables['m_h']*sp.diff(observables['m_h'], x), test_funcs['m_h'])
    + L2(observables['m_h']*observables['m_h'], sp.diff(test_funcs['m_h'], x))
    -1/Re * sp.Function('Integral')(
        1/T_h * (K.diff(x)*F.diff(x)-F.diff(x)*K.diff(x)) *
        (N.diff(x)*G.diff(x)-G.diff(x)*N.diff(x))
    )
)

# -----------------------------
# 5. Function to automatically extract evolution for each observable
# -----------------------------
def derive_evolution(full_bracket, observables, test_funcs):
    evol_eqs = {}
    for obs_name, obs_func in observables.items():
        # Set all other test functions to zero
        subs_dict = {tf: 0 for key, tf in test_funcs.items() if key != obs_name}
        rhs = full_bracket.subs(subs_dict)
        rhs = sp.expand(rhs)                   
        rhs = sp.cancel(rhs)                    
        rhs = sp.simplify(rhs)
        # Time derivative
        obs_dot = sp.diff(obs_func, t)
        obs_dot = sp.simplify(obs_dot)
        evol_eqs[obs_name] = sp.Eq(obs_dot, rhs)
    return evol_eqs

# -----------------------------
# 6. Compute evolution equations
# -----------------------------
evol_eqs = derive_evolution(full_bracket, observables, test_funcs)

# -----------------------------
# 7. Display
# -----------------------------
sp.init_printing()
for obs_name, eq in evol_eqs.items():
    print(f"\nEvolution equation for {obs_name}:")
    sp.pprint(eq)
