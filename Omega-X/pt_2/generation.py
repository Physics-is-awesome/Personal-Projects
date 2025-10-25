import sympy as sp

# -----------------------------
# 1. Symbols and functions
# -----------------------------
x = sp.symbols('x')  # spatial coordinate
gamma, Cv, Re, Pr = sp.symbols('gamma Cv Re Pr', positive=True)

# Discrete observables (trial functions)
m_h = sp.Function('m_h')(x)
rho_h = sp.Function('rho_h')(x)
sigma_h = sp.Function('sigma_h')(x)
u_h = sp.Function('u_h')(x)
T_h = sp.Function('T_h')(x)

# Test functions for L2 projection
phi_m = sp.Function('phi_m')(x)
phi_rho = sp.Function('phi_rho')(x)
phi_sigma = sp.Function('phi_sigma')(x)

# Auxiliary symbolic functions for 4-bracket
F = sp.Function('F')(x)
K = sp.Function('K')(x)
G = sp.Function('G')(x)
N = sp.Function('N')(x)

# Entropy per mass
s = sp.Symbol('s')
s_expr = sigma_h / rho_h

# -----------------------------
# 2. Define internal energy U
# -----------------------------
U_expr = Cv * sp.exp(s / Cv) * rho_h**(gamma - 1) / (gamma - 1)

# Temperature from partial derivative
T_expr = sp.diff(U_expr, s)
T_expr = T_expr.subs(s, s_expr)

# -----------------------------
# 3. L2 inner product helper
# -----------------------------
def L2(f, g):
    return sp.integrate(f * g, (x, 0, 1))  # 1D domain [0,1]

# -----------------------------
# 4. Poisson bracket terms
# -----------------------------
poisson_term = (
    -L2(m_h * u_h.diff(x), phi_m) + L2(m_h * u_h, phi_m.diff(x))
    - L2(rho_h * s_expr.diff(x), phi_rho) + L2(rho_h * u_h, phi_rho.diff(x))
    - L2(sigma_h * T_expr.diff(x), phi_sigma) + L2(sigma_h * u_h, phi_sigma.diff(x))
)

# -----------------------------
# 5. Dissipative 4-bracket term
# -----------------------------
dissipative_term = -1/Re * sp.integrate(
    1/T_h *
    (K.diff(x) * F.diff(x) - F.diff(x) * K.diff(x)) *
    (N.diff(x) * G.diff(x) - G.diff(x) * N.diff(x)),
    (x, 0, 1)
)

# -----------------------------
# 6. Full evolution equation
# -----------------------------
F_dot = poisson_term + dissipative_term

# -----------------------------
# 7. Print and check
# -----------------------------
sp.init_printing()
print("Temperature expression T_expr:")
sp.pprint(T_expr)

print("\nPoisson term:")
sp.pprint(poisson_term)

print("\nDissipative term:")
sp.pprint(dissipative_term)

print("\nFull symbolic evolution equation F_dot:")
sp.pprint(F_dot)
