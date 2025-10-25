import sympy as sp

x = sp.symbols('x')
gamma, Cv, Re, Pr = sp.symbols('gamma Cv Re Pr', positive=True)

# Observables as functions
m_h = sp.Function('m_h')(x)
rho_h = sp.Function('rho_h')(x)
sigma_h = sp.Function('sigma_h')(x)
u_h = sp.Function('u_h')(x)
T_h = sp.Function('T_h')(x)

phi_m = sp.Function('phi_m')(x)
phi_rho = sp.Function('phi_rho')(x)
phi_sigma = sp.Function('phi_sigma')(x)

s = sp.Symbol('s')
s_expr = sigma_h / rho_h

# Internal energy
U_expr = Cv * sp.exp(s / Cv) * rho_h**(gamma - 1) / (gamma - 1)
T_expr = sp.diff(U_expr, s).subs(s, s_expr)

# Define a symbolic L2 inner product
def L2(f, g):
    return sp.Function('L2')(f, g)

# Poisson bracket term (symbolic)
poisson_term = (
    -L2(m_h * u_h.diff(x), phi_m) + L2(m_h * u_h, phi_m.diff(x))
    - L2(rho_h * s_expr.diff(x), phi_rho) + L2(rho_h * u_h, phi_rho.diff(x))
    - L2(sigma_h * T_expr.diff(x), phi_sigma) + L2(sigma_h * u_h, phi_sigma.diff(x))
)

# Dissipative 4-bracket term (symbolic)
F, K, G, N = sp.symbols('F K G N', cls=sp.Function)
dissipative_term = -1/Re * sp.Function('Integral')(
    1/T_h *
    (K(x).diff(x) * F(x).diff(x) - F(x).diff(x) * K(x).diff(x)) *
    (N(x).diff(x) * G(x).diff(x) - G(x).diff(x) * N(x).diff(x))
)

# Full evolution
F_dot = poisson_term + dissipative_term

sp.init_printing()
print("Temperature expression T_expr:")
sp.pprint(T_expr)

print("\nPoisson term (symbolic L2):")
sp.pprint(poisson_term)

print("\nDissipative term (symbolic integral):")
sp.pprint(dissipative_term)

print("\nFull symbolic evolution F_dot:")
sp.pprint(F_dot)

