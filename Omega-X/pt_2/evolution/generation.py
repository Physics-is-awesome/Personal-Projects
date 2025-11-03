import sympy as sp
from sympy import fcode
import os

# -----------------------------
# 1. Symbols and observables
# -----------------------------
x, t = sp.symbols('x t')
gamma, Cv, Re, Pr = sp.symbols('gamma Cv Re Pr', positive=True)

observables = {
    'm_h': sp.Function('m_h')(x, t),
    'rho_h': sp.Function('rho_h')(x, t),
    'sigma_h': sp.Function('sigma_h')(x, t)
}

basis = {
    'm_h': sp.Function('phi_m')(x),
    'rho_h': sp.Function('phi_rho')(x),
    'sigma_h': sp.Function('phi_sigma')(x)
}

T_h = sp.Function('T_h')(x, t)
u_h = sp.Function('u_h')(x, t)
eta_h = sp.Function('eta_h')(x, t)

s = sp.Symbol('s')
rho_h = observables['rho_h']
sigma_h = observables['sigma_h']
U_expr = Cv * sp.exp(s / Cv) * rho_h**(gamma - 1) / (gamma - 1)
T_expr = sp.diff(U_expr, s).subs(s, sigma_h / rho_h)

# -----------------------------
# 2. L2 wrapper
# -----------------------------
class L2Linear(sp.Function):
    nargs = 2
    @classmethod
    def eval(cls, f, g):
        if f == 0 or g == 0:
            return sp.S(0)

def L2(f, g):
    return L2Linear(f, g)

# -----------------------------
# 3. Full bracket
# -----------------------------
full_bracket = (
    -L2(observables['m_h']*sp.diff(u_h, x), basis['m_h'])
    + L2(observables['m_h']*u_h, sp.diff(basis['m_h'], x))
    -L2(observables['rho_h']*sp.diff(eta_h, x), basis['m_h'])
    + L2(observables['rho_h']*u_h, sp.diff(basis['rho_h'], x))
    -L2(observables['sigma_h']*sp.diff(T_h, x), basis['m_h'])
    + L2(observables['sigma_h']*u_h, sp.diff(basis['sigma_h'], x))
    -1/Re * L2(sp.diff(u_h, x), basis['m_h'])
    - L2((sp.diff(u_h, x))**2/T_h, basis['sigma_h'])
    + 1/Pr * (gamma/(gamma-1)) * (
        L2(sp.diff(T_h, x)/T_h, sp.diff(basis['sigma_h'], x))
        - L2((sp.diff(T_h, x))**2/T_h**2, basis['sigma_h'])
    )
)

# -----------------------------
# 4. Evolution equations
# -----------------------------
def remove_zero_terms(expr, zero_funcs):
    if expr in zero_funcs:
        return sp.S(0)
    if expr.is_Atom:
        return expr
    return expr.func(*[remove_zero_terms(a, zero_funcs) for a in expr.args])

def derive_evolution(full_bracket, observables, basis):
    evol_eqs = {}
    for obs_name, obs_func in observables.items():
        zero_funcs = [tf for key, tf in basis.items() if key != obs_name]
        rhs = remove_zero_terms(full_bracket, zero_funcs)
        rhs = sp.expand(rhs)
        rhs = sp.simplify(rhs)
        evol_eqs[obs_name] = sp.Eq(sp.diff(obs_func, t), rhs)
    return evol_eqs

evol_eqs = derive_evolution(full_bracket, observables, basis)
print(evol_eqs)
# -----------------------------
# 5. Code replacements
# -----------------------------
def replace_L2(expr):
    if expr.func == L2Linear:
        f, g = expr.args
        return f*g
    if expr.is_Atom:
        return expr
    return expr.func(*[replace_L2(a) for a in expr.args])

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
# 6. Write Fortran module files
# -----------------------------
output_dir = 'fortran_modules'
os.makedirs(output_dir, exist_ok=True)

for obs_name, eq in evol_eqs.items():
    rhs = eq.rhs
    rhs = replace_L2(rhs)
    rhs = replace_derivatives(rhs)
    rhs = replace_observables(rhs)
    rhs = replace_basis(rhs)

    code_str = fcode(rhs, assign_to=f'F_{obs_name}', source_format='free', standard=95)

    file_path = os.path.join(output_dir, f'{obs_name}_module.f90')
    with open(file_path, 'w') as f:
        f.write(f"! Automatically generated module for {obs_name}\n")
        f.write(f"module {obs_name}_module\n")
        f.write("  implicit none\n")
        f.write("contains\n")
        f.write(f"  subroutine compute_{obs_name}(Re, m_h, phi_m_i, phi_rho_i, phi_sigma_i, u_h, T_h, dT_h_dx, sigma_h, eta_h, deta_h_dx, rho_h, dphi_m_dx, dphi_m_i, du_h_dx, F_{obs_name})\n")
        f.write("    implicit none\n")
        f.write("    ! Declare inputs and outputs as real\n")
        f.write("    int, intent(in) :: Re\n")
        f.write("    real, intent(in) :: m_h, phi_m_i, phi_rho_i, phi_sigma_i, u_h, T_h, dT_h_dx, sigma_h, eta_h, deta_h_dx, rho_h, dphi_m_dx, dphi_m_i, du_h_dx\n")
        f.write(f"    real, intent(out) :: F_{obs_name}\n")
        f.write("\n")
        f.write("    ! Evolution equation\n")
        f.write("    ")
        f.write(code_str.replace('\n', '\n    '))
        f.write("\n")
        f.write(f"  end subroutine compute_{obs_name}\n")
        f.write("end module\n")

    print(f"Module created: {file_path}")
