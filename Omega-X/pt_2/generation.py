from sympy.parsing.latex import parse_latex
from sympy import symbols
from sympy import diff
from sympy.utilities.codegen import codegen


# Define symbols that appear in the LaTeX
rho, vx, vy, vz, Bx, By, Bz = symbols('rho vx vy vz Bx By Bz')

# Parse equation
H_expr = parse_latex(r"\frac{1}{2} \rho (v_x^2 + v_y^2 + v_z^2) + \frac{1}{2}(B_x^2 + B_y^2 + B_z^2)")


dH_drho = diff(H_expr, rho)
dH_dvx  = diff(H_expr, vx)


# Prepare list of (name, expression) pairs
expressions = [
    ("dH_drho", dH_drho),
    ("dH_dvx", dH_dvx),
    ("dH_dvy", dH_dvy),
    ("dH_dvz", dH_dvz),
]

# Generate Fortran code
codegen(
    name_expr=expressions,
    language="F95",
    project="omega_x_hamiltonian",
    to_files=True,
)

