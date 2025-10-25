from sympy.parsing.latex import parse_latex
from sympy import symbols

# Define symbols that appear in the LaTeX
rho, vx, vy, vz, Bx, By, Bz = symbols('rho vx vy vz Bx By Bz')

# Parse equation
H_expr = parse_latex(r"\frac{1}{2} \rho (v_x^2 + v_y^2 + v_z^2) + \frac{1}{2}(B_x^2 + B_y^2 + B_z^2)")
from sympy import diff

dH_drho = diff(H_expr, rho)
dH_dvx  = diff(H_expr, vx)
from sympy.utilities.codegen import codegen

[(name, fcode), (hname, hcode)] = codegen(
    name_expr=[('dH_drho', dH_drho), ('dH_dvx', dH_dvx)],
    language='F95',
    project='xmhd_from_tex'
)
