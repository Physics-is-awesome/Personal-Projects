import sympy as sp
from sympy.utilities.codegen import codegen
# Define variables
x1, x2, x3, x4 = sp.symbols('x1 x2 x3 x4')
H, S, C = sp.Function('H')(x1, x2, x3, x4), sp.Function('S')(x1, x2, x3, x4), sp.Function('C')(x1, x2, x3, x4)

# Define structure tensors (e.g. antisymmetric J_ij, symmetric G_ij)
J = sp.Matrix([[0, 1, 0, 0],
               [-1, 0, 0, 0],
               [0, 0, 0, 1],
               [0, 0, -1, 0]])

# Example 4-bracket definition
def metriplectic_4_bracket(F, G, H, S):
    gradF = sp.Matrix([sp.diff(F, v) for v in (x1, x2, x3, x4)])
    gradG = sp.Matrix([sp.diff(G, v) for v in (x1, x2, x3, x4)])
    gradH = sp.Matrix([sp.diff(H, v) for v in (x1, x2, x3, x4)])
    gradS = sp.Matrix([sp.diff(S, v) for v in (x1, x2, x3, x4)])
    # Example symbolic structure (simplified)
    return gradF.dot(J * (gradG.cross(gradH.cross(gradS))))

# Then you can compute evolution of each variable:
evol_eqs = [metriplectic_4_bracket(x, H, S, C) for x in (x1, x2, x3, x4)]



codegen(
    ("evolution_equations", sp.Matrix(evol_eqs)),
    "F95",
    "metriplectic_evolution",
    to_files=True
)


