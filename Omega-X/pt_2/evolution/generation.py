
"""
Soft-coded SymPy -> Fortran generator (argument-based, 3D arrays, validated)

Usage:
  python3 generate_fortran.py

Output:
  fortran_modules/<observable>_module.f90

Requirements:
  - Python 3.8+
  - sympy (pip install sympy)
"""

import os
import re
import sys
import textwrap
import sympy as sp
from sympy import fcode

# -----------------------------
# 0) Configuration / hints
# -----------------------------
OUTPUT_DIR = "fortran_modules"
SCALAR_HINTS = {"gamma", "Cv", "Re", "Pr"}   # add constants here as needed
FIELD_NAMES = {"m_h", "rho_h", "sigma_h", "T_h", "u_h", "eta_h"}  # common field names
FORTRAN_INTRINSICS = {
    "exp", "log", "sqrt", "sin", "cos", "tan", "asin", "acos", "atan",
    "abs", "max", "min", "real", "int"
}
# Regex helpers
IDENT_RE = re.compile(r"\b[A-Za-z_][A-Za-z0-9_]*\b")
TEMP_RE = re.compile(r"^t[0-9]+$")  # SymPy temporaries pattern
LOOP_INDICES = {"i", "j", "k"}
# -----------------------------
# 1) Symbolic model (edit here)
# -----------------------------
x, t = sp.symbols("x t")
gamma, Cv, Re, Pr = sp.symbols("gamma Cv Re Pr", positive=True)

# Observables (fields depending on x,t)
observables = {
    "m_h": sp.Function("m_h")(x, t),
    "rho_h": sp.Function("rho_h")(x, t),
    "sigma_h": sp.Function("sigma_h")(x, t),
}

# Basis/test functions (depend on x only)
basis_funcs = {
    "m_h": sp.Function("phi_m")(x),
    "rho_h": sp.Function("phi_rho")(x),
    "sigma_h": sp.Function("phi_sigma")(x),
}

# Additional fields used in equations
T_h = sp.Function("T_h")(x, t)
u_h = sp.Function("u_h")(x, t)
eta_h = sp.Function("eta_h")(x, t)

# Example thermodynamics (adaptable)
s = sp.Symbol("s")
rho_sym = observables["rho_h"]
sigma_sym = observables["sigma_h"]
U_expr = Cv * sp.exp(s / Cv) * rho_sym ** (gamma - 1) / (gamma - 1)
T_expr = sp.diff(U_expr, s).subs(s, sigma_sym / rho_sym)

# -----------------------------
# 2) L2 wrapper (weak form)
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
# 3) Full bracket (edit freely)
# -----------------------------
full_bracket = (
    -L2(observables["m_h"] * sp.diff(u_h, x), basis_funcs["m_h"])
    + L2(observables["m_h"] * u_h, sp.diff(basis_funcs["m_h"], x))
    - L2(observables["rho_h"] * sp.diff(eta_h, x), basis_funcs["m_h"])
    + L2(observables["rho_h"] * u_h, sp.diff(basis_funcs["rho_h"], x))
    - L2(observables["sigma_h"] * sp.diff(T_h, x), basis_funcs["m_h"])
    + L2(observables["sigma_h"] * u_h, sp.diff(basis_funcs["sigma_h"], x))
    - 1 / Re * L2(sp.diff(u_h, x), basis_funcs["m_h"])
    - L2((sp.diff(u_h, x)) ** 2 / T_h, basis_funcs["sigma_h"])
    + 1 / Pr * (gamma / (gamma - 1)) * (
        L2(sp.diff(T_h, x) / T_h, sp.diff(basis_funcs["sigma_h"], x))
        - L2((sp.diff(T_h, x)) ** 2 / T_h ** 2, basis_funcs["sigma_h"])
    )
)

# -----------------------------
# 4) Evolution equations builder
# -----------------------------
def remove_zero_terms(expr, zero_funcs):
    if expr in zero_funcs:
        return sp.S(0)
    if expr.is_Atom:
        return expr
    return expr.func(*[remove_zero_terms(a, zero_funcs) for a in expr.args])

def derive_evolution(full_bracket, observables, basis_funcs):
    evol_eqs = {}
    for obs_name, obs_func in observables.items():
        zero_funcs = [tf for key, tf in basis_funcs.items() if key != obs_name]
        rhs = remove_zero_terms(full_bracket, zero_funcs)
        rhs = sp.expand(rhs)
        rhs = sp.simplify(rhs)
        evol_eqs[obs_name] = sp.Eq(sp.diff(obs_func, t), rhs)
    return evol_eqs

evol_eqs = derive_evolution(full_bracket, observables, basis_funcs)

# -----------------------------
# 5) Replacement helpers
# -----------------------------
def derivative_symbol_name(d):
    func = d.expr
    vars_ = d.variables
    base = func.func.__name__
    # Support single-variable derivatives (common case)
    if len(vars_) == 1:
        return f"d{base}_d{vars_[0].name}"
    # For higher-order or multi-variable, join parts
    parts = [f"d{base}"]
    for v in vars_:
        parts.append(f"d{v.name}")
    return "_".join(parts)

def replace_L2(expr):
    if expr.func == L2Linear:
        f, g = expr.args
        return f * g
    if expr.is_Atom:
        return expr
    return expr.func(*[replace_L2(a) for a in expr.args])

def replace_derivatives(expr):
    repl = {}
    for d in expr.atoms(sp.Derivative):
        repl[d] = sp.Symbol(derivative_symbol_name(d))
    return expr.xreplace(repl)

def replace_observables(expr):
    repl = {obs_func: sp.Symbol(obs_name) for obs_name, obs_func in observables.items()}
    repl.update({T_h: sp.Symbol("T_h"), u_h: sp.Symbol("u_h"), eta_h: sp.Symbol("eta_h")})
    return expr.xreplace(repl)

def replace_basis(expr):
    repl = {}
    for key, bf in basis_funcs.items():
        # create phi_<keykind>_i, e.g. phi_m_i
        kind = key.split("_")[0] if "_" in key else key
        repl[bf] = sp.Symbol(f"phi_{kind}_i")
    return expr.xreplace(repl)

# -----------------------------
# 6) Classification helpers
# -----------------------------
def is_basis_array(name: str) -> bool:
    return bool(re.match(r"^phi_[A-Za-z0-9_]+_i$", name))

def is_derivative_array(name: str) -> bool:
    return bool(re.match(r"^d[A-Za-z0-9_]+_d[A-Za-z]+$", name))

def classify_symbol(name: str) -> str:
    if name in SCALAR_HINTS:
        return "scalar"
    if name in FIELD_NAMES:
        return "array"
    if is_basis_array(name) or is_derivative_array(name):
        return "array"
    # default conservative choice: scalar
    return "scalar"

# -----------------------------
# 7) Indexify and validation
# -----------------------------
def indexify_code(code_str: str, array_names: set) -> str:
    """
    Replace bare array names with name(i,j,k) in the Fortran code string.
    Preserve intrinsics and temporaries.
    """
    def repl(match):
        token = match.group(0)
        if token in FORTRAN_INTRINSICS:
            return token
        if TEMP_RE.match(token):
            return token
        if token in array_names:
            return f"{token}(i,j,k)"
        return token
    return IDENT_RE.sub(repl, code_str)

def extract_identifiers(code_str: str) -> set:
    return set(IDENT_RE.findall(code_str))

# -----------------------------
# 8) Fortran writer with checks
# -----------------------------
def build_args_and_decls(symbols, obs_name):
    names = sorted({s.name for s in symbols})
    scalar_ins = [n for n in names if classify_symbol(n) == "scalar"]
    array_ins = [n for n in names if classify_symbol(n) == "array"]
    output_name = f"F_{obs_name}"
    decls = []
    for s in scalar_ins:
        decls.append(f"    real(8), intent(in) :: {s}")
    for a in array_ins:
        decls.append(f"    real(8), intent(in) :: {a}(:,:,:)")  # assumed-shape 3D
    decls.append(f"    real(8), intent(out) :: {output_name}(:,:,:)")
    args_order = scalar_ins + array_ins + [output_name]
    return args_order, decls, set(array_ins), output_name

def validate_generated_expression(indexed_expr: str, declared_arrays: set, declared_scalars: set, output_name: str):
    """
    Validate identifiers in the generated Fortran expression.
    Allows:
      - declared arrays (they appear as name(i,j,k) but the base name is checked),
      - declared scalars,
      - Fortran intrinsics,
      - SymPy temporaries (t0, t1, ...),
      - loop indices i,j,k,
      - the output name (F_<obs>).
    Raises RuntimeError listing any undeclared identifiers.
    """
    idents = extract_identifiers(indexed_expr)

    bad = []
    for ident in idents:
        # ignore numeric-like tokens (IDENT_RE doesn't match numbers)
        if ident in FORTRAN_INTRINSICS:
            continue
        if TEMP_RE.match(ident):
            continue
        if ident in LOOP_INDICES:
            continue
        if ident == output_name:
            continue
        if ident in declared_arrays:
            continue
        if ident in declared_scalars:
            continue
        # If we reach here, ident is unknown
        bad.append(ident)

    if bad:
        raise RuntimeError(f"Undeclared identifiers found in generated expression: {bad}")

def write_fortran_module(obs_name: str, rhs_expr: sp.Expr):
    # Transform RHS
    rhs = replace_L2(rhs_expr)
    rhs = replace_derivatives(rhs)
    rhs = replace_observables(rhs)
    rhs = replace_basis(rhs)

    # Ensure no Derivative objects remain
    if any(isinstance(a, sp.Derivative) for a in rhs.atoms(sp.Derivative)):
        raise RuntimeError(f"Derivative objects remain in RHS for {obs_name}; check replace_derivatives.")

    symbols = sorted(rhs.free_symbols, key=lambda s: s.name)
    args_order, decls, array_name_set, output_name = build_args_and_decls(symbols, obs_name)

    # Generate Fortran expression (expression-only)
    try:
        code_expr = fcode(rhs, assign_to=None, source_format="free", standard=2003)
    except Exception as e:
        raise RuntimeError(f"SymPy fcode failed for {obs_name}: {e}")

    # Indexify arrays
    indexed_expr = indexify_code(code_expr, array_name_set)

    # Validate identifiers
    declared_scalars = {n for n in args_order if classify_symbol(n) == "scalar"}
    declared_arrays = array_name_set
    validate_generated_expression(indexed_expr, declared_arrays, declared_scalars, output_name)

    # Write module
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    file_path = os.path.join(OUTPUT_DIR, f"{obs_name}_module.F90")
    header = textwrap.dedent(f"""\
        ! Automatically generated module for {obs_name}
        ! Purpose:
        !   Compute the weak-form RHS for {obs_name} over a 3D grid (assumed-shape arrays).
        !   Evaluation is element-wise with do concurrent loops; trivial dimensions (size=1) are supported.
        !
        ! Interface:
        !   subroutine compute_{obs_name}({', '.join(args_order)})
        !     Scalars (intent in): {', '.join([a for a in args_order if classify_symbol(a) == 'scalar']) or '(none)'}
        !     Arrays  (intent in): {', '.join([a for a in args_order if classify_symbol(a) == 'array']) or '(none)'}
        !     Output  (intent out): {output_name}(:,:,:)
        !
        ! Notes:
        !   - Arrays are 3D assumed-shape; pass size-1 dimensions for 1D/2D.
        !   - Edit equations in Python; regenerated modules adapt automatically.
        """)
    with open(file_path, "w") as f:
        f.write(header)
        f.write(f"module {obs_name}_module\n")
        f.write("  implicit none\n")
        f.write("contains\n")
        f.write(f"  subroutine compute_{obs_name}({', '.join(args_order)})\n")
        f.write("    implicit none\n")
        for d in decls:
            f.write(d + "\n")
        f.write("    integer :: i, j, k\n\n")
        f.write(f"    do concurrent (k = 1:size({output_name},3), j = 1:size({output_name},2), i = 1:size({output_name},1))\n")
        f.write(f"      {output_name}(i,j,k) = {indexed_expr}\n")
        f.write("    end do concurrent\n\n")
        f.write(f"  end subroutine compute_{obs_name}\n")
        f.write("end module\n")

    print(f"Module created: {file_path}")

# -----------------------------
# 9) Main
# -----------------------------
def main():
    # Basic sanity checks before generation
    if not observables:
        print("No observables defined; nothing to generate.", file=sys.stderr)
        sys.exit(1)
    for obs_name, eq in derive_evolution(full_bracket, observables, basis_funcs).items():
        try:
            write_fortran_module(obs_name, eq.rhs)
        except Exception as e:
            print(f"ERROR generating module for {obs_name}: {e}", file=sys.stderr)
            sys.exit(2)
    print("All modules generated successfully in:", OUTPUT_DIR)

if __name__ == "__main__":
    main()
