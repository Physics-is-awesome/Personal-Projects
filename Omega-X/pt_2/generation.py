#!/usr/bin/env python3
# generate_variational_metriplectic_f90.py
# Generates a Fortran95 module with variational (pointwise) functions for the 1D metriplectic density/momentum/entropy model.
# Produces:
#   - metriplectic_variational_pointwise.f90
#   - test_metriplectic.f90
#   - compile.sh
#
# Usage: python generate_variational_metriplectic_f90.py
# (Requires sympy installed: pip install sympy)

import textwrap
import os
import sys
import sympy as sp
from sympy.utilities.codegen import codegen

# -----------------------------
# User options
# -----------------------------
SUBSTITUTE_U_WITH_IDEAL = True   # if False, U(rho,s) remains symbolic and must be provided in Fortran
OUT_DIR = "."                    # where to write files

# default closure parameters (ideal-gas-like)
Cv, gamma = sp.symbols('Cv gamma')
Cv_val = 1.0
gamma_val = 1.4

# -----------------------------
# 1) Symbols & expressions
# -----------------------------
rho, m, sigma = sp.symbols('rho m sigma', positive=True)
mu, kappa = sp.symbols('mu kappa')         # transport coefficients
rho_x, m_x, sigma_x = sp.symbols('rho_x m_x sigma_x')
u_x, u_xx = sp.symbols('u_x u_xx')
T_x, T_xx = sp.symbols('T_x T_xx')

# derived
u = m / rho
s = sigma / rho
s_expr = sigma / rho

# Define U(rho, s)
U = sp.Function('U')
if SUBSTITUTE_U_WITH_IDEAL:
    # Example ideal-gas-like internal energy per mass:
    # U(rho,s) = Cv * exp(s/Cv) * rho**(gamma-1) / (gamma-1)
    # This is just a convenient parametric form that yields positive T and p.
    U_expr = Cv * sp.exp(s / Cv) * rho**(gamma - 1) / (gamma - 1)
    U_sym = None
else:
    U_expr = U(rho, s)   # leave symbolic
    U_sym = U

# Hamiltonian density h(rho,m,sigma)
# h = m^2 / (2 rho) + rho * U(rho, s)
h_local = (m**2) / (2 * rho) + rho * (U_expr.subs({s: s_expr}) if SUBSTITUTE_U_WITH_IDEAL else U(rho, s_expr))

# compute functional derivatives (use s_expr substitution so derivatives wrt sigma are correct)
h_sub = (m**2) / (2 * rho) + rho * (U_expr.subs(s, s_expr) if SUBSTITUTE_U_WITH_IDEAL else U(rho, s_expr))

dH_dm = sp.simplify(sp.diff(h_sub, m))           # should be m/rho -> u
dH_dsigma = sp.simplify(sp.diff(h_sub, sigma))   # should be U_s(rho,s) evaluated at s_expr
dH_drho = sp.simplify(sp.diff(h_sub, rho))       # full chain-rule result

# Temperature and pressure (explicit)
# T = ∂U/∂s evaluated at s_expr
if SUBSTITUTE_U_WITH_IDEAL:
    T_expr = sp.simplify(sp.diff(U_expr, s).subs(s, s_expr))
    p_expr = sp.simplify(rho**2 * sp.diff(U_expr, rho).subs(s, s_expr))
else:
    T_expr = sp.simplify(sp.diff(U(rho, s), s).subs(s, s_expr))
    p_expr = sp.simplify(rho**2 * sp.diff(U(rho, s), rho).subs(s, s_expr))

# Fluxes and pointwise dissipative terms (placeholders for derivatives passed from Fortran)
F_rho = sp.simplify(rho * u)
F_m_adv = sp.simplify(m * u)
F_sigma = sp.simplify(sigma * u)

viscous_prefactor = sp.simplify(mu * u_x)   # ∂x(viscous_prefactor) will be applied by discretization
kappa_over_T = sp.simplify(kappa / T_expr)  # export this and its x derivative placeholder
kappa_over_T_x = sp.symbols('kappa_over_T_x')  # placeholder: user computes (kappa/T)_x
# entropy pointwise sources:
source_viscous = sp.simplify((mu / T_expr) * (u_x**2))
kappa_Txx_part = sp.simplify(kappa_over_T * T_xx)   # (kappa/T) * T_xx part
kappa_Tgrad2 = sp.simplify((kappa / (T_expr**2)) * (T_x**2))

# ---------------
# Prepare codegen list
# ---------------
expressions = [
    ('dH_dm_pt', dH_dm),
    ('dH_dsigma_pt', dH_dsigma),
    ('dH_drho_pt', dH_drho),
    ('T_pt', T_expr),
    ('p_pt', p_expr),
    ('F_rho_pt', F_rho),
    ('F_m_adv_pt', F_m_adv),
    ('F_sigma_pt', F_sigma),
    ('viscous_prefactor_pt', viscous_prefactor),
    ('kappa_over_T_pt', kappa_over_T),
    ('source_viscous_pt', source_viscous),
    ('kappa_Txx_part_pt', kappa_Txx_part),
    ('kappa_Tgrad2_pt', kappa_Tgrad2),
]

# Codegen to Fortran 95
[(module_name, fcode), (head_name, header_code)] = codegen(
    name_expr=expressions,
    language='F95',
    project='metriplectic_variational_pointwise',
    to_files=True
)

# By default codegen writes a file like 'metriplectic_variational_pointwise.f95' or .f90
generated_filename = module_name + '.f90'
# codegen can create different filenames; search for created file
# but sympy will have created a file named module_name + '.f95' typically; check both
if not os.path.exists(generated_filename):
    # check .f95
    if os.path.exists(module_name + '.f95'):
        generated_filename = module_name + '.f95'
    else:
        # fallback: use fcode variable content and write to .f90 ourselves
        generated_filename = os.path.join(OUT_DIR, 'metriplectic_variational_pointwise.f90')
        with open(generated_filename, 'w') as of:
            of.write(fcode)

# Postprocess: ensure double precision kind and tidy function interfaces.
# For simplicity, we'll create a wrapper module file that imports inner generated functions
# and exposes consistent real(kind=8) interfaces. Easiest is to parse the generated file
# and replace 'real' with 'real(kind=8)' where appropriate. We'll be conservative.

with open(generated_filename, 'r') as gf:
    gen_text = gf.read()

# Replace default REAL declarations: simple heuristic replacements
gen_text = gen_text.replace('real ', 'real(kind=8) ')
gen_text = gen_text.replace(' REAL ', ' real(kind=8) ')
# Ensure the module name is metriplectic_variational_pointwise
# (codegen sets a module name; we normalize to this)
# Write out normalized file
out_mod_file = os.path.join(OUT_DIR, 'metriplectic_variational_pointwise.f90')
with open(out_mod_file, 'w') as outf:
    outf.write('! Auto-generated by generate_variational_metriplectic_f90.py\n')
    outf.write('module metriplectic_variational_pointwise\n')
    outf.write('  implicit none\n')
    outf.write('contains\n\n')
    # For each expression generate a small wrapper function with explicit signature to ensure clarity
    # We will create explicit functions matching the expressions above.
    # - dH_dm_pt(rho,m,sigma) -> real(kind=8)
    # - dH_dsigma_pt(rho,m,sigma)
    # - dH_drho_pt(rho,m,sigma)
    # - T_pt(rho,m,sigma)
    # - p_pt(rho,m,sigma)
    # - F_rho_pt(rho,m,sigma)
    # - F_m_adv_pt(rho,m,sigma)
    # - F_sigma_pt(rho,m,sigma)
    # - viscous_prefactor_pt(mu,u_x)
    # - kappa_over_T_pt(kappa, rho,m,sigma)
    # - source_viscous_pt(mu,u_x,kappa,rho,m,sigma)
    # - kappa_Txx_part_pt(kappa_over_T, T_xx)
    # - kappa_Tgrad2_pt(kappa,T,T_x)
    #
    # We'll emit code using sympy 'ccode' style stringification for expressions to avoid complex parsing.
    #
    # Build mapping of expression strings using sympy.lambdify-like conversion to Fortran expression
    for name, expr in expressions:
        # Determine argument list by inspecting free symbols of expr
        syms = sorted(expr.free_symbols, key=lambda s: s.name)
        # Build argument string (all reals)
        arglist = ', '.join([f'{str(s)}' for s in syms])
        # Build signature line
        fn_args = ', '.join([f'real(kind=8), intent(in) :: {str(s)}' for s in syms])
        if fn_args == '':
            fn_args = ' ! no args'
        # convert expression to Fortran code using sympy.fcode
        try:
            f_expr = sp.fcode(expr, standard=95)
            # sp.fcode returns a string like '      (some fortran expression)'
            # We need to extract RHS; fallback if messy:
            # Simpler: use sp.printing.f90code
            from sympy.printing.fcode import FCodePrinter
            printer = FCodePrinter()
            f_rhs = printer.doprint(sp.simplify(expr))
        except Exception:
            # fallback to f90 printer
            from sympy.printing.fcode import FCodePrinter
            printer = FCodePrinter()
            f_rhs = printer.doprint(sp.simplify(expr))
        # Clean up variable names for Fortran: sympy will use same names
        # Emit function
        outf.write(textwrap.dedent(f'''
        function {name}({arglist}) result(res)
          real(kind=8), intent(in) :: {arglist if arglist else ''}
          real(kind=8) :: res
          res = {f_rhs}
        end function {name}

        '''))
    outf.write('end module metriplectic_variational_pointwise\n')

print(f"Generated Fortran module: {out_mod_file}")

# -----------------------------
# Write a small Fortran test program file
# -----------------------------
test_f90 = r"""
program test_metriplectic
  use metriplectic_variational_pointwise
  implicit none
  real(kind=8) :: rho, m, sigma, mu, kappa, u_x, u_xx, T_x, T_xx
  real(kind=8) :: v1, v2, v3

  ! sample values
  rho = 1.2d0
  m = 0.5d0
  sigma = 0.3d0
  mu = 1.0d-2
  kappa = 2.0d-2
  u_x = 0.1d0
  u_xx = 0.01d0
  T_x = 0.02d0
  T_xx = 0.002d0

  print *, 'rho,m,sigma=', rho, m, sigma
  print *, 'dH_dm_pt=', dH_dm_pt(rho,m,sigma)
  print *, 'dH_dsigma_pt=', dH_dsigma_pt(rho,m,sigma)
  print *, 'dH_drho_pt=', dH_drho_pt(rho,m,sigma)
  print *, 'T_pt=', T_pt(rho,m,sigma)
  print *, 'p_pt=', p_pt(rho,m,sigma)
  print *, 'F_rho_pt=', F_rho_pt(rho,m,sigma)
  print *, 'F_m_adv_pt=', F_m_adv_pt(rho,m,sigma)
  print *, 'F_sigma_pt=', F_sigma_pt(rho,m,sigma)
  print *, 'viscous_prefactor_pt=', viscous_prefactor_pt(mu,u_x)
  print *, 'kappa_over_T_pt=', kappa_over_T_pt(kappa,rho,m,sigma)
  print *, 'source_viscous_pt=', source_viscous_pt(mu,u_x,kappa,rho,m,sigma)
  print *, 'kappa_Txx_part_pt=', kappa_Txx_part_pt(kappa_over_T_pt(kappa,rho,m,sigma), T_xx)
  print *, 'kappa_Tgrad2_pt=', kappa_Tgrad2_pt(kappa, T_pt(rho,m,sigma), T_x)
end program test_metriplectic
"""
test_file = os.path.join(OUT_DIR, 'test_metriplectic.f90')
with open(test_file, 'w') as tf:
    tf.write(test_f90)
print(f"Wrote Fortran test program to {test_file}")

# -----------------------------
# Write compile.sh helper
# -----------------------------
compile_sh = f"""#!/bin/bash
# compile.sh - compile generated Fortran module and test program, and build f2py python module
set -e
MOD=metriplectic_variational_pointwise
F90=gfortran
PYMODULE=metriplectic_pointwise_py

echo "Compiling Fortran module..."
$F90 -c -fPIC -O2 {out_mod_file}

echo "Compiling test program..."
$F90 -c -fPIC -O2 {test_file}

echo "Linking test executable..."
$F90 -o test_metriplectic metriplectic_variational_pointwise.o test_metriplectic.o

echo "Building Python extension via f2py..."
python3 - <<'PYF'
import sys
import numpy
from numpy import f2py
src = '{out_mod_file}'
f2py.compile(open(src).read(), modulename='{PYMODULE}', extra_args='--quiet', verbose=False)
print("f2py build complete: module {PYMODULE}")
PYF

echo "Done. Run ./test_metriplectic to execute Fortran test, or import {PYMODULE} in Python."
"""
compile_path = os.path.join(OUT_DIR, 'compile.sh')
with open(compile_path, 'w') as cs:
    cs.write(compile_sh)
os.chmod(compile_path, 0o755)
print("Wrote compile helper: compile.sh")
print("Next steps:")
print("  1) ./compile.sh")
print("  2) ./test_metriplectic")
print("  3) python3 python_unit_test.py   # to run automatic checks (see python_unit_test.py)")
