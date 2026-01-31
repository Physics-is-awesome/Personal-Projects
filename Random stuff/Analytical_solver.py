# calc_engine_fixed.py
"""
Calc I-III style symbolic derivative & integral engine (SymPy-backed, Option B).
- Parse LaTeX (with fallback to sympify)
- Own differentiation (no sympy.diff)
- Own integration (no sympy.integrate) with:
    - linearity, constant extraction, power rule
    - trig + exponential primitives (with linear arguments)
    - u-substitution detection
    - integration by parts (heuristic)
    - trig identity rewriting (sin^2, cos^2, sin*cos -> sin2x/2)
    - trig substitution for sqrt(a^2 - x^2), sqrt(a^2 + x^2), sqrt(x^2 - a^2)
    - partial-fraction preprocessing via sympy.apart
- Returns SymPy Expr and LaTeX string
"""
from __future__ import annotations
import sympy as sp
from sympy.parsing.latex import parse_latex
from typing import Optional, Tuple

# -----------------------
# Utilities / Safe parse
# -----------------------
def safe_parse(expr_str: str) -> sp.Expr:
    """Try parse LaTeX; on exception, fall back to sympify (plain text)."""
    try:
        return parse_latex(expr_str)
    except Exception:
        return sp.sympify(expr_str)

def make_symbol(varname: str = "x") -> sp.Symbol:
    return sp.Symbol(varname, real=True)

# -----------------------
# Differentiation (custom)
# -----------------------
def my_diff(expr: sp.Expr, var: sp.Symbol) -> sp.Expr:
    expr = sp.simplify(expr)
    if expr.is_Number:
        return sp.Integer(0)
    if expr == var:
        return sp.Integer(1)
    if expr.is_Symbol:
        return sp.Integer(0)

    if expr.is_Add:
        return sp.simplify(sum(my_diff(a, var) for a in expr.args))

    if expr.is_Mul:
        terms = []
        args = expr.args
        # product rule: sum over i (d arg_i * product of others)
        for i, f in enumerate(args):
            d = my_diff(f, var)
            if d != 0:
                # other factors: everything except args[i]
                other = sp.Mul(*args[:i], *args[i+1:]) if len(args) > 1 else sp.Integer(1)
                terms.append(sp.simplify(d * other))
        return sp.simplify(sum(terms))

    if expr.is_Pow:
        base, exp = expr.args
        if exp.is_Number:
            # d/dx base^n = n * base^(n-1) * base'
            return sp.simplify(exp * base**(exp - 1) * my_diff(base, var))
        # general: d(a^b) = a^b*(b' ln a + b a'/a)
        return sp.simplify(expr * (my_diff(exp, var) * sp.log(base) + my_diff(base, var) * exp / base))

    f = expr.func
    a = expr.args[0] if expr.args else None
    if f == sp.exp:
        return sp.simplify(expr * my_diff(a, var))
    if f == sp.log:
        return sp.simplify(my_diff(a, var) / a)
    if f == sp.sin:
        return sp.simplify(sp.cos(a) * my_diff(a, var))
    if f == sp.cos:
        return sp.simplify(-sp.sin(a) * my_diff(a, var))
    if f == sp.tan:
        return sp.simplify((1 + sp.tan(a)**2) * my_diff(a, var))
    if f == sp.sec:
        return sp.simplify(sp.sec(a) * sp.tan(a) * my_diff(a, var))
    if f == sp.csc:
        return sp.simplify(-sp.csc(a) * sp.cot(a) * my_diff(a, var))
    if f == sp.asin:
        return sp.simplify(my_diff(a, var) / sp.sqrt(1 - a**2))
    if f == sp.acos:
        return sp.simplify(-my_diff(a, var) / sp.sqrt(1 - a**2))
    if f == sp.atan:
        return sp.simplify(my_diff(a, var) / (1 + a**2))

    raise NotImplementedError(f"No diff rule implemented for {expr}")

# -----------------------
# Trig rewrite system (fixed)
# -----------------------
def trig_rewrite_once(e: sp.Expr) -> sp.Expr:
    # local rewrite rules for trig forms used in Calc
    if e.is_Pow:
        b, n = e.base, e.exp
        if b.func == sp.sin and n == 2:
            u = b.args[0]; return (1 - sp.cos(2*u)) / 2
        if b.func == sp.cos and n == 2:
            u = b.args[0]; return (1 + sp.cos(2*u)) / 2
        if b.func == sp.tan and n == 2:
            u = b.args[0]; return sp.sec(u)**2 - 1

    if e.is_Mul:
        args = list(e.args)
        # sin(u)*cos(u) -> 1/2 sin(2u)
        for i, a in enumerate(args):
            for j, b in enumerate(args):
                if i != j and hasattr(a, "func") and hasattr(b, "func"):
                    if a.func == sp.sin and b.func == sp.cos and a.args[0] == b.args[0]:
                        u = a.args[0]
                        rest = [r for k, r in enumerate(args) if k not in (i, j)]
                        return sp.Mul(sp.Rational(1, 2), sp.sin(2*u), *rest)

    return e

def full_trig_rewrite(expr: sp.Expr) -> sp.Expr:
    prev = None
    curr = sp.simplify(expr)
    while prev != curr:
        prev = curr
        # apply trig_rewrite_once to every node
        curr = curr.replace(lambda _ : True, trig_rewrite_once)
        curr = sp.simplify(curr)
    return curr

# -----------------------
# Partial fraction attempt (preprocessing)
# -----------------------
def try_partial_fraction(expr: sp.Expr, var: sp.Symbol) -> Optional[sp.Expr]:
    # attempt to decompose rational expressions with sympy.apart
    try:
        ap = sp.apart(expr, var)
    except Exception:
        return None
    if ap != expr:
        # integrate each additive term
        addends = ap.as_ordered_terms() if ap.is_Add else [ap]
        results = []
        for t in addends:
            r = my_integrate(t, var)
            if r is None:
                return None
            results.append(r)
        return sp.simplify(sp.Add(*results))
    return None

# -----------------------
# u-substitution detection (fixed)
# -----------------------
def collect_subexprs(expr: sp.Expr):
    # use preorder_traversal generator and collect unique subexpressions
    seen = set()
    out = []
    for node in sp.preorder_traversal(expr):
        if node in seen:
            continue
        seen.add(node)
        out.append(node)
    # sort by complexity: more complex first
    out.sort(key=lambda s: (-len(tuple(sp.preorder_traversal(s))), str(s)))
    return out

def try_u_sub(expr: sp.Expr, var: sp.Symbol) -> Optional[sp.Expr]:
    expr = sp.simplify(expr)
    for u in collect_subexprs(expr):
        if u == expr or u == var:
            continue
        du = sp.simplify(my_diff(u, var))
        if du == 0:
            continue
        # check if expr / du is independent of var
        with sp.concrete_expression():
            try:
                ratio = sp.simplify(expr / du)
            except Exception:
                continue
        if not ratio.has(var):
            # integrate ratio as function of u: replace u->temp symbol and integrate wrt temp
            t = sp.Symbol("u_temp", real=True)
            f_of_t = ratio.xreplace({u: t})
            inner = my_integrate(f_of_t, t)
            if inner is None:
                # if ratio constant, return const * u
                if f_of_t.free_symbols == set():
                    return sp.simplify(f_of_t * u)
                continue
            return sp.simplify(inner.xreplace({t: u}))
    return None

# -----------------------
# Integration by parts (fixed)
# -----------------------
def pick_u_for_parts(expr: sp.Expr, var: sp.Symbol) -> Optional[Tuple[sp.Expr, sp.Expr]]:
    if not expr.is_Mul:
        return None
    factors = list(expr.args)
    # scoring LIATE-ish
    scored = []
    for f in factors:
        score = 0
        if f.func == sp.log:
            score += 100
        if f.func in (sp.asin, sp.acos, sp.atan):
            score += 80
        # algebraic
        try:
            poly = sp.Poly(f, var)
            if poly.degree() >= 0:
                score += 60
        except Exception:
            if f.is_Symbol or f.is_Pow:
                score += 30
        if f.func in (sp.sin, sp.cos, sp.tan, sp.sec):
            score += 40
        if f.func == sp.exp:
            score += 20
        scored.append((score, f))
    if not scored:
        return None
    scored.sort(reverse=True, key=lambda x: x[0])
    u = scored[0][1]
    dv = sp.Mul(*[f for f in factors if f != u]) if len(factors) > 1 else sp.Integer(1)
    return u, dv

def try_integration_by_parts(expr: sp.Expr, var: sp.Symbol) -> Optional[sp.Expr]:
    if not expr.is_Mul:
        return None
    pick = pick_u_for_parts(expr, var)
    if pick is None:
        return None
    u, dv = pick
    v = my_integrate(dv, var)
    if v is None:
        return None
    du = my_diff(u, var)
    inner = sp.simplify(v * du)
    int_inner = my_integrate(inner, var)
    if int_inner is None:
        return None
    return sp.simplify(u * v - int_inner)

# -----------------------
# Trig substitution detection & application (fixed)
# -----------------------
def detect_trig_sub(expr: sp.Expr, var: sp.Symbol) -> Optional[Tuple[str, sp.Expr, sp.Expr]]:
    # look for sqrt atoms (i.e., Powell with exponent 1/2 or sp.sqrt)
    pow_atoms = [p for p in expr.atoms(sp.Pow) if p.exp == sp.Rational(1, 2)]
    # also include explicit sp.sqrt atoms
    sqrt_atoms = list(expr.atoms(sp.Function))  # catch sp.sqrt too (sqrt is Pow usually)
    candidates = pow_atoms
    for s in candidates:
        inside = sp.simplify(s.base)
        # check form a^2 - x^2  (-> sin)
        a2 = sp.simplify(inside + var**2)
        if a2.free_symbols == set():
            # inside == a^2 - x^2
            if sp.simplify(inside - (a2 - var**2)) == 0:
                return ("sin", sp.sqrt(a2), s)
        # check x^2 + a^2  (-> tan)
        a2b = sp.simplify(inside - var**2)
        if a2b.free_symbols == set():
            return ("tan", sp.sqrt(a2b), s)
        # check x^2 - a^2 (-> sec)
        a2c = sp.simplify(var**2 - inside)
        if a2c.free_symbols == set() and a2c != 0:
            return ("sec", sp.sqrt(a2c), s)
    return None

def apply_trig_sub(expr: sp.Expr, var: sp.Symbol) -> Optional[sp.Expr]:
    sub = detect_trig_sub(expr, var)
    if sub is None:
        return None
    kind, a, radical = sub
    theta = sp.Symbol("theta", real=True)
    if kind == "sin":
        x_sub = a * sp.sin(theta)
        dx = a * sp.cos(theta)
        rad_sub = a * sp.cos(theta)
        new = sp.simplify(expr.xreplace({var: x_sub, radical: rad_sub}) * dx)
        inner = my_integrate(full_trig_rewrite(new), theta)
        if inner is None:
            return None
        back = sp.asin(var / a)
        return sp.simplify(inner.subs(theta, back))
    if kind == "tan":
        x_sub = a * sp.tan(theta)
        dx = a * sp.sec(theta)**2
        rad_sub = a * sp.sec(theta)
        new = sp.simplify(expr.xreplace({var: x_sub, radical: rad_sub}) * dx)
        inner = my_integrate(full_trig_rewrite(new), theta)
        if inner is None:
            return None
        back = sp.atan(var / a)
        return sp.simplify(inner.subs(theta, back))
    if kind == "sec":
        x_sub = a * sp.sec(theta)
        dx = a * sp.sec(theta) * sp.tan(theta)
        rad_sub = a * sp.tan(theta)
        new = sp.simplify(expr.xreplace({var: x_sub, radical: rad_sub}) * dx)
        inner = my_integrate(full_trig_rewrite(new), theta)
        if inner is None:
            return None
        # back-substitute theta = acos(a/x)
        back = sp.acos(a / var)
        return sp.simplify(inner.subs(theta, back))
    return None

# -----------------------
# Core integration engine (unified, fixed)
# -----------------------
def my_integrate(expr: sp.Expr, var: sp.Symbol) -> Optional[sp.Expr]:
    expr = sp.simplify(expr)
    expr = full_trig_rewrite(expr)

    # constants
    if expr.is_Number:
        return sp.simplify(expr * var)

    # variable
    if expr == var:
        return sp.simplify(var**2 / 2)

    # linearity
    if expr.is_Add:
        parts = [my_integrate(a, var) for a in expr.args]
        if any(p is None for p in parts):
            return None
        return sp.simplify(sp.Add(*parts))

    # constant multiple extraction
    if expr.is_Mul:
        const, rest = expr.as_independent(var)
        if const != 1:
            inner = my_integrate(rest, var)
            if inner is not None:
                return sp.simplify(const * inner)

    # power rule for var^n
    if expr.is_Pow and expr.base == var and expr.exp.is_Number:
        n = expr.exp
        if n != -1:
            return sp.simplify(var**(n + 1) / (n + 1))

    # partial fraction preprocessing
    pf = try_partial_fraction(expr, var)
    if pf is not None:
        return pf

    # exp(a*x + b) with linear a
    if expr.func == sp.exp:
        a = expr.args[0]
        # try linear coefficient
        try:
            poly = sp.Poly(sp.expand(a), var)
            if poly.degree() == 1:
                coeff = poly.coeffs()[0]
                if coeff != 0:
                    return sp.simplify(expr / coeff)
        except Exception:
            pass

    # sin(kx + c), cos(kx + c) primitives
    if expr.func in (sp.sin, sp.cos):
        a = expr.args[0]
        try:
            poly = sp.Poly(sp.expand(a), var)
            if poly.degree() == 1:
                k = poly.coeffs()[0]
                if k != 0:
                    if expr.func == sp.sin:
                        return sp.simplify(-sp.cos(a) / k)
                    else:
                        return sp.simplify(sp.sin(a) / k)
        except Exception:
            pass

    # 1/(a x + b)
    if expr.is_Pow and expr.exp == -1:
        base = expr.base
        try:
            poly = sp.Poly(sp.expand(base), var)
            if poly.degree() == 1:
                a = poly.coeffs()[0]
                if a != 0:
                    return sp.simplify(sp.log(base) / a)
        except Exception:
            pass

    # specific rational quadratic forms 1/(x^2 + a^2)
    if expr.is_Pow and expr.exp == -1 and expr.base.is_Add:
        base = sp.expand(expr.base)
        # attempt x^2 + a^2
        try:
            poly = sp.Poly(base, var)
            if poly.degree() == 2:
                A, B, C = poly.coeffs()
                # try completing square pattern if B == 0
                if B == 0 and A != 0 and C != 0:
                    a2 = C / A
                    if a2.is_Number and a2 > 0:
                        a = sp.sqrt(a2)
                        return sp.simplify(sp.atan(var / a) / (A * a))
        except Exception:
            pass

    # u-substitution
    u = try_u_sub(expr, var)
    if u is not None:
        return sp.simplify(u)

    # integration by parts
    p = try_integration_by_parts(expr, var)
    if p is not None:
        return sp.simplify(p)

    # trig substitution
    t = apply_trig_sub(expr, var)
    if t is not None:
        return sp.simplify(t)

    # fallback: couldn't find rule
    return None

# -----------------------
# Public wrapper
# -----------------------
def solve_from_latex(latex_str: str, variable: str = "x", mode: str = "integrate") -> Tuple[sp.Expr, str]:
    var = make_symbol(variable)
    expr = safe_parse(latex_str)
    expr = sp.simplify(expr)
    if mode == "diff":
        res = sp.simplify(my_diff(expr, var))
    elif mode == "integrate":
        res = my_integrate(expr, var)
        if res is None:
            raise NotImplementedError(f"Integration rule not found for integrand: {sp.srepr(expr)}")
        res = sp.simplify(res)
    else:
        raise ValueError("mode must be 'diff' or 'integrate'")
    return res, sp.latex(res)

# -----------------------
# Quick CLI-style tests when run directly
# -----------------------
if __name__ == "__main__":
    tests = [
        (r"\sin(x)e^{x}", "diff"),
        (r"x^3 + 2x", "integrate"),
        (r"\sin(x^2) 2 x", "integrate"),
        (r"x e^{x}", "integrate"),
        (r"\sin^2 x", "integrate"),
        (r"\sqrt{9 - x^2}", "integrate"),
        (r"\frac{1}{x^2 + 4}", "integrate")
    ]

    for tex, mode in tests:
        try:
            res, latex = solve_from_latex(tex, "x", mode)
            print(f"{mode.upper():9} {tex:20} => {res}")
            print(f" LaTeX: {latex}\n")
        except Exception as e:
            print(f"FAILED {mode} {tex}: {e}\n")
