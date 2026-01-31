# calc_engine.py
import sympy as sp
from sympy.parsing.latex import parse_latex

# -----------------------
# Utilities
# -----------------------
def parse_expr_from_latex(latex_str):
    try:
        return parse_latex(latex_str)
    except Exception as e:
        raise ValueError(f"LaTeX parsing failed: {e}")

def make_symbol(varname="x"):
    return sp.Symbol(varname, real=True)

def is_linear_in(e, var):
    # returns (a,b) if e == a*var + b (a,b possibly symbolic), else None
    if e.is_Add or e.is_Mul or e.is_Symbol or e.is_Number or e.is_Pow:
        p = sp.expand(e)
    else:
        p = e
    poly = p.as_poly(var)
    if poly is not None and poly.degree() <= 1:
        coeffs = [poly.coeff_monomial(var**i) for i in range(poly.degree()+1)]
        if poly.degree() == 0:
            a = sp.Integer(0)
            b = coeffs[0]
        else:
            a = coeffs[1]
            b = coeffs[0]
        return sp.simplify(a), sp.simplify(b)
    return None

def try_linear_coeff(expr, var):
    lin = is_linear_in(expr, var)
    if lin is not None:
        return lin
    return None

# -----------------------
# Differentiation (own rules)
# -----------------------
def my_diff(expr, var):
    # Do not call sympy.diff
    expr = sp.simplify(expr)
    if expr.is_Number:
        return sp.Integer(0)
    if expr == var:
        return sp.Integer(1)
    if expr.is_Symbol:
        return sp.Integer(0)

    if expr.is_Add:
        return sp.Add(*(my_diff(a, var) for a in expr.args))

    if expr.is_Mul:
        terms = []
        args = expr.args
        for i in range(len(args)):
            d = my_diff(args[i], var)
            if d != 0:
                other = sp.Mul(*([args[j] for j in range(len(args)) if j != i]))
                terms.append(sp.simplify(d * other))
        return sp.simplify(sp.Add(*terms))

    if expr.is_Pow:
        base, exp = expr.args
        if exp.is_Number:
            return sp.simplify(exp * base**(exp - 1) * my_diff(base, var))
        # power with variable exponent: d(a^b) = a^b*(b' ln a + b a'/a)
        return sp.simplify(
            expr * (my_diff(exp, var) * sp.log(base) + my_diff(base, var) * exp / base)
        )

    # Common functions
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
        return sp.simplify(sp.sec(a)*sp.tan(a) * my_diff(a, var))
    if f == sp.csc:
        return sp.simplify(-sp.csc(a)*sp.cot(a) * my_diff(a, var))
    if f == sp.asin:
        return sp.simplify(my_diff(a, var) / sp.sqrt(1 - a**2))
    if f == sp.atan:
        return sp.simplify(my_diff(a, var) / (1 + a**2))

    raise NotImplementedError(f"No diff rule implemented for {expr}")

# -----------------------
# Trig rewrite system
# -----------------------
def trig_rewrite_once(e):
    # One-step local rewrites (Calc-level)
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
        for i,a in enumerate(args):
            for j,b in enumerate(args):
                if i != j and a.func == sp.sin and b.func == sp.cos and a.args[0] == b.args[0]:
                    u = a.args[0]
                    rest = [r for k,r in enumerate(args) if k not in (i,j)]
                    return sp.Mul(sp.Rational(1,2), sp.sin(2*u), *rest)

    return e

def full_trig_rewrite(expr):
    prev = None
    curr = sp.simplify(expr)
    # iterative fixed-point application
    while prev != curr:
        prev = curr
        curr = curr.replace(lambda e: True, trig_rewrite_once)
        curr = sp.simplify(curr)
    return curr

# -----------------------
# Partial fractions / rational handling
# -----------------------
def try_partial_fraction(expr, var):
    # only try when expr is rational in var
    poly = sp.together(expr)
    poly = sp.simplify(poly)
    try:
        a = sp.apart(poly, var)
    except Exception:
        return None
    if a != expr:
        # apart decomposed it; integrate each term
        terms = a.as_ordered_factors() if a.is_Mul else a
        # but easier: break into addends
        addends = a.as_ordered_terms() if a.is_Add else [a]
        results = []
        for t in addends:
            r = my_integrate(t, var)
            if r is None:
                return None
            results.append(r)
        return sp.Add(*results)
    return None

# -----------------------
# u-substitution detection
# -----------------------
def collect_subexprs(expr):
    # return list of candidate subexpressions ordered by size (larger first)
    seen = set()
    out = []
    def walk(e):
        if e in seen:
            return
        seen.add(e)
        out.append(e)
        for a in e.args:
            walk(a)
    walk(expr)
    # sort so more complex (more args) first
    out.sort(key=lambda s: (-len(tuple(s.preorder_traversal())), str(s)))
    return out

def try_u_sub(expr, var):
    expr = sp.simplify(expr)
    # We want expr = f(u) * u'(x) or const * f(u) * u'(x)
    # Approach: for each candidate u (subexpr), compute du and see if expr is divisible by du
    candidates = collect_subexprs(expr)
    for u in candidates:
        if u == expr:
            continue
        du = sp.simplify(my_diff(u, var))
        if du == 0:
            continue
        # see if expr/du is independent of var (i.e., function of u only or constant)
        ratio = sp.simplify(expr / du)
        # check if ratio still contains var
        if not ratio.has(var):
            # integral is ∫ ratio(u) du
            # replace u variable with dummy symbol t to integrate by our engine recursively
            t = sp.Symbol('u_temp', real=True)
            # form function in t by replacing u->t in ratio
            f_of_u = ratio.xreplace({u: t})
            inner = my_integrate(f_of_u, t)
            if inner is None:
                # fallback: if ratio is just a constant, return that constant * u
                if f_of_u.free_symbols == set():
                    return sp.simplify(f_of_u * u)
                continue
            # substitute back t->u
            return sp.simplify(inner.xreplace({t: u}))
    return None

# -----------------------
# Integration by parts (heuristic)
# -----------------------
def pick_u_for_parts(expr, var):
    # LIATE-like heuristic: Log/Inverse trig/Algebraic/Trig/Exp
    # return chosen u and dv (as expression factors)
    # For simplicity: try single-factor u among multiplicative factors
    if not expr.is_Mul:
        return None
    factors = list(expr.args)
    priority = []
    for f in factors:
        score = 0
        # log
        if f.func == sp.log:
            score += 50
        # inverse trig
        if f.func in (sp.asin, sp.acos, sp.atan):
            score += 40
        # algebraic (polynomial / powers)
        if f.is_Pow or f.is_Symbol or f.is_polynomial:
            try:
                if sp.Poly(f, var).degree() >= 0:
                    score += 30
            except Exception:
                score += 10
        # trig
        if f.func in (sp.sin, sp.cos, sp.tan, sp.sec):
            score += 20
        # exp
        if f.func == sp.exp:
            score += 10
        # default small bonus for simpler shapes
        priority.append((score, f))
    if not priority:
        return None
    priority.sort(reverse=True, key=lambda x: x[0])
    u = priority[0][1]
    dv = sp.Mul(*([f for f in factors if f != u]))
    return u, dv

def try_integration_by_parts(expr, var):
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
# Trig substitution
# -----------------------
def detect_trig_sub(expr, var):
    # Look for radicals sqrt(a^2 - x^2), sqrt(a^2 + x^2), sqrt(x^2 - a^2)
    sqrts = [s for s in expr.atoms(sp.sqrt)]
    for s in sqrts:
        inside = sp.simplify(s.args[0])
        # check patterns
        # a^2 - x^2
        if inside.is_Add:
            # try to detect two-term form
            terms = sp.Add.make_args(inside)
            if len(terms) == 2:
                # try find numeric or symbol a^2 and -x^2
                for t in terms:
                    other = inside - t
                    if t.is_Pow and t.exp == 2 and other == -var**2:
                        a = sp.sqrt(t)
                        return ("sin", a, s)
                    if other.is_Pow and other.exp == 2 and t == -var**2:
                        a = sp.sqrt(other)
                        return ("sin", a, s)
        # x^2 + a^2
        if inside.is_Add:
            terms = sp.Add.make_args(inside)
            # find var**2 and a^2
            if var**2 in terms:
                rest = sp.simplify(inside - var**2)
                if rest.is_Pow and rest.exp == 2:
                    a = sp.sqrt(rest)
                    return ("tan", a, s)
                if rest.free_symbols == set() and rest.is_Number:
                    a = sp.sqrt(rest)
                    return ("tan", a, s)
        # x^2 - a^2
        if inside.is_Add:
            if var**2 in sp.Add.make_args(inside):
                rest = inside - var**2
                if rest.is_Pow and rest.exp == -2:
                    pass
            # try x^2 - a^2
            if inside.has(var**2):
                # attempt to express as var**2 - a**2
                try:
                    a2 = sp.simplify(inside - var**2)
                    if a2.is_Number or (a2.is_Pow and a2.exp == 2):
                        if a2.is_Number:
                            a = sp.sqrt(a2)
                        else:
                            a = sp.sqrt(a2)  # symbolic
                        # ensure sign
                        # confirm shape var**2 - a**2
                        if sp.simplify(var**2 - a**2) == inside:
                            return ("sec", a, s)
                except Exception:
                    continue
    return None

def apply_trig_sub(expr, var):
    sub = detect_trig_sub(expr, var)
    if sub is None:
        return None
    kind, a, radical = sub
    theta = sp.Symbol('theta', real=True)

    if kind == "sin":
        # x = a sin θ, dx = a cos θ dθ, sqrt(a^2 - x^2) = a cos θ
        x_sub = a * sp.sin(theta)
        dx = a * sp.cos(theta)
        sqrt_sub = a * sp.cos(theta)
        new = sp.simplify(expr.xreplace({var: x_sub, radical: sqrt_sub}) * dx)
        res_theta = my_integrate(full_trig_rewrite(new), theta)
        if res_theta is None:
            return None
        # back-substitute θ = asin(x/a)
        back = sp.asin(var / a)
        return sp.simplify(res_theta.subs(theta, back))
    if kind == "tan":
        # x = a tan θ, dx = a sec^2 θ dθ, sqrt(a^2 + x^2) = a sec θ
        x_sub = a * sp.tan(theta)
        dx = a * sp.sec(theta)**2
        sqrt_sub = a * sp.sec(theta)
        new = sp.simplify(expr.xreplace({var: x_sub, radical: sqrt_sub}) * dx)
        res_theta = my_integrate(full_trig_rewrite(new), theta)
        if res_theta is None:
            return None
        back = sp.atan(var / a)
        return sp.simplify(res_theta.subs(theta, back))
    if kind == "sec":
        # x = a sec θ, dx = a sec θ tan θ dθ, sqrt(x^2 - a^2) = a tan θ
        x_sub = a * sp.sec(theta)
        dx = a * sp.sec(theta) * sp.tan(theta)
        sqrt_sub = a * sp.tan(theta)
        new = sp.simplify(expr.xreplace({var: x_sub, radical: sqrt_sub}) * dx)
        res_theta = my_integrate(full_trig_rewrite(new), theta)
        if res_theta is None:
            return None
        # back-substitute θ = acos(a/x)
        back = sp.acos(a / var)
        return sp.simplify(res_theta.subs(theta, back))

# -----------------------
# Core integration engine
# -----------------------
def my_integrate(expr, var):
    expr = sp.simplify(expr)
    # Safety small-step simplification
    expr = full_trig_rewrite(expr)

    # Base: constant
    if expr.is_Number:
        return sp.simplify(expr * var)

    # Variable itself
    if expr == var:
        return sp.simplify(var**2 / 2)

    # Linearity
    if expr.is_Add:
        parts = [my_integrate(a, var) for a in expr.args]
        if any(p is None for p in parts):
            return None
        return sp.simplify(sp.Add(*parts))

    # Constant multiple extraction
    if expr.is_Mul:
        const, rest = expr.as_independent(var)
        if const != 1:
            inner = my_integrate(rest, var)
            if inner is not None:
                return sp.simplify(const * inner)

    # Power rule for var^n
    if expr.is_Pow and expr.base == var and expr.exp.is_Number:
        n = expr.exp
        if n != -1:
            return sp.simplify(var**(n + 1) / (n + 1))

    # Rational / partial fractions (preprocess)
    pf = try_partial_fraction(expr, var)
    if pf is not None:
        return sp.simplify(pf)

    # Exponential with linear argument: integrate e^{a x + b} -> e^{ax+b}/a
    if expr.func == sp.exp:
        arg = expr.args[0]
        lin = try_linear_coeff(arg, var)
        if lin is not None:
            a, b = lin
            if a != 0:
                return sp.simplify(expr / a)

    # sin(kx), cos(kx)
    if expr.func == sp.sin:
        a = expr.args[0]
        lin = try_linear_coeff(a, var)
        if lin is not None:
            k, c = lin
            if k != 0:
                return sp.simplify(-sp.cos(a) / k)
    if expr.func == sp.cos:
        a = expr.args[0]
        lin = try_linear_coeff(a, var)
        if lin is not None:
            k, c = lin
            if k != 0:
                return sp.simplify(sp.sin(a) / k)

    # 1/(a x + b)
    if expr.is_Pow and expr.exp == -1:
        base = expr.base
        lin = try_linear_coeff(base, var)
        if lin is not None:
            a, b = lin
            if a != 0:
                return sp.simplify(sp.log(base) / a)

    # 1/(x^2 + a^2) and 1/(x^2 - a^2)
    if expr.is_Pow and expr.exp == -1 and expr.base.is_Add:
        base = sp.expand(expr.base)
        # x^2 + a^2
        if base.match(var**2 + sp.Wild('A')):
            A = (base - var**2)
            if A.free_symbols == set() and A != 0:
                a = sp.sqrt(A)
                return sp.simplify(sp.atan(var / a) / a)
        # x^2 - a^2
        if base.match(var**2 - sp.Wild('A')):
            A = (var**2 - base)
            # Actually easier: check factorization
            fac = sp.factor(base)
            if fac.is_Mul:
                # leave to partial fractions
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

    # Rational special cases (1/(x^2 + a^2) as direct pattern):
    # Try to match 1/(A*var**2 + B*var + C)
    poly = sp.Poly(sp.together(expr), var) if sp.Poly(sp.together(expr), var).degree() <= 2 else None
    try:
        poly = sp.Poly(sp.together(expr), var)
    except Exception:
        poly = None
    if poly is not None and poly.degree() == 2:
        # attempt specific forms
        A = poly.coeffs()[0]
        B = poly.coeffs()[1] if len(poly.coeffs()) > 1 else 0
        C = poly.coeffs()[-1]
        denom = A*var**2 + B*var + C
        # complete square
        if B == 0 and A != 0 and sp.sign(A) > 0 and C != 0:
            # A(x^2 + C/A)
            if sp.simplify(expr - 1/denom) == 0:
                a2 = C/A
                if a2.is_Number and a2 > 0:
                    a = sp.sqrt(a2)
                    return sp.simplify(sp.atan(var / a) / (A * a))

    return None

# -----------------------
# User-facing wrapper
# -----------------------
def solve_from_latex(latex_str, variable="x", mode="integrate"):
    var = make_symbol(variable)
    expr = parse_expr_from_latex(latex_str)
    expr = sp.simplify(expr)
    if mode == "diff":
        res = sp.simplify(my_diff(expr, var))
    elif mode == "integrate":
        res = sp.simplify(my_integrate(expr, var))
        if res is None:
            raise NotImplementedError("Integration rule not found for this integrand.")
    else:
        raise ValueError("mode must be 'diff' or 'integrate'")
    return res, sp.latex(res)

# -----------------------
# If run directly, quick examples
# -----------------------
if __name__ == "__main__":
    examples = [
        (r"\sin(x)e^{x}", "diff"),
        (r"x^3 + 2x", "integrate"),
        (r"\sin(x^2) 2 x", "integrate"),
        (r"x e^{x}", "integrate"),
        (r"\sin^2 x", "integrate"),
        (r"\sqrt{9 - x^2}", "integrate"),  # trig sub example (note: integrand has no dx multiplier)
        (r"\frac{1}{x^2 + 4}", "integrate")
    ]
    for tex, mode in examples:
        try:
            res, latex = solve_from_latex(tex, "x", mode)
            print(f"{mode.upper()} {tex}  =>  {res}")
            print(f" LaTeX: {latex}\n")
        except Exception as e:
            print(f"Failed on {tex} ({mode}): {e}\n")
