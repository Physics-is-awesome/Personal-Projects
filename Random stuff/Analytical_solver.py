import sympy as sp
from sympy.parsing.latex import parse_latex

# -------------------------
# Safe parse
# -------------------------
def safe_parse(s):
    try:
        return parse_latex(s)
    except Exception:
        return sp.sympify(s)

# -------------------------
# Differentiation (FIXED)
# -------------------------
def my_diff(expr, x):
    if expr.is_Number:
        return 0
    if expr == x:
        return 1
    if expr.is_Symbol:
        return 0

    if expr.is_Add:
        return sum(my_diff(a, x) for a in expr.args)

    if expr.is_Mul:
        terms = []
        args = expr.args
        for i in range(len(args)):
            d = my_diff(args[i], x)
            if d != 0:
                other = sp.Mul(*(args[j] for j in range(len(args)) if j != i))
                terms.append(d * other)
        return sum(terms)

    if expr.is_Pow:
        base, exp = expr.args
        if exp.is_Number:
            return exp * base**(exp - 1) * my_diff(base, x)

    if expr.func == sp.exp:
        return expr * my_diff(expr.args[0], x)

    if expr.func == sp.sin:
        return sp.cos(expr.args[0]) * my_diff(expr.args[0], x)

    if expr.func == sp.cos:
        return -sp.sin(expr.args[0]) * my_diff(expr.args[0], x)

    raise NotImplementedError(expr)

# -------------------------
# Integration helpers
# -------------------------
def integrate_power(expr, x):
    if expr.is_Pow and expr.base == x and expr.exp != -1:
        return x**(expr.exp + 1) / (expr.exp + 1)
    return None

def integrate_elementary(expr, x):
    if expr == x:
        return x**2 / 2
    if expr.func == sp.exp and expr.args[0] == x:
        return expr
    if expr.func == sp.sin and expr.args[0] == x:
        return -sp.cos(x)
    if expr.func == sp.cos and expr.args[0] == x:
        return sp.sin(x)
    return None

def try_u_sub(expr, x):
    for u in sp.preorder_traversal(expr):
        if u == x:
            continue
        du = my_diff(u, x)
        if du == 0:
            continue
        q = sp.simplify(expr / du)
        if not q.has(x):
            t = sp.Symbol("t")
            inner = my_integrate(q.subs(u, t), t)
            if inner is not None:
                return inner.subs(t, u)
    return None

def try_parts(expr, x):
    if not expr.is_Mul:
        return None
    for u in expr.args:
        dv = expr / u
        v = my_integrate(dv, x)
        if v is not None:
            du = my_diff(u, x)
            rest = my_integrate(v * du, x)
            if rest is not None:
                return u * v - rest
    return None

def trig_identity(expr):
    if expr == sp.sin(x)**2:
        return (1 - sp.cos(2*x))/2
    return expr

# -------------------------
# Integration (FIXED ORDER)
# -------------------------
def my_integrate(expr, x):
    expr = sp.simplify(expr)
    expr = trig_identity(expr)

    if expr.is_Number:
        return expr * x

    if expr.is_Add:
        parts = [my_integrate(a, x) for a in expr.args]
        if all(p is not None for p in parts):
            return sum(parts)
        return None

    if expr.is_Mul:
        c, r = expr.as_independent(x)
        if c != 1:
            inner = my_integrate(r, x)
            if inner is not None:
                return c * inner

    for rule in (integrate_power, integrate_elementary):
        out = rule(expr, x)
        if out is not None:
            return out

    u = try_u_sub(expr, x)
    if u is not None:
        return u

    p = try_parts(expr, x)
    if p is not None:
        return p

    return None

# -------------------------
# Interface
# -------------------------
def solve(latex, mode="integrate"):
    x = sp.Symbol("x")
    expr = safe_parse(latex)
    if mode == "diff":
        r = my_diff(expr, x)
    else:
        r = my_integrate(expr, x)
        if r is None:
            raise NotImplementedError(expr)
    return sp.simplify(r), sp.latex(sp.simplify(r))

