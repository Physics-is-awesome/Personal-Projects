import sympy as sp
from sympy.parsing.sympy_parser import (
    parse_expr,
    standard_transformations,
    implicit_multiplication_application,
)
from sympy.parsing.latex import parse_latex
from sympy.integrals.manualintegrate import manualintegrate

x = sp.symbols('x')

TRANSFORMS = standard_transformations + (implicit_multiplication_application,)


def parse_input(expr_str):
    try:
        return parse_latex(expr_str)
    except Exception:
        return parse_expr(expr_str, transformations=TRANSFORMS)


# ---------------- DIFFERENTIATION STEPS ---------------- #

def diff_steps(expr):
    steps = []

    def recurse(e):
        if e.is_Atom:
            steps.append(f"d/dx({e}) = {sp.diff(e, x)}")
            return sp.diff(e, x)

        if isinstance(e, sp.Add):
            steps.append("Using linearity of differentiation:")
            result = sum(recurse(arg) for arg in e.args)
            return result

        if isinstance(e, sp.Mul):
            steps.append("Using the product rule:")
            u, v = e.args[0], sp.Mul(*e.args[1:])
            du = recurse(u)
            dv = recurse(v)
            result = du * v + u * dv
            steps.append(f"d({u}·{v}) = {du}·{v} + {u}·{dv}")
            return result

        if isinstance(e, sp.Pow):
            base, exp = e.args
            if exp.has(x):
                steps.append("Using the chain rule:")
            return sp.diff(e, x)

        steps.append(f"Differentiate {e}")
        return sp.diff(e, x)

    result = recurse(expr)
    return result, steps


# ---------------- INTEGRATION STEPS ---------------- #

def integrate_steps(expr):
    steps = []

    def recurse(e):
        result = manualintegrate(e, x)
        steps.append(f"∫ {e} dx = {result}")
        return result

    result = recurse(expr)
    return result, steps


# ---------------- MAIN PROCESS ---------------- #

def process(expr_str, mode):
    try:
        expr = parse_input(expr_str)

        if mode == "diff":
            result, steps = diff_steps(expr)
        else:
            result, steps = integrate_steps(expr)

        print("\nSTEPS:")
        for s in steps:
            print(" •", s)

        print("\nFINAL RESULT =>", result)
        print("LaTeX =>", sp.latex(result), "\n")

    except Exception as e:
        print(f"\nFAILED {mode} {expr_str}: {e}\n")


def main():
    print("Symbolic Calculus Tool (with Steps)")
    print("Supports LaTeX or SymPy syntax")
    print("Commands: diff, integrate, quit\n")

    while True:
        mode = input("Mode (diff / integrate / quit): ").strip().lower()
        if mode == "quit":
            break
        if mode not in ("diff", "integrate"):
            print("Invalid mode.\n")
            continue

        expr_str = input("Expression: ").strip()
        process(expr_str, mode)


if __name__ == "__main__":
    main()

