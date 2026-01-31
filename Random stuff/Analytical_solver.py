import sympy as sp
from sympy.parsing.sympy_parser import (
    parse_expr,
    standard_transformations,
    implicit_multiplication_application,
)
from sympy.parsing.latex import parse_latex

# Symbol
x = sp.symbols('x')

# Parser transformations
TRANSFORMS = standard_transformations + (implicit_multiplication_application,)


def parse_input(expr_str):
    """
    Try LaTeX first, then fall back to SymPy expression parsing.
    """
    try:
        return parse_latex(expr_str)
    except Exception:
        return parse_expr(expr_str, transformations=TRANSFORMS)


def process(expr_str, mode):
    try:
        expr = parse_input(expr_str)

        if mode == "diff":
            result = sp.diff(expr, x)
        elif mode == "integrate":
            result = sp.integrate(expr, x)
        else:
            raise ValueError("Unknown mode")

        print(f"\nRESULT => {result}")
        print(f"LaTeX  => {sp.latex(result)}\n")

    except Exception as e:
        print(f"\nFAILED {mode} {expr_str}: {e}\n")


def main():
    print("SymPy Differentiation / Integration Tool")
    print("Enter expressions in LaTeX or normal syntax.")
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

