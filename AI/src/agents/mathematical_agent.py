import sympy as sp
import numpy as np
from scipy.integrate import odeint

class MathTools:
    def __init__(self):
        sp.init_printing()

    def solve_symbolic(self, equation, variable):
        """Solve a symbolic equation using SymPy."""
        try:
            var = sp.Symbol(variable)
            solution = sp.solve(equation, var)
            return str(solution)
        except Exception as e:
            return f"Error solving symbolically: {str(e)}"

    def solve_numerical(self, func, y0, t):
        """Solve an ODE numerically using SciPy."""
        try:
            solution = odeint(func, y0, t)
            return solution
        except Exception as e:
            return f"Error solving numerically: {str(e)}"

    def suggest_tool(self, problem_description):
        """Suggest a mathematical tool for a problem."""
        # Simple heuristic-based suggestion (can be enhanced with LLM)
        if "dispersion" in problem_description.lower():
            return "Use SymPy for symbolic derivation of dispersion relations."
        elif "simulation" in problem_description.lower():
            return "Use SciPy for numerical ODE solving or finite element methods."
        return "Consider SymPy for symbolic math or SciPy for numerical analysis."
