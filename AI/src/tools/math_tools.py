import sympy as sp
  import numpy as np
  from scipy.integrate import odeint

  class MathTools:
      def __init__(self):
          sp.init_printing()

      def solve_symbolic(self, equation, variable):
          """Solve a symbolic equation using SymPy."""
          try:
              # Define common plasma physics symbols
              w, wp, k, vth = sp.symbols('w wp k vth')
              # Parse the equation
              expr = sp.sympify(equation)
              # Solve for the variable
              solution = sp.solve(expr, variable)
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
          if "dispersion" in problem_description.lower():
              return "Use SymPy for symbolic derivation of dispersion relations."
          elif "simulate" in problem_description.lower():
              return "Use SciPy for numerical ODE solving or finite element methods."
          return "Consider SymPy for symbolic math or SciPy for numerical analysis."
