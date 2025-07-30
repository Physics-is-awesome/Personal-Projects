from src.tools.math_tools import MathTools
from src.llm.llm_interface import LLMInterface
import re
import sympy as sp

class MathematicalAgent:
    def __init__(self, config, memory):
        self.math_tools = MathTools()
        self.llm = LLMInterface(config["llm"])
        self.memory = memory

    def solve_problem(self, problem):
        """Solve a mathematical problem (symbolic, numerical, or theorem proving)."""
        results, _ = self.memory.query("math_solutions", problem, n_results=1)
        if results and results[0] and "Error" not in results[0]:
            return f"Retrieved from memory: {results[0]}"

        # Extract equation (e.g., "w^2 = wp^2 + 3*k^2*vth^2")
        equation_match = re.search(r'([\w\s\^*/+\-]+)\s*=\s*([\w\s\^*/+\-]+)', problem)
        if "solve" in problem.lower() and "=" in problem and equation_match:
            try:
                left, right = equation_match.groups()
                # Convert to SymPy-compatible format
                equation = f"{left.strip().replace('^', '**')} - ({right.strip().replace('^', '**')})"
                # Define symbols
                symbols = re.findall(r'\b\w+\b', equation)
                for sym in symbols:
                    if sym not in locals():
                        locals()[sym] = sp.Symbol(sym)
                solution = self.math_tools.solve_symbolic(equation, 'w')
                if "Error" not in solution:
                    self.memory.store("math_solutions", solution, {"problem": problem})
                return solution
            except Exception as e:
                error_msg = f"Error processing equation: {str(e)}"
                return error_msg
        elif "simulate" in problem.lower():
            solution = self.math_tools.solve_numerical(lambda y, t: [0], [1], [0, 1])[0]
            self.memory.store("math_solutions", str(solution), {"problem": problem})
            return solution
        else:
            prompt = f"For a mathematical plasma physics problem: {problem}, suggest a solution approach or prove a theorem."
            response = self.llm.generate(prompt)
            self.memory.store("math_solutions", response, {"problem": problem})
            return response
