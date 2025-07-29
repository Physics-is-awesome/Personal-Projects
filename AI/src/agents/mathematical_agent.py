from src.tools.math_tools import MathTools
from src.llm.llm_interface import LLMInterface
import re

class MathematicalAgent:
    def __init__(self, config, memory):
        self.math_tools = MathTools()
        self.llm = LLMInterface(config["llm"])
        self.memory = memory

    def solve_problem(self, problem):
        """Solve a mathematical problem (symbolic, numerical, or theorem proving)."""
        results, _ = self.memory.query("math_solutions", problem, n_results=1)
        if results and results[0]:
            return f"Retrieved from memory: {results[0]}"

        # Extract equation from the problem string (e.g., "w^2 = wp^2 + 3*k^2*vth^2")
        equation_match = re.search(r'[\w\^*/+\-\s=]+\s*=\s*[\w\^*/+\-\s]+', problem)
        if "solve" in problem.lower() and "=" in problem and equation_match:
            try:
                equation = equation_match.group(0)
                # Convert to SymPy-compatible format (e.g., replace ^ with **)
                equation = equation.replace('^', '**')
                # Move all terms to one side (e.g., w**2 = wp**2 + 3*k**2*vth**2 -> w**2 - wp**2 - 3*k**2*vth**2 = 0)
                left, right = equation.split('=')
                equation = f"{left.strip()} - ({right.strip()})"
                solution = self.math_tools.solve_symbolic(equation, 'w')
                self.memory.store("math_solutions", solution, {"problem": problem})
                return solution
            except Exception as e:
                return f"Error processing equation: {str(e)}"
        elif "simulate" in problem.lower():
            return self.math_tools.solve_numerical(lambda y, t: [0], [1], [0, 1])[0]
        else:
            prompt = f"For a mathematical plasma physics problem: {problem}, suggest a solution approach or prove a theorem."
            response = self.llm.generate(prompt)
            self.memory.store("math_solutions", response, {"problem": problem})
            return response
