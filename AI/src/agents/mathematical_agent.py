from src.tools.math_tools import MathTools
from src.llm.llm_interface import LLMInterface

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

        if "solve" in problem.lower() and "=" in problem:
            try:
                var = "x"
                solution = self.math_tools.solve_symbolic(problem, var)
                self.memory.store("math_solutions", solution, {"problem": problem})
                return solution
            except:
                pass
        elif "simulate" in problem.lower():
            return self.math_tools.solve_numerical(lambda y, t: [0], [1], [0, 1])[0]
        else:
            prompt = f"For a mathematical plasma physics problem: {problem}, suggest a solution approach or prove a theorem."
            response = self.llm.generate(prompt)
            self.memory.store("math_solutions", response, {"problem": problem})
            return response
