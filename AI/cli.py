import click
from src.agents.mathematical_agent import MathematicalAgent
from src.agents.sage_agent import SageAgent
from src.agents.mentor_agent import MentorAgent
from src.memory.memory_manager import MemoryManager
from yaml import safe_load

@click.group()
@click.pass_context
def cli(ctx):
    """Plasma Assistant CLI for mathematical plasma physics research."""
    with open("config/config.yaml", "r") as f:
        config = safe_load(f)
    memory = MemoryManager(config["memory"])
    ctx.obj = {
        "mathematical_agent": MathematicalAgent(config, memory),
        "sage_agent": SageAgent(config, memory),
        "mentor_agent": MentorAgent(config, memory)
    }

@cli.command()
@click.argument("problem")
@click.pass_context
def solve(ctx, problem):
    """Solve a mathematical problem (e.g., plasma dispersion relation)."""
    result = ctx.obj["mathematical_agent"].solve_problem(problem)
    click.echo(result)

@cli/UIKit

System: It looks like your response was cut off at the CLI scaffold (`cli.py`) in the `@cli.command()` section. I'll complete the implementation based on your instructions, ensuring a modular, extensible AI assistant for a graduate student in mathematical plasma physics. Below, I’ll provide the full set of Python code scaffolds for the core components (`cli.py`, `memory_manager.py`, `task_planner.py`, `math_tools.py`, and related agent files), along with explanations and an example usage scenario. The system will use OpenAI’s GPT-4, ChromaDB for memory, and a CLI interface, tailored for plasma physics research.
