import click
from src.agents.mathematical_agent import MathematicalAgent
from src.agents.sage_agent import SageAgent
from src.agents.mentor_agent import MentorAgent
from src.memory.memory_manager import MemoryManager
from yaml import safe_load
from dotenv import load_dotenv
import os

os.environ["CHROMADB_TELEMETRY_ENABLED"] = "false"

@click.group()
@click.pass_context
def cli(ctx):
    """Plasma Assistant CLI for mathematical plasma physics research."""
    load_dotenv()
    with open("config/config.yaml", "r") as f:
        config = safe_load(f)
    config["llm"]["api_key"] = os.getenv("OPENAI_API_KEY")
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

@cli.command()
@click.argument("pdf_path")
@click.pass_context
def summarize(ctx, pdf_path):
    """Summarize a PDF or academic paper."""
    summary = ctx.obj["sage_agent"].summarize_pdf(pdf_path)
    click.echo(summary)

@cli.command()
@click.option("--topic", default="plasma physics", help="Topic for research tasks")
@click.pass_context
def research(ctx, topic):
    """Recommend research tasks for a given topic."""
    tasks = ctx.obj["sage_agent"].recommend_tasks(topic)
    click.echo("\n".join(tasks))

@cli.command()
@click.option("--topic", default="plasma physics", help="Topic for learning curriculum")
@click.pass_context
def plan(ctx, topic):
    """Generate a learning curriculum for a topic."""
    curriculum = ctx.obj["mentor_agent"].generate_curriculum(topic)
    click.echo(curriculum)

@cli.command()
@click.pass_context
def reflect(ctx):
    """Reflect on learning progress and suggest next steps."""
    reflection = ctx.obj["mentor_agent"].reflect_progress()
    click.echo(reflection)
