from src.llm.llm_interface import LLMInterface

class TaskPlanner:
    def __init__(self, config, memory):
        self.llm = LLMInterface(config["llm"])
        self.memory = memory

    def generate_tasks(self, topic):
        """Generate research or learning tasks for a topic."""
        prompt = f"Generate a list of 3-5 research or learning tasks for a graduate student studying {topic} in mathematical plasma physics."
        response = self.llm.generate(prompt)
        tasks = response.split("\n")
        self.memory.store("learning_progress", f"Tasks for {topic}: {response}", {"topic": topic})
        return tasks

    def generate_curriculum(self, topic):
        """Generate a personalized learning curriculum."""
        prompt = f"Create a 4-week learning curriculum for a graduate student in mathematical plasma physics focusing on {topic}. Include weekly goals and resources."
        curriculum = self.llm.generate(prompt)
        self.memory.store("learning_progress", curriculum, {"topic": topic, "type": "curriculum"})
        return curriculum

    def reflect_progress(self):
        """Reflect on stored progress and suggest next steps."""
        _, metadata = self.memory.query("learning_progress", "curriculum", n_results=1)
        latest_topic = metadata[0].get("topic", "plasma physics")
        prompt = f"Reflect on a graduate student's progress in {latest_topic} and suggest next steps for learning."
        reflection = self.llm.generate(prompt)
        self.memory.store("learning_progress", reflection, {"type": "reflection"})
        return reflection
