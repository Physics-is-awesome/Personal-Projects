from src.planner.task_planner import TaskPlanner

class MentorAgent:
    def __init__(self, config, memory):
        self.planner = TaskPlanner(config, memory)

    def generate_curriculum(self, topic):
        """Generate a learning curriculum."""
        return self.planner.generate_curriculum(topic)

    def reflect_progress(self):
        """Reflect on learning progress."""
        return self.planner.reflect_progress()
