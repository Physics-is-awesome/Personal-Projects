import pdfplumber
from src.llm.llm_interface import LLMInterface
from src.planner.task_planner import TaskPlanner

class SageAgent:
    def __init__(self, config, memory):
        self.llm = LLMInterface(config["llm"])
        self.memory = memory
        self.planner = TaskPlanner(config, memory)

    def summarize_pdf(self, pdf_path):
        """Summarize a PDF file."""
        try:
            with pdfplumber.open(pdf_path) as pdf:
                text = "".join(page.extract_text() or "" for page in pdf.pages)
            prompt = f"Summarize the following academic paper text for a graduate student in mathematical plasma physics:\n{text[:2000]}"  # Truncate for LLM
            summary = self.llm.generate(prompt)
            self.memory.store("research_summaries", summary, {"pdf_path": pdf_path})
            return summary
        except Exception as e:
            return f"Error summarizing PDF: {str(e)}"

    def recommend_tasks(self, topic):
        """Recommend research tasks for a topic."""
        return self.planner.generate_tasks(topic)
