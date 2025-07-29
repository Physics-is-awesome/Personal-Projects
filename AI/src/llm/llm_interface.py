from openai import OpenAI

class LLMInterface:
    def __init__(self, config):
        self.client = OpenAI(api_key=config["api_key"], timeout=30)

    def generate(self, prompt):
        """Generate a response using OpenAI's GPT-4."""
        try:
            response = self.client.chat.completions.create(
                model="gpt-4",
                messages=[{"role": "user", "content": prompt}],
                timeout=30
            )
            return response.choices[0].message.content
        except Exception as e:
            return f"LLM Error: {str(e)}"
