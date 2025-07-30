import chromadb
from src.memory.memory_manager import MemoryManager

config = {"memory": {"db_path": "data/memory/chromadb"}}
memory = MemoryManager(config["memory"])
memory.clear_collection("math_solutions")
print("Cleared math_solutions collection")
