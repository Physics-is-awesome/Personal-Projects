import chromadb
import uuid
from datetime import datetime

class MemoryManager:
    def __init__(self, config):
        self.client = chromadb.PersistentClient(
            path=config["db_path"],
            settings=chromadb.Settings(allow_reset=True, anonymized_telemetry=False)
        )
        self.collections = {
            "math_solutions": self.client.get_or_create_collection("math_solutions"),
            "research_summaries": self.client.get_or_create_collection("research_summaries"),
            "learning_progress": self.client.get_or_create_collection("learning_progress")
        }

    def store(self, collection_name, data, metadata=None):
        """Store data in a specified collection with metadata."""
        collection = self.collections.get(collection_name)
        if not collection:
            raise ValueError(f"Unknown collection: {collection_name}")
        doc_id = str(uuid.uuid4())
        collection.add(
            documents=[data],
            metadatas=[metadata or {"timestamp": datetime.now().isoformat()}],
            ids=[doc_id]
        )
        return doc_id

    def query(self, collection_name, query_text, n_results=5):
        """Query a collection for relevant data."""
        collection = self.collections.get(collection_name)
        if not collection:
            raise ValueError(f"Unknown collection: {collection_name}")
        results = collection.query(query_texts=[query_text], n_results=n_results)
        return results["documents"][0], results["metadatas"][0]

    def clear_collection(self, collection_name):
        """Clear all data in a specified collection."""
        collection = self.collections.get(collection_name)
        if not collection:
            raise ValueError(f"Unknown collection: {collection_name}")
        # Fetch all IDs in the collection
        all_records = collection.get()
        ids = all_records.get("ids", [])
        if ids:
            collection.delete(ids=ids)
