import re
import os

def read_file(path):
    with open(path, 'r', encoding='utf-8') as file:
        return file.read()

def tokenize(text):
    text = text.lower()
    text = re.sub(r"[^\w\s]", "", text)  # remove punctuation
    text = re.sub(r"\s+", " ", text)     # collapse all whitespace to a single space
    return text.strip().split()

def compare_documents(doc1: str, doc2: str, path1: str, path2: str):
    
    print(f"\n🔍 Comparing all words (unordered):")
    print(f"  📄 {os.path.basename(path1)}")
    print(f"  📄 {os.path.basename(path2)}\n")

    words1 = set(tokenize(doc1))
    words2 = set(tokenize(doc2))

    shared = words1 & words2
    total = words1 | words2
    similarity = len(shared) / len(total) if total else 1.0

    print(f"🔢 Shared words: {len(shared)}")
    print(f"📊 Similarity: {similarity:.2%}")

# Example usage for test
doc3 = "The quick brown fox jumps over the lazy dog."
doc4 = "A lazy dog was jumped over by a quick brown fox."
# === File paths ===
path1 = r"<filepath1>"
path2 = r"<filepath1>"
# === Read and compare ===
doc1 = read_file(path1)
doc2 = read_file(path2)

compare_documents(doc1, doc2, path1, path2)