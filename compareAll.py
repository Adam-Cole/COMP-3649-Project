import os
import re
from collections import Counter

def read_file(path):
    with open(path, 'r', encoding='utf-8') as file:
        return file.read()

def tokenize(text):
    return re.findall(r'\b\w+\b', text.lower())

def compare_documents(doc1: str, doc2: str, path1: str, path2: str):
    print(f"\n🔍 Comparing files (ignoring word order):")
    print(f"  📄 {os.path.basename(path1)}")
    print(f"  📄 {os.path.basename(path2)}\n")

    words1 = tokenize(doc1)
    words2 = tokenize(doc2)

    counter1 = Counter(words1)
    counter2 = Counter(words2)

    # Get shared words with minimum count overlap
    shared = sum((counter1 & counter2).values())
    total = max(sum(counter1.values()), sum(counter2.values()))

    similarity = shared / total if total else 1.0

    print(f"🔢 Total words (larger file): {total}")
    print(f"✅ Shared word occurrences: {shared}")
    print(f"📊 Similarity (ignoring order): {similarity:.2%}")

def normalize_filename(filename):
    name, _ = os.path.splitext(filename)
    name = re.sub(r"\s+\[", "[", name)
    name = re.sub(r"\s+Haskell$", "", name)
    name = re.sub(r"\s+Python$", "", name)
    return name.lower()

def get_file_pairs(haskell_dir, python_dir):
    haskell_map = {
        normalize_filename(f): os.path.join(haskell_dir, f)
        for f in os.listdir(haskell_dir) if f.endswith(".txt")
    }

    python_map = {
        normalize_filename(f): os.path.join(python_dir, f)
        for f in os.listdir(python_dir) if f.endswith(".txt")
    }

    common_keys = sorted(set(haskell_map) & set(python_map))

    haskell_outputs = [haskell_map[k] for k in common_keys]
    python_outputs = [python_map[k] for k in common_keys]

    unmatched = set(haskell_map) ^ set(python_map)
    if unmatched:
        print("\n⚠️ Unmatched files (after normalization):")
        for k in sorted(unmatched):
            if k in haskell_map:
                print(f"  🟥 Haskell only: {os.path.basename(haskell_map[k])}")
            if k in python_map:
                print(f"  🟦 Python only:  {os.path.basename(python_map[k])}")

    return haskell_outputs, python_outputs

if __name__ == "__main__":
    haskell_dir = "Output Files (Haskell)"
    python_dir = "Output Files (Python)"

    haskell_outputs, python_outputs = get_file_pairs(haskell_dir, python_dir)

    for h_file, p_file in zip(haskell_outputs, python_outputs):
        print("=" * 60)
        print(f"📂 Comparing: {os.path.basename(h_file)}")
        doc1 = read_file(h_file)
        doc2 = read_file(p_file)
        compare_documents(doc1, doc2, h_file, p_file)