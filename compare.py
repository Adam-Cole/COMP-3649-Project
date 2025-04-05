from tqdm import tqdm
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
    # print(f"\n🔍 Comparing word-by-word:")
    # print(f"  📄 {os.path.basename(path1)}")
    # print(f"  📄 {os.path.basename(path2)}\n")

    # words1 = tokenize(doc1)
    # words2 = tokenize(doc2)

    # max_len = max(len(words1), len(words2))
    # match_count = 0

    # print("Comparing words by position...")
    # for i in tqdm(range(max_len), desc="Progress"):
    #     w1 = words1[i] if i < len(words1) else "(missing)"
    #     w2 = words2[i] if i < len(words2) else "(missing)"
    #     if w1 == w2:
    #         match_count += 1

    # similarity = match_count / max_len if max_len else 1.0

    # print(f"\n🔢 Total words compared: {max_len}")
    # print(f"✅ Matching words: {match_count}")
    # print(f"📊 Similarity: {similarity:.2%}")
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

# Example usage
doc3 = "The quick brown fox jumps over the lazy dog."
doc4 = "A lazy dog was jumped over by a quick brown fox."
# === File paths ===
path1 = r"C:\Users\Adam\Documents\COMP-3649-Project\Output Files\1000 Word with Mistakes (but the same story)[errors and suggestions].txt"
path2 = r"C:\Users\Adam\Documents\COMP-3649-Project\Haskell Solution\1000 Word with Mistakes (but the same story) [errors and suggestions].txt"
# === Read and compare ===
doc1 = read_file(path1)
doc2 = read_file(path2)

compare_documents(doc1, doc2, path1, path2)