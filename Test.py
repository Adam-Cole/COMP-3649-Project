import re
import string
import os

# Define punctuation to remove (excluding apostrophe)
punctuation_to_remove = string.punctuation.replace("'", "")

# Create a translation table
translator = str.maketrans("", "", punctuation_to_remove)

# Prompt user for text file and dictionary file
text_file = input("Enter the name of the text file: ")
dict_file = input("Enter the name of the dictionary file: ")

# Generate output file name based on input file
base_name, ext = os.path.splitext(text_file)  # Extract name & extension
output_file = f"{base_name}[errors and suggestions]{ext}"

# Load dictionary into a set for fast lookup
try:
    with open(dict_file, "r", encoding="utf-8") as df:
        dictionary = set(word.strip() for word in df) 

except FileNotFoundError:
    print(f"Error: The dictionary file '{dict_file}' was not found.")
    exit()

# 🔹 Function to check for missing characters (e.g., wndow → window)
def missing_character(word, dictionary):
    suggestions = []
    for i in range(len(word) + 1):  # Try inserting a letter at each position
        for char in "abcdefghijklmnopqrstuvwxyz":
            new_word = word[:i] + char + word[i:]
            if new_word in dictionary:
                suggestions.append(new_word)
    return suggestions


# 🔹 Function to check for extra characters (e.g., helllo → hello)
def extra_character(word, dictionary):
    suggestions = []
    for i in range(len(word)):  # Try removing each character
        new_word = word[:i] + word[i + 1 :]
        if new_word in dictionary:
            suggestions.append(new_word)
    return suggestions


# 🔹 Function to check for transposed characters (e.g., wierd → weird)
def transposed_characters(word, dictionary):
    suggestions = []
    word_list = list(word)
    for i in range(len(word) - 1):  # Swap adjacent characters
        word_list[i], word_list[i + 1] = word_list[i + 1], word_list[i]
        new_word = "".join(word_list)
        if new_word in dictionary:
            suggestions.append(new_word)
        word_list[i], word_list[i + 1] = word_list[i + 1], word_list[i]  # Swap back
    return suggestions


# 🔹 Function to check for incorrect characters (e.g., pramise → promise)
def incorrect_character(word, dictionary):
    suggestions = []
    for i in range(len(word)):  # Try replacing each character
        for char in "abcdefghijklmnopqrstuvwxyz":
            if word[i] != char:
                new_word = word[:i] + char + word[i + 1 :]
                if new_word in dictionary:
                    suggestions.append(new_word)
    return suggestions

# 🔹 New Function: Handle Pluralization Errors (e.g., "centurys" → "centuries")
def pluralization_errors(word, dictionary):
    suggestions = []
    
    # Rule 1: Replace "ys" with "ies" (e.g., "centurys" → "centuries")
    if word.endswith("ys"):
        new_word = word[:-2] + "ies"
        if new_word in dictionary:
            suggestions.append(new_word)
    
    # Rule 2: Replace "s" with "" if singular exists (e.g., "cats" → "cat")
    if word.endswith("s") and word[:-1] in dictionary:
        suggestions.append(word[:-1])

    # Rule 3: Try adding "s" if singular exists (e.g., "box" → "boxes")
    singular_forms = [word[:-1], word[:-2] + "y", word + "s"]
    for form in singular_forms:
        if form in dictionary:
            suggestions.append(form)
    
    return suggestions

# Process the text file line by line
try:
    with open(text_file, "r", encoding="utf-8") as tf, open(output_file, "w", encoding="utf-8") as of:
        for line_number, line in enumerate(tf, start=1):
            original_line = line.strip()

            # Remove unwanted punctuation
            clean_line = original_line.translate(translator)

            # Tokenize words using split() (simple approach)
            tokens = clean_line.split()

            # Check each word against the dictionary
            unknown_words = [word for word in tokens if word.lower() not in dictionary]

            # If errors found, write to the output file
            if unknown_words:
                errors_found = True
                of.write(f"Line {line_number}: {', '.join(unknown_words)}\n")

                # Generate suggestions for each misspelled word
                for word in unknown_words:
                    word_lower = word.lower()
                    suggestions = set()

                    suggestions.update(missing_character(word_lower, dictionary))
                    suggestions.update(extra_character(word_lower, dictionary))
                    suggestions.update(transposed_characters(word_lower, dictionary))
                    suggestions.update(incorrect_character(word_lower, dictionary))
                    suggestions.update(pluralization_errors(word_lower, dictionary))

                    if suggestions:
                        of.write(f"  🔹 Suggestions for '{word}': {', '.join(suggestions)}\n")

        # If no errors were found, write a message in the output file
        if not errors_found:
            of.write("No spelling errors found.\n")
        print(f"✅ Errors (if any) and suggestions have been written to: {output_file}")

except FileNotFoundError:
    print(f"Error: The text file '{text_file}' was not found.")