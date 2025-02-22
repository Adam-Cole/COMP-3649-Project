import re
import string
import os
import sys

# Define punctuation to remove (excluding apostrophe)
punctuation_to_remove = string.punctuation.replace("'", "")

# Create a translation table
translator = str.maketrans("", "", punctuation_to_remove)

"""
Retrieves the text file and dictionary file names either from command-line arguments 
or by prompting the user for input.

If command-line arguments are provided, the function expects the first argument to be 
the text file and the second to be the dictionary file. If no arguments are given, 
the user is prompted to enter the filenames manually.

Args:
    arguments:  A list of command-line arguments, where the first element is the script,
                the second is the text file, and the third is the dictionary file.

Returns:
    tuple:
        text_file: The name of the text file.
        dict_file: The name of the dictionary file.

Raises:
    SystemExit: If no command-line arguments are provided and user input is not received.
"""
def get_input_files(arguments):
    if len(arguments) > 1:
        # Uses the command line input to grab text file and dict file.
        text_file = arguments[1]
        dict_file = arguments[2]
    elif len(arguments) == 1:
        # Prompt user for text file and dictionary file
        text_file = input("Enter the name of the text file: ")
        dict_file = input("Enter the name of the dictionary file: ")
    else:
        # This will likely never happen, but print out example use case if it does.
        print("Usage: python Spellchecker.py <text_file> <dictionary_file>")
        sys.exit(1)
    
    print(f"[DEBUG] get_input_files: text_file='{text_file}', dict_file='{dict_file}'")
    return text_file, dict_file

def find_file(file_name):
    for root, dirs, files in os.walk(os.getcwd()):
        if file_name in files:
            found_path = os.path.join(root, file_name)
            print(f"[DEBUG] find_file: Found '{file_name}' at '{found_path}'")
            return found_path
    print(f"[DEBUG] find_file: '{file_name}' not found in directory tree.")
    return None

# 🔹 Function to check for missing characters (e.g., wndow → window)
def missing_character(word, dictionary):
    suggestions = []
    print(f"[DEBUG] missing_character: checking word '{word}'")
    for i in range(len(word) + 1):  # Try inserting a letter at each position
        for char in "abcdefghijklmnopqrstuvwxyz":
            new_word = word[:i] + char + word[i:]
            if new_word in dictionary:
                suggestions.append(new_word)
                print(f"[DEBUG] missing_character: Insert '{char}' at index {i} -> '{new_word}' is in dictionary")
    print(f"[DEBUG] missing_character: suggestions for '{word}': {suggestions}")
    return suggestions

# 🔹 Function to check for extra characters (e.g., helllo → hello)
def extra_character(word, dictionary):
    suggestions = []
    print(f"[DEBUG] extra_character: checking word '{word}'")
    for i in range(len(word)):  # Try removing each character
        new_word = word[:i] + word[i + 1:]
        if new_word in dictionary:
            suggestions.append(new_word)
            print(f"[DEBUG] extra_character: Removed character at index {i} -> '{new_word}' is in dictionary")
    print(f"[DEBUG] extra_character: suggestions for '{word}': {suggestions}")
    return suggestions

# 🔹 Function to check for transposed characters (e.g., wierd → weird)
def transposed_characters(word, dictionary):
    suggestions = []
    word_list = list(word)
    print(f"[DEBUG] transposed_characters: checking word '{word}'")
    for i in range(len(word) - 1):  # Swap adjacent characters
        word_list[i], word_list[i + 1] = word_list[i + 1], word_list[i]
        new_word = "".join(word_list)
        if new_word in dictionary:
            suggestions.append(new_word)
            print(f"[DEBUG] transposed_characters: Swapped indices {i} and {i+1} -> '{new_word}' is in dictionary")
        word_list[i], word_list[i + 1] = word_list[i + 1], word_list[i]  # Swap back
    print(f"[DEBUG] transposed_characters: suggestions for '{word}': {suggestions}")
    return suggestions

# 🔹 Function to check for incorrect characters (e.g., pramise → promise)
def incorrect_character(word, dictionary):
    suggestions = []
    print(f"[DEBUG] incorrect_character: checking word '{word}'")
    for i in range(len(word)):  # Try replacing each character
        for char in "abcdefghijklmnopqrstuvwxyz":
            if word[i] != char:
                new_word = word[:i] + char + word[i + 1:]
                if new_word in dictionary:
                    suggestions.append(new_word)
                    print(f"[DEBUG] incorrect_character: Replaced character at index {i} with '{char}' -> '{new_word}' is in dictionary")
    print(f"[DEBUG] incorrect_character: suggestions for '{word}': {suggestions}")
    return suggestions

# 🔹 New Function: Handle Pluralization Errors (e.g., "centurys" → "centuries")
def pluralization_errors(word, dictionary):
    suggestions = []
    print(f"[DEBUG] pluralization_errors: checking word '{word}'")
    
    # Rule 1: Replace "ys" with "ies" (e.g., "centurys" → "centuries")
    if word.endswith("ys"):
        new_word = word[:-2] + "ies"
        if new_word in dictionary:
            suggestions.append(new_word)
            print(f"[DEBUG] pluralization_errors: Rule 1 applied, '{word}' -> '{new_word}'")
    
    # Rule 2: Replace "s" with "" if singular exists (e.g., "cats" → "cat")
    if word.endswith("s") and word[:-1] in dictionary:
        suggestions.append(word[:-1])
        print(f"[DEBUG] pluralization_errors: Rule 2 applied, '{word}' -> '{word[:-1]}'")
    
    # Rule 3: Try adding "s" if singular exists (e.g., "box" → "boxes")
    singular_forms = [word[:-1], word[:-2] + "y", word + "s"]
    for form in singular_forms:
        if form in dictionary:
            suggestions.append(form)
            print(f"[DEBUG] pluralization_errors: Rule 3 applied, found singular form: '{form}'")
    
    print(f"[DEBUG] pluralization_errors: suggestions for '{word}': {suggestions}")
    return suggestions

if __name__ == "__main__":
    text_file, dict_file = get_input_files(sys.argv)

    # Generate output file name based on input file
    base_name, ext = os.path.splitext(text_file)  # Extract name & extension
    output_dir = os.path.join(os.getcwd(), "Output Files")
    output_file = os.path.join(output_dir, f"{base_name}[errors and suggestions]{ext}")
    
    # Load dictionary into a set for fast lookup
    try:
        dict_dir = find_file(dict_file)

        if dict_dir is None:
            raise FileNotFoundError

        with open(dict_dir, "r", encoding="utf-8") as df:
            dictionary = set(word.strip() for word in df)
        print(f"[DEBUG] Loaded dictionary with {len(dictionary)} words")
    except FileNotFoundError:
        print(f"Error: The dictionary file '{dict_file}' was not found.")
        exit()

    # Process the text file line by line
    try:
        text_dir = find_file(text_file)

        if text_dir is None:
            raise FileNotFoundError
        
        errors_found = False

        with open(text_dir, "r", encoding="utf-8") as tf, open(output_file, "w", encoding="utf-8") as of:
            for line_number, line in enumerate(tf, start=1):
                original_line = line.strip()
                print(f"[DEBUG] Processing line {line_number}: '{original_line}'")

                # Remove unwanted punctuation
                clean_line = original_line.replace("-", " ").replace("'", " ").translate(translator)
                print(f"[DEBUG] Cleaned line: '{clean_line}'")

                # Tokenize words using split() (simple approach)
                tokens = clean_line.split()
                print(f"[DEBUG] Tokenized words: {tokens}")

                # Check each word against the dictionary
                unknown_words = [word for word in tokens if word.lower() not in dictionary]
                print(f"[DEBUG] Unknown words: {unknown_words}")

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
