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
    arguments:  A list of command-line arguments, where the first element is the script
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
    
    return text_file, dict_file


def find_file(file_name):
    for root, dirs, files in os.walk(os.getcwd()):
        # print(f"Checking folder: {root}")             used for testing.
        if file_name in files:
            return os.path.join(root, file_name)
    return None


# 🔹 Function to check for missing characters (e.g., wndow → window)
def missing_character(word):
    variant = set()
    for i in range(len(word) + 1):  # Try inserting a letter at each position
        for char in "abcdefghijklmnopqrstuvwxyz":
            new_word = word[:i] + char + word[i:]  # Insert character
            variant.add(new_word)  # Store all variations
    return variant


# 🔹 Function to check for extra characters (e.g., helllo → hello)
def extra_character(word):
    variant = set()
    for i in range(len(word)):  # Try removing each character
        new_word = word[:i] + word[i + 1:]
        variant.add(new_word)  # Store all variations
    return variant


# 🔹 Function to check for transposed characters (e.g., wierd → weird)
def transposed_characters(word):
    variant = set()
    word_list = list(word)
    for i in range(len(word) - 1):  # Swap adjacent characters
        word_list[i], word_list[i + 1] = word_list[i + 1], word_list[i]
        new_word = "".join(word_list)
        variant.add(new_word)  # Store all variations
        word_list[i], word_list[i + 1] = word_list[i + 1], word_list[i]  # Swap back
    return variant


# 🔹 New Function: Handle Pluralization Errors (e.g., "centurys" → "centuries")
def pluralization_errors(word):
    variant = set()
    
    # Rule 1: Replace "ys" with "ies" (e.g., "centurys" → "centuries")
    if word.endswith("ys"):
        variant.add(word[:-2] + "ies")
    
    # Rule 2: Replace "s" with "" if singular exists (e.g., "cats" → "cat")
    if word.endswith("s"):
        variant.add(word[:-1])

    # Rule 3: Try adding "s" if singular exists (e.g., "box" → "boxes")
    singular_forms = [word[:-1], word[:-2] + "y", word + "s"]
    for form in singular_forms:
        variant.add(form)
    
    return variant


# 🔹 Function to check for incorrect characters (e.g., pramise → promise)
def incorrect_character(word):
    variant = set()
    for i in range(len(word)):  # Try replacing each character
        for char in "abcdefghijklmnopqrstuvwxyz":
            if word[i] != char:
                new_word = word[:i] + char + word[i + 1:]
                variant.add(new_word)  # Store all variations
    return variant


def recursive_correction(word, dictionary, depth=2, current_depth=1, previous_suggestions=set()):
    """
    Recursively checks for errors in suggestions up to a certain depth, ensuring that deeper depths do not
    suggest words already suggested in earlier depths.

    Args:
        word (str): The misspelled word.
        dictionary (set): The dictionary of correct words.
        depth (int): Maximum recursion depth.
        current_depth (int): Tracks the current depth level.
        previous_suggestions (set): Tracks words suggested in earlier depths to prevent duplicates.

    Returns:
        dict: A dictionary where keys are depth levels and values contain sets of suggestions.
    """
    if current_depth > depth:
        return {}

    # Generate all possible variations (both correct and incorrect)
    variants = set()
    variants.update(missing_character(word))
    variants.update(extra_character(word))
    variants.update(transposed_characters(word))
    variants.update(incorrect_character(word))
    variants.update(pluralization_errors(word))

    # Separate valid dictionary suggestions from incorrect variations
    suggestions = {v for v in variants if v in dictionary}
    incorrect_variations = variants - suggestions  # All generated words not in the dictionary

    # Remove words that were already suggested in earlier depths
    unique_suggestions = suggestions - previous_suggestions

    # Store only unique suggestions
    depth_results = {
        current_depth: {
            "suggestions": unique_suggestions,
            "variations": incorrect_variations
        }
    }

    # Update set of all seen suggestions
    updated_previous_suggestions = previous_suggestions | unique_suggestions

    # Ensure that all generated words (correct or incorrect) go into the next layer
    all_deeper_results = {}

    # Recursively check each incorrect variant and store deeper levels
    for variant in variants:
        deeper_results = recursive_correction(variant, dictionary, depth, current_depth + 1, updated_previous_suggestions)

        # Ensure deeper_results is a dictionary before accessing keys
        if not isinstance(deeper_results, dict):
            continue  # Skip any invalid results

        # Merge deeper results into main results
        for d, words in deeper_results.items():
            if d not in all_deeper_results:
                all_deeper_results[d] = {"suggestions": set(), "variations": set()}

            all_deeper_results[d]["suggestions"].update(words.get("suggestions", set()))
            all_deeper_results[d]["variations"].update(words.get("variations", set()))

    # Merge collected deeper results into depth_results
    for d, words in all_deeper_results.items():
        if d in depth_results:
            depth_results[d]["suggestions"].update(words["suggestions"])
            depth_results[d]["variations"].update(words["variations"])
        else:
            depth_results[d] = words

    return depth_results


if __name__ == "__main__":
    text_file, dict_file = get_input_files(sys.argv)

    # Generate output file name based on input file
    base_name, ext = os.path.splitext(text_file)  # Extract name & extension
    output_dir = os.path.join(os.getcwd(), "Output Files")
    output_file = os.path.join(output_dir,f"{base_name}[errors and suggestions]{ext}")
    
    # Load dictionary into a set for fast lookup
    try:
        dict_dir = find_file(dict_file)

        if dict_dir is None:
            raise FileNotFoundError

        with open(dict_dir, "r", encoding="utf-8") as df:
            dictionary = set(word.strip() for word in df) 
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

                # Remove unwanted punctuation
                clean_line = original_line.replace("-", " ").replace("'", " ").translate(translator)

                # Tokenize words using split() (simple approach)
                tokens = clean_line.split()

                # Check each word against the dictionary
                unknown_words = []
                proper_noun_candidates = []

                for word in tokens:
                    if word.lower() not in dictionary:
                        if word[0].isupper(): #check for capitalization
                            proper_noun_candidates.append(word) #sort it as possible proper noun
                        else:
                            unknown_words.append(word) #sort it as regular misspelling

                # If errors found, write to the output file
                if unknown_words or proper_noun_candidates:
                    errors_found = True
                    of.write(f"Line {line_number}:\n")
                             
                    if unknown_words:
                        of.write(f"Misspelled words: {', '.join(unknown_words)}\n")

                    if proper_noun_candidates:
                        for proper_noun in proper_noun_candidates:
                            of.write(f"'{proper_noun}' not found in dictionary. Is it a proper noun?\n")

                    # Generate suggestions for each misspelled word
                    for word in unknown_words:
                        word_lower = word.lower()
                        correction_suggestions = recursive_correction(word_lower, dictionary)

                        for depth_level in sorted(correction_suggestions.keys()):
                            suggestions = ", ".join(correction_suggestions[depth_level]["suggestions"])  # Extract suggestions correctly
                            if suggestions:  # Only print valid words
                                of.write(f"  🔹 Depth {depth_level} suggestions for '{word}': {suggestions}\n")

            # If no errors were found, write a message in the output file
            if not errors_found:
                of.write("No spelling errors found.\n")
            print(f"✅ Errors (if any) and suggestions have been written to: {output_file}")

    except FileNotFoundError:
        print(f"Error: The text file '{text_file}' was not found.")