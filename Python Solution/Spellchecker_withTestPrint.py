import re
import string
import os
import sys

# Define punctuation to remove (excluding apostrophe)
punctuation_to_remove = string.punctuation.replace("'", "")

# Create a translation table
translator = str.maketrans("", "", punctuation_to_remove)

def get_input_files(arguments):
    """
    Retrieves the text file and dictionary file names either from command-line arguments 
    or by prompting the user for input.

    If command-line arguments are provided, the function expects the first argument to be 
    the text file and the second to be the dictionary file. If no arguments are given, 
    the user is prompted to enter the filenames manually.

    Args:
        arguments (list): A list of command-line arguments, where the first element is the script
                          the second is the text file, and the third is the dictionary file.

    Returns:
        tuple:
            text_file (str): The name of the text file.
            dict_file (str): The name of the dictionary file.

    Raises:
        SystemExit: If no command-line arguments are provided and user input is not received.
    """
    if len(arguments) > 1:
        # Uses the command line input to grab text file and dict file.
        text_file = arguments[1]
        dict_file = arguments[2]
    elif len(arguments) == 1:
        # Prompt user for text file and dictionary file
        text_file = input("Enter the name of the text file: ")
        dict_file = input("Enter the name of the dictionary file: ")
    else:
        print("Usage: python Spellchecker.py <text_file> <dictionary_file>")
        sys.exit(1)
    
    print(f"[DEBUG] get_input_files: text_file = '{text_file}', dict_file = '{dict_file}'")
    return text_file, dict_file


def find_file(file_name):
    """
    Searches for a file within the current working directory and its subdirectories.
    
    Args:
        file_name (str): The name of the file to search for.
    
    Returns:
        str or None: The full path of the file if found, otherwise None.
    """
    for root, dirs, files in os.walk(os.getcwd()):
        # Uncomment the next line to see each directory being scanned:
        # print(f"[DEBUG] Checking folder: {root}")
        if file_name in files:
            found_path = os.path.join(root, file_name)
            print(f"[DEBUG] find_file: Found '{file_name}' at '{found_path}'")
            return found_path
    print(f"[DEBUG] find_file: '{file_name}' not found in directory tree.")
    return None


# Function to check for missing characters (e.g., wndow → window)
def missing_character(word):
    """
    Generates possible correct words by inserting each letter of the alphabet at every position.
    
    Args:
        word (str): The misspelled word.
    
    Returns:
        set: A set of possible correct words.
    """
    variant = set()
    print(f"[DEBUG] missing_character: Checking word '{word}'")
    for i in range(len(word) + 1):  # Try inserting a letter at each position
        for char in "abcdefghijklmnopqrstuvwxyz":
            new_word = word[:i] + char + word[i:]  # Insert character
            variant.add(new_word)  # Store all variations
    print(f"[DEBUG] missing_character: Generated {len(variant)} variants for '{word}'")
    return variant


# Function to check for extra characters (e.g., helllo → hello)
def extra_character(word):
    """
    Generates possible correct words by removing each character from the word.
    
    Args:
        word (str): The misspelled word.
    
    Returns:
        set: A set of possible correct words.
    """
    variant = set()
    print(f"[DEBUG] extra_character: Checking word '{word}'")
    for i in range(len(word)):  # Try removing each character
        new_word = word[:i] + word[i + 1:]
        variant.add(new_word)
    print(f"[DEBUG] extra_character: Generated {len(variant)} variants for '{word}'")
    return variant


# Function to check for transposed characters (e.g., wierd → weird)
def transposed_characters(word):
    """
    Generates possible correct words by swapping adjacent characters.
    
    Args:
        word (str): The misspelled word.
    
    Returns:
        set: A set of possible correct words.
    """
    variant = set()
    print(f"[DEBUG] transposed_characters: Checking word '{word}'")
    word_list = list(word)
    for i in range(len(word) - 1):  # Swap adjacent characters
        word_list[i], word_list[i + 1] = word_list[i + 1], word_list[i]
        new_word = "".join(word_list)
        variant.add(new_word)
        print(f"[DEBUG] transposed_characters: Swapped indices {i} and {i+1} -> '{new_word}'")
        word_list[i], word_list[i + 1] = word_list[i + 1], word_list[i]  # Swap back
    print(f"[DEBUG] transposed_characters: Generated {len(variant)} variants for '{word}'")
    return variant


# New Function: Handle Pluralization Errors (e.g., "centurys" → "centuries")
def pluralization_errors(word):
    """
    Generates possible correct words by handling common pluralization mistakes.
    
    Args:
        word (str): The misspelled word.
    
    Returns:
        set: A set of possible correct words.
    """
    variant = set()
    print(f"[DEBUG] pluralization_errors: Checking word '{word}'")
    
    # Rule 1: Replace "ys" with "ies" (e.g., "centurys" → "centuries")
    if word.endswith("ys"):
        new_word = word[:-2] + "ies"
        variant.add(new_word)
        print(f"[DEBUG] pluralization_errors: Rule 1 applied -> '{word}' becomes '{new_word}'")
    
    # Rule 2: Replace "s" with "" if singular exists (e.g., "cats" → "cat")
    if word.endswith("s"):
        new_word = word[:-1]
        variant.add(new_word)
        print(f"[DEBUG] pluralization_errors: Rule 2 applied -> '{word}' becomes '{new_word}'")
    
    # Rule 3: Try adding "s" if singular exists (e.g., "box" → "boxes")
    singular_forms = [word[:-1], word[:-2] + "y", word + "s"]
    for form in singular_forms:
        variant.add(form)
        print(f"[DEBUG] pluralization_errors: Rule 3 added variant -> '{form}'")
    
    return variant


# Function to check for incorrect characters (e.g., pramise → promise)
def incorrect_character(word):
    """
    Generates possible correct words by replacing each character with all other letters of the alphabet.
    
    Args:
        word (str): The misspelled word.
    
    Returns:
        set: A set of possible correct words.
    """
    variant = set()
    print(f"[DEBUG] incorrect_character: Checking word '{word}'")
    for i in range(len(word)):  # Try replacing each character
        for char in "abcdefghijklmnopqrstuvwxyz":
            if word[i] != char:
                new_word = word[:i] + char + word[i + 1:]
                variant.add(new_word)
    print(f"[DEBUG] incorrect_character: Generated {len(variant)} variants for '{word}'")
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
    print(f"[DEBUG] recursive_correction: Depth {current_depth} for word '{word}' with previous_suggestions: {previous_suggestions}")
    if current_depth > depth:
        print(f"[DEBUG] recursive_correction: Maximum depth reached for '{word}', returning empty dict")
        return {}

    # Generate all possible variations (both correct and incorrect)
    variants = set()
    variants.update(missing_character(word))
    variants.update(extra_character(word))
    variants.update(transposed_characters(word))
    variants.update(incorrect_character(word))
    variants.update(pluralization_errors(word))
    print(f"[DEBUG] recursive_correction: Generated {len(variants)} total variants for '{word}' at depth {current_depth}")
    
    # Separate valid dictionary suggestions from incorrect variations
    suggestions = {v for v in variants if v in dictionary}
    incorrect_variations = variants - suggestions  # All generated words not in the dictionary
    print(f"[DEBUG] recursive_correction: Valid suggestions: {suggestions}")
    print(f"[DEBUG] recursive_correction: Incorrect variations: {incorrect_variations}")
    
    # Remove words that were already suggested in earlier depths
    unique_suggestions = suggestions - previous_suggestions
    print(f"[DEBUG] recursive_correction: Unique suggestions after removing previous: {unique_suggestions}")
    
    # Store only unique suggestions
    depth_results = {
        current_depth: {
            "suggestions": unique_suggestions,
            "variations": incorrect_variations
        }
    }
    
    # Update set of all seen suggestions
    updated_previous_suggestions = previous_suggestions | unique_suggestions

    # Ensure that all generated words go into the next layer
    all_deeper_results = {}
    for variant in variants:
        print(f"[DEBUG] recursive_correction: Recursing on variant '{variant}' at depth {current_depth + 1}")
        deeper_results = recursive_correction(variant, dictionary, depth, current_depth + 1, updated_previous_suggestions)
        if not isinstance(deeper_results, dict):
            continue
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

    print(f"[DEBUG] recursive_correction: Returning results for '{word}' at depth {current_depth}: {depth_results}")
    return depth_results

def run_test_files(dict_file):
    """
    Runs predefined test cases for the spellchecker using different input text files.
    
    Args:
        dict_file (str): The dictionary file to use for spellchecking.
    """
    test_files = [
        '"1 Word (depth 2).txt"',
        '"1 Word (depth 3).txt"',
        '"1 Word Correct.txt"',
        '"1 Word Incorrect.txt"',
        '"10 Words with Mistakes.txt"',
        '"10 Words Without Mistakes.txt"',
        '"100 Words with Mistakes.txt"',
        '"100 Words Without Mistakes (But 1 Proper Noun).txt"',
        '"100 Words Without Mistakes (Without Proper Noun).txt"',
        '"500 Word Without Mistakes (with proper nouns).txt"',
        '"500 Words with Mistakes (with proper nouns).txt"',
        '"1000 Word with Mistakes (but the same story).txt"',
        '"1000 Words Without Mistakes (without proper nouns).txt"',
        '"Blank.txt"',
        '"Punctuation and Hyphens Test.txt"',
        '"The Whispering Tome (w mistakes).txt"',
        '"The Whispering Tome.txt"',
        '"White Space Test.txt"'
    ]

    for test_file in test_files:
        print(f"[DEBUG] run_test_files: Running test on: {test_file}\n")
        os.system(f"python Spellchecker.py {test_file} {dict_file}")

if __name__ == "__main__":
    text_file, dict_file = get_input_files(sys.argv)
    if text_file == "test all":
        run_test_files(dict_file)
    else:
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
                    unknown_words = []
                    proper_noun_candidates = []
                    for word in tokens:
                        # Ignore words that are fully numeric (e.g., "1234")
                        if word.isdigit():
                            continue
                        if word.lower() not in dictionary:
                            if word[0].isupper():
                                proper_noun_candidates.append(word)
                                print(f"[DEBUG] Proper noun candidate: '{word}'")
                            else:
                                unknown_words.append(word)
                                print(f"[DEBUG] Unknown word: '{word}'")
                    
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
                            print(f"[DEBUG] Generating corrections for '{word_lower}'")
                            correction_suggestions = recursive_correction(word_lower, dictionary)
                            for depth_level in sorted(correction_suggestions.keys()):
                                suggestions = ", ".join(correction_suggestions[depth_level]["suggestions"])
                                if suggestions:
                                    of.write(f"  🔹 Depth {depth_level} suggestions for '{word}': {suggestions}\n")
                
                if not errors_found:
                    of.write("No spelling errors found.\n")
                print(f"[DEBUG] Processing complete. Output written to: {output_file}")
                print(f"✅ Errors (if any) and suggestions have been written to: {output_file}")
        except FileNotFoundError:
            print(f"Error: The text file '{text_file}' was not found.")