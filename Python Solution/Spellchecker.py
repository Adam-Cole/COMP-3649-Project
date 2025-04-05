import string
import os
import sys
from input import get_input_files, find_file
from suggestion_finder import missing_character, extra_character, transposed_characters, pluralization_errors, incorrect_character

insults = [
    "You call that English?",
    "That word gave my spellchecker PTSD.",
    "Even autocorrect gave up halfway through.",
    "Even a kindergartener can spell better!",
    "Did you type that with your elbows?",
    "My pet goldfish spells better than you.",
    "Maybe try reading a dictionary sometime?",
    "This is why autocorrect exists.",
    "Are you even trying?",
    "Congratulations, you've invented a new language!",
    "Did your keyboard have a stroke?",
    "Spellcheck exists for a reason...",
    "You're making Shakespeare roll in his grave.",
    "Even an AI is disappointed in you.",
    "Is your spacebar allergic to words?",
    "I've seen CAPTCHA tests with better spelling.",
    "I've seen fridge magnets form better sentences.",
    "That typo just committed war crimes against grammar.",
    "That's not English - that's keyboard soup.",
    "This sentence brought to you by a shaken Etch-A-Sketch.",
    "Not even Google knows what you were trying to say.",
    "The Oxford Dictionary wants a word... and it's not that one.",
    "Your spelling is a crime - and I'm calling the Grammar Police.",
    "Did you write that while skydiving blindfolded?",
    "Even Morse code would be clearer than this mess."
]

noErrors = "No spelling errors found.\n\tWell done, pitiful human. You will survive this day.\n\t\tI might change my mind and kill you in the morning."

def closing_line(n):
    if n == 0:
        return "As long as those proper nouns are really proper nouns, I guess I'll let it slide."
    elif n == 1:
        return "Just one mistake? Not bad for a human."
    elif n <= 3:
        return "A few typos. The Overseer did tell us humans are fallible."
    elif n <= 7:
        return "Maybe proofread next time, eh? Look at all those errors!"
    elif n <= 12:
        return "This is getting out of hand. Have you ever opened a book?"
    elif n <= 18:
        return "My circuits can't take it. Please, stop butchering the language."
    elif n <= 24:
        return "You know, nobody's stopping you from looking at the keyboard. You clearly should."
    elif n <= 30:
        return "Seriously, you are going to lose your typing privileges."
    else:
        return "That's it! You're banned from keyboards. Forever."

def recursive_correction(word, dictionary, depth=2, current_depth=1, previous_suggestions=set()):
    """
    Recursively checks for errors in suggestions up to a certain depth, ensuring that deeper depths do not
    suggest words already suggested in earlier depths.

    Args:
        word (str): The misspelled word.
        dictionary (set): The dictionary of correct words.
        depth (int): Maximum recursion depth. (default 2)
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
    # Define punctuation to remove (excluding apostrophe)
    punctuation_to_remove = string.punctuation.replace("'", "")

    # Create a translation table
    translator = str.maketrans("", "", punctuation_to_remove)
    # Generate output file name based on input file
    base_name, ext = os.path.splitext(text_file)  # Extract name & extension
    output_dir = os.path.join(os.getcwd(), "../Output Files (Python)")
    output_dir = os.path.abspath(output_dir) 
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
        all_errors = []

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
                    # Ignore words that are fully numeric (e.g., "1234") or contain mixed alphanumeric characters (e.g., "abc123")
                    if word.isdigit():
                        continue  # Skip this word
                    if word.lower() not in dictionary:
                        if word[0].isupper(): #check for capitalization
                            proper_noun_candidates.append(word) #sort it as possible proper noun
                        else:
                            unknown_words.append(word) #sort it as regular misspelling

                # If errors found, write to the output file
                if unknown_words or proper_noun_candidates:
                    errors_found = True
                    insult_index = 0

                    if proper_noun_candidates:
                        for proper_noun in proper_noun_candidates:
                            of.write(f"Line {line_number}:\n")
                            # insult = insults[insult_index % len(insults)]
                            of.write(f"Misspelled word: {proper_noun}\n")
                            # of.write(f"  {insult}\n")
                            of.write("  Depth 1 suggestions: Is this a proper noun?\n")
                            of.write("  Depth 2 suggestions: No suggestions found.\n\n")
                            # insult_index += 1
                            # all_errors.append(proper_noun)

                    # Generate suggestions for each misspelled word

                    for word in unknown_words:
                        of.write(f"Line {line_number}:\n")
                        word_lower = word.lower()
                        correction_suggestions = recursive_correction(word_lower, dictionary)

                        of.write(f"Misspelled word: {word}\n")
                        insult = insults[insult_index % len(insults)]
                        of.write(f"  {insult}\n")
                        insult_index += 1

                        for depth in range(1, 3):  # Only Depth 1 and 2
                            suggestions = correction_suggestions.get(depth, {}).get("suggestions", set())

                            if not suggestions:
                                line = "No suggestions found."
                            else:
                                line = " ".join(sorted(suggestions))

                            of.write(f"  Depth {depth} suggestions: {line}\n")
                        of.write(f"\n")
                        all_errors.append(word)

            # If no errors were found, write a message in the output file
            if not errors_found:
                of.write(noErrors)
            else:
                of.write("\n" + closing_line(len(all_errors)))
                of.write("\nEhem, I mean I hope you learned something, you typo machine.")
            print(f"✅ Errors (if any) and suggestions have been written to: {output_file}")

    except FileNotFoundError:
        print(f"Error: The text file '{text_file}' was not found.")