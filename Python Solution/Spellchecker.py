import string
import os
import sys
from input import get_input_files, find_file
from output import write_output, write_proper_noun, write_closing_message, insults
from suggestion_finder import generate_single_edit_variants
from recursiveCorrection import recursive_correction

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
            insult_index = 0
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

                    if proper_noun_candidates:
                        for proper_noun in proper_noun_candidates:
                            insult = insults[insult_index % len(insults)]
                            write_proper_noun(of, line_number, proper_noun, insult)
                            insult_index += 1

                    # Generate suggestions for each misspelled word

                    for word in unknown_words:
                        word_lower = word.lower()
                        insult = insults[insult_index % len(insults)]
                        correction_suggestions = recursive_correction(word_lower, dictionary)
                        write_output(of, line_number, word, insult,
                            {d: correction_suggestions[d]['suggestions'] for d in correction_suggestions}
                        )
                        insult_index += 1
                        all_errors.append(word)                

            # If no errors were found, write a message in the output file
            write_closing_message(of, len(all_errors), errors_found)
            print(f"✅ Errors (if any) and suggestions have been written to: {output_file}")

    except FileNotFoundError:
        print(f"Error: The text file '{text_file}' was not found.")