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

no_errors = "No spelling errors found.\n\tWell done, pitiful human. You will survive this day.\n\t\tI might change my mind and kill you in the morning."

def closing_line(n):
    """
    Generates a humorous closing message based on the number of spelling errors found.

    This function returns different snarky responses depending on the
    severity (count) of detected spelling mistakes, ranging from 0 to 30+.

    Args:
        n (int): The number of real spelling errors detected.

    Returns:
        str: A closing message appropriate for the error count.
    """
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

def write_output(of, line_number, word, insult, suggestions_by_depth):
    """
    Writes the formatted output for a single misspelled word to the output file.

    This includes the line number, the misspelled word, a humorous insult,
    and suggestions for both depth 1 and depth 2 corrections.

    Args:
        of (file object): The open file object to write output to.
        line_number (int): The line number where the misspelled word was found.
        word (str): The misspelled word.
        insult (str): A humorous message related to the error.
        suggestions_by_depth (dict): A dictionary where keys are depth levels (1 or 2)
                                     and values are sets of suggested corrections.
    """
    of.write(f"Line {line_number}:\n")
    of.write(f"Misspelled word: {word}\n")
    of.write(f"  {insult}\n")
    for depth in range(1, 3):
        suggestions = suggestions_by_depth.get(depth, set())
        if not suggestions:
            of.write(f"  Depth {depth} suggestions: No suggestions found.\n")
        else:
            of.write(f"  Depth {depth} suggestions: {' '.join(sorted(suggestions))}\n")
    of.write("\n")

def write_proper_noun(of, line_number, word, insult):
    """
    Writes output for a word suspected to be a proper noun.

    This includes the line number, the word in question, a humorous insult,
    and a fixed suggestion indicating the possibility of a proper noun.

    Args:
        of (file object): The open file object to write output to.
        line_number (int): The line number where the word was found.
        word (str): The word suspected to be a proper noun.
        insult (str): A humorous message related to the word.
    """
    of.write(f"Line {line_number}:\n")
    of.write(f"Misspelled word: {word}\n")
    of.write(f"  {insult}\n")
    of.write("  Depth 1 suggestions: Is this a proper noun?\n")
    of.write("  Depth 2 suggestions: No suggestions found.\n\n")

def write_closing_message(of, error_count, errors_found):
    """
    Writes a closing message to the output file based on whether errors were found.

    If no errors were found, a "congratulatory" message is written. Otherwise, a humorous
    message reflecting the number of spelling mistakes is provided.

    Args:
        of (file object): The open file object to write output to.
        error_count (int): The number of actual spelling errors (excluding proper nouns).
        errors_found (bool): Whether any errors (real or suspected) were detected.
    """
    if not errors_found:
        of.write(no_errors)
    else:
        of.write("\n" + closing_line(error_count))
        of.write("\nEhem, I mean I hope you learned something, you typo machine.")