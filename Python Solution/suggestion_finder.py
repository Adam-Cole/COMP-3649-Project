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
    for i in range(len(word) + 1):  # Try inserting a letter at each position
        for char in "abcdefghijklmnopqrstuvwxyz":
            new_word = word[:i] + char + word[i:]  # Insert character
            variant.add(new_word)  # Store all variations
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
    for i in range(len(word)):  # Try removing each character
        new_word = word[:i] + word[i + 1:]
        variant.add(new_word)  # Store all variations
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
    word_list = list(word)
    for i in range(len(word) - 1):  # Swap adjacent characters
        word_list[i], word_list[i + 1] = word_list[i + 1], word_list[i]
        new_word = "".join(word_list)
        variant.add(new_word)  # Store all variations
        word_list[i], word_list[i + 1] = word_list[i + 1], word_list[i]  # Swap back
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
    for i in range(len(word)):  # Try replacing each character
        for char in "abcdefghijklmnopqrstuvwxyz":
            if word[i] != char:
                new_word = word[:i] + char + word[i + 1:]
                variant.add(new_word)  # Store all variations
    return variant
