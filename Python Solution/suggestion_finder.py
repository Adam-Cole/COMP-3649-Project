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
    variants = set()

    # Rule 1: "ys" → "ies" (e.g., "centurys" → "centuries")
    if word.endswith("ys"):
        variants.add(word[:-2] + "ies")

    # Rule 2: Remove "s" (e.g., "cats" → "cat")
    if word.endswith("s"):
        variants.add(word[:-1])

    # Rule 3: Add "s" (e.g., "box" → "boxes")
    variants.add(word + "s")

    # Rule 4: "-f" or "-fe" → "-ves" (e.g., "leaf" → "leaves")
    if word.endswith("f"):
        variants.add(word[:-1] + "ves")
    if word.endswith("fe"):
        variants.add(word[:-2] + "ves")

    # Rule 5: "-o" → "-oes" (e.g., "potato" → "potatoes")
    if word.endswith("o"):
        variants.add(word + "es")

    # Rule 6: Latin/Greek forms
    if word.endswith("us"):
        variants.add(word[:-2] + "i")
    if word.endswith("is"):
        variants.add(word[:-2] + "es")
    if word.endswith("on"):
        variants.add(word[:-2] + "a")

    # Rule 7: Irregular plurals
    irregulars = {
        "foot": "feet",
        "tooth": "teeth",
        "mouse": "mice",
        "man": "men",
        "woman": "women",
        "child": "children",
        "person": "people",
        "ox": "oxen",
        "radius": "radii",
        "alumnus": "alumni"
    }
    if word in irregulars:
        variants.add(irregulars[word])

    return variants


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

# Generate all possible variations (both correct and incorrect)
def generate_single_edit_variants(word: str) -> set:
    variants = set()
    variants.update(missing_character(word))
    variants.update(extra_character(word))
    variants.update(transposed_characters(word))
    variants.update(incorrect_character(word))
    variants.update(pluralization_errors(word))
    return variants
