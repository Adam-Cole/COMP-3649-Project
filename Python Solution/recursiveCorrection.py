from suggestion_finder import generate_single_edit_variants

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

    variants = generate_single_edit_variants(word)

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