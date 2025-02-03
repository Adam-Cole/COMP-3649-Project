import string

# Define punctuation to remove (excluding hyphen and apostrophe)
punctuation_to_remove = string.punctuation.replace("'", "")

# Create a translation table
translator = str.maketrans("", "", punctuation_to_remove)

# Prompt user for file name
file_name = input("Enter the name of the text file: ")

try:
    # Read the file
    with open(file_name, "r", encoding="utf-8") as file:
        text = file.read()

    # Remove unwanted punctuation
    text = text.translate(translator)

    # Tokenize words
    tokens = text.split()

    print(tokens)

except FileNotFoundError:
    print(f"Error: The file '{file_name}' was not found. Please check the file name and try again.")