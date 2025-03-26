import sys
import os

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
    if len(arguments) == 3:
        # Uses the command line input to grab text file and dict file.
        text_file = arguments[1]
        dict_file = arguments[2]
    else:
        # If no arguments are input, prompt user for dictionary file.
        text_file = input("Enter the name of the text file: ")
        dict_file = input("Enter the name of the dictionary file: ")
    
    return text_file, dict_file

def find_file(file_name):
    """
    Searches for a file within specific directories: Test Cases and Test Cases/Dictionary Files.
    
    Args:
        file_name (str): The name of the file to search for.
    
    Returns:
        str or None: The full path of the file if found, otherwise None.
    """
    # Go up one directory from the current working directory
    parent_dir = os.path.abspath(os.path.join(os.getcwd(), '..'))
    
    # Define the directories to search
    directories_to_search = [
        os.path.join(parent_dir, 'Test Cases'),
        os.path.join(parent_dir, 'Test Cases/Dictionary Files')
    ]
    
    # Search through the directories
    for directory in directories_to_search:
        for root, dirs, files in os.walk(directory):
            if file_name in files:
                return os.path.join(root, file_name)