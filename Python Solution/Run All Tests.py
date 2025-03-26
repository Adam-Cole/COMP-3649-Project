import shutil
import os

def find_python_command():
    """Finds the correct Python command available on the system."""
    # This covers all three of our setups (python for Alex, py for Adam, python3 for Matt)
    for cmd in ["python", "python3", "py"]:
        if shutil.which(cmd):
            return cmd
    raise RuntimeError("No valid Python interpreter found! Ensure Python is installed and added to PATH.")

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
        '"Abbreviated Words.txt"',
        '"Capitalization Tests.txt"',
        '"Punctuation and Hyphens Test.txt"',
        '"The Whispering Tome (w mistakes).txt"',
        '"The Whispering Tome.txt"',
        '"White Space Test.txt"'
    ]
    
    python_command = find_python_command()

    for test_file in test_files:
        print(f"Running test on: {test_file}\n")
        os.system(f"{python_command} Spellchecker.py {test_file} {dict_file}")
        
if __name__ == "__main__":
    run_test_files("words_alpha.txt")