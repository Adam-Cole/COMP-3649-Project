# COMP-3649-Project

## Project Purpose
The overall purpose for our project was to create a tool that can take in any .txt file as input (formated in any way), and process it in such a way that compares each word in the input file, one at a time, to a defined dictionary file (.txt). Before the comparison occurs, the words in the input file are tokenized (so they can be worked with individually) and then cleaned up (stripping all punctuation and lowering all letters of the word for a more accurate comparison to the dictionary words). In simple terms, the comparison would either come back as true in which case the current word being analyzed in the input file is correctly spelled, or false in which case the current word being analyzed in the input file is misspelled. If the word is correctly spelled it will be ignored and the program will provide no spelling suggestions. If the word is misspelled, the word will then get passed through our set of functions in order to generate a comprehensive list of depth 1 (1 character manipulation of the misspelled word (ex. 1 character insertion, 1 character deletion, etc.) that results in a valid word in the dictionary) and depth 2 spelling suggestions (2 character manipulations of the misspelled word (ex. 1 character insertion and 1 character transposition, etc.) that results in a valid word in the dictionary) that will be outputted for the user to visualize which words in their input file they misspelled, and provide them a list of words in which they may have meant to write instead. At a very high level, our tool’s purpose is to act as a spell checker.

---
## Python (Interpretive Solution)
**Input:**
- An input text file (.txt) in which you want to review for spelling errors.
- A dictionary file (.txt) in which you would like to act as your basis of correctly spelt words      in any desired language.

**Output:**
- An output text file (.txt) containing all misspelled words found in the input file as well as a comprehensive list of depth 1 and depth 2 spelling suggestions (words in which the user may have meant to type instead of the misspelled word)


### Compiling and Running
Python does not require any compiling, and there are no third party libraries that require installation prior to use. To run the program, you can use either command-line functions or the program will prompt the user with a “dropdown” interface. If using the command-line functions, the user must first enter the sub directory in the project folder before entering the commands.

#### When using the dropdown it will be as follows:
1. Run via python Spellchecker.py
2. Select input file,
3. Select dictionary file

#### Example Command Line Sequences:
`python Spellchecker.py "1 Word (depth 2).txt" "words_alpha.txt"`\
`python Spellchecker.py "1 Word (depth 3).txt" "words_alpha.txt"`\
`python Spellchecker.py "100 Words with Mistakes.txt" "words_alpha.txt"`\
`python Spellchecker.py "500 Words with Mistakes (with proper nouns).txt" "words_alpha.txt"`\
`python Spellchecker.py "1000 Word with Mistakes (but the same story).txt" "words_alpha.txt`"

>Note: python interpreter name may need to be changed (e.g. Matt’s laptop uses python3, Adam uses py)
---
## Haskell (Functional Solution)
**Input:**
- An input text file (.txt) in which you want to review for spelling errors.
- A dictionary file (.txt) in which you would like to act as your basis of correctly spelt words      in any desired language.

**Output:**
- An output text file (.txt) containing all misspelled words found in the input file as well as a comprehensive list of depth 1 and depth 2 spelling suggestions (words in which the user may have meant to type instead of the misspelled word)

### Compiling and Running
We have two iterations of Haskell, one which does not require any third party installations, and one that does require third party installations. The solution found in the `Haskell Solution` folder will not require third party installs. The solution found in the `Haskell Solution (Hashing)` folder required the below installations. 

>Note: This put us in a position that made it more awkward to run our pre-existing solution. Likely because we are Haskell novices. Upon running the below cabal installs, we then had to manually add packages to our pre-existing solution upon compiling, or we would run into package conflicts, which we were unable to figure out how to remove / swap between. `ghc -o main main.hs -hide-all-packages -package base -package containers -package directory -package filepath`

`cabal install --lib hashable unordered-containers`\
`cabal install --lib time`\
`cabal install --lib filepath directory`

As before, you can run using either method, command-line calls or using the "dropdown" selection method. Example calls below.

#### When using the dropdown it will be as follows:
1. Run via python Spellchecker.py
2. Select input file,
3. Select dictionary file

#### Example Command Line Sequences:
`main.exe "1 Word (depth 2).txt" "words_alpha.txt"`\
`main.exe "1 Word (depth 3).txt" "words_alpha.txt"`\
`main.exe "100 Words with Mistakes.txt" "words_alpha.txt"`\
`main.exe "500 Words with Mistakes (with proper nouns).txt" "words_alpha.txt"`\
`main.exe "1000 Word with Mistakes (but the same story).txt" "words_alpha.txt"`

>Note: main.exe call may need to be changed (e.g. Matt’s laptop uses ./main)
---
## Running Test Programs

### Comparison Testing

#### compareAll.py
**Output:**
- Printed statistics about the differences found between the two compared files, or, if no similar filenames are found, it will not compare it and output a message saying it has an unmatched file.

#### compare.py
**Input:**
- The two file paths that should be checked (this was just hard coded in, as this was never intended to be used long term)

**Output:**
- Printed statistics about the differences found between the two compared files, or, if no similar filenames are found, it will not compare it and output a message saying it has an unmatched file.

### Python Testing

#### runAllTests.py
**Input [Prompt]:**
- Asks for a user's python interpreter (py, python3, python as default). Then runs the 
**Output:**
- Multiple output text files (.txt) `Output Files (Python)` directoy containing all misspelled words found in the every input file as well as a comprehensive list of depth 1 and depth 2 spelling suggestions (words in which the user may have meant to type instead of the misspelled word).

### Haskell Testing (Hashing and Standard)

#### RunAllHaskell.py
**Input [Prompt]:**
- Asks for a user's main command interpreter (py, python3, python as default). Then runs the 
**Output:**
- Multiple output text files (.txt) in the `Output Files (Haskell)` directoy containing all misspelled words found in the every input file as well as a comprehensive list of depth 1 and depth 2 spelling suggestions (words in which the user may have meant to type instead of the misspelled word).