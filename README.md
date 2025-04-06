# COMP-3649-Project

## Project Purpose
The overall purpose for our project was to create a tool that can take in any .txt file as input (formated in any way), and process it in such a way that compares each word in the input file, one at a time, to a defined dictionary file (.txt). Before the comparison occurs, the words in the input file are tokenized (so they can be worked with individually) and then cleaned up (stripping all punctuation and lowering all letters of the word for a more accurate comparison to the dictionary words). In simple terms, the comparison would either come back as true in which case the current word being analyzed in the input file is correctly spelled, or false in which case the current word being analyzed in the input file is misspelled. If the word is correctly spelled it will be ignored and the program will provide no spelling suggestions. If the word is misspelled, the word will then get passed through our set of functions in order to generate a comprehensive list of depth 1 (1 character manipulation of the misspelled word (ex. 1 character insertion, 1 character deletion, etc.) that results in a valid word in the dictionary) and depth 2 spelling suggestions (2 character manipulations of the misspelled word (ex. 1 character insertion and 1 character transposition, etc.) that results in a valid word in the dictionary) that will be outputted for the user to visualize which words in their input file they misspelled, and provide them a list of words in which they may have meant to write instead. At a very high level, our tool’s purpose is to act as a spell checker.

## Python (Interprative Solution)

### Compiling and Running
Python does not require any compiling, and there are no third party libraries that require installation prior to use. To run the program, you can use either command-line functions or the program will prompt the user with a “dropdown” interface. If using the command-line functions, the user must first enter the sub directory in the project folder before entering the commands.

#### When using the dropdown it will be as follows:
1. Run via python Spellchecker.py
2. Select input file,
3. Select dictionary file

#### Example Command Line Sequences:
python Spellchecker.py "1 Word (depth 2).txt" "words_alpha.txt"\
python Spellchecker.py "1 Word (depth 3).txt" "words_alpha.txt"\
python Spellchecker.py "100 Words with Mistakes.txt" "words_alpha.txt"
python Spellchecker.py "500 Words with Mistakes (with proper nouns).txt" "words_alpha.txt"
python Spellchecker.py "1000 Word with Mistakes (but the same story).txt" "words_alpha.txt"

**Note: python interpreter name may need to be changed (e.g. Matt’s laptop uses python3, Adam uses py)**

## Haskell (Functional Solution)
We have two iterations of Haskell, one which does not require any third party installations, and one that does require third party installations. The solution found in the `Haskell Solution` folder will not require third party installs. The solution found in the `Haskell Solution (Hashing)` folder required the below installations. Note: This put us in a position that made it more awkward to run our pre-existing solution. Likely because we are Haskell novices. Upon running the below cabal installs, we then had to manually add packages to our pre-existing solution upon compiling, or we would run into package conflicts, which we were unable to figure out an easier way to remove. `ghc -o main main.hs -hide-all-packages -package base -package containers -package directory -package filepath`

`cabal install --lib hashable unordered-containers`
`cabal install --lib time`
`cabal install --lib filepath directory`

### Compiling and Running

#### When using the dropdown it will be as follows:

#### Example Command Line Sequences:
main.exe "1 Word (depth 2).txt" "words_alpha.txt"
main.exe "1 Word (depth 3).txt" "words_alpha.txt"
main.exe "100 Words with Mistakes.txt" "words_alpha.txt"
main.exe "500 Words with Mistakes (with proper nouns).txt" "words_alpha.txt"
main.exe "1000 Word with Mistakes (but the same story).txt" "words_alpha.txt"

**Note: main.exe call may need to be changed (e.g. Matt’s laptop uses ./main)**