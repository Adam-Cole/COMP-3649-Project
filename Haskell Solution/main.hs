module Main where

import Input (loadDictionary, loadTextFile)
import ErrorChecker (findMisspelledWords)
--import SuggestionFinder (generateSuggestions)
import SuggestionFinder (generateTwoLevelSuggestions)
import Output (writeOutput)
import qualified Data.Set as Set
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.Directory (getCurrentDirectory)
import FileSelector (selectFile)

-- Function to modify output file name
generateOutputFileName :: FilePath -> FilePath
generateOutputFileName inputFile =
    let baseName = takeBaseName inputFile  -- Extracts "100 Words with Mistakes"
        ext = takeExtension inputFile      -- Extracts ".txt" (if any)
        outputDir = "../Output Files (Haskell)/"
    in outputDir ++ baseName ++ " [errors and suggestions]" ++ ext

main :: IO ()
main = do
    cwd <- getCurrentDirectory
    putStrLn $ "Current working directory: " ++ cwd

    -- Ask user for dictionary file
    dictFile <- selectFile "../Test Cases/Dictionary Files"

    -- Ask user for input file
    inputFile <- selectFile "../Test Cases"

    -- Load dictionary
    dictionary <- loadDictionary dictFile

    -- Load input text file
    textLines <- loadTextFile inputFile

    -- Find misspelled words
    let misspelledWords = findMisspelledWords dictionary textLines

    -- Generate suggestions
    -- let results = [(line, word, generateSuggestions dictionary word 1, generateSuggestions dictionary word 2)
    --                 | (line, words) <- misspelledWords, word <- words]
    let results = [(line, word, d1, d2)
              | (line, words) <- misspelledWords
              , word <- words
              , let (d1, d2) = generateTwoLevelSuggestions dictionary word]

    -- Generate output file name dynamically
    let outputFile = generateOutputFileName inputFile

    -- Write results to output file
    writeOutput outputFile results

    putStrLn $ "Spell check complete. Results saved to " ++ outputFile