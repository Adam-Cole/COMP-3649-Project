module Main where

import Input (loadDictionary, loadTextFile)
import ErrorChecker (findMisspelledWords)
import SuggestionFinder (generateSuggestions)
import Output (writeOutput)
import qualified Data.Set as Set
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.Directory (getCurrentDirectory)

-- Function to modify output file name
generateOutputFileName :: FilePath -> FilePath
generateOutputFileName inputFile =
    let baseName = takeBaseName inputFile  -- Extracts "100 Words with Mistakes"
        ext = takeExtension inputFile      -- Extracts ".txt" (if any)
    in baseName ++ " [errors and suggestions]" ++ ext

main :: IO ()
main = do
    cwd <- getCurrentDirectory
    putStrLn $ "Current working directory: " ++ cwd

    -- Ask user for dictionary file
    putStr "Enter dictionary file name: "
    dictFileName <- getLine
    let dictFile = ".." </> "Test Cases" </> "Dictionary Files" </> dictFileName

    -- Ask user for input file
    putStr "Enter file name to check for errors: "
    inputFileName <- getLine
    let inputFile = ".." </> "Test Cases" </> inputFileName

    -- Load dictionary
    dictionary <- loadDictionary dictFile

    -- Load input text file
    textLines <- loadTextFile inputFile

    -- Find misspelled words
    let misspelledWords = findMisspelledWords dictionary textLines

    -- Generate suggestions
    let results = [(line, word, generateSuggestions dictionary word 1, generateSuggestions dictionary word 2)
                    | (line, words) <- misspelledWords, word <- words]

    -- Generate output file name dynamically
    let outputFile = generateOutputFileName inputFile

    -- Write results to output file
    writeOutput outputFile results

    putStrLn $ "Spell check complete. Results saved to " ++ outputFile