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
import System.Environment (getArgs)

-- Function to modify output file name and location
generateOutputFileName :: FilePath -> FilePath
generateOutputFileName inputFile =
    let baseName = takeBaseName inputFile  -- Extracts "100 Words with Mistakes"
        ext = takeExtension inputFile      -- Extracts ".txt" (if any)
        outputDir = "../Output Files (Haskell)/"
    in outputDir ++ baseName ++ " [errors and suggestions]" ++ ext

main :: IO ()
main = do
    args <- getArgs

    -- Ensure two arguments are provided (textFile and dictFile)
    -- Will output correct usage if not.
    (dictFile, inputFile) <- 
        if length args == 2 then do
            -- Use the provided arguments for file paths
            let inputFileName = args !! 0
                dictFileName = args !! 1
            let dictFile = ".." </> "Test Cases" </> "Dictionary Files" </> dictFileName
            let inputFile = ".." </> "Test Cases" </> inputFileName
            return (dictFile, inputFile)
        else do
            -- Prompt the user to select files
            dictFileName <- selectFile "../Test Cases/Dictionary Files"
            inputFileName <- selectFile "../Test Cases"
            let dictFile = ".." </> "Test Cases" </> "Dictionary Files" </> dictFileName
            let inputFile = ".." </> "Test Cases" </> inputFileName
            return (dictFile, inputFile)

    -- Load dictionary
    dictionary <- loadDictionary dictFile

    -- Load input text file
    textLines <- loadTextFile inputFile

    -- Find misspelled words
    let misspelledWords = findMisspelledWords dictionary textLines

    -- Generate suggestions
    -- let results = [(line, word, generateSuggestions dictionary word 1, generateSuggestions dictionary word 2)
    --                 | (line, words) <- misspelledWords, word <- words]
    -- ^^^ old version that wasn't quite right for the recursion ^^^
    
    let results = [(line, word, d1, d2)
              | (line, words) <- misspelledWords
              , word <- words
              , let (d1, d2) = generateTwoLevelSuggestions dictionary word]

    -- Generate output file name dynamically
    let outputFile = generateOutputFileName inputFile

    -- Write results to output file
    writeOutput outputFile results