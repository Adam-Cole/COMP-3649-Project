module Input (loadDictionary, loadTextFile) where

import qualified Data.Set as Set
import System.IO
import Data.Char (isAlpha)

-- Load dictionary into a Set for fast lookup
loadDictionary :: FilePath -> IO (Set.Set String)
loadDictionary filePath = do
    content <- readFile filePath
    return (Set.fromList (lines content))

-- Normalize each line by replacing hyphens with spaces
normalizeLine :: String -> String
-- normalizeLine = map (\c -> if c `elem` ['-', '\'', '’', '"', ':'] then ' ' else c)
normalizeLine = map (\c -> if isAlpha c || c == ' ' then c else ' ')

-- Load input text file and return a list of (line number, words in line)
loadTextFile :: FilePath -> IO [(Int, [String])]
loadTextFile filePath = do
    content <- readFile filePath
    let cleanedLines = map normalizeLine (lines content)
    --mapM_ print (zip [1..] (map words cleanedLines))
    return (zip [1..] (map words cleanedLines))  -- Line numbers with words

-- -- Get command line arguments
--     args <- getArgs

--     -- Ensure two arguments are provided (textFile and dictFile)
--     -- Will output correct usage if not.
--     if length args /= 2 then
--         putStrLn "Usage: ./program.exe <textFile> <dictionaryFile>"
--     else do
--         let textFile = args !! 0
--             dictFile = args !! 1
        
--         -- Makes it so the user won't have to specify entire directory for path files.
--         -- Limitation: text file must be inside the Test Cases directory, and dictionary 
--         -- must be in the Test Cases/Dictionary Files directory.
--         let fullTextFile = "../Test Cases/" ++ textFile
--             fullDictFile = "../Test Cases/Dictionary Files/" ++ dictFile
        
--         -- Get words from the dictionary and the text files
--         dictWords <- readLines fullDictFile
--         textWords <- readLines fullTextFile

--         -- outputs every line of the input text file.
--         putStrLn "Text File Input"
--         mapM_ putStrLn textWords