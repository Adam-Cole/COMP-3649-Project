module Input (loadDictionary, loadTextFile) where

import System.IO ()
import Data.Char (isAlpha)
import qualified Data.HashSet as HashSet

-- Load dictionary into a Set for fast lookup
loadDictionary :: FilePath -> IO (HashSet.HashSet String)
loadDictionary filePath = do
    content <- readFile filePath
    return (HashSet.fromList (lines content))

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