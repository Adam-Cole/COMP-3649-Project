module Input (readLines) where

import System.IO

-- Reads a file and returns it on a line by line basis.
-- For dictionarys, this will be a single word on each line.
-- For text files, this will be n-many words on each line.
readLines :: FilePath -> IO [String]
readLines filePath = do
    contents <- readFile filePath
    return (lines contents)