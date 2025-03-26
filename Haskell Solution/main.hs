import qualified Data.Set as Set
import Data.Char (toLower, isAlpha, ord)
import System.Environment (getArgs)
import Input (readLines)

-- to compile, run: ghc -no-keep-hi-files -no-keep-o-files -o program main.hs input.hs
-- to run: .\program.exe "100 Words with Mistakes.txt" "words_alpha.txt"

main :: IO ()
main = do
    -- Get command line arguments
    args <- getArgs

    -- Ensure two arguments are provided (textFile and dictFile)
    -- Will output correct usage if not.
    if length args /= 2 then
        putStrLn "Usage: ./program.exe <textFile> <dictionaryFile>"
    else do
        let textFile = args !! 0
            dictFile = args !! 1
        
        -- Makes it so the user won't have to specify entire directory for path files.
        -- Limitation: text file must be inside the Test Cases directory, and dictionary 
        -- must be in the Test Cases/Dictionary Files directory.
        let fullTextFile = "../Test Cases/" ++ textFile
            fullDictFile = "../Test Cases/Dictionary Files/" ++ dictFile
        
        -- Get words from the dictionary and the text files
        dictWords <- readLines fullDictFile
        textWords <- readLines fullTextFile

        -- outputs every line of the input text file.
        putStrLn "Text File Input"
        mapM_ putStrLn textWords
