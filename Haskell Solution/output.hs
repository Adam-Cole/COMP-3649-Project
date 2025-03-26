module Output (writeOutput) where

import System.IO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (sortOn)

-- List of random insults to add after spelling mistakes
insults :: [String]
insults =
    [ "You call that English?"
    , "Even a kindergartener can spell better!"
    , "My pet goldfish spells better than you."
    , "Maybe try reading a dictionary sometime?"
    , "This is why autocorrect exists."
    , "Are you even trying?"
    , "Did your keyboard have a stroke?"
    , "Spellcheck exists for a reason..."
    , "You're making Shakespeare roll in his grave."
    , "Even an AI is disappointed in you."
    ]

-- Function to pick a "random" insult based on system time
getTimeBasedInsult :: IO String
getTimeBasedInsult = do
    time <- getPOSIXTime  -- Get current time in seconds
    let index = round (time * 1000) `mod` length insults  -- Use time to create an index
    return (insults !! index)

-- Write results to an output file
writeOutput :: FilePath -> [(Int, String, [String], [String])] -> IO ()
writeOutput filePath results = do
  if null results
        then do
            writeFile filePath "No spelling errors found!\n\tWell done, pitiful human. You will survive this day.\n\t\tI might change my mind and kill you in the morning."
            putStrLn $ "Results saved to: " ++ filePath  -- Console message
        else do
            writeFile filePath (unlines (map formatResult results))
            putStrLn $ "Results saved to: " ++ filePath

-- Format the output for each misspelled word
formatResult :: (Int, String, [String], [String]) -> String

formatResult (line, word, ["Is this a proper noun?"], _) =
    "Line " ++ show line ++ ":\n" ++
    "Misspelled word: " ++ word ++ "\n" ++
    "  Suggestion: Is this a proper noun?\n"

formatResult (line, word, ["No suggestions found."], ["No suggestions found."]) =
    "Line " ++ show line ++ ":\n" ++
    "Misspelled word: " ++ word ++ "\n" ++
    "  No suggestions found.\n"

formatResult (line, word, d1, d2) =
    "Line " ++ show line ++ ":\n" ++
    "Misspelled word: " ++ word ++ "\n" ++
    "  Depth 1 suggestions: " ++ unwords d1 ++ "\n" ++
    "  Depth 2 suggestions: " ++ unwords d2 ++ "\n"