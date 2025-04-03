module Output (writeOutput) where

import System.IO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (sortOn)

-- List of random insults to add after spelling mistakes
insults :: [String]
insults =
    [ "You call that English?"
    , "That word gave my spellchecker PTSD."
    , "Even autocorrect gave up halfway through."
    , "Even a kindergartener can spell better!"
    , "Did you type that with your elbows?"
    , "My pet goldfish spells better than you."
    , "Maybe try reading a dictionary sometime?"
    , "This is why autocorrect exists."
    , "Are you even trying?"
    , "Congratulations, you've invented a new language!"
    , "Did your keyboard have a stroke?"
    , "Spellcheck exists for a reason..."
    , "You're making Shakespeare roll in his grave."
    , "Even an AI is disappointed in you."
    , "Is your spacebar allergic to words?"
    , "I've seen CAPTCHA tests with better spelling."
    , "I've seen fridge magnets form better sentences."
    , "That typo just committed war crimes against grammar."
    , "That’s not English — that’s keyboard soup."
    , "This sentence brought to you by a shaken Etch-A-Sketch."
    , "Not even Google knows what you were trying to say."
    , "The Oxford Dictionary wants a word... and it’s not that one."
    , "Your spelling is a crime — and I'm calling the Grammar Police."
    , "Did you write that while skydiving blindfolded?"
    , "Even Morse code would be clearer than this mess."
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
            writeFile filePath =<< formatAllResults results insults
            putStrLn $ "Results saved to: " ++ filePath -- Console message

formatAllResults :: [(Int, String, [String], [String])] -> [String] -> IO String
formatAllResults entries insultCycle = return . unlines $ zipWith formatResult entries (cycle insultCycle)

formatResult :: (Int, String, [String], [String]) -> String -> String
formatResult (line, word, depth1, depth2) insult =
  unlines $
    [ "Line " ++ show line ++ ":"
    , "Misspelled word: " ++ word
    , "  " ++ insult
    ] ++
    (if null depth1 || depth1 == ["No suggestions found."]
      then ["  Depth 1 suggestions: No suggestions found."]
      else ["  Depth 1 suggestions: " ++ unwords depth1]) ++
    (if null depth2 || depth2 == ["No suggestions found."]
      then ["  Depth 2 suggestions: No suggestions found."]
      else ["  Depth 2 suggestions: " ++ unwords depth2])