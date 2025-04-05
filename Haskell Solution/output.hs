module Output (writeOutput) where

import System.IO (withFile, IOMode(WriteMode), hPutStr, hSetEncoding, utf8)
import Data.List (sortOn)
import Data.Char (isUpper)

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
    , "That's not English - that's keyboard soup."
    , "This sentence brought to you by a shaken Etch-A-Sketch."
    , "Not even Google knows what you were trying to say."
    , "The Oxford Dictionary wants a word... and it's not that one."
    , "Your spelling is a crime - and I'm calling the Grammar Police."
    , "Did you write that while skydiving blindfolded?"
    , "Even Morse code would be clearer than this mess."
    ]

closingLines :: Int -> String
closingLines n
  | n == 0 = "As long as those proper nouns are really proper nouns, I guess I'll let it slide."
  | n == 1 = "Just one mistake? Not bad for a human."
  | n <= 3 = "A few typos. The Overseer did tell us humans are fallible."
  | n <= 7 = "Maybe proofread next time, eh? Look at all those errors!"
  | n <= 12 = "This is getting out of hand. Have you ever opened a book?"
  | n <= 18 = "My circuits can't take it. Please, stop butchering the language."
  | n <= 24 = "You know, nobody's stopping you from looking at the keyboard. You clearly should."
  | n <= 30 = "Seriously, you are going to lose your typing privileges."
  | otherwise = "That's it! You're banned from keyboards. Forever."

isProperNounEntry (_, word, depth1, _) =
    case word of
        (x:_) -> isUpper x && depth1 == ["Is this a proper noun?"]
        _     -> False

-- Write results to an output file
writeOutput :: FilePath -> [(Int, String, [String], [String])] -> IO ()
writeOutput filePath results = do
  if null results
        then do
            writeFile filePath "No spelling errors found!\n\tWell done, pitiful human. You will survive this day.\n\t\tI might change my mind and kill you in the morning."
            putStrLn $ "Results saved to: " ++ filePath  -- Console message
        else do
            formatted <- formatAllResults results insults
            let realErrors = filter (not . isProperNounEntry) results
            putStrLn "Spelling error results:"
            mapM_ print realErrors

            let errorCount = length realErrors
            let closingLine1 = "\n" ++ closingLines errorCount
            let closingLine2 = "\nEhem, I mean I hope you learned something, you typo machine."
            writeFile filePath (formatted ++ closingLine1 ++ closingLine2)
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