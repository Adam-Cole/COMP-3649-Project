module SuggestionFinder (generateSuggestions) where

import Pluralization (pluralizationErrors)
import ErrorChecker (isProperNoun, cleanWord)
import qualified Data.Set as Set
import Data.List (nub)


-- Generate word variations by removing one character at a time
deletion :: String -> [String]
deletion word = [take i word ++ drop (i + 1) word | i <- [0..length word - 1]]

-- Generate word variations by inserting a character at every position
insertion :: String -> [String]
insertion word = [take i word ++ [c] ++ drop i word | i <- [0..length word], c <- ['a'..'z']]

-- Generate word variations by swapping adjacent characters
transposition :: String -> [String]
transposition (x:y:xs) = (y:x:xs) : [x:z | z <- transposition (y:xs)]
transposition _ = []

-- Generate word variations by replacing each character with another letter
replacement :: String -> [String]
replacement word = [take i word ++ [c] ++ drop (i + 1) word 
    | i <- [0..length word - 1], c <- ['a'..'z'], c /= word !! i]

-- Generate all possible suggestions within a certain depth
generateSuggestions :: Set.Set String -> String -> Int -> [String]
generateSuggestions dictionary word depth
    | isProperNoun word = ["Is this a proper noun?"]  -- Special message for proper nouns
    | depth == 0 = []
    | otherwise  = 
        let edits = nub (deletion word ++ insertion word ++ transposition word ++ replacement word ++ pluralizationErrors word)
            cleanedEdits = map cleanWord edits
            validEdits = filter (`Set.member` dictionary) cleanedEdits
            -- deeperSuggestions = concatMap (\w -> generateSuggestions dictionary (cleanWord w) (depth - 1)) cleanedEdits
        in if null validEdits -- && null deeperSuggestions
            then ["No suggestions found."]  -- Print this when no suggestions exist
            else validEdits ++ concatMap (\w -> generateSuggestions dictionary (cleanWord w) (depth - 1)) validEdits