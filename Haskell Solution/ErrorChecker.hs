module ErrorChecker (findMisspelledWords, isProperNoun, cleanWord) where

import qualified Data.Set as Set
import Data.Char (isAlpha, toLower, isUpper)

-- Identify proper nouns by checking if the first letter is uppercase
isProperNoun :: String -> Bool
isProperNoun [] = False  -- Empty words are not proper nouns
isProperNoun (x:_) = isUpper x  -- Check if the first character is uppercase

-- Remove punctuation, hyphens, and apostrophes from a word
cleanWord :: String -> String
cleanWord = map toLower . filter isAlpha  -- Keep only alphabetic characters

-- Check for words not in the dictionary, return (line number, list of misspelled words)
findMisspelledWords :: Set.Set String -> [(Int, [String])] -> [(Int, [String])]
findMisspelledWords dictionary textLines =
    [ (ln, filter (\w -> Set.notMember (cleanWord w) dictionary) words)
    | (ln, words) <- textLines, any (\w -> Set.notMember (cleanWord w) dictionary) words]