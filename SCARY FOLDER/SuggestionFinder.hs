-- module SuggestionFinder (generateSuggestions) where
module SuggestionFinder (recursiveSuggestions,generateTwoLevelSuggestions) where

import Pluralization (pluralizationErrors)
import ErrorChecker (isProperNoun, cleanWord)
--import qualified Data.Set as Set
import Data.List (nub, find)
--import qualified Data.Map.Strict as Map
import qualified Data.HashSet as HashSet
import Data.Hashable

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
-- generateSuggestions :: Set.Set String -> String -> Int -> [String]
-- generateSuggestions dictionary word depth
--     | isProperNoun word = ["Is this a proper noun?"]  -- Special message for proper nouns
--     | depth == 0 = []
--     | otherwise  = 
--         let edits = nub (deletion word ++ insertion word ++ transposition word ++ replacement word ++ pluralizationErrors word)
--             cleanedEdits = map cleanWord edits
--             validEdits = filter (`HashSet.member` dictionary) cleanedEdits
--             -- deeperSuggestions = concatMap (\w -> generateSuggestions dictionary (cleanWord w) (depth - 1)) cleanedEdits
--         in if null validEdits -- && null deeperSuggestions
--             then ["No suggestions found."]  -- Print this when no suggestions exist
--             else validEdits ++ concatMap (\w -> generateSuggestions dictionary (cleanWord w) (depth - 1)) validEdits

-- Optimized recursive suggestion generation with caching and minimal recomputation

recursiveSuggestions :: HashSet.HashSet String -> String -> [(Int, HashSet.HashSet String)]
recursiveSuggestions dict word
  | isProperNoun word =
      [ (2, HashSet.singleton "Is this a proper noun? No suggestions found.") ]
  | otherwise =
      let baseWord = cleanWord word
          level1   = generateLevel dict baseWord
          level2All = HashSet.foldr (HashSet.union . generateLevel dict)
                                    HashSet.empty
                                    level1
          level2Filtered = HashSet.intersection level2All dict
          level2Suggestions = HashSet.difference level2Filtered level1
          lvl1Result = if HashSet.null level1
                         then HashSet.singleton "No suggestions found."
                         else level1
          lvl2Result = if HashSet.null level2Suggestions
                         then HashSet.singleton "No suggestions found."
                         else level2Suggestions
      in [ (1, lvl1Result)
         , (2, lvl2Result)
         ]

-- Generate first-order edits from a word
generateLevel :: HashSet.HashSet String -> String -> HashSet.HashSet String
generateLevel dict word =
    let candidates =
          deletion word
          ++ insertion word
          ++ replacement word
          ++ transposition word
          ++ pluralizationErrors word
        -- Filter candidates so we only keep those actually in 'dict'
        filtered = filter (`HashSet.member` dict) candidates
        cleaned  = map cleanWord filtered
    in HashSet.fromList cleaned

-- Generate depth 1 and 2 suggestions separately
generateTwoLevelSuggestions :: HashSet.HashSet String -> String -> ([String], [String])
generateTwoLevelSuggestions dict word =
    let grouped = recursiveSuggestions dict word
        depth1  = maybe ["No suggestions found."] HashSet.toList (lookup 1 grouped)
        depth2  = maybe ["No suggestions found."] HashSet.toList (lookup 2 grouped)
    in (depth1, depth2)