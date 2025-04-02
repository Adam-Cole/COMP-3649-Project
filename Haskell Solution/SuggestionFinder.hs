-- module SuggestionFinder (generateSuggestions) where
module SuggestionFinder (recursiveSuggestions,generateTwoLevelSuggestions) where

import Pluralization (pluralizationErrors)
import ErrorChecker (isProperNoun, cleanWord)
import qualified Data.Set as Set
import Data.List (nub, find)
import qualified Data.Map.Strict as Map

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
--             validEdits = filter (`Set.member` dictionary) cleanedEdits
--             -- deeperSuggestions = concatMap (\w -> generateSuggestions dictionary (cleanWord w) (depth - 1)) cleanedEdits
--         in if null validEdits -- && null deeperSuggestions
--             then ["No suggestions found."]  -- Print this when no suggestions exist
--             else validEdits ++ concatMap (\w -> generateSuggestions dictionary (cleanWord w) (depth - 1)) validEdits

-- Optimized recursive suggestion generation with caching and minimal recomputation

recursiveSuggestions :: Set.Set String -> String -> Int -> [(Int, Set.Set String)]
recursiveSuggestions dict word maxDepth
  | isProperNoun word && not (Set.member (cleanWord word) dict) =
      [(1, Set.singleton "Is this a proper noun?"), (2, Set.singleton "No suggestions found.")]
  | otherwise =
      let baseWord = cleanWord word
          level1 = generateLevel dict baseWord
          level1Suggestions = Set.filter (`Set.member` dict) level1
          level2Seeds = Set.toList level1
          level2 = Set.fromList $ concatMap (Set.toList . generateLevel dict) level2Seeds
          level2Suggestions = Set.difference (Set.filter (`Set.member` dict) level2) level1Suggestions
      in
        [ (1, if Set.null level1Suggestions then Set.singleton "No suggestions found." else level1Suggestions)
        , (2, if Set.null level2Suggestions then Set.singleton "No suggestions found." else level2Suggestions)
        ]

-- Generate first-order edits from a word
generateLevel :: Set.Set String -> String -> Set.Set String
generateLevel _ word = Set.fromList . map cleanWord . Set.toList . Set.fromList $
  deletion word ++
  insertion word ++
  transposition word ++
  replacement word ++
  pluralizationErrors word

-- Generate depth 1 and 2 suggestions separately
generateTwoLevelSuggestions :: Set.Set String -> String -> ([String], [String])
generateTwoLevelSuggestions dict word =
  let grouped = recursiveSuggestions dict word 2
      depth1 = maybe ["No suggestions found."] Set.toList $ lookup 1 grouped
      depth2 = maybe ["No suggestions found."] Set.toList $ lookup 2 grouped
  in (depth1, depth2)