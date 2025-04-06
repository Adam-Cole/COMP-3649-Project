-- module SuggestionFinder (generateSuggestions) where
module SuggestionFinder (recursiveSuggestions,generateTwoLevelSuggestions) where

import Pluralization (pluralizationErrors)
import ErrorChecker (isProperNoun, cleanWord)
import Data.List (nub, find)
import qualified Data.HashSet as HashSet

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

-- recursiveSuggestions :: Int -> String -> HashSet.HashSet String -> [(Int, HashSet.HashSet String)]
-- recursiveSuggestions 1 w dict
--   | isProperNoun w && not (HashSet.member w dict) =
--       [ (1, HashSet.singleton "Is this a proper noun? No suggestions found.") ]
--   | otherwise =
--       let candidates = generateLevel w
--           valid      = filter (`HashSet.member` dict) candidates
--       in [ (1, HashSet.fromList valid) ]

-- recursiveSuggestions 2 w dict
--   | isProperNoun w && not (HashSet.member w dict) =
--       [ (2, HashSet.singleton "Is this a proper noun? No suggestions found.") ]
--   | otherwise =
--       let level1Candidates = generateLevel w
--           level2All        = concatMap generateLevel level1Candidates
--           level1Filtered   = filter (`HashSet.member` dict) level1Candidates
--           level2Valid      = filter (`HashSet.member` dict) level2All
--       in [ (1, HashSet.fromList level1Filtered)
--          , (2, HashSet.fromList level2Valid)
--          ]

-- generateLevel :: String -> [String]
-- generateLevel word =
--   deletion word ++
--   insertion word ++
--   replacement word ++
--   transposition word ++
--   pluralizationErrors word

-- generateTwoLevelSuggestions :: HashSet.HashSet String -> String -> ([String], [String])
-- generateTwoLevelSuggestions dict word =
--   let grouped = recursiveSuggestions 2 word dict
--       depth1  = maybe ["No suggestions found."] HashSet.toList (lookup 1 grouped)
--       depth2  = maybe ["No suggestions found."] HashSet.toList (lookup 2 grouped)
--   in (depth1, depth2)

recursiveSuggestions :: HashSet.HashSet String -> String -> Int -> [(Int, HashSet.HashSet String)]
recursiveSuggestions dict word maxDepth
  | isProperNoun word && not (HashSet.member (cleanWord word) dict) =
      [(1, HashSet.singleton "Is this a proper noun?"), (2, HashSet.singleton "No suggestions found.")]
  | otherwise =
      let baseWord = cleanWord word
          level1 = generateLevel dict baseWord
          level1Suggestions = HashSet.filter (`HashSet.member` dict) level1
          level2Seeds = HashSet.toList level1
          level2 = HashSet.fromList $ concatMap (HashSet.toList . generateLevel dict) level2Seeds
          -- Remove level1 suggestions from level2 suggestions
          level2Suggestions = HashSet.difference (HashSet.filter (`HashSet.member` dict) level2) level1Suggestions
      in
        [ (1, if HashSet.null level1Suggestions then HashSet.singleton "No suggestions found." else level1Suggestions)
        , (2, if HashSet.null level2Suggestions then HashSet.singleton "No suggestions found." else level2Suggestions)
        ]

-- Generate first-order edits from a word
generateLevel :: HashSet.HashSet String -> String -> HashSet.HashSet String
generateLevel _ word = HashSet.fromList . map cleanWord . HashSet.toList . HashSet.fromList $
  deletion word ++
  insertion word ++
  transposition word ++
  replacement word ++
  pluralizationErrors word

-- Generate depth 1 and 2 suggestions separately
generateTwoLevelSuggestions :: HashSet.HashSet String -> String -> ([String], [String])
generateTwoLevelSuggestions dict word =
  let grouped = recursiveSuggestions dict word 2
      depth1 = maybe ["No suggestions found."] HashSet.toList $ lookup 1 grouped
      depth2 = maybe ["No suggestions found."] HashSet.toList $ lookup 2 grouped
  in (depth1, depth2)
