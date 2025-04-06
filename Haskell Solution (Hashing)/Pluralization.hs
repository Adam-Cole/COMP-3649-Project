module Pluralization (pluralizationErrors) where

import Data.List (isSuffixOf, nub)

-- Generate possible correct words by handling common pluralization mistakes
pluralizationErrors :: String -> [String]
pluralizationErrors word = nub $ concat
    [ ruleYS word
    , ruleRemoveS word
    , ruleAddS word
    , ruleFtoVES word
    , ruleOtoOES word
    , ruleLatinGreek word
    , ruleIrregular word
    ]

-- Rule 1: "ys" → "ies" (e.g., "centurys" → "centuries")
ruleYS :: String -> [String]
ruleYS word
    | "ys" `isSuffixOf` word = [take (length word - 2) word ++ "ies"]
    | otherwise = []

-- Rule 2: Remove "s" if singular exists (e.g., "cats" → "cat")
ruleRemoveS :: String -> [String]
ruleRemoveS word
    | "s" `isSuffixOf` word = [init word]
    | otherwise = []

-- Rule 3: Try adding "s" if singular exists (e.g., "box" → "boxes")
ruleAddS :: String -> [String]
ruleAddS word = [word ++ "s"]

-- Rule 4: Convert "-f" to "-ves" (e.g., "leaf" → "leaves")
ruleFtoVES :: String -> [String]
ruleFtoVES word
    | "f" `isSuffixOf` word = [init word ++ "ves"]
    | "fe" `isSuffixOf` word = [take (length word - 2) word ++ "ves"]
    | otherwise = []

-- Rule 5: Convert "-o" to "-oes" (e.g., "potato" → "potatoes")
ruleOtoOES :: String -> [String]
ruleOtoOES word
    | "o" `isSuffixOf` word = [word ++ "es"]
    | otherwise = []

-- Rule 6: Latin/Greek pluralization (e.g., "cactus" → "cacti", "analysis" → "analyses")
ruleLatinGreek :: String -> [String]
ruleLatinGreek word
    | "us" `isSuffixOf` word = [take (length word - 2) word ++ "i"]
    | "is" `isSuffixOf` word = [take (length word - 2) word ++ "es"]
    | "on" `isSuffixOf` word = [take (length word - 2) word ++ "a"]
    | otherwise = []

-- Rule 7: Irregular cases (e.g., "mouse" → "mice", "man" → "men")
ruleIrregular :: String -> [String]
ruleIrregular word = case word of
    "foot" -> ["feet"]
    "tooth" -> ["teeth"]
    "mouse" -> ["mice"]
    "man" -> ["men"]
    "woman" -> ["women"]
    "child" -> ["children"]
    "person" -> ["people"]
    "ox" -> ["oxen"]
    "radius" -> ["radii"]
    "alumnus" -> ["alumni"]
    _ -> []