-- | Haskell tr implementation. Just supports the swap and delete modes:
-- * tr string1 string2
-- * tr -d string1
--
-- PLEASE DON'T CHANGE THE INTERFACE OF THIS FILE AS WE WILL EXPECT IT TO BE
-- THE SAME WHEN TESTING!
module Tr where

import Data.List

-- | Just to give `tr` a more descriptive type
type CharSet = String

-- | 'tr' - the characters in the first argument are translated into characters
-- in the second argument, where first character in the first CharSet is mapped
-- to the first character in the second CharSet. If the first CharSet is longer
-- than the second CharSet, the last character found in the second CharSet is
-- duplicated until it matches in length.
--
-- If the second CharSet is a `Nothing` value, then 'tr' should run in delete
-- mode where any characters in the input string that match in the first
-- CharSet should be removed.
--
-- The third argument is the string to be translated (i.e., STDIN) and the
-- return type is the output / translated-string (i.e., STDOUT).
-- 
-- translate mode: tr "eo" (Just "oe") "hello" -> "holle"
-- delete mode: tr "e" Nothing "hello" -> "hllo"
--
-- It's up to you how to handle the first argument being the empty string, or
-- the second argument being `Just ""`, we will not be testing this edge case.

tr :: CharSet -> Maybe CharSet -> String -> String
tr prohibitedChars Nothing = deleteEvery prohibitedChars
tr inset (Just outset) = replaceEvery $ zip inset $
    if length inset > length outset
    then replicate (length inset - length outset + 1) (outset !! (length outset - 1))
    else outset

deleteEvery :: String -> String -> String
deleteEvery prohibitedChars = filter $ \c -> all (/= c) prohibitedChars

replaceEvery :: [(Char, Char)] -> String -> String
replaceEvery charsToReplace = 
    map
        (\c -> case find (\(x, _) -> x == c) charsToReplace of
            Just (_, charToReplace) -> charToReplace
            Nothing -> c)