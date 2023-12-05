{-# LANGUAGE InstanceSigs #-}
module HsRegex (
    Pattern (..),
    isMatch,
    removeStartingMatchAll,
    findStartingMatchAll,
    removeStartingMatchAny,
    findStartingMatchAny,
    removeStartingMatchLongest,
    findStartingMatchLongest,
) where

import Data.Maybe (isJust, listToMaybe)
import Data.List (isPrefixOf, intercalate)
import Data.Char (isAlphaNum, isNumber, isAlpha)

data Pattern = Null -- Matches immediately
    | Char Char
    | WordChar -- (just a helper)
    | AlphaChar -- (just a helper)
    | DigitChar -- (just a helper)
    | String String -- (just a helper)
    | Seq [Pattern]
    | Sum [Pattern]
    | Star Pattern -- Zero or more times
    | Plus Pattern -- One or more times (just a helper)
    | Opt Pattern -- Zero or one times (just a helper)
    -- TODO - capture groups

showBracketed :: Show a => a -> String
showBracketed x = "(" ++ show x ++ ")"

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c `elem` "_"

isAlphaChar :: Char -> Bool
isAlphaChar = isAlpha

isDigitChar :: Char -> Bool
isDigitChar = isNumber

instance Show Pattern where
    show :: Pattern -> String
    show Null = "∅"
    show (Char c) = [c]
    show WordChar = "\\w"
    show AlphaChar = "[A-z]"
    show DigitChar = "[0-9]"
    show (String s) = s
    show (Seq ps) = concatMap showBracketed ps
    show (Sum ps) = intercalate "|" (map showBracketed ps)
    show (Star p) = showBracketed p ++ "*"
    show (Plus p) = showBracketed p ++ "+"
    show (Opt p) = showBracketed p ++ "?"

prependToFst :: [a] -> ([a], b) -> ([a], b)
prependToFst x1 (x2, y) = (x1++x2, y)

-- |For matching patterns. Returns all possibilities of the reversed matched/consumed string and the non-reversed remaining string to consume
matchAux :: Pattern -> String -> [(String, String)]
matchAux p s = case (p, s) of
    -- Null
    (Null, s) -> [("", s)]
    -- Char
    (Char c, "") -> []
    (Char c, h:ts) -> [([h], ts) | h == c]
    -- Word Char (just a helper)
    (WordChar, "") -> []
    (WordChar, h:ts) -> [([h], ts) | isWordChar h]
    -- Alpha Char (just a helper)
    (AlphaChar, "") -> []
    (AlphaChar, h:ts) -> [([h], ts) | isAlphaChar h]
    -- Digit Char (just a helper)
    (DigitChar, "") -> []
    (DigitChar, h:ts) -> [([h], ts) | isDigitChar h]
    -- String (just a helper)
    (String pStr, s) -> matchAux (Seq (map Char pStr)) s
    -- Seq
    (Seq [], s) -> [("", s)]
    (Seq (ph:pts), s) -> let ms = matchAux ph s in
        concatMap f ms where
            f :: (String, String) -> [(String, String)]
            f (m, rem) = map (prependToFst m) (matchAux (Seq pts) rem)
    -- Sum
    (Sum [], _) -> []
    (Sum (ph:pts), s) -> matchAux ph s ++ matchAux (Sum pts) s
    -- Star
    (Star p', s) -> ("", s) : concatMap f (matchAux p' s) where
        f :: (String, String) -> [(String, String)]
        f (m, rem) = map (prependToFst m) (matchAux (Star p') rem)
    -- Plus (just a helper)
    (Plus p', s) -> matchAux (Seq [p', Star p']) s
    -- Opt (just a helper)
    (Opt p', s) -> matchAux (Sum [Null, p']) s

-- |Find all matches for a regex on a string starting at the start of the string and return all the possible remaining strings after removing the matches
removeStartingMatchAll :: Pattern -> String -> [String]
removeStartingMatchAll p s = map snd (matchAux p s)

-- |Find all matches for a regex on a string starting at the start of the string and return all of them
findStartingMatchAll :: Pattern -> String -> [String]
findStartingMatchAll p s = map fst (matchAux p s)

-- |Find any match for a regex on a string starting at the start of the string and return the remaining string after removing the match
removeStartingMatchAny :: Pattern -> String -> Maybe String
removeStartingMatchAny p s = listToMaybe (removeStartingMatchAll p s)

-- |Find any match for a regex on a string starting at the start of the string and return it
findStartingMatchAny :: Pattern -> String -> Maybe String
findStartingMatchAny p s = listToMaybe (findStartingMatchAll p s)

-- |Find the longest match for a regex on a string starting at the start of the string and return the remaining string after removing the match
removeStartingMatchLongest :: Pattern -> String -> Maybe String
removeStartingMatchLongest p s = (Just . snd) =<< foldl f Nothing (removeStartingMatchAll p s) where
    f Nothing x = Just (length x, x)
    f (Just (n1, x1)) x2 = let n2 = length x2 in
        if n2 < n1 then Just (n2, x2) else Just (n1, x1)

-- |Find the longest match for a regex on a string starting at the start of the string and return it
findStartingMatchLongest :: Pattern -> String -> Maybe String
findStartingMatchLongest p s = (Just . snd) =<< foldl f Nothing (findStartingMatchAll p s) where
    f Nothing x = Just (length x, x)
    f (Just (n1, x1)) x2 = let n2 = length x2 in
        if n2 > n1 then Just (n2, x2) else Just (n1, x1)

-- |Check if a string exactly matches a regex
isMatch :: Pattern -> String -> Bool
isMatch p s = "" `elem` removeStartingMatchAll p s
