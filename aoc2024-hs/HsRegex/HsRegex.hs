{-# LANGUAGE InstanceSigs #-}
module HsRegex (
    Pattern (..),
    isMatch,
    removeStartingMatch,
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

matchAux :: Pattern -> String -> [String]
matchAux p s = case (p, s) of
    -- Null
    (Null, s) -> [s]
    -- Char
    (Char c, "") -> []
    (Char c, h:ts) -> [ts | h == c]
    -- Word Char (just a helper)
    (WordChar, "") -> []
    (WordChar, h:ts) -> [ts | isWordChar h]
    -- Alpha Char (just a helper)
    (AlphaChar, "") -> []
    (AlphaChar, h:ts) -> [ts | isAlphaChar h]
    -- Digit Char (just a helper)
    (DigitChar, "") -> []
    (DigitChar, h:ts) -> [ts | isDigitChar h]
    -- String (just a helper)
    (String pStr, s) -> matchAux (Seq (map Char pStr)) s
    -- Seq
    (Seq [], s) -> [s]
    (Seq (ph:pts), s) -> let ms = matchAux ph s in
        concatMap (matchAux (Seq pts)) ms
    -- Sum
    (Sum [], _) -> []
    (Sum (ph:pts), s) -> matchAux ph s ++ matchAux (Sum pts) s
    -- Star
    (Star p', s) -> s : concatMap (\ x -> x : matchAux (Star p') x) (matchAux p' s)
    -- Plus (just a helper)
    (Plus p', s) -> matchAux (Seq [p', Star p']) s
    -- Opt (just a helper)
    (Opt p', s) -> matchAux (Sum [Null, p']) s

-- |Find any match for a regex on a string starting at the start of the string and return the remaining string after removing the match
removeStartingMatch :: Pattern -> String -> Maybe String
removeStartingMatch p s = listToMaybe (matchAux p s)

-- |Match a regex pattern to a string and return the named capture groups
isMatch :: Pattern -> String -> Bool
isMatch p s = "" `elem` matchAux p s
