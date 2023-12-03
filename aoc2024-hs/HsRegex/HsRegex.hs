module HsRegex (
    Pattern (..),
    Groups,
    isMatch,
) where

import Data.Maybe (isJust)
import Data.List (isPrefixOf)

data Pattern = Char Char
    | String String
    | Seq [Pattern]
    | Sum [Pattern]
    -- | Star Pattern -- Zero or more times -- TODO
    -- | Plus Pattern -- One or more times -- TODO
    -- | Opt Pattern -- Zero or one times -- TODO

-- TODO - Show instance for Pattern

type Groups = [(String, String)]

-- |Match as much of the string as can be matched and return a Just of the non-matched string or return Nothing if string cannot be matched.
-- |Also returns the named capturing groups
matchAux :: Pattern -> String -> Maybe String
matchAux p s = case (p, s) of
    -- Char
    (Char c, "") -> Nothing
    (Char c, h:ts) -> if h == c then Just ts else Nothing
    -- String
    (String pStr, s) -> matchAux (Seq (map Char pStr)) s
    -- Seq
    (Seq [], s) -> Just s
    (Seq (ph:pts), s) -> case matchAux ph s of
        Nothing -> Nothing
        Just s' -> matchAux (Seq pts) s'
    -- Sum
    (Sum [], _) -> Nothing
    (Sum (ph:pts), s) -> case matchAux ph s of
        Nothing -> matchAux (Sum pts) s
        Just s' -> Just s'

-- |Match a regex pattern to a string and return the named capture groups
isMatch :: Pattern -> String -> Bool
isMatch p s = case matchAux p s of
    Nothing -> False
    Just "" -> True
    Just (_:_) -> False
