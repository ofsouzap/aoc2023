{-# LANGUAGE InstanceSigs #-}
module HsRegex (
    Pattern (..),
    Groups,
    isMatch,
) where

import Data.Maybe (isJust)
import Data.List (isPrefixOf, intercalate)

data Pattern = Null -- Matches immediately
    | Char Char
    | String String -- (just a helper)
    | Seq [Pattern]
    | Sum [Pattern]
    | Star Pattern -- Zero or more times
    | Plus Pattern -- One or more times (just a helper)
    | Opt Pattern -- Zero or one times (just a helper)

showBracketed :: Show a => a -> String
showBracketed x = "(" ++ show x ++ ")"

instance Show Pattern where
    show :: Pattern -> String
    show Null = "∅"
    show (Char c) = [c]
    show (String s) = s
    show (Seq ps) = concatMap showBracketed ps
    show (Sum ps) = intercalate "|" (map showBracketed ps)
    show (Star p) = showBracketed p ++ "*"
    show (Plus p) = showBracketed p ++ "+"
    show (Opt p) = showBracketed p ++ "?"

type Groups = [(String, String)]

matchAux :: Pattern -> String -> [String]
matchAux p s = case (p, s) of
    -- Null
    (Null, s) -> [s]
    -- Char
    (Char c, "") -> []
    (Char c, h:ts) -> [ts | h == c]
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

-- |Match a regex pattern to a string and return the named capture groups
isMatch :: Pattern -> String -> Bool
isMatch p s = "" `elem` matchAux p s
