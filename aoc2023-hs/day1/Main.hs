module Main where

import Data.Char (isDigit)
import System.Environment (getArgs)
import Data.Foldable (Foldable(sum))
import Utils (readFileLines)
import Data.Maybe (fromMaybe, isJust)
import Data.List (isPrefixOf, find)

inputFileName :: String
inputFileName = "inputs/day1.txt"

testAInputFileName :: String
testAInputFileName = "inputs/day1-testa.txt"

testBInputFileName :: String
testBInputFileName = "inputs/day1-testb.txt"

-- Part A

firstLastDigit :: String -> (Maybe Char, Maybe Char)
firstLastDigit = foldl
        (\ (c1, c2) x -> (replFirst c1 x, replLast c2 x))
        (Nothing, Nothing) . filter isDigit
    where
        replFirst Nothing b = Just b
        replFirst a@(Just _) _ = a
        replLast _ = Just

firstLastToValue :: (Maybe Char, Maybe Char) -> Maybe Int
firstLastToValue (Just a, Just b) = Just (read [a, b])
firstLastToValue (_, _) = Nothing

-- Part B

tryReadDigitWritten :: String -> Int -> String -> Maybe Int
tryReadDigitWritten exp n inp = if exp `isPrefixOf` inp then Just n else Nothing

tryReadDigitChar :: String -> Maybe Int
tryReadDigitChar [] = Nothing
tryReadDigitChar (h:ts) = if isDigit h then Just (read [h]) else Nothing

tryReadFuncs :: [String -> Maybe Int]
tryReadFuncs = [
    tryReadDigitWritten "one" 1,
    tryReadDigitWritten "two" 2,
    tryReadDigitWritten "three" 3,
    tryReadDigitWritten "four" 4,
    tryReadDigitWritten "five" 5,
    tryReadDigitWritten "six" 6,
    tryReadDigitWritten "seven" 7,
    tryReadDigitWritten "eight" 8,
    tryReadDigitWritten "nine" 9,
    tryReadDigitChar
    ]

-- |Take a string and try to read either a digit or a written-out digit and return the digit as an integer
tryReadDigitInclWritten :: String -> Maybe Int
tryReadDigitInclWritten s = fromMaybe Nothing (find isJust (map (\ f -> f s) tryReadFuncs))

firstLastDigitInclWritten :: String -> (Maybe Int, Maybe Int)
firstLastDigitInclWritten = aux (Nothing, Nothing) where
    aux c "" = c
    aux c s@(_:ts) = case tryReadDigitInclWritten s of
        Nothing -> aux c ts
        Just x -> aux (replParts c x) ts

    replParts (c1, c2) x = (replFirst c1 x, replSecond c2 x)

    replFirst :: Maybe a -> a -> Maybe a
    replFirst Nothing y = Just y
    replFirst x@(Just _) _ = x

    replSecond :: Maybe a -> a -> Maybe a
    replSecond _ = Just

intDigitsCombined :: (Maybe Int, Maybe Int) -> Maybe Int
intDigitsCombined (Just a, Just b) = Just ((10*a)+b)
intDigitsCombined (_, _) = Nothing

-- Main

mainFor :: String -> FilePath -> IO ()
mainFor name fn = do
    lines <- readFileLines fn
    putStr ("Part A (" ++ name ++ "): ")
    let tot = (sum . map (fromMaybe 0 . firstLastToValue . firstLastDigit)) lines
    print tot
    putStr ("Part B (" ++ name ++ "): ")
    let tot = (sum . map (fromMaybe 0 . intDigitsCombined . firstLastDigitInclWritten)) lines
    print tot

main :: IO ()
main = do
    mainFor "Test A" testAInputFileName
    mainFor "Test B" testBInputFileName
    mainFor "Main" inputFileName
