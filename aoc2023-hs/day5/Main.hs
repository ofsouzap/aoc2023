module Main where

import Data.Maybe (fromMaybe)
import Utils (readFileLines, splitByOrdered, removeWhitespace, splitByNewLineOrdered)
import HsRegex (Pattern(..), removeStartingMatchLongest, isMatch)

inputFileName :: String
inputFileName = "inputs/day5.txt"

testInputFileName :: String
testInputFileName = "inputs/day5-test.txt"

-- The mappings

type AocMap = [(Int, Int, Int)]

empty :: AocMap
empty = []

insert :: (Int, Int, Int) -> AocMap -> AocMap
insert v m = v : m

use :: AocMap -> Int -> Int
use [] x = x
use ((dst,src,range):ts) x = let diff = x - src in
    if 0 <= diff && diff < range
        then dst + diff
        else use ts x

useOnRange :: AocMap -> (Int, Int) -> [(Int, Int)]
useOnRange [] r = [r]
useOnRange _ (_, 0) = []
useOnRange ((mDst,mSrc,mRange):ts) x@(xStart,xRange) = within ++ before ++ after where
    beforeEnd = min (mSrc - 1) (xStart + xRange - 1)
    beforeStart = min (beforeEnd + 1) xStart
    afterStart = max (mSrc + mRange) xStart
    afterEnd = max (afterStart - 1) (xStart + xRange - 1)
    withinStart = max mSrc xStart
    withinEnd = min (mSrc + mRange - 1) (xStart + xRange - 1)

    beforeRange = max 0 (beforeEnd - beforeStart + 1)
    afterRange = max 0 (afterEnd - afterStart + 1)
    withinRange = max 0 (withinEnd - withinStart + 1)

    beforeSpan = (beforeStart, beforeRange)
    afterSpan = (afterStart, afterRange)

    before = if beforeRange > 0 then useOnRange ts beforeSpan else []
    after = if afterRange > 0 then useOnRange ts afterSpan else []
    within = let diff = withinStart - mSrc in [(mDst + diff, withinRange) | withinRange > 0]

-- Reading Inputs

everyOther :: [a] -> [a]
everyOther = reverse . aux [] where
    aux acc [] = acc
    aux acc [x] = x : acc
    aux acc (x:_:ts) = aux (x : acc) ts

pairUp :: [a] -> [(a, a)]
pairUp xs = zip (everyOther xs) (everyOther (tail xs))

readSeeds :: String -> [Int]
readSeeds = map read . splitByOrdered (' ' ==) . fromMaybe "" . removeStartingMatchLongest (String "seeds: ")

readSeedsAsRanges :: String -> [(Int, Int)]
readSeedsAsRanges = pairUp . readSeeds

mappingTitlePattern :: Pattern
mappingTitlePattern = Seq [Star AlphaChar, String "-to-", Star AlphaChar, String " map:"]

mappingLinePattern :: Pattern
mappingLinePattern = Seq [Plus DigitChar, Char ' ', Plus DigitChar, Char ' ', Plus DigitChar]

readConsumeMapping :: AocMap -> [String] -> ([String], AocMap)
readConsumeMapping acc [] = ([], acc)
readConsumeMapping acc (line:ts)
    | isMatch mappingTitlePattern line = readConsumeMapping acc ts
    | isMatch mappingLinePattern line = let acc' = readMappingLineInto line acc in readConsumeMapping acc' ts
    | otherwise = (ts, acc)

readMappingsRev :: ([String], [AocMap]) -> [AocMap]
readMappingsRev ([], acc) = acc
readMappingsRev (lines, acc) = let (lines', newMap) = readConsumeMapping empty lines in readMappingsRev (lines', newMap : acc)

readInput :: (String -> a) -> String -> (a, [AocMap])
readInput readSeedsFunc s = let lines = splitByNewLineOrdered s in
    (readSeedsFunc (head lines), reverse (readMappingsRev (drop 2 lines, [])))

readMappingLineInto :: String -> AocMap -> AocMap
readMappingLineInto line m = let parts = splitByOrdered (' ' ==) line in
    let (dst, src, range) = (read (head parts), read (parts !! 1), read (parts !! 2)) in
        insert (dst ,src, range) m

-- Processing

sendThroughMappings :: [Int] -> [AocMap] -> [Int]
sendThroughMappings = foldl (\ x m -> map (use m) x)

sendRangesThroughMapping :: [(Int, Int)] -> AocMap -> [(Int, Int)]
sendRangesThroughMapping = aux [] where
    aux :: [(Int, Int)] -> [(Int, Int)] -> AocMap -> [(Int, Int)]
    aux acc [] m = acc
    aux acc (h:ts) m = aux (useOnRange m h ++ acc) ts m

sendRangesThroughMappings :: [(Int, Int)] -> [AocMap] -> [(Int, Int)]
sendRangesThroughMappings = foldl sendRangesThroughMapping

rangesMinimum :: [(Int, Int)] -> Int
rangesMinimum = minimum . map fst

-- Parts

doPartA :: [Int] -> [AocMap] -> Int
doPartA seeds maps = minimum (sendThroughMappings seeds maps)

doPartB :: [(Int, Int)] -> [AocMap] -> Int
doPartB seedRanges maps = rangesMinimum (sendRangesThroughMappings seedRanges maps)

-- Main

mainFor :: String -> FilePath -> IO ()
mainFor name fn = do
    s <- readFile fn
    let (seeds, maps) = readInput readSeeds s
    let partA = doPartA seeds maps
    putStrLn ("Part A (" ++ name ++ "): " ++ show partA)
    let (seedRanges, maps) = readInput readSeedsAsRanges s
    let partB = doPartB seedRanges maps
    putStrLn ("Part B (" ++ name ++ "): " ++ show partB)

main :: IO ()
main = do
    mainFor "Test" testInputFileName
    mainFor "Main" inputFileName
