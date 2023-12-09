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

-- useOnRange ((dst,src,mRange):ts) x@(xStart,xRange)
--     -- Input range end before map range start
--     | xStart + xRange - 1 < src = useOnRange ts x
--     -- Input range start after map range end
--     | xStart > src + mRange - 1 = useOnRange ts x
--     -- Input range fully in map range
--     | xStart >= src && xStart + xRange - 1 <= src + mRange - 1 = let diff = xStart - src in [(dst+diff, xRange)]
--     -- Map range fully in input range
--     | xStart < src && xStart + xRange - 1 > src + mRange - 1 = within : before ++ after where (
--         before = useOnRange ts (xStart, src-1)
--         after = useOnRange ts (dst+1, xStart+xRange-1)
--         within = (dst, dst+mRange-1) )
--     -- Input range before and within map range
--     | xStart < src &&
--     -- Input range within and after map range
--     -- TODO
--     -- (For helping me find errors)
--     | otherwise = [(-1, -1)]

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

x = "seeds: 79 14 55 13\n\
\\n\
\seed-to-soil map:\n\
\50 98 2\n\
\52 50 48\n\
\\n\
\soil-to-fertilizer map:\n\
\0 15 37\n\
\37 52 2\n\
\39 0 15\n\
\\n\
\fertilizer-to-water map:\n\
\49 53 8\n\
\0 11 42\n\
\42 0 7\n\
\57 7 4\n\
\\n\
\water-to-light map:\n\
\88 18 7\n\
\18 25 70\n\
\\n\
\light-to-temperature map:\n\
\45 77 23\n\
\81 45 19\n\
\68 64 13\n\
\\n\
\temperature-to-humidity map:\n\
\0 69 1\n\
\1 0 69\n\
\\n\
\humidity-to-location map:\n\
\60 56 37\n\
\56 93 4"

(seeds, maps) = readInput readSeeds x

(seedRanges, _) = readInput readSeedsAsRanges x

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
