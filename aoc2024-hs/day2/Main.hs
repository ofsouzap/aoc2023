module Main where

import Utils (splitByOrdered, splitByNewLineOrdered, readFileLines, removeWhitespace)
import HsRegex (Pattern(..), removeStartingMatchLongest)
import Data.Maybe (fromMaybe, isNothing, catMaybes)

inputFileName :: String
inputFileName = "inputs/day2.txt"

testInputFileName :: String
testInputFileName = "inputs/day2-test.txt"

data Color = Red | Green | Blue

data Draw = Draw {
    reds :: Int,
    greens :: Int,
    blues :: Int
}

instance Show Draw where
    show ds = "{r=" ++ show (reds ds) ++ ",g=" ++ show (greens ds) ++ ",b=" ++ show (blues ds) ++ "}"

data Game = Game {
    gid :: Int,
    draws :: [Draw]
}

instance Show Game where
    show g = "[gid=" ++ show (gid g) ++ "]" ++ show (draws g)

emptyDraw :: Draw
emptyDraw = Draw {reds=0, greens=0, blues=0}

-- Reading input

-- |Take a string of form "Game n" and extract n as an integer
extractGameId :: String -> Int
extractGameId = read . fromMaybe "" . removeStartingMatchLongest (String "Game") . removeWhitespace

updateDraw :: Maybe Draw -> String -> Maybe Draw
updateDraw d' s = case d' of
    Nothing -> Nothing
    Just d -> case removeStartingMatchLongest (Star DigitChar) (removeWhitespace s) of
        Just color -> let n = (read . take (length s - length color)) s in
            case color of
                "red" -> Just (d {reds=n})
                "green" -> Just (d {greens=n})
                "blue" -> Just (d {blues=n})
                _ -> Nothing
        _ -> Nothing

-- |Take a single draw data (no ';') and extract the draw data
extractSingleDraw :: String -> Maybe Draw
extractSingleDraw = foldl updateDraw (Just emptyDraw) . splitByOrdered (',' ==) . removeWhitespace

-- |Take the draws in a game, separated by ';'s and extract the draw datas
extractGameDraws :: String -> [Maybe Draw]
extractGameDraws = map extractSingleDraw . splitByOrdered (';' ==) . removeWhitespace

lineToGame :: String -> Maybe Game
lineToGame line = let parts = splitByOrdered (':' ==) line in
    let gid = extractGameId (head parts) in
        let draws = extractGameDraws (parts !! 1) in
            if any isNothing draws then Nothing else Just (Game {gid=gid, draws=catMaybes draws})

readGames :: String -> [Maybe Game]
readGames = map lineToGame . splitByNewLineOrdered

enforceMaybeGamesRev :: [Maybe Game] -> Maybe [Game]
enforceMaybeGamesRev = foldl f (Just []) where
    f Nothing _ = Nothing
    f (Just xs) Nothing = Nothing
    f (Just xs) (Just x) = Just (x:xs)

tryReadGames :: String -> Maybe [Game]
tryReadGames s = (enforceMaybeGamesRev . readGames) s >>= Just . reverse

-- Computation

doPartA :: [Game] -> Int
doPartA = sum . map gid . filter condGame where
    condGame = all condDraw . draws
    condDraw g = (reds g <= 12) && (greens g <= 13) && (blues g <= 14)

doPartB :: [Game] -> Int
doPartB = sum . map (cubeSetPow . minCubeSet) where
    minCubeSet :: Game -> Draw
    minCubeSet = foldl foldFunc emptyDraw . draws

    foldFunc :: Draw -> Draw -> Draw
    foldFunc curr x = Draw
        { reds=max (reds curr) (reds x),
          greens=max (greens curr) (greens x),
          blues=max (blues curr) (blues x) }

    cubeSetPow :: Draw -> Int
    cubeSetPow d = reds d * greens d * blues d

-- doPartB :: [Game] -> Int

-- Main

-- x = (splitByOrdered (';' ==) . removeWhitespace) " 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
-- y = extractSingleDraw "3blue,4red"
-- str = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
-- \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
-- \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
-- \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
-- \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
-- z = fromMaybe [] (tryReadGames str)

mainFor :: String -> FilePath -> IO ()
mainFor name fn = do
    lines <- readFile fn
    case tryReadGames lines of
        Nothing -> putStrLn "Couldn't read game"
        Just gs -> do
            let partA = doPartA gs
            putStrLn ("Part A (" ++ name ++"): " ++ show partA)
            let partB = doPartB gs
            putStrLn ("Part B (" ++ name ++"): " ++ show partB)

main :: IO ()
main = do
    mainFor "Test" testInputFileName
    mainFor "Main" inputFileName
