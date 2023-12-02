module Main where

import Utils (splitByOrdered, splitByNewLineOrdered, readFileLines)

inputFileName :: String
inputFileName = "inputs/day2.txt"

testAInputFileName :: String
testAInputFileName = "inputs/day2-testa.txt"

-- testBInputFileName :: String
-- testBInputFileName = "inputs/day2-testb.txt"

data Color = Red | Green | Blue

data Draw = Draw {
    reds :: Int,
    greens :: Int,
    blues :: Int
}

data Game = Game {
    gid :: Int,
    draws :: [Draw]
}

readDraw :: String -> Draw
readDraw _ = Draw { reds = 0, greens = 0, blues = 0} -- TODO

extractGameId :: String -> Int
extractGameId _ = 0 -- TODO

extractGameDraws :: String -> [Draw]
extractGameDraws _ = [] -- TODO

lineToGame :: String -> Game
lineToGame line = let parts = splitByOrdered (':' ==) line in
    Game {
        gid = extractGameId (head parts),
        draws = extractGameDraws (parts !! 1)
    }

readGames :: String -> [Game]
readGames = map lineToGame . splitByNewLineOrdered

-- Main

mainFor :: String -> FilePath -> IO ()
mainFor name fn = do
    lines <- readFileLines fn
    putStrLn "" -- TODO

main :: IO ()
main = do
    mainFor "Test A" testAInputFileName
    -- mainFor "Test B" testBInputFileName
    mainFor "Main" inputFileName
