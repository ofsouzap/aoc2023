module Utils (
    splitByRev,
    splitByOrdered,
    splitByNewLineRev,
    splitByNewLineOrdered,
    readFileLines,
    removeWhitespace,
) where

-- Splitting

-- |Split a string into a list of strings, splitting on characters fulfilling a provided predicate. Output is reversed
splitByRev :: (Char -> Bool) -> String -> [String]
splitByRev f = (\ (yacc, ys) -> reverse yacc : ys) . foldl g ("", []) where
    g (yacc, ys) x = if f x then ("", reverse yacc : ys) else (x:yacc, ys)

splitByOrdered :: (Char -> Bool) -> String -> [String]
splitByOrdered f = reverse . splitByRev f

splitByNewLineRev :: String -> [String]
splitByNewLineRev = splitByRev ('\n' ==)

splitByNewLineOrdered :: String -> [String]
splitByNewLineOrdered = splitByOrdered ('\n' ==)

readFileLines :: FilePath -> IO [String]
readFileLines fn = do
    contents <- readFile fn
    return (splitByNewLineOrdered contents)

-- Formatting

whitespace :: [Char]
whitespace = " \t\r\n"

removeWhitespace :: String -> String
removeWhitespace = filter (`elem` whitespace)
