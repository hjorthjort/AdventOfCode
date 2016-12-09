import Data.Char


parse = parse' . words

parse' ("rect":[x]) = parseRect x
parse' ("rotate":"row ":rest) = undefined
parse' ("rotate":"column ":rest) = undefined
 
parseRect :: String -> (Int, Int)
parseRect s = (read first, read second)
    where
        first = takeWhile isDigit s
        second = takeWhile isDigit $ tail $ dropWhile isDigit s

