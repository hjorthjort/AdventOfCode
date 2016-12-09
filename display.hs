import Data.Char

parse = parse' . words

parse' ("rect":[x]) = (rect, parseRect x)
parse' ("rotate":"row":rest) = (row, parseRC rest)
parse' ("rotate":"column":rest) = (col, parseRC rest)
 
parseRect :: String -> (Int, Int)
parseRect s = (read first, read second)
    where
        first = takeWhile isDigit s
        second = takeWhile isDigit $ tail $ dropWhile isDigit s

parseRC (('y':'=':row):_:rot:[]) = (read row, read rot)

rect = 0
row = 1
col = 2
