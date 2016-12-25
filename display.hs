import Data.Char
import Data.List

updateDisplay (instruction, (x,y)) = instruction x y display

type Display = [[Maybe ()]]

display :: [[Maybe ()]]
display = replicate 50 (replicate 6 Nothing)

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

rect x y d = -- List comprehension
row rowNum sh d = transpose $ (\c ->  d' !!= (sh, c))  $ shift sh $ d' !! rowNum 
    where
        d' = transpose d
col colNum sh d = (\c ->  d !!= (sh, c))  $ shift sh $ d !! colNum 

shift steps list = take (length list) $ drop steps $ cycle list

list !!= (pos, elem) = take pos list ++ elem:drop (pos + 1) list
