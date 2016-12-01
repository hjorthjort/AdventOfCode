import Data.List.Split(splitOn)

main :: IO ()
main = do
    input <- readFile "taxi.txt"

    print $ solve $ filter (/='\n') input

parse :: String -> [(Char, Int)]
parse = map toDir . splitOn ", "
    where
        toDir (h:i) = (h, read i)

data Direction = North | East | South | West
    deriving (Show, Eq)

succ' West = North
succ' North = East
succ' East = South
succ' South = West
pred' North = West
pred' East = North
pred' South = East
pred' West = South

val dir (x,y) n
    | dir == North = (x, y+n)
    | dir == East = (x+n, y)
    | dir == South = (x, y-n)
    | dir == West = (x-n, y)

solve :: String -> Int
solve = solve' (0,0) North . parse

solve' (x,y) _ [] = abs x + abs y
solve' pos dir (a:as) = solve' pos' dir' as
    where
        lr = fst a
        n = snd a
        dir' = if lr == 'R' then succ' dir else pred' dir
        pos' = val dir' pos n
