import Data.List.Split(splitOn)
import Data.List(delete)

main :: IO ()
main = do
    input1 <- readFile "taxi1.txt"
    input2 <- readFile "taxi2.txt"
    print $ ("Part 1: " ++) $ show $ solve1 $ filter (/='\n') input1
    print $ ("Part 2: " ++) $ show $ solve2 $ filter (/='\n') input2

parse :: String -> [(Char, Int)]
parse = map toDir . splitOn ", "
    where
        toDir (h:i) = (h, read i)

data Direction = North | East | South | West
    deriving (Show, Eq)

type Pos = (Int, Int)

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

solve1 :: String -> Int
solve1 = solve1' (0,0) North . parse

solve1' (x,y) _ [] = abs x + abs y
solve1' pos dir (a:as) = solve1' pos' dir' as
    where
        lr = fst a
        n = snd a
        dir' = if lr == 'R' then succ' dir else pred' dir
        pos' = val dir' pos n

-- Part 2
-- Gets all the visited positions, finds duplicates, gets the first and
-- returns its distance.
solve2 :: String -> Int
solve2 = (\(x,y) -> abs x + abs y) . head . dups . solve2' (0,0) North [] . parse

-- Returns all visited positions in order.
solve2' _ _ visited [] = visited
solve2' pos@(x,y) dir visited (a:as)
  = solve2' pos' dir' (visited ++ (delete pos' $ visits pos pos')) as
    where
        lr = fst a
        n = snd a
        dir' = if lr == 'R' then succ' dir else pred' dir
        pos' = val dir' pos n

-- Returns all positions between two positions, incusive. Assumes only one of
-- the coordinates differ between the positions.
visits :: Pos -> Pos -> [Pos]
visits (x, y) (x', y')
  | x == x' = [ (x,j) | j <- [(min y y')..(max y y')] ]
  | y == y' = [ (i,y) | i <- [(min x x')..(max x x')] ]

-- Naive function that removes the last occurence of an element in a list
dups :: Eq a => [a] -> [a]
dups [] = []
dups (a:as) | a `elem` as = a:(dups as)
            | otherwise = dups as
