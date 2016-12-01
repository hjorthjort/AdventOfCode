import Data.List.Split(splitOn)

main :: IO String
main = do
    input <- readFile "taxi.txt"

    return $ show $ solve $ filter (/='\n') input

solve :: String -> Int
solve s = undefined

data Direction = L | R deriving (Show, Eq)

parse :: String -> [(Direction, Int)]
parse = map toDir . splitOn ", "
    where
        toDir ('R':i) = (R, read i)
        toDir ('L':i) = (L, read i)
