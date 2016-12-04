import Data.List

main = do
    input <- readFile "input.txt"
    let triangles = map ( sort . (map read ) . words) $ lines input
        printable = [x !! 0 + x !! 1 + x !! 2 | x <- triangles]

    print $ length $  possibles triangles

possibles :: [[Integer]] -> [[Integer]]
possibles = filter (\(a:b:c:[]) -> abs a + abs b > abs c)
