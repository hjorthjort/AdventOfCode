import Data.List
import Data.List.Split

main = do
    input <- readFile "input.txt"
    let triangles = map ( (map read ) . words) $ lines input
        triangles1 = map sort triangles
        triangles2 = map sort $ chunksOf 3 $ concat $ transpose triangles
        printable = [x !! 0 + x !! 1 + x !! 2 | x <- triangles1]

    print $ length $ possibles triangles1
    print $ length $ possibles triangles2

possibles :: [[Integer]] -> [[Integer]]
possibles = filter (\(a:b:c:[]) -> abs a + abs b > abs c)
