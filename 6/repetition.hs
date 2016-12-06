import Data.List

main = do
    input <- readFile "input.txt"
    print $ solvePart1 input

solvePart1 = map mostCommonLetter . transpose . lines

mostCommonLetter = head . head . reverse . sortOn length . group . sort

