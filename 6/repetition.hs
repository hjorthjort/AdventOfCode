import Data.List

main = do
    input <- readFile "input.txt"
    print $ solvePart1 input
    print $ solvePart2 input

solvePart1 = map mostCommonLetter . transpose . lines
solvePart2 = map leastCommonLetter . transpose . lines

mostCommonLetter = head . head . reverse . groupAndSort
leastCommonLetter = head . head . groupAndSort

groupAndSort = sortOn length . group . sort

