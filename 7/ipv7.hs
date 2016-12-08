import Data.Char
import Data.List
import Data.List.Split

main = do
    input <- readFile "input.txt"
    print $ solve1 input

solve1 :: String -> Int
solve1 = length . filter supportsTLS . lines

type HypSeq = String
type Id = String

supportsTLS = supportsTLS' . parse

supportsTLS' (ids, hs) = any hasPalin4s ids && (not $ any hasPalin4s hs)

-- parse :: String -> ([Id], [HypSeq])
parse = (\l -> (evens l, odds l)) . splitOneOf ['[',']']

evens (i:i':is) = i:evens is
evens (i:is) = [i]
evens _ = []

odds [] = []
odds l = evens $ tail l

groups size l = take (length l - size + 1) $ map (take size) $ tails l

hasPalin4s :: String -> Bool
hasPalin4s = any isPalin4 . fours
    where
        isPalin4 (a:b:c:d:[]) = a == d && b == c && a /= b
        fours = groups 4
