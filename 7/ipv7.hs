import Data.Char
import Data.List.Split

-- main = do
--     input <- "input.txt"
--     print $ parseId $ head input

solve1 = length . filter supportsTLS . lines

type HypSeq = String
type Id = String

supportsTLS = undefined

-- parse :: String -> ([Id], [HypSeq])
parse = (\l -> (evens l, odds l)) . splitOneOf ['[',']']

evens (i:i':is) = i:evens is
evens (i:is) = [i]
evens _ = []

odds [] = []
odds l = evens $ tail l
