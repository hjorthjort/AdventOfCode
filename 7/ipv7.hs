import Data.Char

-- main = do
--     input <- "input.txt"
--     print $ parseId $ head input

solve1 = length . filter supportsTLS . lines

type HypSeq = String
type Id = String

supportsTLS = undefined

parse :: String -> ([Id], [HypSeq])
parse s = undefined

parseId [] = []
parseId s = firstId:parseId rest
    where
        (firstId, rest) = (takeWhile isAlpha s, dropWhile (==']') $ dropWhile isAlpha $ dropWhile (=='[') $ dropWhile isAlpha s)

parseHS [] = []
parseHS s 
  | all isAlpha s = []
  | otherwise = firstHS:parseHS rest
    where
        (firstHS, rest) = (takeWhile isAlpha $ dropWhile (=='[') $ dropWhile isAlpha s, dropWhile (==']') $ dropWhile isAlpha $ dropWhile (=='[') $ dropWhile isAlpha s)
