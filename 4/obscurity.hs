import Data.List
import Data.Char

main = do
    input <- readFile "input.txt"
    print "Part 1"
    print $ part1 input
    print $ find (\r -> name r == "northpolefobjectfstorage") $ part2 input

data Room = Room {name :: String, rId :: Int, cs :: String} deriving Show

toRoom :: String -> Room
toRoom s = Room (parseName s) (parseId s) (parseChecksum s)
    where
        parseName =  reverse . tail . reverse . takeWhile (not . isDigit)
        parseId = read . takeWhile (isDigit) . dropWhile (not . isDigit)
        parseChecksum = takeWhile (/=']') . tail . dropWhile (/='[')

checksum :: String -> String
checksum = take 5 . map head . reverse . sortOn length . group . reverse .
    sort . filter isAlpha

filterFalseRooms = filter (\r -> cs r == checksum (name r))

-- Part 1

-- 1. Turn input into rooms
-- 2. Filter false rooms
-- 3. Sum ids
part1 = sum . map rId . -- Sum id:s
        filterFalseRooms . 
        map toRoom . lines --Turn into list of rooms

-- Part 2

rotate :: Room -> Room
rotate (Room n i c) = Room (n `rotateBy` i) i c

rotateBy :: String -> Int -> String
rotateBy s i = map (letters!!) $ map (+(i-97)) $ map ord s
    where
        letters = cycle ['a'..'z']

part2 = map rotate . filterFalseRooms . 
        map toRoom . lines --Turn into list of rooms

