import Data.Maybe

main = do
    solve "input1.txt"

solve file = do
    input <- readFile file
    print $ getCode1 $ lines input
    print $ getCode2 $ lines input

-- A positon on the keyboarcd from (0,0) to (2,2)
type Pos = (Int,Int)

keyPad1 = [[1,2,3],[4,5,6],[7,8,9]]

-- Takes a list of input and generates the key code for it.
getCode1 = map toKeyCode . getPositons1
    where
        toKeyCode (x,y) = keyPad1 !! y !! x

data Direction = North | East | South | West
    deriving (Show, Eq)

move1 (x,y) dir 
  | validPos1 m = m
  | otherwise = (x,y)
    where
        m = move' (x,y) dir

validPos1 (x,y) = x >=0 && x <= 2 && y >=0 && y<=2

getPositons1 = tail . reverse . 
    foldl (\(p:ps) s -> (findNewPosition p s):p:ps) [(1,1)]
        where
            findNewPosition = foldl move1 

-- Takes a starting point, an instruction string, and returns a new position.

-- Part 2
keyPad2 = [
            [Nothing, Nothing, Just 1, Nothing, Nothing],
            [Nothing, Just 2, Just 3, Just 4, Nothing],
            map Just [5..9],
            [Nothing, Just 10, Just 11, Just 12, Nothing],
            [Nothing, Nothing, Just 13, Nothing, Nothing]
          ]

getCode2 = map toKeyCode . getPositons2
    where
        toKeyCode (x,y) = keyPad2 !! y !! x

move2 (x,y) dir 
  | validPos2 m = m
  | otherwise = (x,y)
    where
        m = move' (x,y) dir

validPos2 (x,y) 
  -- Check that the index is valid.
  | y < 0 || y >= length keyPad2 || x < 0 || x >= length (keyPad2 !! y) = False
  | otherwise = isJust $ keyPad2 !! y !! x

getPositons2 = tail . reverse . 
    foldl (\(p:ps) s -> (findNewPosition p s):p:ps) [(2,2)]
        where
            findNewPosition = foldl move2 

--- COMMON
move' (x,y) dir 
    | dir == 'U' = (x, y-1)
    | dir == 'R' = (x+1, y)
    | dir == 'D' = (x, y+1)
    | dir == 'L' = (x-1, y)

