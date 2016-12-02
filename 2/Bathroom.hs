solve file = do
    input <- readFile file
    print $ getCode $ lines input

-- A positon on the keyboarcd from (0,0) to (2,2)
type Pos = (Int,Int)

keyPad1 = [[1,2,3],[4,5,6],[7,8,9]]

-- Takes a list of input and generates the key code for it.
getCode :: [String] -> [Int]
getCode = map toKeyCode . getPositons
    where
        toKeyCode (x,y) = keyPad1 !! y !! x

data Direction = North | East | South | West
    deriving (Show, Eq)

move (x,y) dir 
  | validPos m = m
  | otherwise = (x,y)
    where
        m = move' (x,y) dir

move' (x,y) dir 
    | dir == 'U' = (x, y-1)
    | dir == 'R' = (x+1, y)
    | dir == 'D' = (x, y+1)
    | dir == 'L' = (x-1, y)

validPos (x,y) = x >=0 && x <= 2 && y >=0 && y<=2

getPositons :: [String] -> [Pos]
getPositons = tail . reverse . 
    foldl (\(p:ps) s -> (findNewPosition p s):p:ps) [(1,1)]

-- Takes a starting point, an instruction string, and returns a new position.
findNewPosition :: Pos -> [Char] -> Pos
findNewPosition = foldl move 
