-- | Requires the "MissingH-1.4.0.1" Hackage package.
import Data.Char
import Data.Hash.MD5
import Data.Maybe

main = do
    print getPassword1
    -- For the love of God, compile this with all optimizations ON.
    -- getPassword2 looks through the list of possible hashes from the start
    -- for every position in the password. This is very readable and simple,
    -- but also naive. Make sure you let GHC memoize properly by tunring the
    -- optimizations on.
    print getPassword2

prefix = replicate 5 '0'
myInput = "ffykfhsq"
-- My input followed by all possible indexes, as MD5 class data types
allInputs = map (Str . (\i -> myInput ++ show i)) [0..]

getPassword1 = take 8 $ map (!!5) $ filter (\s -> take 5 s == prefix) $ map md5s allInputs

-- For positions 0 through 7, find the first occurence
getPassword2 = map (\pos -> fromJust $ lookup pos allPositions) [0..7]

-- Infinite list of the interesting characters and their positions.
allPositions =
    map (\(p:c:s) -> (digitToInt p, c)) $
    filter (\(h:t) -> h `elem` ['0'..'7'] ) $ 
    map (drop 5) $
    filter (\s -> take 5 s == prefix) $
    map md5s allInputs
