-- | Requires the "MissingH-1.4.0.1" Hackage package.
import Data.Char
import Data.Hash.MD5
import Data.Maybe

main = do
    -- print getPassword1
    print getPassword2

prefix = replicate 5 '0'
myInput = "ffykfhsq"
-- My input followed by all possible indexes, as MD5 class data types
allInputs = map (Str . (\i -> myInput ++ show i)) [0..]

getPassword1 = take 8 $ map (!!5) $ filter (\s -> take 5 s == prefix) $ map md5s allInputs

-- For positions 0 through 7, find the first occurence
getPassword2 = getPassword2' allPositions (replicate 8 Nothing)

getPassword2' :: [(Int, Char)] -> [Maybe Char] -> String
getPassword2' _ acc | not (Nothing `elem` acc) = map fromJust acc
getPassword2' ((p,c):xs) pass
  | isNothing (pass !! p) = getPassword2' xs (insertAt p (Just c) pass)
  | otherwise = getPassword2' xs pass
      where
          insertAt i e l = take i l ++ [e] ++ drop (i+1) l
  
-- Infinite list of the interesting characters and their positions.
allPositions =
    map (\(p:c:s) -> (digitToInt p, c)) $
    filter (\(h:t) -> h `elem` ['0'..'7'] ) $ 
    map (drop 5) $
    filter (\s -> take 5 s == prefix) $
    map md5s allInputs
