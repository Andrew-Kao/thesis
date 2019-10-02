import System.IO

import Data.List
import Data.Char
import Data.String


main :: IO ()
main = readFile "../../Data/migration/ctyxcty_hisp_us0610.txt" >>= writeFile "../../Data/migration/ctyxcty_hisp_us0610Clean.txt" -- 
    . clean

-- | Clean the data
clean :: String -> String
clean = unlines . map dropMid . lines

-- | want list of strings
dropMid :: String -> String
dropMid = unwords . firstL2 . words

-- | drop everything but the initial number and the last two 
firstL2 :: [String] -> [String]
firstL2 strings
    | length strings <= 4 = strings
    | otherwise = firstL2 . kill3rdWord $ strings

-- | Goes from string and index of character to drop to string sans character
kill3rdWord :: [String] -> [String]
kill3rdWord strings = (fst $ splitAt 2 strings) ++ (tail . snd $ splitAt 2 strings)



