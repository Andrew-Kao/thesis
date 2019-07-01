import System.IO

import Data.List
import Data.Char


main :: IO ()
main = readFile "../../Data/instrument/all-cdbs-files/ownership_structure.dat" >>= writeFile "../../Data/instrument/all-cdbs-files/ownership_structure_clean.dat"
    . clean

-- | Clean the data
clean :: String -> String
clean = unlines . map fix50 . lines

-- | Checks to make sure it has 49 instances of the delim '|' If not (there are 50), gets rid of the 3rd to last one
fix50 :: String -> String
fix50 line
    | countPipes line 0 == 49 = line
    | otherwise = cleanPipe line

-- | Counts the number of '|' in the string
countPipes :: String -> Int -> Int
countPipes [] n = n
countPipes (a:as) n
    | a == '|' = countPipes as (n+1)
    | otherwise = countPipes as n

-- | Kills the third to last pipe as many times as is necessary to bring us to 49 pipes/50 fields
-- *should* be lossless
cleanPipe :: String -> String
cleanPipe as
    | countPipes as 0 > 49 = cleanPipe . reverse . kill3rdPipe . reverse $ as
    | otherwise = as

-- | Kills the third pipe
kill3rdPipe :: String -> String
kill3rdPipe = killChar . get3rdLoc 0 0

-- | Goes from string and index of character to drop to string sans character
killChar :: (String, Int) -> String
killChar (as, n) = (fst $ splitAt n as) ++ (tail . snd $ splitAt n as)

-- | Gets location of the third pipe
-- We don't worry about exceptions like less than 3 pipes etc. because data shouldn't fail like that
get3rdLoc :: Int -> Int -> String -> (String, Int)
get3rdLoc nchar npipes as
    | nchar + 1 > length as = (as,0)
    | npipes == 3 = (as, nchar)
    | as !! nchar == '|' = get3rdLoc (nchar + 1) (npipes + 1) as
    | otherwise = get3rdLoc (nchar + 1) npipes as