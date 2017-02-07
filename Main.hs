module Main where

import Data.List


nQueens :: Int -> [[Int]]
nQueens size = 
    filter diagTest $ permutations [1..size]


diagTest :: [Int] -> Bool
diagTest ls = 
    and . concat $ [dChk, dChk . reverse] <*> [ls]
    where
        dChk xs = map ((==1) . length) $ group $ sort $ zipWith (+) xs [1..]


main :: IO ()
main = 
    print $ map (length . nQueens) [1..11]
