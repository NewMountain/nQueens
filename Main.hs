module Main where

import Data.List


nQueens :: Int -> [[Int]]
nQueens size = 
    filter diagTest $ permutations [1..size]


diagTest :: [Int] -> Bool
diagTest ls = 
    and $ northEast ++ southEast
    where
        northEast = checkCoords $ zipWith (+) ls [1..]
        southEast = checkCoords $ zipWith (+) (reverse ls) [1..]    


checkCoords :: [Int] -> [Bool]
checkCoords xs = 
    map ((==1) . length) $ group $ sort xs


main :: IO ()
main = 
    print $ map (length . nQueens) [1..11]
