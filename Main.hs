module Main where

import Data.List


nQueens :: Int -> [[Int]]
nQueens size =
    permutations [1..size]


main :: IO ()
main = do
    putStrLn "What size board would you like to solve for?"
    size <- readLn
    print (size :: Int)