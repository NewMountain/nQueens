module Main where

import Data.List
import Data.List.Split
import Control.Exception
import System.Environment
import Control.Parallel.Strategies
import Control.DeepSeq

-- Based shamelessly on the Simon Marlow Sudoku solver
-- http://chimera.labs.oreilly.com/books/1230000000929/ch02.html#sec_par-eval-whnf


nQueens :: Int -> [[Int]]
nQueens size = 
    filter diagTest $ permutations [1..size]


diagTest :: [Int] -> Bool
diagTest ls = 
    and . concat $ [dChk, dChk . reverse] <*> [ls]
    where
        dChk xs = map ((==1) . length) $ group $ sort $ zipWith (+) xs [1..]

main :: IO ()
main = do
    putStrLn "Please enter a number here: "
    
    num <- getLine

    let inputs = permutations [1..(read num :: Int)]

        subsetSizes = (length inputs `div` 4)

        [as, bs, cs, ds] = divvy subsetSizes subsetSizes inputs

        solutions = runEval $ do
                        as' <- rpar (force (filter diagTest as))
                        bs' <- rpar (force (filter diagTest bs))
                        cs' <- rpar (force (filter diagTest cs))
                        ds' <- rpar (force (filter diagTest ds))
                        rseq as'
                        rseq bs'
                        rseq cs'
                        rseq ds'
                        return (as' ++ bs' ++ cs' ++ ds') 

    print $ length solutions
