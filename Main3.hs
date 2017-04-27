module Lib
    ( someFunc
    ) where

import Data.List
import qualified System.IO.Streams as Streams


nQueens :: Int -> IO [[Int]]
nQueens size = 
    Streams.fromList (permutations [1..size]) >>=
    Streams.filter diagTest >>=
    Streams.toList


diagTest :: [Int] -> Bool
diagTest ls = 
    and . concat $ [dChk, dChk . reverse] <*> [ls]
    where
        dChk xs = map ((==1) . length) $ group $ sort $ zipWith (+) xs [1..]


someFunc :: IO ()
someFunc = 
    length <$> nQueens 11 >>= print

