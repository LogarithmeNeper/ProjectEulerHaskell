module Lib
    ( pb1,
      pb2
    ) where

import Utils

pb1 :: Int
pb1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

pb2 :: Int
pb2 = sum $ filter is_even $ takeWhile (<4000000) fibonacci_sequence