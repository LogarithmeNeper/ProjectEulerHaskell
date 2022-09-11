module Utils where

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibonacci_sequence :: [Int]
fibonacci_sequence = 0 : 1 : zipWith (+) fibonacci_sequence (tail fibonacci_sequence)

is_even :: Int -> Bool
is_even n = n `mod` 2 == 0