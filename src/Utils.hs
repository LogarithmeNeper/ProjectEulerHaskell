module Utils 
    ( fibonacci,
      fibonacci_sequence,
      prime_factors,
      is_even ) where

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibonacci_sequence :: [Int]
fibonacci_sequence = 0 : 1 : zipWith (+) fibonacci_sequence (tail fibonacci_sequence)

prime_factors :: Int -> [Int]
prime_factors n = prime_factors' n 2
  where
    prime_factors' 1 _ = []
    prime_factors' n f
      | n `mod` f == 0 = f : prime_factors' (n `div` f) f
      | otherwise      = prime_factors' n (f+1)
    
is_even :: Int -> Bool
is_even n = mod n 2 == 0