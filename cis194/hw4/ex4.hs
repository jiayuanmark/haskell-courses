import Data.List (sort)

sieve :: (Ord a) => [a] -> [a] -> [a]
sieve l@(a:as) r@(b:bs)
  | a < b     = a : sieve as r
  | a > b     = sieve l bs
  | otherwise = sieve as bs
sieve a [] = a
sieve [] _ = []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+ 1) . (* 2)) rmd
  where tp = [ i+j+2*i*j | j <- [1..n], i <- [1..j] ]
        rmv  = sort $ filter (<= n) tp
        rmd  = sieve [1..n] rmv
