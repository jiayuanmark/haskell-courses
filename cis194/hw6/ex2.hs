
fibs2 :: [Integer]
fibs2 = [0,1] ++ zipWith (+) fibs2 (tail fibs2)
