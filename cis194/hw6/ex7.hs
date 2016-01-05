{-# OPTIONS_GHC -fno-warn-missing-methods #-}

data Matrix = Matrix Integer Integer Integer Integer
            deriving (Show, Eq)

instance Num Matrix where
  fromInteger n           = Matrix n n n n
  negate (Matrix a b c d) = Matrix (-a) (-b) (-c) (-d)
  (+) (Matrix a b c d) (Matrix e f g h) =
    Matrix (a+e) (b+f) (c+g) (d+h)
  (*) (Matrix a b c d) (Matrix e f g h) =
    let a' = a * e + b * g
        b' = a * f + b * h
        c' = c * e + d * g
        d' = c * f + d * h
    in Matrix a' b' c' d'

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = let m = Matrix 1 1 1 0
              (Matrix s _ _ _) = m^(n-1)
          in s

main :: IO ()
main = do
  n <- read <$> getLine
  putStrLn . show . length . show $ fibs4 n
