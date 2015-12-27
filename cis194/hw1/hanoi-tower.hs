import Data.Function (on)
import Data.List (minimumBy)

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = m1 ++ ((a,b):m2)
  where m1 = hanoi (n-1) a c b
        m2 = hanoi (n-1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a,b)]
hanoi4 n a b c d = minimumBy (compare `on` length) lst
  where lst = map f [1..n-1]
        f k = let m1 = hanoi4 k a c b d
                  m2 = hanoi (n - k) a b d
                  m3 = hanoi4 k c b a d
              in m1 ++ m2 ++ m3
