{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Stream

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRequest 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRequest 0)
  negate        = streamMap negate
  (+) (Cons a as) (Cons b bs) = Cons (a + b) $ as + bs
  (*) (Cons a as) r@(Cons b bs) =
    let
      p1 = streamMap (* a) bs
      p2 = as * r
    in Cons (a * b) $ p1 + p2

instance Fractional (Stream Integer) where
  (/) l@(Cons a as) r@(Cons b bs) =
    let q  = a `div` b
        qp = streamMap (`div` b) $ as - (l / r) * bs
    in Cons q qp

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)
