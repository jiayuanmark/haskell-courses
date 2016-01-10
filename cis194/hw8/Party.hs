{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2 = case (l1 `compare` l2) of
  GT        -> l1
  otherwise -> l2
