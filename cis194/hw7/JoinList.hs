{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.List (foldr1)
import Buffer
import Scrabble
import Sized

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

type JoinListBuffer = JoinList (Score, Size) String

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) lhs rhs = Append (l `mappend` r) lhs rhs
  where l = tag lhs
        r = tag rhs

-- Test code
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- ex2
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single m a)
  | n == 0    = Just a
  | otherwise = Nothing
indexJ n (Append m lhs rhs)
  | n < 0     = Nothing
  | n >= sz   = Nothing
  | n < lsize = indexJ n lhs
  | otherwise = indexJ (n-lsize) rhs
  where sz    = getSize . size $ m
        lsize = getSize . size . tag $ lhs

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty           = Empty
dropJ n jl | n <= 0     = jl
dropJ n (Append m lhs rhs)
  | n >= sz    = Empty
  | n < lsize  = (dropJ n lhs) +++ rhs
  | otherwise  = dropJ (n-lsize) rhs
  where sz    = getSize . size $ m
        lsize = getSize . size . tag $ lhs

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty           = Empty
takeJ n _ | n <= 0      = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n jl@(Append m lhs rhs)
  | n >= sz    = jl
  | n <= lsize = takeJ n lhs
  | otherwise = lhs +++ (takeJ (n-lsize) rhs)
  where sz    = getSize . size $ m
        lsize = getSize . size . tag $ lhs

instance Buffer JoinListBuffer where
  -- | toString
  toString = unlines . jlToList

  -- | fromString
  fromString = foldr1 (+++) . map f . lines
    where f = \l -> (Single (scoreString l, Size 1) l)

  -- | line
  line = indexJ

  -- | replaceLine
  replaceLine ix s b
    | ix < 0 || ix >= (numLines b) = b
    | otherwise = lhs +++ mid +++ rhs
    where mid = fromString s
          lhs = takeJ ix b
          rhs = dropJ (ix + 1) b

  -- | numLines
  numLines = getSize . size . tag

  -- | value
  value b = let Score s = fst . tag $ b
            in s

-- ex2 test code
testJ :: Int -> Bool
testJ ix = let
  makeSingleton :: Int -> JoinList Size Int
  makeSingleton = Single (Size 1)

  makeList :: [Int] -> JoinList Size Int
  makeList = foldl (+++) Empty . map makeSingleton

  testcase = [1..100]
  jl       = makeList testcase
  in (jlToList jl == testcase) &&
     (indexJ ix jl) == (jlToList jl !!? ix) &&
     (jlToList (dropJ ix jl) == drop ix (jlToList jl)) &&
     (jlToList (takeJ ix jl) == take ix (jlToList jl))

-- ex3 test code
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
