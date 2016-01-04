module Stream where

-- ex3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- ex4
streamRequest :: a -> Stream a
streamRequest a = Cons a (streamRequest a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f (f a)
