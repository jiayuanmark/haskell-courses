
xor :: [Bool] -> Bool
xor = foldr f False
  where f False t   = t
        f True True = False
        f True _    = True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr f' []
  where f' x y = (f x) : y

-- foldl doesn't have to work with infinite list
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- tricky rewrite
-- https://wiki.haskell.org/Foldl_as_foldr_alternative
myFoldl' :: (a -> b -> a) -> a -> [b] -> a
myFoldl' f base xs = foldr (\x r a -> r (f a x)) id xs base
