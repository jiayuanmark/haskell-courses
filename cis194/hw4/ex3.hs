
xor :: [Bool] -> Bool
xor = foldr f False
  where f False t   = t
        f True True = False
        f True _    = True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr f' []
  where f' x y = (f x) : y

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base xs
