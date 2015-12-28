
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf           = -1
height (Node h _ _ _) = h

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where insert :: a -> (Tree a) -> (Tree a)
        insert a Leaf               = Node 0 Leaf a Leaf
        insert a (Node h lhs b rhs)
          | l <= r    = let lhs' = insert a lhs
                            h'   = height lhs'
                        in Node (1 + max h' r) lhs' b rhs
          | otherwise = let rhs' = insert a rhs
                            h'   = height rhs'
                        in Node (1 + max h' l) lhs b rhs'
          where l = height lhs
                r = height rhs
