import Stream

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = go 1
  where go n        = Cons (compute n) $ go (n+1)
        divible n x = n `mod` x == 0
        compute n = pred . head . dropWhile (\x -> divible n (2^x)) $ [0..]

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) b = Cons a $ interleaveStreams b as

ruler' :: Stream Integer
ruler' = foldr1 interleaveStreams $ map streamRequest [0..]
