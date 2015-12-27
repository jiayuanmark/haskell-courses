module Golf where

import Data.List

skips :: [a] -> [[a]]
skips l  = map skip' [1..length l]
  where skip' k = map fst . filter (f k) $ zip l [1..]
        f k x   = (snd x) `mod` k == 0

localMaxima :: [Integer] -> [Integer]
localMaxima []       = []
localMaxima l@(_:ns) = map snd . filter fst $ zip mx l
  where gt = (zipWith (>) l ns) ++ [False]
        lt = False:(zipWith (<) l ns)
        mx = zipWith (&&) gt lt

histogram :: [Integer] -> String
histogram ns = (unlines . reverse . plot $ ct) ++ "==========\n0123456789\n"
  where ct      = map (\l@(x:xs) -> (x, length l)) . group . sort $ ns
        draw x  = let d n = if n `elem` x then '*' else ' '
                  in map d [0..9]
        plot [] = []
        plot n  = let rst = filter ((> 0) . snd) $ map (\(x,y) -> (x,y-1)) n
                  in (draw $ map fst n) : (plot rst)
