{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.Array
import Data.List (sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

play :: [DieValue] -> [DieValue] -> (Int, Int)
play attackDie defendDie = (length zp - n, n)
  where f  = sortBy (flip compare)
        zp = zipWith (compare) (f attackDie) (f defendDie)
        n  = length . filter (== GT) $ zp

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do
  attack <- replicateM na die
  defend <- replicateM nd die
  let (da, dd) = play attack defend
  return $ Battlefield (a-da) (d-dd)
  where na = min 3 a - 1
        nd = min 2 d

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
  | d == 0 || a < 2 = return b
  | otherwise       = battle b >>= invade

isAttackWin :: Battlefield -> Rand StdGen Bool
isAttackWin initial = do
  (Battlefield a d) <- invade initial
  return (a >= 2)

successProb :: Battlefield -> Rand StdGen Double
successProb initial = do
  res <- replicateM 100000 (isAttackWin initial)
  return . calc $ res
  where calc r = let win = length . filter id $ r
                 in fromIntegral win / 100000.0

exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield a d) = prob ! (a,d)
  where prob   :: Array (Int, Int) Double
        prob   = array ((0,0),(a,d)) [ ((i,j),go i j) | i <- [0..a],
                                                        j <- [0..d] ]
        go a d
          | a < 2     = 0
          | d == 0    = 1
          | d < 2     = (prob ! (a, d-1) + prob ! (a-1, d)) / 2.0
          | otherwise = sum [ prob ! (a-1,d-1),
                              prob ! (a-2,d),
                              prob ! (a,d-2) ] / 3.0
