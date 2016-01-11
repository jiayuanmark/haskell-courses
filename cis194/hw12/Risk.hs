{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
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

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do
  ad <- replicateM na die
  sd <- replicateM nb die
  let (da, dd) = play ad sd
  return (Battlefield (a-da) (d-dd))
  where na       = min 3 na - 1
        nb       = min 2 nb
        play :: [DieValue] -> [DieValue] -> (Int, Int)
        play a d = let f  = sortBy (flip compare)
                       zp = zipWith (compare) (f a) (f d)
                       n  = length . filter (== GT) $ zp
                   in (length zp - n, n)
