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
play attackDie defendDie = (length cmp - awn, awn)
  where dsc  = sortBy (flip compare)
        cmp  = zipWith (>) (dsc attackDie) (dsc defendDie)
        awn  = length $ filter id cmp

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do
  attack <- replicateM na die
  defend <- replicateM nd die
  let (da, dd) = play attack defend
  return $ Battlefield (a-da) (d-dd)
  where na = min 3 (a-1)
        nd = min 2 d

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
  | d == 0 || a < 2 = return b
  | otherwise       = battle b >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  res <- replicateM 100000 (invade b)
  return $ (fl . filter attackWin $ res) / (fl res)
  where attackWin (Battlefield _ d) = d == 0
        fl = fromIntegral . length

-- Exact probability calculation
data Result = Attacker | Defender | Tie
  deriving (Show, Eq)

prb :: Int -> Int -> [Double]
prb a' d' = let sim = enumerate a' d'
                a   = filter (== Attacker) sim
                d   = filter (== Defender) sim
                t   = filter (== Tie) sim
                tot = 6.0 ^ (fromIntegral (a' + d'))
            in map (\x -> (fromIntegral . length $ x) / tot) [a,d,t]
  where go :: [Int] -> [Int] -> Result
        go as bs
          | a1 >  d1 && a2 >  d2 = Attacker
          | a1 <= d1 && a2 <= d2 = Defender
          | otherwise            = Tie
          where f         = sortBy (flip compare)
                (a1:a2:_) = f as
                (d1:d2:_) = f bs
        enumerate na nd = do
          as <- replicateM na [1..6]
          bs <- replicateM nd [1..6]
          return $ go as bs

exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield a d) = prob ! (a,d)
  where rate32 = (prb 3 2) :: [Double]
        rate22 = (prb 2 2) :: [Double]
        prob   :: Array (Int, Int) Double
        prob   = array ((0,0),(a,d)) [ ((i,j), go i j) | i <- [0..a],
                                                         j <- [0..d] ]
        go a d
          | a < 2     = 0
          | d == 0    = 1
          -- only 1 defender die
          | d < 2     = let a'   = min 3 (a-1)
                            dwin = (sum . map (\x -> (x / 6) ^ a') $ [1..6]) / 6
                        in (1 - dwin) * prob ! (a, d-1) + dwin * prob ! (a-1, d)
          -- only 1 attacker die
          | a == 2    = let awin = (sum . map (\x -> (x / 6) ^ 2) $ [1..5]) / 6
                        in awin * prob ! (a, d-1) + (1 - awin) * prob ! (a-1, d)
          -- 3 attacker die and 2 defender die
          | a >= 4    = sum $ zipWith (*) rate32 sub
          -- 2 attacker die and 2 defender die
          | otherwise = sum $ zipWith (*) rate22 sub
          where sub = [ prob ! (a,d-2), prob ! (a-2,d), prob ! (a-1,d-1) ]
