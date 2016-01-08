module Scrabble where

import Data.List (elem, foldl')

data Score = Score Int
           deriving (Show, Eq)

instance Monoid Score where
  mempty = Score 0
  mappend (Score x) (Score y) = Score (x + y)

score :: Char -> Score
score c
  | c `elem` "aeilnorstu" = Score 1
  | c `elem` "dg"         = Score 2
  | c `elem` "bcmp"       = Score 3
  | c `elem` "fhvwy"      = Score 4
  | c == 'k'              = Score 5
  | c `elem` "jx"         = Score 8
  | c `elem` "qz"         = Score 10
  | otherwise             = mempty

scoreString :: String -> Score
scoreString = foldl' mappend mempty . map score
