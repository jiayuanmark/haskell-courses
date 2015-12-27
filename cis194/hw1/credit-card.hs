import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0    = []
  | otherwise = map (fromIntegral . digitToInt) (show x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = go . reverse
  where go []       = []
        go (x:y:xs) = (go xs) ++ [(2 * y), x]
        go (x:xs)   = (go xs) ++ [x]

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate x = (f x) `mod` 10 == 0
  where f = sumDigits . doubleEveryOther . toDigits
