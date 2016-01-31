-- | Run Haskell tr implementation.
--
-- We will be testing using your `Tr` module, not `Main`, so don't worry too
-- much about compatibility of argument parsing.
module Main where

import Tr

-- | Main - parse args, and read from stdin.
main :: IO ()
main = do
  args <- words <$> getLine
  case args of
    a1 : a2 : a3 : _ -> putStrLn $ tr a1 (Just a2) a3
    a1 : a2 : _  | a1 == "-d" -> putStrLn $ tr a1 Nothing a2
                 | otherwise  -> putStrLn $ "Unrecognizable option " ++ a1
    _ -> putStrLn "Insufficient arguments!"
