{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Control.Applicative ((<$>))
import Data.List (sort)
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2 = case (l1 `compare` l2) of
  GT        -> l1
  otherwise -> l2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root chld) = f root $ map (treeFold f) chld

nextLevel :: Employee
          -> [(GuestList, GuestList)]
          -> (GuestList, GuestList)
nextLevel boss subtree = (glCons boss p2, p1)
  where p1 = mconcat $ map (uncurry moreFun) subtree
        p2 = mconcat $ map snd subtree

maxFun :: Tree Employee -> GuestList
maxFun = (uncurry moreFun) . treeFold nextLevel

readEmpTree :: FilePath -> IO (Tree Employee)
readEmpTree path = read <$> readFile path

outputResult :: GuestList -> IO ()
outputResult (GL e f) = do
  putStrLn $ "Total fun: " ++ (show f)
  putStrLn . unlines . sort . map empName $ e

main :: IO ()
main = do
  tree <- readEmpTree "company.txt"
  outputResult (maxFun tree)
