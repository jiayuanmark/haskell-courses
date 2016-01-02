{-# LANGUAGE FlexibleInstances #-}
import Expr
import qualified Data.Map as M

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Var String
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit       = const . Just
  add f1 f2 = \m -> do
    a <- f1 m
    b <- f2 m
    return (a + b)
  mul f1 f2 = \m -> do
    a <- f1 m
    b <- f2 m
    return (a * b)

withVars :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
