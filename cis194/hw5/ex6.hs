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

instance HasVars (M.Map String Integer -> Maybe Integer)
  var =

instance Expr (M.Map String Integer -> Maybe Integer)
