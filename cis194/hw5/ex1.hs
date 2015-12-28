
import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit i)   = i
eval (Add l r) = (eval l) + (eval r)
eval (Mul l r) = (eval l) * (eval r)

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
  Just e  -> Just (eval e)
  Nothing -> Nothing
