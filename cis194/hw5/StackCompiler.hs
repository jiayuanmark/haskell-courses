{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Applicative ((<$>))
import Expr
import Parser
import StackVM

instance Expr Program where
  lit p   = [PushI p]
  add p q = p ++ q ++ [Add]
  mul p q = p ++ q ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

exec :: String -> Maybe (Either String StackVal)
exec s = stackVM <$> (compile s)
