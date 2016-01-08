module Main where

import Buffer
import Editor
import JoinList

main = runEditor editor b
  where b = (fromString . unlines $ s) :: JoinListBuffer
        s = [ "This buffer is for notes you don't want to save, and for"
            , "evaluation of steam valve coefficients."
            , "To load a different file, type the character L followed"
            , "by the name of the file."
            ]
