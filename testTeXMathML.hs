module Main where

import Text.TeXMath
import Text.XML.Light

main :: IO ()
main = do
  inp <- getContents
  putStr . ppTopElement . texMathToMathML DisplayBlock $! inp
