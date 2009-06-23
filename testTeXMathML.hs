module Main where

import Text.TeXMathML
import Text.XML.Light

main = do
  inp <- getContents
  putStrLn . ppTopElement . texMathToMathML DisplayBlock $! inp
