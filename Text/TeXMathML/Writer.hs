module Text.TeXMathML.Writer (toMathML, DisplayType(..), showExp)
where

import Control.Monad
import qualified Data.Map as M
import Text.XML.Light
import Text.TeXMathML.Parser

data DisplayType = DisplayBlock
                 | DisplayInline
                 deriving Show

toMathML :: DisplayType -> [Exp] -> Element
toMathML dt exprs =
  add_attr dtattr $ math $ map showExp exprs
    where dtattr = Attr (unqual "display") dt'
          dt' =  case dt of
                      DisplayBlock  -> "block"
                      DisplayInline -> "inline"

math :: [Element] -> Element
math = add_attr (Attr (unqual "xmlns") "http://www.w3.org/1998/Math/MathML") . unode "math"

mrow :: [Element] -> Element
mrow = unode "mrow"

showSymbol s =
  case s of
    Ord   x  -> unode "mo" x
    Op    x  -> unode "mo" x
    Bin   x  -> unode "mo" x
    Rel   x  -> unode "mo" x
    Open  x  -> unode "mo" x
    Close x  -> unode "mo" x
    Pun   x  -> unode "mo" x

unaryOps = M.fromList
  [ ("sqrt", "msqrt")
  ]

showUnary c x =
  case M.lookup c unaryOps of
       Just c'  -> unode c' (showExp x)
       Nothing  -> error $ "Unknown unary op: " ++ c

binaryOps = M.fromList
  [ ("_", "msub")
  , ("^", "msup")
  , ("frac", "mfrac")
  , ("root", "mroot")
  , ("stack", "mover")
  ]

showBinary c x y =
  case M.lookup c binaryOps of
       Just c'  -> unode c' [showExp x, showExp y]
       Nothing  -> error $ "Unknown binary op: " ++ c

showExp e =
 case e of
   EInteger x   -> unode "mn" $ show x
   EFloat   x   -> unode "mn" $ show x
   EGrouped xs  -> mrow $ map showExp xs
   EIdentifier x -> unode "mi" x
   ESymbol x    -> showSymbol x
   EBinary c x y  -> showBinary c x y
   EUnary c x     -> showUnary c x

