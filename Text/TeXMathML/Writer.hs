module Text.TeXMathML.Writer (toMathML, DisplayType(..), showExp)
where

import Control.Monad
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

showExp e =
 case e of
   EInteger x   -> unode "mn" $ show x
   EFloat   x   -> unode "mn" $ show x
   EParenthesized xs -> mrow $ unode "mo" "(" : map showExp xs ++ [unode "mo" ")"]
   EGrouped xs  -> mrow $ map showExp xs
   EVariable x  -> unode "mi" x
   ESymbol x    -> showSymbol x
   EFraction x y -> unode "mfrac" [showExp x, showExp y]
   ESuperscripted x y -> unode "msup" [showExp x, showExp y]
   ESubscripted x y -> unode "msub" [showExp x, showExp y]

