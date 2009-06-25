module Text.TeXMath.MathMLWriter (toMathML, DisplayType(..), showExp)
where

import qualified Data.Map as M
import Text.XML.Light
import Text.TeXMath.Parser

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

{- Firefox seems to set spacing based on its own dictionary,
-  so I believe this is unnecessary.
 
setSpacing :: String -> String -> Bool -> Element -> Element
setSpacing left right stretchy elt =
  add_attr (Attr (unqual "lspace") left) $
  add_attr (Attr (unqual "rspace") right) $
  if stretchy
     then add_attr (Attr (unqual "stretchy") "true") elt
     else elt

showSymbol s =
  case s of
    Ord   x  -> unode "mo" x
    Op    x  -> setSpacing "0" "0.167em" True $ unode "mo" x
    Bin   x  -> setSpacing "0.222em" "0.222em" False $ unode "mo" x
    Rel   x  -> setSpacing "0.278em" "0.278em" False $ unode "mo" x
    Open  x  -> setSpacing "0" "0" True $ unode "mo" x
    Close x  -> setSpacing "0" "0" True $ unode "mo" x
    Pun   x  -> setSpacing "0" "0.167em" False $ unode "mo" x
-}

showSymbol :: TeXSymbol -> Element
showSymbol s =
  case s of
    Ord   x  -> unode "mo" x
    Op    x  -> unode "mo" x
    Bin   x  -> unode "mo" x
    Rel   x  -> unode "mo" x
    Open  x  -> unode "mo" x
    Close x  -> unode "mo" x
    Pun   x  -> unode "mo" x

unaryOps :: M.Map String String
unaryOps = M.fromList
  [ ("sqrt", "msqrt")
  ]

showUnary :: String -> Exp -> Element
showUnary c x =
  case M.lookup c unaryOps of
       Just c'  -> unode c' (showExp x)
       Nothing  -> error $ "Unknown unary op: " ++ c

binaryOps :: M.Map String String
binaryOps = M.fromList
  [ ("_", "msub")
  , ("^", "msup")
  , ("frac", "mfrac")
  , ("root", "mroot")
  , ("stack", "mover")
  ]

showBinary :: String -> Exp -> Exp -> Element
showBinary c x y =
  case M.lookup c binaryOps of
       Just c'  -> unode c' [showExp x, showExp y]
       Nothing  -> error $ "Unknown binary op: " ++ c

showExp :: Exp -> Element
showExp e =
 case e of
   EInteger x   -> unode "mn" $ show x
   EFloat   x   -> unode "mn" $ show x
   EGrouped xs  -> mrow $ map showExp xs
   EIdentifier x -> unode "mi" x
   ESymbol x    -> showSymbol x
   EBinary c x y  -> showBinary c x y
   EUnary c x     -> showUnary c x

