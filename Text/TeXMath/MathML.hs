{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- | Functions for writing a parsed formula as MathML.
-}

module Text.TeXMath.MathML (toMathML, showExp)
where

import qualified Data.Map as M
import Text.XML.Light
import Text.TeXMath.Types
import Data.Generics (everywhere, mkT)

toMathML :: DisplayType -> [Exp] -> Element
toMathML dt exprs =
  add_attr dtattr $ math $ map showExp $ everywhere (mkT $ handleDownup dt) exprs
    where dtattr = Attr (unqual "display") dt'
          dt' =  case dt of
                      DisplayBlock  -> "block"
                      DisplayInline -> "inline"

math :: [Element] -> Element
math = add_attr (Attr (unqual "xmlns") "http://www.w3.org/1998/Math/MathML") . unode "math" . unode "mrow"

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

showSymbol (ESymbol s x) =
  case s of
    Ord   x  -> unode "mo" x
    Op    x  -> setSpacing "0" "0.167em" True $ unode "mo" x
    Bin   x  -> setSpacing "0.222em" "0.222em" False $ unode "mo" x
    Rel   x  -> setSpacing "0.278em" "0.278em" False $ unode "mo" x
    Open  x  -> setSpacing "0" "0" True $ unode "mo" x
    Close x  -> setSpacing "0" "0" True $ unode "mo" x
    Pun   x  -> setSpacing "0" "0.167em" False $ unode "mo" x
-}

unaryOps :: M.Map String String
unaryOps = M.fromList
  [ ("\\sqrt", "msqrt")
  , ("\\surd", "msqrt")
  ]

showUnary :: String -> Exp -> Element
showUnary c x =
  case M.lookup c unaryOps of
       Just c'  -> unode c' (showExp x)
       Nothing  -> error $ "Unknown unary op: " ++ c

binaryOps :: M.Map String ([Element] -> Element)
binaryOps = M.fromList
  [ ("\\frac", unode "mfrac")
  , ("\\tfrac", withAttribute "displaystyle" "false" .
                  unode "mstyle" . unode "mfrac")
  , ("\\dfrac", withAttribute "displaystyle" "true" .
                  unode "mstyle" . unode "mfrac")
  , ("\\sqrt", unode "mroot")
  , ("\\stackrel", unode "mover")
  , ("\\overset", unode "mover")
  , ("\\underset", unode "munder")
  , ("\\binom", showBinom)
  ]

showBinom :: [Element] -> Element
showBinom lst = unode "mfenced" $ withAttribute "linethickness" "0" $ unode "mfrac" lst

showBinary :: String -> Exp -> Exp -> Element
showBinary c x y =
  case M.lookup c binaryOps of
       Just f   -> f [showExp x, showExp y]
       Nothing  -> error $ "Unknown binary op: " ++ c

spaceWidth :: String -> Element
spaceWidth w = withAttribute "width" w $ unode "mspace" ()

makeStretchy :: Element -> Element
makeStretchy = withAttribute "stretchy" "true"

makeScaled :: String -> Element -> Element
makeScaled s = withAttribute "minsize" s . withAttribute "maxsize" s

makeText :: TextType -> String -> Element
makeText a s = if trailingSp
                  then mrow [s', sp]
                  else s'
  where sp = spaceWidth "0.333em"
        s' = withAttribute "mathvariant" attr $ unode "mtext" s
        trailingSp = not (null s) && last s `elem` " \t"
        attr = case a of
                    TextNormal       -> "normal"
                    TextBold         -> "bold"
                    TextItalic       -> "italic"
                    TextMonospace    -> "monospace"
                    TextSansSerif    -> "sans-serif"
                    TextDoubleStruck -> "double-struck"
                    TextScript       -> "script"
                    TextFraktur      -> "fraktur"
                    TextBoldItalic          -> "bold-italic"
                    TextBoldSansSerif       -> "bold-sans-serif"
                    TextBoldSansSerifItalic -> "sans-serif-bold-italic"
                    TextBoldScript          -> "bold-script"
                    TextBoldFraktur         -> "bold-fraktur"
                    TextSansSerifItalic     -> "sans-serif-italic"

makeArray :: [Alignment] -> [ArrayLine] -> Element
makeArray as ls = unode "mtable" $
  map (unode "mtr" .
    zipWith (\a -> setAlignment a .  unode "mtd". map showExp) as') ls
   where setAlignment AlignLeft    = withAttribute "columnalign" "left"
         setAlignment AlignRight   = withAttribute "columnalign" "right"
         setAlignment AlignCenter  = withAttribute "columnalign" "center"
         setAlignment AlignDefault = id 
         as'                       = as ++ cycle [AlignDefault]

withAttribute :: String -> String -> Element -> Element
withAttribute a = add_attr . Attr (unqual a)

accent :: String -> Element
accent = add_attr (Attr (unqual "accent") "true") .
           unode "mo"

handleDownup :: DisplayType -> Exp -> Exp
handleDownup DisplayInline (EDown x y)     = ESub x y
handleDownup DisplayBlock  (EDown x y)     = EUnder x y
handleDownup DisplayInline (EUp x y)       = ESuper x y
handleDownup DisplayBlock  (EUp x y)       = EOver x y
handleDownup DisplayInline (EDownup x y z) = ESubsup x y z
handleDownup DisplayBlock  (EDownup x y z) = EUnderover x y z
handleDownup _             x               = x

showExp :: Exp -> Element
showExp e =
 case e of
   ENumber x        -> unode "mn" x
   EGrouped [x]     -> showExp x
   EGrouped xs      -> mrow $ map showExp xs
   EDelimited start end xs -> mrow $
                       [ makeStretchy (unode "mo" start) | not (null start) ] ++
                       map showExp xs ++
                       [ makeStretchy (unode "mo" end) | not (null end) ] 
   EIdentifier x    -> unode "mi" x
   EMathOperator x  -> unode "mi" x
   ESymbol Accent x -> accent x
   EStretchy (ESymbol Open x)  -> makeStretchy $ unode "mo" x
   EStretchy (ESymbol Close x) -> makeStretchy $ unode "mo" x
   ESymbol Open x   -> withAttribute "stretchy" "false" $ unode "mo" x
   ESymbol Close x  -> withAttribute "stretchy" "false" $ unode "mo" x
   ESymbol _ x      -> unode "mo" x
   ESpace x         -> spaceWidth x
   EBinary c x y    -> showBinary c x y
   ESub x y         -> unode "msub" $ map showExp [x, y]
   ESuper x y       -> unode "msup" $ map showExp [x, y]
   ESubsup x y z    -> unode "msubsup" $ map showExp [x, y, z]
   EUnder x y       -> unode "munder" $ map showExp [x, y]
   EOver x y        -> unode "mover" $ map showExp [x, y]
   EUnderover x y z -> unode "munderover" $ map showExp [x, y, z]
   EUnary c x       -> showUnary c x
   EStretchy x      -> makeStretchy $ showExp x
   EScaled s x      -> makeScaled s $ showExp x
   EArray as ls     -> makeArray as ls
   EText a s        -> makeText a s
   x                -> error $ "showExp encountered " ++ show x
                       -- note: EUp, EDown, EDownup should be removed by handleDownup

