{-# LANGUAGE ViewPatterns #-}
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

module Text.TeXMath.Writers.MathML (writeMathML)
where

import qualified Data.Map as M
import Text.XML.Light
import Text.TeXMath.Types
import Text.TeXMath.Unicode.ToUnicode
import Data.Generics (everywhere, mkT)
import Text.TeXMath.Shared (getMMLType)
import Text.TeXMath.Readers.MathML.MMLDict (getMathMLOperator)
import Control.Applicative ((<$>))
import Text.Printf

-- | Transforms an expression tree to a MathML XML tree
writeMathML :: DisplayType -> [Exp] -> Element
writeMathML dt exprs =
  add_attr dtattr $ math $ showExp TextNormal $ EGrouped
  $ everywhere (mkT $ handleDownup dt) exprs
    where dtattr = Attr (unqual "display") dt'
          dt' =  case dt of
                      DisplayBlock  -> "block"
                      DisplayInline -> "inline"

math :: Element -> Element
math = add_attr (Attr (unqual "xmlns") "http://www.w3.org/1998/Math/MathML") . unode "math"

mrow :: [Element] -> Element
mrow = unode "mrow"

unaryOps :: M.Map String String
unaryOps = M.fromList
  [ ("\\sqrt", "msqrt")
  , ("\\surd", "msqrt")
  ]

showUnary :: TextType -> String -> Exp -> Element
showUnary tt c x =
  case M.lookup c unaryOps of
       Just c'  -> unode c' (showExp tt x)
       Nothing  -> error $ "Unknown unary op: " ++ c

binaryOps :: M.Map String ([Element] -> Element)
binaryOps = M.fromList
  [ ("\\frac", unode "mfrac")
  , ("\\tfrac", withAttribute "displaystyle" "false" .
                  unode "mstyle" . unode "mfrac")
  , ("\\dfrac", withAttribute "displaystyle" "true" .
                  unode "mstyle" . unode "mfrac")
  , ("\\sqrt", unode "mroot" . reverse)
  , ("\\stackrel", unode "mover" . reverse)
  , ("\\overset", unode "mover" . reverse)
  , ("\\underset", unode "munder" . reverse)
  , ("\\binom", showBinom)
  , ("\\genfrac{}{}{0.0mm}{}", withAttribute "linethickness" "0" .
                                unode "mfrac")
  ]

showBinom :: [Element] -> Element
showBinom lst = unode "mfenced" $ withAttribute "linethickness" "0" $ unode "mfrac" lst

showBinary :: TextType -> String -> Exp -> Exp -> Element
showBinary tt c x y =
  case M.lookup c binaryOps of
       Just f   -> f [showExp tt x, showExp tt y]
       Nothing  -> error $ "Unknown binary op: " ++ c

spaceWidth :: Double -> Element
spaceWidth w =
  withAttribute "width" (dropTrailing0s (printf "%.3f" w) ++ "em")
  $ unode "mspace" ()

makeStretchy :: FormType -> Element -> Element
makeStretchy (fromForm -> t)  = withAttribute "stretchy" "true"
                                . withAttribute "form" t

fromForm :: FormType -> String
fromForm FInfix   = "infix"
fromForm FPostfix = "postfix"
fromForm FPrefix  = "prefix"


makeScaled :: Double -> Element -> Element
makeScaled x = withAttribute "minsize" s . withAttribute "maxsize" s
  where s = dropTrailing0s $ printf "%.3f" x

dropTrailing0s :: String -> String
dropTrailing0s = reverse . go . reverse
  where go ('0':'.':xs) = '0':'.':xs
        go ('0':xs) = go xs
        go xs       = xs

makeStyled :: TextType -> [Element] -> Element
makeStyled a es = withAttribute "mathvariant" attr
                $ unode "mstyle" es
  where attr = getMMLType a

-- Note: Converts strings to unicode directly, as few renderers support those mathvariants.
makeText :: TextType -> String -> Element
makeText a s = if trailingSp
                  then mrow [s', sp]
                  else s'
  where sp = spaceWidth 0.333
        s' = withAttribute "mathvariant" attr $ unode "mtext" $ toUnicode a s
        trailingSp = not (null s) && last s `elem` " \t"
        attr = getMMLType a

makeArray :: TextType -> [Alignment] -> [ArrayLine] -> Element
makeArray tt as ls = unode "mtable" $
  map (unode "mtr" .
    zipWith (\a -> setAlignment a .  unode "mtd". map (showExp tt)) as') ls
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
handleDownup DisplayInline (EUnder True x y)       = ESub x y
handleDownup DisplayInline (EOver True x y)        = ESuper x y
handleDownup DisplayInline (EUnderover True x y z) = ESubsup x y z
handleDownup DisplayBlock  (EUnder True x y)       = EUnder False x y
handleDownup DisplayBlock  (EOver True x y)        = EOver False  x y
handleDownup DisplayBlock  (EUnderover True x y z) = EUnderover False x y z
handleDownup _             x                       = x

makeFence :: FormType -> Element -> Element
makeFence (fromForm -> t) = withAttribute "stretchy" "false" . withAttribute "form" t

op :: String -> Element
op s = accentCons $ unode "mo" s
  where
    accentCons = 
      case (elem "accent") . properties <$> getMathMLOperator s FPostfix of
            Nothing -> id
            Just False -> id
            Just True -> withAttribute "accent" "false"
            

showExp :: TextType -> Exp -> Element
showExp tt e =
 case e of
   ENumber x        -> unode "mn" x
   EGrouped [x]     -> showExp tt x
   EGrouped xs      -> mrow $ map (showExp tt) xs
   EDelimited start end xs -> mrow $
                       [ makeStretchy FPrefix (unode "mo" start) | not (null start) ] ++
                       map (either (makeStretchy FInfix . unode "mo") (showExp tt)) xs ++
                       [ makeStretchy FPostfix (unode "mo" end) | not (null end) ]
   EIdentifier x    -> unode "mi" $ toUnicode tt x
   EMathOperator x  -> unode "mi" x
   ESymbol Accent x -> accent x
   ESymbol Open x   -> makeFence FPrefix $ op x
   ESymbol Close x  -> makeFence FPostfix $ op x
   ESymbol _ x      -> op x
   ESpace x         -> spaceWidth x
   EBinary c x y    -> showBinary tt c x y
   ESub x y         -> unode "msub" $ map (showExp tt) [x, y]
   ESuper x y       -> unode "msup" $ map (showExp tt) [x, y]
   ESubsup x y z    -> unode "msubsup" $ map (showExp tt) [x, y, z]
   EUnder _ x y     -> unode "munder" $ map (showExp tt) [x, y]
   EOver _ x y      -> unode "mover" $ map (showExp tt) [x, y]
   EUnderover _ x y z -> unode "munderover" $ map (showExp tt) [x, y, z]
   EUnary c x       -> showUnary tt c x
   EPhantom x       -> unode "mphantom" $ showExp tt x
   EScaled s x      -> makeScaled s $ showExp tt x
   EArray as ls     -> makeArray tt as ls
   EText a s        -> makeText a s
   EStyled a es     -> makeStyled a $ map (showExp a) es
