{-
Copyright (C) 2012 John MacFarlane <jgm@berkeley.edu>

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

{- | Functions for writing a parsed formula as OMML.
-}

module Text.TeXMath.OMML (toOMML, showExp)
where

import qualified Data.Map as M
import Text.XML.Light
import Text.TeXMath.Types
import Data.Generics (everywhere, mkT)

-- Namespaces: w: is used for ordinary OpenXML, m: for OMML.
toOMML :: DisplayType -> [Exp] -> Element
toOMML dt exprs =
  container $ map showExp $ everywhere (mkT $ handleDownup dt) exprs
    where container = case dt of
                  DisplayBlock  -> mnode "oMathPara" . mnode "oMath"
                  DisplayInline -> mnode "oMath"

mnode :: Node t => String -> t -> Element
mnode s = node (QName s Nothing (Just "m"))

mnodeAttr :: Node t => String -> [(String,String)] -> t -> Element
mnodeAttr s [] = mnode s
mnodeAttr s ((k,v):rest) = add_attr (Attr (QName k Nothing (Just "m")) v) . mnodeAttr s rest

str :: [Element] -> String -> Element
str props s = mnode "r" [ mnode "rPr" props
                        , mnode "t" s
                        ]

{- Firefox seems to set spacing based on its own dictionary,
-  so I believe this is unnecessary.
 
setSpacing :: String -> String -> Bool -> Element -> Element
setSpacing left right stretchy elt =
  add_attr (Attr (name "lspace") left) $
  add_attr (Attr (name "rspace") right) $
  if stretchy
     then add_attr (Attr (name "stretchy") "true") elt
     else elt

showSymbol (ESymbol s x) =
  case s of
    Ord   x  -> mnode "mo" x
    Op    x  -> setSpacing "0" "0.167em" True $ mnode "mo" x
    Bin   x  -> setSpacing "0.222em" "0.222em" False $ mnode "mo" x
    Rel   x  -> setSpacing "0.278em" "0.278em" False $ mnode "mo" x
    Open  x  -> setSpacing "0" "0" True $ mnode "mo" x
    Close x  -> setSpacing "0" "0" True $ mnode "mo" x
    Pun   x  -> setSpacing "0" "0.167em" False $ mnode "mo" x
-}

tofrac :: [Element] -> Element
tofrac [num,den] = mnode "f" [mnode "num" num, mnode "den" den]
tofrac _ = error "tofrac requires two arguments"

binaryOps :: M.Map String ([Element] -> Element)
binaryOps = M.fromList
  [ ("\\frac", tofrac)
--  , ("\\tfrac", withAttribute "displaystyle" "false" .
--                  mnode "mstyle" . mnode "mfrac")
--  , ("\\dfrac", withAttribute "displaystyle" "true" .
--                  mnode "mstyle" . mnode "mfrac")
--  , ("\\sqrt", mnode "mroot")
--  , ("\\stackrel", mnode "mover")
--  , ("\\overset", mnode "mover")
--  , ("\\underset", mnode "munder")
--  , ("\\binom", showBinom)
  ]

-- showBinom :: [Element] -> Element
-- showBinom lst = mnode "mfenced" $ withAttribute "linethickness" "0" $ mnode "mfrac" lst

showBinary :: String -> Exp -> Exp -> Element
showBinary c x y =
  case M.lookup c binaryOps of
       Just f   -> f [showExp x, showExp y]
       Nothing  -> error $ "Unknown binary op: " ++ c

{-

spaceWidth :: String -> Element
spaceWidth w = withAttribute "width" w $ mnode "mspace" ()

makeStretchy :: Element -> Element
makeStretchy = withAttribute "stretchy" "true"

makeScaled :: String -> Element -> Element
makeScaled s = withAttribute "minsize" s . withAttribute "maxsize" s

makeText :: String -> String -> Element
makeText a s = if trailingSp
                  then mrow [s', sp]
                  else s'
  where sp = spaceWidth "0.333em"
        s' = withAttribute "mathvariant" a $ mnode "mtext" s
        trailingSp = not (null s) && last s `elem` " \t"

makeArray :: [Alignment] -> [ArrayLine] -> Element
makeArray as ls = mnode "mtable" $
  map (mnode "mtr" .
    zipWith (\a -> setAlignment a .  mnode "mtd". map showExp) as') ls
   where setAlignment AlignLeft    = withAttribute "columnalign" "left"
         setAlignment AlignRight   = withAttribute "columnalign" "right"
         setAlignment AlignCenter  = withAttribute "columnalign" "center"
         setAlignment AlignDefault = id 
         as'                       = as ++ cycle [AlignDefault]

withAttribute :: String -> String -> Element -> Element
withAttribute a = add_attr . Attr (name a)

accent :: String -> Element
accent = add_attr (Attr (name "accent") "true") .
           mnode "mo"

-}

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
   ENumber x        -> str [] x
   EGrouped [x]     -> showExp x
--   EGrouped xs      -> mnode "r" $ map showExp xs
   EIdentifier x    -> str [] x
   EMathOperator x  -> str [] x
--   ESymbol Accent x -> accent x
--   EStretchy (ESymbol Open x)  -> makeStretchy $ mnode "mo" x
--   EStretchy (ESymbol Close x) -> makeStretchy $ mnode "mo" x
--   ESymbol Open x   -> withAttribute "stretchy" "false" $ mnode "mo" x
--   ESymbol Close x  -> withAttribute "stretchy" "false" $ mnode "mo" x
--   ESymbol _ x      -> mnode "mo" x
--   ESpace x         -> spaceWidth x
   EBinary c x y    -> showBinary c x y
--   ESub x y         -> mnode "msub" $ map showExp [x, y]
--   ESuper x y       -> mnode "msup" $ map showExp [x, y]
--   ESubsup x y z    -> mnode "msubsup" $ map showExp [x, y, z]
--   EUnder x y       -> mnode "munder" $ map showExp [x, y]
--   EOver x y        -> mnode "mover" $ map showExp [x, y]
--   EUnderover x y z -> mnode "munderover" $ map showExp [x, y, z]
   EUnary "\\sqrt" x  -> mnode "rad" [ mnode "radPr" $ mnodeAttr "degHide" [("val","on")] ()
                                     , mnode "deg" ()
                                     , mnode "e" $ showExp x
                                     ]
   EUnary "\\surd" x  -> showExp $ EUnary "\\sqrt" x
--   EStretchy x      -> makeStretchy $ showExp x
--   EScaled s x      -> makeScaled s $ showExp x
--   EArray as ls     -> makeArray as ls
--   EText a s        -> makeText a s
   x                -> error $ "showExp encountered " ++ show x
                       -- note: EUp, EDown, EDownup should be removed by handleDownup

