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

import Text.XML.Light
import Text.TeXMath.Types
import Data.Generics (everywhere, mkT)

toOMML :: DisplayType -> [Exp] -> Element
toOMML dt = container . concatMap showExp
            . everywhere (mkT $ handleDownup dt)
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
                        , mnode "t" s ]

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

-- showBinom :: [Element] -> Element
-- showBinom lst = mnode "mfenced" $ withAttribute "linethickness" "0" $ mnode "mfrac" lst

showBinary :: String -> Exp -> Exp -> [Element]
showBinary c x y =
  case c of
       "\\frac" -> [mnode "f" [ mnode "fPr" $
                                 mnodeAttr "type" [("val","bar")] ()
                              , mnode "num" x'
                              , mnode "den" y']]
       "\\dfrac" -> showBinary "\\frac" x y
       "\\tfrac" -> [mnode "f" [ mnode "fPr" $
                                  mnodeAttr "type" [("val","lin")] ()
                               , mnode "num" x'
                               , mnode "den" y']]
       "\\sqrt"  -> [mnode "rad" [ mnode "radPr" $
                                    mnodeAttr "degHide" [("val","on")] ()
                                , mnode "deg" $ showExp y
                                , mnode "e" $ showExp x]]
       _ -> error $ "Unknown binary operator " ++ c
    where x' = showExp x
          y' = showExp y

--  , ("\\stackrel", mnode "mover")
--  , ("\\overset", mnode "mover")
--  , ("\\underset", mnode "munder")
--  , ("\\binom", showBinom)
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

-}

handleDownup :: DisplayType -> Exp -> Exp
handleDownup DisplayInline (EDown x y)     = ESub x y
handleDownup DisplayBlock  (EDown x y)     = EUnder x y
handleDownup DisplayInline (EUp x y)       = ESuper x y
handleDownup DisplayBlock  (EUp x y)       = EOver x y
handleDownup DisplayInline (EDownup x y z) = ESubsup x y z
handleDownup DisplayBlock  (EDownup x y z) = EUnderover x y z
handleDownup _             x               = x

showExp :: Exp -> [Element]
showExp e =
 case e of
   ENumber x        -> [str [] x]
   EGrouped xs      -> concatMap showExp xs
   EIdentifier x    -> [str [] x]
   EMathOperator x  -> [str [] x]
--   EStretchy (ESymbol Open x)  -> makeStretchy $ mnode "mo" x
--   EStretchy (ESymbol Close x) -> makeStretchy $ mnode "mo" x
--   ESymbol Open x   -> withAttribute "stretchy" "false" $ mnode "mo" x
--   ESymbol Close x  -> withAttribute "stretchy" "false" $ mnode "mo" x
   ESymbol _ x      -> [str [] x]
--   ESpace x         -> spaceWidth x
   EBinary c x y    -> showBinary c x y
   ESub x y         -> [mnode "sSub" [ mnode "e" $ showExp x
                                     , mnode "sub" $ showExp y]]
   ESuper x y       -> [mnode "sSup" [ mnode "e" $ showExp x
                                     , mnode "sup" $ showExp y]]
   ESubsup x y z    -> [mnode "sSubSup" [ mnode "e" $ showExp x
                                        , mnode "sub" $ showExp y
                                        , mnode "sup" $ showExp z]]
   EUnder x y       -> [mnode "limLow" [ mnode "e" $ showExp x
                                       , mnode "lim" $ showExp y]]
   EOver x y        -> [mnode "limUpp" [ mnode "e" $ showExp x
                                       , mnode "lim" $ showExp y]]
   EUnderover x y z -> showExp (EUnder x (EOver y z))
   EUnary "\\sqrt" x  -> [mnode "rad" [ mnode "radPr" $ mnodeAttr "degHide" [("val","on")] ()
                                      , mnode "deg" ()
                                      , mnode "e" $ showExp x]]
   EUnary "\\surd" x  -> showExp $ EUnary "\\sqrt" x
--   EStretchy x      -> makeStretchy $ showExp x
--   EScaled s x      -> makeScaled s $ showExp x
--   EArray as ls     -> makeArray as ls
--   EText a s        -> makeText a s
--   -- a is normal, bold, italic, monospace, fraktur, double-struck, script, sans-serif
   x                -> error $ "showExp encountered " ++ show x
                       -- note: EUp, EDown, EDownup should be removed by handleDownup

