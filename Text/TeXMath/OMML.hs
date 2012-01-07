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

showBinary :: String -> Exp -> Exp -> Element
showBinary c x y =
  case c of
       "\\frac" -> mnode "f" [ mnode "fPr" $
                                mnodeAttr "type" [("val","bar")] ()
                             , mnode "num" x'
                             , mnode "den" y']
       "\\dfrac" -> showBinary "\\frac" x y
       "\\tfrac" -> mnode "f" [ mnode "fPr" $
                                 mnodeAttr "type" [("val","lin")] ()
                              , mnode "num" x'
                              , mnode "den" y']
       "\\sqrt"  -> mnode "rad" [ mnode "radPr" $
                                   mnodeAttr "degHide" [("val","on")] ()
                                , mnode "deg" y'
                                , mnode "e" x']
       "\\stackrel" -> mnode "limUpp" [ mnode "e" x'
                                       , mnode "lim" y']
       "\\overset" -> mnode "limUpp" [ mnode "e" x'
                                     , mnode "lim" y' ]
       "\\underset" -> mnode "limLow" [ mnode "e" x'
                                      , mnode "lim" y' ]
       "\\binom"    -> mnode "d" [ mnode "dPr" $
                                     mnodeAttr "sepChr" [("val",",")] ()
                                 , mnode "e" $
                                     mnode "f" [ mnode "fPr" $
                                                   mnodeAttr "type"
                                                     [("val","noBar")] ()
                                               , mnode "num" x'
                                               , mnode "den" y' ]] 
       _ -> error $ "Unknown binary operator " ++ c
    where x' = showExp x
          y' = showExp y

makeArray :: [Alignment] -> [ArrayLine] -> Element
makeArray as rs = mnode "m" $ mProps : map toMr rs
  where mProps = mnode "mPr"
                  [ mnodeAttr "baseJc" [("val","center")] ()
                  , mnodeAttr "plcHide" [("val","on")] ()
                  , mnode "mcs" $ map toMc as' ]
        as'    = take (length rs) $ as ++ cycle [AlignDefault]
        toMr r = mnode "mr" $ map (mnode "e" . concatMap showExp) r
        toMc a = mnode "mc" $ mnode "mcPr"
                            $ mnodeAttr "mcJc" [("val",toAlign a)] ()
        toAlign AlignLeft    = "left"
        toAlign AlignRight   = "right"
        toAlign AlignCenter  = "center"
        toAlign AlignDefault = "left"

makeText :: TextType -> String -> Element
makeText a s = str attrs s
  where attrs = case a of
                     TextNormal       -> [sty "p"]
                     TextBold         -> [sty "b"]
                     TextItalic       -> [sty "i"]
                     TextMonospace    -> [sty "p", scr "monospace"]
                     TextSansSerif    -> [sty "p", scr "sans-serif"]
                     TextDoubleStruck -> [sty "p", scr "double-struck"]
                     TextScript       -> [sty "p", scr "script"]
                     TextFraktur      -> [sty "p", scr "fraktur"]
        sty x = mnodeAttr "sty" [("val",x)] ()
        scr x = mnodeAttr "scr" [("val",x)] ()

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
   EStretchy x      -> showExp x  -- no support for stretchy in OMML
   ESymbol _ x      -> [str [] x]
   ESpace _         -> [] -- This seems to be how the stylesheet behaves
                          -- wouldn't it be better to use unicode space chars?
   EBinary c x y    -> [showBinary c x y]
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
   EScaled _ x      -> showExp x   -- no support for scaler?
   EArray as ls     -> [makeArray as ls]
   EText a s        -> [makeText a s]
   x                -> error $ "showExp encountered " ++ show x
   -- note: EUp, EDown, EDownup should be removed by handleDownup

