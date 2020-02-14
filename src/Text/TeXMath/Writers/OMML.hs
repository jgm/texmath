{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
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

module Text.TeXMath.Writers.OMML (writeOMML)
where

import Text.XML.Light
import Text.TeXMath.Types
import Data.Generics (everywhere, mkT)
import Data.Char (isSymbol, isPunctuation)
import qualified Data.Text as T

-- | Transforms an expression tree to an OMML XML Tree
writeOMML :: DisplayType -> [Exp] -> Element
writeOMML dt = container . concatMap (showExp [])
            . everywhere (mkT $ handleDownup dt)
            . everywhere (mkT $ handleDownup' dt)
    where container = case dt of
                  DisplayBlock  -> \x -> mnode "oMathPara"
                                    [ mnode "oMathParaPr"
                                      $ mnodeA "jc" "center" ()
                                    , mnode "oMath" x ]
                  DisplayInline -> mnode "oMath"

-- Kept as String for Text.XML.Light
mnode :: Node t => String -> t -> Element
mnode s = node (QName s Nothing (Just "m"))

-- Kept as String for Text.XML.Light
mnodeA :: Node t => String -> String -> t -> Element
mnodeA s v = add_attr (Attr (QName "val" Nothing (Just "m")) v) . mnode s

str :: [Element] -> T.Text -> Element
str []    s = mnode "r" [ mnode "t" $ T.unpack s ]
str props s = mnode "r" [ mnode "rPr" props
                        , mnode "t" $ T.unpack s ]

showFraction :: [Element] -> FractionType -> Exp -> Exp -> Element
showFraction props ft x y =
  case ft of
       NormalFrac -> mnode "f" [ mnode "fPr" $
                                mnodeA "type" "bar" ()
                             , mnode "num" x'
                             , mnode "den" y']
       DisplayFrac -> showFraction props NormalFrac x y
       InlineFrac -> mnode "f" [ mnode "fPr" $
                                 mnodeA "type" "lin" ()
                              , mnode "num" x'
                              , mnode "den" y']
       NoLineFrac -> mnode "f" [ mnode "fPr" $
                                              mnodeA "type" "noBar" ()
                                             , mnode "num" x'
                                             , mnode "den" y'
                                             ]
    where x' = showExp props x
          y' = showExp props y

maximum' :: [Int] -> Int
maximum' [] = 0
maximum' xs = maximum xs

makeArray :: [Element] -> [Alignment] -> [ArrayLine] -> Element
makeArray props as rs = mnode "m" $ mProps : map toMr rs
  where mProps = mnode "mPr"
                  [ mnodeA "baseJc" "center" ()
                  , mnodeA "plcHide" "1" ()
                  , mnode "mcs" $ map toMc as' ]
        as'    = take (maximum' $ map length rs) $ as ++ cycle [AlignCenter]
        toMr r = mnode "mr" $ map (mnode "e" . concatMap (showExp props)) r
        toMc a = mnode "mc" $ mnode "mcPr"
                            [ mnodeA "mcJc" (toAlign a) ()
                            , mnodeA "count" "1" ()
                            ]
        toAlign AlignLeft    = "left"
        toAlign AlignRight   = "right"
        toAlign AlignCenter  = "center"

makeText :: TextType -> T.Text -> Element
makeText a s = str (mnode "nor" () : setProps a) s

setProps :: TextType -> [Element]
setProps tt =
  case tt of
       TextNormal       -> [sty "p"]
       TextBold         -> [sty "b"]
       TextItalic       -> [sty "i"]
       TextMonospace    -> [sty "p", scr "monospace"]
       TextSansSerif    -> [sty "p", scr "sans-serif"]
       TextDoubleStruck -> [sty "p", scr "double-struck"]
       TextScript       -> [sty "p", scr "script"]
       TextFraktur      -> [sty "p", scr "fraktur"]
       TextBoldItalic    -> [sty "bi"]
       TextSansSerifBold -> [sty "b", scr "sans-serif"]
       TextBoldScript    -> [sty "b", scr "script"]
       TextBoldFraktur   -> [sty "b", scr "fraktur"]
       TextSansSerifItalic -> [sty "i", scr "sans-serif"]
       TextSansSerifBoldItalic -> [sty "bi", scr "sans-serif"]
   where sty x = mnodeA "sty" x ()
         scr x = mnodeA "scr" x ()

handleDownup :: DisplayType -> [Exp] -> [Exp]
handleDownup dt (exp' : xs) =
  case exp' of
       EOver convertible x y
         | isNary x  ->
             EGrouped [EUnderover convertible x emptyGroup y, next] : rest
         | convertible && dt == DisplayInline -> ESuper x y : xs
       EUnder convertible x y
         | isNary x  ->
             EGrouped [EUnderover convertible x y emptyGroup, next] : rest
         | convertible && dt == DisplayInline -> ESub x y : xs
       EUnderover convertible x y z
         | isNary x  ->
             EGrouped [EUnderover convertible x y z, next] : rest
         | convertible && dt == DisplayInline -> ESubsup x y z : xs
       ESub x y
         | isNary x  -> EGrouped [ESubsup x y emptyGroup, next] : rest
       ESuper x y
         | isNary x  -> EGrouped [ESubsup x emptyGroup y, next] : rest
       ESubsup x y z
         | isNary x  -> EGrouped [ESubsup x y z, next] : rest
       _             -> exp' : xs
    where (next, rest) = case xs of
                              (t:ts) -> (t,ts)
                              []     -> (emptyGroup, [])
          emptyGroup = EGrouped []
handleDownup _ []            = []

-- TODO This duplication is ugly and inefficient.  See #92.
handleDownup' :: DisplayType -> [InEDelimited] -> [InEDelimited]
handleDownup' dt ((Right exp') : xs) =
  case exp' of
       EOver convertible x y
         | isNary x  ->
             Right (EGrouped [EUnderover convertible x emptyGroup y, next]) :
             rest
         | convertible && dt == DisplayInline -> Right (ESuper x y) : xs
       EUnder convertible x y
         | isNary x  ->
             Right (EGrouped [EUnderover convertible x y emptyGroup, next]) :
             rest
         | convertible && dt == DisplayInline -> Right (ESub x y) : xs
       EUnderover convertible x y z
         | isNary x  ->
             Right (EGrouped [EUnderover convertible x y z, next]) : rest
         | convertible && dt == DisplayInline -> Right (ESubsup x y z) : xs
       ESub x y
         | isNary x  -> Right (EGrouped [ESubsup x y emptyGroup, next]) : rest
       ESuper x y
         | isNary x  -> Right (EGrouped [ESubsup x emptyGroup y, next]) : rest
       ESubsup x y z
         | isNary x  -> Right (EGrouped [ESubsup x y z, next]) : rest
       _             -> Right exp' : xs
    where (next, rest) = case xs of
                              (Right t:ts) -> (t,ts)
                              _            -> (emptyGroup, xs)
          emptyGroup = EGrouped []
handleDownup' _ xs = xs

showExp :: [Element] -> Exp -> [Element]
showExp props e =
 case e of
   ENumber x        -> [str props x]
   EGrouped [EUnderover _ (ESymbol Op s) y z, w] ->
     [makeNary props "undOvr" s y z w]
   EGrouped [ESubsup (ESymbol Op s) y z, w] ->
     [makeNary props "subSup" s y z w]
   EGrouped []      -> [str props "\x200B"] -- avoid dashed box, see #118
   EGrouped xs      -> concatMap (showExp props) xs
   EDelimited start end xs ->
                       [mnode "d" [ mnode "dPr"
                                    [ mnodeA "begChr" (T.unpack start) ()
                                    , mnodeA "endChr" (T.unpack end) ()
                                    , mnode "grow" () ]
                                  , mnode "e" $ concatMap
                                    (either ((:[]) . str props) (showExp props)) xs
                                  ] ]
   EIdentifier ""   -> [str props "\x200B"]  -- 0-width space
                       -- to avoid the dashed box we get otherwise; see #118
   EIdentifier x    -> [str props x]
   EMathOperator x  -> [makeText TextNormal x]  -- TODO revisit, use props?
   ESymbol ty xs
    | Just (c, xs') <- T.uncons xs
    , T.null xs'
    , isSymbol c || isPunctuation c
                    -> [str props xs]
    | ty `elem` [Op, Bin, Rel]
                    -> [mnode "box"
                        [ mnode "boxPr"
                          [ mnodeA "opEmu" "1" () ]
                        , mnode "e"
                          [str props xs]
                        ]]
    | otherwise     -> [str props xs]
   ESpace n
     | n > 0 && n <= 0.17    -> [str props "\x2009"]
     | n > 0.17 && n <= 0.23 -> [str props "\x2005"]
     | n > 0.23 && n <= 0.28 -> [str props "\x2004"]
     | n > 0.28 && n <= 0.5  -> [str props "\x2004"]
     | n > 0.5 && n <= 1.8   -> [str props "\x2001"]
     | n > 1.8               -> [str props "\x2001\x2001"]
     | otherwise             -> [str props "\x200B"]
       -- this is how the xslt sheet handles all spaces
   EUnder _ x (ESymbol TUnder t) | T.all isBarChar t ->
                       [mnode "bar" [ mnode "barPr" $
                                        mnodeA "pos" "bot" ()
                                    , mnode "e" $ showExp props x ]]
   EOver _ x (ESymbol TOver t) | T.all isBarChar t ->
                       [mnode "bar" [ mnode "barPr" $
                                        mnodeA "pos" "top" ()
                                    , mnode "e" $ showExp props x ]]
   EOver _ x (ESymbol st (T.unpack -> y))
    | st == Accent  -> [mnode "acc" [ mnode "accPr" [ mnodeA "chr" y () ]
                                    , mnode "e" $ showExp props x ]]
    | st == TUnder  -> [mnode "groupChr" [ mnode "groupChrPr"
                                           [ mnodeA "chr" y ()
                                           , mnodeA "pos" "bot" ()
                                           , mnodeA "vertJc" "top" () ]
                                    , mnode "e" $ showExp props x ]]
    | st == TOver   -> [mnode "groupChr" [ mnode "groupChrPr"
                                           [ mnodeA "chr" y ()
                                           , mnodeA "pos" "top" ()
                                           , mnodeA "vertJc" "bot" () ]
                                    , mnode "e" $ showExp props x ]]
   ESub x y         -> [mnode "sSub" [ mnode "e" $ showExp props x
                                     , mnode "sub" $ showExp props y]]
   ESuper x y       -> [mnode "sSup" [ mnode "e" $ showExp props x
                                     , mnode "sup" $ showExp props y]]
   ESubsup x y z    -> [mnode "sSubSup" [ mnode "e" $ showExp props x
                                        , mnode "sub" $ showExp props y
                                        , mnode "sup" $ showExp props z]]
   EUnder _ x y  -> [mnode "limLow" [ mnode "e" $ showExp props x
                                       , mnode "lim" $ showExp props y]]
   EOver _ x y   -> [mnode "limUpp" [ mnode "e" $ showExp props x
                                       , mnode "lim" $ showExp props y]]
   EUnderover c x y z -> showExp props (EUnder c (EOver c x z) y)
   ESqrt x       -> [mnode "rad" [ mnode "radPr" $ mnodeA "degHide" "1" ()
                                      , mnode "deg" ()
                                      , mnode "e" $ showExp props x]]
   ERoot i x     -> [mnode "rad" [ mnode "deg" $ showExp props i
                                 , mnode "e" $ showExp props x]]
   EFraction ft x y -> [showFraction props ft x y]
   EPhantom x       -> [mnode "phant" [ mnode "phantPr"
                                            [ mnodeA "show" "0" () ]
                                          , mnode "e" $ showExp props x]]
   EBoxed   x       -> [mnode "borderBox" [ mnode "e" $ showExp props x]]
   EScaled _ x      -> showExp props x -- no support for scaler?
   EArray as ls     -> [makeArray props as ls]
   EText a s        -> [makeText a s]
   EStyled a es     -> concatMap (showExp (setProps a)) es

isBarChar :: Char -> Bool
isBarChar c = c == '\x203E' || c == '\x00AF' ||
              c == '\x0304' || c == '\x0333'

isNary :: Exp -> Bool
isNary (ESymbol Op _) = True
isNary _ = False

-- Kept as String for Text.XML.Light
makeNary :: [Element] -> String -> T.Text -> Exp -> Exp -> Exp -> Element
makeNary props t s y z w =
  mnode "nary" [ mnode "naryPr"
                 [ mnodeA "chr" (T.unpack s) ()
                 , mnodeA "limLoc" t ()
                 , mnodeA "subHide"
                    (if y == EGrouped [] then "1" else "0") ()
                 , mnodeA "supHide"
                    (if z == EGrouped [] then "1" else "0") ()
                 ]
               , mnode "sub" $ showExp props y
               , mnode "sup" $ showExp props z
               , mnode "e" $ showExp props w ]

