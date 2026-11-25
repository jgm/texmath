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
import Text.TeXMath.Shared (isUppercaseGreek, isRLSequence,
                            everywhereExpList, mapExpChildren,
                            combineStyles)
import Data.Char (isSymbol, isPunctuation)
import Data.Either (lefts, isLeft, rights)
import qualified Data.Text as T
import Data.List.Split  (splitWhen)
import qualified Data.List as List

-- | Transforms an expression tree to an OMML XML Tree
writeOMML :: DisplayType -> [Exp] -> Element
writeOMML dt = container . concatMap (showExp [])
            . everywhereExpList (handleDownup dt)
            . handleDownupDelims dt
            . everywhereExpList handleScaledDelims
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

str :: [TextType] -> T.Text -> Element
str []    s = mnode "r" [ mnode "t" $ T.unpack s ]
str props s = mnode "r" [ mnode "rPr" (setProps props)
                        , mnode "t" $ T.unpack s ]

showFraction :: [TextType] -> FractionType -> Exp -> Exp -> Element
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

makeArray :: [TextType] -> [Alignment] -> [ArrayLine] -> Element
makeArray props as rs
  | isRLSequence as = mnode "eqArr" $ map toE rs
  where toE r = mnode "e" (List.intercalate ampersand
                            (map (concatMap (showExp props)) r))
        ampersand = [mnode "r" [ mnode "t" ("&" :: String) ]]
makeArray props as rs = mnode "m" $ mProps : map toMr rs
  where mProps = mnode "mPr"
                  [ mnodeA "baseJc" "center" ()
                  , mnodeA "plcHide" "on" ()
                  , mnode "mcs" $ map toMc as' ]
        as'    = take (maximum' $ map length rs) $ as ++ repeat AlignCenter
        toMr r = mnode "mr" $ map (mnode "e" . concatMap (showExp props)) r
        toMc a = mnode "mc" $ mnode "mcPr"
                            [ mnodeA "count" "1" ()
                            , mnodeA "mcJc" (toAlign a) ()
                            ]
        toAlign AlignLeft    = "left"
        toAlign AlignRight   = "right"
        toAlign AlignCenter  = "center"

makeText :: TextType -> T.Text -> Element
makeText TextNormal s =
  mnode "r" [ mnode "rPr" [mnode "nor" ()]
            , mnode "t" $ T.unpack s ]
makeText a s = str [a] s

defaultTo :: TextType -> [TextType] -> [TextType]
defaultTo tt []  = [tt]
defaultTo _  tts = tts

setProps :: [TextType] -> [Element]
setProps tts =
  case combineStyles tts of
       Nothing               -> []
       Just TextNormal       -> [sty "p"]
       Just TextBold         -> [sty "b"]
       Just TextItalic       -> [sty "i"]
       Just TextMonospace    -> [scr "monospace", sty "p"]
       Just TextSansSerif    -> [scr "sans-serif", sty "p"]
       Just TextDoubleStruck -> [scr "double-struck", sty "p"]
       Just TextScript       -> [scr "script", sty "p"]
       Just TextFraktur      -> [scr "fraktur", sty "p"]
       Just TextBoldItalic    -> [sty "bi"]
       Just TextSansSerifBold -> [scr "sans-serif", sty "b"]
       Just TextBoldScript    -> [scr "script", sty "b"]
       Just TextBoldFraktur   -> [scr "fraktur", sty "b"]
       Just TextSansSerifItalic -> [scr "sans-serif", sty "i"]
       Just TextSansSerifBoldItalic -> [scr "sans-serif", sty "bi"]
   where sty x = mnodeA "sty" x ()
         scr x = mnodeA "scr" x ()

-- OMML cannot represent a numeric scale factor for delimiters, so
-- render a pair of scaled delimiters as an m:d, which grows the
-- delimiters to fit their content.
handleScaledDelims :: [Exp] -> [Exp]
handleScaledDelims (x@(EScaled _ (ESymbol Open op)) : xs) =
  case break isCloser xs of
    (ys, EScaled _ (ESymbol Close cl) : zs) ->
      EDelimited op cl (map Right ys) : zs
    _ -> x:xs
 where
  isCloser (EScaled _ (ESymbol Close _)) = True
  isCloser _ = False
handleScaledDelims xs = xs


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

-- | Apply @handleDownup' dt@ to every delimited-expression list,
-- bottom up, including every tail of every list (the same traversal
-- the SYB @everywhere@ it replaces performed).
handleDownupDelims :: DisplayType -> [Exp] -> [Exp]
handleDownupDelims dt = map goE
  where
    goE (EDelimited o c ds) = EDelimited o c (goDs ds)
    goE e = mapExpChildren goE (map goE) e
    goDs = foldr (\d acc -> f (fmap goE d : acc)) (f [])
    f = handleDownup' dt

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

showExp :: [TextType] -> Exp -> [Element]
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
                  [ mnode "d" $ mnode "dPr"
                               [ mnodeA "begChr" (T.unpack start) ()
                               , mnodeA "sepChr" (T.unpack sepchr) ()
                               , mnodeA "endChr" (T.unpack end) ()
                               , mnode "grow" () ]
                              : elements
                  ]
      where
       seps = lefts xs
       -- m:d allows only one separator character; if the separators
       -- differ, write them as ordinary (non-stretching) characters
       -- in a single m:e rather than losing all but the first
       sameSeps = case seps of
                    []     -> True
                    (s:ss) -> all (== s) ss
       sepchr = case seps of
                  (s:_) | sameSeps -> s
                  _ -> ""
       elements
         | sameSeps = map (mnode "e" . concatMap (showExp props)) es
         | otherwise = [mnode "e" $ concatMap
                          (either (\s -> [str props s]) (showExp props)) xs]
       es   = map rights $ splitWhen isLeft xs

   EIdentifier ""   -> [str props "\x200B"]  -- 0-width space
                       -- to avoid the dashed box we get otherwise; see #118
   EIdentifier x
     | isUppercaseGreek x
     , null props -> [str [TextNormal] x]
     | not (isUppercaseGreek x)
     , TextBold `elem` props
      -> [str (TextBold : TextItalic : props) x] -- #280
     | otherwise  -> [str props x]
   EMathOperator x  -> [str (TextNormal : props) x]
   ESymbol ty xs
    | Just (c, xs') <- T.uncons xs
    , T.null xs'
    , isSymbol c || isPunctuation c
                    -> [str (defaultTo TextNormal props) xs]
    | ty `elem` [Op, Bin, Rel]
                    -> [mnode "box"
                        [ mnode "boxPr"
                          [ mnodeA "opEmu" "on" () ]
                        , mnode "e"
                          [str (defaultTo TextNormal props) xs]
                        ]]
    | otherwise     -> [str (defaultTo TextNormal props) xs]
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
   -- e.g. \underbrace, \underbracket: a stretchy character below
   EUnder _ x (ESymbol TUnder (T.unpack -> y)) ->
                       [mnode "groupChr" [ mnode "groupChrPr"
                                           [ mnodeA "chr" y ()
                                           , mnodeA "pos" "bot" ()
                                           , mnodeA "vertJc" "top" () ]
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
   ESqrt x       -> [mnode "rad" [ mnode "radPr" $ mnodeA "degHide" "on" ()
                                      , mnode "deg" ()
                                      , mnode "e" $ showExp props x]]
   ERoot i x     -> [mnode "rad" [ mnode "deg" $ showExp props i
                                 , mnode "e" $ showExp props x]]
   EFraction ft x y -> [showFraction props ft x y]
   EPhantom x       -> [mnode "phant" [ mnode "phantPr"
                                            [ mnodeA "show" "off" () ]
                                          , mnode "e" $ showExp props x]]
   EBoxed   x       -> [mnode "borderBox" [ mnode "e" $ showExp props x]]
   ECancel st x     -> [makeCancel props st x]
   EScaled _ x      -> showExp props x -- no support for scaler?
   EArray as ls     -> [makeArray props as ls]
   EText a s        -> [makeText a s]
   EStyled a es     -> concatMap (showExp (a : props)) es

isBarChar :: Char -> Bool
isBarChar c = c == '\x203E' || c == '\x00AF' ||
              c == '\x0304' || c == '\x0333' ||
              c == '_'

-- | Checks whether an expression marks the start of an nary operator
-- expression. These are different integrals, sums, products, and
-- coproducts.
isNary :: Exp -> Bool
isNary (ESymbol Op s) = case s of
  "\x222b" -> True  -- integral
  "\x222c" -> True  -- double integral
  "\x222d" -> True  -- triple integral
  "\x222e" -> True  -- line integral
  "\x222f" -> True  -- double line integral
  "\x2230" -> True  -- triple line integral
  "\x220f" -> True  -- product
  "\x2210" -> True  -- coproduct
  "\x2211" -> True  -- sum
  _        -> False
isNary _ = False

-- Kept as String for Text.XML.Light
makeNary :: [TextType] -> String -> T.Text -> Exp -> Exp -> Exp -> Element
makeNary props t s y z w =
  mnode "nary" [ mnode "naryPr"
                 [ mnodeA "chr" (T.unpack s) ()
                 , mnodeA "limLoc" t ()
                 , mnodeA "subHide"
                    (if isEmptySub then "on" else "off") ()
                 , mnodeA "supHide"
                    (if isEmptySup then "on" else "off") ()
                 ]
               , mnode "sub" (if isEmptySub then [] else showExp props y)
               , mnode "sup" (if isEmptySup then [] else showExp props z)
               , mnode "e" $ showExp props w ]
  where isEmptySub = y == EGrouped []
        isEmptySup = z == EGrouped []
        -- An empty sub/sup is already hidden from Word via the
        -- subHide/supHide flags set above, so there is no dashed
        -- box to avoid here and the ZWSP that showExp would inject
        -- for the empty group is pure noise: it leaks U+200B into
        -- docx roundtrips and breaks LaTeX.  Emit an empty
        -- <m:sub/> / <m:sup/> instead (cf. #118).

makeCancel :: [TextType] -> StrokeType -> Exp -> Element
makeCancel props st x = do
  let sn = case st of
             ForwardSlash -> [ mnodeA "strikeBLTR" "1" () ]
             BackSlash    -> [ mnodeA "strikeTLBR" "1" () ]
             XSlash       -> [ mnodeA "strikeBLTR" "1" () 
                             , mnodeA "strikeTLBR" "1" () ]
  mnode "borderBox" [ mnode "borderBoxPr" $ 
                      [ mnodeA "hideTop" "1" ()
                      , mnodeA "hideBot" "1" ()
                      , mnodeA "hideLeft" "1" ()
                      , mnodeA "hideRight" "1" () ] ++ sn
                    , mnode "e" $ showExp props x ]
