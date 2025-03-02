{-# LANGUAGE ViewPatterns, ScopedTypeVariables, OverloadedStrings,
   TupleSections #-}
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

import Text.XML.Light
import Text.TeXMath.Types
import Text.TeXMath.Unicode.ToUnicode
import Data.Generics (everywhere, mkT)
import Text.TeXMath.Shared (getMMLType, handleDownup,
                            isUppercaseGreek, isRLSequence)
import Text.TeXMath.Readers.MathML.MMLDict (getMathMLOperator)
import qualified Data.Text as T
import Text.Printf

-- | Transforms an expression tree to a MathML XML tree
writeMathML :: DisplayType -> [Exp] -> Element
writeMathML dt exprs =
  add_attr dtattr $ math $ showExp Nothing $ EGrouped
  $ everywhere (mkT $ handleDownup dt) exprs
    where dtattr = Attr (unqual "display") dt'
          dt' =  case dt of
                      DisplayBlock  -> "block"
                      DisplayInline -> "inline"

math :: Element -> Element
math = add_attr (Attr (unqual "xmlns") "http://www.w3.org/1998/Math/MathML") . unode "math"

mrow :: [Element] -> Element
mrow = unode "mrow"

showFraction :: Maybe TextType -> FractionType -> Exp -> Exp -> Element
showFraction tt ft x y =
  case ft of
       NormalFrac   -> unode "mfrac" [x', y']
       InlineFrac   -> withAttribute "displaystyle" "false" .
                         unode "mstyle" .
                         unode "mfrac" $ [x', y']
       DisplayFrac  -> withAttribute "displaystyle" "true" .
                         unode "mstyle" .
                         unode "mfrac" $ [x', y']
       NoLineFrac   -> withAttribute "linethickness" "0" .
                         unode "mfrac" $ [x', y']
  where x' = showExp tt x
        y' = showExp tt y

spaceWidth :: Rational -> Element
spaceWidth w =
  withAttribute "width" (dropTrailing0s
     (T.pack $ printf "%.3f" (fromRational w :: Double)) <> "em") $ unode "mspace" ()

makeStretchy :: FormType -> Element -> Element
makeStretchy (fromForm -> t)  = withAttribute "stretchy" "true"
                                . withAttribute "form" t

fromForm :: FormType -> T.Text
fromForm FInfix   = "infix"
fromForm FPostfix = "postfix"
fromForm FPrefix  = "prefix"

makeScaled :: Rational -> Element -> Element
makeScaled x = withAttribute "minsize" s . withAttribute "maxsize" s
  where s = dropTrailing0s $ T.pack $ printf "%.3f" (fromRational x :: Double)


dropTrailing0s :: T.Text -> T.Text
dropTrailing0s t = case T.unsnoc t of -- T.spanEnd does not exist
  Just (ts, '0') -> addZero $ T.dropWhileEnd (== '0') ts
  _              -> t
  where
    addZero x = case T.unsnoc x of
      Just (_, '.') -> T.snoc x '0'
      _ -> x

-- Note: Converts strings to unicode directly, as few renderers support those mathvariants.
makeText :: TextType -> T.Text -> Element
makeText a s = case (leadingSp, trailingSp) of
                   (False, False) -> s'
                   (True,  False) -> mrow [sp, s']
                   (False, True)  -> mrow [s', sp]
                   (True,  True)  -> mrow [sp, s', sp]
  where sp = spaceWidth (1/3)
        s' = withAttribute "mathvariant" attr $ tunode "mtext" $ toUnicode a s
        trailingSp = case T.unsnoc s of
          Just (_, c) -> T.any (== c) " \t"
          _           -> False
        leadingSp  = case T.uncons s of
          Just (c, _) -> T.any (== c) " \t"
          _           -> False
        attr = getMMLType a

makeArray :: Maybe TextType -> [Alignment] -> [ArrayLine] -> Element
makeArray tt as ls = unode "mtable" $
  map (unode "mtr" .
    zipWith (\a -> setAlignment a .  unode "mtd". showExps tt) as') ls
   -- see #205 on the need for style attributes:
   where setAlignment AlignLeft    =
           withAttribute "columnalign" "left" .
           withAttribute "style"
             (if isRLSequence as
                 then "text-align: left; padding-left: 0"
                 else "text-align: left")
         setAlignment AlignRight   =
           withAttribute "columnalign" "right" .
           withAttribute "style"
             (if isRLSequence as
                 then "text-align: right; padding-right: 0"
                 else "text-align: right")
         setAlignment AlignCenter  =
           withAttribute "columnalign" "center" .
           withAttribute "style" "text-align: center"
         as'                       = as ++ repeat AlignCenter

-- Kept as String for Text.XML.Light
withAttribute :: String -> T.Text -> Element -> Element
withAttribute a = add_attr . Attr (unqual a) . T.unpack

accent :: T.Text -> Element
accent = add_attr (Attr (unqual "accent") "true") .
           tunode "mo"

makeFence :: FormType -> Element -> Element
makeFence (fromForm -> t) = withAttribute "stretchy" "false" . withAttribute "form" t

showExp' :: Maybe TextType -> Exp -> Element
showExp' tt e =
  case e of
    ESymbol Accent x -> accent x
    ESymbol _ x      ->
      let isaccent = case (elem "accent") . properties <$>
                           getMathMLOperator x FPostfix of
                             Just True -> "true"
                             _         -> "false"
      in  withAttribute "accent" isaccent $ tunode "mo" x
    _                -> showExp tt e

showExps :: Maybe TextType -> [Exp] -> [Element]
showExps tt = map (showExp tt) . insertFunctionApps

insertFunctionApps :: [Exp] -> [Exp]
insertFunctionApps [] = []
insertFunctionApps (e@EMathOperator{} : ESymbol _ "\x2061" : es) =
  e : ESymbol Pun "\x2061" : insertFunctionApps es
insertFunctionApps (e@EMathOperator{} : es) =
  e : ESymbol Pun "\x2061" : insertFunctionApps es
insertFunctionApps (e:es) = e : insertFunctionApps es

showExp :: Maybe TextType -> Exp -> Element
showExp tt e =
 let toUnicodeMaybe :: TextType -> T.Text -> Maybe T.Text
     toUnicodeMaybe textStyle t =
       T.pack <$> mapM (toUnicodeChar . (textStyle,)) (T.unpack t)
     -- variant node: tries to convert text to appropriate unicode
     -- characters depending on style
     vnode :: String -> T.Text -> Element
     vnode elname t
       = case tt of
           Nothing ->
             if isUppercaseGreek t -- see #255
                then withAttribute "mathvariant" "normal" $ tunode elname t
                else tunode elname t
           Just TextNormal -> withAttribute "mathvariant" "normal" $
                                tunode elname t
           Just textStyle ->
             case toUnicodeMaybe textStyle t of
               -- if we can't find unicode equivalents, rely on mathvariant:
               Nothing -> withAttribute "mathvariant" (getMMLType textStyle) $
                             tunode elname t
               Just t' -> tunode elname t'
  in case e of
   ENumber x        -> vnode "mn" x
   EGrouped [x]     -> showExp tt x
   EGrouped xs      -> mrow $ showExps tt xs
   EDelimited start end xs -> mrow $
     [ makeStretchy FPrefix (vnode "mo" start) | not (T.null start) ] ++
     map (either (makeStretchy FInfix . vnode "mo") (showExp tt)) xs ++
     [ makeStretchy FPostfix (vnode "mo" end)
        | not (T.null end) ]
   EIdentifier x    -> vnode "mi" x
   EMathOperator x  -> vnode "mi" x -- see #257
   ESymbol Open x   -> makeFence FPrefix $ vnode "mo" x
   ESymbol Close x  -> makeFence FPostfix $ vnode "mo" x
   ESymbol Ord x
     | x == "\x2061" -> vnode "mo" x
     | otherwise    -> vnode "mi" x
   ESymbol _ x      -> vnode "mo" x
   ESpace x         -> spaceWidth x
   EFraction ft x y -> showFraction tt ft x y
   ESub x y         -> unode "msub" $ showExps tt [x, y]
   ESuper x y       -> unode "msup" $ showExps tt [x, y]
   ESubsup x y z    -> unode "msubsup" $ showExps tt [x, y, z]
   EUnder _ x y     -> unode "munder" $ showExps tt [x] ++ [showExp' tt y]
   EOver _ x (ESymbol Accent "\8407") -- see #218, gives better rendering for vectors
                    -> unode "mover" $ showExps tt [x] ++ [showExp' tt (ESymbol Accent "\8594")]
   EOver _ x y      -> unode "mover" $ showExps tt [x] ++ [showExp' tt y]
   EUnderover _ x y z -> unode "munderover" $
                          showExps tt [x] ++ [showExp' tt y, showExp' tt z]
   EPhantom x       -> unode "mphantom" $ showExps tt [x]
   EBoxed x         -> withAttribute "notation" "box" .
                       unode "menclose" $ showExp tt x
   ESqrt x          -> unode "msqrt" $ showExp tt x
   ERoot i x        -> unode "mroot" [showExp tt x, showExp tt i]
   EScaled s x      -> makeScaled s $ showExp tt x
   EArray as ls     -> makeArray tt as ls
   EText a s        -> case (tt, a) of
                         (Just ty, TextNormal) -> makeText ty s
                         _ -> makeText a s
   EStyled a es     -> showExp (Just a) (EGrouped es)
   -- see https://developer.mozilla.org/en-US/docs/Web/MathML/Element/mstyle
   -- Historically, this element accepted almost all the MathML attributes and
   -- it was used to override the default attribute values of its descendants.
   -- It was later restricted to only a few relevant styling attributes that
   -- were used in existing web pages. Nowadays, these styling attributes are
   -- common to all MathML elements and so <mstyle> is really just equivalent
   -- to an <mrow> element. However, <mstyle> may still be relevant for
   -- compatibility with MathML implementations outside browsers.

-- Kept as String for Text.XML.Light
tunode :: String -> T.Text -> Element
tunode s = unode s . T.unpack
