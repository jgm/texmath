{-
Copyright (C) 2010-2013 John MacFarlane <jgm@berkeley.edu>

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

{- | Functions for writing a parsed formula as a list of Pandoc
     Inlines.
-}

module Text.TeXMath.Writers.Pandoc (writePandoc)
where
import Text.Pandoc.Definition
import Text.TeXMath.Unicode.ToUnicode
import Text.TeXMath.Types
import Text.TeXMath.Shared (getSpaceChars)

-- | Attempts to convert a formula to a list of 'Pandoc' inlines.
writePandoc :: DisplayType
         -> [Exp]
         -> Maybe [Inline]
writePandoc _ exps = expsToInlines TextNormal exps

expsToInlines :: TextType -> [Exp] -> Maybe [Inline]
expsToInlines tt xs = do
  res <- mapM (expToInlines tt) xs
  return (concat res)

renderStr :: TextType -> String -> Inline
renderStr tt s =
  case tt of
       TextNormal       -> Str s
       TextBold         -> Strong [Str s]
       TextItalic       -> Emph   [Str s]
       TextMonospace    -> Code nullAttr s
       TextSansSerif    -> Str s
       TextDoubleStruck -> Str $ toUnicode tt s
       TextScript       -> Str $ toUnicode tt s
       TextFraktur      -> Str $ toUnicode tt s
       TextBoldItalic    -> Strong [Emph [Str s]]
       TextSansSerifBold -> Strong [Str s]
       TextBoldScript    -> Strong [Str $ toUnicode tt s]
       TextBoldFraktur   -> Strong [Str $ toUnicode tt s]
       TextSansSerifItalic -> Emph [Str s]
       TextSansSerifBoldItalic -> Strong [Emph [Str s]]

expToInlines :: TextType -> Exp -> Maybe [Inline]
expToInlines tt (ENumber s) = Just [renderStr tt s]
expToInlines TextNormal (EIdentifier s) = Just [renderStr TextItalic s]
expToInlines tt (EIdentifier s) = Just [renderStr tt s]
expToInlines tt (EMathOperator s) = Just [renderStr tt s]
expToInlines tt (ESymbol t s) = Just $ addSpace t $ renderStr tt s
  where addSpace Op x = [x]
        addSpace Bin x = [medspace, x, medspace]
        addSpace Rel x = [widespace, x, widespace]
        addSpace Pun x = [x, thinspace]
        addSpace _ x = [x]
        thinspace = Str "\x2006"
        medspace  = Str "\x2005"
        widespace = Str "\x2004"
expToInlines tt (EDelimited start end xs) = do
  xs' <- mapM (either (return . (:[]) . renderStr tt) (expToInlines tt)) xs
  return $ [renderStr tt start] ++ concat xs' ++ [renderStr tt end]
expToInlines tt (EGrouped xs)    = expsToInlines tt xs
expToInlines _ (EStyled tt' xs)  = expsToInlines tt' xs
expToInlines _ (ESpace n)        = Just [Str $ getSpaceChars n]
expToInlines _ (ESqrt _)         = Nothing
expToInlines _ (ERoot _ _)       = Nothing
expToInlines _ (EFraction _ _ _) = Nothing
expToInlines tt (ESub x y) = do
  x' <- expToInlines tt x
  y' <- expToInlines tt y
  return $ x' ++ [Subscript y']
expToInlines tt (ESuper x y) = do
  x' <- expToInlines tt x
  y' <- expToInlines tt y
  return $ x' ++ [Superscript y']
expToInlines tt (ESubsup x y z) = do
  x' <- expToInlines tt x
  y' <- expToInlines tt y
  z' <- expToInlines tt z
  return $ x' ++ [Subscript y'] ++ [Superscript z']
expToInlines _ (EText tt' x) = Just [renderStr tt' x]
expToInlines tt (EOver _ (EGrouped [EIdentifier [c]]) (ESymbol Accent [accent])) =
    case accent of
         '\x203E' -> Just [renderStr tt' [c,'\x0304']]  -- bar
         '\x00B4' -> Just [renderStr tt' [c,'\x0301']]  -- acute
         '\x0060' -> Just [renderStr tt' [c,'\x0300']]  -- grave
         '\x02D8' -> Just [renderStr tt' [c,'\x0306']]  -- breve
         '\x02C7' -> Just [renderStr tt' [c,'\x030C']]  -- check
         '.'      -> Just [renderStr tt' [c,'\x0307']]  -- dot
         '\x00B0' -> Just [renderStr tt' [c,'\x030A']]  -- ring
         '\x20D7' -> Just [renderStr tt' [c,'\x20D7']]  -- arrow right
         '\x20D6' -> Just [renderStr tt' [c,'\x20D6']]  -- arrow left
         '\x005E' -> Just [renderStr tt' [c,'\x0302']]  -- hat
         '\x0302' -> Just [renderStr tt' [c,'\x0302']]  -- hat
         '~'      -> Just [renderStr tt' [c,'\x0303']]  -- tilde
         _        -> Nothing
      where tt' = if tt == TextNormal then TextItalic else tt
expToInlines tt (EScaled _ e) = expToInlines tt e
expToInlines tt (EUnder convertible b e)
  | convertible = expToInlines tt (ESub b e)
  | otherwise   = Nothing
expToInlines tt (EOver convertible b e)
  | convertible = expToInlines tt (ESuper b e)
  | otherwise   = Nothing
expToInlines _ (EUnderover _ _ _ _) = Nothing
expToInlines _ (EPhantom _) = Nothing
expToInlines _ (EArray _ _) = Nothing
