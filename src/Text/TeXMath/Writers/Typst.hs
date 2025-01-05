{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns, GADTs, OverloadedStrings #-}
{-
Copyright (C) 2023 John MacFarlane <jgm@berkeley.edu>

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

module Text.TeXMath.Writers.Typst (writeTypst) where

import Data.List (transpose)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.TeXMath.Types
import qualified Text.TeXMath.Shared as S
import Typst.Symbols (typstSymbols)
import Data.Generics (everywhere, mkT)
import Data.Text (Text)
import Data.Char (isDigit, isAlpha, isAscii)
import Data.Maybe (fromMaybe)

-- import Debug.Trace
-- tr' x = trace (show x) x

-- | Transforms an expression tree to equivalent Typst
writeTypst :: DisplayType -> [Exp] -> Text
writeTypst dt exprs =
  writeExps $ everywhere (mkT $ S.handleDownup dt) exprs

writeExps :: [Exp] -> Text
writeExps = go . map writeExp
 where
   go (a : b : es)
    | T.take 1 b == "'" -- avoid space before a prime #239
     = a <> go (b:es)
   go (a : as)
     = a <> if null as
               then mempty
               else " " <> go as
   go [] = mempty

inParens :: Text -> Text
inParens s = "(" <> s <> ")"

inQuotes :: Text -> Text
inQuotes s = "\"" <> escInQuotes s <> "\""

esc :: Text -> Text
esc t =
  if T.any needsEscape t
     then T.concatMap escapeChar t
     else t
  where
    escapeChar c
      | needsEscape c = "\\" <> T.singleton c
      | otherwise = T.singleton c
    needsEscape '[' = True
    needsEscape ']' = True
    needsEscape '|' = True
    needsEscape '#' = True
    needsEscape '$' = True
    needsEscape '(' = True
    needsEscape ')' = True
    needsEscape '_' = True
    needsEscape '*' = True
    needsEscape '^' = True
    needsEscape '"' = True
    needsEscape '/' = True
    needsEscape '\\' = True
    needsEscape ',' = True -- #242
    needsEscape _ = False

escInQuotes :: Text -> Text
escInQuotes t =
  if T.any needsEscape t
    then T.concatMap escapeChar t
    else t
  where
    escapeChar c
      | needsEscape c = "\\" <> T.singleton c
      | otherwise = T.singleton c
    needsEscape c = c == '\\' || c == '"'

writeExpS :: Exp -> Text
writeExpS e =
  case writeExp e of
    "" -> "()"
    t | Just (c,_) <- T.uncons t
      , c >= '\8988' && c <= '\8991'
      -> "\\" <> t -- see #245
    t | T.all (\c -> isDigit c || c == '.') t -> t
      | T.all (\c -> isAlpha c || c == '.') t -> t
      | otherwise -> "(" <> t <> ")"

writeExpB :: Exp -> Text
writeExpB e =
  case writeExp e of
    "" -> "zws"
    t -> t

writeExp :: Exp -> Text
writeExp (ENumber s) = s
writeExp (ESymbol _t s)
  | T.all isAscii s = esc s  -- use '+' not 'plus'
  | s == "\x2212" = "-" -- use '-' not 'minus'
  | s == "\8242" = "'" -- use ' for prime, see #239
  | s == "\8243" = "''"
  | s == "\8244" = "'''"
  | s == "\8279" = "''''"
  | otherwise = fromMaybe (esc s) $ M.lookup s typstSymbolMap
writeExp (EIdentifier s) =
  if T.length s == 1
     then writeExp (ESymbol Ord s)
     else inQuotes s
writeExp (EMathOperator s)
  | s `elem` ["arccos", "arcsin", "arctan", "arg", "cos", "cosh",
              "cot", "ctg", "coth", "csc", "deg", "det", "dim", "exp",
              "gcd", "hom", "mod", "inf", "ker", "lg", "lim", "ln",
              "log", "max", "min", "Pr", "sec", "sin", "sinh", "sup",
              "tan", "tg", "tanh", "liminf", "and", "limsup"]
    = s
  | otherwise = "\"" <> s <> "\""
writeExp (EGrouped es) = writeExps es
writeExp (EFraction _fractype e1 e2) =
  case (e1, e2) of
    (EGrouped _, _) -> "frac(" <> writeExp e1 <> ", " <> writeExp e2 <> ")"
    (_, EGrouped _) -> "frac(" <> writeExp e1 <> ", " <> writeExp e2 <> ")"
    _ -> writeExp e1 <> " / " <> writeExp e2
writeExp (ESub b e1) = writeExpB b <> "_" <> writeExpS e1
writeExp (ESuper b e1) = writeExpB b <> "^" <> writeExpS e1
writeExp (ESubsup b e1 e2) = writeExpB b <> "_" <> writeExpS e1 <>
                                           "^" <> writeExpS e2
writeExp (EOver _ (EOver _ b (ESymbol TOver "\9182")) e1) =
  "overbrace(" <> writeExp b <> ", " <> writeExp e1 <> ")"
writeExp (EOver _ (EOver _ b (ESymbol TOver "\9140")) e1) =
  "overbracket(" <> writeExp b <> ", " <> writeExp e1 <> ")"
writeExp (EOver _convertible b (ESymbol Accent ac))
  = case getAccentCommand ac of
     Just accCommand
       | not (isGrouped b) -> accCommand <> inParens (writeExp b)
     _ -> "accent" <> inParens (writeExp b <> ", " <> ac)
writeExp (EOver _convertible b e1) =
  case e1 of
    ESymbol TOver "\9182" -> "overbrace" <> inParens (writeExp b)
    ESymbol TOver "\9140" -> "overbracket" <> inParens (writeExp b)
    ESymbol TOver "\175" -> "overline" <> inParens (writeExp b)
    _ -> writeExpB b <> "^" <> writeExpS e1
writeExp (EUnder _ (EUnder _ b (ESymbol TUnder "\9183")) e1) =
  "underbrace(" <> writeExp b <> ", " <> writeExp e1 <> ")"
writeExp (EUnder _ (EUnder _ b (ESymbol TUnder "\9140")) e1) =
  "underbrace(" <> writeExp b <> ", " <> writeExp e1 <> ")"
writeExp (EUnder _convertible b e1) =
  case e1 of
    ESymbol TUnder "_" -> "underline(" <> writeExp b <> ")"
    ESymbol TUnder "\817" -> "underline(" <> writeExp b <> ")"
    ESymbol TUnder "\9183" -> "underbrace(" <> writeExp b <> ")"
    ESymbol TUnder "\9140" -> "underbracket(" <> writeExp b <> ")"
    _ -> writeExpB b <> "_" <> writeExpS e1
writeExp (EUnderover convertible b e1 e2) =
  case (e1, e2) of
    (_, ESymbol Accent _) -> writeExp (EUnder convertible (EOver False b e2) e1)
    (_, ESymbol TOver _) -> writeExp (EUnder convertible (EOver False b e2) e1)
    (ESymbol TUnder _, _) -> writeExp (EOver convertible (EUnder False b e1) e2)
    _ -> writeExpB b <> "_" <> writeExpS e1 <> "^" <> writeExpS e2
writeExp (ESqrt e) = "sqrt(" <> writeExp e <> ")"
writeExp (ERoot i e) = "root(" <> writeExp i <> ", " <> writeExp e <> ")"
writeExp (ESpace width) =
  case (floor (width * 18) :: Int) of
    0 -> "zws"
    3 -> "thin"
    4 -> "med"
    6 -> "thick"
    18 -> "quad"
    n -> "#h(" <> tshow (n `div` 18) <> "em)"
writeExp (EText ttype s) =
  case ttype of
       TextNormal -> "upright" <> inParens (inQuotes s)
       TextItalic -> "italic" <> inParens (inQuotes s)
       TextBold   -> "bold" <> inParens (inQuotes s)
       TextBoldItalic -> "bold" <> inParens ("italic" <> inParens (inQuotes s))
       TextMonospace -> "mono" <> inParens (inQuotes s)
       TextSansSerif -> "sans" <> inParens (inQuotes s)
       TextDoubleStruck -> "bb" <> inParens (inQuotes s)
       TextScript -> "cal" <> inParens (inQuotes s)
       TextFraktur -> "frak" <> inParens (inQuotes s)
       TextSansSerifBold -> "bold" <> inParens ("sans" <> inParens (inQuotes s))
       TextSansSerifBoldItalic -> "bold" <>
         inParens ("italic" <> inParens ("sans" <> inParens (inQuotes s)))
       TextBoldScript -> "bold" <> inParens ("cal" <> inParens (inQuotes s))
       TextBoldFraktur -> "bold" <> inParens ("frak" <> inParens (inQuotes s))
       TextSansSerifItalic -> "italic" <>
          inParens ("sans" <> inParens (inQuotes s))
writeExp (EStyled ttype es) =
  let contents = writeExps es
  in case ttype of
       TextNormal -> "upright" <> inParens contents
       TextItalic -> "italic" <> inParens contents
       TextBold   -> "bold" <> inParens contents
       TextBoldItalic -> "bold" <> inParens ("italic" <> inParens contents)
       TextMonospace -> "mono" <> inParens contents
       TextSansSerif -> "sans" <> inParens contents
       TextDoubleStruck -> "bb" <> inParens contents
       TextScript -> "cal" <> inParens contents
       TextFraktur -> "frak" <> inParens contents
       TextSansSerifBold -> "bold" <> inParens ("sans" <> inParens contents)
       TextSansSerifBoldItalic -> "bold" <>
         inParens ("italic" <> inParens ("sans" <> inParens contents))
       TextBoldScript -> "bold" <> inParens ("cal" <> inParens contents)
       TextBoldFraktur -> "bold" <> inParens ("frak" <> inParens contents)
       TextSansSerifItalic -> "italic" <> inParens ("sans" <> inParens contents)
writeExp (EBoxed e) = "#box(stroke: black, inset: 3pt, [$ " <> writeExp e <> " $])"
writeExp (EPhantom e) = "#hide[" <> writeExp e <> "]"
writeExp (EScaled size e) =
  "#scale(x: " <> tshow (floor (100 * size) :: Int) <>
          "%, y: " <> tshow (floor (100 * size) :: Int) <>
          "%)[" <> writeExp e <> "]"
writeExp (EDelimited "(" ")" [ Right (EFraction NoLineFrac x y) ]) =
  "binom(" <> writeExp x <> ", " <> writeExp y <> ")"
writeExp (EDelimited "(" ")" [Right (EArray _aligns rows)])
  | all (\row -> length row == 1) rows = -- vector
  "vec(" <> mkArray (transpose rows) <> ")"
writeExp (EDelimited "(" ")" [Right (EArray _aligns [[xs],[ys]])]) =
  "binom(" <> writeExps xs <> ", " <> writeExps ys <> ")"
writeExp (EDelimited "(" ")" [Right (EArray _aligns rows)]) =
  "mat(delim: \"(\", " <> mkArray rows <> ")"
writeExp (EDelimited "[" "]" [Right (EArray _aligns rows)]) =
  "mat(delim: \"[\", " <> mkArray rows <> ")"
writeExp (EDelimited "{" "}" [Right (EArray _aligns rows)]) =
  "mat(delim: \"{\", " <> mkArray rows <> ")"
writeExp (EDelimited "|" "|" [Right (EArray _aligns rows)]) =
  "mat(delim: \"|\", " <> mkArray rows <> ")"
writeExp (EDelimited "||" "||" [Right (EArray _aligns rows)]) =
  "mat(delim: \"||\", " <> mkArray rows <> ")"
writeExp (EDelimited "\x2223" "\x2223" [Right (EArray _aligns rows)]) =
  "mat(delim: \"||\", " <> mkArray rows <> ")"
writeExp (EDelimited "\x2225" "\x2225" [Right (EArray _aligns rows)]) =
  "mat(delim: \"||\", " <> mkArray rows <> ")"
writeExp (EDelimited op "" [Right (EArray [AlignLeft, AlignLeft] rows)]) =
  "cases" <> inParens("delim: " <> inQuotes op <> mconcat (map toCase rows))
   where toCase = (", " <>) . T.intercalate " & " . map writeExps
writeExp (EDelimited open close es) =
  if isDelim open && isDelim close
     then
       (if matchedPair open close &&  -- see #233
            not (any (\x -> x == Left open || x == Left close) es)
           then id
           else ("lr" <>) . inParens)
          (renderOpen open <> body <> renderClose close)
     else esc open <> body <> esc close
  where fromDelimited (Left e)  = "mid(" <> renderSymbol e <> ")"
        fromDelimited (Right e) = writeExp e
        isDelim c = c `elem` ["(",")","[","]","{","}","|","||","\x2016"]
        renderOpen e =
          if T.all isAscii e
             then e
             else renderSymbol e <> " "
        renderClose e =
          if T.all isAscii e
             then e
             else " " <> renderSymbol e
        renderSymbol e = fromMaybe (esc e) (M.lookup e typstSymbolMap)
        matchedPair "(" ")" = True
        matchedPair "[" "]" = True
        matchedPair "{" "}" = True
        matchedPair _ _ = False
        body = T.unwords (map fromDelimited es)
writeExp (EArray _aligns rows)
  = T.intercalate "\\\n" $ map mkRow rows
     where mkRow = T.intercalate " & " . map writeExps

mkArray :: [[[Exp]]] -> Text
mkArray rows =
  T.intercalate "; " $ map mkRow rows
 where
   mkRow = T.intercalate ", " . mkCells . map mkCell
   mkCells cs =
     case cs of
       ("":rest) -> "#none" : rest
       _ -> cs
   mkCell = writeExps

tshow :: Show a => a -> Text
tshow = T.pack . show

typstSymbolMap :: M.Map Text Text
typstSymbolMap = M.fromList $
  ("\776", "dot.double") -- see #231
  : [(s,name) | (name, _, s) <- typstSymbols]

getAccentCommand :: Text -> Maybe Text
getAccentCommand ac = do
  case ac of
    "`" -> Just "grave"
    "\180" -> Just "acute"
    "^" -> Just "hat"
    "~" -> Just "tilde"
    "." -> Just "dot"
    "\168" -> Just "diaer"
    "\175" -> Just "macron"
    "\711" -> Just "caron"
    "\728" -> Just "breve"
    "\733" -> Just "acute.double"
    "\768" -> Just "grave"
    "\769" -> Just "acute"
    "\770" -> Just "hat"
    "\771" -> Just "tilde"
    "\772" -> Just "macron"
    "\773" -> Just "overline"
    "\774" -> Just "breve"
    "\775" -> Just "dot"
    "\776" -> Just "dot.double"
    "\777" -> Just "harpoon"
    "\778" -> Just "circle"
    "\779" -> Just "acute.double"
    "\780" -> Just "caron"
    "\781" -> Just "overline"
    "\x2218" -> Just "circle"
    "\x2192" -> Just "->"
    "\x2190" -> Just "<-"
    "\8254" -> Just "macron"
    "\8400" -> Just "harpoon.lt"
    "\8401" -> Just "harpoon"
    "\8406" -> Just "arrow.l"
    "\8407" -> Just "arrow"
    "\8417" -> Just "arrow.l.r"
    _ -> Nothing

isGrouped :: Exp -> Bool
isGrouped (EGrouped _) = True
isGrouped _ = False
