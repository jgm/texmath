{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-
Copyright (C) 2014 Matthew Pickering <matthewtpickering@gmail.com>

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
module Text.TeXMath.Shared
  ( getMMLType
  , getTextType
  , getLaTeXTextCommand
  , getScalerCommand
  , getScalerValue
  , scalers
  , getSpaceWidth
  , getSpaceChars
  , getDiacriticalCommand
  , diacriticals
  , getOperator
  , readLength
  , fixTree
  , isEmpty
  , empty
  , handleDownup
  ) where


import Text.TeXMath.Types
import Text.TeXMath.TeX
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.List (sort)
import Control.Monad (guard)
import Text.Parsec (Parsec, parse, getInput, digit, char, many1, option)
import Data.Generics (everywhere, mkT)

-- As we constuct from the bottom up, this situation can occur.
removeNesting :: Exp -> Exp
removeNesting (EDelimited o c [Right (EDelimited "" "" xs)]) = EDelimited o c xs
removeNesting (EDelimited "" "" [x]) = either (ESymbol Ord) id x
removeNesting (EGrouped [x]) = x
removeNesting x = x

removeEmpty :: [Exp] -> [Exp]
removeEmpty xs = filter (not . isEmpty) xs

-- | An empty group of expressions
empty :: Exp
empty = EGrouped []

-- | Test to see whether an expression is @empty@.
isEmpty :: Exp -> Bool
isEmpty (EGrouped []) = True
isEmpty _ = False

-- | Walks over a tree of expressions, removing empty expressions, and
-- fixing delimited expressions with no delimiters and unnecessarily
-- grouped expressions.
fixTree :: Exp -> Exp
fixTree = everywhere (mkT removeNesting) . everywhere (mkT removeEmpty)

-- | Maps TextType to the corresponding MathML mathvariant
getMMLType :: TextType -> T.Text
getMMLType t = fromMaybe "normal" (fst <$> M.lookup t textTypesMap)

-- | Maps TextType to corresponding LaTeX command
getLaTeXTextCommand :: Env -> TextType -> T.Text
getLaTeXTextCommand e t =
  let textCmd = fromMaybe "\\mathrm"
                  (snd <$> M.lookup t textTypesMap) in
  if textPackage textCmd e
    then textCmd
    else fromMaybe "\\mathrm" (M.lookup textCmd alts)

-- | Maps MathML mathvariant to the corresponing TextType
getTextType :: T.Text -> TextType
getTextType s = fromMaybe TextNormal (M.lookup s revTextTypesMap)

-- | Maps a LaTeX scaling command to the percentage scaling
getScalerCommand :: Rational -> Maybe T.Text
getScalerCommand width =
  case sort [ (w, cmd) | (cmd, w) <- scalers, w >= width ] of
       ((_,cmd):_) -> Just cmd
       _           -> Nothing
  -- note, we don't use a Map here because we need the first
  -- match:  \Big, not \Bigr

-- | Gets percentage scaling from LaTeX scaling command
getScalerValue :: T.Text -> Maybe Rational
getScalerValue command = lookup command scalers

-- | Given a diacritical mark, returns the corresponding LaTeX command
getDiacriticalCommand  :: Position -> T.Text -> Maybe T.Text
getDiacriticalCommand pos symbol = do
  command <- M.lookup symbol diaMap
  guard (not $ command `elem` unavailable)
  let below = command `elem` under
  case pos of
    Under -> if below then Just command else Nothing
    Over -> if not below then Just command else Nothing
  where
    diaMap = M.fromList diacriticals

-- Operator Table

getOperator :: Exp -> Maybe TeX
getOperator op = fmap ControlSeq $ M.lookup op operators

operators :: M.Map Exp T.Text
operators = M.fromList
           [ (EMathOperator "arccos", "\\arccos")
           , (EMathOperator "arcsin", "\\arcsin")
           , (EMathOperator "arctan", "\\arctan")
           , (EMathOperator "arg", "\\arg")
           , (EMathOperator "cos", "\\cos")
           , (EMathOperator "cosh", "\\cosh")
           , (EMathOperator "cot", "\\cot")
           , (EMathOperator "coth", "\\coth")
           , (EMathOperator "csc", "\\csc")
           , (EMathOperator "deg", "\\deg")
           , (EMathOperator "det", "\\det")
           , (EMathOperator "dim", "\\dim")
           , (EMathOperator "exp", "\\exp")
           , (EMathOperator "gcd", "\\gcd")
           , (EMathOperator "hom", "\\hom")
           , (EMathOperator "inf", "\\inf")
           , (EMathOperator "ker", "\\ker")
           , (EMathOperator "lg", "\\lg")
           , (EMathOperator "lim", "\\lim")
           , (EMathOperator "liminf", "\\liminf")
           , (EMathOperator "limsup", "\\limsup")
           , (EMathOperator "ln", "\\ln")
           , (EMathOperator "log", "\\log")
           , (EMathOperator "max", "\\max")
           , (EMathOperator "min", "\\min")
           , (EMathOperator "Pr", "\\Pr")
           , (EMathOperator "sec", "\\sec")
           , (EMathOperator "sin", "\\sin")
           , (EMathOperator "sinh", "\\sinh")
           , (EMathOperator "sup", "\\sup")
           , (EMathOperator "tan", "\\tan")
           , (EMathOperator "tanh", "\\tanh") ]

-- | Attempts to convert a string into
readLength :: T.Text -> Maybe Rational
readLength s = do
  (n, unit) <- case (parse parseLength "" s) of
                  Left _ -> Nothing
                  Right v -> Just v
  (n *) <$> unitToMultiplier unit


parseLength :: Parsec T.Text () (Rational, T.Text)
parseLength = do
    neg <- option "" ((:[]) <$> char '-')
    dec <- many1 digit
    frac <- option "" ((:) <$> char '.' <*> many1 digit)
    unit <- getInput
    -- This is safe as dec and frac must be a double of some kind
    case reads (neg ++ dec ++ frac) of
       [(n :: Double, [])] -> return (round (n * 18) % 18, unit)
       _ -> fail $ "Could not parse " ++ neg ++ dec ++ frac ++ " as Double"

textTypesMap :: M.Map TextType (T.Text, T.Text)
textTypesMap = M.fromList textTypes

revTextTypesMap :: M.Map T.Text TextType
revTextTypesMap = M.fromList $ map (\(k, (v,_)) -> (v,k)) textTypes

--TextType to (MathML, LaTeX)
textTypes :: [(TextType, (T.Text, T.Text))]
textTypes =
  [ ( TextNormal       , ("normal", "\\mathrm"))
  , ( TextBold         , ("bold", "\\mathbf"))
  , ( TextItalic       , ("italic","\\mathit"))
  , ( TextMonospace    , ("monospace","\\mathtt"))
  , ( TextSansSerif    , ("sans-serif","\\mathsf"))
  , ( TextDoubleStruck , ("double-struck","\\mathbb"))
  , ( TextScript       , ("script","\\mathcal"))
  , ( TextFraktur      , ("fraktur","\\mathfrak"))
  , ( TextBoldItalic          , ("bold-italic","\\mathbfit"))
  , ( TextSansSerifBold       , ("bold-sans-serif","\\mathbfsfup"))
  , ( TextSansSerifBoldItalic , ("sans-serif-bold-italic","\\mathbfsfit"))
  , ( TextBoldScript          , ("bold-script","\\mathbfscr"))
  , ( TextBoldFraktur         , ("bold-fraktur","\\mathbffrak"))
  , ( TextSansSerifItalic     , ("sans-serif-italic","\\mathsfit")) ]

unicodeMath, base :: Set.Set T.Text
unicodeMath = Set.fromList
  ["\\mathbfit", "\\mathbfsfup", "\\mathbfsfit", "\\mathbfscr",
   "\\mathbffrak", "\\mathsfit"]
base = Set.fromList
  ["\\mathbb", "\\mathrm", "\\mathbf", "\\mathit", "\\mathsf",
   "\\mathtt", "\\mathfrak", "\\mathcal"]

alts :: M.Map T.Text T.Text
alts = M.fromList
  [ ("\\mathbfit", "\\mathbf")
  , ("\\mathbfsfup", "\\mathbf")
  , ("\\mathbfsfit", "\\mathbf")
  , ("\\mathbfscr", "\\mathcal")
  , ("\\mathbffrak", "\\mathfrak")
  , ("\\mathsfit", "\\mathsf")
  ]

textPackage :: T.Text -> [T.Text] -> Bool
textPackage s e
  | s `Set.member` unicodeMath = "unicode-math" `elem` e
  | s `Set.member` base    = True
  | otherwise = True

-- | Mapping between LaTeX scaling commands and the scaling factor
scalers :: [(T.Text, Rational)]
scalers =
          [ ("\\bigg", widthbigg)
          , ("\\Bigg", widthBigg)
          , ("\\big", widthbig)
          , ("\\Big", widthBig)
          , ("\\biggr", widthbigg)
          , ("\\Biggr", widthBigg)
          , ("\\bigr", widthbig)
          , ("\\Bigr", widthBig)
          , ("\\biggl", widthbigg)
          , ("\\Biggl", widthBigg)
          , ("\\bigl", widthbig)]
  where widthbig = 6 / 5
        widthBig = 9 / 5
        widthbigg = 12 / 5
        widthBigg = 3

-- | Returns the space width for a unicode space character, or Nothing.
getSpaceWidth :: Char -> Maybe Rational
getSpaceWidth ' '      = Just (4/18)
getSpaceWidth '\xA0'   = Just (4/18)
getSpaceWidth '\x2000' = Just (1/2)
getSpaceWidth '\x2001' = Just 1
getSpaceWidth '\x2002' = Just (1/2)
getSpaceWidth '\x2003' = Just 1
getSpaceWidth '\x2004' = Just (1/3)
getSpaceWidth '\x2005' = Just (4/18)
getSpaceWidth '\x2006' = Just (1/6)
getSpaceWidth '\x2007' = Just (1/3) -- ? width of a digit
getSpaceWidth '\x2008' = Just (1/6) -- ? width of a period
getSpaceWidth '\x2009' = Just (1/6)
getSpaceWidth '\x200A' = Just (1/9)
getSpaceWidth '\x200B' = Just 0
getSpaceWidth '\x202F' = Just (3/18)
getSpaceWidth '\x205F' = Just (4/18)
getSpaceWidth _        = Nothing

-- | Returns the sequence of unicode space characters closest to the
-- specified width.
getSpaceChars :: Rational -> T.Text
getSpaceChars r
  | n < 0 = "\x200B" -- no negative space chars in unicode
  | otherwise = fracSpaces f <> emQuads n
  where
    (n, f) = properFraction r
    emQuads x = T.replicate x "\x2001"
    fracSpaces x
      | x <= 2/18 = "\x200A"
      | x <= 3/18 = "\x2006"
      | x <= 4/18 = "\xA0"   -- could also be "\x2005"
      | x <= 5/18 = "\x2005"
      | x <= 7/18 = "\x2004"
      | x <= 9/18 = "\x2000"
      | otherwise = T.cons '\x2000' $ fracSpaces (x - (1/2))

-- Accents which go under the character
under :: [T.Text]
under = ["\\underbrace", "\\underline", "\\underbar", "\\underbracket"]

-- We want to parse these but we can't represent them in LaTeX
unavailable :: [T.Text]
unavailable = ["\\overbracket", "\\underbracket"]


-- | Mapping between unicode combining character and LaTeX accent command
diacriticals :: [(T.Text, T.Text)]
diacriticals =
               [ ("\x00B4", "\\acute")
               , ("\x0301", "\\acute")
               , ("\x0060", "\\grave")
               , ("\x0300", "\\grave")
               , ("\x02D8", "\\breve")
               , ("\x0306", "\\breve")
               , ("\x02C7", "\\check")
               , ("\x030C", "\\check")
               , ("\x307", "\\dot")
               , ("\x308", "\\ddot")
               , ("\x20DB", "\\dddot")
               , ("\x20DC", "\\ddddot")
               , ("\x00B0", "\\mathring")
               , ("\x030A", "\\mathring")
               , ("\x20D7", "\\vec")
               , ("\x20D7", "\\overrightarrow")
               , ("\x20D6", "\\overleftarrow")
               , ("\x005E", "\\hat")
               , ("\x02C6", "\\widehat")
               , ("\x0302", "\\widehat")
               , ("\x02DC", "\\widetilde")
               , ("\x0303", "\\tilde")
               , ("\x0303", "\\widetilde")
               , ("\x0304", "\\bar")
               , ("\x203E", "\\bar")
               , ("\x23DE", "\\overbrace")
               , ("\x23B4", "\\overbracket") -- Only availible in mathtools
               , ("\x00AF", "\\overline")
               , ("\x0305", "\\overline")
               , ("\x23DF", "\\underbrace")
               , ("\x23B5", "\\underbracket") -- mathtools
               , ("\x0332", "\\underline")
               , ("_", "\\underline")
               , ("\x0333", "\\underbar")
               ]




-- Converts unit to multiplier to reach em
unitToMultiplier :: T.Text -> Maybe Rational
unitToMultiplier s = M.lookup s units
  where
    units = M.fromList  [ ( "pt" , 10)
                        , ( "mm" , (351/10))
                        , ( "cm" , (35/100))
                        , ( "in" , (14/100))
                        , ( "ex" , (232/100))
                        , ( "em" , 1)
                        , ( "mu" , 18)
                        , ( "dd" , (93/100))
                        , ( "bp" , (996/1000))
                        , ( "pc" , (83/100)) ]

handleDownup :: DisplayType -> Exp -> Exp
handleDownup DisplayInline (EUnder True x y)       = ESub x y
handleDownup DisplayInline (EOver True x y)        = ESuper x y
handleDownup DisplayInline (EUnderover True x y z) = ESubsup x y z
handleDownup DisplayBlock  (EUnder True x y)       = EUnder False x y
handleDownup DisplayBlock  (EOver True x y)        = EOver False  x y
handleDownup DisplayBlock  (EUnderover True x y z) = EUnderover False x y z
handleDownup _             x                       = x
