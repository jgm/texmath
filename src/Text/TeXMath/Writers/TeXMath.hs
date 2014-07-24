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

module Text.TeXMath.Writers.TeXMath (writeTeXMath, writeTeXMathIn) where

import Text.TeXMath.Types
import Data.List (intersperse)
import Text.TeXMath.Unicode.ToTeXMath (getTeXMath)
import qualified Text.TeXMath.Shared as S
import Data.Maybe (fromMaybe)
import Data.Generics (everywhere, mkT)
import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative ((<$), (<$>), (<|>))
import Data.Char (isLetter, isAlphaNum, isPunctuation)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)

-- | Transforms an expression tree to equivalent LaTeX without any
-- surrounding environment
writeTeXMath :: [Exp] -> String
writeTeXMath es =  -- we remove the leading { and trailing } at outer level
  drop 1 $ init $ renderTeX (Grouped $ concatMap (writeExp . fixTree) es) ""

-- | Transforms an expression tree to LaTeX with the
-- corresponding LaTeX environment
writeTeXMathIn :: DisplayType -> [Exp] -> String
writeTeXMathIn dt es =
  let math = writeTeXMath es in
    case dt of
      DisplayInline -> "\\(" ++ math ++ "\\)"
      DisplayBlock  -> "\\[" ++ math ++ "\\]"

-- | An intermediate representation of TeX math, to be used in rendering.
data TeX = ControlSeq String
         | Token Char
         | Literal String
         | Grouped [TeX]
         | Space
         deriving Show

-- | Render a 'TeX' to a string, appending to the front of the given string.
renderTeX :: TeX -> String -> String
renderTeX (Token c) cs
  | isPunctuation c        = c:cs
  | startsWith isLetter cs = c:' ':cs
  | otherwise              = c:cs
renderTeX (Literal s) cs
  | startsWith isLetter cs = s ++ (' ':cs)
  | otherwise              = s ++ cs
renderTeX (ControlSeq s) cs
  | startsWith isAlphaNum cs = s ++ (' ':cs)
  | otherwise                = s ++ cs
renderTeX (Grouped [Grouped xs]) cs  = renderTeX (Grouped xs) cs
renderTeX (Grouped [Token c]) cs  = renderTeX (Token c) cs
renderTeX (Grouped xs) cs     = '{' : foldr renderTeX "" xs ++ "}" ++ cs
renderTeX Space cs
  | startsWith (==' ') cs     = cs
  | otherwise                 = ' ':cs

startsWith :: (Char -> Bool) -> String -> Bool
startsWith p (c:cs) = p c
startsWith _ []     = False

square :: [String]
square = ["\\sqrt", "\\surd"]

isControlSeq :: String -> Bool
isControlSeq ['\\',c] = c /= ' '
isControlSeq ('\\':xs) = all isLetter xs
isControlSeq _ = False

writeExp :: Exp -> [TeX]
writeExp (ENumber s) = map Token (getTeXMath s)
writeExp (EGrouped es) = [Grouped $ concatMap writeExp es]
writeExp (EDelimited open close es) =
  ControlSeq "\\left" :
  Literal (getTeXMath open) :
  concatMap writeExp es ++
  [ControlSeq "\\right",
   Literal (getTeXMath close)]
writeExp (EIdentifier s) =
  case getTeXMath s of
       [c] -> [Token c] -- don't brace single token identifiers
       cs | isControlSeq cs -> [ControlSeq cs]
          | otherwise       -> writeExp (EMathOperator s)
writeExp o@(EMathOperator s) =
  case getOperator o of
       Just o   -> [o]
       Nothing  -> case getTeXMath s of
                        ""  -> []
                        xs | all isLetter xs ->
                              [ControlSeq "\\operatorname",
                               Grouped [Literal xs]]
                           | otherwise -> [Literal xs]
writeExp (ESymbol _ s) = [Literal $ getTeXMath s]
writeExp (ESpace width) = [ControlSeq $ getSpaceCommand width]
writeExp (EBinary s e1 e2)
  | s `elem` square = [ControlSeq s,
                       Token '['] ++
                       writeExp e1 ++
                      [Token ']',
                       Grouped (writeExp e2)]
  | otherwise = [ControlSeq s,
                 Grouped (writeExp e1),
                 Grouped (writeExp e2)]
writeExp (ESub b e1) =
  writeExp b ++ [Token '_', Grouped (writeExp e1)]
writeExp (ESuper b e1) =
  writeExp b ++ [Token '^', Grouped (writeExp e1)]
writeExp (ESubsup b e1 e2) =
  writeExp b ++ [Token '_', Grouped (writeExp e1),
   Token '^', Grouped (writeExp e2)]
writeExp (EDown b e1) =
   writeExp b ++ [ControlSeq "\\limits", Token '_',
   Grouped (writeExp e1)]
writeExp (EUp b e1) =
   writeExp b ++ [ControlSeq "\\limits", Token '^',
   Grouped (writeExp e1)]
writeExp (EDownup b e1 e2) =
   writeExp b ++ [ControlSeq "\\limits",
   Token '_', Grouped (writeExp e1),
   Token '^', Grouped (writeExp e2)]
writeExp (EOver b e1) =
  case b of
    (EMathOperator _) -> writeExp (EUp b e1)
    _ -> [ControlSeq "\\overset", Grouped (writeExp b), Grouped (writeExp e1)]
writeExp (EUnder b e1) =
  case b of
    (EMathOperator _) -> writeExp (EDown b e1)
    _ -> [ControlSeq "\\underset", Grouped (writeExp b), Grouped (writeExp e1)]
writeExp (EUnderover b e1 e2) =
  case b of
    (EMathOperator _) -> writeExp (EDownup b e1 e2)
    _ -> writeExp (EUnder (EOver b e2) e1)
writeExp (EUnary s e) = [ControlSeq s, Grouped (writeExp e)]
writeExp (EScaled size e) =
  case S.getScalerCommand size of
       Just s  -> [ControlSeq s, Grouped (writeExp e)]
       Nothing -> [Grouped (writeExp e)]
writeExp (EStretchy (ESymbol Open e)) =
  case getTeXMath e of
       "" -> []
       e' -> [ControlSeq "\\left", Literal e']
writeExp (EStretchy (ESymbol Close e)) =
  case getTeXMath e of
       "" -> []
       e' -> [ControlSeq "\\right", Literal e']
writeExp (EStretchy e) = writeExp e
writeExp (EText ttype s) =
  [ControlSeq (getTeXMathTextCommand ttype),
   Grouped [Literal $ getTeXMath s]]
writeExp (EArray aligns rows) = table aligns rows

table :: [Alignment] -> [ArrayLine] -> [TeX]
table as rows =
  [ControlSeq "\\begin",
   Grouped [Literal "array"],
   Grouped [Literal columnAligns],
   Token '\n'] ++
   concatMap row rows ++
   [ControlSeq "\\end",
   Grouped [Literal "array"]]
  where
    columnAligns = map alignmentToLetter as
    alignmentToLetter AlignLeft = 'l'
    alignmentToLetter AlignCenter = 'c'
    alignmentToLetter AlignRight = 'r'
    alignmentToLetter AlignDefault = 'c'

row :: ArrayLine -> [TeX]
row cells =
  concat  (intersperse [Space, Token '&', Space] (map cell cells)) ++ [Space,
  Literal "\\\\", Token '\n']
  where
    cell es = concatMap writeExp es

-- Utility

-- | Maps a length in em to the nearest bigger LaTeX space command
getSpaceCommand :: String -> String
getSpaceCommand width = snd $ fromMaybe (M.findMax spaceMap) (lookupGE (readSpace width) spaceMap)
  where
    spaceMap = M.fromList (map (\(k, ESpace s) -> (readSpace s, k)) spaceCommands)
    readSpace :: String -> Float
    readSpace s = maybe 0 fst $ listToMaybe $ reads s

lookupGE :: Ord k =>  k -> M.Map k v -> Maybe (k, v)
lookupGE k m = let (_, v, g) = M.splitLookup k m in
                    (fmap ((,) k) (v <|> (fst <$> M.minView g)))

spaceCommands :: [(String, Exp)]
spaceCommands =
           [ ("\\!", ESpace "-0.167em")
           , (""   , ESpace "0.0em")
           , ("\\,", ESpace "0.167em")
           , ("\\>", ESpace "0.222em")
           , ("\\:", ESpace "0.222em")
           , ("\\;", ESpace "0.278em")
           , ("~", ESpace "0.333em")
           , ("\\quad", ESpace "1.0em")
           , ("\\qquad", ESpace "2.0em")]


-- Text commands availible in amsmath
formats :: [String]
formats = ["\\mathrm", "\\mathit", "\\mathsf", "\\mathtt", "\\mathfrak", "\\mathcal"]

alts :: [(String, String)]
alts = [ ("\\mathbfit", "\\mathbf"), ("\\mathbfsfup", "\\mathbf"), ("\\mathbfsfit", "\\mathbf")
       , ("\\mathbfscr", "\\mathcal"), ("\\mathbffrak", "\\mathfrak"), ("\\mathsfit", "\\mathsf")]

getTeXMathTextCommand :: TextType -> String
getTeXMathTextCommand t
  | cmd `elem` formats = cmd
  | otherwise = fromMaybe "\\mathrm" (lookup cmd alts)
  where
    cmd = S.getLaTeXTextCommand t

-- Constructors

-- Fix up

removeAccentStretch :: Exp -> Exp
removeAccentStretch (EStretchy e@(ESymbol Accent _)) = e
removeAccentStretch x = x

reorderDiacritical' :: Position -> Exp -> Exp -> Exp
reorderDiacritical' p b e@(ESymbol Accent a) =
  case S.getDiacriticalCommand p a of
    Just accentCmd -> EUnary accentCmd b
    Nothing -> EBinary def e b
  where
    def = case p of
            Over -> "\\overset"
            Under -> "\\underset"
reorderDiacritical' _ _ _ = error "Must be called with Accent"

reorderDiacritical :: Exp -> Exp
reorderDiacritical (EOver b e@(ESymbol Accent _)) =
  reorderDiacritical' Over b e
reorderDiacritical (EUnder b e@(ESymbol Accent _)) =
  reorderDiacritical' Under b e
reorderDiacritical (EUnderover b e@(ESymbol Accent _) e1) =
  reorderDiacritical' Under (EOver b e1) e
reorderDiacritical (EUnderover b e1 e@(ESymbol Accent _)) =
  reorderDiacritical' Over (EUnder b e1) e
reorderDiacritical x = x

matchStretch' :: [Exp] -> Int
matchStretch'  [] = 0
matchStretch' ((EStretchy (ESymbol Open s)): xs) =
  let s' = getTeXMath s in
    case s' of {"" -> 0; _ -> 1} + (matchStretch' xs)
matchStretch' ((EStretchy (ESymbol Close s)): xs) =
  let s' = getTeXMath s in
    case s' of {"" -> 0; _ -> (-1)} + (matchStretch' xs)
matchStretch' (_:xs) = matchStretch' xs

-- Ensure that the lefts match the rights.
matchStretch :: [Exp] -> [Exp]
matchStretch es
  | n < 0 = (replicate (0 - n) $ EStretchy (ESymbol Open ".")) ++ es
  | n > 0 = es ++ (replicate n $ EStretchy (ESymbol Close "."))
  | otherwise = es
  where
    n = matchStretch' es

ms :: Exp -> Exp
ms (EGrouped xs) = EGrouped (matchStretch xs)
ms (EDelimited o c xs) = EDelimited o c (matchStretch xs)
ms (EArray as rs) = EArray as (map (map matchStretch) rs)
ms x = x

fixTree :: Exp -> Exp
fixTree = everywhere
            ( mkT ms
            . mkT reorderDiacritical
            . mkT removeAccentStretch )

-- Operator Table

getOperator :: Exp -> Maybe TeX
getOperator op = fmap ControlSeq $ lookup op operators

operators :: [(Exp, String)]
operators =
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

