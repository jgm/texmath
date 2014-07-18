{-# LANGUAGE CPP #-}
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
  , getSpaceCommand
  , getLaTeXTextCommand
  , getScalerCommand
  , getScalerValue
  , getDiacriticalCommand
  , getDiacriticalCons
  ) where


import Text.TeXMath.Types
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Applicative ((<$>), (<|>)) 
import Control.Monad (guard)

-- | Maps TextType to the corresponding MathML mathvariant
getMMLType :: TextType -> String
getMMLType t = fromMaybe "normal" (fst <$> M.lookup t (M.fromList types))

-- | Maps TextType to corresponding LaTeX command
getLaTeXTextCommand :: TextType -> String
getLaTeXTextCommand t = fromMaybe "\\mathrm" (snd <$> M.lookup t (M.fromList types))

-- | Maps MathML mathvariant to the corresponing TextType
getTextType :: String -> TextType
getTextType s = fromMaybe TextNormal (M.lookup s revTypes)
  where
    revTypes = M.fromList (map (\(k,v) -> (fst v,k)) types)

lookupGE :: Ord k =>  k -> M.Map k v -> Maybe (k, v)
lookupGE k m = let (_, v, g) = M.splitLookup k m in
                    (fmap ((,) k) (v <|> (fst <$> M.minView g)))

-- | Maps a length in em to the nearest bigger LaTeX space command
getSpaceCommand :: String -> String 
getSpaceCommand width = snd $ fromMaybe (M.findMax spaceMap) (lookupGE (readSpace width) spaceMap)
  where 
    spaceMap = M.fromList (map (\(k, ESpace s) -> (readSpace s, k)) spaceCommands)
    readSpace :: String -> Float
    readSpace s = maybe 0 fst $ listToMaybe $ reads s 

-- | Maps a LaTeX scaling command to the percentage scaling 
getScalerCommand :: String -> Maybe String
getScalerCommand width = (M.lookup width scalerMap)
  where
    scalerMap = M.fromList (reverseKeys scalers)

-- | Gets percentage scaling from LaTeX scaling command
getScalerValue :: String -> Maybe String 
getScalerValue command = M.lookup command scalerMap
  where 
    scalerMap = M.fromList scalers

-- | Returns the correct constructor given a LaTeX command
getDiacriticalCons :: String -> Maybe (Exp -> Exp)
getDiacriticalCons command = 
    f <$> M.lookup command diaMap
  where
    diaMap = M.fromList (reverseKeys diacriticals)
    f s e = (if command `elem` under then EUnder else EOver) e (ESymbol Accent s)

-- | Given a diacritical mark, returns the corresponding LaTeX command
getDiacriticalCommand  :: Position -> String -> Maybe String
getDiacriticalCommand pos symbol = do
  command <- M.lookup symbol diaMap
  guard (not $ command `elem` unavailible) 
  let below = command `elem` under
  case pos of
    Under -> if below then Just command else Nothing
    Over -> if not below then Just command else Nothing
  where
    diaMap = M.fromList diacriticals

reverseKeys :: [(a, b)] -> [(b, a)]
reverseKeys = map (\(k,v) -> (v, k)) 

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


--TextType to (MathML, LaTeX)
types :: [(TextType, (String, String))]
types = 
  [ ( TextNormal       , ("normal", "\\mathrm"))
  , ( TextBold         , ("bold", "\\mathbf"))
  , ( TextItalic       , ("italic","\\mathit"))
  , ( TextMonospace    , ("monospace","\\mathtt"))
  , ( TextSansSerif    , ("sans-serif","\\mathsf"))
  , ( TextDoubleStruck , ("double-struck","\\mathbb"))
  , ( TextScript       , ("script","\\mathcal"))
  , ( TextFraktur      , ("fraktur","\\mathfrak"))
  , ( TextBoldItalic          , ("bold-italic","\\mathbfit"))
  , ( TextBoldSansSerif       , ("bold-sans-serif","\\mathbfsfup"))
  , ( TextBoldSansSerifItalic , ("sans-serif-bold-italic","\\mathbfsfit"))
  , ( TextBoldScript          , ("bold-script","\\mathbfscr"))
  , ( TextBoldFraktur         , ("bold-fraktur","\\mathbffrak"))
  , ( TextSansSerifItalic     , ("sans-serif-italic","\\mathsfit")) ]


scalers :: [(String, String)]
scalers = 
          [ ("\\bigg", "2.2")
          , ("\\Bigg", "2.9")
          , ("\\big", "1.2")
          , ("\\Big", "1.6")
          , ("\\biggr", "2.2")
          , ("\\Biggr", "2.9")
          , ("\\bigr", "1.2")
          , ("\\Bigr", "1.6")
          , ("\\biggl", "2.2")
          , ("\\Biggl", "2.9")
          , ("\\bigl", "1.2")]

-- Accents which go under the character
under :: [String]
under = ["\\underbrace", "\\underline", "\\underbar", "\\underbracket"]

-- We want to parse these but we can't represent them in LaTeX
unavailible :: [String]
unavailible = ["\\overbracket", "\\underbracket"]


diacriticals :: [(String, String)]
diacriticals = 
               [ ("\x00B4", "\\acute")
               , (("\x0060", "\\grave"))
               , (("\x02D8", "\\breve"))
               , (("\x02C7", "\\check"))
               , (("\x307", "\\dot"))
               , (("\x308", "\\ddot"))
               , (("\x00B0", "\\mathring"))
               , (("\x20D7", "\\vec"))
               , (("\x20D7", "\\overrightarrow"))
               , (("\x20D6", "\\overleftarrow"))
               , (("\x005E", "\\hat"))
               , (("\x0302", "\\widehat"))
               , (("\x02C6", "\\widehat"))
               , (("\x0303", "\\tilde"))
               , (("\x02DC", "\\widetilde"))
               , (("\x203E", "\\bar"))
               , (("\x23DE", "\\overbrace"))
               , (("\xFE37", "\\overbrace"))
               , (("\x23B4", "\\overbracket")) -- Only availible in mathtools
               , (("\x00AF", "\\overline"))
               , (("\x23DF", "\\underbrace"))
               , (("\xFE38", "\\underbrace"))
               , (("\x23B5", "\\underbracket")) -- mathtools
               , (("\x0332", "\\underline"))
               , (("\x0333", "\\underbar"))
               ]

