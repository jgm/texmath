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

{- | Functions for writing a parsed formula back as LaTeX.
-}

module Text.TeXMath.LaTeX (toLaTeX, showExp)
where

import qualified Data.Map as M
import Text.TeXMath.Types
import Text.TeXMath.ToUnicode
import Data.Generics (everywhere, mkT)


toLaTeX :: DisplayType -> [Exp] -> String
toLaTeX dt exprs =
  (fst delim) ++ (concat (map showExp $ exprs)) ++ (snd delim)
    where delim =  case dt of
                      DisplayBlock  -> ("\\[","\\]")
                      DisplayInline -> ("$","$")

makeText ::  TextType -> String -> String
makeText a s = tt
    where tt = case a of
                TextNormal       -> "\\mathrm{"++s++"}"
                TextBold         -> "\\mathbf{"++s++"}"
                TextItalic       -> s
                TextMonospace    -> "\\mathtt{"++s++"}"
                TextSansSerif    -> "\\mathsf{"++s++"}"
                TextDoubleStruck -> "\\mathbb{"++s++"}"
                TextScript       -> "\\mathscr"++s++"}"
                TextFraktur      -> "\\mathfrak"++s++"}"
                TextBoldItalic          -> "\\mathbfit{"++s++"}"
                TextBoldSansSerif       -> "\\mathbfsfup{"++s++"}"
                TextBoldSansSerifItalic -> "\\mathbfsfit{"++s++"}"
                TextBoldScript          -> "\\mathbfscr{"++s++"}"
                TextBoldFraktur         -> "\\mathbffrak{"++s++"}"
                TextSansSerifItalic     -> "\\mathsfit{"++s++"}"

showExp :: Exp -> String
showExp e = 
  case e of
    EGrouped [x]     -> showExp x
    EGrouped xs      -> "{" ++ concat (map showExp xs) ++ "}"
    ENumber x        -> "\\num{"++x++"}"
    EText a s        -> makeText a s
    EIdentifier x    -> x
    EMathOperator x  -> x -- Should use mathop here if it is a custom operator.
{-
    ESymbol Accent x -> accent x
    EStretchy (ESymbol Open x)  -> "\\left"++x
    EStretchy (ESymbol Close x) -> "\\right"++y
    ESymbol Open x   -> x
    ESymbol Close x  -> x
    ESymbol _ x      -> x
    ESpace x         -> spaceWidth x
    EBinary c x y    -> showBinary c x y
    ESub x y         -> "{" ++ showExp x ++ "}_{" ++ showExp y ++ "}"
    ESuper x y       -> "{" ++ showExp x ++ "}^{" ++ showExp y ++ "}"
    ESuper x y       -> unode "msup" $ map showExp [x, y]
    ESubsup x y z    -> unode "msubsup" $ map showExp [x, y, z]
    EUnder x y       -> unode "munder" $ map showExp [x, y]
    EOver x y        -> unode "mover" $ map showExp [x, y]
    EUnderover x y z -> unode "munderover" $ map showExp [x, y, z]
    EUnary c x       -> showUnary c x
    EStretchy x      -> showExp x
    EScaled s x      -> "\\scalebox{" ++ s ++ "}{" ++ showExp x ++ "}"
    EArray as ls     -> makeArray as ls
    EDelimited start end xs -> mrow $
                       [ makeStretchy (unode "mo" start) | not (null start) ] ++
                       map showExp xs ++
                       [ makeStretchy (unode "mo" end) | not (null end) ] 
    x                -> error $ "showExp encountered " ++ show x
                        -- note: EUp, EDown, EDownup should be removed by handleDownup
-}  
