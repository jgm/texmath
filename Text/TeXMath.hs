{-
Copyright (C) 2009-2012 John MacFarlane <jgm@berkeley.edu>

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

{- | Functions for converting LaTeX math formulas to MathML.
-}

module Text.TeXMath ( texMathToMathML,
                      texMathToOMML,
                      texMathToPandoc,
                      mathMLToOMML,
                      mathMLToPandoc,
                      mathMLToLaTeX,
                      readMathML,
                      readTeXMath,
                      toTeXMath,
                      toOMML,
                      toMathML,
                      DisplayType(..),
                      )
where
import Text.TeXMath.Parser
import Text.TeXMath.MathMLParser
import Text.TeXMath.MathML
import Text.TeXMath.OMML
import Text.TeXMath.Pandoc
import Text.TeXMath.LaTeX
import Text.TeXMath.Types
import Text.XML.Light
import Text.Pandoc.Definition
import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)

-- | Convert texMath string to MathML
texMathToMathML :: DisplayType -> String -> Either String Element
texMathToMathML dt inp = inp `seq`
  toMathML dt <$> readTeXMath inp

-- | Convert texMath string to OMML (Office Math Markup Language)
texMathToOMML :: DisplayType -> String -> Either String Element
texMathToOMML dt inp = inp `seq`
  toOMML dt <$> readTeXMath inp

-- | Convert texMath to Pandoc inlines
texMathToPandoc :: DisplayType -> String -> Either String [Inline]
texMathToPandoc dt inp = inp `seq`
  fromMaybe fallback . toPandoc dt <$> readTeXMath inp
  where fallback = [Str $ delim ++ inp ++ delim]
        delim    = case dt of { DisplayInline -> "$"; DisplayBlock -> "$$" }

-- | Convert MathML to OMML (Office Math Markup Language)
mathMLToOMML :: DisplayType -> String -> Either String Element
mathMLToOMML dt inp = inp `seq`
   (toOMML dt) <$> readMathML inp

-- | Convert MathML to Pandoc inlines
mathMLToPandoc :: DisplayType -> String -> Either String [Inline]
mathMLToPandoc dt inp = inp `seq`
  fromMaybe fallback . toPandoc dt <$> readMathML inp
  where fallback = [Str $ delim ++ inp ++ delim]
        delim    = case dt of { DisplayInline -> "$"; DisplayBlock -> "$$" }

-- | Convert MathML to Pandoc Math Element
mathMLToLaTeX :: DisplayType -> String -> Either String Inline
mathMLToLaTeX dt inp = inp `seq`
  rt =<< (toTeXMath dt <$> readMathML inp)
  where
    mathType = case dt of { DisplayInline -> InlineMath;
                            DisplayBlock -> DisplayMath }
    rt s = case s of
            "" -> Left "Conversion resulted in empty string"
            _  -> Right $ Math mathType s
