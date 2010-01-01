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

{- | Functions for converting LaTeX math formulas to MathML.
-}

module Text.TeXMath ( addNamespace, texMathToMathML, DisplayType(..) )
where
import Text.TeXMath.Parser
import Text.TeXMath.MathMLWriter
import Text.XML.Light
import Text.ParserCombinators.Parsec (parse)

addNamespace :: String -> Element -> Element
addNamespace ns elt = case addNamespace' ns (Elem elt) of
                           Elem elt' -> elt' 
                           _         -> elt  -- just return original if error

addNamespace' :: String -> Content -> Content 
addNamespace' ns (Elem elt) = Elem newelem
  where newelem = elt{ elName = oldname{ qPrefix = Just ns }
                     , elAttribs = map (addAttrNs ns) $ elAttribs elt
                     , elContent = map (addNamespace' ns) $ elContent elt
                     , elLine = elLine elt }
        oldname = elName elt
        addAttrNs n attr = let k = attrKey attr
                           in  attr{ attrKey = k{ qPrefix = Just n} } 
addNamespace' _ x = x

texMathToMathML :: DisplayType -> String -> Either String Element
texMathToMathML dt inp = inp `seq`
  case parse formula "TeX math input" inp of
       Left err        -> Left (show err)
       Right v         -> Right $ toMathML dt v
