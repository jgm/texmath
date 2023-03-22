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

{- | Functions for converting between different representations of
mathematical formulas.

Also note that in general @writeLaTeX . readLaTeX /= id@.

A typical use is to combine together a reader and writer.

> import Control.Applicative ((<$>))
> import Data.Text (Text)
> import Text.TeXMath (writeMathML, readTeX)
>
> texMathToMathML :: DisplayType -> Text -> Either Text Element
> texMathToMathML dt s = writeMathML dt <$> readTeX s

It is also possible to manipulate the AST using 'Data.Generics'. For
example, if you wanted to replace all occurences of the identifier
x in your expression, you do could do so with the following
script.

@
&#x7b;-\# LANGUAGE OverloadedStrings -\#&#x7d;

import Control.Applicative ((\<$\>))
import Data.Text (Text)
import Data.Generics (everywhere, mkT)
import Text.TeXMath (writeMathML, readTeX)
import Text.TeXMath.Types
import Text.XML.Light (Element)

changeIdent :: Exp -> Exp
changeIdent (EIdentifier "x") = EIdentifier "y"
changeIdent e = e

texToMMLWithChangeIdent :: DisplayType -> Text -> Either Text Element
texToMMLWithChangeIdent dt s =
  writeMathML dt . everywhere (mkT changeIdent) \<$\> readTeX s
@
-}

module Text.TeXMath ( readMathML,
                      readOMML,
                      readTeX,
                      writeTeX,
                      writeTeXWith,
                      addLaTeXEnvironment,
                      writeEqn,
                      writeTypst,
                      writeOMML,
                      writeMathML,
                      writePandoc,
                      DisplayType(..),
                      Exp
                      )
where
import Text.TeXMath.Readers.TeX
import Text.TeXMath.Readers.MathML
import Text.TeXMath.Readers.OMML
import Text.TeXMath.Writers.MathML
import Text.TeXMath.Writers.OMML
import Text.TeXMath.Writers.Pandoc
import Text.TeXMath.Writers.TeX
import Text.TeXMath.Writers.Eqn
import Text.TeXMath.Writers.Typst
import Text.TeXMath.Types
