{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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

{- | Functions for parsing a Typst formula to a Haskell representation.
-}

module Text.TeXMath.Readers.Typst (readTypst)
where

import Data.List (intercalate, intersperse, find)
import Control.Monad
import Data.Char (isDigit, isAscii, isLetter)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Text.Parsec hiding (label)
import Text.Parsec.Error
import Text.Parsec.Text
import Text.TeXMath.Types
import Data.Functor (($>))
import qualified Text.TeXMath.Shared as S
import Text.TeXMath.Shared (getSpaceChars)
import Data.Generics (everywhere, mkT)

-- | Parse a formula, returning a list of 'Exp'.
readTypst :: Text -> Either Text [Exp]
readTypst = undefined

