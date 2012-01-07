{-# LANGUAGE DeriveDataTypeable #-}
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

{- | Types for representing a structured formula.
-}

module Text.TeXMath.Types (Exp(..), TeXSymbolType(..), ArrayLine,
                           TextType(..), Alignment(..), DisplayType(..))
where

import Data.Generics

data TeXSymbolType = Ord | Op | Bin | Rel | Open | Close | Pun | Accent
                     deriving (Show, Read, Eq, Data, Typeable)

data Alignment = AlignLeft | AlignCenter | AlignRight | AlignDefault
                 deriving (Show, Read, Eq, Data, Typeable)

type ArrayLine = [[Exp]]

data Exp =
    ENumber String
  | EGrouped [Exp]
  | EDelimited String String [Exp]
  | EIdentifier String
  | EMathOperator String
  | ESymbol TeXSymbolType String
  | ESpace String
  | EBinary String Exp Exp
  | ESub Exp Exp
  | ESuper Exp Exp
  | ESubsup Exp Exp Exp
  | EOver Exp Exp
  | EUnder Exp Exp
  | EUnderover Exp Exp Exp
  | EUp Exp Exp
  | EDown Exp Exp
  | EDownup Exp Exp Exp
  | EUnary String Exp
  | EScaled String Exp
  | EStretchy Exp
  | EArray [Alignment] [ArrayLine]
  | EText TextType String
  deriving (Show, Read, Eq, Data, Typeable)

data DisplayType = DisplayBlock
                 | DisplayInline
                 deriving Show

data TextType = TextNormal
              | TextBold
              | TextItalic
              | TextMonospace
              | TextSansSerif
              | TextDoubleStruck
              | TextScript
              | TextFraktur
              deriving (Show, Read, Eq, Data, Typeable)
