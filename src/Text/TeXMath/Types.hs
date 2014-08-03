{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}
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
                           TextType(..), Alignment(..), DisplayType(..),
                           Operator(..), FormType(..), Record(..),
                           Property, Position(..), Env, defaultEnv, InEDelimited)
where

import Data.Generics

data TeXSymbolType = Ord | Op | Bin | Rel | Open | Close | Pun | Accent
                     | Fence | TOver | TUnder | Alpha | BotAccent | Rad
                     deriving (Show, Read, Eq, Data, Typeable)

data Alignment = AlignLeft | AlignCenter | AlignRight | AlignDefault
                 deriving (Show, Read, Eq, Data, Typeable)

type ArrayLine = [[Exp]]

data Exp =
    ENumber String  -- ^ A number (@\<mn\>@ in MathML).
  | EGrouped [Exp]  -- ^ A group of expressions that function as a unit
                    -- (e.g. @{...}@) in TeX, @\<mrow\>...\</mrow\>@ in MathML.
  | EDelimited String String [InEDelimited] -- ^ A group of expressions inside
                    -- paired open and close delimiters (which may in some
                    -- cases be null).
  | EIdentifier String  -- ^ An identifier, e.g. a variable (@\<mi\>...\</mi\>@
                    -- in MathML.  Note that MathML tends to use @\<mi\>@ tags
                    -- for "sin" and other mathematical operators; these
                    -- are represented as 'EMathOperator' in TeXMath.
  | EMathOperator String  -- ^ A spelled-out operator like @lim@ or @sin@.
  | ESymbol TeXSymbolType String  -- ^ A symbol.
  | ESpace Double -- ^ A space, with the width specified in em.
  | ESub Exp Exp  -- ^ An expression with a subscript.  First argument is base,
                  -- second subscript.
  | ESuper Exp Exp -- ^ An expresion with a superscript.  First argument is base,
                   -- second subscript.
  | ESubsup Exp Exp Exp  -- ^ An expression with both a sub and a superscript.
                   -- First argument is base, second subscript, third
                   -- superscript.
  | EOver Bool Exp Exp  -- ^ An expression with something over it.
                        -- The first argument is True if the formula is
                        -- "convertible:" that is, if the material over the
                        -- formula should appear as a regular superscript in
                        -- inline math. The second argument is the base,
                        -- the third the expression that goes over it.
  | EUnder Bool Exp Exp -- ^ An expression with something under it.
                        -- The arguments work as in @EOver@.
  | EUnderover Bool Exp Exp Exp  -- ^ An expression with something over and
                       -- something under it.
  | EUnary String Exp  -- ^ A unary operator. The first argument is a LaTeX
                       -- operator name like @\\sqrt@; the other is its argument.
  | EBinary String Exp Exp -- ^ A binary operator.  The first argument is a
                  -- LaTeX operator name like @\\root@; the others are the
                  -- arguments.
  | EPhantom Exp  -- ^ A "phantom" operator that takes space but doesn't display.
  | EScaled Double Exp -- ^ An expression that is scaled to some factor
                  -- of its normal size.
  | EArray [Alignment] [ArrayLine] -- ^ An array or matrix.  The first argument
                  -- specifies the alignments of the columns; the second gives
                  -- the contents of the lines.  All of these lists should be
                  -- the same length.
  | EText TextType String  -- ^ Some normal text, possibly styled.
  | EStyled TextType [Exp] -- ^  A group of styled expressions.
  deriving (Show, Read, Eq, Data, Typeable)

-- | An @EDelimited@ element contains a string of ordinary expressions
-- (represented here as @Right@ values) or fences (represented here as
-- @Left@, and in LaTeX using @\mid@).
type InEDelimited = Either Middle Exp
type Middle = String

data DisplayType = DisplayBlock  -- ^ A displayed formula.
                 | DisplayInline  -- ^ A formula rendered inline in text.
                 deriving (Show, Eq)

data TextType = TextNormal
              | TextBold
              | TextItalic
              | TextMonospace
              | TextSansSerif
              | TextDoubleStruck
              | TextScript
              | TextFraktur
              | TextBoldItalic
              | TextSansSerifBold
              | TextSansSerifBoldItalic
              | TextBoldScript
              | TextBoldFraktur
              | TextSansSerifItalic
              deriving (Show, Ord, Read, Eq, Data, Typeable)

data FormType = FPrefix | FPostfix | FInfix deriving (Show, Ord, Eq)

type Property = String

-- | A record of the MathML dictionary as defined
-- <http://www.w3.org/TR/MathML3/appendixc.html in the specification>
data Operator = Operator
                  { oper :: String -- ^ Operator
                  , description :: String -- ^ Plain English Description
                  , form :: FormType -- ^ Whether Prefix, Postfix or Infix
                  , priority :: Int -- ^ Default priority for implicit
                                    --   nesting
                  , lspace :: Int -- ^ Default Left Spacing
                  , rspace :: Int -- ^ Default Right Spacing
                  , properties :: [Property] -- ^ List of MathML properties
                  }
                  deriving (Show)

-- | A record of the Unicode to LaTeX lookup table
-- a full descripton can be seen
-- <http://milde.users.sourceforge.net/LUCR/Math/data/unimathsymbols.txt
-- here>
data Record = Record { uchar :: Char -- ^ Unicode Character
                     , commands :: [(String, String)] -- ^ LaTeX commands (package, command)
                     , category :: TeXSymbolType -- ^ TeX math category
                     , comments :: String -- ^ Plain english description
                     } deriving (Show)

data Position = Under | Over

-- | List of available packages
type Env = [String]

-- | Contains @amsmath@ and @amssymbol@
defaultEnv :: [String]
defaultEnv = ["amsmath", "amssymbol"]
