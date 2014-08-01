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
{- |
Dictionary of operators to MathML attributes as specified by the W3C standard.

The original file can be downloaded from <http://www.w3.org/TR/xml-entity-names/#source here>
-}

module Text.TeXMath.Readers.MathML.MMLDict (getMathMLOperator, operators) where

import Text.TeXMath.Types
import qualified Data.Map as M
import Data.Monoid (First(..), mconcat)

dict :: M.Map (String, FormType) Operator
dict = M.fromList (map (\o -> ((oper o, form o), o)) operators)

-- | Tries to find the 'Operator' record based on a given position. If
-- there is no exact match then the positions will be tried in the
-- following order (Infix, Postfix, Prefix) with the first match (if any) being returned.
getMathMLOperator :: String -> FormType -> Maybe Operator
getMathMLOperator s p =
  getFirst $ mconcat $ (map (\x -> First $ M.lookup (s, x) dict) lookupOrder)
  where
    lookupOrder = [p, FInfix, FPostfix, FPrefix]

-- | A table of all operators as defined by the MathML operator dictionary.
operators :: [Operator]
operators =
  [ Operator {oper = "!", description = "EXCLAMATION MARK", form = FPostfix, priority = 810, lspace = 1, rspace = 0, properties = []}
  , Operator {oper = "!!", description = "MULTIPLE CHARACTER OPERATOR: !!", form = FPostfix, priority = 810, lspace = 1, rspace = 0, properties = []}
  , Operator {oper = "!=", description = "MULTIPLE CHARACTER OPERATOR: !=", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\"", description = "QUOTATION MARK", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "%", description = "PERCENT SIGN", form = FInfix, priority = 640, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "&", description = "AMPERSAND", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "&&", description = "MULTIPLE CHARACTER OPERATOR: &&", form = FInfix, priority = 200, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "'", description = "APOSTROPHE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "(", description = "LEFT PARENTHESIS", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = ")", description = "RIGHT PARENTHESIS", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "*", description = "ASTERISK", form = FInfix, priority = 390, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "**", description = "MULTIPLE CHARACTER OPERATOR: **", form = FInfix, priority = 780, lspace = 1, rspace = 1, properties = []}
  , Operator {oper = "*=", description = "MULTIPLE CHARACTER OPERATOR: *=", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "+", description = "PLUS SIGN", form = FInfix, priority = 275, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "+", description = "PLUS SIGN", form = FPrefix, priority = 275, lspace = 0, rspace = 1, properties = []}
  , Operator {oper = "++", description = "MULTIPLE CHARACTER OPERATOR: ++", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "+=", description = "MULTIPLE CHARACTER OPERATOR: +=", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = ",", description = "COMMA", form = FInfix, priority = 40, lspace = 0, rspace = 3, properties = ["separator"]}
  , Operator {oper = "-", description = "HYPHEN-MINUS", form = FInfix, priority = 275, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "-", description = "HYPHEN-MINUS", form = FPrefix, priority = 275, lspace = 0, rspace = 1, properties = []}
  , Operator {oper = "--", description = "MULTIPLE CHARACTER OPERATOR: --", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "-=", description = "MULTIPLE CHARACTER OPERATOR: -=", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "->", description = "MULTIPLE CHARACTER OPERATOR: ->", form = FInfix, priority = 90, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = ".", description = "FULL STOP", form = FInfix, priority = 390, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "..", description = "MULTIPLE CHARACTER OPERATOR: ..", form = FPostfix, priority = 100, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "...", description = "MULTIPLE CHARACTER OPERATOR: ...", form = FPostfix, priority = 100, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "/", description = "SOLIDUS", form = FInfix, priority = 660, lspace = 1, rspace = 1, properties = []}
  , Operator {oper = "//", description = "MULTIPLE CHARACTER OPERATOR: //", form = FInfix, priority = 820, lspace = 1, rspace = 1, properties = []}
  , Operator {oper = "/=", description = "MULTIPLE CHARACTER OPERATOR: /=", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = ":", description = "COLON", form = FInfix, priority = 100, lspace = 1, rspace = 2, properties = []}
  , Operator {oper = ":=", description = "MULTIPLE CHARACTER OPERATOR: :=", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = ";", description = "SEMICOLON", form = FInfix, priority = 30, lspace = 0, rspace = 3, properties = ["separator"]}
  , Operator {oper = "<", description = "LESS-THAN SIGN", form = FInfix, priority = 245, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "<=", description = "MULTIPLE CHARACTER OPERATOR: <=", form = FInfix, priority = 241, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "<>", description = "MULTIPLE CHARACTER OPERATOR: <>", form = FInfix, priority = 780, lspace = 1, rspace = 1, properties = []}
  , Operator {oper = "=", description = "EQUALS SIGN", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "==", description = "MULTIPLE CHARACTER OPERATOR: ==", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = ">", description = "GREATER-THAN SIGN", form = FInfix, priority = 243, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = ">=", description = "MULTIPLE CHARACTER OPERATOR: >=", form = FInfix, priority = 243, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "?", description = "QUESTION MARK", form = FInfix, priority = 835, lspace = 1, rspace = 1, properties = []}
  , Operator {oper = "@", description = "COMMERCIAL AT", form = FInfix, priority = 825, lspace = 1, rspace = 1, properties = []}
  , Operator {oper = "[", description = "LEFT SQUARE BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\\", description = "REVERSE SOLIDUS", form = FInfix, priority = 650, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "]", description = "RIGHT SQUARE BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "^", description = "CIRCUMFLEX ACCENT", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "^", description = "CIRCUMFLEX ACCENT", form = FInfix, priority = 780, lspace = 1, rspace = 1, properties = []}
  , Operator {oper = "_", description = "LOW LINE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "_", description = "LOW LINE", form = FInfix, priority = 900, lspace = 1, rspace = 1, properties = []}
  , Operator {oper = "`", description = "GRAVE ACCENT", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "{", description = "LEFT CURLY BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "|", description = "VERTICAL LINE", form = FInfix, priority = 270, lspace = 2, rspace = 2, properties = ["stretchy","symmetric","fence"]}
  , Operator {oper = "|", description = "VERTICAL LINE", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy"]}
  , Operator {oper = "|", description = "VERTICAL LINE", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy"]}
  , Operator {oper = "||", description = "MULTIPLE CHARACTER OPERATOR: ||", form = FInfix, priority = 270, lspace = 2, rspace = 2, properties = ["stretchy","symmetric","fence"]}
  , Operator {oper = "||", description = "MULTIPLE CHARACTER OPERATOR: ||", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy"]}
  , Operator {oper = "||", description = "MULTIPLE CHARACTER OPERATOR: ||", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy"]}
  , Operator {oper = "|||", description = "MULTIPLE CHARACTER OPERATOR: |||", form = FInfix, priority = 270, lspace = 2, rspace = 2, properties = ["stretchy","symmetric","fence"]}
  , Operator {oper = "|||", description = "MULTIPLE CHARACTER OPERATOR: |||", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy"]}
  , Operator {oper = "|||", description = "MULTIPLE CHARACTER OPERATOR: |||", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy"]}
  , Operator {oper = "}", description = "RIGHT CURLY BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "~", description = "TILDE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\168", description = "DIAERESIS", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\170", description = "FEMININE ORDINAL INDICATOR", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\172", description = "NOT SIGN", form = FPrefix, priority = 680, lspace = 2, rspace = 1, properties = []}
  , Operator {oper = "\175", description = "MACRON", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\176", description = "DEGREE SIGN", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "\177", description = "PLUS-MINUS SIGN", form = FInfix, priority = 275, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\177", description = "PLUS-MINUS SIGN", form = FPrefix, priority = 275, lspace = 0, rspace = 1, properties = []}
  , Operator {oper = "\178", description = "SUPERSCRIPT TWO", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\179", description = "SUPERSCRIPT THREE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\180", description = "ACUTE ACCENT", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\183", description = "MIDDLE DOT", form = FInfix, priority = 400, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\184", description = "CEDILLA", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\185", description = "SUPERSCRIPT ONE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\186", description = "MASCULINE ORDINAL INDICATOR", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\215", description = "MULTIPLICATION SIGN", form = FInfix, priority = 390, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\247", description = "DIVISION SIGN", form = FInfix, priority = 660, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\710", description = "MODIFIER LETTER CIRCUMFLEX ACCENT", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\711", description = "CARON", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\713", description = "MODIFIER LETTER MACRON", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\714", description = "MODIFIER LETTER ACUTE ACCENT", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\715", description = "MODIFIER LETTER GRAVE ACCENT", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\717", description = "MODIFIER LETTER LOW MACRON", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\728", description = "BREVE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\729", description = "DOT ABOVE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\730", description = "RING ABOVE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\732", description = "SMALL TILDE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\733", description = "DOUBLE ACUTE ACCENT", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\759", description = "MODIFIER LETTER LOW TILDE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\770", description = "COMBINING CIRCUMFLEX ACCENT", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\785", description = "COMBINING INVERTED BREVE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\1014", description = "GREEK REVERSED LUNATE EPSILON SYMBOL", form = FInfix, priority = 110, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8214", description = "DOUBLE VERTICAL LINE", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["fence","stretchy"]}
  , Operator {oper = "\8214", description = "DOUBLE VERTICAL LINE", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["fence","stretchy"]}
  , Operator {oper = "\8216", description = "LEFT SINGLE QUOTATION MARK", form = FPrefix, priority = 10, lspace = 0, rspace = 0, properties = ["fence","mirrorable"]}
  , Operator {oper = "\8217", description = "RIGHT SINGLE QUOTATION MARK", form = FPostfix, priority = 10, lspace = 0, rspace = 0, properties = ["fence","mirrorable"]}
  , Operator {oper = "\8218", description = "SINGLE LOW-9 QUOTATION MARK", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","mirrorable"]}
  , Operator {oper = "\8219", description = "SINGLE HIGH-REVERSED-9 QUOTATION MARK", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","mirrorable"]}
  , Operator {oper = "\8220", description = "LEFT DOUBLE QUOTATION MARK", form = FPrefix, priority = 10, lspace = 0, rspace = 0, properties = ["fence","mirrorable"]}
  , Operator {oper = "\8221", description = "RIGHT DOUBLE QUOTATION MARK", form = FPostfix, priority = 10, lspace = 0, rspace = 0, properties = ["fence","mirrorable"]}
  , Operator {oper = "\8222", description = "DOUBLE LOW-9 QUOTATION MARK", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","mirrorable"]}
  , Operator {oper = "\8223", description = "DOUBLE HIGH-REVERSED-9 QUOTATION MARK", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","mirrorable"]}
  , Operator {oper = "\8226", description = "BULLET", form = FInfix, priority = 390, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8230", description = "HORIZONTAL ELLIPSIS", form = FInfix, priority = 150, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "\8242", description = "PRIME", form = FPostfix, priority = 800, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "\8243", description = "DOUBLE PRIME", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\8244", description = "TRIPLE PRIME", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\8245", description = "REVERSED PRIME", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\8246", description = "REVERSED DOUBLE PRIME", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\8247", description = "REVERSED TRIPLE PRIME", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\8254", description = "OVERLINE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\8259", description = "HYPHEN BULLET", form = FInfix, priority = 390, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8260", description = "FRACTION SLASH", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = ["stretchy"]}
  , Operator {oper = "\8279", description = "QUADRUPLE PRIME", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\8289", description = "FUNCTION APPLICATION", form = FInfix, priority = 850, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "\8290", description = "INVISIBLE TIMES", form = FInfix, priority = 390, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "\8291", description = "INVISIBLE SEPARATOR", form = FInfix, priority = 40, lspace = 0, rspace = 0, properties = ["separator"]}
  , Operator {oper = "\8292", description = "INVISIBLE PLUS", form = FInfix, priority = 880, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "\8411", description = "COMBINING THREE DOTS ABOVE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\8412", description = "COMBINING FOUR DOTS ABOVE", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent"]}
  , Operator {oper = "\8517", description = "DOUBLE-STRUCK ITALIC CAPITAL D", form = FPrefix, priority = 845, lspace = 2, rspace = 1, properties = []}
  , Operator {oper = "\8518", description = "DOUBLE-STRUCK ITALIC SMALL D", form = FPrefix, priority = 845, lspace = 2, rspace = 0, properties = []}
  , Operator {oper = "\8592", description = "LEFTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent","stretchy"]}
  , Operator {oper = "\8593", description = "UPWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8594", description = "RIGHTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8595", description = "DOWNWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8596", description = "LEFT RIGHT ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8597", description = "UP DOWN ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8598", description = "NORTH WEST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8599", description = "NORTH EAST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8600", description = "SOUTH EAST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8601", description = "SOUTH WEST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8602", description = "LEFTWARDS ARROW WITH STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8603", description = "RIGHTWARDS ARROW WITH STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8604", description = "LEFTWARDS WAVE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8605", description = "RIGHTWARDS WAVE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8606", description = "LEFTWARDS TWO HEADED ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8607", description = "UPWARDS TWO HEADED ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8608", description = "RIGHTWARDS TWO HEADED ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8609", description = "DOWNWARDS TWO HEADED ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8610", description = "LEFTWARDS ARROW WITH TAIL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8611", description = "RIGHTWARDS ARROW WITH TAIL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8612", description = "LEFTWARDS ARROW FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8613", description = "UPWARDS ARROW FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8614", description = "RIGHTWARDS ARROW FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8615", description = "DOWNWARDS ARROW FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8616", description = "UP DOWN ARROW WITH BASE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8617", description = "LEFTWARDS ARROW WITH HOOK", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8618", description = "RIGHTWARDS ARROW WITH HOOK", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8619", description = "LEFTWARDS ARROW WITH LOOP", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8620", description = "RIGHTWARDS ARROW WITH LOOP", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8621", description = "LEFT RIGHT WAVE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8622", description = "LEFT RIGHT ARROW WITH STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8623", description = "DOWNWARDS ZIGZAG ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8624", description = "UPWARDS ARROW WITH TIP LEFTWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8625", description = "UPWARDS ARROW WITH TIP RIGHTWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8626", description = "DOWNWARDS ARROW WITH TIP LEFTWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8627", description = "DOWNWARDS ARROW WITH TIP RIGHTWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8628", description = "RIGHTWARDS ARROW WITH CORNER DOWNWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8629", description = "DOWNWARDS ARROW WITH CORNER LEFTWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8630", description = "ANTICLOCKWISE TOP SEMICIRCLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8631", description = "CLOCKWISE TOP SEMICIRCLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8632", description = "NORTH WEST ARROW TO LONG BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8633", description = "LEFTWARDS ARROW TO BAR OVER RIGHTWARDS ARROW TO BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8634", description = "ANTICLOCKWISE OPEN CIRCLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8635", description = "CLOCKWISE OPEN CIRCLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8636", description = "LEFTWARDS HARPOON WITH BARB UPWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8637", description = "LEFTWARDS HARPOON WITH BARB DOWNWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8638", description = "UPWARDS HARPOON WITH BARB RIGHTWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8639", description = "UPWARDS HARPOON WITH BARB LEFTWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8640", description = "RIGHTWARDS HARPOON WITH BARB UPWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8641", description = "RIGHTWARDS HARPOON WITH BARB DOWNWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8642", description = "DOWNWARDS HARPOON WITH BARB RIGHTWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8643", description = "DOWNWARDS HARPOON WITH BARB LEFTWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8644", description = "RIGHTWARDS ARROW OVER LEFTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8645", description = "UPWARDS ARROW LEFTWARDS OF DOWNWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8646", description = "LEFTWARDS ARROW OVER RIGHTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8647", description = "LEFTWARDS PAIRED ARROWS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8648", description = "UPWARDS PAIRED ARROWS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8649", description = "RIGHTWARDS PAIRED ARROWS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8650", description = "DOWNWARDS PAIRED ARROWS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8651", description = "LEFTWARDS HARPOON OVER RIGHTWARDS HARPOON", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8652", description = "RIGHTWARDS HARPOON OVER LEFTWARDS HARPOON", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8653", description = "LEFTWARDS DOUBLE ARROW WITH STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8654", description = "LEFT RIGHT DOUBLE ARROW WITH STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8655", description = "RIGHTWARDS DOUBLE ARROW WITH STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8656", description = "LEFTWARDS DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8657", description = "UPWARDS DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8658", description = "RIGHTWARDS DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8659", description = "DOWNWARDS DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8660", description = "LEFT RIGHT DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8661", description = "UP DOWN DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8662", description = "NORTH WEST DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8663", description = "NORTH EAST DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8664", description = "SOUTH EAST DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8665", description = "SOUTH WEST DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8666", description = "LEFTWARDS TRIPLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8667", description = "RIGHTWARDS TRIPLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8668", description = "LEFTWARDS SQUIGGLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8669", description = "RIGHTWARDS SQUIGGLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8670", description = "UPWARDS ARROW WITH DOUBLE STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8671", description = "DOWNWARDS ARROW WITH DOUBLE STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8672", description = "LEFTWARDS DASHED ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8673", description = "UPWARDS DASHED ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8674", description = "RIGHTWARDS DASHED ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8675", description = "DOWNWARDS DASHED ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8676", description = "LEFTWARDS ARROW TO BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8677", description = "RIGHTWARDS ARROW TO BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8678", description = "LEFTWARDS WHITE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8679", description = "UPWARDS WHITE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8680", description = "RIGHTWARDS WHITE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8681", description = "DOWNWARDS WHITE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8682", description = "UPWARDS WHITE ARROW FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8683", description = "UPWARDS WHITE ARROW ON PEDESTAL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8684", description = "UPWARDS WHITE ARROW ON PEDESTAL WITH HORIZONTAL BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8685", description = "UPWARDS WHITE ARROW ON PEDESTAL WITH VERTICAL BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8686", description = "UPWARDS WHITE DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8687", description = "UPWARDS WHITE DOUBLE ARROW ON PEDESTAL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8688", description = "RIGHTWARDS WHITE ARROW FROM WALL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8689", description = "NORTH WEST ARROW TO CORNER", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8690", description = "SOUTH EAST ARROW TO CORNER", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8691", description = "UP DOWN WHITE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8692", description = "RIGHT ARROW WITH SMALL CIRCLE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8693", description = "DOWNWARDS ARROW LEFTWARDS OF UPWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\8694", description = "THREE RIGHTWARDS ARROWS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8695", description = "LEFTWARDS ARROW WITH VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8696", description = "RIGHTWARDS ARROW WITH VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8697", description = "LEFT RIGHT ARROW WITH VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8698", description = "LEFTWARDS ARROW WITH DOUBLE VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8699", description = "RIGHTWARDS ARROW WITH DOUBLE VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8700", description = "LEFT RIGHT ARROW WITH DOUBLE VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\8701", description = "LEFTWARDS OPEN-HEADED ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8702", description = "RIGHTWARDS OPEN-HEADED ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8703", description = "LEFT RIGHT OPEN-HEADED ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\8704", description = "FOR ALL", form = FPrefix, priority = 230, lspace = 2, rspace = 1, properties = []}
  , Operator {oper = "\8705", description = "COMPLEMENT", form = FInfix, priority = 240, lspace = 1, rspace = 2, properties = []}
  , Operator {oper = "\8706", description = "PARTIAL DIFFERENTIAL", form = FPrefix, priority = 740, lspace = 2, rspace = 1, properties = []}
  , Operator {oper = "\8707", description = "THERE EXISTS", form = FPrefix, priority = 230, lspace = 2, rspace = 1, properties = []}
  , Operator {oper = "\8708", description = "THERE DOES NOT EXIST", form = FPrefix, priority = 230, lspace = 2, rspace = 1, properties = []}
  , Operator {oper = "\8710", description = "INCREMENT", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\8711", description = "NABLA", form = FPrefix, priority = 740, lspace = 2, rspace = 1, properties = []}
  , Operator {oper = "\8712", description = "ELEMENT OF", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8713", description = "NOT AN ELEMENT OF", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8714", description = "SMALL ELEMENT OF", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8715", description = "CONTAINS AS MEMBER", form = FInfix, priority = 160, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8716", description = "DOES NOT CONTAIN AS MEMBER", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8717", description = "SMALL CONTAINS AS MEMBER", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8718", description = "END OF PROOF", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\8719", description = "N-ARY PRODUCT", form = FPrefix, priority = 350, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\8720", description = "N-ARY COPRODUCT", form = FPrefix, priority = 350, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\8721", description = "N-ARY SUMMATION", form = FPrefix, priority = 290, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits","mirrorable"]}
  , Operator {oper = "\8722", description = "MINUS SIGN", form = FInfix, priority = 275, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8722", description = "MINUS SIGN", form = FPrefix, priority = 275, lspace = 0, rspace = 1, properties = []}
  , Operator {oper = "\8723", description = "MINUS-OR-PLUS SIGN", form = FInfix, priority = 275, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8723", description = "MINUS-OR-PLUS SIGN", form = FPrefix, priority = 275, lspace = 0, rspace = 1, properties = []}
  , Operator {oper = "\8724", description = "DOT PLUS", form = FInfix, priority = 275, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8725", description = "DIVISION SLASH", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = ["stretchy","mirrorable"]}
  , Operator {oper = "\8726", description = "SET MINUS", form = FInfix, priority = 650, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8727", description = "ASTERISK OPERATOR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8728", description = "RING OPERATOR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8729", description = "BULLET OPERATOR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8730", description = "SQUARE ROOT", form = FPrefix, priority = 845, lspace = 1, rspace = 1, properties = ["stretchy","mirrorable"]}
  , Operator {oper = "\8731", description = "CUBE ROOT", form = FPrefix, priority = 845, lspace = 1, rspace = 1, properties = []}
  , Operator {oper = "\8732", description = "FOURTH ROOT", form = FPrefix, priority = 845, lspace = 1, rspace = 1, properties = []}
  , Operator {oper = "\8733", description = "PROPORTIONAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8735", description = "RIGHT ANGLE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8736", description = "ANGLE", form = FPrefix, priority = 670, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "\8737", description = "MEASURED ANGLE", form = FPrefix, priority = 670, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "\8738", description = "SPHERICAL ANGLE", form = FPrefix, priority = 670, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "\8739", description = "DIVIDES", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8740", description = "DOES NOT DIVIDE", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8741", description = "PARALLEL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8742", description = "NOT PARALLEL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8743", description = "LOGICAL AND", form = FInfix, priority = 200, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8744", description = "LOGICAL OR", form = FInfix, priority = 190, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8745", description = "INTERSECTION", form = FInfix, priority = 350, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8746", description = "UNION", form = FInfix, priority = 350, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8747", description = "INTEGRAL", form = FPrefix, priority = 310, lspace = 0, rspace = 1, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\8748", description = "DOUBLE INTEGRAL", form = FPrefix, priority = 300, lspace = 0, rspace = 1, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\8749", description = "TRIPLE INTEGRAL", form = FPrefix, priority = 300, lspace = 0, rspace = 1, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\8750", description = "CONTOUR INTEGRAL", form = FPrefix, priority = 310, lspace = 0, rspace = 1, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\8751", description = "SURFACE INTEGRAL", form = FPrefix, priority = 310, lspace = 0, rspace = 1, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\8752", description = "VOLUME INTEGRAL", form = FPrefix, priority = 310, lspace = 0, rspace = 1, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\8753", description = "CLOCKWISE INTEGRAL", form = FPrefix, priority = 310, lspace = 0, rspace = 1, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\8754", description = "CLOCKWISE CONTOUR INTEGRAL", form = FPrefix, priority = 310, lspace = 0, rspace = 1, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\8755", description = "ANTICLOCKWISE CONTOUR INTEGRAL", form = FPrefix, priority = 310, lspace = 0, rspace = 1, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\8756", description = "THEREFORE", form = FInfix, priority = 70, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8757", description = "BECAUSE", form = FInfix, priority = 70, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8758", description = "RATIO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8759", description = "PROPORTION", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8760", description = "DOT MINUS", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8761", description = "EXCESS", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8762", description = "GEOMETRIC PROPORTION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8763", description = "HOMOTHETIC", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8764", description = "TILDE OPERATOR", form = FInfix, priority = 250, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8765", description = "REVERSED TILDE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8765\817", description = "REVERSED TILDE with underline", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\8766", description = "INVERTED LAZY S", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8767", description = "SINE WAVE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\8768", description = "WREATH PRODUCT", form = FInfix, priority = 340, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8769", description = "NOT TILDE", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8770", description = "MINUS TILDE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8770\824", description = "MINUS TILDE with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8771", description = "ASYMPTOTICALLY EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8772", description = "NOT ASYMPTOTICALLY EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8773", description = "APPROXIMATELY EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8774", description = "APPROXIMATELY BUT NOT ACTUALLY EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8775", description = "NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8776", description = "ALMOST EQUAL TO", form = FInfix, priority = 247, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8777", description = "NOT ALMOST EQUAL TO", form = FInfix, priority = 250, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8778", description = "ALMOST EQUAL OR EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8779", description = "TRIPLE TILDE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8780", description = "ALL EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8781", description = "EQUIVALENT TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8782", description = "GEOMETRICALLY EQUIVALENT TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8782\824", description = "GEOMETRICALLY EQUIVALENT TO with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8783", description = "DIFFERENCE BETWEEN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8783\824", description = "DIFFERENCE BETWEEN with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8784", description = "APPROACHES THE LIMIT", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8785", description = "GEOMETRICALLY EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8786", description = "APPROXIMATELY EQUAL TO OR THE IMAGE OF", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8787", description = "IMAGE OF OR APPROXIMATELY EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8788", description = "COLON EQUALS", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8789", description = "EQUALS COLON", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8790", description = "RING IN EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8791", description = "RING EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8792", description = "CORRESPONDS TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8793", description = "ESTIMATES", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8794", description = "EQUIANGULAR TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8795", description = "STAR EQUALS", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8796", description = "DELTA EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8797", description = "EQUAL TO BY DEFINITION", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8798", description = "MEASURED BY", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8799", description = "QUESTIONED EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8800", description = "NOT EQUAL TO", form = FInfix, priority = 255, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8801", description = "IDENTICAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8802", description = "NOT IDENTICAL TO", form = FInfix, priority = 252, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8803", description = "STRICTLY EQUIVALENT TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8804", description = "LESS-THAN OR EQUAL TO", form = FInfix, priority = 241, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8805", description = "GREATER-THAN OR EQUAL TO", form = FInfix, priority = 242, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8806", description = "LESS-THAN OVER EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8806\824", description = "LESS-THAN OVER EQUAL TO with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8807", description = "GREATER-THAN OVER EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8808", description = "LESS-THAN BUT NOT EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8809", description = "GREATER-THAN BUT NOT EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8810", description = "MUCH LESS-THAN", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8810\824", description = "MUCH LESS THAN with slash", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8811", description = "MUCH GREATER-THAN", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8811\824", description = "MUCH GREATER THAN with slash", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8812", description = "BETWEEN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8813", description = "NOT EQUIVALENT TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8814", description = "NOT LESS-THAN", form = FInfix, priority = 246, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8815", description = "NOT GREATER-THAN", form = FInfix, priority = 244, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8816", description = "NEITHER LESS-THAN NOR EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8817", description = "NEITHER GREATER-THAN NOR EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8818", description = "LESS-THAN OR EQUIVALENT TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8819", description = "GREATER-THAN OR EQUIVALENT TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8820", description = "NEITHER LESS-THAN NOR EQUIVALENT TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8821", description = "NEITHER GREATER-THAN NOR EQUIVALENT TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8822", description = "LESS-THAN OR GREATER-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8823", description = "GREATER-THAN OR LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8824", description = "NEITHER LESS-THAN NOR GREATER-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8825", description = "NEITHER GREATER-THAN NOR LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8826", description = "PRECEDES", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8827", description = "SUCCEEDS", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8828", description = "PRECEDES OR EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8829", description = "SUCCEEDS OR EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8830", description = "PRECEDES OR EQUIVALENT TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8831", description = "SUCCEEDS OR EQUIVALENT TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8831\824", description = "SUCCEEDS OR EQUIVALENT TO with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8832", description = "DOES NOT PRECEDE", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8833", description = "DOES NOT SUCCEED", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8834", description = "SUBSET OF", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8834\8402", description = "SUBSET OF with vertical line", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8835", description = "SUPERSET OF", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8835\8402", description = "SUPERSET OF with vertical line", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8836", description = "NOT A SUBSET OF", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8837", description = "NOT A SUPERSET OF", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8838", description = "SUBSET OF OR EQUAL TO", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8839", description = "SUPERSET OF OR EQUAL TO", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8840", description = "NEITHER A SUBSET OF NOR EQUAL TO", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8841", description = "NEITHER A SUPERSET OF NOR EQUAL TO", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8842", description = "SUBSET OF WITH NOT EQUAL TO", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8843", description = "SUPERSET OF WITH NOT EQUAL TO", form = FInfix, priority = 240, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8844", description = "MULTISET", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8845", description = "MULTISET MULTIPLICATION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8846", description = "MULTISET UNION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8847", description = "SQUARE IMAGE OF", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8847\824", description = "SQUARE IMAGE OF with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8848", description = "SQUARE ORIGINAL OF", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8848\824", description = "SQUARE ORIGINAL OF with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8849", description = "SQUARE IMAGE OF OR EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8850", description = "SQUARE ORIGINAL OF OR EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8851", description = "SQUARE CAP", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8852", description = "SQUARE CUP", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8853", description = "CIRCLED PLUS", form = FInfix, priority = 300, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8854", description = "CIRCLED MINUS", form = FInfix, priority = 300, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8855", description = "CIRCLED TIMES", form = FInfix, priority = 410, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8856", description = "CIRCLED DIVISION SLASH", form = FInfix, priority = 300, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8857", description = "CIRCLED DOT OPERATOR", form = FInfix, priority = 710, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8858", description = "CIRCLED RING OPERATOR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8859", description = "CIRCLED ASTERISK OPERATOR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8860", description = "CIRCLED EQUALS", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8861", description = "CIRCLED DASH", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8862", description = "SQUARED PLUS", form = FInfix, priority = 275, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8863", description = "SQUARED MINUS", form = FInfix, priority = 275, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8864", description = "SQUARED TIMES", form = FInfix, priority = 390, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8865", description = "SQUARED DOT OPERATOR", form = FInfix, priority = 390, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8866", description = "RIGHT TACK", form = FInfix, priority = 170, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8867", description = "LEFT TACK", form = FInfix, priority = 170, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8868", description = "DOWN TACK", form = FInfix, priority = 170, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8869", description = "UP TACK", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8870", description = "ASSERTION", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8871", description = "MODELS", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8872", description = "TRUE", form = FInfix, priority = 170, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8873", description = "FORCES", form = FInfix, priority = 170, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8874", description = "TRIPLE VERTICAL BAR RIGHT TURNSTILE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8875", description = "DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8876", description = "DOES NOT PROVE", form = FInfix, priority = 170, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8877", description = "NOT TRUE", form = FInfix, priority = 170, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8878", description = "DOES NOT FORCE", form = FInfix, priority = 170, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8879", description = "NEGATED DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE", form = FInfix, priority = 170, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8880", description = "PRECEDES UNDER RELATION", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8881", description = "SUCCEEDS UNDER RELATION", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8882", description = "NORMAL SUBGROUP OF", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8883", description = "CONTAINS AS NORMAL SUBGROUP", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8884", description = "NORMAL SUBGROUP OF OR EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8885", description = "CONTAINS AS NORMAL SUBGROUP OR EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8886", description = "ORIGINAL OF", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8887", description = "IMAGE OF", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8888", description = "MULTIMAP", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8889", description = "HERMITIAN CONJUGATE MATRIX", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8890", description = "INTERCALATE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8891", description = "XOR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8892", description = "NAND", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8893", description = "NOR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8894", description = "RIGHT ANGLE WITH ARC", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\8895", description = "RIGHT TRIANGLE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\8896", description = "N-ARY LOGICAL AND", form = FPrefix, priority = 330, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\8897", description = "N-ARY LOGICAL OR", form = FPrefix, priority = 330, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\8898", description = "N-ARY INTERSECTION", form = FPrefix, priority = 330, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\8899", description = "N-ARY UNION", form = FPrefix, priority = 320, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\8900", description = "DIAMOND OPERATOR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8901", description = "DOT OPERATOR", form = FInfix, priority = 390, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8902", description = "STAR OPERATOR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8903", description = "DIVISION TIMES", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8904", description = "BOWTIE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8905", description = "LEFT NORMAL FACTOR SEMIDIRECT PRODUCT", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8906", description = "RIGHT NORMAL FACTOR SEMIDIRECT PRODUCT", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8907", description = "LEFT SEMIDIRECT PRODUCT", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8908", description = "RIGHT SEMIDIRECT PRODUCT", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8909", description = "REVERSED TILDE EQUALS", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8910", description = "CURLY LOGICAL OR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8911", description = "CURLY LOGICAL AND", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8912", description = "DOUBLE SUBSET", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8913", description = "DOUBLE SUPERSET", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8914", description = "DOUBLE INTERSECTION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8915", description = "DOUBLE UNION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\8916", description = "PITCHFORK", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8917", description = "EQUAL AND PARALLEL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8918", description = "LESS-THAN WITH DOT", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8919", description = "GREATER-THAN WITH DOT", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8920", description = "VERY MUCH LESS-THAN", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8921", description = "VERY MUCH GREATER-THAN", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8922", description = "LESS-THAN EQUAL TO OR GREATER-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8923", description = "GREATER-THAN EQUAL TO OR LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8924", description = "EQUAL TO OR LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8925", description = "EQUAL TO OR GREATER-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8926", description = "EQUAL TO OR PRECEDES", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8927", description = "EQUAL TO OR SUCCEEDS", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8928", description = "DOES NOT PRECEDE OR EQUAL", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8929", description = "DOES NOT SUCCEED OR EQUAL", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8930", description = "NOT SQUARE IMAGE OF OR EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8931", description = "NOT SQUARE ORIGINAL OF OR EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8932", description = "SQUARE IMAGE OF OR NOT EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8933", description = "SQUARE ORIGINAL OF OR NOT EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8934", description = "LESS-THAN BUT NOT EQUIVALENT TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8935", description = "GREATER-THAN BUT NOT EQUIVALENT TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8936", description = "PRECEDES BUT NOT EQUIVALENT TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8937", description = "SUCCEEDS BUT NOT EQUIVALENT TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8938", description = "NOT NORMAL SUBGROUP OF", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8939", description = "DOES NOT CONTAIN AS NORMAL SUBGROUP", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8940", description = "NOT NORMAL SUBGROUP OF OR EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8941", description = "DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8942", description = "VERTICAL ELLIPSIS", form = FInfix, priority = 150, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8943", description = "MIDLINE HORIZONTAL ELLIPSIS", form = FInfix, priority = 150, lspace = 0, rspace = 0, properties = []}
  , Operator {oper = "\8944", description = "UP RIGHT DIAGONAL ELLIPSIS", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8945", description = "DOWN RIGHT DIAGONAL ELLIPSIS", form = FInfix, priority = 150, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8946", description = "ELEMENT OF WITH LONG HORIZONTAL STROKE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8947", description = "ELEMENT OF WITH VERTICAL BAR AT END OF HORIZONTAL STROKE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8948", description = "SMALL ELEMENT OF WITH VERTICAL BAR AT END OF HORIZONTAL STROKE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8949", description = "ELEMENT OF WITH DOT ABOVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8950", description = "ELEMENT OF WITH OVERBAR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8951", description = "SMALL ELEMENT OF WITH OVERBAR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8952", description = "ELEMENT OF WITH UNDERBAR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8953", description = "ELEMENT OF WITH TWO HORIZONTAL STROKES", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8954", description = "CONTAINS WITH LONG HORIZONTAL STROKE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8955", description = "CONTAINS WITH VERTICAL BAR AT END OF HORIZONTAL STROKE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8956", description = "SMALL CONTAINS WITH VERTICAL BAR AT END OF HORIZONTAL STROKE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8957", description = "CONTAINS WITH OVERBAR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8958", description = "SMALL CONTAINS WITH OVERBAR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8959", description = "Z NOTATION BAG MEMBERSHIP", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\8968", description = "LEFT CEILING", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\8969", description = "RIGHT CEILING", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\8970", description = "LEFT FLOOR", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\8971", description = "RIGHT FLOOR", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\9001", description = "LEFT-POINTING ANGLE BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\9002", description = "RIGHT-POINTING ANGLE BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\9140", description = "TOP SQUARE BRACKET", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\9141", description = "BOTTOM SQUARE BRACKET", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\9180", description = "TOP PARENTHESIS", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\9181", description = "BOTTOM PARENTHESIS", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\9182", description = "TOP CURLY BRACKET", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\9183", description = "BOTTOM CURLY BRACKET", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\9184", description = "TOP TORTOISE SHELL BRACKET", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\9185", description = "BOTTOM TORTOISE SHELL BRACKET", form = FPostfix, priority = 880, lspace = 0, rspace = 0, properties = ["accent","stretchy"]}
  , Operator {oper = "\9632", description = "BLACK SQUARE", form = FInfix, priority = 260, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\9633", description = "WHITE SQUARE", form = FInfix, priority = 260, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\9642", description = "BLACK SMALL SQUARE", form = FInfix, priority = 260, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\9643", description = "WHITE SMALL SQUARE", form = FInfix, priority = 260, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\9645", description = "WHITE RECTANGLE", form = FInfix, priority = 260, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\9646", description = "BLACK VERTICAL RECTANGLE", form = FInfix, priority = 260, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\9647", description = "WHITE VERTICAL RECTANGLE", form = FInfix, priority = 260, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\9648", description = "BLACK PARALLELOGRAM", form = FInfix, priority = 260, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\9649", description = "WHITE PARALLELOGRAM", form = FInfix, priority = 260, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\9650", description = "BLACK UP-POINTING TRIANGLE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9651", description = "WHITE UP-POINTING TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9652", description = "BLACK UP-POINTING SMALL TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9653", description = "WHITE UP-POINTING SMALL TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9654", description = "BLACK RIGHT-POINTING TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9655", description = "WHITE RIGHT-POINTING TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9656", description = "BLACK RIGHT-POINTING SMALL TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9657", description = "WHITE RIGHT-POINTING SMALL TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9660", description = "BLACK DOWN-POINTING TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9661", description = "WHITE DOWN-POINTING TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9662", description = "BLACK DOWN-POINTING SMALL TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9663", description = "WHITE DOWN-POINTING SMALL TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9664", description = "BLACK LEFT-POINTING TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9665", description = "WHITE LEFT-POINTING TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9666", description = "BLACK LEFT-POINTING SMALL TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9667", description = "WHITE LEFT-POINTING SMALL TRIANGLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9668", description = "BLACK LEFT-POINTING POINTER", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9669", description = "WHITE LEFT-POINTING POINTER", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9670", description = "BLACK DIAMOND", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9671", description = "WHITE DIAMOND", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9672", description = "WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9673", description = "FISHEYE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9676", description = "DOTTED CIRCLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9677", description = "CIRCLE WITH VERTICAL FILL", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9678", description = "BULLSEYE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9679", description = "BLACK CIRCLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9686", description = "LEFT HALF BLACK CIRCLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9687", description = "RIGHT HALF BLACK CIRCLE", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9702", description = "WHITE BULLET", form = FInfix, priority = 260, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\9837", description = "MUSIC FLAT SIGN", form = FPostfix, priority = 800, lspace = 0, rspace = 2, properties = []}
  , Operator {oper = "\9838", description = "MUSIC NATURAL SIGN", form = FPostfix, priority = 800, lspace = 0, rspace = 2, properties = []}
  , Operator {oper = "\9839", description = "MUSIC SHARP SIGN", form = FPostfix, priority = 800, lspace = 0, rspace = 2, properties = []}
  , Operator {oper = "\10072", description = "LIGHT VERTICAL BAR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10098", description = "LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10099", description = "LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10214", description = "MATHEMATICAL LEFT WHITE SQUARE BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10215", description = "MATHEMATICAL RIGHT WHITE SQUARE BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10216", description = "MATHEMATICAL LEFT ANGLE BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10217", description = "MATHEMATICAL RIGHT ANGLE BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10218", description = "MATHEMATICAL LEFT DOUBLE ANGLE BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10219", description = "MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10220", description = "MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10221", description = "MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10222", description = "MATHEMATICAL LEFT FLATTENED PARENTHESIS", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10223", description = "MATHEMATICAL RIGHT FLATTENED PARENTHESIS", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10224", description = "UPWARDS QUADRUPLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10225", description = "DOWNWARDS QUADRUPLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10229", description = "LONG LEFTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10230", description = "LONG RIGHTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10231", description = "LONG LEFT RIGHT ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10232", description = "LONG LEFTWARDS DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10233", description = "LONG RIGHTWARDS DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10234", description = "LONG LEFT RIGHT DOUBLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10235", description = "LONG LEFTWARDS ARROW FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10236", description = "LONG RIGHTWARDS ARROW FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10237", description = "LONG LEFTWARDS DOUBLE ARROW FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10238", description = "LONG RIGHTWARDS DOUBLE ARROW FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10239", description = "LONG RIGHTWARDS SQUIGGLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10496", description = "RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10497", description = "RIGHTWARDS TWO-HEADED ARROW WITH DOUBLE VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10498", description = "LEFTWARDS DOUBLE ARROW WITH VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10499", description = "RIGHTWARDS DOUBLE ARROW WITH VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10500", description = "LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10501", description = "RIGHTWARDS TWO-HEADED ARROW FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10502", description = "LEFTWARDS DOUBLE ARROW FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10503", description = "RIGHTWARDS DOUBLE ARROW FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10504", description = "DOWNWARDS ARROW WITH HORIZONTAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10505", description = "UPWARDS ARROW WITH HORIZONTAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10506", description = "UPWARDS TRIPLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10507", description = "DOWNWARDS TRIPLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10508", description = "LEFTWARDS DOUBLE DASH ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10509", description = "RIGHTWARDS DOUBLE DASH ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10510", description = "LEFTWARDS TRIPLE DASH ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10511", description = "RIGHTWARDS TRIPLE DASH ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10512", description = "RIGHTWARDS TWO-HEADED TRIPLE DASH ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10513", description = "RIGHTWARDS ARROW WITH DOTTED STEM", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10514", description = "UPWARDS ARROW TO BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10515", description = "DOWNWARDS ARROW TO BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10516", description = "RIGHTWARDS ARROW WITH TAIL WITH VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10517", description = "RIGHTWARDS ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10518", description = "RIGHTWARDS TWO-HEADED ARROW WITH TAIL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10519", description = "RIGHTWARDS TWO-HEADED ARROW WITH TAIL WITH VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10520", description = "RIGHTWARDS TWO-HEADED ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10521", description = "LEFTWARDS ARROW-TAIL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10522", description = "RIGHTWARDS ARROW-TAIL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10523", description = "LEFTWARDS DOUBLE ARROW-TAIL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10524", description = "RIGHTWARDS DOUBLE ARROW-TAIL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10525", description = "LEFTWARDS ARROW TO BLACK DIAMOND", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10526", description = "RIGHTWARDS ARROW TO BLACK DIAMOND", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10527", description = "LEFTWARDS ARROW FROM BAR TO BLACK DIAMOND", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10528", description = "RIGHTWARDS ARROW FROM BAR TO BLACK DIAMOND", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10529", description = "NORTH WEST AND SOUTH EAST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10530", description = "NORTH EAST AND SOUTH WEST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10531", description = "NORTH WEST ARROW WITH HOOK", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10532", description = "NORTH EAST ARROW WITH HOOK", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10533", description = "SOUTH EAST ARROW WITH HOOK", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10534", description = "SOUTH WEST ARROW WITH HOOK", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10535", description = "NORTH WEST ARROW AND NORTH EAST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10536", description = "NORTH EAST ARROW AND SOUTH EAST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10537", description = "SOUTH EAST ARROW AND SOUTH WEST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10538", description = "SOUTH WEST ARROW AND NORTH WEST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10539", description = "RISING DIAGONAL CROSSING FALLING DIAGONAL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10540", description = "FALLING DIAGONAL CROSSING RISING DIAGONAL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10541", description = "SOUTH EAST ARROW CROSSING NORTH EAST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10542", description = "NORTH EAST ARROW CROSSING SOUTH EAST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10543", description = "FALLING DIAGONAL CROSSING NORTH EAST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10544", description = "RISING DIAGONAL CROSSING SOUTH EAST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10545", description = "NORTH EAST ARROW CROSSING NORTH WEST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10546", description = "NORTH WEST ARROW CROSSING NORTH EAST ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10547", description = "WAVE ARROW POINTING DIRECTLY RIGHT", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10548", description = "ARROW POINTING RIGHTWARDS THEN CURVING UPWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10549", description = "ARROW POINTING RIGHTWARDS THEN CURVING DOWNWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10550", description = "ARROW POINTING DOWNWARDS THEN CURVING LEFTWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10551", description = "ARROW POINTING DOWNWARDS THEN CURVING RIGHTWARDS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10552", description = "RIGHT-SIDE ARC CLOCKWISE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10553", description = "LEFT-SIDE ARC ANTICLOCKWISE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10554", description = "TOP ARC ANTICLOCKWISE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10555", description = "BOTTOM ARC ANTICLOCKWISE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10556", description = "TOP ARC CLOCKWISE ARROW WITH MINUS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10557", description = "TOP ARC ANTICLOCKWISE ARROW WITH PLUS", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10558", description = "LOWER RIGHT SEMICIRCULAR CLOCKWISE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10559", description = "LOWER LEFT SEMICIRCULAR ANTICLOCKWISE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10560", description = "ANTICLOCKWISE CLOSED CIRCLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10561", description = "CLOCKWISE CLOSED CIRCLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10562", description = "RIGHTWARDS ARROW ABOVE SHORT LEFTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10563", description = "LEFTWARDS ARROW ABOVE SHORT RIGHTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10564", description = "SHORT RIGHTWARDS ARROW ABOVE LEFTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10565", description = "RIGHTWARDS ARROW WITH PLUS BELOW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10566", description = "LEFTWARDS ARROW WITH PLUS BELOW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10567", description = "RIGHTWARDS ARROW THROUGH X", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10568", description = "LEFT RIGHT ARROW THROUGH SMALL CIRCLE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10569", description = "UPWARDS TWO-HEADED ARROW FROM SMALL CIRCLE", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10570", description = "LEFT BARB UP RIGHT BARB DOWN HARPOON", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10571", description = "LEFT BARB DOWN RIGHT BARB UP HARPOON", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10572", description = "UP BARB RIGHT DOWN BARB LEFT HARPOON", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10573", description = "UP BARB LEFT DOWN BARB RIGHT HARPOON", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10574", description = "LEFT BARB UP RIGHT BARB UP HARPOON", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10575", description = "UP BARB RIGHT DOWN BARB RIGHT HARPOON", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10576", description = "LEFT BARB DOWN RIGHT BARB DOWN HARPOON", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10577", description = "UP BARB LEFT DOWN BARB LEFT HARPOON", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10578", description = "LEFTWARDS HARPOON WITH BARB UP TO BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10579", description = "RIGHTWARDS HARPOON WITH BARB UP TO BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10580", description = "UPWARDS HARPOON WITH BARB RIGHT TO BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10581", description = "DOWNWARDS HARPOON WITH BARB RIGHT TO BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10582", description = "LEFTWARDS HARPOON WITH BARB DOWN TO BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10583", description = "RIGHTWARDS HARPOON WITH BARB DOWN TO BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10584", description = "UPWARDS HARPOON WITH BARB LEFT TO BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10585", description = "DOWNWARDS HARPOON WITH BARB LEFT TO BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10586", description = "LEFTWARDS HARPOON WITH BARB UP FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10587", description = "RIGHTWARDS HARPOON WITH BARB UP FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10588", description = "UPWARDS HARPOON WITH BARB RIGHT FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10589", description = "DOWNWARDS HARPOON WITH BARB RIGHT FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10590", description = "LEFTWARDS HARPOON WITH BARB DOWN FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10591", description = "RIGHTWARDS HARPOON WITH BARB DOWN FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy","accent"]}
  , Operator {oper = "\10592", description = "UPWARDS HARPOON WITH BARB LEFT FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10593", description = "DOWNWARDS HARPOON WITH BARB LEFT FROM BAR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10594", description = "LEFTWARDS HARPOON WITH BARB UP ABOVE LEFTWARDS HARPOON WITH BARB DOWN", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10595", description = "UPWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10596", description = "RIGHTWARDS HARPOON WITH BARB UP ABOVE RIGHTWARDS HARPOON WITH BARB DOWN", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10597", description = "DOWNWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10598", description = "LEFTWARDS HARPOON WITH BARB UP ABOVE RIGHTWARDS HARPOON WITH BARB UP", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10599", description = "LEFTWARDS HARPOON WITH BARB DOWN ABOVE RIGHTWARDS HARPOON WITH BARB DOWN", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10600", description = "RIGHTWARDS HARPOON WITH BARB UP ABOVE LEFTWARDS HARPOON WITH BARB UP", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10601", description = "RIGHTWARDS HARPOON WITH BARB DOWN ABOVE LEFTWARDS HARPOON WITH BARB DOWN", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10602", description = "LEFTWARDS HARPOON WITH BARB UP ABOVE LONG DASH", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10603", description = "LEFTWARDS HARPOON WITH BARB DOWN BELOW LONG DASH", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10604", description = "RIGHTWARDS HARPOON WITH BARB UP ABOVE LONG DASH", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10605", description = "RIGHTWARDS HARPOON WITH BARB DOWN BELOW LONG DASH", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10606", description = "UPWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10607", description = "DOWNWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\10608", description = "RIGHT DOUBLE ARROW WITH ROUNDED HEAD", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10609", description = "EQUALS SIGN ABOVE RIGHTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10610", description = "TILDE OPERATOR ABOVE RIGHTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10611", description = "LEFTWARDS ARROW ABOVE TILDE OPERATOR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10612", description = "RIGHTWARDS ARROW ABOVE TILDE OPERATOR", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10613", description = "RIGHTWARDS ARROW ABOVE ALMOST EQUAL TO", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10614", description = "LESS-THAN ABOVE LEFTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10615", description = "LEFTWARDS ARROW THROUGH LESS-THAN", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10616", description = "GREATER-THAN ABOVE RIGHTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10617", description = "SUBSET ABOVE RIGHTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10618", description = "LEFTWARDS ARROW THROUGH SUBSET", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10619", description = "SUPERSET ABOVE LEFTWARDS ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10620", description = "LEFT FISH TAIL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10621", description = "RIGHT FISH TAIL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["accent"]}
  , Operator {oper = "\10622", description = "UP FISH TAIL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10623", description = "DOWN FISH TAIL", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10624", description = "TRIPLE VERTICAL BAR DELIMITER", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["fence","stretchy"]}
  , Operator {oper = "\10624", description = "TRIPLE VERTICAL BAR DELIMITER", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["fence","stretchy"]}
  , Operator {oper = "\10625", description = "Z NOTATION SPOT", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10626", description = "Z NOTATION TYPE COLON", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10627", description = "LEFT WHITE CURLY BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10628", description = "RIGHT WHITE CURLY BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10629", description = "LEFT WHITE PARENTHESIS", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10630", description = "RIGHT WHITE PARENTHESIS", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10631", description = "Z NOTATION LEFT IMAGE BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10632", description = "Z NOTATION RIGHT IMAGE BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10633", description = "Z NOTATION LEFT BINDING BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10634", description = "Z NOTATION RIGHT BINDING BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10635", description = "LEFT SQUARE BRACKET WITH UNDERBAR", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10636", description = "RIGHT SQUARE BRACKET WITH UNDERBAR", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10637", description = "LEFT SQUARE BRACKET WITH TICK IN TOP CORNER", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10638", description = "RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10639", description = "LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10640", description = "RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10641", description = "LEFT ANGLE BRACKET WITH DOT", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10642", description = "RIGHT ANGLE BRACKET WITH DOT", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10643", description = "LEFT ARC LESS-THAN BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10644", description = "RIGHT ARC GREATER-THAN BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10645", description = "DOUBLE LEFT ARC GREATER-THAN BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10646", description = "DOUBLE RIGHT ARC LESS-THAN BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10647", description = "LEFT BLACK TORTOISE SHELL BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10648", description = "RIGHT BLACK TORTOISE SHELL BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10649", description = "DOTTED FENCE", form = FInfix, priority = 270, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10650", description = "VERTICAL ZIGZAG LINE", form = FInfix, priority = 270, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10651", description = "MEASURED ANGLE OPENING LEFT", form = FInfix, priority = 270, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10652", description = "RIGHT ANGLE VARIANT WITH SQUARE", form = FInfix, priority = 270, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10653", description = "MEASURED RIGHT ANGLE WITH DOT", form = FInfix, priority = 270, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10654", description = "ANGLE WITH S INSIDE", form = FInfix, priority = 270, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10655", description = "ACUTE ANGLE", form = FInfix, priority = 270, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10656", description = "SPHERICAL ANGLE OPENING LEFT", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10657", description = "SPHERICAL ANGLE OPENING UP", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10658", description = "TURNED ANGLE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10659", description = "REVERSED ANGLE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10660", description = "ANGLE WITH UNDERBAR", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10661", description = "REVERSED ANGLE WITH UNDERBAR", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10662", description = "OBLIQUE ANGLE OPENING UP", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10663", description = "OBLIQUE ANGLE OPENING DOWN", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10664", description = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING UP AND RIGHT", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10665", description = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING UP AND LEFT", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10666", description = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING DOWN AND RIGHT", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10667", description = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING DOWN AND LEFT", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10668", description = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING RIGHT AND UP", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10669", description = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING LEFT AND UP", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10670", description = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING RIGHT AND DOWN", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10671", description = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING LEFT AND DOWN", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10672", description = "REVERSED EMPTY SET", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10673", description = "EMPTY SET WITH OVERBAR", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10674", description = "EMPTY SET WITH SMALL CIRCLE ABOVE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10675", description = "EMPTY SET WITH RIGHT ARROW ABOVE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10676", description = "EMPTY SET WITH LEFT ARROW ABOVE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10677", description = "CIRCLE WITH HORIZONTAL BAR", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10678", description = "CIRCLED VERTICAL BAR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10679", description = "CIRCLED PARALLEL", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10680", description = "CIRCLED REVERSE SOLIDUS", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10681", description = "CIRCLED PERPENDICULAR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10682", description = "CIRCLE DIVIDED BY HORIZONTAL BAR AND TOP HALF DIVIDED BY VERTICAL BAR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10683", description = "CIRCLE WITH SUPERIMPOSED X", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10684", description = "CIRCLED ANTICLOCKWISE-ROTATED DIVISION SIGN", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10685", description = "UP ARROW THROUGH CIRCLE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10686", description = "CIRCLED WHITE BULLET", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10687", description = "CIRCLED BULLET", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10688", description = "CIRCLED LESS-THAN", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10689", description = "CIRCLED GREATER-THAN", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10690", description = "CIRCLE WITH SMALL CIRCLE TO THE RIGHT", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10691", description = "CIRCLE WITH TWO HORIZONTAL STROKES TO THE RIGHT", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10692", description = "SQUARED RISING DIAGONAL SLASH", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10693", description = "SQUARED FALLING DIAGONAL SLASH", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10694", description = "SQUARED ASTERISK", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10695", description = "SQUARED SMALL CIRCLE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10696", description = "SQUARED SQUARE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10697", description = "TWO JOINED SQUARES", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10698", description = "TRIANGLE WITH DOT ABOVE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10699", description = "TRIANGLE WITH UNDERBAR", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10700", description = "S IN TRIANGLE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10701", description = "TRIANGLE WITH SERIFS AT BOTTOM", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10702", description = "RIGHT TRIANGLE ABOVE LEFT TRIANGLE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10703", description = "LEFT TRIANGLE BESIDE VERTICAL BAR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10703\824", description = "LEFT TRIANGLE BESIDE VERTICAL BAR with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10704", description = "VERTICAL BAR BESIDE RIGHT TRIANGLE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10704\824", description = "VERTICAL BAR BESIDE RIGHT TRIANGLE with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10705", description = "BOWTIE WITH LEFT HALF BLACK", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10706", description = "BOWTIE WITH RIGHT HALF BLACK", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10707", description = "BLACK BOWTIE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10708", description = "TIMES WITH LEFT HALF BLACK", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10709", description = "TIMES WITH RIGHT HALF BLACK", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10710", description = "WHITE HOURGLASS", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10711", description = "BLACK HOURGLASS", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10712", description = "LEFT WIGGLY FENCE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10713", description = "RIGHT WIGGLY FENCE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10715", description = "RIGHT DOUBLE WIGGLY FENCE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10716", description = "INCOMPLETE INFINITY", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10717", description = "TIE OVER INFINITY", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10718", description = "INFINITY NEGATED WITH VERTICAL BAR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10719", description = "DOUBLE-ENDED MULTIMAP", form = FInfix, priority = 270, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10720", description = "SQUARE WITH CONTOURED OUTLINE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10721", description = "INCREASES AS", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10722", description = "SHUFFLE PRODUCT", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10723", description = "EQUALS SIGN AND SLANTED PARALLEL", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10724", description = "EQUALS SIGN AND SLANTED PARALLEL WITH TILDE ABOVE", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10725", description = "IDENTICAL TO AND SLANTED PARALLEL", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10726", description = "GLEICH STARK", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10727", description = "THERMODYNAMIC", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10728", description = "DOWN-POINTING TRIANGLE WITH LEFT HALF BLACK", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10729", description = "DOWN-POINTING TRIANGLE WITH RIGHT HALF BLACK", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10730", description = "BLACK DIAMOND WITH DOWN ARROW", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10731", description = "BLACK LOZENGE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10732", description = "WHITE CIRCLE WITH DOWN ARROW", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10733", description = "BLACK CIRCLE WITH DOWN ARROW", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10734", description = "ERROR-BARRED WHITE SQUARE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10735", description = "ERROR-BARRED BLACK SQUARE", form = FInfix, priority = 270, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10736", description = "ERROR-BARRED WHITE DIAMOND", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10737", description = "ERROR-BARRED BLACK DIAMOND", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10738", description = "ERROR-BARRED WHITE CIRCLE", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10739", description = "ERROR-BARRED BLACK CIRCLE", form = FInfix, priority = 260, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10740", description = "RULE-DELAYED", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10741", description = "REVERSE SOLIDUS OPERATOR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10742", description = "SOLIDUS WITH OVERBAR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10743", description = "REVERSE SOLIDUS WITH HORIZONTAL STROKE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10744", description = "BIG SOLIDUS", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10745", description = "BIG REVERSE SOLIDUS", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10746", description = "DOUBLE PLUS", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10747", description = "TRIPLE PLUS", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10748", description = "LEFT-POINTING CURVED ANGLE BRACKET", form = FPrefix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10749", description = "RIGHT-POINTING CURVED ANGLE BRACKET", form = FPostfix, priority = 20, lspace = 0, rspace = 0, properties = ["symmetric","fence","stretchy","mirrorable"]}
  , Operator {oper = "\10750", description = "TINY", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10751", description = "MINY", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10752", description = "N-ARY CIRCLED DOT OPERATOR", form = FPrefix, priority = 330, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\10753", description = "N-ARY CIRCLED PLUS OPERATOR", form = FPrefix, priority = 300, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\10754", description = "N-ARY CIRCLED TIMES OPERATOR", form = FPrefix, priority = 330, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\10755", description = "N-ARY UNION OPERATOR WITH DOT", form = FPrefix, priority = 320, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\10756", description = "N-ARY UNION OPERATOR WITH PLUS", form = FPrefix, priority = 320, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\10757", description = "N-ARY SQUARE INTERSECTION OPERATOR", form = FPrefix, priority = 330, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\10758", description = "N-ARY SQUARE UNION OPERATOR", form = FPrefix, priority = 330, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\10759", description = "TWO LOGICAL AND OPERATOR", form = FPrefix, priority = 330, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\10760", description = "TWO LOGICAL OR OPERATOR", form = FPrefix, priority = 330, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\10761", description = "N-ARY TIMES OPERATOR", form = FPrefix, priority = 330, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\10762", description = "MODULO TWO SUM", form = FPrefix, priority = 290, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits","mirrorable"]}
  , Operator {oper = "\10763", description = "SUMMATION WITH INTEGRAL", form = FPrefix, priority = 290, lspace = 1, rspace = 2, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\10764", description = "QUADRUPLE INTEGRAL OPERATOR", form = FPrefix, priority = 310, lspace = 0, rspace = 1, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\10765", description = "FINITE PART INTEGRAL", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\10766", description = "INTEGRAL WITH DOUBLE STROKE", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\10767", description = "INTEGRAL AVERAGE WITH SLASH", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\10768", description = "CIRCULATION FUNCTION", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits","mirrorable"]}
  , Operator {oper = "\10769", description = "ANTICLOCKWISE INTEGRATION", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits","mirrorable"]}
  , Operator {oper = "\10770", description = "LINE INTEGRATION WITH RECTANGULAR PATH AROUND POLE", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits","mirrorable"]}
  , Operator {oper = "\10771", description = "LINE INTEGRATION WITH SEMICIRCULAR PATH AROUND POLE", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits","mirrorable"]}
  , Operator {oper = "\10772", description = "LINE INTEGRATION NOT INCLUDING THE POLE", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits","mirrorable"]}
  , Operator {oper = "\10773", description = "INTEGRAL AROUND A POINT OPERATOR", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\10774", description = "QUATERNION INTEGRAL OPERATOR", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\10775", description = "INTEGRAL WITH LEFTWARDS ARROW WITH HOOK", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\10776", description = "INTEGRAL WITH TIMES SIGN", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\10777", description = "INTEGRAL WITH INTERSECTION", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\10778", description = "INTEGRAL WITH UNION", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\10779", description = "INTEGRAL WITH OVERBAR", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\10780", description = "INTEGRAL WITH UNDERBAR", form = FPrefix, priority = 310, lspace = 1, rspace = 2, properties = ["symmetric","largeop","mirrorable"]}
  , Operator {oper = "\10781", description = "JOIN", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10782", description = "LARGE LEFT TRIANGLE OPERATOR", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10783", description = "Z NOTATION SCHEMA COMPOSITION", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10784", description = "Z NOTATION SCHEMA PIPING", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10785", description = "Z NOTATION SCHEMA PROJECTION", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\10786", description = "PLUS SIGN WITH SMALL CIRCLE ABOVE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10787", description = "PLUS SIGN WITH CIRCUMFLEX ACCENT ABOVE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10788", description = "PLUS SIGN WITH TILDE ABOVE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10789", description = "PLUS SIGN WITH DOT BELOW", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10790", description = "PLUS SIGN WITH TILDE BELOW", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10791", description = "PLUS SIGN WITH SUBSCRIPT TWO", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10792", description = "PLUS SIGN WITH BLACK TRIANGLE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10793", description = "MINUS SIGN WITH COMMA ABOVE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10794", description = "MINUS SIGN WITH DOT BELOW", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10795", description = "MINUS SIGN WITH FALLING DOTS", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10796", description = "MINUS SIGN WITH RISING DOTS", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10797", description = "PLUS SIGN IN LEFT HALF CIRCLE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10798", description = "PLUS SIGN IN RIGHT HALF CIRCLE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10799", description = "VECTOR OR CROSS PRODUCT", form = FInfix, priority = 390, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10800", description = "MULTIPLICATION SIGN WITH DOT ABOVE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10801", description = "MULTIPLICATION SIGN WITH UNDERBAR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10802", description = "SEMIDIRECT PRODUCT WITH BOTTOM CLOSED", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10803", description = "SMASH PRODUCT", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10804", description = "MULTIPLICATION SIGN IN LEFT HALF CIRCLE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10805", description = "MULTIPLICATION SIGN IN RIGHT HALF CIRCLE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10806", description = "CIRCLED MULTIPLICATION SIGN WITH CIRCUMFLEX ACCENT", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10807", description = "MULTIPLICATION SIGN IN DOUBLE CIRCLE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10808", description = "CIRCLED DIVISION SIGN", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10809", description = "PLUS SIGN IN TRIANGLE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10810", description = "MINUS SIGN IN TRIANGLE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10811", description = "MULTIPLICATION SIGN IN TRIANGLE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10812", description = "INTERIOR PRODUCT", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10813", description = "RIGHTHAND INTERIOR PRODUCT", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10814", description = "Z NOTATION RELATIONAL COMPOSITION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10815", description = "AMALGAMATION OR COPRODUCT", form = FInfix, priority = 390, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10816", description = "INTERSECTION WITH DOT", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10817", description = "UNION WITH MINUS SIGN", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10818", description = "UNION WITH OVERBAR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10819", description = "INTERSECTION WITH OVERBAR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10820", description = "INTERSECTION WITH LOGICAL AND", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10821", description = "UNION WITH LOGICAL OR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10822", description = "UNION ABOVE INTERSECTION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10823", description = "INTERSECTION ABOVE UNION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10824", description = "UNION ABOVE BAR ABOVE INTERSECTION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10825", description = "INTERSECTION ABOVE BAR ABOVE UNION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10826", description = "UNION BESIDE AND JOINED WITH UNION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10827", description = "INTERSECTION BESIDE AND JOINED WITH INTERSECTION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10828", description = "CLOSED UNION WITH SERIFS", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10829", description = "CLOSED INTERSECTION WITH SERIFS", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10830", description = "DOUBLE SQUARE INTERSECTION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10831", description = "DOUBLE SQUARE UNION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10832", description = "CLOSED UNION WITH SERIFS AND SMASH PRODUCT", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10833", description = "LOGICAL AND WITH DOT ABOVE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10834", description = "LOGICAL OR WITH DOT ABOVE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10835", description = "DOUBLE LOGICAL AND", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10836", description = "DOUBLE LOGICAL OR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10837", description = "TWO INTERSECTING LOGICAL AND", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10838", description = "TWO INTERSECTING LOGICAL OR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10839", description = "SLOPING LARGE OR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10840", description = "SLOPING LARGE AND", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10841", description = "LOGICAL OR OVERLAPPING LOGICAL AND", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10842", description = "LOGICAL AND WITH MIDDLE STEM", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10843", description = "LOGICAL OR WITH MIDDLE STEM", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10844", description = "LOGICAL AND WITH HORIZONTAL DASH", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10845", description = "LOGICAL OR WITH HORIZONTAL DASH", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10846", description = "LOGICAL AND WITH DOUBLE OVERBAR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10847", description = "LOGICAL AND WITH UNDERBAR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10848", description = "LOGICAL AND WITH DOUBLE UNDERBAR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10849", description = "SMALL VEE WITH UNDERBAR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10850", description = "LOGICAL OR WITH DOUBLE OVERBAR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10851", description = "LOGICAL OR WITH DOUBLE UNDERBAR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10852", description = "Z NOTATION DOMAIN ANTIRESTRICTION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10853", description = "Z NOTATION RANGE ANTIRESTRICTION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10854", description = "EQUALS SIGN WITH DOT BELOW", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10855", description = "IDENTICAL WITH DOT ABOVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10856", description = "TRIPLE HORIZONTAL BAR WITH DOUBLE VERTICAL STROKE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10857", description = "TRIPLE HORIZONTAL BAR WITH TRIPLE VERTICAL STROKE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10858", description = "TILDE OPERATOR WITH DOT ABOVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10859", description = "TILDE OPERATOR WITH RISING DOTS", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10860", description = "SIMILAR MINUS SIMILAR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10861", description = "CONGRUENT WITH DOT ABOVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10862", description = "EQUALS WITH ASTERISK", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10863", description = "ALMOST EQUAL TO WITH CIRCUMFLEX ACCENT", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10864", description = "APPROXIMATELY EQUAL OR EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10865", description = "EQUALS SIGN ABOVE PLUS SIGN", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10866", description = "PLUS SIGN ABOVE EQUALS SIGN", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10867", description = "EQUALS SIGN ABOVE TILDE OPERATOR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10868", description = "DOUBLE COLON EQUAL", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10869", description = "TWO CONSECUTIVE EQUALS SIGNS", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10870", description = "THREE CONSECUTIVE EQUALS SIGNS", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10871", description = "EQUALS SIGN WITH TWO DOTS ABOVE AND TWO DOTS BELOW", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10872", description = "EQUIVALENT WITH FOUR DOTS ABOVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10873", description = "LESS-THAN WITH CIRCLE INSIDE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10874", description = "GREATER-THAN WITH CIRCLE INSIDE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10875", description = "LESS-THAN WITH QUESTION MARK ABOVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10876", description = "GREATER-THAN WITH QUESTION MARK ABOVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10877", description = "LESS-THAN OR SLANTED EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10877\824", description = "LESS-THAN OR SLANTED EQUAL TO with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10878", description = "GREATER-THAN OR SLANTED EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10878\824", description = "GREATER-THAN OR SLANTED EQUAL TO with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10879", description = "LESS-THAN OR SLANTED EQUAL TO WITH DOT INSIDE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10880", description = "GREATER-THAN OR SLANTED EQUAL TO WITH DOT INSIDE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10881", description = "LESS-THAN OR SLANTED EQUAL TO WITH DOT ABOVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10882", description = "GREATER-THAN OR SLANTED EQUAL TO WITH DOT ABOVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10883", description = "LESS-THAN OR SLANTED EQUAL TO WITH DOT ABOVE RIGHT", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10884", description = "GREATER-THAN OR SLANTED EQUAL TO WITH DOT ABOVE LEFT", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10885", description = "LESS-THAN OR APPROXIMATE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10886", description = "GREATER-THAN OR APPROXIMATE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10887", description = "LESS-THAN AND SINGLE-LINE NOT EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10888", description = "GREATER-THAN AND SINGLE-LINE NOT EQUAL TO", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10889", description = "LESS-THAN AND NOT APPROXIMATE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10890", description = "GREATER-THAN AND NOT APPROXIMATE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10891", description = "LESS-THAN ABOVE DOUBLE-LINE EQUAL ABOVE GREATER-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10892", description = "GREATER-THAN ABOVE DOUBLE-LINE EQUAL ABOVE LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10893", description = "LESS-THAN ABOVE SIMILAR OR EQUAL", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10894", description = "GREATER-THAN ABOVE SIMILAR OR EQUAL", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10895", description = "LESS-THAN ABOVE SIMILAR ABOVE GREATER-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10896", description = "GREATER-THAN ABOVE SIMILAR ABOVE LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10897", description = "LESS-THAN ABOVE GREATER-THAN ABOVE DOUBLE-LINE EQUAL", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10898", description = "GREATER-THAN ABOVE LESS-THAN ABOVE DOUBLE-LINE EQUAL", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10899", description = "LESS-THAN ABOVE SLANTED EQUAL ABOVE GREATER-THAN ABOVE SLANTED EQUAL", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10900", description = "GREATER-THAN ABOVE SLANTED EQUAL ABOVE LESS-THAN ABOVE SLANTED EQUAL", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10901", description = "SLANTED EQUAL TO OR LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10902", description = "SLANTED EQUAL TO OR GREATER-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10903", description = "SLANTED EQUAL TO OR LESS-THAN WITH DOT INSIDE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10904", description = "SLANTED EQUAL TO OR GREATER-THAN WITH DOT INSIDE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10905", description = "DOUBLE-LINE EQUAL TO OR LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10906", description = "DOUBLE-LINE EQUAL TO OR GREATER-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10907", description = "DOUBLE-LINE SLANTED EQUAL TO OR LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10908", description = "DOUBLE-LINE SLANTED EQUAL TO OR GREATER-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10909", description = "SIMILAR OR LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10910", description = "SIMILAR OR GREATER-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10911", description = "SIMILAR ABOVE LESS-THAN ABOVE EQUALS SIGN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10912", description = "SIMILAR ABOVE GREATER-THAN ABOVE EQUALS SIGN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10913", description = "DOUBLE NESTED LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10913\824", description = "DOUBLE NESTED LESS-THAN with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10914", description = "DOUBLE NESTED GREATER-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10914\824", description = "DOUBLE NESTED GREATER-THAN with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10915", description = "DOUBLE NESTED LESS-THAN WITH UNDERBAR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10916", description = "GREATER-THAN OVERLAPPING LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10917", description = "GREATER-THAN BESIDE LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10918", description = "LESS-THAN CLOSED BY CURVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10919", description = "GREATER-THAN CLOSED BY CURVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10920", description = "LESS-THAN CLOSED BY CURVE ABOVE SLANTED EQUAL", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10921", description = "GREATER-THAN CLOSED BY CURVE ABOVE SLANTED EQUAL", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10922", description = "SMALLER THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10923", description = "LARGER THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10924", description = "SMALLER THAN OR EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10925", description = "LARGER THAN OR EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10926", description = "EQUALS SIGN WITH BUMPY ABOVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10927", description = "PRECEDES ABOVE SINGLE-LINE EQUALS SIGN", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10927\824", description = "PRECEDES ABOVE SINGLE-LINE EQUALS SIGN with slash", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10928", description = "SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10928\824", description = "SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN with slash", form = FInfix, priority = 260, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10929", description = "PRECEDES ABOVE SINGLE-LINE NOT EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10930", description = "SUCCEEDS ABOVE SINGLE-LINE NOT EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10931", description = "PRECEDES ABOVE EQUALS SIGN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10932", description = "SUCCEEDS ABOVE EQUALS SIGN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10933", description = "PRECEDES ABOVE NOT EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10934", description = "SUCCEEDS ABOVE NOT EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10935", description = "PRECEDES ABOVE ALMOST EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10936", description = "SUCCEEDS ABOVE ALMOST EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10937", description = "PRECEDES ABOVE NOT ALMOST EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10938", description = "SUCCEEDS ABOVE NOT ALMOST EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10939", description = "DOUBLE PRECEDES", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10940", description = "DOUBLE SUCCEEDS", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10941", description = "SUBSET WITH DOT", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10942", description = "SUPERSET WITH DOT", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10943", description = "SUBSET WITH PLUS SIGN BELOW", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10944", description = "SUPERSET WITH PLUS SIGN BELOW", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10945", description = "SUBSET WITH MULTIPLICATION SIGN BELOW", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10946", description = "SUPERSET WITH MULTIPLICATION SIGN BELOW", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10947", description = "SUBSET OF OR EQUAL TO WITH DOT ABOVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10948", description = "SUPERSET OF OR EQUAL TO WITH DOT ABOVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10949", description = "SUBSET OF ABOVE EQUALS SIGN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10950", description = "SUPERSET OF ABOVE EQUALS SIGN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10951", description = "SUBSET OF ABOVE TILDE OPERATOR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10952", description = "SUPERSET OF ABOVE TILDE OPERATOR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10953", description = "SUBSET OF ABOVE ALMOST EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10954", description = "SUPERSET OF ABOVE ALMOST EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10955", description = "SUBSET OF ABOVE NOT EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10956", description = "SUPERSET OF ABOVE NOT EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10957", description = "SQUARE LEFT OPEN BOX OPERATOR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10958", description = "SQUARE RIGHT OPEN BOX OPERATOR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10959", description = "CLOSED SUBSET", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10960", description = "CLOSED SUPERSET", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10961", description = "CLOSED SUBSET OR EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10962", description = "CLOSED SUPERSET OR EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10963", description = "SUBSET ABOVE SUPERSET", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10964", description = "SUPERSET ABOVE SUBSET", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10965", description = "SUBSET ABOVE SUBSET", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10966", description = "SUPERSET ABOVE SUPERSET", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10967", description = "SUPERSET BESIDE SUBSET", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10968", description = "SUPERSET BESIDE AND JOINED BY DASH WITH SUBSET", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10969", description = "ELEMENT OF OPENING DOWNWARDS", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10970", description = "PITCHFORK WITH TEE TOP", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10971", description = "TRANSVERSAL INTERSECTION", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10973", description = "NONFORKING", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10973\824", description = "NONFORKING with slash", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10974", description = "SHORT LEFT TACK", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10975", description = "SHORT DOWN TACK", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10976", description = "SHORT UP TACK", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10977", description = "PERPENDICULAR WITH S", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10978", description = "VERTICAL BAR TRIPLE RIGHT TURNSTILE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10979", description = "DOUBLE VERTICAL BAR LEFT TURNSTILE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10980", description = "VERTICAL BAR DOUBLE LEFT TURNSTILE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10981", description = "DOUBLE VERTICAL BAR DOUBLE LEFT TURNSTILE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10982", description = "LONG DASH FROM LEFT MEMBER OF DOUBLE VERTICAL", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10983", description = "SHORT DOWN TACK WITH OVERBAR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10984", description = "SHORT UP TACK WITH UNDERBAR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10985", description = "SHORT UP TACK ABOVE SHORT DOWN TACK", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10986", description = "DOUBLE DOWN TACK", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10987", description = "DOUBLE UP TACK", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10988", description = "DOUBLE STROKE NOT SIGN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10989", description = "REVERSED DOUBLE STROKE NOT SIGN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10990", description = "DOES NOT DIVIDE WITH REVERSED NEGATION SLASH", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10991", description = "VERTICAL LINE WITH CIRCLE ABOVE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10992", description = "VERTICAL LINE WITH CIRCLE BELOW", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10993", description = "DOWN TACK WITH CIRCLE BELOW", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10994", description = "PARALLEL WITH HORIZONTAL STROKE", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10995", description = "PARALLEL WITH TILDE OPERATOR", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\10996", description = "TRIPLE VERTICAL BAR BINARY RELATION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10997", description = "TRIPLE VERTICAL BAR WITH HORIZONTAL STROKE", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10998", description = "TRIPLE COLON OPERATOR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\10999", description = "TRIPLE NESTED LESS-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\11000", description = "TRIPLE NESTED GREATER-THAN", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\11001", description = "DOUBLE-LINE SLANTED LESS-THAN OR EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\11002", description = "DOUBLE-LINE SLANTED GREATER-THAN OR EQUAL TO", form = FInfix, priority = 265, lspace = 5, rspace = 5, properties = []}
  , Operator {oper = "\11003", description = "TRIPLE SOLIDUS BINARY RELATION", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\11004", description = "LARGE TRIPLE VERTICAL BAR OPERATOR", form = FPrefix, priority = 330, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\11005", description = "DOUBLE SOLIDUS OPERATOR", form = FInfix, priority = 265, lspace = 4, rspace = 4, properties = []}
  , Operator {oper = "\11006", description = "WHITE VERTICAL BAR", form = FInfix, priority = 265, lspace = 3, rspace = 3, properties = []}
  , Operator {oper = "\11007", description = "N-ARY WHITE VERTICAL BAR", form = FPrefix, priority = 330, lspace = 1, rspace = 2, properties = ["symmetric","largeop","movablelimits"]}
  , Operator {oper = "\11077", description = "LEFTWARDS QUADRUPLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\11078", description = "RIGHTWARDS QUADRUPLE ARROW", form = FInfix, priority = 270, lspace = 5, rspace = 5, properties = ["stretchy"]}
  , Operator {oper = "\65079", description = "PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET" , form = FInfix, priority = 880, lspace = 0, rspace = 0, properties = ["stretchy", "accent"]}
  , Operator {oper = "\65080", description = "PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET", form = FInfix, priority = 880, lspace = 0, rspace = 0, properties = ["stretchy", "accent"]}]
