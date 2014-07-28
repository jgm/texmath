{-# LANGUAGE ViewPatterns #-}
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

This module is derived from the list of unicode to LaTeX mappings
compiled by Günter Milde.

An unmodified original copy of this work can be obtained from <http://milde.users.sourceforge.net/LUCR/Math/ here>

-}

{-

All request for support should be sent to the
current maintainer of this module and NOT the aforementioned original author.

The work was originally licensed under the LaTeX Project Public License.

Changes to the work can be seen via the git commit history to this module.

Whilst distributed under the GPL in conformance with clause 10a, all
deriviations of this work must also conform with clause 6 of the the
LaTeX Project Public License.

-}

module Text.TeXMath.Unicode.ToTeXMath ( getTeXMath
                                      , getSymbolType
                                      , records
                                      ) where

import qualified Data.Map as M
import Text.TeXMath.TeX
import Text.TeXMath.Types
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Control.Applicative hiding (optional)
import Text.TeXMath.Unicode.ToASCII (getASCII)
import Text.TeXMath.Unicode.ToUnicode (fromUnicode)
import qualified Text.TeXMath.Shared as S

-- | Converts a string of unicode characters into a strong of equivalent
-- TeXMath commands. An environment is a list of strings specifying which
-- additional packages are available.
getTeXMath :: String -> Env -> [TeX]
getTeXMath s e = concatMap (charToString e) s

-- Categories which require braces
commandTypes :: [TeXSymbolType]
commandTypes = [Accent, Rad, TOver, TUnder]

-- Guaranteed to return latex safe string
charToString :: Env -> Char -> [TeX]
charToString e c =
  fromMaybe fallback
    (charToLaTeXString e c <|> textConvert e c)
  where
    fallback = concatMap asciiToLaTeX $ getASCII c
    asciiToLaTeX ac = fromMaybe [escapeLaTeX ac] (charToLaTeXString e ac)

-- Takes a single character and attempts to convert it to a latex string
charToLaTeXString :: Env -> Char -> Maybe [TeX]
charToLaTeXString environment c = do
  v <- M.lookup c recordsMap
  -- Required packages for the command
  let toLit [c'] = [Token c']
      toLit []   = []
      toLit cs   = [Literal cs]
  raw <- listToMaybe (mapMaybe (flip lookup (commands v)) environment)
  let latexCommand = if isControlSeq raw
                        then [ControlSeq raw]
                        else toLit raw
  return $ if category v `elem` commandTypes
              then latexCommand ++ [Grouped []]
              else latexCommand

-- Convert special unicode characters not in the standard mapping
textConvert :: Env -> Char -> Maybe [TeX]
textConvert env c = do
  (ttype, v) <- fromUnicode c
  return [ControlSeq (S.getLaTeXTextCommand env ttype), Grouped [Token v]]


recordsMap :: M.Map Char Record
recordsMap = M.fromList (map f records)
  where
    f r = (uchar r, r)

-- | Returns TeX symbol type corresponding to a unicode character.
getSymbolType :: Char -> TeXSymbolType
getSymbolType c = fromMaybe Ord (category <$> M.lookup c recordsMap)


records :: [Record]
records =
  [ Record {uchar = '!', commands = [("base","!"),("unicode","\\exclam")], category = Pun, comments = "EXCLAMATION MARK"}
  , Record {uchar = '#', commands = [("base","\\#"),("oz","\\#"),("unicode","\\octothorpe")], category = Ord, comments = "NUMBER SIGN"}
  , Record {uchar = '$', commands = [("base","\\$"),("base","\\mathdollar"),("unicode","\\mathdollar")], category = Ord, comments = "DOLLAR SIGN"}
  , Record {uchar = '%', commands = [("base","\\%"),("unicode","\\percent")], category = Ord, comments = "PERCENT SIGN"}
  , Record {uchar = '&', commands = [("base","\\&"),("stmaryrd","\\binampersand"),("unicode","\\ampersand")], category = Ord, comments = ""}
  , Record {uchar = '(', commands = [("base","("),("unicode","\\lparen")], category = Open, comments = "LEFT PARENTHESIS"}
  , Record {uchar = ')', commands = [("base",")"),("unicode","\\rparen")], category = Close, comments = "RIGHT PARENTHESIS"}
  , Record {uchar = '*', commands = [("base","*"),("base","\\ast"),("unicode","")], category = Ord, comments = "(high) ASTERISK, star"}
  , Record {uchar = '+', commands = [("base","+"),("unicode","\\plus")], category = Bin, comments = "PLUS SIGN"}
  , Record {uchar = ',', commands = [("base",","),("unicode","\\comma")], category = Pun, comments = "COMMA"}
  , Record {uchar = '-', commands = [("base","-"),("unicode","")], category = Bin, comments = "HYPHEN-MINUS (deprecated for math)"}
  , Record {uchar = '.', commands = [("base","."),("unicode","\\period")], category = Alpha, comments = "FULL STOP, period"}
  , Record {uchar = '/', commands = [("base","/"),("base","\\slash"),("unicode","\\mathslash")], category = Ord, comments = "SOLIDUS"}
  , Record {uchar = '0', commands = [("base","0"),("unicode","")], category = Ord, comments = "DIGIT ZERO"}
  , Record {uchar = '1', commands = [("base","1"),("unicode","")], category = Ord, comments = "DIGIT ONE"}
  , Record {uchar = '2', commands = [("base","2"),("unicode","")], category = Ord, comments = "DIGIT TWO"}
  , Record {uchar = '3', commands = [("base","3"),("unicode","")], category = Ord, comments = "DIGIT THREE"}
  , Record {uchar = '4', commands = [("base","4"),("unicode","")], category = Ord, comments = "DIGIT FOUR"}
  , Record {uchar = '5', commands = [("base","5"),("unicode","")], category = Ord, comments = "DIGIT FIVE"}
  , Record {uchar = '6', commands = [("base","6"),("unicode","")], category = Ord, comments = "DIGIT SIX"}
  , Record {uchar = '7', commands = [("base","7"),("unicode","")], category = Ord, comments = "DIGIT SEVEN"}
  , Record {uchar = '8', commands = [("base","8"),("unicode","")], category = Ord, comments = "DIGIT EIGHT"}
  , Record {uchar = '9', commands = [("base","9"),("unicode","")], category = Ord, comments = "DIGIT NINE"}
  , Record {uchar = ':', commands = [("base",":"),("literal","\\colon"),("unicode","\\mathcolon")], category = Pun, comments = "COLON (not ratio)"}
  , Record {uchar = ';', commands = [("base",";"),("unicode","\\semicolon")], category = Pun, comments = "SEMICOLON p:"}
  , Record {uchar = '<', commands = [("base","<"),("unicode","\\less")], category = Rel, comments = "LESS-THAN SIGN r:"}
  , Record {uchar = '=', commands = [("base","="),("unicode","\\equal")], category = Rel, comments = "EQUALS SIGN r:"}
  , Record {uchar = '>', commands = [("base",">"),("unicode","\\greater")], category = Rel, comments = "GREATER-THAN SIGN r:"}
  , Record {uchar = '?', commands = [("base","?"),("unicode","\\question")], category = Ord, comments = "QUESTION MARK"}
  , Record {uchar = '@', commands = [("base","@"),("unicode","\\atsign")], category = Ord, comments = "at"}
  , Record {uchar = 'A', commands = [("base","A"),("base","\\mathrm{A}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER A"}
  , Record {uchar = 'B', commands = [("base","B"),("base","\\mathrm{B}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER B"}
  , Record {uchar = 'C', commands = [("base","C"),("base","\\mathrm{C}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER C"}
  , Record {uchar = 'D', commands = [("base","D"),("base","\\mathrm{D}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER D"}
  , Record {uchar = 'E', commands = [("base","E"),("base","\\mathrm{E}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER E"}
  , Record {uchar = 'F', commands = [("base","F"),("base","\\mathrm{F}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER F"}
  , Record {uchar = 'G', commands = [("base","G"),("base","\\mathrm{G}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER G"}
  , Record {uchar = 'H', commands = [("base","H"),("base","\\mathrm{H}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER H"}
  , Record {uchar = 'I', commands = [("base","I"),("base","\\mathrm{I}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER I"}
  , Record {uchar = 'J', commands = [("base","J"),("base","\\mathrm{J}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER J"}
  , Record {uchar = 'K', commands = [("base","K"),("base","\\mathrm{K}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER K"}
  , Record {uchar = 'L', commands = [("base","L"),("base","\\mathrm{L}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER L"}
  , Record {uchar = 'M', commands = [("base","M"),("base","\\mathrm{M}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER M"}
  , Record {uchar = 'N', commands = [("base","N"),("base","\\mathrm{N}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER N"}
  , Record {uchar = 'O', commands = [("base","O"),("base","\\mathrm{O}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER O"}
  , Record {uchar = 'P', commands = [("base","P"),("base","\\mathrm{P}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER P"}
  , Record {uchar = 'Q', commands = [("base","Q"),("base","\\mathrm{Q}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER Q"}
  , Record {uchar = 'R', commands = [("base","R"),("base","\\mathrm{R}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER R"}
  , Record {uchar = 'S', commands = [("base","S"),("base","\\mathrm{S}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER S"}
  , Record {uchar = 'T', commands = [("base","T"),("base","\\mathrm{T}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER T"}
  , Record {uchar = 'U', commands = [("base","U"),("base","\\mathrm{U}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER U"}
  , Record {uchar = 'V', commands = [("base","V"),("base","\\mathrm{V}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER V"}
  , Record {uchar = 'W', commands = [("base","W"),("base","\\mathrm{W}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER W"}
  , Record {uchar = 'X', commands = [("base","X"),("base","\\mathrm{X}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER X"}
  , Record {uchar = 'Y', commands = [("base","Y"),("base","\\mathrm{Y}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER Y"}
  , Record {uchar = 'Z', commands = [("base","Z"),("base","\\mathrm{Z}"),("unicode","")], category = Alpha, comments = "LATIN CAPITAL LETTER Z"}
  , Record {uchar = '[', commands = [("base","\\lbrack"),("unicode","\\lbrack")], category = Open, comments = "LEFT SQUARE BRACKET"}
  , Record {uchar = '\\', commands = [("base","\\backslash"),("unicode","\\backslash")], category = Ord, comments = "REVERSE SOLIDUS"}
  , Record {uchar = ']', commands = [("base","\\rbrack"),("unicode","\\rbrack")], category = Close, comments = "RIGHT SQUARE BRACKET"}
  , Record {uchar = '^', commands = [("base","\\hat{}"),("mathord",""),("unicode","\\sphat")], category = Ord, comments = "amsxtra^CIRCUMFLEX ACCENT, TeX superscript operator"}
  , Record {uchar = '_', commands = [("base","\\_"),("unicode","")], category = Ord, comments = "LOW LINE, TeX subscript operator"}
  , Record {uchar = '`', commands = [("base",""),("unicode","")], category = Ord, comments = "grave, alias for 0300"}
  , Record {uchar = 'a', commands = [("base","a"),("base","\\mathrm{a}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER A"}
  , Record {uchar = 'b', commands = [("base","b"),("base","\\mathrm{b}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER B"}
  , Record {uchar = 'c', commands = [("base","c"),("base","\\mathrm{c}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER C"}
  , Record {uchar = 'd', commands = [("base","d"),("base","\\mathrm{d}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER D"}
  , Record {uchar = 'e', commands = [("base","e"),("base","\\mathrm{e}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER E"}
  , Record {uchar = 'f', commands = [("base","f"),("base","\\mathrm{f}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER F"}
  , Record {uchar = 'g', commands = [("base","g"),("base","\\mathrm{g}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER G"}
  , Record {uchar = 'h', commands = [("base","h"),("base","\\mathrm{h}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER H"}
  , Record {uchar = 'i', commands = [("base","i"),("base","\\mathrm{i}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER I"}
  , Record {uchar = 'j', commands = [("base","j"),("base","\\mathrm{j}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER J"}
  , Record {uchar = 'k', commands = [("base","k"),("base","\\mathrm{k}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER K"}
  , Record {uchar = 'l', commands = [("base","l"),("base","\\mathrm{l}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER L"}
  , Record {uchar = 'm', commands = [("base","m"),("base","\\mathrm{m}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER M"}
  , Record {uchar = 'n', commands = [("base","n"),("base","\\mathrm{n}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER N"}
  , Record {uchar = 'o', commands = [("base","o"),("base","\\mathrm{o}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER O"}
  , Record {uchar = 'p', commands = [("base","p"),("base","\\mathrm{p}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER P"}
  , Record {uchar = 'q', commands = [("base","q"),("base","\\mathrm{q}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER Q"}
  , Record {uchar = 'r', commands = [("base","r"),("base","\\mathrm{r}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER R"}
  , Record {uchar = 's', commands = [("base","s"),("base","\\mathrm{s}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER S"}
  , Record {uchar = 't', commands = [("base","t"),("base","\\mathrm{t}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER T"}
  , Record {uchar = 'u', commands = [("base","u"),("base","\\mathrm{u}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER U"}
  , Record {uchar = 'v', commands = [("base","v"),("base","\\mathrm{v}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER V"}
  , Record {uchar = 'w', commands = [("base","w"),("base","\\mathrm{w}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER W"}
  , Record {uchar = 'x', commands = [("base","x"),("base","\\mathrm{x}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER X"}
  , Record {uchar = 'y', commands = [("base","y"),("base","\\mathrm{y}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER Y"}
  , Record {uchar = 'z', commands = [("base","z"),("base","\\mathrm{z}"),("unicode","")], category = Alpha, comments = "LATIN SMALL LETTER Z"}
  , Record {uchar = '{', commands = [("base","\\{"),("base","\\lbrace"),("unicode","\\lbrace")], category = Open, comments = "LEFT CURLY BRACKET"}
  , Record {uchar = '|', commands = [("base","|"),("base","\\vert"),("unicode","\\vert")], category = Fence, comments = "vertical bar"}
  , Record {uchar = '}', commands = [("base","\\}"),("base","\\rbrace"),("unicode","\\rbrace")], category = Close, comments = "RIGHT CURLY BRACKET"}
  , Record {uchar = '~', commands = [("amsxtra","\\sptilde"),("base","\\sim"),("unicode","")], category = Ord, comments = "TILDE"}
  , Record {uchar = '\160', commands = [("base","~"),("unicode","")], category = Ord, comments = "nbsp"}
  , Record {uchar = '\161', commands = [("base",""),("unicode","")], category = Ord, comments = "iexcl"}
  , Record {uchar = '\162', commands = [("wasysym","\\cent"),("txfonts","\\mathcent"),("unicode","")], category = Ord, comments = "cent"}
  , Record {uchar = '\163', commands = [("base","\\pounds"),("txfonts","\\mathsterling"),("unicode","\\sterling")], category = Ord, comments = "POUND SIGN, fourier prints a dollar sign"}
  , Record {uchar = '\164', commands = [("base",""),("unicode","")], category = Ord, comments = "curren"}
  , Record {uchar = '\165', commands = [("amsfonts","\\yen"),("unicode","\\yen")], category = Ord, comments = "YEN SIGN"}
  , Record {uchar = '\166', commands = [("base",""),("unicode","")], category = Ord, comments = "brvbar (vertical)"}
  , Record {uchar = '\167', commands = [("base",""),("unicode","")], category = Ord, comments = "sect"}
  , Record {uchar = '\168', commands = [("amsxtra","\\spddot"),("unicode","")], category = Ord, comments = "Dot /die, alias for 0308"}
  , Record {uchar = '\172', commands = [("base","\\neg"),("base","\\lnot"),("unicode","\\neg")], category = Ord, comments = "NOT SIGN"}
  , Record {uchar = '\174', commands = [("amsfonts","\\circledR"),("unicode","")], category = Ord, comments = "REGISTERED SIGN"}
  , Record {uchar = '\175', commands = [("base",""),("unicode","")], category = Ord, comments = "macr, alias for 0304"}
  , Record {uchar = '\176', commands = [("base",""),("unicode","")], category = Ord, comments = "deg"}
  , Record {uchar = '\177', commands = [("base","\\pm"),("unicode","\\pm")], category = Bin, comments = "plus-or-minus sign"}
  , Record {uchar = '\178', commands = [("base",""),("unicode","")], category = Ord, comments = "sup2"}
  , Record {uchar = '\179', commands = [("base",""),("unicode","")], category = Ord, comments = "sup3"}
  , Record {uchar = '\180', commands = [("base",""),("unicode","")], category = Ord, comments = "acute, alias for 0301"}
  , Record {uchar = '\181', commands = [("wrisym","\\Micro"),("mathcomp","\\tcmu"),("unicode","")], category = Alpha, comments = "t \\textmu (textcomp), # \\mathrm{\\mu} (omlmathrm), # \\muup (kpfonts mathdesign), MICRO SIGN"}
  , Record {uchar = '\182', commands = [("base",""),("unicode","")], category = Ord, comments = "para (paragraph sign, pilcrow)"}
  , Record {uchar = '\183', commands = [("base",""),("base","\\cdot"),("unicode","\\cdotp")], category = Bin, comments = "x \\centerdot, b: MIDDLE DOT"}
  , Record {uchar = '\185', commands = [("base",""),("unicode","")], category = Ord, comments = "sup1"}
  , Record {uchar = '\188', commands = [("base",""),("unicode","")], category = Ord, comments = "frac14"}
  , Record {uchar = '\189', commands = [("base",""),("unicode","")], category = Ord, comments = "frac12"}
  , Record {uchar = '\190', commands = [("base",""),("unicode","")], category = Ord, comments = "frac34"}
  , Record {uchar = '\191', commands = [("base",""),("unicode","")], category = Ord, comments = "iquest"}
  , Record {uchar = '\215', commands = [("base","\\times"),("unicode","\\times")], category = Bin, comments = "MULTIPLICATION SIGN, z notation Cartesian product"}
  , Record {uchar = '\240', commands = [("amssymb","\\eth"),("arevmath","\\eth"),("unicode","\\matheth")], category = Alpha, comments = "eth"}
  , Record {uchar = '\247', commands = [("base","\\div"),("unicode","\\div")], category = Bin, comments = "divide sign"}
  , Record {uchar = '\305', commands = [("base","\\imath"),("unicode","")], category = Alpha, comments = "imath"}
  , Record {uchar = '\437', commands = [("base",""),("unicode","\\Zbar")], category = Ord, comments = "impedance"}
  , Record {uchar = '\567', commands = [("base","\\jmath"),("unicode","")], category = Alpha, comments = "jmath"}
  , Record {uchar = '\710', commands = [("base","\\hat{}"),("unicode","")], category = Alpha, comments = "circ, alias for 0302"}
  , Record {uchar = '\711', commands = [("base",""),("unicode","")], category = Alpha, comments = "CARON, alias for 030C"}
  , Record {uchar = '\728', commands = [("base",""),("unicode","")], category = Ord, comments = "BREVE, alias for 0306"}
  , Record {uchar = '\729', commands = [("base",""),("unicode","")], category = Ord, comments = "dot, alias for 0307"}
  , Record {uchar = '\730', commands = [("base",""),("unicode","")], category = Ord, comments = "ring, alias for 030A"}
  , Record {uchar = '\732', commands = [("base",""),("unicode","")], category = Ord, comments = "alias for 0303"}
  , Record {uchar = '\768', commands = [("base","\\grave"),("unicode","\\grave")], category = Accent, comments = "grave accent"}
  , Record {uchar = '\769', commands = [("base","\\acute"),("unicode","\\acute")], category = Accent, comments = "acute accent"}
  , Record {uchar = '\770', commands = [("base","\\hat"),("amssymb","\\widehat"),("unicode","\\hat")], category = Accent, comments = "circumflex accent"}
  , Record {uchar = '\771', commands = [("base","\\tilde"),("yhmath, fourier","\\widetilde"),("unicode","\\tilde")], category = Accent, comments = "tilde"}
  , Record {uchar = '\772', commands = [("base","\\bar"),("unicode","\\bar")], category = Accent, comments = "macron"}
  , Record {uchar = '\773', commands = [("base","\\overline"),("unicode","\\overbar")], category = Accent, comments = "overbar embellishment"}
  , Record {uchar = '\774', commands = [("base","\\breve"),("unicode","\\breve")], category = Accent, comments = "breve"}
  , Record {uchar = '\775', commands = [("base","\\dot"),("wrisym","\\Dot"),("unicode","\\dot")], category = Accent, comments = "dot above"}
  , Record {uchar = '\776', commands = [("base","\\ddot"),("wrisym","\\DDot"),("unicode","\\ddot")], category = Accent, comments = "dieresis"}
  , Record {uchar = '\777', commands = [("base",""),("unicode","\\ovhook")], category = Accent, comments = "COMBINING HOOK ABOVE"}
  , Record {uchar = '\778', commands = [("amssymb","\\mathring"),("yhmath","\\ring"),("unicode","\\ocirc")], category = Accent, comments = "ring"}
  , Record {uchar = '\780', commands = [("base","\\check"),("unicode","\\check")], category = Accent, comments = "caron"}
  , Record {uchar = '\784', commands = [("base",""),("unicode","\\candra")], category = Accent, comments = "candrabindu (non-spacing)"}
  , Record {uchar = '\785', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING INVERTED BREVE"}
  , Record {uchar = '\786', commands = [("base",""),("unicode","\\oturnedcomma")], category = Accent, comments = "COMBINING TURNED COMMA ABOVE"}
  , Record {uchar = '\789', commands = [("base",""),("unicode","\\ocommatopright")], category = Accent, comments = "COMBINING COMMA ABOVE RIGHT"}
  , Record {uchar = '\794', commands = [("base",""),("unicode","\\droang")], category = Accent, comments = "left angle above (non-spacing)"}
  , Record {uchar = '\803', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING DOT BELOW"}
  , Record {uchar = '\812', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING CARON BELOW"}
  , Record {uchar = '\813', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING CIRCUMFLEX ACCENT BELOW"}
  , Record {uchar = '\814', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING BREVE BELOW"}
  , Record {uchar = '\815', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING INVERTED BREVE BELOW"}
  , Record {uchar = '\816', commands = [("undertilde","\\utilde"),("unicode","\\wideutilde")], category = Accent, comments = "under tilde accent (multiple characters and non-spacing)"}
  , Record {uchar = '\817', commands = [("base","\\underbar"),("unicode","\\underbar")], category = Accent, comments = "COMBINING MACRON BELOW"}
  , Record {uchar = '\818', commands = [("base","\\underline"),("unicode","")], category = Accent, comments = "COMBINING LOW LINE"}
  , Record {uchar = '\819', commands = [("base",""),("unicode","")], category = Accent, comments = "2lowbar"}
  , Record {uchar = '\824', commands = [("base","\\not"),("unicode","\\not")], category = Accent, comments = "COMBINING LONG SOLIDUS OVERLAY"}
  , Record {uchar = '\826', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING INVERTED BRIDGE BELOW"}
  , Record {uchar = '\831', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING DOUBLE OVERLINE"}
  , Record {uchar = '\838', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING BRIDGE ABOVE"}
  , Record {uchar = '\913', commands = [("base",""),("unicode","\\upAlpha")], category = Alpha, comments = "capital alpha, greek"}
  , Record {uchar = '\914', commands = [("base",""),("unicode","\\upBeta")], category = Alpha, comments = "capital beta, greek"}
  , Record {uchar = '\915', commands = [("base","\\Gamma"),("-slantedGreek","\\Gamma"),("unicode","\\upGamma")], category = Alpha, comments = "= \\mathrm{\\Gamma}, capital gamma, greek"}
  , Record {uchar = '\916', commands = [("base","\\Delta"),("-slantedGreek","\\Delta"),("unicode","\\upDelta")], category = Alpha, comments = "= \\mathrm{\\Delta}, capital delta, greek"}
  , Record {uchar = '\917', commands = [("base",""),("unicode","\\upEpsilon")], category = Alpha, comments = "capital epsilon, greek"}
  , Record {uchar = '\918', commands = [("base",""),("unicode","\\upZeta")], category = Alpha, comments = "capital zeta, greek"}
  , Record {uchar = '\919', commands = [("base",""),("unicode","\\upEta")], category = Alpha, comments = "capital eta, greek"}
  , Record {uchar = '\920', commands = [("base","\\Theta"),("-slantedGreek","\\Theta"),("unicode","\\upTheta")], category = Alpha, comments = "= \\mathrm{\\Theta}, capital theta, greek"}
  , Record {uchar = '\921', commands = [("base",""),("unicode","\\upIota")], category = Alpha, comments = "capital iota, greek"}
  , Record {uchar = '\922', commands = [("base",""),("unicode","\\upKappa")], category = Alpha, comments = "capital kappa, greek"}
  , Record {uchar = '\923', commands = [("base","\\Lambda"),("-slantedGreek","\\Lambda"),("unicode","\\upLambda")], category = Alpha, comments = "= \\mathrm{\\Lambda}, capital lambda, greek"}
  , Record {uchar = '\924', commands = [("base",""),("unicode","\\upMu")], category = Alpha, comments = "capital mu, greek"}
  , Record {uchar = '\925', commands = [("base",""),("unicode","\\upNu")], category = Alpha, comments = "capital nu, greek"}
  , Record {uchar = '\926', commands = [("base","\\Xi"),("-slantedGreek","\\Xi"),("unicode","\\upXi")], category = Alpha, comments = "= \\mathrm{\\Xi}, capital xi, greek"}
  , Record {uchar = '\927', commands = [("base",""),("unicode","\\upOmicron")], category = Alpha, comments = "capital omicron, greek"}
  , Record {uchar = '\928', commands = [("base","\\Pi"),("-slantedGreek","\\Pi"),("unicode","\\upPi")], category = Alpha, comments = "= \\mathrm{\\Pi}, capital pi, greek"}
  , Record {uchar = '\929', commands = [("base",""),("unicode","\\upRho")], category = Alpha, comments = "capital rho, greek"}
  , Record {uchar = '\931', commands = [("base","\\Sigma"),("-slantedGreek","\\Sigma"),("unicode","\\upSigma")], category = Alpha, comments = "= \\mathrm{\\Sigma}, capital sigma, greek"}
  , Record {uchar = '\932', commands = [("base",""),("unicode","\\upTau")], category = Alpha, comments = "capital tau, greek"}
  , Record {uchar = '\933', commands = [("base","\\Upsilon"),("-slantedGreek","\\Upsilon"),("unicode","\\upUpsilon")], category = Alpha, comments = "= \\mathrm{\\Upsilon}, capital upsilon, greek"}
  , Record {uchar = '\934', commands = [("base","\\Phi"),("-slantedGreek","\\Phi"),("unicode","\\upPhi")], category = Alpha, comments = "= \\mathrm{\\Phi}, capital phi, greek"}
  , Record {uchar = '\935', commands = [("base",""),("unicode","\\upChi")], category = Alpha, comments = "capital chi, greek"}
  , Record {uchar = '\936', commands = [("base","\\Psi"),("-slantedGreek","\\Psi"),("unicode","\\upPsi")], category = Alpha, comments = "= \\mathrm{\\Psi}, capital psi, greek"}
  , Record {uchar = '\937', commands = [("base","\\Omega"),("-slantedGreek","\\Omega"),("unicode","\\upOmega")], category = Alpha, comments = "= \\mathrm{\\Omega}, capital omega, greek"}
  , Record {uchar = '\945', commands = [("base","\\alpha"),("omlmathrm","\\mathrm{\\alpha}"),("unicode","\\upalpha")], category = Alpha, comments = "= \\alphaup (kpfonts mathdesign), = \\upalpha (upgreek), alpha, greek"}
  , Record {uchar = '\946', commands = [("base","\\beta"),("omlmathrm","\\mathrm{\\beta}"),("unicode","\\upbeta")], category = Alpha, comments = "= \\betaup (kpfonts mathdesign), = \\upbeta (upgreek), beta, greek"}
  , Record {uchar = '\947', commands = [("base","\\gamma"),("omlmathrm","\\mathrm{\\gamma}"),("unicode","\\upgamma")], category = Alpha, comments = "= \\gammaup (kpfonts mathdesign), = \\upgamma (upgreek), gamma, greek"}
  , Record {uchar = '\948', commands = [("base","\\delta"),("omlmathrm","\\mathrm{\\delta}"),("unicode","\\updelta")], category = Alpha, comments = "= \\deltaup (kpfonts mathdesign), = \\updelta (upgreek), delta, greek"}
  , Record {uchar = '\949', commands = [("base","\\varepsilon"),("omlmathrm","\\mathrm{\\varepsilon}"),("unicode","\\upepsilon")], category = Alpha, comments = "= \\varepsilonup (kpfonts mathdesign), = \\upepsilon (upgreek), rounded epsilon, greek"}
  , Record {uchar = '\950', commands = [("base","\\zeta"),("omlmathrm","\\mathrm{\\zeta}"),("unicode","\\upzeta")], category = Alpha, comments = "= \\zetaup (kpfonts mathdesign), = \\upzeta (upgreek), zeta, greek"}
  , Record {uchar = '\951', commands = [("base","\\eta"),("omlmathrm","\\mathrm{\\eta}"),("unicode","\\upeta")], category = Alpha, comments = "= \\etaup (kpfonts mathdesign), = \\upeta (upgreek), eta, greek"}
  , Record {uchar = '\952', commands = [("base","\\theta"),("omlmathrm","\\mathrm{\\theta}"),("unicode","\\uptheta")], category = Alpha, comments = "= \\thetaup (kpfonts mathdesign), straight theta, = \\uptheta (upgreek), theta, greek"}
  , Record {uchar = '\953', commands = [("base","\\iota"),("omlmathrm","\\mathrm{\\iota}"),("unicode","\\upiota")], category = Alpha, comments = "= \\iotaup (kpfonts mathdesign), = \\upiota (upgreek), iota, greek"}
  , Record {uchar = '\954', commands = [("base","\\kappa"),("omlmathrm","\\mathrm{\\kappa}"),("unicode","\\upkappa")], category = Alpha, comments = "= \\kappaup (kpfonts mathdesign), = \\upkappa (upgreek), kappa, greek"}
  , Record {uchar = '\955', commands = [("base","\\lambda"),("omlmathrm","\\mathrm{\\lambda}"),("unicode","\\uplambda")], category = Alpha, comments = "= \\lambdaup (kpfonts mathdesign), = \\uplambda (upgreek), lambda, greek"}
  , Record {uchar = '\956', commands = [("base","\\mu"),("omlmathrm","\\mathrm{\\mu}"),("unicode","\\upmu")], category = Alpha, comments = "= \\muup (kpfonts mathdesign), = \\upmu (upgreek), mu, greek"}
  , Record {uchar = '\957', commands = [("base","\\nu"),("omlmathrm","\\mathrm{\\nu}"),("unicode","\\upnu")], category = Alpha, comments = "= \\nuup (kpfonts mathdesign), = \\upnu (upgreek), nu, greek"}
  , Record {uchar = '\958', commands = [("base","\\xi"),("omlmathrm","\\mathrm{\\xi}"),("unicode","\\upxi")], category = Alpha, comments = "= \\xiup (kpfonts mathdesign), = \\upxi (upgreek), xi, greek"}
  , Record {uchar = '\959', commands = [("base",""),("unicode","\\upomicron")], category = Alpha, comments = "small omicron, greek"}
  , Record {uchar = '\960', commands = [("base","\\pi"),("omlmathrm","\\mathrm{\\pi}"),("unicode","\\uppi")], category = Alpha, comments = "= \\piup (kpfonts mathdesign), = \\uppi (upgreek), pi, greek"}
  , Record {uchar = '\961', commands = [("base","\\rho"),("omlmathrm","\\mathrm{\\rho}"),("unicode","\\uprho")], category = Alpha, comments = "= \\rhoup (kpfonts mathdesign), = \\uprho (upgreek), rho, greek"}
  , Record {uchar = '\962', commands = [("base","\\varsigma"),("omlmathrm","\\mathrm{\\varsigma}"),("unicode","\\upvarsigma")], category = Alpha, comments = "= \\varsigmaup (kpfonts mathdesign), = \\upvarsigma (upgreek), terminal sigma, greek"}
  , Record {uchar = '\963', commands = [("base","\\sigma"),("omlmathrm","\\mathrm{\\sigma}"),("unicode","\\upsigma")], category = Alpha, comments = "= \\sigmaup (kpfonts mathdesign), = \\upsigma (upgreek), sigma, greek"}
  , Record {uchar = '\964', commands = [("base","\\tau"),("omlmathrm","\\mathrm{\\tau}"),("unicode","\\uptau")], category = Alpha, comments = "= \\tauup (kpfonts mathdesign), = \\uptau (upgreek), tau, greek"}
  , Record {uchar = '\965', commands = [("base","\\upsilon"),("omlmathrm","\\mathrm{\\upsilon}"),("unicode","\\upupsilon")], category = Alpha, comments = "= \\upsilonup (kpfonts mathdesign), = \\upupsilon (upgreek), upsilon, greek"}
  , Record {uchar = '\966', commands = [("base","\\varphi"),("omlmathrm","\\mathrm{\\varphi}"),("unicode","\\upvarphi")], category = Alpha, comments = "= \\varphiup (kpfonts mathdesign), = \\upvarphi (upgreek), curly or open phi, greek"}
  , Record {uchar = '\967', commands = [("base","\\chi"),("omlmathrm","\\mathrm{\\chi}"),("unicode","\\upchi")], category = Alpha, comments = "= \\chiup (kpfonts mathdesign), = \\upchi (upgreek), chi, greek"}
  , Record {uchar = '\968', commands = [("base","\\psi"),("omlmathrm","\\mathrm{\\psi}"),("unicode","\\uppsi")], category = Alpha, comments = "= \\psiup (kpfonts mathdesign), = \\uppsi (upgreek), psi, greek"}
  , Record {uchar = '\969', commands = [("base","\\omega"),("omlmathrm","\\mathrm{\\omega}"),("unicode","\\upomega")], category = Alpha, comments = "= \\omegaup (kpfonts mathdesign), = \\upomega (upgreek), omega, greek"}
  , Record {uchar = '\976', commands = [("arevmath","\\varbeta"),("unicode","\\upvarbeta")], category = Alpha, comments = "rounded beta, greek"}
  , Record {uchar = '\977', commands = [("base","\\vartheta"),("omlmathrm","\\mathrm{\\vartheta}"),("unicode","\\upvartheta")], category = Alpha, comments = "= \\varthetaup (kpfonts mathdesign), curly or open theta"}
  , Record {uchar = '\978', commands = [("base",""),("base","\\mathrm{\\Upsilon}"),("unicode","\\upUpsilon")], category = Alpha, comments = "GREEK UPSILON WITH HOOK SYMBOL"}
  , Record {uchar = '\981', commands = [("base","\\phi"),("omlmathrm","\\mathrm{\\phi}"),("unicode","\\upphi")], category = Alpha, comments = "= \\phiup (kpfonts mathdesign), GREEK PHI SYMBOL (straight)"}
  , Record {uchar = '\982', commands = [("base","\\varpi"),("omlmathrm","\\mathrm{\\varpi}"),("unicode","\\upvarpi")], category = Alpha, comments = "= \\varpiup (kpfonts mathdesign), GREEK PI SYMBOL (pomega)"}
  , Record {uchar = '\984', commands = [("arevmath","\\Qoppa"),("wrisym","\\Koppa"),("unicode","\\upoldKoppa")], category = Ord, comments = "t \\Qoppa (LGR), GREEK LETTER ARCHAIC KOPPA"}
  , Record {uchar = '\985', commands = [("arevmath","\\qoppa"),("wrisym","\\koppa"),("unicode","\\upoldkoppa")], category = Ord, comments = "t \\qoppa (LGR), GREEK SMALL LETTER ARCHAIC KOPPA"}
  , Record {uchar = '\986', commands = [("arevmath","\\Stigma"),("wrisym","\\Stigma"),("unicode","\\upStigma")], category = Alpha, comments = "capital stigma"}
  , Record {uchar = '\987', commands = [("arevmath","\\stigma"),("wrisym","\\stigma"),("unicode","\\upstigma")], category = Alpha, comments = "GREEK SMALL LETTER STIGMA"}
  , Record {uchar = '\988', commands = [("wrisym","\\Digamma"),("amssymb","\\digamma"),("unicode","\\upDigamma")], category = Alpha, comments = "capital digamma"}
  , Record {uchar = '\989', commands = [("arevmath","\\digamma"),("wrisym","\\digamma"),("unicode","\\updigamma")], category = Alpha, comments = "GREEK SMALL LETTER DIGAMMA"}
  , Record {uchar = '\990', commands = [("arevmath","\\Koppa"),("unicode","\\upKoppa")], category = Alpha, comments = "capital koppa"}
  , Record {uchar = '\991', commands = [("arevmath","\\koppa"),("unicode","\\upkoppa")], category = Alpha, comments = "GREEK SMALL LETTER KOPPA"}
  , Record {uchar = '\992', commands = [("arevmath","\\Sampi"),("wrisym","\\Sampi"),("unicode","\\upSampi")], category = Alpha, comments = "capital sampi"}
  , Record {uchar = '\993', commands = [("arevmath","\\sampi"),("wrisym","\\sampi"),("unicode","\\upsampi")], category = Alpha, comments = "GREEK SMALL LETTER SAMPI"}
  , Record {uchar = '\1008', commands = [("base",""),("unicode","\\upvarkappa")], category = Alpha, comments = "GREEK KAPPA SYMBOL (round)"}
  , Record {uchar = '\1009', commands = [("base","\\varrho"),("omlmathrm","\\mathrm{\\varrho}"),("unicode","\\upvarrho")], category = Alpha, comments = "= \\varrhoup (kpfonts mathdesign), GREEK RHO SYMBOL (round)"}
  , Record {uchar = '\1012', commands = [("base",""),("unicode","\\upvarTheta")], category = Alpha, comments = "GREEK CAPITAL THETA SYMBOL"}
  , Record {uchar = '\1013', commands = [("base","\\epsilon"),("omlmathrm","\\mathrm{\\epsilon}"),("unicode","\\upvarepsilon")], category = Alpha, comments = "= \\epsilonup (kpfonts mathdesign), GREEK LUNATE EPSILON SYMBOL"}
  , Record {uchar = '\1014', commands = [("amssymb","\\backepsilon"),("wrisym","\\backepsilon"),("unicode","\\upbackepsilon")], category = Ord, comments = "GREEK REVERSED LUNATE EPSILON SYMBOL"}
  , Record {uchar = '\1064', commands = [("base",""),("unicode","")], category = Alpha, comments = "Shcy, CYRILLIC CAPITAL LETTER SHA"}
  , Record {uchar = '\8192', commands = [("base",""),("unicode","")], category = Ord, comments = "enquad"}
  , Record {uchar = '\8193', commands = [("base","\\quad"),("unicode","")], category = Ord, comments = "emquad"}
  , Record {uchar = '\8194', commands = [("base",""),("unicode","")], category = Ord, comments = "ensp (half an em)"}
  , Record {uchar = '\8195', commands = [("base",""),("unicode","")], category = Ord, comments = "emsp"}
  , Record {uchar = '\8196', commands = [("base",""),("unicode","")], category = Ord, comments = "THREE-PER-EM SPACE"}
  , Record {uchar = '\8197', commands = [("base",""),("unicode","")], category = Ord, comments = "FOUR-PER-EM SPACE, mid space"}
  , Record {uchar = '\8198', commands = [("base",""),("unicode","")], category = Ord, comments = "SIX-PER-EM SPACE"}
  , Record {uchar = '\8199', commands = [("base",""),("unicode","")], category = Ord, comments = "FIGURE SPACE"}
  , Record {uchar = '\8201', commands = [("base",""),("unicode","")], category = Ord, comments = "THIN SPACE"}
  , Record {uchar = '\8202', commands = [("base",""),("unicode","")], category = Ord, comments = "HAIR SPACE"}
  , Record {uchar = '\8203', commands = [("base",""),("base","\\hspace{0pt}"),("unicode","")], category = Ord, comments = "zwsp"}
  , Record {uchar = '\8208', commands = [("base",""),("unicode","")], category = Ord, comments = "HYPHEN (true graphic)"}
  , Record {uchar = '\8210', commands = [("base",""),("unicode","")], category = Ord, comments = "dash"}
  , Record {uchar = '\8211', commands = [("base",""),("unicode","")], category = Ord, comments = "ndash"}
  , Record {uchar = '\8212', commands = [("base",""),("unicode","")], category = Ord, comments = "mdash"}
  , Record {uchar = '\8213', commands = [("base",""),("unicode","\\horizbar")], category = Ord, comments = "HORIZONTAL BAR"}
  , Record {uchar = '\8214', commands = [("base","\\|"),("base","\\Vert"),("unicode","\\Vert")], category = Fence, comments = "double vertical bar"}
  , Record {uchar = '\8215', commands = [("base",""),("unicode","\\twolowline")], category = Ord, comments = "DOUBLE LOW LINE (spacing)"}
  , Record {uchar = '\8220', commands = [("base","``")], category = Pun, comments = "Opening curly quote"}
  , Record {uchar = '\8221', commands = [("base","\"")], category = Pun, comments = "Closing curly quote"}
  , Record {uchar = '\8224', commands = [("base","\\dagger"),("unicode","\\dagger")], category = Bin, comments = "DAGGER relation"}
  , Record {uchar = '\8225', commands = [("base","\\ddagger"),("unicode","\\ddagger")], category = Bin, comments = "DOUBLE DAGGER relation"}
  , Record {uchar = '\8226', commands = [("base",""),("base","\\bullet"),("unicode","\\smblkcircle")], category = Bin, comments = "b: round BULLET, filled"}
  , Record {uchar = '\8229', commands = [("base",""),("unicode","\\enleadertwodots")], category = Ord, comments = "double baseline dot (en leader)"}
  , Record {uchar = '\8230', commands = [("base","\\ldots"),("unicode","\\unicodeellipsis")], category = Ord, comments = "ellipsis (horizontal)"}
  , Record {uchar = '\8242', commands = [("base","\\prime"),("unicode","\\prime")], category = Ord, comments = "PRIME or minute, not superscripted"}
  , Record {uchar = '\8243', commands = [("mathabx","\\second"),("unicode","\\dprime")], category = Ord, comments = "DOUBLE PRIME or second, not superscripted"}
  , Record {uchar = '\8244', commands = [("mathabx","\\third"),("unicode","\\trprime")], category = Ord, comments = "TRIPLE PRIME (not superscripted)"}
  , Record {uchar = '\8245', commands = [("amssymb","\\backprime"),("unicode","\\backprime")], category = Ord, comments = "reverse prime, not superscripted"}
  , Record {uchar = '\8246', commands = [("base",""),("unicode","\\backdprime")], category = Ord, comments = "double reverse prime, not superscripted"}
  , Record {uchar = '\8247', commands = [("base",""),("unicode","\\backtrprime")], category = Ord, comments = "not superscripted"}
  , Record {uchar = '\8248', commands = [("base",""),("unicode","\\caretinsert")], category = Ord, comments = "CARET (insertion mark)"}
  , Record {uchar = '\8251', commands = [("base",""),("unicode","")], category = Ord, comments = "REFERENCE MARK, Japanese kome jirushi"}
  , Record {uchar = '\8252', commands = [("base",""),("base","!!"),("unicode","\\Exclam")], category = Ord, comments = "DOUBLE EXCLAMATION MARK"}
  , Record {uchar = '\8256', commands = [("oz","\\cat"),("unicode","\\tieconcat")], category = Bin, comments = "CHARACTER TIE, z notation sequence concatenation"}
  , Record {uchar = '\8259', commands = [("base",""),("unicode","\\hyphenbullet")], category = Ord, comments = "rectangle, filled (HYPHEN BULLET)"}
  , Record {uchar = '\8260', commands = [("base",""),("base","/"),("unicode","\\fracslash")], category = Bin, comments = "FRACTION SLASH"}
  , Record {uchar = '\8263', commands = [("base",""),("base","??"),("unicode","\\Question")], category = Ord, comments = "DOUBLE QUESTION MARK"}
  , Record {uchar = '\8270', commands = [("base",""),("base","\\ast"),("unicode","")], category = Bin, comments = "lowast, LOW ASTERISK"}
  , Record {uchar = '\8271', commands = [("base",""),("unicode","")], category = Ord, comments = "bsemi, REVERSED SEMICOLON"}
  , Record {uchar = '\8272', commands = [("base",""),("unicode","\\closure")], category = Rel, comments = "CLOSE UP (editing mark)"}
  , Record {uchar = '\8273', commands = [("base",""),("unicode","")], category = Ord, comments = "Ast"}
  , Record {uchar = '\8274', commands = [("base",""),("base","./."),("unicode","")], category = Ord, comments = "COMMERCIAL MINUS SIGN"}
  , Record {uchar = '\8279', commands = [("mathabx","\\fourth"),("unicode","\\qprime")], category = Ord, comments = "QUADRUPLE PRIME, not superscripted"}
  , Record {uchar = '\8287', commands = [("base","\\:"),("amsmath","\\medspace"),("unicode","")], category = Ord, comments = "MEDIUM MATHEMATICAL SPACE, four-eighteenths of an em"}
  , Record {uchar = '\8289', commands = [("base",""),("unicode","")], category = Ord, comments = "FUNCTION APPLICATION"}
  , Record {uchar = '\8290', commands = [("base",""),("unicode","")], category = Ord, comments = "INVISIBLE TIMES"}
  , Record {uchar = '\8291', commands = [("base",""),("unicode","")], category = Ord, comments = "INVISIBLE SEPARATOR"}
  , Record {uchar = '\8292', commands = [("base",""),("unicode","")], category = Ord, comments = "INVISIBLE PLUS"}
  , Record {uchar = '\8314', commands = [("base",""),("unicode","")], category = Ord, comments = "SUPERSCRIPT PLUS SIGN subscript operators"}
  , Record {uchar = '\8315', commands = [("base",""),("unicode","")], category = Ord, comments = "SUPERSCRIPT MINUS subscript operators"}
  , Record {uchar = '\8316', commands = [("base",""),("unicode","")], category = Ord, comments = "SUPERSCRIPT EQUALS SIGN subscript operators"}
  , Record {uchar = '\8317', commands = [("base",""),("unicode","")], category = Open, comments = "SUPERSCRIPT LEFT PARENTHESIS subscript operators"}
  , Record {uchar = '\8318', commands = [("base",""),("unicode","")], category = Close, comments = "SUPERSCRIPT RIGHT PARENTHESIS subscript operators"}
  , Record {uchar = '\8330', commands = [("base",""),("unicode","")], category = Ord, comments = "SUBSCRIPT PLUS SIGN superscript operators"}
  , Record {uchar = '\8331', commands = [("base",""),("unicode","")], category = Ord, comments = "SUBSCRIPT MINUS superscript operators"}
  , Record {uchar = '\8332', commands = [("base",""),("unicode","")], category = Ord, comments = "SUBSCRIPT EQUALS SIGN superscript operators"}
  , Record {uchar = '\8333', commands = [("base",""),("unicode","")], category = Open, comments = "SUBSCRIPT LEFT PARENTHESIS superscript operators"}
  , Record {uchar = '\8334', commands = [("base",""),("unicode","")], category = Close, comments = "SUBSCRIPT RIGHT PARENTHESIS superscript operators"}
  , Record {uchar = '\8364', commands = [("base",""),("unicode","\\euro")], category = Ord, comments = "EURO SIGN"}
  , Record {uchar = '\8400', commands = [("wrisym","\\lvec"),("unicode","\\leftharpoonaccent")], category = Accent, comments = "COMBINING LEFT HARPOON ABOVE"}
  , Record {uchar = '\8401', commands = [("wrisym","\\vec"),("unicode","\\rightharpoonaccent")], category = Accent, comments = "COMBINING RIGHT HARPOON ABOVE"}
  , Record {uchar = '\8402', commands = [("base",""),("unicode","\\vertoverlay")], category = Accent, comments = "COMBINING LONG VERTICAL LINE OVERLAY"}
  , Record {uchar = '\8403', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING SHORT VERTICAL LINE OVERLAY"}
  , Record {uchar = '\8404', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING ANTICLOCKWISE ARROW ABOVE"}
  , Record {uchar = '\8406', commands = [("wrisym","\\LVec"),("base","\\overleftarrow"),("unicode","\\overleftarrow")], category = Accent, comments = "COMBINING LEFT ARROW ABOVE"}
  , Record {uchar = '\8407', commands = [("base","\\vec"),("wrisym","\\Vec"),("unicode","\\vec")], category = Accent, comments = "# \\overrightarrow, COMBINING RIGHT ARROW ABOVE"}
  , Record {uchar = '\8408', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING RING OVERLAY"}
  , Record {uchar = '\8409', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING CLOCKWISE RING OVERLAY"}
  , Record {uchar = '\8410', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING ANTICLOCKWISE RING OVERLAY"}
  , Record {uchar = '\8411', commands = [("amsmath","\\dddot"),("wrisym","\\DDDot"),("unicode","\\dddot")], category = Accent, comments = "COMBINING THREE DOTS ABOVE"}
  , Record {uchar = '\8412', commands = [("amsmath","\\ddddot"),("unicode","\\ddddot")], category = Accent, comments = "COMBINING FOUR DOTS ABOVE"}
  , Record {uchar = '\8413', commands = [("base",""),("unicode","\\enclosecircle")], category = Accent, comments = "COMBINING ENCLOSING CIRCLE"}
  , Record {uchar = '\8414', commands = [("base",""),("unicode","\\enclosesquare")], category = Accent, comments = "COMBINING ENCLOSING SQUARE"}
  , Record {uchar = '\8415', commands = [("base",""),("unicode","\\enclosediamond")], category = Accent, comments = "COMBINING ENCLOSING DIAMOND"}
  , Record {uchar = '\8417', commands = [("amsmath","\\overleftrightarrow"),("unicode","\\overleftrightarrow")], category = Accent, comments = "COMBINING LEFT RIGHT ARROW ABOVE"}
  , Record {uchar = '\8420', commands = [("base",""),("unicode","\\enclosetriangle")], category = Accent, comments = "COMBINING ENCLOSING UPWARD POINTING TRIANGLE"}
  , Record {uchar = '\8421', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING REVERSE SOLIDUS OVERLAY"}
  , Record {uchar = '\8422', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING DOUBLE VERTICAL STROKE OVERLAY, z notation finite function diacritic"}
  , Record {uchar = '\8423', commands = [("base",""),("unicode","\\annuity")], category = Accent, comments = "COMBINING ANNUITY SYMBOL"}
  , Record {uchar = '\8424', commands = [("base",""),("unicode","\\threeunderdot")], category = Accent, comments = "COMBINING TRIPLE UNDERDOT"}
  , Record {uchar = '\8425', commands = [("base",""),("unicode","\\widebridgeabove")], category = Accent, comments = "COMBINING WIDE BRIDGE ABOVE"}
  , Record {uchar = '\8426', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING LEFTWARDS ARROW OVERLAY"}
  , Record {uchar = '\8427', commands = [("base",""),("unicode","")], category = Accent, comments = "COMBINING LONG DOUBLE SOLIDUS OVERLAY"}
  , Record {uchar = '\8428', commands = [("base",""),("unicode","\\underrightharpoondown")], category = Accent, comments = "COMBINING RIGHTWARDS HARPOON WITH BARB DOWNWARDS"}
  , Record {uchar = '\8429', commands = [("base",""),("unicode","\\underleftharpoondown")], category = Accent, comments = "COMBINING LEFTWARDS HARPOON WITH BARB DOWNWARDS"}
  , Record {uchar = '\8430', commands = [("amsmath","\\underleftarrow"),("unicode","\\underleftarrow")], category = Accent, comments = "COMBINING LEFT ARROW BELOW"}
  , Record {uchar = '\8431', commands = [("amsmath","\\underrightarrow"),("unicode","\\underrightarrow")], category = Accent, comments = "COMBINING RIGHT ARROW BELOW"}
  , Record {uchar = '\8432', commands = [("base",""),("unicode","\\asteraccent")], category = Accent, comments = "COMBINING ASTERISK ABOVE"}
  , Record {uchar = '\8450', commands = [("mathbb","\\mathbb{C}"),("dsfont","\\mathds{C}"),("unicode","\\BbbC")], category = Alpha, comments = "open face C"}
  , Record {uchar = '\8455', commands = [("wrisym","\\Euler"),("unicode","\\Eulerconst")], category = Ord, comments = "EULER CONSTANT"}
  , Record {uchar = '\8458', commands = [("urwchancal","\\mathcal{g}"),("unicode","\\mscrg")], category = Alpha, comments = "/scr g, script small letter g"}
  , Record {uchar = '\8459', commands = [("base","\\mathcal{H}"),("unicode","\\mscrH")], category = Alpha, comments = "hamiltonian (script capital H)"}
  , Record {uchar = '\8460', commands = [("eufrak","\\mathfrak{H}"),("unicode","\\mfrakH")], category = Alpha, comments = "/frak H, black-letter capital H"}
  , Record {uchar = '\8461', commands = [("mathbb","\\mathbb{H}"),("dsfont","\\mathds{H}"),("unicode","\\BbbH")], category = Alpha, comments = "open face capital H"}
  , Record {uchar = '\8462', commands = [("base",""),("base","h"),("unicode","\\Planckconst")], category = Ord, comments = "Planck constant"}
  , Record {uchar = '\8463', commands = [("amssymb","\\hslash"),("fourier","\\hslash"),("arevmath","\\hslash"),("wrisym","\\HBar"),("unicode","\\hslash")], category = Alpha, comments = "#\\hbar, Planck's h over 2pi"}
  , Record {uchar = '\8464', commands = [("base","\\mathcal{I}"),("unicode","\\mscrI")], category = Alpha, comments = "/scr I, script capital I"}
  , Record {uchar = '\8465', commands = [("base","\\Im"),("eufrak","\\mathfrak{I}"),("unicode","\\Im")], category = Alpha, comments = "imaginary part"}
  , Record {uchar = '\8466', commands = [("base","\\mathcal{L}"),("unicode","\\mscrL")], category = Alpha, comments = "lagrangian (script capital L)"}
  , Record {uchar = '\8467', commands = [("base","\\ell"),("unicode","\\ell")], category = Alpha, comments = "cursive small l"}
  , Record {uchar = '\8469', commands = [("mathbb","\\mathbb{N}"),("dsfont","\\mathds{N}"),("unicode","\\BbbN")], category = Alpha, comments = "open face N"}
  , Record {uchar = '\8472', commands = [("amssymb","\\wp"),("unicode","\\wp")], category = Alpha, comments = "weierstrass p"}
  , Record {uchar = '\8473', commands = [("mathbb","\\mathbb{P}"),("dsfont","\\mathds{P}"),("unicode","\\BbbP")], category = Alpha, comments = "open face P"}
  , Record {uchar = '\8474', commands = [("mathbb","\\mathbb{Q}"),("dsfont","\\mathds{Q}"),("unicode","\\BbbQ")], category = Alpha, comments = "open face Q"}
  , Record {uchar = '\8475', commands = [("base","\\mathcal{R}"),("unicode","\\mscrR")], category = Alpha, comments = "/scr R, script capital R"}
  , Record {uchar = '\8476', commands = [("base","\\Re"),("eufrak","\\mathfrak{R}"),("unicode","\\Re")], category = Alpha, comments = "real part"}
  , Record {uchar = '\8477', commands = [("mathbb","\\mathbb{R}"),("dsfont","\\mathds{R}"),("unicode","\\BbbR")], category = Alpha, comments = "open face R"}
  , Record {uchar = '\8484', commands = [("mathbb","\\mathbb{Z}"),("dsfont","\\mathds{Z}"),("unicode","\\BbbZ")], category = Alpha, comments = "open face Z"}
  , Record {uchar = '\8486', commands = [("mathcomp","\\tcohm"),("base","\\mathrm{\\Omega}"),("unicode","")], category = Alpha, comments = "ohm (deprecated in math, use greek letter)"}
  , Record {uchar = '\8487', commands = [("amsfonts","\\mho"),("arevmath","\\mho"),("wrisym","\\Mho"),("unicode","\\mho")], category = Ord, comments = "t \\agemO (wasysym), conductance"}
  , Record {uchar = '\8488', commands = [("eufrak","\\mathfrak{Z}"),("unicode","\\mfrakZ")], category = Alpha, comments = "/frak Z, black-letter capital Z"}
  , Record {uchar = '\8489', commands = [("base",""),("unicode","\\turnediota")], category = Alpha, comments = "\197ngstr\246m capital A with ring"}
  , Record {uchar = '\8492', commands = [("base","\\mathcal{B}"),("unicode","\\mscrB")], category = Alpha, comments = "bernoulli function (script capital B)"}
  , Record {uchar = '\8493', commands = [("eufrak","\\mathfrak{C}"),("unicode","\\mfrakC")], category = Alpha, comments = "black-letter capital C"}
  , Record {uchar = '\8495', commands = [("urwchancal","\\mathcal{e}"),("unicode","\\mscre")], category = Alpha, comments = "/scr e, script small letter e"}
  , Record {uchar = '\8496', commands = [("base","\\mathcal{E}"),("unicode","\\mscrE")], category = Alpha, comments = "/scr E, script capital E"}
  , Record {uchar = '\8497', commands = [("base","\\mathcal{F}"),("unicode","\\mscrF")], category = Alpha, comments = "/scr F, script capital F"}
  , Record {uchar = '\8498', commands = [("amssymb","\\Finv"),("unicode","\\Finv")], category = Ord, comments = "TURNED CAPITAL F"}
  , Record {uchar = '\8499', commands = [("base","\\mathcal{M}"),("unicode","\\mscrM")], category = Alpha, comments = "physics m-matrix (SCRIPT CAPITAL M)"}
  , Record {uchar = '\8500', commands = [("urwchancal","\\mathcal{o}"),("unicode","\\mscro")], category = Alpha, comments = "order of (SCRIPT SMALL O)"}
  , Record {uchar = '\8501', commands = [("base","\\aleph"),("unicode","\\aleph")], category = Alpha, comments = "aleph, hebrew"}
  , Record {uchar = '\8502', commands = [("amssymb","\\beth"),("wrisym","\\beth"),("unicode","\\beth")], category = Alpha, comments = "beth, hebrew"}
  , Record {uchar = '\8503', commands = [("amssymb","\\gimel"),("wrisym","\\gimel"),("unicode","\\gimel")], category = Alpha, comments = "gimel, hebrew"}
  , Record {uchar = '\8504', commands = [("amssymb","\\daleth"),("wrisym","\\daleth"),("unicode","\\daleth")], category = Alpha, comments = "daleth, hebrew"}
  , Record {uchar = '\8508', commands = [("bbold","\\mathbb{\\pi}"),("unicode","\\Bbbpi")], category = Ord, comments = "\\DoublePi (wrisym), DOUBLE-STRUCK SMALL PI"}
  , Record {uchar = '\8509', commands = [("bbold","\\mathbb{\\gamma}"),("unicode","\\Bbbgamma")], category = Alpha, comments = "\\EulerGamma (wrisym), DOUBLE-STRUCK SMALL GAMMA"}
  , Record {uchar = '\8510', commands = [("bbold","\\mathbb{\\Gamma}"),("unicode","\\BbbGamma")], category = Alpha, comments = "DOUBLE-STRUCK CAPITAL GAMMA"}
  , Record {uchar = '\8511', commands = [("bbold","\\mathbb{\\Pi}"),("unicode","\\BbbPi")], category = Alpha, comments = "DOUBLE-STRUCK CAPITAL PI"}
  , Record {uchar = '\8512', commands = [("bbold","\\mathbb{\\Sigma}"),("unicode","\\Bbbsum")], category = Op, comments = "DOUBLE-STRUCK N-ARY SUMMATION"}
  , Record {uchar = '\8513', commands = [("base",""),("amssymb","\\Game"),("unicode","\\Game")], category = Ord, comments = "TURNED SANS-SERIF CAPITAL G (amssymb has mirrored G)"}
  , Record {uchar = '\8514', commands = [("base",""),("unicode","\\sansLturned")], category = Ord, comments = "TURNED SANS-SERIF CAPITAL L"}
  , Record {uchar = '\8515', commands = [("base",""),("unicode","\\sansLmirrored")], category = Ord, comments = "REVERSED SANS-SERIF CAPITAL L"}
  , Record {uchar = '\8516', commands = [("stmaryrd","\\Yup"),("unicode","\\Yup")], category = Ord, comments = "TURNED SANS-SERIF CAPITAL Y"}
  , Record {uchar = '\8517', commands = [("wrisym","\\CapitalDifferentialD"),("wrisym","\\DD"),("unicode","\\mitBbbD")], category = Ord, comments = "DOUBLE-STRUCK ITALIC CAPITAL D"}
  , Record {uchar = '\8518', commands = [("wrisym","\\DifferentialD"),("wrisym","\\dd"),("unicode","\\mitBbbd")], category = Ord, comments = "DOUBLE-STRUCK ITALIC SMALL D"}
  , Record {uchar = '\8519', commands = [("wrisym","\\ExponetialE"),("wrisym","\\ee"),("unicode","\\mitBbbe")], category = Ord, comments = "DOUBLE-STRUCK ITALIC SMALL E"}
  , Record {uchar = '\8520', commands = [("wrisym","\\ComplexI"),("wrisym","\\ii"),("unicode","\\mitBbbi")], category = Ord, comments = "DOUBLE-STRUCK ITALIC SMALL I"}
  , Record {uchar = '\8521', commands = [("wrisym","\\ComplexJ"),("wrisym","\\jj"),("unicode","\\mitBbbj")], category = Ord, comments = "DOUBLE-STRUCK ITALIC SMALL J"}
  , Record {uchar = '\8522', commands = [("base",""),("unicode","\\PropertyLine")], category = Ord, comments = "PROPERTY LINE"}
  , Record {uchar = '\8523', commands = [("txfonts","\\invamp"),("stmaryrd","\\bindnasrepma"),("unicode","\\upand")], category = Bin, comments = "TURNED AMPERSAND"}
  , Record {uchar = '\8592', commands = [("base","\\leftarrow"),("base","\\gets"),("unicode","\\leftarrow")], category = Rel, comments = "a: leftward arrow"}
  , Record {uchar = '\8593', commands = [("base","\\uparrow"),("unicode","\\uparrow")], category = Rel, comments = "upward arrow"}
  , Record {uchar = '\8594', commands = [("base","\\rightarrow"),("base","\\to"),("unicode","\\rightarrow")], category = Rel, comments = "= \\tfun (oz), = \\fun (oz), rightward arrow, z notation total function"}
  , Record {uchar = '\8595', commands = [("base","\\downarrow"),("unicode","\\downarrow")], category = Rel, comments = "downward arrow"}
  , Record {uchar = '\8596', commands = [("base","\\leftrightarrow"),("oz","\\rel"),("unicode","\\leftrightarrow")], category = Rel, comments = "LEFT RIGHT ARROW, z notation relation"}
  , Record {uchar = '\8597', commands = [("base","\\updownarrow"),("unicode","\\updownarrow")], category = Rel, comments = "up and down arrow"}
  , Record {uchar = '\8598', commands = [("amssymb","\\nwarrow"),("unicode","\\nwarrow")], category = Rel, comments = "nw pointing arrow"}
  , Record {uchar = '\8599', commands = [("base","\\nearrow"),("unicode","\\nearrow")], category = Rel, comments = "ne pointing arrow"}
  , Record {uchar = '\8600', commands = [("base","\\searrow"),("unicode","\\searrow")], category = Rel, comments = "se pointing arrow"}
  , Record {uchar = '\8601', commands = [("base","\\swarrow"),("unicode","\\swarrow")], category = Rel, comments = "sw pointing arrow"}
  , Record {uchar = '\8602', commands = [("amssymb","\\nleftarrow"),("unicode","\\nleftarrow")], category = Rel, comments = "not left arrow"}
  , Record {uchar = '\8603', commands = [("amssymb","\\nrightarrow"),("unicode","\\nrightarrow")], category = Rel, comments = "not right arrow"}
  , Record {uchar = '\8604', commands = [("base",""),("unicode","\\leftwavearrow")], category = Rel, comments = "left arrow-wavy"}
  , Record {uchar = '\8605', commands = [("base",""),("unicode","\\rightwavearrow")], category = Rel, comments = "right arrow-wavy"}
  , Record {uchar = '\8606', commands = [("amssymb","\\twoheadleftarrow"),("unicode","\\twoheadleftarrow")], category = Rel, comments = "left two-headed arrow"}
  , Record {uchar = '\8607', commands = [("base",""),("unicode","\\twoheaduparrow")], category = Rel, comments = "up two-headed arrow"}
  , Record {uchar = '\8608', commands = [("amssymb","\\twoheadrightarrow"),("oz","\\tsur"),("unicode","\\twoheadrightarrow")], category = Rel, comments = "= \\surj (oz), right two-headed arrow, z notation total surjection"}
  , Record {uchar = '\8609', commands = [("base",""),("unicode","\\twoheaddownarrow")], category = Rel, comments = "down two-headed arrow"}
  , Record {uchar = '\8610', commands = [("amssymb","\\leftarrowtail"),("unicode","\\leftarrowtail")], category = Rel, comments = "left arrow-tailed"}
  , Record {uchar = '\8611', commands = [("amssymb","\\rightarrowtail"),("oz","\\tinj"),("unicode","\\rightarrowtail")], category = Rel, comments = "= \\inj (oz), right arrow-tailed, z notation total injection"}
  , Record {uchar = '\8612', commands = [("stmaryrd","\\mapsfrom"),("kpfonts","\\mappedfrom"),("unicode","\\mapsfrom")], category = Rel, comments = "maps to, leftward"}
  , Record {uchar = '\8613', commands = [("wrisym","\\MapsUp"),("unicode","\\mapsup")], category = Rel, comments = "maps to, upward"}
  , Record {uchar = '\8614', commands = [("base","\\mapsto"),("unicode","\\mapsto")], category = Rel, comments = "maps to, rightward, z notation maplet"}
  , Record {uchar = '\8615', commands = [("wrisym","\\MapsDown"),("unicode","\\mapsdown")], category = Rel, comments = "maps to, downward"}
  , Record {uchar = '\8616', commands = [("base",""),("unicode","\\updownarrowbar")], category = Ord, comments = "UP DOWN ARROW WITH BASE (perpendicular)"}
  , Record {uchar = '\8617', commands = [("base","\\hookleftarrow"),("unicode","\\hookleftarrow")], category = Rel, comments = "left arrow-hooked"}
  , Record {uchar = '\8618', commands = [("base","\\hookrightarrow"),("unicode","\\hookrightarrow")], category = Rel, comments = "right arrow-hooked"}
  , Record {uchar = '\8619', commands = [("amssymb","\\looparrowleft"),("unicode","\\looparrowleft")], category = Rel, comments = "left arrow-looped"}
  , Record {uchar = '\8620', commands = [("amssymb","\\looparrowright"),("unicode","\\looparrowright")], category = Rel, comments = "right arrow-looped"}
  , Record {uchar = '\8621', commands = [("amssymb","\\leftrightsquigarrow"),("unicode","\\leftrightsquigarrow")], category = Rel, comments = "left and right arr-wavy"}
  , Record {uchar = '\8622', commands = [("amssymb","\\nleftrightarrow"),("unicode","\\nleftrightarrow")], category = Rel, comments = "not left and right arrow"}
  , Record {uchar = '\8623', commands = [("stmaryrd","\\lightning"),("unicode","\\downzigzagarrow")], category = Rel, comments = "DOWNWARDS ZIGZAG ARROW"}
  , Record {uchar = '\8624', commands = [("amssymb","\\Lsh"),("unicode","\\Lsh")], category = Rel, comments = "a: UPWARDS ARROW WITH TIP LEFTWARDS"}
  , Record {uchar = '\8625', commands = [("amssymb","\\Rsh"),("unicode","\\Rsh")], category = Rel, comments = "a: UPWARDS ARROW WITH TIP RIGHTWARDS"}
  , Record {uchar = '\8626', commands = [("mathabx","\\dlsh"),("unicode","\\Ldsh")], category = Rel, comments = "left down angled arrow"}
  , Record {uchar = '\8627', commands = [("mathabx","\\drsh"),("unicode","\\Rdsh")], category = Rel, comments = "right down angled arrow"}
  , Record {uchar = '\8628', commands = [("base",""),("unicode","\\linefeed")], category = Ord, comments = "RIGHTWARDS ARROW WITH CORNER DOWNWARDS"}
  , Record {uchar = '\8629', commands = [("base",""),("unicode","\\carriagereturn")], category = Ord, comments = "downwards arrow with corner leftward = carriage return"}
  , Record {uchar = '\8630', commands = [("amssymb","\\curvearrowleft"),("fourier","\\curvearrowleft"),("unicode","\\curvearrowleft")], category = Rel, comments = "left curved arrow"}
  , Record {uchar = '\8631', commands = [("amssymb","\\curvearrowright"),("fourier","\\curvearrowright"),("unicode","\\curvearrowright")], category = Rel, comments = "right curved arrow"}
  , Record {uchar = '\8632', commands = [("base",""),("unicode","\\barovernorthwestarrow")], category = Ord, comments = "NORTH WEST ARROW TO LONG BAR"}
  , Record {uchar = '\8633', commands = [("base",""),("unicode","\\barleftarrowrightarrowba")], category = Ord, comments = "LEFTWARDS ARROW TO BAR OVER RIGHTWARDS ARROW TO BAR"}
  , Record {uchar = '\8634', commands = [("amssymb","\\circlearrowleft"),("wasysym","\\leftturn"),("unicode","\\acwopencirclearrow")], category = Ord, comments = "ANTICLOCKWISE OPEN CIRCLE ARROW"}
  , Record {uchar = '\8635', commands = [("amssymb","\\circlearrowright"),("wasysym","\\rightturn"),("unicode","\\cwopencirclearrow")], category = Ord, comments = "CLOCKWISE OPEN CIRCLE ARROW"}
  , Record {uchar = '\8636', commands = [("base","\\leftharpoonup"),("unicode","\\leftharpoonup")], category = Rel, comments = "left harpoon-up"}
  , Record {uchar = '\8637', commands = [("base","\\leftharpoondown"),("unicode","\\leftharpoondown")], category = Rel, comments = "left harpoon-down"}
  , Record {uchar = '\8638', commands = [("amssymb","\\upharpoonright"),("amssymb","\\restriction"),("unicode","\\upharpoonright")], category = Rel, comments = "= \\upharpoonrightup (wrisym), a: up harpoon-right"}
  , Record {uchar = '\8639', commands = [("amssymb","\\upharpoonleft"),("wrisym","\\upharpoonleftup"),("unicode","\\upharpoonleft")], category = Rel, comments = "up harpoon-left"}
  , Record {uchar = '\8640', commands = [("base","\\rightharpoonup"),("unicode","\\rightharpoonup")], category = Rel, comments = "right harpoon-up"}
  , Record {uchar = '\8641', commands = [("base","\\rightharpoondown"),("unicode","\\rightharpoondown")], category = Rel, comments = "right harpoon-down"}
  , Record {uchar = '\8642', commands = [("amssymb","\\downharpoonright"),("wrisym","\\upharpoonrightdown"),("unicode","\\downharpoonright")], category = Rel, comments = "down harpoon-right"}
  , Record {uchar = '\8643', commands = [("amssymb","\\downharpoonleft"),("wrisym","\\upharpoonleftdown"),("unicode","\\downharpoonleft")], category = Rel, comments = "down harpoon-left"}
  , Record {uchar = '\8644', commands = [("amssymb","\\rightleftarrows"),("wrisym","\\rightleftarrow"),("unicode","\\rightleftarrows")], category = Rel, comments = "right arrow over left arrow"}
  , Record {uchar = '\8645', commands = [("mathabx","\\updownarrows"),("wrisym","\\uparrowdownarrow"),("unicode","\\updownarrows")], category = Rel, comments = "up arrow, down arrow"}
  , Record {uchar = '\8646', commands = [("amssymb","\\leftrightarrows"),("wrisym","\\leftrightarrow"),("unicode","\\leftrightarrows")], category = Rel, comments = "left arrow over right arrow"}
  , Record {uchar = '\8647', commands = [("amssymb","\\leftleftarrows"),("fourier","\\leftleftarrows"),("unicode","\\leftleftarrows")], category = Rel, comments = "left harpoon over right"}
  , Record {uchar = '\8652', commands = [("base","\\rightleftharpoons"),("wrisym","\\equilibrium"),("unicode","\\rightleftharpoons")], category = Rel, comments = "right harpoon over left"}
  , Record {uchar = '\8653', commands = [("amssymb","\\nLeftarrow"),("unicode","\\nLeftarrow")], category = Rel, comments = "not implied by"}
  , Record {uchar = '\8654', commands = [("amssymb","\\nLeftrightarrow"),("unicode","\\nLeftrightarrow")], category = Rel, comments = "not left and right double arrows"}
  , Record {uchar = '\8655', commands = [("amssymb","\\nRightarrow"),("unicode","\\nRightarrow")], category = Rel, comments = "not implies"}
  , Record {uchar = '\8656', commands = [("base","\\Leftarrow"),("unicode","\\Leftarrow")], category = Rel, comments = "left double arrow"}
  , Record {uchar = '\8657', commands = [("base","\\Uparrow"),("unicode","\\Uparrow")], category = Rel, comments = "up double arrow"}
  , Record {uchar = '\8658', commands = [("base","\\Rightarrow"),("unicode","\\Rightarrow")], category = Rel, comments = "right double arrow"}
  , Record {uchar = '\8659', commands = [("base","\\Downarrow"),("unicode","\\Downarrow")], category = Rel, comments = "down double arrow"}
  , Record {uchar = '\8660', commands = [("base","\\Leftrightarrow"),("unicode","\\Leftrightarrow")], category = Rel, comments = "left and right double arrow"}
  , Record {uchar = '\8661', commands = [("base","\\Updownarrow"),("unicode","\\Updownarrow")], category = Rel, comments = "up and down double arrow"}
  , Record {uchar = '\8662', commands = [("txfonts","\\Nwarrow"),("unicode","\\Nwarrow")], category = Rel, comments = "nw pointing double arrow"}
  , Record {uchar = '\8663', commands = [("txfonts","\\Nearrow"),("unicode","\\Nearrow")], category = Rel, comments = "ne pointing double arrow"}
  , Record {uchar = '\8664', commands = [("txfonts","\\Searrow"),("unicode","\\Searrow")], category = Rel, comments = "se pointing double arrow"}
  , Record {uchar = '\8665', commands = [("txfonts","\\Swarrow"),("unicode","\\Swarrow")], category = Rel, comments = "sw pointing double arrow"}
  , Record {uchar = '\8666', commands = [("amssymb","\\Lleftarrow"),("unicode","\\Lleftarrow")], category = Rel, comments = "left triple arrow"}
  , Record {uchar = '\8667', commands = [("amssymb","\\Rrightarrow"),("unicode","\\Rrightarrow")], category = Rel, comments = "right triple arrow"}
  , Record {uchar = '\8668', commands = [("mathabx","\\leftsquigarrow"),("txfonts","\\leftsquigarrow"),("unicode","\\leftsquigarrow")], category = Rel, comments = "LEFTWARDS SQUIGGLE ARROW"}
  , Record {uchar = '\8669', commands = [("amssymb","\\rightsquigarrow"),("unicode","\\rightsquigarrow")], category = Rel, comments = "RIGHTWARDS SQUIGGLE ARROW"}
  , Record {uchar = '\8670', commands = [("base",""),("unicode","\\nHuparrow")], category = Ord, comments = "UPWARDS ARROW WITH DOUBLE STROKE"}
  , Record {uchar = '\8671', commands = [("base",""),("unicode","\\nHdownarrow")], category = Ord, comments = "DOWNWARDS ARROW WITH DOUBLE STROKE"}
  , Record {uchar = '\8672', commands = [("amsfonts","\\dashleftarrow"),("unicode","\\leftdasharrow")], category = Ord, comments = "LEFTWARDS DASHED ARROW"}
  , Record {uchar = '\8673', commands = [("base",""),("unicode","\\updasharrow")], category = Ord, comments = "UPWARDS DASHED ARROW"}
  , Record {uchar = '\8674', commands = [("amsfonts","\\dashrightarrow"),("amsfonts","\\dasharrow"),("unicode","\\rightdasharrow")], category = Ord, comments = "RIGHTWARDS DASHED ARROW"}
  , Record {uchar = '\8675', commands = [("base",""),("unicode","\\downdasharrow")], category = Ord, comments = "DOWNWARDS DASHED ARROW"}
  , Record {uchar = '\8676', commands = [("wrisym","\\LeftArrowBar"),("unicode","\\barleftarrow")], category = Rel, comments = "LEFTWARDS ARROW TO BAR"}
  , Record {uchar = '\8677', commands = [("wrisym","\\RightArrowBar"),("unicode","\\rightarrowbar")], category = Rel, comments = "RIGHTWARDS ARROW TO BAR"}
  , Record {uchar = '\8678', commands = [("base",""),("unicode","\\leftwhitearrow")], category = Ord, comments = "LEFTWARDS WHITE ARROW"}
  , Record {uchar = '\8679', commands = [("base",""),("unicode","\\upwhitearrow")], category = Ord, comments = "UPWARDS WHITE ARROW"}
  , Record {uchar = '\8680', commands = [("base",""),("unicode","\\rightwhitearrow")], category = Ord, comments = "RIGHTWARDS WHITE ARROW"}
  , Record {uchar = '\8681', commands = [("base",""),("unicode","\\downwhitearrow")], category = Ord, comments = "DOWNWARDS WHITE ARROW"}
  , Record {uchar = '\8682', commands = [("base",""),("unicode","\\whitearrowupfrombar")], category = Ord, comments = "UPWARDS WHITE ARROW FROM BAR"}
  , Record {uchar = '\8683', commands = [("base",""),("unicode","")], category = Ord, comments = "UPWARDS WHITE ARROW ON PEDESTAL"}
  , Record {uchar = '\8684', commands = [("base",""),("unicode","")], category = Ord, comments = "UPWARDS WHITE ARROW ON PEDESTAL WITH HORIZONTAL BAR"}
  , Record {uchar = '\8685', commands = [("base",""),("unicode","")], category = Ord, comments = "UPWARDS WHITE ARROW ON PEDESTAL WITH VERTICAL BAR"}
  , Record {uchar = '\8686', commands = [("base",""),("unicode","")], category = Ord, comments = "UPWARDS WHITE DOUBLE ARROW"}
  , Record {uchar = '\8687', commands = [("base",""),("unicode","")], category = Ord, comments = "UPWARDS WHITE DOUBLE ARROW ON PEDESTAL"}
  , Record {uchar = '\8688', commands = [("base",""),("unicode","")], category = Ord, comments = "RIGHTWARDS WHITE ARROW FROM WALL"}
  , Record {uchar = '\8689', commands = [("base",""),("unicode","")], category = Ord, comments = "NORTH WEST ARROW TO CORNER"}
  , Record {uchar = '\8690', commands = [("base",""),("unicode","")], category = Ord, comments = "SOUTH EAST ARROW TO CORNER"}
  , Record {uchar = '\8691', commands = [("base",""),("unicode","")], category = Ord, comments = "UP DOWN WHITE ARROW"}
  , Record {uchar = '\8692', commands = [("base",""),("unicode","\\circleonrightarrow")], category = Rel, comments = "RIGHT ARROW WITH SMALL CIRCLE"}
  , Record {uchar = '\8693', commands = [("mathabx","\\downuparrows"),("wrisym","\\downarrowuparrow"),("unicode","\\downuparrows")], category = Rel, comments = "DOWNWARDS ARROW LEFTWARDS OF UPWARDS ARROW"}
  , Record {uchar = '\8694', commands = [("base",""),("unicode","\\rightthreearrows")], category = Rel, comments = "THREE RIGHTWARDS ARROWS"}
  , Record {uchar = '\8695', commands = [("base",""),("unicode","\\nvleftarrow")], category = Rel, comments = "LEFTWARDS ARROW WITH VERTICAL STROKE"}
  , Record {uchar = '\8696', commands = [("oz","\\pfun"),("unicode","\\nvrightarrow")], category = Rel, comments = "RIGHTWARDS ARROW WITH VERTICAL STROKE, z notation partial function"}
  , Record {uchar = '\8697', commands = [("base",""),("unicode","\\nvleftrightarrow")], category = Rel, comments = "LEFT RIGHT ARROW WITH VERTICAL STROKE, z notation partial relation"}
  , Record {uchar = '\8698', commands = [("base",""),("unicode","\\nVleftarrow")], category = Rel, comments = "LEFTWARDS ARROW WITH DOUBLE VERTICAL STROKE"}
  , Record {uchar = '\8699', commands = [("oz","\\ffun"),("unicode","\\nVrightarrow")], category = Rel, comments = "RIGHTWARDS ARROW WITH DOUBLE VERTICAL STROKE, z notation finite function"}
  , Record {uchar = '\8700', commands = [("base",""),("unicode","\\nVleftrightarrow")], category = Rel, comments = "LEFT RIGHT ARROW WITH DOUBLE VERTICAL STROKE, z notation finite relation"}
  , Record {uchar = '\8701', commands = [("stmaryrd","\\leftarrowtriangle"),("unicode","\\leftarrowtriangle")], category = Rel, comments = "LEFTWARDS OPEN-HEADED ARROW"}
  , Record {uchar = '\8702', commands = [("stmaryrd","\\rightarrowtriangle"),("unicode","\\rightarrowtriangle")], category = Rel, comments = "RIGHTWARDS OPEN-HEADED ARROW"}
  , Record {uchar = '\8703', commands = [("stmaryrd","\\leftrightarrowtriangle"),("unicode","\\leftrightarrowtriangle")], category = Rel, comments = "LEFT RIGHT OPEN-HEADED ARROW"}
  , Record {uchar = '\8704', commands = [("base","\\forall"),("unicode","\\forall")], category = Ord, comments = "FOR ALL"}
  , Record {uchar = '\8705', commands = [("amssymb","\\complement"),("fourier","\\complement"),("unicode","\\complement")], category = Ord, comments = "COMPLEMENT sign"}
  , Record {uchar = '\8706', commands = [("base","\\partial"),("kpfonts","\\partialup"),("unicode","\\partial")], category = Ord, comments = "PARTIAL DIFFERENTIAL"}
  , Record {uchar = '\8707', commands = [("base","\\exists"),("oz","\\exi"),("unicode","\\exists")], category = Ord, comments = "at least one exists"}
  , Record {uchar = '\8708', commands = [("amssymb","\\nexists"),("fourier","\\nexists"),("oz","\\nexi"),("unicode","\\nexists")], category = Ord, comments = "negated exists"}
  , Record {uchar = '\8709', commands = [("amssymb","\\varnothing"),("unicode","\\varnothing")], category = Ord, comments = "circle, slash"}
  , Record {uchar = '\8710', commands = [("base",""),("base","\\mathrm{\\Delta}"),("unicode","\\increment")], category = Ord, comments = "laplacian (Delta; nabla square)"}
  , Record {uchar = '\8711', commands = [("base","\\nabla"),("unicode","\\nabla")], category = Ord, comments = "NABLA, del, hamilton operator"}
  , Record {uchar = '\8712', commands = [("base","\\in"),("unicode","\\in")], category = Rel, comments = "set membership, variant"}
  , Record {uchar = '\8713', commands = [("base","\\notin"),("wrisym","\\nin"),("unicode","\\notin")], category = Rel, comments = "negated set membership"}
  , Record {uchar = '\8714', commands = [("base",""),("unicode","\\smallin")], category = Rel, comments = "set membership (small set membership)"}
  , Record {uchar = '\8715', commands = [("base","\\ni"),("base","\\owns"),("unicode","\\ni")], category = Rel, comments = "contains, variant"}
  , Record {uchar = '\8716', commands = [("wrisym","\\nni"),("txfonts","\\notni"),("unicode","\\nni")], category = Rel, comments = "= \\notowner (mathabx), = \\notowns (fourier), negated contains, variant"}
  , Record {uchar = '\8717', commands = [("base",""),("unicode","\\smallni")], category = Rel, comments = "r: contains (SMALL CONTAINS AS MEMBER)"}
  , Record {uchar = '\8718', commands = [("base",""),("amssymb","\\blacksquare"),("unicode","\\QED")], category = Ord, comments = "END OF PROOF"}
  , Record {uchar = '\8719', commands = [("base","\\prod"),("unicode","\\prod")], category = Op, comments = "product operator"}
  , Record {uchar = '\8720', commands = [("base","\\coprod"),("unicode","\\coprod")], category = Op, comments = "coproduct operator"}
  , Record {uchar = '\8721', commands = [("base","\\sum"),("unicode","\\sum")], category = Op, comments = "summation operator"}
  , Record {uchar = '\8722', commands = [("base","-"),("unicode","\\minus")], category = Bin, comments = "MINUS SIGN"}
  , Record {uchar = '\8723', commands = [("base","\\mp"),("unicode","\\mp")], category = Bin, comments = "MINUS-OR-PLUS SIGN"}
  , Record {uchar = '\8724', commands = [("amssymb","\\dotplus"),("unicode","\\dotplus")], category = Bin, comments = "plus sign, dot above"}
  , Record {uchar = '\8725', commands = [("base","\\slash"),("unicode","\\divslash")], category = Bin, comments = "DIVISION SLASH"}
  , Record {uchar = '\8726', commands = [("amssymb","\\smallsetminus"),("fourier","\\smallsetminus"),("unicode","\\smallsetminus")], category = Bin, comments = "small SET MINUS (cf. reverse solidus)"}
  , Record {uchar = '\8727', commands = [("base","\\ast"),("unicode","\\ast")], category = Bin, comments = "ASTERISK OPERATOR (Hodge star operator)"}
  , Record {uchar = '\8728', commands = [("base","\\circ"),("unicode","\\vysmwhtcircle")], category = Bin, comments = "composite function (small circle)"}
  , Record {uchar = '\8729', commands = [("base","\\bullet"),("unicode","\\vysmblkcircle")], category = Bin, comments = "BULLET OPERATOR"}
  , Record {uchar = '\8730', commands = [("base","\\sqrt"),("unicode","\\sqrt")], category = Rad, comments = "radical"}
  , Record {uchar = '\8731', commands = [("base","\\sqrt[3]"),("unicode","\\cuberoot")], category = Rad, comments = "CUBE ROOT"}
  , Record {uchar = '\8732', commands = [("base","\\sqrt[4]"),("unicode","\\fourthroot")], category = Rad, comments = "FOURTH ROOT"}
  , Record {uchar = '\8733', commands = [("base","\\propto"),("amssymb","\\varpropto"),("unicode","\\propto")], category = Rel, comments = "is PROPORTIONAL TO"}
  , Record {uchar = '\8734', commands = [("base","\\infty"),("unicode","\\infty")], category = Ord, comments = "INFINITY"}
  , Record {uchar = '\8735', commands = [("wrisym","\\rightangle"),("unicode","\\rightangle")], category = Ord, comments = "right (90 degree) angle"}
  , Record {uchar = '\8736', commands = [("base","\\angle"),("unicode","\\angle")], category = Ord, comments = "ANGLE"}
  , Record {uchar = '\8737', commands = [("amssymb","\\measuredangle"),("wrisym","\\measuredangle"),("unicode","\\measuredangle")], category = Ord, comments = "MEASURED ANGLE"}
  , Record {uchar = '\8738', commands = [("amssymb","\\sphericalangle"),("wrisym","\\sphericalangle"),("unicode","\\sphericalangle")], category = Ord, comments = "SPHERICAL ANGLE"}
  , Record {uchar = '\8739', commands = [("base","\\mid"),("unicode","\\mid")], category = Rel, comments = "r: DIVIDES"}
  , Record {uchar = '\8740', commands = [("amssymb","\\nmid"),("unicode","\\nmid")], category = Rel, comments = "negated mid, DOES NOT DIVIDE"}
  , Record {uchar = '\8741', commands = [("base","\\parallel"),("unicode","\\parallel")], category = Rel, comments = "parallel"}
  , Record {uchar = '\8742', commands = [("amssymb","\\nparallel"),("fourier","\\nparallel"),("unicode","\\nparallel")], category = Rel, comments = "not parallel"}
  , Record {uchar = '\8743', commands = [("amssymb","\\wedge"),("base","\\land"),("unicode","\\wedge")], category = Bin, comments = "b: LOGICAL AND"}
  , Record {uchar = '\8744', commands = [("base","\\vee"),("base","\\lor"),("unicode","\\vee")], category = Bin, comments = "b: LOGICAL OR"}
  , Record {uchar = '\8745', commands = [("base","\\cap"),("unicode","\\cap")], category = Bin, comments = "INTERSECTION"}
  , Record {uchar = '\8746', commands = [("base","\\cup"),("unicode","\\cup")], category = Bin, comments = "UNION or logical sum"}
  , Record {uchar = '\8747', commands = [("base","\\int"),("unicode","\\int")], category = Op, comments = "INTEGRAL operator"}
  , Record {uchar = '\8748', commands = [("amsmath","\\iint"),("fourier","\\iint"),("esint","\\iint"),("wasysym","\\iint"),("unicode","\\iint")], category = Op, comments = "DOUBLE INTEGRAL operator"}
  , Record {uchar = '\8749', commands = [("amsmath","\\iiint"),("fourier","\\iiint"),("esint","\\iiint"),("wasysym","\\iiint"),("unicode","\\iiint")], category = Op, comments = "TRIPLE INTEGRAL operator"}
  , Record {uchar = '\8750', commands = [("base","\\oint"),("unicode","\\oint")], category = Op, comments = "CONTOUR INTEGRAL operator"}
  , Record {uchar = '\8751', commands = [("esint","\\oiint"),("wasysym","\\oiint"),("fourier","\\oiint"),("wrisym","\\dbloint"),("unicode","\\oiint")], category = Op, comments = "double contour integral operator"}
  , Record {uchar = '\8752', commands = [("txfonts","\\oiiint"),("fourier","\\oiiint"),("unicode","\\oiiint")], category = Op, comments = "contour integral, clockwise"}
  , Record {uchar = '\8755', commands = [("esint","\\ointctrclockwise"),("wrisym","\\cntclockoint"),("unicode","\\ointctrclockwise")], category = Op, comments = "contour integral, anticlockwise"}
  , Record {uchar = '\8756', commands = [("amssymb","\\therefore"),("wrisym","\\therefore"),("wasysym","\\wasytherefore"),("unicode","\\therefore")], category = Ord, comments = "THEREFORE"}
  , Record {uchar = '\8757', commands = [("amssymb","\\because"),("wrisym","\\because"),("unicode","\\because")], category = Ord, comments = "BECAUSE"}
  , Record {uchar = '\8758', commands = [("base",":"),("unicode","\\mathratio")], category = Rel, comments = "RATIO"}
  , Record {uchar = '\8759', commands = [("wrisym","\\Proportion"),("base","::"),("unicode","\\Colon")], category = Rel, comments = "two colons"}
  , Record {uchar = '\8760', commands = [("base",""),("unicode","\\dotminus")], category = Bin, comments = "minus sign, dot above"}
  , Record {uchar = '\8761', commands = [("txfonts","\\eqcolon"),("base","-:"),("unicode","\\dashcolon")], category = Rel, comments = "EXCESS"}
  , Record {uchar = '\8762', commands = [("base",""),("unicode","\\dotsminusdots")], category = Rel, comments = "minus with four dots, GEOMETRIC PROPORTION"}
  , Record {uchar = '\8763', commands = [("base",""),("unicode","\\kernelcontraction")], category = Rel, comments = "HOMOTHETIC"}
  , Record {uchar = '\8764', commands = [("base","\\sim"),("unicode","\\sim")], category = Rel, comments = "similar to, TILDE OPERATOR"}
  , Record {uchar = '\8765', commands = [("amssymb","\\backsim"),("unicode","\\backsim")], category = Rel, comments = "reverse similar"}
  , Record {uchar = '\8766', commands = [("base",""),("unicode","\\invlazys")], category = Bin, comments = "most positive, INVERTED LAZY S"}
  , Record {uchar = '\8767', commands = [("wasysym","\\AC"),("unicode","\\sinewave")], category = Ord, comments = "SINE WAVE, alternating current"}
  , Record {uchar = '\8768', commands = [("amssymb","\\wr"),("unicode","\\wr")], category = Bin, comments = "WREATH PRODUCT"}
  , Record {uchar = '\8769', commands = [("amssymb","\\nsim"),("wrisym","\\nsim"),("unicode","\\nsim")], category = Rel, comments = "not similar"}
  , Record {uchar = '\8770', commands = [("amssymb","\\eqsim"),("unicode","\\eqsim")], category = Rel, comments = "equals, similar"}
  , Record {uchar = '\8771', commands = [("base","\\simeq"),("unicode","\\simeq")], category = Rel, comments = "similar, equals"}
  , Record {uchar = '\8772', commands = [("txfonts","\\nsimeq"),("unicode","\\nsime")], category = Rel, comments = "not similar, equals"}
  , Record {uchar = '\8773', commands = [("base","\\cong"),("unicode","\\cong")], category = Rel, comments = "congruent with"}
  , Record {uchar = '\8774', commands = [("base",""),("unicode","\\simneqq")], category = Rel, comments = "similar, not equals [vert only for 9573 entity]"}
  , Record {uchar = '\8775', commands = [("amssymb","\\ncong"),("wrisym","\\ncong"),("unicode","\\ncong")], category = Rel, comments = "not congruent with"}
  , Record {uchar = '\8776', commands = [("base","\\approx"),("unicode","\\approx")], category = Rel, comments = "approximate"}
  , Record {uchar = '\8777', commands = [("wrisym","\\napprox"),("unicode","\\napprox")], category = Rel, comments = "not approximate"}
  , Record {uchar = '\8778', commands = [("amssymb","\\approxeq"),("unicode","\\approxeq")], category = Rel, comments = "approximate, equals"}
  , Record {uchar = '\8779', commands = [("base",""),("unicode","\\approxident")], category = Rel, comments = "approximately identical to"}
  , Record {uchar = '\8780', commands = [("base",""),("unicode","\\backcong")], category = Rel, comments = "ALL EQUAL TO"}
  , Record {uchar = '\8781', commands = [("base","\\asymp"),("unicode","\\asymp")], category = Rel, comments = "asymptotically equal to"}
  , Record {uchar = '\8782', commands = [("amssymb","\\Bumpeq"),("wrisym","\\Bumpeq"),("unicode","\\Bumpeq")], category = Rel, comments = "bumpy equals"}
  , Record {uchar = '\8783', commands = [("amssymb","\\bumpeq"),("wrisym","\\bumpeq"),("unicode","\\bumpeq")], category = Rel, comments = "bumpy equals, equals"}
  , Record {uchar = '\8784', commands = [("base","\\doteq"),("wrisym","\\dotequal"),("unicode","\\doteq")], category = Rel, comments = "equals, single dot above"}
  , Record {uchar = '\8785', commands = [("amssymb","\\Doteq"),("amssymb","\\doteqdot"),("unicode","\\Doteq")], category = Rel, comments = "/doteq r: equals, even dots"}
  , Record {uchar = '\8786', commands = [("amssymb","\\fallingdotseq"),("unicode","\\fallingdotseq")], category = Rel, comments = "equals, falling dots"}
  , Record {uchar = '\8787', commands = [("amssymb","\\risingdotseq"),("unicode","\\risingdotseq")], category = Rel, comments = "equals, rising dots"}
  , Record {uchar = '\8788', commands = [("mathabx","\\coloneq"),("txfonts","\\coloneqq"),("unicode","\\coloneq")], category = Rel, comments = "= \\SetDelayed (wrisym), # := colon, equals"}
  , Record {uchar = '\8789', commands = [("mathabx","\\eqcolon"),("txfonts","\\eqqcolon"),("unicode","\\eqcolon")], category = Rel, comments = "# =:, equals, colon"}
  , Record {uchar = '\8790', commands = [("amssymb","\\eqcirc"),("unicode","\\eqcirc")], category = Rel, comments = "circle on equals sign"}
  , Record {uchar = '\8791', commands = [("amssymb","\\circeq"),("unicode","\\circeq")], category = Rel, comments = "circle, equals"}
  , Record {uchar = '\8792', commands = [("base",""),("unicode","\\arceq")], category = Rel, comments = "arc, equals; CORRESPONDS TO"}
  , Record {uchar = '\8793', commands = [("mathabx","\\corresponds"),("oz","\\sdef"),("unicode","\\wedgeq")], category = Rel, comments = "t \\Corresponds (marvosym), corresponds to (wedge over equals)"}
  , Record {uchar = '\8794', commands = [("base",""),("unicode","\\veeeq")], category = Rel, comments = "logical or, equals"}
  , Record {uchar = '\8795', commands = [("base",""),("unicode","\\stareq")], category = Rel, comments = "STAR EQUALS"}
  , Record {uchar = '\8796', commands = [("amssymb","\\triangleq"),("oz","\\varsdef"),("unicode","\\triangleq")], category = Rel, comments = "triangle, equals"}
  , Record {uchar = '\8797', commands = [("base",""),("unicode","\\eqdef")], category = Rel, comments = "equals by definition"}
  , Record {uchar = '\8798', commands = [("base",""),("unicode","\\measeq")], category = Rel, comments = "MEASURED BY (m over equals)"}
  , Record {uchar = '\8799', commands = [("base",""),("unicode","\\questeq")], category = Rel, comments = "equal with questionmark"}
  , Record {uchar = '\8800', commands = [("base","\\neq"),("base","\\ne"),("unicode","\\ne")], category = Rel, comments = "r: not equal"}
  , Record {uchar = '\8801', commands = [("base","\\equiv"),("unicode","\\equiv")], category = Rel, comments = "identical with"}
  , Record {uchar = '\8802', commands = [("wrisym","\\nequiv"),("unicode","\\nequiv")], category = Rel, comments = "not identical with"}
  , Record {uchar = '\8803', commands = [("base",""),("unicode","\\Equiv")], category = Rel, comments = "strict equivalence (4 lines)"}
  , Record {uchar = '\8804', commands = [("base","\\leq"),("base","\\le"),("unicode","\\leq")], category = Rel, comments = "r: less-than-or-equal"}
  , Record {uchar = '\8805', commands = [("base","\\geq"),("base","\\ge"),("unicode","\\geq")], category = Rel, comments = "r: greater-than-or-equal"}
  , Record {uchar = '\8806', commands = [("amssymb","\\leqq"),("unicode","\\leqq")], category = Rel, comments = "less, double equals"}
  , Record {uchar = '\8807', commands = [("amssymb","\\geqq"),("unicode","\\geqq")], category = Rel, comments = "greater, double equals"}
  , Record {uchar = '\8808', commands = [("amssymb","\\lneqq"),("unicode","\\lneqq")], category = Rel, comments = "less, not double equals"}
  , Record {uchar = '\8809', commands = [("amssymb","\\gneqq"),("unicode","\\gneqq")], category = Rel, comments = "greater, not double equals"}
  , Record {uchar = '\8810', commands = [("base","\\ll"),("unicode","\\ll")], category = Rel, comments = "much less than, type 2"}
  , Record {uchar = '\8811', commands = [("base","\\gg"),("unicode","\\gg")], category = Rel, comments = "much greater than, type 2"}
  , Record {uchar = '\8812', commands = [("amssymb","\\between"),("unicode","\\between")], category = Rel, comments = "BETWEEN"}
  , Record {uchar = '\8813', commands = [("mathabx","\\notasymp"),("wrisym","\\nasymp"),("unicode","\\nasymp")], category = Rel, comments = "not asymptotically equal to"}
  , Record {uchar = '\8814', commands = [("amssymb","\\nless"),("unicode","\\nless")], category = Rel, comments = "NOT LESS-THAN"}
  , Record {uchar = '\8815', commands = [("amssymb","\\ngtr"),("unicode","\\ngtr")], category = Rel, comments = "NOT GREATER-THAN"}
  , Record {uchar = '\8816', commands = [("amssymb","\\nleq"),("wrisym","\\nleq"),("fourier","\\nleqslant"),("unicode","\\nleq")], category = Rel, comments = "not less-than-or-equal"}
  , Record {uchar = '\8817', commands = [("amssymb","\\ngeq"),("wrisym","\\ngeq"),("fourier","\\ngeqslant"),("unicode","\\ngeq")], category = Rel, comments = "not greater-than-or-equal"}
  , Record {uchar = '\8818', commands = [("amssymb","\\lesssim"),("wasysym","\\apprle"),("unicode","\\lesssim")], category = Rel, comments = "= \\LessTilde (wrisym), less, similar"}
  , Record {uchar = '\8819', commands = [("amssymb","\\gtrsim"),("wasysym","\\apprge"),("unicode","\\gtrsim")], category = Rel, comments = "= \\GreaterTilde (wrisym), greater, similar"}
  , Record {uchar = '\8820', commands = [("wrisym","\\NotLessTilde"),("unicode","\\nlesssim")], category = Rel, comments = "not less, similar"}
  , Record {uchar = '\8821', commands = [("wrisym","\\NotGreaterTilde"),("unicode","\\ngtrsim")], category = Rel, comments = "not greater, similar"}
  , Record {uchar = '\8822', commands = [("amssymb","\\lessgtr"),("unicode","\\lessgtr")], category = Rel, comments = "less, greater"}
  , Record {uchar = '\8823', commands = [("amssymb","\\gtrless"),("wrisym","\\GreaterLess"),("unicode","\\gtrless")], category = Rel, comments = "greater, less"}
  , Record {uchar = '\8824', commands = [("wrisym",""),("unicode","\\nlessgtr")], category = Rel, comments = "not less, greater"}
  , Record {uchar = '\8825', commands = [("wrisym","\\NotGreaterLess"),("unicode","\\ngtrless")], category = Rel, comments = "not greater, less"}
  , Record {uchar = '\8826', commands = [("base","\\prec"),("unicode","\\prec")], category = Rel, comments = "PRECEDES"}
  , Record {uchar = '\8827', commands = [("base","\\succ"),("unicode","\\succ")], category = Rel, comments = "SUCCEEDS"}
  , Record {uchar = '\8828', commands = [("amssymb","\\preccurlyeq"),("wrisym","\\PrecedesSlantEqual"),("unicode","\\preccurlyeq")], category = Rel, comments = "precedes, curly equals"}
  , Record {uchar = '\8829', commands = [("amssymb","\\succcurlyeq"),("wrisym","\\SucceedsSlantEqual"),("unicode","\\succcurlyeq")], category = Rel, comments = "succeeds, curly equals"}
  , Record {uchar = '\8830', commands = [("amssymb","\\precsim"),("wrisym","\\PrecedesTilde"),("unicode","\\precsim")], category = Rel, comments = "precedes, similar"}
  , Record {uchar = '\8831', commands = [("amssymb","\\succsim"),("wrisym","\\SucceedsTilde"),("unicode","\\succsim")], category = Rel, comments = "succeeds, similar"}
  , Record {uchar = '\8832', commands = [("amssymb","\\nprec"),("wrisym","\\nprec"),("unicode","\\nprec")], category = Rel, comments = "not precedes"}
  , Record {uchar = '\8833', commands = [("amssymb","\\nsucc"),("wrisym","\\nsucc"),("unicode","\\nsucc")], category = Rel, comments = "not succeeds"}
  , Record {uchar = '\8834', commands = [("base","\\subset"),("unicode","\\subset")], category = Rel, comments = "subset or is implied by"}
  , Record {uchar = '\8835', commands = [("base","\\supset"),("unicode","\\supset")], category = Rel, comments = "superset or implies"}
  , Record {uchar = '\8836', commands = [("wrisym","\\nsubset"),("unicode","\\nsubset")], category = Rel, comments = "not subset, variant [slash negation]"}
  , Record {uchar = '\8837', commands = [("wrisym","\\nsupset"),("unicode","\\nsupset")], category = Rel, comments = "not superset, variant [slash negation]"}
  , Record {uchar = '\8838', commands = [("base","\\subseteq"),("unicode","\\subseteq")], category = Rel, comments = "subset, equals"}
  , Record {uchar = '\8839', commands = [("base","\\supseteq"),("unicode","\\supseteq")], category = Rel, comments = "superset, equals"}
  , Record {uchar = '\8840', commands = [("amssymb","\\nsubseteq"),("wrisym","\\nsubseteq"),("unicode","\\nsubseteq")], category = Rel, comments = "not subset, equals"}
  , Record {uchar = '\8841', commands = [("amssymb","\\nsupseteq"),("wrisym","\\nsupseteq"),("unicode","\\nsupseteq")], category = Rel, comments = "not superset, equals"}
  , Record {uchar = '\8842', commands = [("amssymb","\\subsetneq"),("fourier","\\varsubsetneq"),("unicode","\\subsetneq")], category = Rel, comments = "subset, not equals"}
  , Record {uchar = '\8843', commands = [("amssymb","\\supsetneq"),("unicode","\\supsetneq")], category = Rel, comments = "superset, not equals"}
  , Record {uchar = '\8844', commands = [("base",""),("unicode","\\cupleftarrow")], category = Bin, comments = "MULTISET"}
  , Record {uchar = '\8845', commands = [("base",""),("unicode","\\cupdot")], category = Bin, comments = "union, with dot"}
  , Record {uchar = '\8846', commands = [("base","\\uplus"),("oz","\\buni"),("unicode","\\uplus")], category = Bin, comments = "plus sign in union"}
  , Record {uchar = '\8847', commands = [("amsfonts","\\sqsubset"),("unicode","\\sqsubset")], category = Rel, comments = "square subset"}
  , Record {uchar = '\8848', commands = [("amsfonts","\\sqsupset"),("unicode","\\sqsupset")], category = Rel, comments = "square superset"}
  , Record {uchar = '\8849', commands = [("base","\\sqsubseteq"),("unicode","\\sqsubseteq")], category = Rel, comments = "square subset, equals"}
  , Record {uchar = '\8850', commands = [("base","\\sqsupseteq"),("unicode","\\sqsupseteq")], category = Rel, comments = "square superset, equals"}
  , Record {uchar = '\8851', commands = [("base","\\sqcap"),("unicode","\\sqcap")], category = Bin, comments = "square intersection"}
  , Record {uchar = '\8852', commands = [("base","\\sqcup"),("unicode","\\sqcup")], category = Bin, comments = "square union"}
  , Record {uchar = '\8853', commands = [("base","\\oplus"),("unicode","\\oplus")], category = Bin, comments = "plus sign in circle"}
  , Record {uchar = '\8854', commands = [("base","\\ominus"),("unicode","\\ominus")], category = Bin, comments = "minus sign in circle"}
  , Record {uchar = '\8855', commands = [("base","\\otimes"),("unicode","\\otimes")], category = Bin, comments = "multiply sign in circle"}
  , Record {uchar = '\8856', commands = [("base","\\oslash"),("unicode","\\oslash")], category = Bin, comments = "solidus in circle"}
  , Record {uchar = '\8857', commands = [("base","\\odot"),("unicode","\\odot")], category = Bin, comments = "middle dot in circle"}
  , Record {uchar = '\8858', commands = [("amssymb","\\circledcirc"),("unicode","\\circledcirc")], category = Bin, comments = "small circle in circle"}
  , Record {uchar = '\8859', commands = [("amssymb","\\circledast"),("unicode","\\circledast")], category = Bin, comments = "asterisk in circle"}
  , Record {uchar = '\8860', commands = [("base",""),("unicode","\\circledequal")], category = Bin, comments = "equal in circle"}
  , Record {uchar = '\8861', commands = [("amssymb","\\circleddash"),("unicode","\\circleddash")], category = Bin, comments = "hyphen in circle"}
  , Record {uchar = '\8862', commands = [("amssymb","\\boxplus"),("unicode","\\boxplus")], category = Bin, comments = "plus sign in box"}
  , Record {uchar = '\8863', commands = [("amssymb","\\boxminus"),("unicode","\\boxminus")], category = Bin, comments = "minus sign in box"}
  , Record {uchar = '\8864', commands = [("amssymb","\\boxtimes"),("unicode","\\boxtimes")], category = Bin, comments = "multiply sign in box"}
  , Record {uchar = '\8865', commands = [("amssymb","\\boxdot"),("stmaryrd","\\boxdot"),("unicode","\\boxdot")], category = Bin, comments = "/dotsquare /boxdot b: small dot in box"}
  , Record {uchar = '\8866', commands = [("base","\\vdash"),("unicode","\\vdash")], category = Rel, comments = "RIGHT TACK, proves, implies, yields, (vertical, dash)"}
  , Record {uchar = '\8867', commands = [("amssymb","\\dashv"),("unicode","\\dashv")], category = Rel, comments = "LEFT TACK, non-theorem, does not yield, (dash, vertical)"}
  , Record {uchar = '\8868', commands = [("base","\\top"),("unicode","\\top")], category = Ord, comments = "DOWN TACK, top"}
  , Record {uchar = '\8869', commands = [("base","\\bot"),("unicode","\\bot")], category = Ord, comments = "UP TACK, bottom"}
  , Record {uchar = '\8870', commands = [("base",""),("base","\\vdash"),("unicode","\\assert")], category = Rel, comments = "ASSERTION (vertical, short dash)"}
  , Record {uchar = '\8871', commands = [("base","\\models"),("unicode","\\models")], category = Rel, comments = "MODELS (vertical, short double dash)"}
  , Record {uchar = '\8872', commands = [("amssymb","\\vDash"),("fourier","\\vDash"),("unicode","\\vDash")], category = Rel, comments = "TRUE (vertical, double dash)"}
  , Record {uchar = '\8873', commands = [("amssymb","\\Vdash"),("unicode","\\Vdash")], category = Rel, comments = "double vertical, dash"}
  , Record {uchar = '\8874', commands = [("amssymb","\\Vvdash"),("unicode","\\Vvdash")], category = Rel, comments = "dash"}
  , Record {uchar = '\8875', commands = [("mathabx","\\VDash"),("txfonts","\\VDash"),("unicode","\\VDash")], category = Rel, comments = "double vert, double dash"}
  , Record {uchar = '\8876', commands = [("amssymb","\\nvdash"),("unicode","\\nvdash")], category = Rel, comments = "not vertical, dash"}
  , Record {uchar = '\8877', commands = [("amssymb","\\nvDash"),("fourier","\\nvDash"),("unicode","\\nvDash")], category = Rel, comments = "not vertical, double dash"}
  , Record {uchar = '\8878', commands = [("amssymb","\\nVdash"),("unicode","\\nVdash")], category = Rel, comments = "not double vertical, dash"}
  , Record {uchar = '\8879', commands = [("amssymb","\\nVDash"),("unicode","\\nVDash")], category = Rel, comments = "not double vert, double dash"}
  , Record {uchar = '\8880', commands = [("base",""),("unicode","\\prurel")], category = Rel, comments = "element PRECEDES UNDER RELATION"}
  , Record {uchar = '\8881', commands = [("base",""),("unicode","\\scurel")], category = Rel, comments = "SUCCEEDS UNDER RELATION"}
  , Record {uchar = '\8882', commands = [("amssymb","\\vartriangleleft"),("unicode","\\vartriangleleft")], category = Rel, comments = "left triangle, open, variant"}
  , Record {uchar = '\8883', commands = [("amssymb","\\vartriangleright"),("unicode","\\vartriangleright")], category = Rel, comments = "right triangle, open, variant"}
  , Record {uchar = '\8884', commands = [("amssymb","\\trianglelefteq"),("wrisym","\\unlhd"),("unicode","\\trianglelefteq")], category = Rel, comments = "left triangle, equals"}
  , Record {uchar = '\8885', commands = [("amssymb","\\trianglerighteq"),("wrisym","\\unrhd"),("unicode","\\trianglerighteq")], category = Rel, comments = "right triangle, equals"}
  , Record {uchar = '\8886', commands = [("txfonts","\\multimapdotbothA"),("unicode","\\origof")], category = Rel, comments = "ORIGINAL OF"}
  , Record {uchar = '\8887', commands = [("txfonts","\\multimapdotbothB"),("unicode","\\imageof")], category = Rel, comments = "IMAGE OF"}
  , Record {uchar = '\8888', commands = [("amssymb","\\multimap"),("unicode","\\multimap")], category = Rel, comments = "/MULTIMAP a:"}
  , Record {uchar = '\8889', commands = [("base",""),("unicode","\\hermitmatrix")], category = Ord, comments = "HERMITIAN CONJUGATE MATRIX"}
  , Record {uchar = '\8890', commands = [("amssymb","\\intercal"),("fourier","\\intercal"),("unicode","\\intercal")], category = Bin, comments = "intercal"}
  , Record {uchar = '\8891', commands = [("amssymb","\\veebar"),("unicode","\\veebar")], category = Bin, comments = "logical or, bar below (large vee); exclusive disjunction"}
  , Record {uchar = '\8892', commands = [("amssymb","\\barwedge"),("unicode","\\barwedge")], category = Bin, comments = "logical NAND (bar over wedge)"}
  , Record {uchar = '\8893', commands = [("base",""),("unicode","\\barvee")], category = Bin, comments = "bar, vee (large vee)"}
  , Record {uchar = '\8894', commands = [("base",""),("unicode","\\measuredrightangle")], category = Ord, comments = "right angle-measured [with arc]"}
  , Record {uchar = '\8895', commands = [("base",""),("unicode","\\varlrtriangle")], category = Ord, comments = "RIGHT TRIANGLE"}
  , Record {uchar = '\8896', commands = [("base","\\bigwedge"),("unicode","\\bigwedge")], category = Op, comments = "logical or operator"}
  , Record {uchar = '\8897', commands = [("base","\\bigvee"),("unicode","\\bigvee")], category = Op, comments = "logical and operator"}
  , Record {uchar = '\8898', commands = [("base","\\bigcap"),("oz","\\dint"),("unicode","\\bigcap")], category = Op, comments = "\\dinter (oz), intersection operator"}
  , Record {uchar = '\8899', commands = [("base","\\bigcup"),("oz","\\duni"),("unicode","\\bigcup")], category = Op, comments = "\\dunion (oz), union operator"}
  , Record {uchar = '\8900', commands = [("base","\\diamond"),("unicode","\\smwhtdiamond")], category = Bin, comments = "DIAMOND OPERATOR (white diamond)"}
  , Record {uchar = '\8901', commands = [("base","\\cdot"),("unicode","\\cdot")], category = Bin, comments = "DOT OPERATOR (small middle dot)"}
  , Record {uchar = '\8902', commands = [("base","\\star"),("unicode","\\star")], category = Bin, comments = "small star, filled, low"}
  , Record {uchar = '\8903', commands = [("amssymb","\\divideontimes"),("unicode","\\divideontimes")], category = Bin, comments = "division on times"}
  , Record {uchar = '\8904', commands = [("base","\\bowtie"),("txfonts","\\lrtimes"),("unicode","\\bowtie")], category = Rel, comments = "BOWTIE"}
  , Record {uchar = '\8905', commands = [("amssymb","\\ltimes"),("unicode","\\ltimes")], category = Bin, comments = "left closed"}
  , Record {uchar = '\8906', commands = [("amssymb","\\rtimes"),("unicode","\\rtimes")], category = Bin, comments = "right closed"}
  , Record {uchar = '\8907', commands = [("amssymb","\\leftthreetimes"),("unicode","\\leftthreetimes")], category = Bin, comments = "LEFT SEMIDIRECT PRODUCT"}
  , Record {uchar = '\8908', commands = [("amssymb","\\rightthreetimes"),("unicode","\\rightthreetimes")], category = Bin, comments = "RIGHT SEMIDIRECT PRODUCT"}
  , Record {uchar = '\8909', commands = [("amssymb","\\backsimeq"),("unicode","\\backsimeq")], category = Rel, comments = "reverse similar, equals"}
  , Record {uchar = '\8910', commands = [("amssymb","\\curlyvee"),("unicode","\\curlyvee")], category = Bin, comments = "CURLY LOGICAL OR"}
  , Record {uchar = '\8911', commands = [("amssymb","\\curlywedge"),("unicode","\\curlywedge")], category = Bin, comments = "CURLY LOGICAL AND"}
  , Record {uchar = '\8912', commands = [("amssymb","\\Subset"),("unicode","\\Subset")], category = Rel, comments = "DOUBLE SUBSET"}
  , Record {uchar = '\8913', commands = [("amssymb","\\Supset"),("unicode","\\Supset")], category = Rel, comments = "DOUBLE SUPERSET"}
  , Record {uchar = '\8914', commands = [("amssymb","\\Cap"),("unicode","\\Cap")], category = Bin, comments = "/cap /doublecap b: DOUBLE INTERSECTION"}
  , Record {uchar = '\8915', commands = [("amssymb","\\Cup"),("unicode","\\Cup")], category = Bin, comments = "/cup /doublecup b: DOUBLE UNION"}
  , Record {uchar = '\8916', commands = [("amssymb","\\pitchfork"),("unicode","\\pitchfork")], category = Rel, comments = "PITCHFORK"}
  , Record {uchar = '\8917', commands = [("mathabx","\\hash"),("unicode","\\equalparallel")], category = Rel, comments = "parallel, equal; equal or parallel"}
  , Record {uchar = '\8918', commands = [("amssymb","\\lessdot"),("unicode","\\lessdot")], category = Rel, comments = "less than, with dot"}
  , Record {uchar = '\8919', commands = [("amssymb","\\gtrdot"),("unicode","\\gtrdot")], category = Rel, comments = "greater than, with dot"}
  , Record {uchar = '\8920', commands = [("amssymb","\\lll"),("unicode","\\lll")], category = Rel, comments = "equals, greater"}
  , Record {uchar = '\8923', commands = [("amssymb","\\gtreqless"),("unicode","\\gtreqless")], category = Rel, comments = "greater, equals, less"}
  , Record {uchar = '\8924', commands = [("base",""),("unicode","\\eqless")], category = Rel, comments = "equal-or-less"}
  , Record {uchar = '\8925', commands = [("base",""),("unicode","\\eqgtr")], category = Rel, comments = "equal-or-greater"}
  , Record {uchar = '\8926', commands = [("amssymb","\\curlyeqprec"),("unicode","\\curlyeqprec")], category = Rel, comments = "curly equals, precedes"}
  , Record {uchar = '\8927', commands = [("amssymb","\\curlyeqsucc"),("unicode","\\curlyeqsucc")], category = Rel, comments = "curly equals, succeeds"}
  , Record {uchar = '\8928', commands = [("amssymb","\\npreceq"),("wrisym","\\npreceq"),("unicode","\\npreccurlyeq")], category = Rel, comments = "DOES NOT PRECEDE OR EQUAL"}
  , Record {uchar = '\8929', commands = [("amssymb","\\nsucceq"),("wrisym","\\nsucceq"),("unicode","\\nsucccurlyeq")], category = Rel, comments = "not succeeds, curly equals"}
  , Record {uchar = '\8930', commands = [("wrisym","\\nsqsubseteq"),("unicode","\\nsqsubseteq")], category = Rel, comments = "not, square subset, equals"}
  , Record {uchar = '\8931', commands = [("wrisym","\\nsqsupseteq"),("unicode","\\nsqsupseteq")], category = Rel, comments = "not, square superset, equals"}
  , Record {uchar = '\8932', commands = [("base",""),("unicode","\\sqsubsetneq")], category = Rel, comments = "square subset, not equals"}
  , Record {uchar = '\8933', commands = [("base",""),("unicode","\\sqsupsetneq")], category = Rel, comments = "square superset, not equals"}
  , Record {uchar = '\8934', commands = [("amssymb","\\lnsim"),("unicode","\\lnsim")], category = Rel, comments = "less, not similar"}
  , Record {uchar = '\8935', commands = [("amssymb","\\gnsim"),("unicode","\\gnsim")], category = Rel, comments = "greater, not similar"}
  , Record {uchar = '\8936', commands = [("amssymb","\\precnsim"),("unicode","\\precnsim")], category = Rel, comments = "precedes, not similar"}
  , Record {uchar = '\8937', commands = [("amssymb","\\succnsim"),("unicode","\\succnsim")], category = Rel, comments = "succeeds, not similar"}
  , Record {uchar = '\8938', commands = [("amssymb","\\ntriangleleft"),("wrisym","\\NotLeftTriangle"),("unicode","\\ntriangleleft")], category = Rel, comments = "not left triangle"}
  , Record {uchar = '\8939', commands = [("amssymb","\\ntriangleright"),("wrisym","\\NotRightTriangle"),("unicode","\\ntriangleright")], category = Rel, comments = "not right triangle"}
  , Record {uchar = '\8940', commands = [("amssymb","\\ntrianglelefteq"),("wrisym","\\nunlhd"),("unicode","\\ntrianglelefteq")], category = Rel, comments = "not left triangle, equals"}
  , Record {uchar = '\8941', commands = [("amssymb","\\ntrianglerighteq"),("wrisym","\\nunrhd"),("unicode","\\ntrianglerighteq")], category = Rel, comments = "not right triangle, equals"}
  , Record {uchar = '\8942', commands = [("base","\\vdots"),("unicode","\\vdots")], category = Rel, comments = "VERTICAL ELLIPSIS"}
  , Record {uchar = '\8943', commands = [("base","\\cdots"),("unicode","\\unicodecdots")], category = Ord, comments = "centered"}
  , Record {uchar = '\8944', commands = [("mathdots","\\iddots"),("yhmath","\\adots"),("unicode","\\adots")], category = Rel, comments = "three dots, ascending"}
  , Record {uchar = '\8945', commands = [("base","\\ddots"),("unicode","\\ddots")], category = Rel, comments = "descending"}
  , Record {uchar = '\8946', commands = [("base",""),("unicode","\\disin")], category = Rel, comments = "ELEMENT OF WITH LONG HORIZONTAL STROKE"}
  , Record {uchar = '\8947', commands = [("base",""),("unicode","\\varisins")], category = Rel, comments = "ELEMENT OF WITH VERTICAL BAR AT END OF HORIZONTAL STROKE"}
  , Record {uchar = '\8948', commands = [("base",""),("unicode","\\isins")], category = Rel, comments = "SMALL ELEMENT OF WITH VERTICAL BAR AT END OF HORIZONTAL STROKE"}
  , Record {uchar = '\8949', commands = [("base",""),("unicode","\\isindot")], category = Rel, comments = "ELEMENT OF WITH DOT ABOVE"}
  , Record {uchar = '\8950', commands = [("mathabx","\\barin"),("unicode","\\varisinobar")], category = Rel, comments = "ELEMENT OF WITH OVERBAR"}
  , Record {uchar = '\8951', commands = [("base",""),("unicode","\\isinobar")], category = Rel, comments = "SMALL ELEMENT OF WITH OVERBAR"}
  , Record {uchar = '\8952', commands = [("base",""),("unicode","\\isinvb")], category = Rel, comments = "ELEMENT OF WITH UNDERBAR"}
  , Record {uchar = '\8953', commands = [("base",""),("unicode","\\isinE")], category = Rel, comments = "ELEMENT OF WITH TWO HORIZONTAL STROKES"}
  , Record {uchar = '\8954', commands = [("base",""),("unicode","\\nisd")], category = Rel, comments = "CONTAINS WITH LONG HORIZONTAL STROKE"}
  , Record {uchar = '\8955', commands = [("base",""),("unicode","\\varnis")], category = Rel, comments = "CONTAINS WITH VERTICAL BAR AT END OF HORIZONTAL STROKE"}
  , Record {uchar = '\8956', commands = [("base",""),("unicode","\\nis")], category = Rel, comments = "SMALL CONTAINS WITH VERTICAL BAR AT END OF HORIZONTAL STROKE"}
  , Record {uchar = '\8957', commands = [("base",""),("unicode","\\varniobar")], category = Rel, comments = "CONTAINS WITH OVERBAR"}
  , Record {uchar = '\8958', commands = [("base",""),("unicode","\\niobar")], category = Rel, comments = "SMALL CONTAINS WITH OVERBAR"}
  , Record {uchar = '\8959', commands = [("base",""),("base","\\mathsf{E}"),("unicode","\\bagmember")], category = Rel, comments = "Z NOTATION BAG MEMBERSHIP"}
  , Record {uchar = '\8960', commands = [("mathabx","\\diameter"),("amssymb","\\varnothing"),("unicode","\\diameter")], category = Ord, comments = "DIAMETER SIGN"}
  , Record {uchar = '\8962', commands = [("base",""),("unicode","\\house")], category = Ord, comments = "HOUSE"}
  , Record {uchar = '\8965', commands = [("base",""),("amssymb","\\barwedge"),("unicode","\\varbarwedge")], category = Bin, comments = "PROJECTIVE (bar over small wedge) not nand"}
  , Record {uchar = '\8966', commands = [("base",""),("amssymb","\\doublebarwedge"),("unicode","\\vardoublebarwedge")], category = Bin, comments = "PERSPECTIVE (double bar over small wedge)"}
  , Record {uchar = '\8968', commands = [("base","\\lceil"),("unicode","\\lceil")], category = Open, comments = "LEFT CEILING"}
  , Record {uchar = '\8969', commands = [("base","\\rceil"),("unicode","\\rceil")], category = Close, comments = "RIGHT CEILING"}
  , Record {uchar = '\8970', commands = [("base","\\lfloor"),("unicode","\\lfloor")], category = Open, comments = "LEFT FLOOR"}
  , Record {uchar = '\8971', commands = [("base","\\rfloor"),("unicode","\\rfloor")], category = Close, comments = "RIGHT FLOOR"}
  , Record {uchar = '\8976', commands = [("wasysym","\\invneg"),("unicode","\\invnot")], category = Ord, comments = "reverse not"}
  , Record {uchar = '\8977', commands = [("wasysym","\\wasylozenge"),("unicode","\\sqlozenge")], category = Ord, comments = "SQUARE LOZENGE"}
  , Record {uchar = '\8978', commands = [("base",""),("unicode","\\profline")], category = Ord, comments = "profile of a line"}
  , Record {uchar = '\8979', commands = [("base",""),("unicode","\\profsurf")], category = Ord, comments = "profile of a surface"}
  , Record {uchar = '\8983', commands = [("base",""),("unicode","\\viewdata")], category = Ord, comments = "VIEWDATA SQUARE"}
  , Record {uchar = '\8985', commands = [("base",""),("unicode","\\turnednot")], category = Ord, comments = "TURNED NOT SIGN"}
  , Record {uchar = '\8988', commands = [("amsfonts","\\ulcorner"),("unicode","\\ulcorner")], category = Open, comments = "upper left corner"}
  , Record {uchar = '\8989', commands = [("amsfonts","\\urcorner"),("unicode","\\urcorner")], category = Close, comments = "upper right corner"}
  , Record {uchar = '\8990', commands = [("amsfonts","\\llcorner"),("unicode","\\llcorner")], category = Open, comments = "lower left corner"}
  , Record {uchar = '\8991', commands = [("amsfonts","\\lrcorner"),("unicode","\\lrcorner")], category = Close, comments = "lower right corner"}
  , Record {uchar = '\8992', commands = [("base",""),("unicode","\\inttop")], category = Ord, comments = "TOP HALF INTEGRAL"}
  , Record {uchar = '\8993', commands = [("base",""),("unicode","\\intbottom")], category = Ord, comments = "BOTTOM HALF INTEGRAL"}
  , Record {uchar = '\8994', commands = [("base","\\frown"),("base","\\smallfrown"),("unicode","\\frown")], category = Rel, comments = "FROWN (down curve)"}
  , Record {uchar = '\8995', commands = [("base","\\smile"),("base","\\smallsmile"),("unicode","\\smile")], category = Rel, comments = "SMILE (up curve)"}
  , Record {uchar = '\9001', commands = [("base","\\langle"),("unicode","\\langle")], category = Open, comments = "Left angle bracket"}
  , Record {uchar = '\9002', commands = [("base","\\rangle"),("unicode","\\rangle")], category = Close, comments = "Right angle bracket"}
  , Record {uchar = '\9004', commands = [("base",""),("unicode","\\varhexagonlrbonds")], category = Ord, comments = "six carbon ring, corner down, double bonds lower right etc"}
  , Record {uchar = '\9010', commands = [("base",""),("unicode","\\conictaper")], category = Ord, comments = "CONICAL TAPER"}
  , Record {uchar = '\9014', commands = [("base",""),("unicode","\\topbot")], category = Ord, comments = "APL FUNCTIONAL SYMBOL I-BEAM, top and bottom"}
  , Record {uchar = '\9015', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL SQUISH QUAD"}
  , Record {uchar = '\9016', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD EQUAL"}
  , Record {uchar = '\9017', commands = [("wasysym","\\APLinv"),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD DIVIDE"}
  , Record {uchar = '\9018', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD DIAMOND"}
  , Record {uchar = '\9019', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD JOT"}
  , Record {uchar = '\9020', commands = [("base",""),("wasysym","\\APLcirc{\\APLbox}"),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD CIRCLE"}
  , Record {uchar = '\9021', commands = [("base",""),("wasysym","\\APLvert{\\Circle}"),("unicode","\\obar")], category = Bin, comments = "x \\obar (stmaryrd), APL FUNCTIONAL SYMBOL CIRCLE STILE, circle with vertical bar"}
  , Record {uchar = '\9022', commands = [("base",""),("wasysym","\\APLcirc{\\Circle}"),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL CIRCLE JOT"}
  , Record {uchar = '\9023', commands = [("wasysym","\\notslash"),("unicode","\\APLnotslash")], category = Rel, comments = "APL FUNCTIONAL SYMBOL SLASH BAR, solidus, bar through"}
  , Record {uchar = '\9024', commands = [("wasysym","\\notbackslash"),("unicode","\\APLnotbackslash")], category = Ord, comments = "APL FUNCTIONAL SYMBOL BACKSLASH BAR"}
  , Record {uchar = '\9025', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD SLASH"}
  , Record {uchar = '\9026', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD BACKSLASH"}
  , Record {uchar = '\9027', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD LESS-THAN"}
  , Record {uchar = '\9028', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD GREATER-THAN"}
  , Record {uchar = '\9029', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL LEFTWARDS VANE"}
  , Record {uchar = '\9030', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL RIGHTWARDS VANE"}
  , Record {uchar = '\9031', commands = [("wasysym","\\APLleftarrowbox"),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD LEFTWARDS ARROW"}
  , Record {uchar = '\9032', commands = [("wasysym","\\APLrightarrowbox"),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD RIGHTWARDS ARROW"}
  , Record {uchar = '\9033', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL CIRCLE BACKSLASH"}
  , Record {uchar = '\9034', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DOWN TACK UNDERBAR"}
  , Record {uchar = '\9035', commands = [("base",""),("wasysym","\\APLvert{\\APLup}"),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DELTA STILE"}
  , Record {uchar = '\9036', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD DOWN CARET"}
  , Record {uchar = '\9037', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD DELTA"}
  , Record {uchar = '\9038', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DOWN TACK JOT"}
  , Record {uchar = '\9039', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL UPWARDS VANE"}
  , Record {uchar = '\9040', commands = [("wasysym","\\APLuparrowbox"),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD UPWARDS ARROW"}
  , Record {uchar = '\9041', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL UP TACK OVERBAR"}
  , Record {uchar = '\9042', commands = [("wasysym",""),("wasysym","\\APLvert{\\APLdown}"),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DEL STILE"}
  , Record {uchar = '\9043', commands = [("base",""),("unicode","\\APLboxupcaret")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD UP CARET"}
  , Record {uchar = '\9044', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD DEL"}
  , Record {uchar = '\9045', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL UP TACK JOT"}
  , Record {uchar = '\9046', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DOWNWARDS VANE"}
  , Record {uchar = '\9047', commands = [("wasysym","\\APLdownarrowbox"),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD DOWNWARDS ARROW"}
  , Record {uchar = '\9048', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUOTE UNDERBAR"}
  , Record {uchar = '\9049', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DELTA UNDERBAR"}
  , Record {uchar = '\9050', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DIAMOND UNDERBAR"}
  , Record {uchar = '\9051', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL JOT UNDERBAR"}
  , Record {uchar = '\9052', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL CIRCLE UNDERBAR"}
  , Record {uchar = '\9053', commands = [("wasysym","\\APLcomment"),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL UP SHOE JOT"}
  , Record {uchar = '\9054', commands = [("wasysym","\\APLinput"),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUOTE QUAD"}
  , Record {uchar = '\9055', commands = [("wasysym","\\APLlog"),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL CIRCLE STAR"}
  , Record {uchar = '\9056', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD COLON"}
  , Record {uchar = '\9057', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL UP TACK DIAERESIS"}
  , Record {uchar = '\9058', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DEL DIAERESIS"}
  , Record {uchar = '\9059', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL STAR DIAERESIS"}
  , Record {uchar = '\9060', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL JOT DIAERESIS"}
  , Record {uchar = '\9061', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL CIRCLE DIAERESIS"}
  , Record {uchar = '\9062', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DOWN SHOE STILE"}
  , Record {uchar = '\9063', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL LEFT SHOE STILE"}
  , Record {uchar = '\9064', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL TILDE DIAERESIS"}
  , Record {uchar = '\9065', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL GREATER-THAN DIAERESIS"}
  , Record {uchar = '\9066', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL COMMA BAR"}
  , Record {uchar = '\9067', commands = [("base",""),("wasysym","\\APLnot{\\APLdown}"),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DEL TILDE"}
  , Record {uchar = '\9068', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL ZILDE"}
  , Record {uchar = '\9069', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL STILE TILDE"}
  , Record {uchar = '\9070', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL SEMICOLON UNDERBAR"}
  , Record {uchar = '\9071', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD NOT EQUAL"}
  , Record {uchar = '\9072', commands = [("base",""),("unicode","\\APLboxquestion")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD QUESTION"}
  , Record {uchar = '\9073', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DOWN CARET TILDE"}
  , Record {uchar = '\9074', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL UP CARET TILDE"}
  , Record {uchar = '\9075', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL IOTA"}
  , Record {uchar = '\9076', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL RHO"}
  , Record {uchar = '\9077', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL OMEGA"}
  , Record {uchar = '\9078', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL ALPHA UNDERBAR"}
  , Record {uchar = '\9079', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL EPSILON UNDERBAR"}
  , Record {uchar = '\9080', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL IOTA UNDERBAR"}
  , Record {uchar = '\9081', commands = [("base",""),("unicode","")], category = Ord, comments = "APL FUNCTIONAL SYMBOL OMEGA UNDERBAR"}
  , Record {uchar = '\9084', commands = [("base",""),("unicode","\\rangledownzigzagarrow")], category = Ord, comments = "RIGHT ANGLE WITH DOWNWARDS ZIGZAG ARROW"}
  , Record {uchar = '\9108', commands = [("base",""),("unicode","\\hexagon")], category = Ord, comments = "horizontal benzene ring [hexagon flat open]"}
  , Record {uchar = '\9115', commands = [("base",""),("unicode","\\lparenuend")], category = Ord, comments = "LEFT PARENTHESIS UPPER HOOK"}
  , Record {uchar = '\9116', commands = [("base",""),("unicode","\\lparenextender")], category = Ord, comments = "LEFT PARENTHESIS EXTENSION"}
  , Record {uchar = '\9117', commands = [("base",""),("unicode","\\lparenlend")], category = Ord, comments = "LEFT PARENTHESIS LOWER HOOK"}
  , Record {uchar = '\9118', commands = [("base",""),("unicode","\\rparenuend")], category = Ord, comments = "RIGHT PARENTHESIS UPPER HOOK"}
  , Record {uchar = '\9119', commands = [("base",""),("unicode","\\rparenextender")], category = Ord, comments = "RIGHT PARENTHESIS EXTENSION"}
  , Record {uchar = '\9120', commands = [("base",""),("unicode","\\rparenlend")], category = Ord, comments = "RIGHT PARENTHESIS LOWER HOOK"}
  , Record {uchar = '\9121', commands = [("base",""),("unicode","\\lbrackuend")], category = Ord, comments = "LEFT SQUARE BRACKET UPPER CORNER"}
  , Record {uchar = '\9122', commands = [("base",""),("unicode","\\lbrackextender")], category = Ord, comments = "LEFT SQUARE BRACKET EXTENSION"}
  , Record {uchar = '\9123', commands = [("base",""),("unicode","\\lbracklend")], category = Ord, comments = "LEFT SQUARE BRACKET LOWER CORNER"}
  , Record {uchar = '\9124', commands = [("base",""),("unicode","\\rbrackuend")], category = Ord, comments = "RIGHT SQUARE BRACKET UPPER CORNER"}
  , Record {uchar = '\9125', commands = [("base",""),("unicode","\\rbrackextender")], category = Ord, comments = "RIGHT SQUARE BRACKET EXTENSION"}
  , Record {uchar = '\9126', commands = [("base",""),("unicode","\\rbracklend")], category = Ord, comments = "RIGHT SQUARE BRACKET LOWER CORNER"}
  , Record {uchar = '\9127', commands = [("base",""),("unicode","\\lbraceuend")], category = Ord, comments = "LEFT CURLY BRACKET UPPER HOOK"}
  , Record {uchar = '\9128', commands = [("base",""),("unicode","\\lbracemid")], category = Ord, comments = "LEFT CURLY BRACKET MIDDLE PIECE"}
  , Record {uchar = '\9129', commands = [("base",""),("unicode","\\lbracelend")], category = Ord, comments = "LEFT CURLY BRACKET LOWER HOOK"}
  , Record {uchar = '\9130', commands = [("base",""),("unicode","\\vbraceextender")], category = Ord, comments = "CURLY BRACKET EXTENSION"}
  , Record {uchar = '\9131', commands = [("base",""),("unicode","\\rbraceuend")], category = Ord, comments = "RIGHT CURLY BRACKET UPPER HOOK"}
  , Record {uchar = '\9132', commands = [("base",""),("unicode","\\rbracemid")], category = Ord, comments = "RIGHT CURLY BRACKET MIDDLE PIECE"}
  , Record {uchar = '\9133', commands = [("base",""),("unicode","\\rbracelend")], category = Ord, comments = "RIGHT CURLY BRACKET LOWER HOOK"}
  , Record {uchar = '\9134', commands = [("base",""),("unicode","\\intextender")], category = Ord, comments = "INTEGRAL EXTENSION"}
  , Record {uchar = '\9135', commands = [("base",""),("unicode","\\harrowextender")], category = Ord, comments = "HORIZONTAL LINE EXTENSION (used to extend arrows)"}
  , Record {uchar = '\9136', commands = [("base",""),("unicode","\\lmoustache")], category = Ord, comments = "? \\lmoustache, UPPER LEFT OR LOWER RIGHT CURLY BRACKET SECTION"}
  , Record {uchar = '\9137', commands = [("base",""),("unicode","\\rmoustache")], category = Ord, comments = "? \\rmoustache, UPPER RIGHT OR LOWER LEFT CURLY BRACKET SECTION"}
  , Record {uchar = '\9138', commands = [("base",""),("unicode","\\sumtop")], category = Ord, comments = "SUMMATION TOP"}
  , Record {uchar = '\9139', commands = [("base",""),("unicode","\\sumbottom")], category = Ord, comments = "SUMMATION BOTTOM"}
  , Record {uchar = '\9140', commands = [("base",""),("unicode","\\overbracket")], category = TOver, comments = "TOP SQUARE BRACKET"}
  , Record {uchar = '\9141', commands = [("base",""),("unicode","\\underbracket")], category = TUnder, comments = "BOTTOM SQUARE BRACKET"}
  , Record {uchar = '\9142', commands = [("base",""),("unicode","\\bbrktbrk")], category = Ord, comments = "BOTTOM SQUARE BRACKET OVER TOP SQUARE BRACKET"}
  , Record {uchar = '\9143', commands = [("base",""),("unicode","\\sqrtbottom")], category = Ord, comments = "RADICAL SYMBOL BOTTOM"}
  , Record {uchar = '\9144', commands = [("base",""),("unicode","\\lvboxline")], category = Ord, comments = "LEFT VERTICAL BOX LINE"}
  , Record {uchar = '\9145', commands = [("base",""),("unicode","\\rvboxline")], category = Ord, comments = "RIGHT VERTICAL BOX LINE"}
  , Record {uchar = '\9166', commands = [("base",""),("unicode","\\varcarriagereturn")], category = Ord, comments = "RETURN SYMBOL"}
  , Record {uchar = '\9168', commands = [("base",""),("unicode","")], category = Ord, comments = "VERTICAL LINE EXTENSION (VERTICAL LINE EXTENSION)"}
  , Record {uchar = '\9180', commands = [("wrisym","\\overparen"),("yhmath mathabx fourier","\\wideparen"),("unicode","\\overparen")], category = TOver, comments = "TOP PARENTHESIS (mathematical use)"}
  , Record {uchar = '\9181', commands = [("wrisym","\\underparen"),("unicode","\\underparen")], category = TUnder, comments = "BOTTOM PARENTHESIS (mathematical use)"}
  , Record {uchar = '\9182', commands = [("base","\\overbrace"),("unicode","\\overbrace")], category = TOver, comments = "TOP CURLY BRACKET (mathematical use)"}
  , Record {uchar = '\9183', commands = [("base","\\underbrace"),("unicode","\\underbrace")], category = TUnder, comments = "BOTTOM CURLY BRACKET (mathematical use)"}
  , Record {uchar = '\9184', commands = [("base",""),("unicode","\\obrbrak")], category = Ord, comments = "TOP TORTOISE SHELL BRACKET (mathematical use)"}
  , Record {uchar = '\9185', commands = [("base",""),("unicode","\\ubrbrak")], category = Ord, comments = "BOTTOM TORTOISE SHELL BRACKET (mathematical use)"}
  , Record {uchar = '\9186', commands = [("base",""),("unicode","\\trapezium")], category = Ord, comments = "WHITE TRAPEZIUM"}
  , Record {uchar = '\9187', commands = [("base",""),("unicode","\\benzenr")], category = Ord, comments = "BENZENE RING WITH CIRCLE"}
  , Record {uchar = '\9188', commands = [("base",""),("unicode","\\strns")], category = Ord, comments = "STRAIGHTNESS"}
  , Record {uchar = '\9189', commands = [("base",""),("unicode","\\fltns")], category = Ord, comments = "FLATNESS"}
  , Record {uchar = '\9190', commands = [("base",""),("wasysym","\\AC"),("unicode","\\accurrent")], category = Ord, comments = "AC CURRENT"}
  , Record {uchar = '\9191', commands = [("base",""),("unicode","\\elinters")], category = Ord, comments = "ELECTRICAL INTERSECTION"}
  , Record {uchar = '\9416', commands = [("base",""),("unicode","")], category = Ord, comments = "oS capital S in circle"}
  , Record {uchar = '\9478', commands = [("base",""),("unicode","\\bdtriplevdash")], category = Ord, comments = "doubly broken vert"}
  , Record {uchar = '\9600', commands = [("base",""),("unicode","\\blockuphalf")], category = Ord, comments = "UPPER HALF BLOCK"}
  , Record {uchar = '\9604', commands = [("base",""),("unicode","\\blocklowhalf")], category = Ord, comments = "LOWER HALF BLOCK"}
  , Record {uchar = '\9608', commands = [("base",""),("unicode","\\blockfull")], category = Ord, comments = "FULL BLOCK"}
  , Record {uchar = '\9612', commands = [("base",""),("unicode","\\blocklefthalf")], category = Ord, comments = "LEFT HALF BLOCK"}
  , Record {uchar = '\9616', commands = [("base",""),("unicode","\\blockrighthalf")], category = Ord, comments = "RIGHT HALF BLOCK"}
  , Record {uchar = '\9617', commands = [("base",""),("unicode","\\blockqtrshaded")], category = Ord, comments = "25\\% shaded block"}
  , Record {uchar = '\9618', commands = [("base",""),("unicode","\\blockhalfshaded")], category = Ord, comments = "50\\% shaded block"}
  , Record {uchar = '\9619', commands = [("base",""),("unicode","\\blockthreeqtrshaded")], category = Ord, comments = "75\\% shaded block"}
  , Record {uchar = '\9632', commands = [("base","\\blacksquare"),("unicode","\\mdlgblksquare")], category = Ord, comments = "square, filled"}
  , Record {uchar = '\9633', commands = [("base","\\square"),("unicode","\\mdlgwhtsquare")], category = Ord, comments = "square, open"}
  , Record {uchar = '\9634', commands = [("base",""),("unicode","\\squoval")], category = Ord, comments = "WHITE SQUARE WITH ROUNDED CORNERS"}
  , Record {uchar = '\9635', commands = [("base",""),("unicode","\\blackinwhitesquare")], category = Ord, comments = "WHITE SQUARE CONTAINING BLACK SMALL SQUARE"}
  , Record {uchar = '\9636', commands = [("base",""),("unicode","\\squarehfill")], category = Ord, comments = "square, horizontal rule filled"}
  , Record {uchar = '\9637', commands = [("base",""),("unicode","\\squarevfill")], category = Ord, comments = "square, vertical rule filled"}
  , Record {uchar = '\9638', commands = [("base",""),("unicode","\\squarehvfill")], category = Ord, comments = "SQUARE WITH ORTHOGONAL CROSSHATCH FILL"}
  , Record {uchar = '\9639', commands = [("base",""),("unicode","\\squarenwsefill")], category = Ord, comments = "square, nw-to-se rule filled"}
  , Record {uchar = '\9640', commands = [("base",""),("unicode","\\squareneswfill")], category = Ord, comments = "square, ne-to-sw rule filled"}
  , Record {uchar = '\9641', commands = [("base",""),("unicode","\\squarecrossfill")], category = Ord, comments = "SQUARE WITH DIAGONAL CROSSHATCH FILL"}
  , Record {uchar = '\9642', commands = [("base",""),("unicode","\\smblksquare")], category = Ord, comments = "sq bullet, filled"}
  , Record {uchar = '\9643', commands = [("base",""),("unicode","\\smwhtsquare")], category = Ord, comments = "WHITE SMALL SQUARE"}
  , Record {uchar = '\9644', commands = [("base",""),("unicode","\\hrectangleblack")], category = Ord, comments = "BLACK RECTANGLE"}
  , Record {uchar = '\9645', commands = [("base",""),("unicode","\\hrectangle")], category = Ord, comments = "horizontal rectangle, open"}
  , Record {uchar = '\9646', commands = [("base",""),("unicode","\\vrectangleblack")], category = Ord, comments = "BLACK VERTICAL RECTANGLE"}
  , Record {uchar = '\9647', commands = [("base",""),("unicode","\\vrectangle")], category = Ord, comments = "rectangle, white (vertical)"}
  , Record {uchar = '\9648', commands = [("base",""),("unicode","\\parallelogramblack")], category = Ord, comments = "BLACK PARALLELOGRAM"}
  , Record {uchar = '\9649', commands = [("base",""),("unicode","\\parallelogram")], category = Ord, comments = "parallelogram, open"}
  , Record {uchar = '\9650', commands = [("base",""),("unicode","\\bigblacktriangleup")], category = Ord, comments = "BLACK UP-POINTING TRIANGLE"}
  , Record {uchar = '\9651', commands = [("base","\\bigtriangleup"),("amsfonts","\\triangle"),("unicode","\\bigtriangleup")], category = Bin, comments = "# \\vartriangle (amssymb), big up triangle, open"}
  , Record {uchar = '\9652', commands = [("mathabx","\\blacktriangleup"),("unicode","\\blacktriangle")], category = Bin, comments = "up triangle, filled"}
  , Record {uchar = '\9653', commands = [("mathabx","\\smalltriangleup"),("amssymb","\\vartriangle"),("unicode","\\vartriangle")], category = Bin, comments = "small up triangle, open"}
  , Record {uchar = '\9654', commands = [("wasysym","\\RHD"),("fourier -mathabx","\\blacktriangleright"),("unicode","\\blacktriangleright")], category = Bin, comments = "(large) right triangle, filled"}
  , Record {uchar = '\9655', commands = [("amssymb","\\rhd"),("wasysym","\\rhd"),("oz","\\rres"),("unicode","\\triangleright")], category = Bin, comments = "= \\RightTriangle (wrisym), (large) right triangle, open; z notation range restriction"}
  , Record {uchar = '\9656', commands = [("mathabx","\\blacktriangleright"),("unicode","\\smallblacktriangleright")], category = Bin, comments = "right triangle, filled"}
  , Record {uchar = '\9657', commands = [("mathabx","\\smalltriangleright"),("base","\\triangleright"),("unicode","\\smalltriangleright")], category = Bin, comments = "x \\triangleright (mathabx), right triangle, open"}
  , Record {uchar = '\9658', commands = [("base",""),("unicode","\\blackpointerright")], category = Ord, comments = "BLACK RIGHT-POINTING POINTER"}
  , Record {uchar = '\9659', commands = [("base",""),("mathabx","\\triangleright"),("unicode","\\whitepointerright")], category = Ord, comments = "WHITE RIGHT-POINTING POINTER"}
  , Record {uchar = '\9660', commands = [("base",""),("unicode","\\bigblacktriangledown")], category = Ord, comments = "big down triangle, filled"}
  , Record {uchar = '\9661', commands = [("base","\\bigtriangledown"),("unicode","\\bigtriangledown")], category = Bin, comments = "big down triangle, open"}
  , Record {uchar = '\9662', commands = [("mathabx","\\blacktriangledown"),("unicode","\\blacktriangledown")], category = Bin, comments = "BLACK DOWN-POINTING SMALL TRIANGLE"}
  , Record {uchar = '\9663', commands = [("mathabx","\\smalltriangledown"),("amssymb","\\triangledown"),("unicode","\\triangledown")], category = Bin, comments = "WHITE DOWN-POINTING SMALL TRIANGLE"}
  , Record {uchar = '\9664', commands = [("wasysym","\\LHD"),("fourier -mathabx","\\blacktriangleleft"),("unicode","\\blacktriangleleft")], category = Bin, comments = "(large) left triangle, filled"}
  , Record {uchar = '\9665', commands = [("amssymb","\\lhd"),("wasysym","\\lhd"),("oz","\\dres"),("unicode","\\triangleleft")], category = Bin, comments = "= \\LeftTriangle (wrisym), (large) left triangle, open; z notation domain restriction"}
  , Record {uchar = '\9666', commands = [("mathabx","\\blacktriangleleft"),("unicode","\\smallblacktriangleleft")], category = Bin, comments = "left triangle, filled"}
  , Record {uchar = '\9667', commands = [("mathabx","\\smalltriangleleft"),("base","\\triangleleft"),("unicode","\\smalltriangleleft")], category = Bin, comments = "x \\triangleleft (mathabx), left triangle, open"}
  , Record {uchar = '\9668', commands = [("base",""),("unicode","\\blackpointerleft")], category = Ord, comments = "BLACK LEFT-POINTING POINTER"}
  , Record {uchar = '\9669', commands = [("base",""),("mathabx","\\triangleleft"),("unicode","\\whitepointerleft")], category = Ord, comments = "WHITE LEFT-POINTING POINTER"}
  , Record {uchar = '\9670', commands = [("txfonts","\\Diamondblack"),("unicode","\\mdlgblkdiamond")], category = Ord, comments = "BLACK DIAMOND"}
  , Record {uchar = '\9671', commands = [("amssymb","\\Diamond"),("unicode","\\mdlgwhtdiamond")], category = Ord, comments = "WHITE DIAMOND; diamond, open"}
  , Record {uchar = '\9672', commands = [("base",""),("unicode","\\blackinwhitediamond")], category = Ord, comments = "WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND"}
  , Record {uchar = '\9673', commands = [("base",""),("unicode","\\fisheye")], category = Ord, comments = "FISHEYE"}
  , Record {uchar = '\9674', commands = [("amssymb","\\lozenge"),("unicode","\\mdlgwhtlozenge")], category = Ord, comments = "LOZENGE or total mark"}
  , Record {uchar = '\9675', commands = [("wasysym","\\Circle"),("unicode","\\mdlgwhtcircle")], category = Bin, comments = "medium large circle"}
  , Record {uchar = '\9676', commands = [("base",""),("unicode","\\dottedcircle")], category = Ord, comments = "DOTTED CIRCLE"}
  , Record {uchar = '\9677', commands = [("base",""),("unicode","\\circlevertfill")], category = Ord, comments = "CIRCLE WITH VERTICAL FILL"}
  , Record {uchar = '\9678', commands = [("base",""),("amssymb","\\circledcirc"),("unicode","\\bullseye")], category = Ord, comments = "BULLSEYE"}
  , Record {uchar = '\9679', commands = [("wasysym","\\CIRCLE"),("unicode","\\mdlgblkcircle")], category = Ord, comments = "circle, filled"}
  , Record {uchar = '\9680', commands = [("wasysym","\\LEFTcircle"),("unicode","\\circlelefthalfblack")], category = Ord, comments = "circle, filled left half [harvey ball]"}
  , Record {uchar = '\9681', commands = [("wasysym","\\RIGHTcircle"),("unicode","\\circlerighthalfblack")], category = Ord, comments = "circle, filled right half"}
  , Record {uchar = '\9682', commands = [("base",""),("unicode","\\circlebottomhalfblack")], category = Ord, comments = "circle, filled bottom half"}
  , Record {uchar = '\9683', commands = [("base",""),("unicode","\\circletophalfblack")], category = Ord, comments = "circle, filled top half"}
  , Record {uchar = '\9684', commands = [("base",""),("unicode","\\circleurquadblack")], category = Ord, comments = "CIRCLE WITH UPPER RIGHT QUADRANT BLACK"}
  , Record {uchar = '\9685', commands = [("base",""),("unicode","\\blackcircleulquadwhite")], category = Ord, comments = "CIRCLE WITH ALL BUT UPPER LEFT QUADRANT BLACK"}
  , Record {uchar = '\9686', commands = [("wasysym","\\LEFTCIRCLE"),("unicode","\\blacklefthalfcircle")], category = Ord, comments = "LEFT HALF BLACK CIRCLE"}
  , Record {uchar = '\9687', commands = [("wasysym","\\RIGHTCIRCLE"),("unicode","\\blackrighthalfcircle")], category = Ord, comments = "RIGHT HALF BLACK CIRCLE"}
  , Record {uchar = '\9688', commands = [("base",""),("unicode","\\inversebullet")], category = Ord, comments = "INVERSE BULLET"}
  , Record {uchar = '\9689', commands = [("base",""),("unicode","\\inversewhitecircle")], category = Ord, comments = "INVERSE WHITE CIRCLE"}
  , Record {uchar = '\9690', commands = [("base",""),("unicode","\\invwhiteupperhalfcircle")], category = Ord, comments = "UPPER HALF INVERSE WHITE CIRCLE"}
  , Record {uchar = '\9691', commands = [("base",""),("unicode","\\invwhitelowerhalfcircle")], category = Ord, comments = "LOWER HALF INVERSE WHITE CIRCLE"}
  , Record {uchar = '\9692', commands = [("base",""),("unicode","\\ularc")], category = Ord, comments = "UPPER LEFT QUADRANT CIRCULAR ARC"}
  , Record {uchar = '\9693', commands = [("base",""),("unicode","\\urarc")], category = Ord, comments = "UPPER RIGHT QUADRANT CIRCULAR ARC"}
  , Record {uchar = '\9694', commands = [("base",""),("unicode","\\lrarc")], category = Ord, comments = "LOWER RIGHT QUADRANT CIRCULAR ARC"}
  , Record {uchar = '\9695', commands = [("base",""),("unicode","\\llarc")], category = Ord, comments = "LOWER LEFT QUADRANT CIRCULAR ARC"}
  , Record {uchar = '\9696', commands = [("base",""),("unicode","\\topsemicircle")], category = Ord, comments = "UPPER HALF CIRCLE"}
  , Record {uchar = '\9697', commands = [("base",""),("unicode","\\botsemicircle")], category = Ord, comments = "LOWER HALF CIRCLE"}
  , Record {uchar = '\9698', commands = [("base",""),("unicode","\\lrblacktriangle")], category = Ord, comments = "lower right triangle, filled"}
  , Record {uchar = '\9699', commands = [("base",""),("unicode","\\llblacktriangle")], category = Ord, comments = "lower left triangle, filled"}
  , Record {uchar = '\9700', commands = [("base",""),("unicode","\\ulblacktriangle")], category = Ord, comments = "upper left triangle, filled"}
  , Record {uchar = '\9701', commands = [("base",""),("unicode","\\urblacktriangle")], category = Ord, comments = "upper right triangle, filled"}
  , Record {uchar = '\9702', commands = [("base",""),("unicode","\\smwhtcircle")], category = Ord, comments = "WHITE BULLET"}
  , Record {uchar = '\9703', commands = [("base",""),("unicode","\\squareleftblack")], category = Ord, comments = "square, filled left half"}
  , Record {uchar = '\9704', commands = [("base",""),("unicode","\\squarerightblack")], category = Ord, comments = "square, filled right half"}
  , Record {uchar = '\9705', commands = [("base",""),("unicode","\\squareulblack")], category = Ord, comments = "square, filled top left corner"}
  , Record {uchar = '\9706', commands = [("base",""),("unicode","\\squarelrblack")], category = Ord, comments = "square, filled bottom right corner"}
  , Record {uchar = '\9707', commands = [("stmaryrd","\\boxbar"),("txfonts","\\boxbar"),("unicode","\\boxbar")], category = Bin, comments = "vertical bar in box"}
  , Record {uchar = '\9708', commands = [("base",""),("unicode","\\trianglecdot")], category = Ord, comments = "filled"}
  , Record {uchar = '\9734', commands = [("base",""),("unicode","\\bigwhitestar")], category = Ord, comments = "star, open"}
  , Record {uchar = '\9737', commands = [("mathabx","\\Sun"),("unicode","\\astrosun")], category = Ord, comments = "SUN"}
  , Record {uchar = '\9740', commands = [("wasysym",""),("unicode","")], category = Ord, comments = "CONJUNCTION"}
  , Record {uchar = '\9744', commands = [("wasysym","\\Square"),("unicode","")], category = Ord, comments = "BALLOT BOX"}
  , Record {uchar = '\9745', commands = [("wasysym","\\CheckedBox"),("unicode","")], category = Ord, comments = "BALLOT BOX WITH CHECK"}
  , Record {uchar = '\9746', commands = [("wasysym","\\XBox"),("unicode","")], category = Ord, comments = "BALLOT BOX WITH X"}
  , Record {uchar = '\9749', commands = [("arevmath","\\steaming"),("unicode","")], category = Ord, comments = "HOT BEVERAGE"}
  , Record {uchar = '\9758', commands = [("arevmath","\\pointright"),("unicode","")], category = Ord, comments = "WHITE RIGHT POINTING INDEX"}
  , Record {uchar = '\9760', commands = [("arevmath","\\skull"),("unicode","")], category = Ord, comments = "SKULL AND CROSSBONES"}
  , Record {uchar = '\9761', commands = [("base",""),("unicode","\\danger")], category = Ord, comments = "CAUTION SIGN, dangerous bend"}
  , Record {uchar = '\9762', commands = [("arevmath","\\radiation"),("unicode","")], category = Ord, comments = "RADIOACTIVE SIGN"}
  , Record {uchar = '\9763', commands = [("arevmath","\\biohazard"),("unicode","")], category = Ord, comments = "BIOHAZARD SIGN"}
  , Record {uchar = '\9775', commands = [("arevmath","\\yinyang"),("unicode","")], category = Ord, comments = "YIN YANG"}
  , Record {uchar = '\9785', commands = [("wasysym","\\frownie"),("arevmath","\\sadface"),("unicode","")], category = Ord, comments = "WHITE FROWNING FACE"}
  , Record {uchar = '\9786', commands = [("wasysym","\\smiley"),("arevmath","\\smileface"),("unicode","")], category = Ord, comments = "WHITE SMILING FACE"}
  , Record {uchar = '\9787', commands = [("wasysym","\\blacksmiley"),("arevmath","\\invsmileface"),("unicode","\\blacksmiley")], category = Ord, comments = "BLACK SMILING FACE"}
  , Record {uchar = '\9788', commands = [("wasysym","\\sun"),("unicode","\\sun")], category = Ord, comments = "WHITE SUN WITH RAYS"}
  , Record {uchar = '\9789', commands = [("wasysym","\\rightmoon"),("mathabx","\\rightmoon"),("unicode","\\rightmoon")], category = Ord, comments = "FIRST QUARTER MOON"}
  , Record {uchar = '\9790', commands = [("wasysym","\\leftmoon"),("mathabx","\\leftmoon"),("unicode","\\leftmoon")], category = Ord, comments = "LAST QUARTER MOON"}
  , Record {uchar = '\9791', commands = [("wasysym","\\mercury"),("mathabx","\\Mercury"),("unicode","")], category = Ord, comments = "MERCURY"}
  , Record {uchar = '\9792', commands = [("wasysym","\\female"),("mathabx","\\Venus"),("unicode","\\female")], category = Ord, comments = "= \\girl (mathabx), venus, female"}
  , Record {uchar = '\9793', commands = [("wasysym","\\earth"),("mathabx","\\varEarth"),("unicode","")], category = Ord, comments = "EARTH"}
  , Record {uchar = '\9794', commands = [("wasysym","\\male"),("mathabx","\\Mars"),("unicode","\\male")], category = Ord, comments = "= \\boy (mathabx), mars, male"}
  , Record {uchar = '\9795', commands = [("wasysym","\\jupiter"),("mathabx","\\Jupiter"),("unicode","")], category = Ord, comments = "JUPITER"}
  , Record {uchar = '\9796', commands = [("wasysym","\\saturn"),("mathabx","\\Saturn"),("unicode","")], category = Ord, comments = "SATURN"}
  , Record {uchar = '\9797', commands = [("wasysym","\\uranus"),("mathabx","\\Uranus"),("unicode","")], category = Ord, comments = "URANUS"}
  , Record {uchar = '\9798', commands = [("wasysym","\\neptune"),("mathabx","\\Neptune"),("unicode","")], category = Ord, comments = "NEPTUNE"}
  , Record {uchar = '\9799', commands = [("wasysym","\\pluto"),("mathabx","\\Pluto"),("unicode","")], category = Ord, comments = "PLUTO"}
  , Record {uchar = '\9800', commands = [("wasysym","\\aries"),("mathabx","\\Aries"),("unicode","")], category = Ord, comments = "ARIES"}
  , Record {uchar = '\9801', commands = [("wasysym","\\taurus"),("mathabx","\\Taurus"),("unicode","")], category = Ord, comments = "TAURUS"}
  , Record {uchar = '\9802', commands = [("wasysym","\\gemini"),("mathabx","\\Gemini"),("unicode","")], category = Ord, comments = "GEMINI"}
  , Record {uchar = '\9803', commands = [("wasysym","\\cancer"),("unicode","")], category = Ord, comments = "CANCER"}
  , Record {uchar = '\9804', commands = [("wasysym","\\leo"),("mathabx","\\Leo"),("unicode","")], category = Ord, comments = "LEO"}
  , Record {uchar = '\9805', commands = [("wasysym","\\virgo"),("unicode","")], category = Ord, comments = "VIRGO"}
  , Record {uchar = '\9806', commands = [("wasysym","\\libra"),("mathabx","\\Libra"),("unicode","")], category = Ord, comments = "LIBRA"}
  , Record {uchar = '\9807', commands = [("wasysym","\\scorpio"),("mathabx","\\Scorpio"),("unicode","")], category = Ord, comments = "SCORPIUS"}
  , Record {uchar = '\9808', commands = [("wasysym","\\sagittarius"),("unicode","")], category = Ord, comments = "SAGITTARIUS"}
  , Record {uchar = '\9809', commands = [("wasysym","\\capricornus"),("unicode","")], category = Ord, comments = "CAPRICORN"}
  , Record {uchar = '\9810', commands = [("wasysym","\\aquarius"),("unicode","")], category = Ord, comments = "AQUARIUS"}
  , Record {uchar = '\9811', commands = [("wasysym","\\pisces"),("unicode","")], category = Ord, comments = "PISCES"}
  , Record {uchar = '\9824', commands = [("base","\\spadesuit"),("unicode","\\spadesuit")], category = Ord, comments = "spades suit symbol"}
  , Record {uchar = '\9825', commands = [("base","\\heartsuit"),("unicode","\\heartsuit")], category = Ord, comments = "heart suit symbol"}
  , Record {uchar = '\9826', commands = [("base","\\diamondsuit"),("unicode","\\diamondsuit")], category = Ord, comments = "diamond suit symbol"}
  , Record {uchar = '\9827', commands = [("base","\\clubsuit"),("unicode","\\clubsuit")], category = Ord, comments = "club suit symbol"}
  , Record {uchar = '\9828', commands = [("txfonts","\\varspadesuit"),("arevmath","\\varspade"),("unicode","\\varspadesuit")], category = Ord, comments = "spade, white (card suit)"}
  , Record {uchar = '\9829', commands = [("txfonts","\\varheartsuit"),("arevmath","\\varheart"),("unicode","\\varheartsuit")], category = Ord, comments = "filled heart (card suit)"}
  , Record {uchar = '\9830', commands = [("txfonts","\\vardiamondsuit"),("arevmath","\\vardiamond"),("unicode","\\vardiamondsuit")], category = Ord, comments = "filled diamond (card suit)"}
  , Record {uchar = '\9831', commands = [("txfonts","\\varclubsuit"),("arevmath","\\varclub"),("unicode","\\varclubsuit")], category = Ord, comments = "club, white (card suit)"}
  , Record {uchar = '\9833', commands = [("arevmath","\\quarternote"),("wasysym","\\quarternote"),("unicode","\\quarternote")], category = Ord, comments = "music note (sung text sign)"}
  , Record {uchar = '\9834', commands = [("arevmath","\\eighthnote"),("unicode","\\eighthnote")], category = Ord, comments = "EIGHTH NOTE"}
  , Record {uchar = '\9835', commands = [("wasysym","\\twonotes"),("unicode","\\twonotes")], category = Ord, comments = "BEAMED EIGHTH NOTES"}
  , Record {uchar = '\9836', commands = [("arevmath","\\sixteenthnote"),("unicode","")], category = Ord, comments = "BEAMED SIXTEENTH NOTES"}
  , Record {uchar = '\9837', commands = [("base","\\flat"),("unicode","\\flat")], category = Ord, comments = "musical flat"}
  , Record {uchar = '\9838', commands = [("base","\\natural"),("unicode","\\natural")], category = Ord, comments = "music natural"}
  , Record {uchar = '\9839', commands = [("base","\\sharp"),("oz","\\#"),("unicode","\\sharp")], category = Ord, comments = "MUSIC SHARP SIGN, z notation infix bag count"}
  , Record {uchar = '\9851', commands = [("arevmath","\\recycle"),("unicode","")], category = Ord, comments = "BLACK UNIVERSAL RECYCLING SYMBOL"}
  , Record {uchar = '\9854', commands = [("base",""),("unicode","\\acidfree")], category = Ord, comments = "PERMANENT PAPER SIGN"}
  , Record {uchar = '\9856', commands = [("base",""),("unicode","\\dicei")], category = Ord, comments = "DIE FACE-1"}
  , Record {uchar = '\9857', commands = [("base",""),("unicode","\\diceii")], category = Ord, comments = "DIE FACE-2"}
  , Record {uchar = '\9858', commands = [("base",""),("unicode","\\diceiii")], category = Ord, comments = "DIE FACE-3"}
  , Record {uchar = '\9859', commands = [("base",""),("unicode","\\diceiv")], category = Ord, comments = "DIE FACE-4"}
  , Record {uchar = '\9860', commands = [("base",""),("unicode","\\dicev")], category = Ord, comments = "DIE FACE-5"}
  , Record {uchar = '\9861', commands = [("base",""),("unicode","\\dicevi")], category = Ord, comments = "DIE FACE-6"}
  , Record {uchar = '\9862', commands = [("base",""),("unicode","\\circledrightdot")], category = Ord, comments = "WHITE CIRCLE WITH DOT RIGHT"}
  , Record {uchar = '\9863', commands = [("base",""),("unicode","\\circledtwodots")], category = Ord, comments = "WHITE CIRCLE WITH TWO DOTS"}
  , Record {uchar = '\9864', commands = [("base",""),("unicode","\\blackcircledrightdot")], category = Ord, comments = "BLACK CIRCLE WITH WHITE DOT RIGHT"}
  , Record {uchar = '\9865', commands = [("base",""),("unicode","\\blackcircledtwodots")], category = Ord, comments = "BLACK CIRCLE WITH TWO WHITE DOTS"}
  , Record {uchar = '\9875', commands = [("arevmath","\\anchor"),("unicode","")], category = Ord, comments = "ANCHOR"}
  , Record {uchar = '\9876', commands = [("arevmath","\\swords"),("unicode","")], category = Ord, comments = "CROSSED SWORDS"}
  , Record {uchar = '\9888', commands = [("arevmath","\\warning"),("unicode","")], category = Ord, comments = "WARNING SIGN"}
  , Record {uchar = '\9893', commands = [("base",""),("unicode","\\Hermaphrodite")], category = Ord, comments = "MALE AND FEMALE SIGN"}
  , Record {uchar = '\9898', commands = [("txfonts","\\medcirc"),("unicode","\\mdwhtcircle")], category = Ord, comments = "MEDIUM WHITE CIRCLE"}
  , Record {uchar = '\9899', commands = [("txfonts","\\medbullet"),("unicode","\\mdblkcircle")], category = Ord, comments = "MEDIUM BLACK CIRCLE"}
  , Record {uchar = '\9900', commands = [("base",""),("unicode","\\mdsmwhtcircle")], category = Ord, comments = "MEDIUM SMALL WHITE CIRCLE"}
  , Record {uchar = '\9906', commands = [("base",""),("unicode","\\neuter")], category = Ord, comments = "NEUTER"}
  , Record {uchar = '\9998', commands = [("arevmath","\\pencil"),("unicode","")], category = Ord, comments = "LOWER RIGHT PENCIL"}
  , Record {uchar = '\10003', commands = [("amsfonts","\\checkmark"),("arevmath","\\ballotcheck"),("unicode","\\checkmark")], category = Ord, comments = "tick, CHECK MARK"}
  , Record {uchar = '\10007', commands = [("arevmath","\\ballotx"),("unicode","")], category = Ord, comments = "BALLOT X"}
  , Record {uchar = '\10016', commands = [("amsfonts","\\maltese"),("unicode","\\maltese")], category = Ord, comments = "MALTESE CROSS"}
  , Record {uchar = '\10026', commands = [("base",""),("unicode","\\circledstar")], category = Ord, comments = "CIRCLED WHITE STAR"}
  , Record {uchar = '\10038', commands = [("base",""),("unicode","\\varstar")], category = Ord, comments = "SIX POINTED BLACK STAR"}
  , Record {uchar = '\10045', commands = [("base",""),("unicode","\\dingasterisk")], category = Ord, comments = "HEAVY TEARDROP-SPOKED ASTERISK"}
  , Record {uchar = '\10098', commands = [("base",""),("unicode","\\lbrbrak")], category = Open, comments = "LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT"}
  , Record {uchar = '\10099', commands = [("base",""),("unicode","\\rbrbrak")], category = Close, comments = "LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT"}
  , Record {uchar = '\10139', commands = [("base",""),("unicode","\\draftingarrow")], category = Ord, comments = "right arrow with bold head (drafting)"}
  , Record {uchar = '\10146', commands = [("arevmath","\\arrowbullet"),("unicode","")], category = Ord, comments = "THREE-D TOP-LIGHTED RIGHTWARDS ARROWHEAD"}
  , Record {uchar = '\10176', commands = [("base",""),("unicode","\\threedangle")], category = Ord, comments = "THREE DIMENSIONAL ANGLE"}
  , Record {uchar = '\10177', commands = [("base",""),("unicode","\\whiteinwhitetriangle")], category = Ord, comments = "WHITE TRIANGLE CONTAINING SMALL WHITE TRIANGLE"}
  , Record {uchar = '\10178', commands = [("base","\\perp"),("unicode","\\perp")], category = Rel, comments = "PERPENDICULAR"}
  , Record {uchar = '\10179', commands = [("base",""),("unicode","\\subsetcirc")], category = Ord, comments = "OPEN SUBSET"}
  , Record {uchar = '\10180', commands = [("base",""),("unicode","\\supsetcirc")], category = Ord, comments = "OPEN SUPERSET"}
  , Record {uchar = '\10181', commands = [("stmaryrd","\\Lbag"),("txfonts","\\Lbag"),("stmaryrd -oz","\\lbag"),("unicode","\\lbag")], category = Open, comments = "LEFT S-SHAPED BAG DELIMITER"}
  , Record {uchar = '\10182', commands = [("stmaryrd","\\Rbag"),("txfonts","\\Rbag"),("stmaryrd -oz","\\rbag"),("unicode","\\rbag")], category = Close, comments = "RIGHT S-SHAPED BAG DELIMITER"}
  , Record {uchar = '\10183', commands = [("base",""),("unicode","\\veedot")], category = Bin, comments = "OR WITH DOT INSIDE"}
  , Record {uchar = '\10184', commands = [("base",""),("unicode","\\bsolhsub")], category = Rel, comments = "REVERSE SOLIDUS PRECEDING SUBSET"}
  , Record {uchar = '\10185', commands = [("base",""),("unicode","\\suphsol")], category = Rel, comments = "SUPERSET PRECEDING SOLIDUS"}
  , Record {uchar = '\10188', commands = [("base",""),("unicode","\\longdivision")], category = Open, comments = "LONG DIVISION"}
  , Record {uchar = '\10192', commands = [("txfonts","\\Diamonddot"),("unicode","\\diamondcdot")], category = Ord, comments = "WHITE DIAMOND WITH CENTRED DOT"}
  , Record {uchar = '\10193', commands = [("base",""),("unicode","\\wedgedot")], category = Bin, comments = "AND WITH DOT"}
  , Record {uchar = '\10194', commands = [("base",""),("unicode","\\upin")], category = Rel, comments = "ELEMENT OF OPENING UPWARDS"}
  , Record {uchar = '\10195', commands = [("base",""),("unicode","\\pullback")], category = Rel, comments = "LOWER RIGHT CORNER WITH DOT"}
  , Record {uchar = '\10196', commands = [("base",""),("unicode","\\pushout")], category = Rel, comments = "UPPER LEFT CORNER WITH DOT"}
  , Record {uchar = '\10197', commands = [("base",""),("unicode","\\leftouterjoin")], category = Op, comments = "LEFT OUTER JOIN"}
  , Record {uchar = '\10198', commands = [("base",""),("unicode","\\rightouterjoin")], category = Op, comments = "RIGHT OUTER JOIN"}
  , Record {uchar = '\10199', commands = [("base",""),("unicode","\\fullouterjoin")], category = Op, comments = "FULL OUTER JOIN"}
  , Record {uchar = '\10200', commands = [("base",""),("unicode","\\bigbot")], category = Op, comments = "LARGE UP TACK"}
  , Record {uchar = '\10201', commands = [("base",""),("unicode","\\bigtop")], category = Op, comments = "LARGE DOWN TACK"}
  , Record {uchar = '\10202', commands = [("base",""),("unicode","\\DashVDash")], category = Rel, comments = "LEFT AND RIGHT DOUBLE TURNSTILE"}
  , Record {uchar = '\10203', commands = [("base",""),("unicode","\\dashVdash")], category = Rel, comments = "LEFT AND RIGHT TACK"}
  , Record {uchar = '\10204', commands = [("txfonts","\\multimapinv"),("unicode","\\multimapinv")], category = Rel, comments = "LEFT MULTIMAP"}
  , Record {uchar = '\10205', commands = [("base",""),("unicode","\\vlongdash")], category = Rel, comments = "long left tack"}
  , Record {uchar = '\10206', commands = [("base",""),("unicode","\\longdashv")], category = Rel, comments = "long right tack"}
  , Record {uchar = '\10207', commands = [("base",""),("unicode","\\cirbot")], category = Rel, comments = "UP TACK WITH CIRCLE ABOVE"}
  , Record {uchar = '\10208', commands = [("base",""),("unicode","\\lozengeminus")], category = Bin, comments = "LOZENGE DIVIDED BY HORIZONTAL RULE"}
  , Record {uchar = '\10209', commands = [("base",""),("unicode","\\concavediamond")], category = Bin, comments = "WHITE CONCAVE-SIDED DIAMOND"}
  , Record {uchar = '\10210', commands = [("base",""),("unicode","\\concavediamondtickleft")], category = Bin, comments = "WHITE CONCAVE-SIDED DIAMOND WITH LEFTWARDS TICK"}
  , Record {uchar = '\10211', commands = [("base",""),("unicode","\\concavediamondtickright")], category = Bin, comments = "WHITE CONCAVE-SIDED DIAMOND WITH RIGHTWARDS TICK"}
  , Record {uchar = '\10212', commands = [("base",""),("unicode","\\whitesquaretickleft")], category = Bin, comments = "WHITE SQUARE WITH LEFTWARDS TICK"}
  , Record {uchar = '\10213', commands = [("base",""),("unicode","\\whitesquaretickright")], category = Bin, comments = "WHITE SQUARE WITH RIGHTWARDS TICK"}
  , Record {uchar = '\10214', commands = [("stmaryrd","\\llbracket"),("wrisym","\\llbracket"),("kpfonts","\\llbracket"),("fourier","\\llbracket"),("mathbbol","\\Lbrack"),("unicode","\\lBrack")], category = Open, comments = "= \\lbag (oz -stmaryrd), MATHEMATICAL LEFT WHITE SQUARE BRACKET"}
  , Record {uchar = '\10215', commands = [("stmaryrd","\\rrbracket"),("wrisym","\\rrbracket"),("kpfonts","\\rrbracket"),("fourier","\\rrbracket"),("mathbbol","\\Rbrack"),("unicode","\\rBrack")], category = Close, comments = "= \\rbag (oz -stmaryrd), MATHEMATICAL RIGHT WHITE SQUARE BRACKET"}
  , Record {uchar = '\10216', commands = [("base","\\langle"),("unicode","\\langle")], category = Open, comments = "MATHEMATICAL LEFT ANGLE BRACKET"}
  , Record {uchar = '\10217', commands = [("base","\\rangle"),("unicode","\\rangle")], category = Close, comments = "MATHEMATICAL RIGHT ANGLE BRACKET"}
  , Record {uchar = '\10218', commands = [("oz","\\lang"),("unicode","\\lAngle")], category = Open, comments = "MATHEMATICAL LEFT DOUBLE ANGLE BRACKET, z notation left chevron bracket"}
  , Record {uchar = '\10219', commands = [("oz","\\rang"),("unicode","\\rAngle")], category = Close, comments = "MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET, z notation right chevron bracket"}
  , Record {uchar = '\10220', commands = [("base",""),("unicode","\\Lbrbrak")], category = Open, comments = "MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET"}
  , Record {uchar = '\10221', commands = [("base",""),("unicode","\\Rbrbrak")], category = Close, comments = "MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET"}
  , Record {uchar = '\10222', commands = [("base","\\lgroup"),("unicode","")], category = Open, comments = "MATHEMATICAL LEFT FLATTENED PARENTHESIS"}
  , Record {uchar = '\10223', commands = [("base","\\rgroup"),("unicode","")], category = Close, comments = "MATHEMATICAL RIGHT FLATTENED PARENTHESIS"}
  , Record {uchar = '\10224', commands = [("base",""),("unicode","\\UUparrow")], category = Rel, comments = "UPWARDS QUADRUPLE ARROW"}
  , Record {uchar = '\10225', commands = [("base",""),("unicode","\\DDownarrow")], category = Rel, comments = "DOWNWARDS QUADRUPLE ARROW"}
  , Record {uchar = '\10226', commands = [("base",""),("unicode","\\acwgapcirclearrow")], category = Rel, comments = "ANTICLOCKWISE GAPPED CIRCLE ARROW"}
  , Record {uchar = '\10227', commands = [("base",""),("unicode","\\cwgapcirclearrow")], category = Rel, comments = "CLOCKWISE GAPPED CIRCLE ARROW"}
  , Record {uchar = '\10228', commands = [("base",""),("unicode","\\rightarrowonoplus")], category = Rel, comments = "RIGHT ARROW WITH CIRCLED PLUS"}
  , Record {uchar = '\10229', commands = [("base","\\longleftarrow"),("unicode","\\longleftarrow")], category = Rel, comments = "LONG LEFTWARDS ARROW"}
  , Record {uchar = '\10230', commands = [("base","\\longrightarrow"),("unicode","\\longrightarrow")], category = Rel, comments = "LONG RIGHTWARDS ARROW"}
  , Record {uchar = '\10231', commands = [("base","\\longleftrightarrow"),("unicode","\\longleftrightarrow")], category = Rel, comments = "LONG LEFT RIGHT ARROW"}
  , Record {uchar = '\10232', commands = [("base","\\Longleftarrow"),("amsmath","\\impliedby"),("unicode","\\Longleftarrow")], category = Rel, comments = "LONG LEFTWARDS DOUBLE ARROW"}
  , Record {uchar = '\10233', commands = [("base","\\Longrightarrow"),("amsmath","\\implies"),("unicode","\\Longrightarrow")], category = Rel, comments = "LONG RIGHTWARDS DOUBLE ARROW"}
  , Record {uchar = '\10234', commands = [("base","\\Longleftrightarrow"),("oz","\\iff"),("unicode","\\Longleftrightarrow")], category = Rel, comments = "LONG LEFT RIGHT DOUBLE ARROW"}
  , Record {uchar = '\10235', commands = [("stmaryrd","\\longmapsfrom"),("kpfonts","\\longmappedfrom"),("unicode","\\longmapsfrom")], category = Rel, comments = "LONG LEFTWARDS ARROW FROM BAR"}
  , Record {uchar = '\10236', commands = [("base","\\longmapsto"),("unicode","\\longmapsto")], category = Rel, comments = "LONG RIGHTWARDS ARROW FROM BAR"}
  , Record {uchar = '\10237', commands = [("stmaryrd","\\Longmapsfrom"),("kpfonts","\\Longmappedfrom"),("unicode","\\Longmapsfrom")], category = Rel, comments = "LONG LEFTWARDS DOUBLE ARROW FROM BAR"}
  , Record {uchar = '\10238', commands = [("stmaryrd","\\Longmapsto"),("unicode","\\Longmapsto")], category = Rel, comments = "LONG RIGHTWARDS DOUBLE ARROW FROM BAR"}
  , Record {uchar = '\10239', commands = [("base",""),("unicode","\\longrightsquigarrow")], category = Rel, comments = "LONG RIGHTWARDS SQUIGGLE ARROW"}
  , Record {uchar = '\10496', commands = [("oz","\\psur"),("oz","\\psurj"),("unicode","\\nvtwoheadrightarrow")], category = Rel, comments = "RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE, z notation partial surjection"}
  , Record {uchar = '\10497', commands = [("base",""),("unicode","\\nVtwoheadrightarrow")], category = Rel, comments = "RIGHTWARDS TWO-HEADED ARROW WITH DOUBLE VERTICAL STROKE, z notation finite surjection"}
  , Record {uchar = '\10498', commands = [("base",""),("unicode","\\nvLeftarrow")], category = Rel, comments = "LEFTWARDS DOUBLE ARROW WITH VERTICAL STROKE"}
  , Record {uchar = '\10499', commands = [("base",""),("unicode","\\nvRightarrow")], category = Rel, comments = "RIGHTWARDS DOUBLE ARROW WITH VERTICAL STROKE"}
  , Record {uchar = '\10500', commands = [("base",""),("unicode","\\nvLeftrightarrow")], category = Rel, comments = "LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE"}
  , Record {uchar = '\10501', commands = [("base",""),("unicode","\\twoheadmapsto")], category = Rel, comments = "RIGHTWARDS TWO-HEADED ARROW FROM BAR"}
  , Record {uchar = '\10502', commands = [("stmaryrd","\\Mapsfrom"),("kpfonts","\\Mappedfrom"),("unicode","\\Mapsfrom")], category = Rel, comments = "LEFTWARDS DOUBLE ARROW FROM BAR"}
  , Record {uchar = '\10503', commands = [("stmaryrd","\\Mapsto"),("unicode","\\Mapsto")], category = Rel, comments = "RIGHTWARDS DOUBLE ARROW FROM BAR"}
  , Record {uchar = '\10504', commands = [("base",""),("unicode","\\downarrowbarred")], category = Rel, comments = "DOWNWARDS ARROW WITH HORIZONTAL STROKE"}
  , Record {uchar = '\10505', commands = [("base",""),("unicode","\\uparrowbarred")], category = Rel, comments = "UPWARDS ARROW WITH HORIZONTAL STROKE"}
  , Record {uchar = '\10506', commands = [("base",""),("unicode","\\Uuparrow")], category = Rel, comments = "UPWARDS TRIPLE ARROW"}
  , Record {uchar = '\10507', commands = [("base",""),("unicode","\\Ddownarrow")], category = Rel, comments = "DOWNWARDS TRIPLE ARROW"}
  , Record {uchar = '\10508', commands = [("base",""),("unicode","\\leftbkarrow")], category = Rel, comments = "LEFTWARDS DOUBLE DASH ARROW"}
  , Record {uchar = '\10509', commands = [("base",""),("unicode","\\rightbkarrow")], category = Rel, comments = "RIGHTWARDS DOUBLE DASH ARROW"}
  , Record {uchar = '\10510', commands = [("base",""),("unicode","\\leftdbkarrow")], category = Rel, comments = "LEFTWARDS TRIPLE DASH ARROW"}
  , Record {uchar = '\10511', commands = [("base",""),("unicode","\\dbkarow")], category = Rel, comments = "RIGHTWARDS TRIPLE DASH ARROW"}
  , Record {uchar = '\10512', commands = [("base",""),("unicode","\\drbkarow")], category = Rel, comments = "RIGHTWARDS TWO-HEADED TRIPLE DASH ARROW"}
  , Record {uchar = '\10513', commands = [("base",""),("unicode","\\rightdotarrow")], category = Rel, comments = "RIGHTWARDS ARROW WITH DOTTED STEM"}
  , Record {uchar = '\10514', commands = [("wrisym","\\UpArrowBar"),("unicode","\\baruparrow")], category = Rel, comments = "UPWARDS ARROW TO BAR"}
  , Record {uchar = '\10515', commands = [("wrisym","\\DownArrowBar"),("unicode","\\downarrowbar")], category = Rel, comments = "DOWNWARDS ARROW TO BAR"}
  , Record {uchar = '\10516', commands = [("oz","\\pinj"),("unicode","\\nvrightarrowtail")], category = Rel, comments = "RIGHTWARDS ARROW WITH TAIL WITH VERTICAL STROKE, z notation partial injection"}
  , Record {uchar = '\10517', commands = [("oz","\\finj"),("unicode","\\nVrightarrowtail")], category = Rel, comments = "RIGHTWARDS ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE, z notation finite injection"}
  , Record {uchar = '\10518', commands = [("oz","\\bij"),("unicode","\\twoheadrightarrowtail")], category = Rel, comments = "RIGHTWARDS TWO-HEADED ARROW WITH TAIL, z notation bijection"}
  , Record {uchar = '\10519', commands = [("base",""),("unicode","\\nvtwoheadrightarrowtail")], category = Rel, comments = "RIGHTWARDS TWO-HEADED ARROW WITH TAIL WITH VERTICAL STROKE, z notation surjective injection"}
  , Record {uchar = '\10520', commands = [("base",""),("unicode","\\nVtwoheadrightarrowtail")], category = Rel, comments = "RIGHTWARDS TWO-HEADED ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE, z notation finite surjective injection"}
  , Record {uchar = '\10521', commands = [("base",""),("unicode","\\lefttail")], category = Rel, comments = "LEFTWARDS ARROW-TAIL"}
  , Record {uchar = '\10522', commands = [("base",""),("unicode","\\righttail")], category = Rel, comments = "RIGHTWARDS ARROW-TAIL"}
  , Record {uchar = '\10523', commands = [("base",""),("unicode","\\leftdbltail")], category = Rel, comments = "LEFTWARDS DOUBLE ARROW-TAIL"}
  , Record {uchar = '\10524', commands = [("base",""),("unicode","\\rightdbltail")], category = Rel, comments = "RIGHTWARDS DOUBLE ARROW-TAIL"}
  , Record {uchar = '\10525', commands = [("base",""),("unicode","\\diamondleftarrow")], category = Rel, comments = "LEFTWARDS ARROW TO BLACK DIAMOND"}
  , Record {uchar = '\10526', commands = [("base",""),("unicode","\\rightarrowdiamond")], category = Rel, comments = "RIGHTWARDS ARROW TO BLACK DIAMOND"}
  , Record {uchar = '\10527', commands = [("base",""),("unicode","\\diamondleftarrowbar")], category = Rel, comments = "LEFTWARDS ARROW FROM BAR TO BLACK DIAMOND"}
  , Record {uchar = '\10528', commands = [("base",""),("unicode","\\barrightarrowdiamond")], category = Rel, comments = "RIGHTWARDS ARROW FROM BAR TO BLACK DIAMOND"}
  , Record {uchar = '\10529', commands = [("base",""),("unicode","\\nwsearrow")], category = Rel, comments = "NORTH WEST AND SOUTH EAST ARROW"}
  , Record {uchar = '\10530', commands = [("base",""),("unicode","\\neswarrow")], category = Rel, comments = "NORTH EAST AND SOUTH WEST ARROW"}
  , Record {uchar = '\10531', commands = [("base",""),("unicode","\\hknwarrow")], category = Rel, comments = "NORTH WEST ARROW WITH HOOK"}
  , Record {uchar = '\10532', commands = [("base",""),("unicode","\\hknearrow")], category = Rel, comments = "NORTH EAST ARROW WITH HOOK"}
  , Record {uchar = '\10533', commands = [("base",""),("unicode","\\hksearow")], category = Rel, comments = "SOUTH EAST ARROW WITH HOOK"}
  , Record {uchar = '\10534', commands = [("base",""),("unicode","\\hkswarow")], category = Rel, comments = "SOUTH WEST ARROW WITH HOOK"}
  , Record {uchar = '\10535', commands = [("base",""),("unicode","\\tona")], category = Rel, comments = "NORTH WEST ARROW AND NORTH EAST ARROW"}
  , Record {uchar = '\10536', commands = [("base",""),("unicode","\\toea")], category = Rel, comments = "NORTH EAST ARROW AND SOUTH EAST ARROW"}
  , Record {uchar = '\10537', commands = [("base",""),("unicode","\\tosa")], category = Rel, comments = "SOUTH EAST ARROW AND SOUTH WEST ARROW"}
  , Record {uchar = '\10538', commands = [("base",""),("unicode","\\towa")], category = Rel, comments = "SOUTH WEST ARROW AND NORTH WEST ARROW"}
  , Record {uchar = '\10539', commands = [("base",""),("unicode","\\rdiagovfdiag")], category = Ord, comments = "RISING DIAGONAL CROSSING FALLING DIAGONAL"}
  , Record {uchar = '\10540', commands = [("base",""),("unicode","\\fdiagovrdiag")], category = Ord, comments = "FALLING DIAGONAL CROSSING RISING DIAGONAL"}
  , Record {uchar = '\10541', commands = [("base",""),("unicode","\\seovnearrow")], category = Ord, comments = "SOUTH EAST ARROW CROSSING NORTH EAST ARROW"}
  , Record {uchar = '\10542', commands = [("base",""),("unicode","\\neovsearrow")], category = Ord, comments = "NORTH EAST ARROW CROSSING SOUTH EAST ARROW"}
  , Record {uchar = '\10543', commands = [("base",""),("unicode","\\fdiagovnearrow")], category = Ord, comments = "FALLING DIAGONAL CROSSING NORTH EAST ARROW"}
  , Record {uchar = '\10544', commands = [("base",""),("unicode","\\rdiagovsearrow")], category = Ord, comments = "RISING DIAGONAL CROSSING SOUTH EAST ARROW"}
  , Record {uchar = '\10545', commands = [("base",""),("unicode","\\neovnwarrow")], category = Ord, comments = "NORTH EAST ARROW CROSSING NORTH WEST ARROW"}
  , Record {uchar = '\10546', commands = [("base",""),("unicode","\\nwovnearrow")], category = Ord, comments = "NORTH WEST ARROW CROSSING NORTH EAST ARROW"}
  , Record {uchar = '\10547', commands = [("txfonts","\\leadsto"),("unicode","\\rightcurvedarrow")], category = Rel, comments = "WAVE ARROW POINTING DIRECTLY RIGHT"}
  , Record {uchar = '\10548', commands = [("base",""),("unicode","\\uprightcurvearrow")], category = Ord, comments = "ARROW POINTING RIGHTWARDS THEN CURVING UPWARDS"}
  , Record {uchar = '\10549', commands = [("base",""),("unicode","\\downrightcurvedarrow")], category = Ord, comments = "ARROW POINTING RIGHTWARDS THEN CURVING DOWNWARDS"}
  , Record {uchar = '\10550', commands = [("base",""),("unicode","\\leftdowncurvedarrow")], category = Rel, comments = "ARROW POINTING DOWNWARDS THEN CURVING LEFTWARDS"}
  , Record {uchar = '\10551', commands = [("base",""),("unicode","\\rightdowncurvedarrow")], category = Rel, comments = "ARROW POINTING DOWNWARDS THEN CURVING RIGHTWARDS"}
  , Record {uchar = '\10552', commands = [("base",""),("unicode","\\cwrightarcarrow")], category = Rel, comments = "RIGHT-SIDE ARC CLOCKWISE ARROW"}
  , Record {uchar = '\10553', commands = [("base",""),("unicode","\\acwleftarcarrow")], category = Rel, comments = "LEFT-SIDE ARC ANTICLOCKWISE ARROW"}
  , Record {uchar = '\10554', commands = [("base",""),("unicode","\\acwoverarcarrow")], category = Rel, comments = "TOP ARC ANTICLOCKWISE ARROW"}
  , Record {uchar = '\10555', commands = [("base",""),("unicode","\\acwunderarcarrow")], category = Rel, comments = "BOTTOM ARC ANTICLOCKWISE ARROW"}
  , Record {uchar = '\10556', commands = [("base",""),("unicode","\\curvearrowrightminus")], category = Rel, comments = "TOP ARC CLOCKWISE ARROW WITH MINUS"}
  , Record {uchar = '\10557', commands = [("base",""),("unicode","\\curvearrowleftplus")], category = Rel, comments = "TOP ARC ANTICLOCKWISE ARROW WITH PLUS"}
  , Record {uchar = '\10558', commands = [("base",""),("unicode","\\cwundercurvearrow")], category = Rel, comments = "LOWER RIGHT SEMICIRCULAR CLOCKWISE ARROW"}
  , Record {uchar = '\10559', commands = [("base",""),("unicode","\\ccwundercurvearrow")], category = Rel, comments = "LOWER LEFT SEMICIRCULAR ANTICLOCKWISE ARROW"}
  , Record {uchar = '\10560', commands = [("base",""),("unicode","\\acwcirclearrow")], category = Rel, comments = "ANTICLOCKWISE CLOSED CIRCLE ARROW"}
  , Record {uchar = '\10561', commands = [("base",""),("unicode","\\cwcirclearrow")], category = Rel, comments = "CLOCKWISE CLOSED CIRCLE ARROW"}
  , Record {uchar = '\10562', commands = [("base",""),("unicode","\\rightarrowshortleftarrow")], category = Rel, comments = "RIGHTWARDS ARROW ABOVE SHORT LEFTWARDS ARROW"}
  , Record {uchar = '\10563', commands = [("base",""),("unicode","\\leftarrowshortrightarrow")], category = Rel, comments = "LEFTWARDS ARROW ABOVE SHORT RIGHTWARDS ARROW"}
  , Record {uchar = '\10564', commands = [("base",""),("unicode","\\shortrightarrowleftarrow")], category = Rel, comments = "SHORT RIGHTWARDS ARROW ABOVE LEFTWARDS ARROW"}
  , Record {uchar = '\10565', commands = [("base",""),("unicode","\\rightarrowplus")], category = Rel, comments = "RIGHTWARDS ARROW WITH PLUS BELOW"}
  , Record {uchar = '\10566', commands = [("base",""),("unicode","\\leftarrowplus")], category = Rel, comments = "LEFTWARDS ARROW WITH PLUS BELOW"}
  , Record {uchar = '\10567', commands = [("base",""),("unicode","\\rightarrowx")], category = Rel, comments = "RIGHTWARDS ARROW THROUGH X"}
  , Record {uchar = '\10568', commands = [("base",""),("unicode","\\leftrightarrowcircle")], category = Rel, comments = "LEFT RIGHT ARROW THROUGH SMALL CIRCLE"}
  , Record {uchar = '\10569', commands = [("base",""),("unicode","\\twoheaduparrowcircle")], category = Rel, comments = "UPWARDS TWO-HEADED ARROW FROM SMALL CIRCLE"}
  , Record {uchar = '\10570', commands = [("mathabx","\\leftrightharpoon"),("unicode","\\leftrightharpoonupdown")], category = Rel, comments = "LEFT BARB UP RIGHT BARB DOWN HARPOON"}
  , Record {uchar = '\10571', commands = [("mathabx","\\rightleftharpoon"),("unicode","\\leftrightharpoondownup")], category = Rel, comments = "LEFT BARB DOWN RIGHT BARB UP HARPOON"}
  , Record {uchar = '\10572', commands = [("base",""),("unicode","\\updownharpoonrightleft")], category = Rel, comments = "UP BARB RIGHT DOWN BARB LEFT HARPOON"}
  , Record {uchar = '\10573', commands = [("base",""),("unicode","\\updownharpoonleftright")], category = Rel, comments = "UP BARB LEFT DOWN BARB RIGHT HARPOON"}
  , Record {uchar = '\10574', commands = [("wrisym","\\leftrightharpoonup"),("unicode","\\leftrightharpoonupup")], category = Rel, comments = "LEFT BARB UP RIGHT BARB UP HARPOON"}
  , Record {uchar = '\10575', commands = [("wrisym","\\rightupdownharpoon"),("unicode","\\updownharpoonrightright")], category = Rel, comments = "UP BARB RIGHT DOWN BARB RIGHT HARPOON"}
  , Record {uchar = '\10576', commands = [("wrisym","\\leftrightharpoondown"),("unicode","\\leftrightharpoondowndown")], category = Rel, comments = "LEFT BARB DOWN RIGHT BARB DOWN HARPOON"}
  , Record {uchar = '\10577', commands = [("wrisym","\\leftupdownharpoon"),("unicode","\\updownharpoonleftleft")], category = Rel, comments = "UP BARB LEFT DOWN BARB LEFT HARPOON"}
  , Record {uchar = '\10578', commands = [("wrisym","\\LeftVectorBar"),("unicode","\\barleftharpoonup")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB UP TO BAR"}
  , Record {uchar = '\10579', commands = [("wrisym","\\RightVectorBar"),("unicode","\\rightharpoonupbar")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB UP TO BAR"}
  , Record {uchar = '\10580', commands = [("wrisym","\\RightUpVectorBar"),("unicode","\\barupharpoonright")], category = Rel, comments = "UPWARDS HARPOON WITH BARB RIGHT TO BAR"}
  , Record {uchar = '\10581', commands = [("wrisym","\\RightDownVectorBar"),("unicode","\\downharpoonrightbar")], category = Rel, comments = "DOWNWARDS HARPOON WITH BARB RIGHT TO BAR"}
  , Record {uchar = '\10582', commands = [("wrisym","\\DownLeftVectorBar"),("unicode","\\barleftharpoondown")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB DOWN TO BAR"}
  , Record {uchar = '\10583', commands = [("wrisym","\\DownRightVectorBar"),("unicode","\\rightharpoondownbar")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB DOWN TO BAR"}
  , Record {uchar = '\10584', commands = [("wrisym","\\LeftUpVectorBar"),("unicode","\\barupharpoonleft")], category = Rel, comments = "UPWARDS HARPOON WITH BARB LEFT TO BAR"}
  , Record {uchar = '\10585', commands = [("wrisym","\\LeftDownVectorBar"),("unicode","\\downharpoonleftbar")], category = Rel, comments = "DOWNWARDS HARPOON WITH BARB LEFT TO BAR"}
  , Record {uchar = '\10586', commands = [("wrisym","\\LeftTeeVector"),("unicode","\\leftharpoonupbar")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB UP FROM BAR"}
  , Record {uchar = '\10587', commands = [("wrisym","\\RightTeeVector"),("unicode","\\barrightharpoonup")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB UP FROM BAR"}
  , Record {uchar = '\10588', commands = [("wrisym","\\RightUpTeeVector"),("unicode","\\upharpoonrightbar")], category = Rel, comments = "UPWARDS HARPOON WITH BARB RIGHT FROM BAR"}
  , Record {uchar = '\10589', commands = [("wrisym","\\RightDownTeeVector"),("unicode","\\bardownharpoonright")], category = Rel, comments = "DOWNWARDS HARPOON WITH BARB RIGHT FROM BAR"}
  , Record {uchar = '\10590', commands = [("wrisym","\\DownLeftTeeVector"),("unicode","\\leftharpoondownbar")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB DOWN FROM BAR"}
  , Record {uchar = '\10591', commands = [("wrisym","\\DownRightTeeVector"),("unicode","\\barrightharpoondown")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB DOWN FROM BAR"}
  , Record {uchar = '\10592', commands = [("wrisym","\\LeftUpTeeVector"),("unicode","\\upharpoonleftbar")], category = Rel, comments = "UPWARDS HARPOON WITH BARB LEFT FROM BAR"}
  , Record {uchar = '\10593', commands = [("wrisym","\\LeftDownTeeVector"),("unicode","\\bardownharpoonleft")], category = Rel, comments = "DOWNWARDS HARPOON WITH BARB LEFT FROM BAR"}
  , Record {uchar = '\10594', commands = [("mathabx","\\leftleftharpoons"),("unicode","\\leftharpoonsupdown")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB UP ABOVE LEFTWARDS HARPOON WITH BARB DOWN"}
  , Record {uchar = '\10595', commands = [("mathabx","\\upupharpoons"),("unicode","\\upharpoonsleftright")], category = Rel, comments = "UPWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT"}
  , Record {uchar = '\10596', commands = [("mathabx","\\rightrightharpoons"),("unicode","\\rightharpoonsupdown")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB UP ABOVE RIGHTWARDS HARPOON WITH BARB DOWN"}
  , Record {uchar = '\10597', commands = [("mathabx","\\downdownharpoons"),("unicode","\\downharpoonsleftright")], category = Rel, comments = "DOWNWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT"}
  , Record {uchar = '\10598', commands = [("base",""),("unicode","\\leftrightharpoonsup")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB UP ABOVE RIGHTWARDS HARPOON WITH BARB UP"}
  , Record {uchar = '\10599', commands = [("base",""),("unicode","\\leftrightharpoonsdown")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB DOWN ABOVE RIGHTWARDS HARPOON WITH BARB DOWN"}
  , Record {uchar = '\10600', commands = [("base",""),("unicode","\\rightleftharpoonsup")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB UP ABOVE LEFTWARDS HARPOON WITH BARB UP"}
  , Record {uchar = '\10601', commands = [("base",""),("unicode","\\rightleftharpoonsdown")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB DOWN ABOVE LEFTWARDS HARPOON WITH BARB DOWN"}
  , Record {uchar = '\10602', commands = [("mathabx","\\leftbarharpoon"),("unicode","\\leftharpoonupdash")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB UP ABOVE LONG DASH"}
  , Record {uchar = '\10603', commands = [("mathabx","\\barleftharpoon"),("unicode","\\dashleftharpoondown")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB DOWN BELOW LONG DASH"}
  , Record {uchar = '\10604', commands = [("mathabx","\\rightbarharpoon"),("unicode","\\rightharpoonupdash")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB UP ABOVE LONG DASH"}
  , Record {uchar = '\10605', commands = [("mathabx","\\barrightharpoon"),("unicode","\\dashrightharpoondown")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB DOWN BELOW LONG DASH"}
  , Record {uchar = '\10606', commands = [("mathabx","\\updownharpoons"),("wrisym","\\upequilibrium"),("unicode","\\updownharpoonsleftright")], category = Rel, comments = "UPWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT"}
  , Record {uchar = '\10607', commands = [("mathabx","\\downupharpoons"),("wrisym","\\uprevequilibrium"),("unicode","\\downupharpoonsleftright")], category = Rel, comments = "DOWNWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT"}
  , Record {uchar = '\10608', commands = [("base",""),("unicode","\\rightimply")], category = Rel, comments = "RIGHT DOUBLE ARROW WITH ROUNDED HEAD"}
  , Record {uchar = '\10609', commands = [("base",""),("unicode","\\equalrightarrow")], category = Rel, comments = "EQUALS SIGN ABOVE RIGHTWARDS ARROW"}
  , Record {uchar = '\10610', commands = [("base",""),("unicode","\\similarrightarrow")], category = Rel, comments = "TILDE OPERATOR ABOVE RIGHTWARDS ARROW"}
  , Record {uchar = '\10611', commands = [("base",""),("unicode","\\leftarrowsimilar")], category = Rel, comments = "LEFTWARDS ARROW ABOVE TILDE OPERATOR"}
  , Record {uchar = '\10612', commands = [("base",""),("unicode","\\rightarrowsimilar")], category = Rel, comments = "RIGHTWARDS ARROW ABOVE TILDE OPERATOR"}
  , Record {uchar = '\10613', commands = [("base",""),("unicode","\\rightarrowapprox")], category = Rel, comments = "RIGHTWARDS ARROW ABOVE ALMOST EQUAL TO"}
  , Record {uchar = '\10614', commands = [("base",""),("unicode","\\ltlarr")], category = Rel, comments = "LESS-THAN ABOVE LEFTWARDS ARROW"}
  , Record {uchar = '\10615', commands = [("base",""),("unicode","\\leftarrowless")], category = Rel, comments = "LEFTWARDS ARROW THROUGH LESS-THAN"}
  , Record {uchar = '\10616', commands = [("base",""),("unicode","\\gtrarr")], category = Rel, comments = "GREATER-THAN ABOVE RIGHTWARDS ARROW"}
  , Record {uchar = '\10617', commands = [("base",""),("unicode","\\subrarr")], category = Rel, comments = "SUBSET ABOVE RIGHTWARDS ARROW"}
  , Record {uchar = '\10618', commands = [("base",""),("unicode","\\leftarrowsubset")], category = Rel, comments = "LEFTWARDS ARROW THROUGH SUBSET"}
  , Record {uchar = '\10619', commands = [("base",""),("unicode","\\suplarr")], category = Rel, comments = "SUPERSET ABOVE LEFTWARDS ARROW"}
  , Record {uchar = '\10620', commands = [("txfonts","\\strictfi"),("unicode","\\leftfishtail")], category = Rel, comments = "LEFT FISH TAIL"}
  , Record {uchar = '\10621', commands = [("txfonts","\\strictif"),("unicode","\\rightfishtail")], category = Rel, comments = "RIGHT FISH TAIL"}
  , Record {uchar = '\10622', commands = [("base",""),("unicode","\\upfishtail")], category = Rel, comments = "UP FISH TAIL"}
  , Record {uchar = '\10623', commands = [("base",""),("unicode","\\downfishtail")], category = Rel, comments = "DOWN FISH TAIL"}
  , Record {uchar = '\10624', commands = [("fourier","\\VERT"),("unicode","\\Vvert")], category = Fence, comments = "TRIPLE VERTICAL BAR DELIMITER"}
  , Record {uchar = '\10625', commands = [("oz","\\spot"),("oz","\\dot"),("unicode","\\mdsmblkcircle")], category = Ord, comments = "Z NOTATION SPOT"}
  , Record {uchar = '\10626', commands = [("base",""),("unicode","\\typecolon")], category = Bin, comments = "Z NOTATION TYPE COLON, (present in bbold font but no command)"}
  , Record {uchar = '\10627', commands = [("base",""),("unicode","\\lBrace")], category = Open, comments = "LEFT WHITE CURLY BRACKET"}
  , Record {uchar = '\10628', commands = [("base",""),("unicode","\\rBrace")], category = Close, comments = "RIGHT WHITE CURLY BRACKET"}
  , Record {uchar = '\10629', commands = [("mathbbol","\\Lparen"),("unicode","\\lParen")], category = Open, comments = "LEFT WHITE PARENTHESIS"}
  , Record {uchar = '\10630', commands = [("mathbbol","\\Rparen"),("unicode","\\rParen")], category = Close, comments = "RIGHT WHITE PARENTHESIS"}
  , Record {uchar = '\10631', commands = [("oz","\\limg"),("stmaryrd","\\llparenthesis"),("unicode","\\llparenthesis")], category = Open, comments = "Z NOTATION LEFT IMAGE BRACKET"}
  , Record {uchar = '\10632', commands = [("oz","\\rimg"),("stmaryrd","\\rrparenthesis"),("unicode","\\rrparenthesis")], category = Close, comments = "Z NOTATION RIGHT IMAGE BRACKET"}
  , Record {uchar = '\10633', commands = [("oz","\\lblot"),("unicode","\\llangle")], category = Open, comments = "Z NOTATION LEFT BINDING BRACKET"}
  , Record {uchar = '\10634', commands = [("oz","\\rblot"),("unicode","\\rrangle")], category = Close, comments = "Z NOTATION RIGHT BINDING BRACKET"}
  , Record {uchar = '\10635', commands = [("base",""),("unicode","\\lbrackubar")], category = Open, comments = "LEFT SQUARE BRACKET WITH UNDERBAR"}
  , Record {uchar = '\10636', commands = [("base",""),("unicode","\\rbrackubar")], category = Close, comments = "RIGHT SQUARE BRACKET WITH UNDERBAR"}
  , Record {uchar = '\10637', commands = [("base",""),("unicode","\\lbrackultick")], category = Open, comments = "LEFT SQUARE BRACKET WITH TICK IN TOP CORNER"}
  , Record {uchar = '\10638', commands = [("base",""),("unicode","\\rbracklrtick")], category = Close, comments = "RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER"}
  , Record {uchar = '\10639', commands = [("base",""),("unicode","\\lbracklltick")], category = Open, comments = "LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER"}
  , Record {uchar = '\10640', commands = [("base",""),("unicode","\\rbrackurtick")], category = Close, comments = "RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER"}
  , Record {uchar = '\10641', commands = [("base",""),("unicode","\\langledot")], category = Open, comments = "LEFT ANGLE BRACKET WITH DOT"}
  , Record {uchar = '\10642', commands = [("base",""),("unicode","\\rangledot")], category = Close, comments = "RIGHT ANGLE BRACKET WITH DOT"}
  , Record {uchar = '\10643', commands = [("base",""),("unicode","\\lparenless")], category = Open, comments = "LEFT ARC LESS-THAN BRACKET"}
  , Record {uchar = '\10644', commands = [("base",""),("unicode","\\rparengtr")], category = Close, comments = "RIGHT ARC GREATER-THAN BRACKET"}
  , Record {uchar = '\10645', commands = [("base",""),("unicode","\\Lparengtr")], category = Open, comments = "DOUBLE LEFT ARC GREATER-THAN BRACKET"}
  , Record {uchar = '\10646', commands = [("base",""),("unicode","\\Rparenless")], category = Close, comments = "DOUBLE RIGHT ARC LESS-THAN BRACKET"}
  , Record {uchar = '\10647', commands = [("base",""),("unicode","\\lblkbrbrak")], category = Open, comments = "LEFT BLACK TORTOISE SHELL BRACKET"}
  , Record {uchar = '\10648', commands = [("base",""),("unicode","\\rblkbrbrak")], category = Close, comments = "RIGHT BLACK TORTOISE SHELL BRACKET"}
  , Record {uchar = '\10649', commands = [("base",""),("unicode","\\fourvdots")], category = Ord, comments = "DOTTED FENCE"}
  , Record {uchar = '\10650', commands = [("base",""),("unicode","\\vzigzag")], category = Ord, comments = "VERTICAL ZIGZAG LINE"}
  , Record {uchar = '\10651', commands = [("base",""),("unicode","\\measuredangleleft")], category = Ord, comments = "MEASURED ANGLE OPENING LEFT"}
  , Record {uchar = '\10652', commands = [("base",""),("unicode","\\rightanglesqr")], category = Ord, comments = "RIGHT ANGLE VARIANT WITH SQUARE"}
  , Record {uchar = '\10653', commands = [("base",""),("unicode","\\rightanglemdot")], category = Ord, comments = "MEASURED RIGHT ANGLE WITH DOT"}
  , Record {uchar = '\10654', commands = [("base",""),("unicode","\\angles")], category = Ord, comments = "ANGLE WITH S INSIDE"}
  , Record {uchar = '\10655', commands = [("base",""),("unicode","\\angdnr")], category = Ord, comments = "ACUTE ANGLE"}
  , Record {uchar = '\10656', commands = [("base",""),("unicode","\\gtlpar")], category = Ord, comments = "SPHERICAL ANGLE OPENING LEFT"}
  , Record {uchar = '\10657', commands = [("base",""),("unicode","\\sphericalangleup")], category = Ord, comments = "SPHERICAL ANGLE OPENING UP"}
  , Record {uchar = '\10658', commands = [("base",""),("unicode","\\turnangle")], category = Ord, comments = "TURNED ANGLE"}
  , Record {uchar = '\10659', commands = [("base",""),("unicode","\\revangle")], category = Ord, comments = "REVERSED ANGLE"}
  , Record {uchar = '\10660', commands = [("base",""),("unicode","\\angleubar")], category = Ord, comments = "ANGLE WITH UNDERBAR"}
  , Record {uchar = '\10661', commands = [("base",""),("unicode","\\revangleubar")], category = Ord, comments = "REVERSED ANGLE WITH UNDERBAR"}
  , Record {uchar = '\10662', commands = [("base",""),("unicode","\\wideangledown")], category = Ord, comments = "OBLIQUE ANGLE OPENING UP"}
  , Record {uchar = '\10663', commands = [("base",""),("unicode","\\wideangleup")], category = Ord, comments = "OBLIQUE ANGLE OPENING DOWN"}
  , Record {uchar = '\10664', commands = [("base",""),("unicode","\\measanglerutone")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING UP AND RIGHT"}
  , Record {uchar = '\10665', commands = [("base",""),("unicode","\\measanglelutonw")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING UP AND LEFT"}
  , Record {uchar = '\10666', commands = [("base",""),("unicode","\\measanglerdtose")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING DOWN AND RIGHT"}
  , Record {uchar = '\10667', commands = [("base",""),("unicode","\\measangleldtosw")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING DOWN AND LEFT"}
  , Record {uchar = '\10668', commands = [("base",""),("unicode","\\measangleurtone")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING RIGHT AND UP"}
  , Record {uchar = '\10669', commands = [("base",""),("unicode","\\measangleultonw")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING LEFT AND UP"}
  , Record {uchar = '\10670', commands = [("base",""),("unicode","\\measangledrtose")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING RIGHT AND DOWN"}
  , Record {uchar = '\10671', commands = [("base",""),("unicode","\\measangledltosw")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING LEFT AND DOWN"}
  , Record {uchar = '\10672', commands = [("base",""),("unicode","\\revemptyset")], category = Ord, comments = "REVERSED EMPTY SET"}
  , Record {uchar = '\10673', commands = [("base",""),("unicode","\\emptysetobar")], category = Ord, comments = "EMPTY SET WITH OVERBAR"}
  , Record {uchar = '\10674', commands = [("base",""),("unicode","\\emptysetocirc")], category = Ord, comments = "EMPTY SET WITH SMALL CIRCLE ABOVE"}
  , Record {uchar = '\10675', commands = [("base",""),("unicode","\\emptysetoarr")], category = Ord, comments = "EMPTY SET WITH RIGHT ARROW ABOVE"}
  , Record {uchar = '\10676', commands = [("base",""),("unicode","\\emptysetoarrl")], category = Ord, comments = "EMPTY SET WITH LEFT ARROW ABOVE"}
  , Record {uchar = '\10677', commands = [("base",""),("unicode","\\circlehbar")], category = Bin, comments = "CIRCLE WITH HORIZONTAL BAR"}
  , Record {uchar = '\10678', commands = [("base",""),("unicode","\\circledvert")], category = Bin, comments = "CIRCLED VERTICAL BAR"}
  , Record {uchar = '\10679', commands = [("base",""),("unicode","\\circledparallel")], category = Bin, comments = "CIRCLED PARALLEL"}
  , Record {uchar = '\10680', commands = [("txfonts","\\circledbslash"),("unicode","\\obslash")], category = Bin, comments = "CIRCLED REVERSE SOLIDUS"}
  , Record {uchar = '\10681', commands = [("base",""),("unicode","\\operp")], category = Bin, comments = "CIRCLED PERPENDICULAR"}
  , Record {uchar = '\10682', commands = [("base",""),("unicode","\\obot")], category = Ord, comments = "CIRCLE DIVIDED BY HORIZONTAL BAR AND TOP HALF DIVIDED BY VERTICAL BAR"}
  , Record {uchar = '\10683', commands = [("base",""),("unicode","\\olcross")], category = Ord, comments = "CIRCLE WITH SUPERIMPOSED X"}
  , Record {uchar = '\10684', commands = [("base",""),("unicode","\\odotslashdot")], category = Ord, comments = "CIRCLED ANTICLOCKWISE-ROTATED DIVISION SIGN"}
  , Record {uchar = '\10685', commands = [("base",""),("unicode","\\uparrowoncircle")], category = Ord, comments = "UP ARROW THROUGH CIRCLE"}
  , Record {uchar = '\10686', commands = [("base",""),("unicode","\\circledwhitebullet")], category = Ord, comments = "CIRCLED WHITE BULLET"}
  , Record {uchar = '\10687', commands = [("base",""),("unicode","\\circledbullet")], category = Ord, comments = "CIRCLED BULLET"}
  , Record {uchar = '\10688', commands = [("txfonts","\\circledless"),("unicode","\\olessthan")], category = Bin, comments = "CIRCLED LESS-THAN"}
  , Record {uchar = '\10689', commands = [("txfonts","\\circledgtr"),("unicode","\\ogreaterthan")], category = Bin, comments = "CIRCLED GREATER-THAN"}
  , Record {uchar = '\10690', commands = [("base",""),("unicode","\\cirscir")], category = Ord, comments = "CIRCLE WITH SMALL CIRCLE TO THE RIGHT"}
  , Record {uchar = '\10691', commands = [("base",""),("unicode","\\cirE")], category = Ord, comments = "CIRCLE WITH TWO HORIZONTAL STROKES TO THE RIGHT"}
  , Record {uchar = '\10692', commands = [("stmaryrd","\\boxslash"),("txfonts","\\boxslash"),("unicode","\\boxdiag")], category = Bin, comments = "SQUARED RISING DIAGONAL SLASH"}
  , Record {uchar = '\10693', commands = [("stmaryrd","\\boxbslash"),("txfonts","\\boxbslash"),("unicode","\\boxbslash")], category = Bin, comments = "SQUARED FALLING DIAGONAL SLASH"}
  , Record {uchar = '\10694', commands = [("stmaryrd","\\boxast"),("txfonts","\\boxast"),("unicode","\\boxast")], category = Bin, comments = "SQUARED ASTERISK"}
  , Record {uchar = '\10695', commands = [("stmaryrd","\\boxcircle"),("unicode","\\boxcircle")], category = Bin, comments = "SQUARED SMALL CIRCLE"}
  , Record {uchar = '\10696', commands = [("stmaryrd","\\boxbox"),("unicode","\\boxbox")], category = Bin, comments = "SQUARED SQUARE"}
  , Record {uchar = '\10697', commands = [("base",""),("unicode","\\boxonbox")], category = Ord, comments = "TWO JOINED SQUARES"}
  , Record {uchar = '\10698', commands = [("base",""),("unicode","\\triangleodot")], category = Ord, comments = "TRIANGLE WITH DOT ABOVE"}
  , Record {uchar = '\10699', commands = [("base",""),("unicode","\\triangleubar")], category = Ord, comments = "TRIANGLE WITH UNDERBAR"}
  , Record {uchar = '\10700', commands = [("base",""),("unicode","\\triangles")], category = Ord, comments = "S IN TRIANGLE"}
  , Record {uchar = '\10701', commands = [("base",""),("unicode","\\triangleserifs")], category = Bin, comments = "TRIANGLE WITH SERIFS AT BOTTOM"}
  , Record {uchar = '\10702', commands = [("base",""),("unicode","\\rtriltri")], category = Rel, comments = "RIGHT TRIANGLE ABOVE LEFT TRIANGLE"}
  , Record {uchar = '\10703', commands = [("wrisym","\\LeftTriangleBar"),("unicode","\\ltrivb")], category = Rel, comments = "LEFT TRIANGLE BESIDE VERTICAL BAR"}
  , Record {uchar = '\10704', commands = [("wrisym","\\RightTriangleBar"),("unicode","\\vbrtri")], category = Rel, comments = "VERTICAL BAR BESIDE RIGHT TRIANGLE"}
  , Record {uchar = '\10705', commands = [("base",""),("unicode","\\lfbowtie")], category = Rel, comments = "left black bowtie"}
  , Record {uchar = '\10706', commands = [("base",""),("unicode","\\rfbowtie")], category = Rel, comments = "right black bowtie"}
  , Record {uchar = '\10707', commands = [("base",""),("unicode","\\fbowtie")], category = Rel, comments = "BLACK BOWTIE"}
  , Record {uchar = '\10708', commands = [("base",""),("unicode","\\lftimes")], category = Rel, comments = "left black times"}
  , Record {uchar = '\10709', commands = [("base",""),("unicode","\\rftimes")], category = Rel, comments = "right black times"}
  , Record {uchar = '\10710', commands = [("base",""),("unicode","\\hourglass")], category = Bin, comments = "WHITE HOURGLASS"}
  , Record {uchar = '\10711', commands = [("base",""),("unicode","\\blackhourglass")], category = Bin, comments = "BLACK HOURGLASS"}
  , Record {uchar = '\10712', commands = [("base",""),("unicode","\\lvzigzag")], category = Open, comments = "LEFT WIGGLY FENCE"}
  , Record {uchar = '\10713', commands = [("base",""),("unicode","\\rvzigzag")], category = Close, comments = "RIGHT WIGGLY FENCE"}
  , Record {uchar = '\10714', commands = [("base",""),("unicode","\\Lvzigzag")], category = Open, comments = "LEFT DOUBLE WIGGLY FENCE"}
  , Record {uchar = '\10715', commands = [("base",""),("unicode","\\Rvzigzag")], category = Close, comments = "RIGHT DOUBLE WIGGLY FENCE"}
  , Record {uchar = '\10716', commands = [("base",""),("unicode","\\iinfin")], category = Ord, comments = "INCOMPLETE INFINITY"}
  , Record {uchar = '\10717', commands = [("base",""),("unicode","\\tieinfty")], category = Ord, comments = "TIE OVER INFINITY"}
  , Record {uchar = '\10718', commands = [("base",""),("unicode","\\nvinfty")], category = Ord, comments = "INFINITY NEGATED WITH VERTICAL BAR"}
  , Record {uchar = '\10719', commands = [("txfonts","\\multimapboth"),("unicode","\\dualmap")], category = Rel, comments = "DOUBLE-ENDED MULTIMAP"}
  , Record {uchar = '\10720', commands = [("base",""),("unicode","\\laplac")], category = Ord, comments = "SQUARE WITH CONTOURED OUTLINE"}
  , Record {uchar = '\10721', commands = [("base",""),("unicode","\\lrtriangleeq")], category = Rel, comments = "INCREASES AS"}
  , Record {uchar = '\10722', commands = [("base",""),("unicode","\\shuffle")], category = Bin, comments = "SHUFFLE PRODUCT"}
  , Record {uchar = '\10723', commands = [("base",""),("unicode","\\eparsl")], category = Rel, comments = "EQUALS SIGN AND SLANTED PARALLEL"}
  , Record {uchar = '\10724', commands = [("base",""),("unicode","\\smeparsl")], category = Rel, comments = "EQUALS SIGN AND SLANTED PARALLEL WITH TILDE ABOVE"}
  , Record {uchar = '\10725', commands = [("base",""),("unicode","\\eqvparsl")], category = Rel, comments = "IDENTICAL TO AND SLANTED PARALLEL"}
  , Record {uchar = '\10726', commands = [("base",""),("unicode","\\gleichstark")], category = Rel, comments = "GLEICH STARK"}
  , Record {uchar = '\10727', commands = [("base",""),("unicode","\\thermod")], category = Ord, comments = "THERMODYNAMIC"}
  , Record {uchar = '\10728', commands = [("base",""),("unicode","\\downtriangleleftblack")], category = Ord, comments = "DOWN-POINTING TRIANGLE WITH LEFT HALF BLACK"}
  , Record {uchar = '\10729', commands = [("base",""),("unicode","\\downtrianglerightblack")], category = Ord, comments = "DOWN-POINTING TRIANGLE WITH RIGHT HALF BLACK"}
  , Record {uchar = '\10730', commands = [("base",""),("unicode","\\blackdiamonddownarrow")], category = Ord, comments = "BLACK DIAMOND WITH DOWN ARROW"}
  , Record {uchar = '\10731', commands = [("amssymb","\\blacklozenge"),("unicode","\\mdlgblklozenge")], category = Bin, comments = "BLACK LOZENGE"}
  , Record {uchar = '\10732', commands = [("base",""),("unicode","\\circledownarrow")], category = Ord, comments = "WHITE CIRCLE WITH DOWN ARROW"}
  , Record {uchar = '\10733', commands = [("base",""),("unicode","\\blackcircledownarrow")], category = Ord, comments = "BLACK CIRCLE WITH DOWN ARROW"}
  , Record {uchar = '\10734', commands = [("base",""),("unicode","\\errbarsquare")], category = Ord, comments = "ERROR-BARRED WHITE SQUARE"}
  , Record {uchar = '\10735', commands = [("base",""),("unicode","\\errbarblacksquare")], category = Ord, comments = "ERROR-BARRED BLACK SQUARE"}
  , Record {uchar = '\10736', commands = [("base",""),("unicode","\\errbardiamond")], category = Ord, comments = "ERROR-BARRED WHITE DIAMOND"}
  , Record {uchar = '\10737', commands = [("base",""),("unicode","\\errbarblackdiamond")], category = Ord, comments = "ERROR-BARRED BLACK DIAMOND"}
  , Record {uchar = '\10738', commands = [("base",""),("unicode","\\errbarcircle")], category = Ord, comments = "ERROR-BARRED WHITE CIRCLE"}
  , Record {uchar = '\10739', commands = [("base",""),("unicode","\\errbarblackcircle")], category = Ord, comments = "ERROR-BARRED BLACK CIRCLE"}
  , Record {uchar = '\10740', commands = [("base",""),("unicode","\\ruledelayed")], category = Rel, comments = "RULE-DELAYED"}
  , Record {uchar = '\10741', commands = [("base","\\setminus"),("unicode","\\setminus")], category = Bin, comments = "REVERSE SOLIDUS OPERATOR"}
  , Record {uchar = '\10742', commands = [("base",""),("unicode","\\dsol")], category = Bin, comments = "SOLIDUS WITH OVERBAR"}
  , Record {uchar = '\10743', commands = [("base",""),("unicode","\\rsolbar")], category = Bin, comments = "REVERSE SOLIDUS WITH HORIZONTAL STROKE"}
  , Record {uchar = '\10744', commands = [("base",""),("unicode","\\xsol")], category = Op, comments = "BIG SOLIDUS"}
  , Record {uchar = '\10745', commands = [("oz","\\zhide"),("oz","\\hide"),("unicode","\\xbsol")], category = Op, comments = "BIG REVERSE SOLIDUS, z notation schema hiding"}
  , Record {uchar = '\10746', commands = [("base",""),("unicode","\\doubleplus")], category = Bin, comments = "DOUBLE PLUS"}
  , Record {uchar = '\10747', commands = [("base",""),("unicode","\\tripleplus")], category = Bin, comments = "TRIPLE PLUS"}
  , Record {uchar = '\10748', commands = [("base",""),("unicode","\\lcurvyangle")], category = Open, comments = "left pointing curved angle bracket"}
  , Record {uchar = '\10749', commands = [("base",""),("unicode","\\rcurvyangle")], category = Close, comments = "right pointing curved angle bracket"}
  , Record {uchar = '\10750', commands = [("base",""),("unicode","\\tplus")], category = Bin, comments = "TINY"}
  , Record {uchar = '\10751', commands = [("base",""),("unicode","\\tminus")], category = Bin, comments = "MINY"}
  , Record {uchar = '\10752', commands = [("base","\\bigodot"),("unicode","\\bigodot")], category = Op, comments = "N-ARY CIRCLED DOT OPERATOR"}
  , Record {uchar = '\10753', commands = [("base","\\bigoplus"),("unicode","\\bigoplus")], category = Op, comments = "N-ARY CIRCLED PLUS OPERATOR"}
  , Record {uchar = '\10754', commands = [("base","\\bigotimes"),("unicode","\\bigotimes")], category = Op, comments = "N-ARY CIRCLED TIMES OPERATOR"}
  , Record {uchar = '\10755', commands = [("base",""),("unicode","\\bigcupdot")], category = Op, comments = "N-ARY UNION OPERATOR WITH DOT"}
  , Record {uchar = '\10756', commands = [("base","\\biguplus"),("unicode","\\biguplus")], category = Op, comments = "N-ARY UNION OPERATOR WITH PLUS"}
  , Record {uchar = '\10757', commands = [("txfonts","\\bigsqcap"),("unicode","\\bigsqcap")], category = Op, comments = "N-ARY SQUARE INTERSECTION OPERATOR"}
  , Record {uchar = '\10758', commands = [("base","\\bigsqcup"),("unicode","\\bigsqcup")], category = Op, comments = "N-ARY SQUARE UNION OPERATOR"}
  , Record {uchar = '\10759', commands = [("base",""),("unicode","\\conjquant")], category = Op, comments = "TWO LOGICAL AND OPERATOR"}
  , Record {uchar = '\10760', commands = [("base",""),("unicode","\\disjquant")], category = Op, comments = "TWO LOGICAL OR OPERATOR"}
  , Record {uchar = '\10761', commands = [("txfonts","\\varprod"),("unicode","\\bigtimes")], category = Op, comments = "N-ARY TIMES OPERATOR"}
  , Record {uchar = '\10762', commands = [("base",""),("unicode","\\modtwosum")], category = Ord, comments = "MODULO TWO SUM"}
  , Record {uchar = '\10763', commands = [("base",""),("unicode","\\sumint")], category = Op, comments = "SUMMATION WITH INTEGRAL"}
  , Record {uchar = '\10764', commands = [("amsmath","\\iiiint"),("esint","\\iiiint"),("unicode","\\iiiint")], category = Op, comments = "QUADRUPLE INTEGRAL OPERATOR"}
  , Record {uchar = '\10765', commands = [("base",""),("unicode","\\intbar")], category = Op, comments = "FINITE PART INTEGRAL"}
  , Record {uchar = '\10766', commands = [("base",""),("unicode","\\intBar")], category = Op, comments = "INTEGRAL WITH DOUBLE STROKE"}
  , Record {uchar = '\10767', commands = [("esint","\\fint"),("wrisym","\\fint"),("unicode","\\fint")], category = Op, comments = "INTEGRAL AVERAGE WITH SLASH"}
  , Record {uchar = '\10768', commands = [("base",""),("unicode","\\cirfnint")], category = Op, comments = "CIRCULATION FUNCTION"}
  , Record {uchar = '\10769', commands = [("base",""),("unicode","\\awint")], category = Op, comments = "ANTICLOCKWISE INTEGRATION"}
  , Record {uchar = '\10770', commands = [("base",""),("unicode","\\rppolint")], category = Op, comments = "LINE INTEGRATION WITH RECTANGULAR PATH AROUND POLE"}
  , Record {uchar = '\10771', commands = [("base",""),("unicode","\\scpolint")], category = Op, comments = "LINE INTEGRATION WITH SEMICIRCULAR PATH AROUND POLE"}
  , Record {uchar = '\10772', commands = [("base",""),("unicode","\\npolint")], category = Op, comments = "LINE INTEGRATION NOT INCLUDING THE POLE"}
  , Record {uchar = '\10773', commands = [("base",""),("unicode","\\pointint")], category = Op, comments = "INTEGRAL AROUND A POINT OPERATOR"}
  , Record {uchar = '\10774', commands = [("esint","\\sqint"),("wrisym","\\sqrint"),("unicode","\\sqint")], category = Op, comments = "QUATERNION INTEGRAL OPERATOR"}
  , Record {uchar = '\10775', commands = [("base",""),("unicode","\\intlarhk")], category = Op, comments = "INTEGRAL WITH LEFTWARDS ARROW WITH HOOK"}
  , Record {uchar = '\10776', commands = [("base",""),("unicode","\\intx")], category = Op, comments = "INTEGRAL WITH TIMES SIGN"}
  , Record {uchar = '\10777', commands = [("base",""),("unicode","\\intcap")], category = Op, comments = "INTEGRAL WITH INTERSECTION"}
  , Record {uchar = '\10778', commands = [("base",""),("unicode","\\intcup")], category = Op, comments = "INTEGRAL WITH UNION"}
  , Record {uchar = '\10779', commands = [("base",""),("unicode","\\upint")], category = Op, comments = "INTEGRAL WITH OVERBAR"}
  , Record {uchar = '\10780', commands = [("base",""),("unicode","\\lowint")], category = Op, comments = "INTEGRAL WITH UNDERBAR"}
  , Record {uchar = '\10781', commands = [("amssymb","\\Join"),("unicode","\\Join")], category = Op, comments = "JOIN"}
  , Record {uchar = '\10782', commands = [("base",""),("unicode","\\bigtriangleleft")], category = Op, comments = "LARGE LEFT TRIANGLE OPERATOR"}
  , Record {uchar = '\10783', commands = [("oz","\\zcmp"),("oz","\\semi"),("unicode","\\zcmp")], category = Op, comments = "= \\fatsemi (stmaryrd), Z NOTATION SCHEMA COMPOSITION"}
  , Record {uchar = '\10784', commands = [("oz","\\zpipe"),("unicode","\\zpipe")], category = Op, comments = "Z NOTATION SCHEMA PIPING"}
  , Record {uchar = '\10785', commands = [("oz","\\zproject"),("oz","\\project"),("unicode","\\zproject")], category = Op, comments = "Z NOTATION SCHEMA PROJECTION"}
  , Record {uchar = '\10786', commands = [("base",""),("unicode","\\ringplus")], category = Bin, comments = "PLUS SIGN WITH SMALL CIRCLE ABOVE"}
  , Record {uchar = '\10787', commands = [("base",""),("unicode","\\plushat")], category = Bin, comments = "PLUS SIGN WITH CIRCUMFLEX ACCENT ABOVE"}
  , Record {uchar = '\10788', commands = [("base",""),("unicode","\\simplus")], category = Bin, comments = "PLUS SIGN WITH TILDE ABOVE"}
  , Record {uchar = '\10789', commands = [("base",""),("unicode","\\plusdot")], category = Bin, comments = "PLUS SIGN WITH DOT BELOW"}
  , Record {uchar = '\10790', commands = [("base",""),("unicode","\\plussim")], category = Bin, comments = "PLUS SIGN WITH TILDE BELOW"}
  , Record {uchar = '\10791', commands = [("base",""),("unicode","\\plussubtwo")], category = Bin, comments = "PLUS SIGN WITH SUBSCRIPT TWO"}
  , Record {uchar = '\10792', commands = [("base",""),("unicode","\\plustrif")], category = Bin, comments = "PLUS SIGN WITH BLACK TRIANGLE"}
  , Record {uchar = '\10793', commands = [("base",""),("unicode","\\commaminus")], category = Bin, comments = "MINUS SIGN WITH COMMA ABOVE"}
  , Record {uchar = '\10794', commands = [("base",""),("unicode","\\minusdot")], category = Bin, comments = "MINUS SIGN WITH DOT BELOW"}
  , Record {uchar = '\10795', commands = [("base",""),("unicode","\\minusfdots")], category = Bin, comments = "MINUS SIGN WITH FALLING DOTS"}
  , Record {uchar = '\10796', commands = [("base",""),("unicode","\\minusrdots")], category = Bin, comments = "MINUS SIGN WITH RISING DOTS"}
  , Record {uchar = '\10797', commands = [("base",""),("unicode","\\opluslhrim")], category = Bin, comments = "PLUS SIGN IN LEFT HALF CIRCLE"}
  , Record {uchar = '\10798', commands = [("base",""),("unicode","\\oplusrhrim")], category = Bin, comments = "PLUS SIGN IN RIGHT HALF CIRCLE"}
  , Record {uchar = '\10799', commands = [("base",""),("base","\\times"),("unicode","\\vectimes")], category = Bin, comments = "VECTOR OR CROSS PRODUCT"}
  , Record {uchar = '\10800', commands = [("base",""),("unicode","\\dottimes")], category = Bin, comments = "MULTIPLICATION SIGN WITH DOT ABOVE"}
  , Record {uchar = '\10801', commands = [("base",""),("unicode","\\timesbar")], category = Bin, comments = "MULTIPLICATION SIGN WITH UNDERBAR"}
  , Record {uchar = '\10802', commands = [("base",""),("unicode","\\btimes")], category = Bin, comments = "SEMIDIRECT PRODUCT WITH BOTTOM CLOSED"}
  , Record {uchar = '\10803', commands = [("base",""),("unicode","\\smashtimes")], category = Bin, comments = "SMASH PRODUCT"}
  , Record {uchar = '\10804', commands = [("base",""),("unicode","\\otimeslhrim")], category = Bin, comments = "MULTIPLICATION SIGN IN LEFT HALF CIRCLE"}
  , Record {uchar = '\10805', commands = [("base",""),("unicode","\\otimesrhrim")], category = Bin, comments = "MULTIPLICATION SIGN IN RIGHT HALF CIRCLE"}
  , Record {uchar = '\10806', commands = [("base",""),("unicode","\\otimeshat")], category = Bin, comments = "CIRCLED MULTIPLICATION SIGN WITH CIRCUMFLEX ACCENT"}
  , Record {uchar = '\10807', commands = [("base",""),("unicode","\\Otimes")], category = Bin, comments = "MULTIPLICATION SIGN IN DOUBLE CIRCLE"}
  , Record {uchar = '\10808', commands = [("base",""),("unicode","\\odiv")], category = Bin, comments = "CIRCLED DIVISION SIGN"}
  , Record {uchar = '\10809', commands = [("base",""),("unicode","\\triangleplus")], category = Bin, comments = "PLUS SIGN IN TRIANGLE"}
  , Record {uchar = '\10810', commands = [("base",""),("unicode","\\triangleminus")], category = Bin, comments = "MINUS SIGN IN TRIANGLE"}
  , Record {uchar = '\10811', commands = [("base",""),("unicode","\\triangletimes")], category = Bin, comments = "MULTIPLICATION SIGN IN TRIANGLE"}
  , Record {uchar = '\10812', commands = [("base",""),("unicode","\\intprod")], category = Bin, comments = "INTERIOR PRODUCT"}
  , Record {uchar = '\10813', commands = [("base",""),("unicode","\\intprodr")], category = Bin, comments = "RIGHTHAND INTERIOR PRODUCT"}
  , Record {uchar = '\10814', commands = [("oz","\\fcmp"),("oz","\\comp"),("unicode","\\fcmp")], category = Bin, comments = "Z NOTATION RELATIONAL COMPOSITION"}
  , Record {uchar = '\10815', commands = [("base","\\amalg"),("unicode","\\amalg")], category = Bin, comments = "AMALGAMATION OR COPRODUCT"}
  , Record {uchar = '\10816', commands = [("base",""),("unicode","\\capdot")], category = Bin, comments = "INTERSECTION WITH DOT"}
  , Record {uchar = '\10817', commands = [("base",""),("unicode","\\uminus")], category = Bin, comments = "UNION WITH MINUS SIGN, z notation bag subtraction"}
  , Record {uchar = '\10818', commands = [("base",""),("unicode","\\barcup")], category = Bin, comments = "UNION WITH OVERBAR"}
  , Record {uchar = '\10819', commands = [("base",""),("unicode","\\barcap")], category = Bin, comments = "INTERSECTION WITH OVERBAR"}
  , Record {uchar = '\10820', commands = [("base",""),("unicode","\\capwedge")], category = Bin, comments = "INTERSECTION WITH LOGICAL AND"}
  , Record {uchar = '\10821', commands = [("base",""),("unicode","\\cupvee")], category = Bin, comments = "UNION WITH LOGICAL OR"}
  , Record {uchar = '\10822', commands = [("base",""),("unicode","\\cupovercap")], category = Bin, comments = "UNION ABOVE INTERSECTION"}
  , Record {uchar = '\10823', commands = [("base",""),("unicode","\\capovercup")], category = Bin, comments = "INTERSECTION ABOVE UNION"}
  , Record {uchar = '\10824', commands = [("base",""),("unicode","\\cupbarcap")], category = Bin, comments = "UNION ABOVE BAR ABOVE INTERSECTION"}
  , Record {uchar = '\10825', commands = [("base",""),("unicode","\\capbarcup")], category = Bin, comments = "INTERSECTION ABOVE BAR ABOVE UNION"}
  , Record {uchar = '\10826', commands = [("base",""),("unicode","\\twocups")], category = Bin, comments = "UNION BESIDE AND JOINED WITH UNION"}
  , Record {uchar = '\10827', commands = [("base",""),("unicode","\\twocaps")], category = Bin, comments = "INTERSECTION BESIDE AND JOINED WITH INTERSECTION"}
  , Record {uchar = '\10828', commands = [("base",""),("unicode","\\closedvarcup")], category = Bin, comments = "CLOSED UNION WITH SERIFS"}
  , Record {uchar = '\10829', commands = [("base",""),("unicode","\\closedvarcap")], category = Bin, comments = "CLOSED INTERSECTION WITH SERIFS"}
  , Record {uchar = '\10830', commands = [("base",""),("unicode","\\Sqcap")], category = Bin, comments = "DOUBLE SQUARE INTERSECTION"}
  , Record {uchar = '\10831', commands = [("base",""),("unicode","\\Sqcup")], category = Bin, comments = "DOUBLE SQUARE UNION"}
  , Record {uchar = '\10832', commands = [("base",""),("unicode","\\closedvarcupsmashprod")], category = Bin, comments = "CLOSED UNION WITH SERIFS AND SMASH PRODUCT"}
  , Record {uchar = '\10833', commands = [("base",""),("unicode","\\wedgeodot")], category = Bin, comments = "LOGICAL AND WITH DOT ABOVE"}
  , Record {uchar = '\10834', commands = [("base",""),("unicode","\\veeodot")], category = Bin, comments = "LOGICAL OR WITH DOT ABOVE"}
  , Record {uchar = '\10835', commands = [("base",""),("unicode","\\Wedge")], category = Bin, comments = "DOUBLE LOGICAL AND"}
  , Record {uchar = '\10836', commands = [("base",""),("unicode","\\Vee")], category = Bin, comments = "DOUBLE LOGICAL OR"}
  , Record {uchar = '\10837', commands = [("base",""),("unicode","\\wedgeonwedge")], category = Bin, comments = "TWO INTERSECTING LOGICAL AND"}
  , Record {uchar = '\10838', commands = [("base",""),("unicode","\\veeonvee")], category = Bin, comments = "TWO INTERSECTING LOGICAL OR"}
  , Record {uchar = '\10839', commands = [("base",""),("unicode","\\bigslopedvee")], category = Bin, comments = "SLOPING LARGE OR"}
  , Record {uchar = '\10840', commands = [("base",""),("unicode","\\bigslopedwedge")], category = Bin, comments = "SLOPING LARGE AND"}
  , Record {uchar = '\10841', commands = [("base",""),("unicode","\\veeonwedge")], category = Rel, comments = "LOGICAL OR OVERLAPPING LOGICAL AND"}
  , Record {uchar = '\10842', commands = [("base",""),("unicode","\\wedgemidvert")], category = Bin, comments = "LOGICAL AND WITH MIDDLE STEM"}
  , Record {uchar = '\10843', commands = [("base",""),("unicode","\\veemidvert")], category = Bin, comments = "LOGICAL OR WITH MIDDLE STEM"}
  , Record {uchar = '\10844', commands = [("base",""),("unicode","\\midbarwedge")], category = Bin, comments = "ogical and with horizontal dash"}
  , Record {uchar = '\10845', commands = [("base",""),("unicode","\\midbarvee")], category = Bin, comments = "LOGICAL OR WITH HORIZONTAL DASH"}
  , Record {uchar = '\10846', commands = [("amssymb","\\doublebarwedge"),("unicode","\\doublebarwedge")], category = Bin, comments = "LOGICAL AND WITH DOUBLE OVERBAR"}
  , Record {uchar = '\10847', commands = [("base",""),("unicode","\\wedgebar")], category = Bin, comments = "LOGICAL AND WITH UNDERBAR"}
  , Record {uchar = '\10848', commands = [("base",""),("unicode","\\wedgedoublebar")], category = Bin, comments = "LOGICAL AND WITH DOUBLE UNDERBAR"}
  , Record {uchar = '\10849', commands = [("base",""),("unicode","\\varveebar")], category = Bin, comments = "SMALL VEE WITH UNDERBAR"}
  , Record {uchar = '\10850', commands = [("base",""),("unicode","\\doublebarvee")], category = Bin, comments = "LOGICAL OR WITH DOUBLE OVERBAR"}
  , Record {uchar = '\10851', commands = [("base",""),("unicode","\\veedoublebar")], category = Bin, comments = "LOGICAL OR WITH DOUBLE UNDERBAR"}
  , Record {uchar = '\10852', commands = [("oz","\\dsub"),("oz","\\ndres"),("unicode","\\dsub")], category = Bin, comments = "Z NOTATION DOMAIN ANTIRESTRICTION"}
  , Record {uchar = '\10853', commands = [("oz","\\rsub"),("oz","\\nrres"),("unicode","\\rsub")], category = Bin, comments = "Z NOTATION RANGE ANTIRESTRICTION"}
  , Record {uchar = '\10854', commands = [("base",""),("unicode","\\eqdot")], category = Rel, comments = "EQUALS SIGN WITH DOT BELOW"}
  , Record {uchar = '\10855', commands = [("base",""),("unicode","\\dotequiv")], category = Rel, comments = "IDENTICAL WITH DOT ABOVE"}
  , Record {uchar = '\10856', commands = [("base",""),("unicode","\\equivVert")], category = Rel, comments = "TRIPLE HORIZONTAL BAR WITH DOUBLE VERTICAL STROKE"}
  , Record {uchar = '\10857', commands = [("base",""),("unicode","\\equivVvert")], category = Rel, comments = "TRIPLE HORIZONTAL BAR WITH TRIPLE VERTICAL STROKE"}
  , Record {uchar = '\10858', commands = [("base",""),("unicode","\\dotsim")], category = Rel, comments = "TILDE OPERATOR WITH DOT ABOVE"}
  , Record {uchar = '\10859', commands = [("base",""),("unicode","\\simrdots")], category = Rel, comments = "TILDE OPERATOR WITH RISING DOTS"}
  , Record {uchar = '\10860', commands = [("base",""),("unicode","\\simminussim")], category = Rel, comments = "SIMILAR MINUS SIMILAR"}
  , Record {uchar = '\10861', commands = [("base",""),("unicode","\\congdot")], category = Rel, comments = "CONGRUENT WITH DOT ABOVE"}
  , Record {uchar = '\10862', commands = [("base",""),("unicode","\\asteq")], category = Rel, comments = "EQUALS WITH ASTERISK"}
  , Record {uchar = '\10863', commands = [("base",""),("unicode","\\hatapprox")], category = Rel, comments = "ALMOST EQUAL TO WITH CIRCUMFLEX ACCENT"}
  , Record {uchar = '\10864', commands = [("base",""),("unicode","\\approxeqq")], category = Rel, comments = "APPROXIMATELY EQUAL OR EQUAL TO"}
  , Record {uchar = '\10865', commands = [("base",""),("unicode","\\eqqplus")], category = Bin, comments = "EQUALS SIGN ABOVE PLUS SIGN"}
  , Record {uchar = '\10866', commands = [("base",""),("unicode","\\pluseqq")], category = Bin, comments = "PLUS SIGN ABOVE EQUALS SIGN"}
  , Record {uchar = '\10867', commands = [("base",""),("unicode","\\eqqsim")], category = Rel, comments = "EQUALS SIGN ABOVE TILDE OPERATOR"}
  , Record {uchar = '\10868', commands = [("txfonts","\\Coloneqq"),("base","::="),("unicode","\\Coloneq")], category = Rel, comments = "x \\Coloneq (txfonts), DOUBLE COLON EQUAL"}
  , Record {uchar = '\10869', commands = [("wrisym","\\Equal"),("base","=="),("unicode","\\eqeq")], category = Rel, comments = "TWO CONSECUTIVE EQUALS SIGNS"}
  , Record {uchar = '\10870', commands = [("wrisym","\\Same"),("base","==="),("unicode","\\eqeqeq")], category = Rel, comments = "THREE CONSECUTIVE EQUALS SIGNS"}
  , Record {uchar = '\10871', commands = [("base",""),("unicode","\\ddotseq")], category = Rel, comments = "EQUALS SIGN WITH TWO DOTS ABOVE AND TWO DOTS BELOW"}
  , Record {uchar = '\10872', commands = [("base",""),("unicode","\\equivDD")], category = Rel, comments = "EQUIVALENT WITH FOUR DOTS ABOVE"}
  , Record {uchar = '\10873', commands = [("base",""),("unicode","\\ltcir")], category = Rel, comments = "LESS-THAN WITH CIRCLE INSIDE"}
  , Record {uchar = '\10874', commands = [("base",""),("unicode","\\gtcir")], category = Rel, comments = "GREATER-THAN WITH CIRCLE INSIDE"}
  , Record {uchar = '\10875', commands = [("base",""),("unicode","\\ltquest")], category = Rel, comments = "LESS-THAN WITH QUESTION MARK ABOVE"}
  , Record {uchar = '\10876', commands = [("base",""),("unicode","\\gtquest")], category = Rel, comments = "GREATER-THAN WITH QUESTION MARK ABOVE"}
  , Record {uchar = '\10877', commands = [("amssymb","\\leqslant"),("fourier","\\leqslant"),("unicode","\\leqslant")], category = Rel, comments = "LESS-THAN OR SLANTED EQUAL TO"}
  , Record {uchar = '\10878', commands = [("amssymb","\\geqslant"),("fourier","\\geqslant"),("unicode","\\geqslant")], category = Rel, comments = "GREATER-THAN OR SLANTED EQUAL TO"}
  , Record {uchar = '\10879', commands = [("base",""),("unicode","\\lesdot")], category = Rel, comments = "LESS-THAN OR SLANTED EQUAL TO WITH DOT INSIDE"}
  , Record {uchar = '\10880', commands = [("base",""),("unicode","\\gesdot")], category = Rel, comments = "GREATER-THAN OR SLANTED EQUAL TO WITH DOT INSIDE"}
  , Record {uchar = '\10881', commands = [("base",""),("unicode","\\lesdoto")], category = Rel, comments = "LESS-THAN OR SLANTED EQUAL TO WITH DOT ABOVE"}
  , Record {uchar = '\10882', commands = [("base",""),("unicode","\\gesdoto")], category = Rel, comments = "GREATER-THAN OR SLANTED EQUAL TO WITH DOT ABOVE"}
  , Record {uchar = '\10883', commands = [("base",""),("unicode","\\lesdotor")], category = Rel, comments = "LESS-THAN OR SLANTED EQUAL TO WITH DOT ABOVE RIGHT"}
  , Record {uchar = '\10884', commands = [("base",""),("unicode","\\gesdotol")], category = Rel, comments = "GREATER-THAN OR SLANTED EQUAL TO WITH DOT ABOVE LEFT"}
  , Record {uchar = '\10885', commands = [("amssymb","\\lessapprox"),("unicode","\\lessapprox")], category = Rel, comments = "LESS-THAN OR APPROXIMATE"}
  , Record {uchar = '\10886', commands = [("amssymb","\\gtrapprox"),("unicode","\\gtrapprox")], category = Rel, comments = "GREATER-THAN OR APPROXIMATE"}
  , Record {uchar = '\10887', commands = [("amssymb","\\lneq"),("unicode","\\lneq")], category = Rel, comments = "LESS-THAN AND SINGLE-LINE NOT EQUAL TO"}
  , Record {uchar = '\10888', commands = [("amssymb","\\gneq"),("unicode","\\gneq")], category = Rel, comments = "GREATER-THAN AND SINGLE-LINE NOT EQUAL TO"}
  , Record {uchar = '\10889', commands = [("amssymb","\\lnapprox"),("unicode","\\lnapprox")], category = Rel, comments = "LESS-THAN AND NOT APPROXIMATE"}
  , Record {uchar = '\10890', commands = [("amssymb","\\gnapprox"),("unicode","\\gnapprox")], category = Rel, comments = "GREATER-THAN AND NOT APPROXIMATE"}
  , Record {uchar = '\10891', commands = [("amssymb","\\lesseqqgtr"),("unicode","\\lesseqqgtr")], category = Rel, comments = "LESS-THAN ABOVE DOUBLE-LINE EQUAL ABOVE GREATER-THAN"}
  , Record {uchar = '\10892', commands = [("amssymb","\\gtreqqless"),("unicode","\\gtreqqless")], category = Rel, comments = "GREATER-THAN ABOVE DOUBLE-LINE EQUAL ABOVE LESS-THAN"}
  , Record {uchar = '\10893', commands = [("base",""),("unicode","\\lsime")], category = Rel, comments = "LESS-THAN ABOVE SIMILAR OR EQUAL"}
  , Record {uchar = '\10894', commands = [("base",""),("unicode","\\gsime")], category = Rel, comments = "GREATER-THAN ABOVE SIMILAR OR EQUAL"}
  , Record {uchar = '\10895', commands = [("base",""),("unicode","\\lsimg")], category = Rel, comments = "LESS-THAN ABOVE SIMILAR ABOVE GREATER-THAN"}
  , Record {uchar = '\10896', commands = [("base",""),("unicode","\\gsiml")], category = Rel, comments = "GREATER-THAN ABOVE SIMILAR ABOVE LESS-THAN"}
  , Record {uchar = '\10897', commands = [("base",""),("unicode","\\lgE")], category = Rel, comments = "LESS-THAN ABOVE GREATER-THAN ABOVE DOUBLE-LINE EQUAL"}
  , Record {uchar = '\10898', commands = [("base",""),("unicode","\\glE")], category = Rel, comments = "GREATER-THAN ABOVE LESS-THAN ABOVE DOUBLE-LINE EQUAL"}
  , Record {uchar = '\10899', commands = [("base",""),("unicode","\\lesges")], category = Rel, comments = "LESS-THAN ABOVE SLANTED EQUAL ABOVE GREATER-THAN ABOVE SLANTED EQUAL"}
  , Record {uchar = '\10900', commands = [("base",""),("unicode","\\gesles")], category = Rel, comments = "GREATER-THAN ABOVE SLANTED EQUAL ABOVE LESS-THAN ABOVE SLANTED EQUAL"}
  , Record {uchar = '\10901', commands = [("amssymb","\\eqslantless"),("unicode","\\eqslantless")], category = Rel, comments = "SLANTED EQUAL TO OR LESS-THAN"}
  , Record {uchar = '\10902', commands = [("amssymb","\\eqslantgtr"),("unicode","\\eqslantgtr")], category = Rel, comments = "SLANTED EQUAL TO OR GREATER-THAN"}
  , Record {uchar = '\10903', commands = [("base",""),("unicode","\\elsdot")], category = Rel, comments = "SLANTED EQUAL TO OR LESS-THAN WITH DOT INSIDE"}
  , Record {uchar = '\10904', commands = [("base",""),("unicode","\\egsdot")], category = Rel, comments = "SLANTED EQUAL TO OR GREATER-THAN WITH DOT INSIDE"}
  , Record {uchar = '\10905', commands = [("base",""),("unicode","\\eqqless")], category = Rel, comments = "DOUBLE-LINE EQUAL TO OR LESS-THAN"}
  , Record {uchar = '\10906', commands = [("base",""),("unicode","\\eqqgtr")], category = Rel, comments = "DOUBLE-LINE EQUAL TO OR GREATER-THAN"}
  , Record {uchar = '\10907', commands = [("base",""),("unicode","\\eqqslantless")], category = Rel, comments = "DOUBLE-LINE SLANTED EQUAL TO OR LESS-THAN"}
  , Record {uchar = '\10908', commands = [("base",""),("unicode","\\eqqslantgtr")], category = Rel, comments = "DOUBLE-LINE SLANTED EQUAL TO OR GREATER-THAN"}
  , Record {uchar = '\10909', commands = [("base",""),("unicode","\\simless")], category = Rel, comments = "SIMILAR OR LESS-THAN"}
  , Record {uchar = '\10910', commands = [("base",""),("unicode","\\simgtr")], category = Rel, comments = "SIMILAR OR GREATER-THAN"}
  , Record {uchar = '\10911', commands = [("base",""),("unicode","\\simlE")], category = Rel, comments = "SIMILAR ABOVE LESS-THAN ABOVE EQUALS SIGN"}
  , Record {uchar = '\10912', commands = [("base",""),("unicode","\\simgE")], category = Rel, comments = "SIMILAR ABOVE GREATER-THAN ABOVE EQUALS SIGN"}
  , Record {uchar = '\10913', commands = [("wrisym","\\NestedLessLess"),("mathabx -amssymb","\\lll"),("unicode","\\Lt")], category = Rel, comments = "DOUBLE NESTED LESS-THAN"}
  , Record {uchar = '\10914', commands = [("wrisym","\\NestedGreaterGreater"),("mathabx -amssymb","\\ggg"),("unicode","\\Gt")], category = Rel, comments = "DOUBLE NESTED GREATER-THAN"}
  , Record {uchar = '\10915', commands = [("base",""),("unicode","\\partialmeetcontraction")], category = Rel, comments = "double less-than with underbar"}
  , Record {uchar = '\10916', commands = [("base",""),("unicode","\\glj")], category = Rel, comments = "GREATER-THAN OVERLAPPING LESS-THAN"}
  , Record {uchar = '\10917', commands = [("base",""),("unicode","\\gla")], category = Rel, comments = "GREATER-THAN BESIDE LESS-THAN"}
  , Record {uchar = '\10918', commands = [("stmaryrd","\\leftslice"),("unicode","\\ltcc")], category = Rel, comments = "LESS-THAN CLOSED BY CURVE"}
  , Record {uchar = '\10919', commands = [("stmaryrd","\\rightslice"),("unicode","\\gtcc")], category = Rel, comments = "GREATER-THAN CLOSED BY CURVE"}
  , Record {uchar = '\10920', commands = [("base",""),("unicode","\\lescc")], category = Rel, comments = "LESS-THAN CLOSED BY CURVE ABOVE SLANTED EQUAL"}
  , Record {uchar = '\10921', commands = [("base",""),("unicode","\\gescc")], category = Rel, comments = "GREATER-THAN CLOSED BY CURVE ABOVE SLANTED EQUAL"}
  , Record {uchar = '\10922', commands = [("base",""),("unicode","\\smt")], category = Rel, comments = "SMALLER THAN"}
  , Record {uchar = '\10923', commands = [("base",""),("unicode","\\lat")], category = Rel, comments = "LARGER THAN"}
  , Record {uchar = '\10924', commands = [("base",""),("unicode","\\smte")], category = Rel, comments = "SMALLER THAN OR EQUAL TO"}
  , Record {uchar = '\10925', commands = [("base",""),("unicode","\\late")], category = Rel, comments = "LARGER THAN OR EQUAL TO"}
  , Record {uchar = '\10926', commands = [("base",""),("unicode","\\bumpeqq")], category = Rel, comments = "EQUALS SIGN WITH BUMPY ABOVE"}
  , Record {uchar = '\10927', commands = [("base","\\preceq"),("unicode","\\preceq")], category = Rel, comments = "PRECEDES ABOVE SINGLE-LINE EQUALS SIGN"}
  , Record {uchar = '\10928', commands = [("base","\\succeq"),("unicode","\\succeq")], category = Rel, comments = "SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN"}
  , Record {uchar = '\10929', commands = [("base",""),("unicode","\\precneq")], category = Rel, comments = "PRECEDES ABOVE SINGLE-LINE NOT EQUAL TO"}
  , Record {uchar = '\10930', commands = [("base",""),("unicode","\\succneq")], category = Rel, comments = "SUCCEEDS ABOVE SINGLE-LINE NOT EQUAL TO"}
  , Record {uchar = '\10931', commands = [("txfonts","\\preceqq"),("unicode","\\preceqq")], category = Rel, comments = "PRECEDES ABOVE EQUALS SIGN"}
  , Record {uchar = '\10932', commands = [("txfonts","\\succeqq"),("unicode","\\succeqq")], category = Rel, comments = "SUCCEEDS ABOVE EQUALS SIGN"}
  , Record {uchar = '\10933', commands = [("amssymb",""),("unicode","\\precneqq")], category = Rel, comments = "PRECEDES ABOVE NOT EQUAL TO"}
  , Record {uchar = '\10934', commands = [("amssymb",""),("unicode","\\succneqq")], category = Rel, comments = "SUCCEEDS ABOVE NOT EQUAL TO"}
  , Record {uchar = '\10935', commands = [("amssymb","\\precapprox"),("unicode","\\precapprox")], category = Rel, comments = "PRECEDES ABOVE ALMOST EQUAL TO"}
  , Record {uchar = '\10936', commands = [("amssymb","\\succapprox"),("unicode","\\succapprox")], category = Rel, comments = "SUCCEEDS ABOVE ALMOST EQUAL TO"}
  , Record {uchar = '\10937', commands = [("amssymb","\\precnapprox"),("unicode","\\precnapprox")], category = Rel, comments = "PRECEDES ABOVE NOT ALMOST EQUAL TO"}
  , Record {uchar = '\10938', commands = [("amssymb","\\succnapprox"),("unicode","\\succnapprox")], category = Rel, comments = "SUCCEEDS ABOVE NOT ALMOST EQUAL TO"}
  , Record {uchar = '\10939', commands = [("mathabx","\\llcurly"),("unicode","\\Prec")], category = Rel, comments = "DOUBLE PRECEDES"}
  , Record {uchar = '\10940', commands = [("mathabx","\\ggcurly"),("unicode","\\Succ")], category = Rel, comments = "DOUBLE SUCCEEDS"}
  , Record {uchar = '\10941', commands = [("base",""),("unicode","\\subsetdot")], category = Rel, comments = "SUBSET WITH DOT"}
  , Record {uchar = '\10942', commands = [("base",""),("unicode","\\supsetdot")], category = Rel, comments = "SUPERSET WITH DOT"}
  , Record {uchar = '\10943', commands = [("base",""),("unicode","\\subsetplus")], category = Rel, comments = "SUBSET WITH PLUS SIGN BELOW"}
  , Record {uchar = '\10944', commands = [("base",""),("unicode","\\supsetplus")], category = Rel, comments = "SUPERSET WITH PLUS SIGN BELOW"}
  , Record {uchar = '\10945', commands = [("base",""),("unicode","\\submult")], category = Rel, comments = "SUBSET WITH MULTIPLICATION SIGN BELOW"}
  , Record {uchar = '\10946', commands = [("base",""),("unicode","\\supmult")], category = Rel, comments = "SUPERSET WITH MULTIPLICATION SIGN BELOW"}
  , Record {uchar = '\10947', commands = [("base",""),("unicode","\\subedot")], category = Rel, comments = "SUBSET OF OR EQUAL TO WITH DOT ABOVE"}
  , Record {uchar = '\10948', commands = [("base",""),("unicode","\\supedot")], category = Rel, comments = "SUPERSET OF OR EQUAL TO WITH DOT ABOVE"}
  , Record {uchar = '\10949', commands = [("amssymb","\\subseteqq"),("unicode","\\subseteqq")], category = Rel, comments = "SUBSET OF ABOVE EQUALS SIGN"}
  , Record {uchar = '\10950', commands = [("amssymb","\\supseteqq"),("unicode","\\supseteqq")], category = Rel, comments = "SUPERSET OF ABOVE EQUALS SIGN"}
  , Record {uchar = '\10951', commands = [("base",""),("unicode","\\subsim")], category = Rel, comments = "SUBSET OF ABOVE TILDE OPERATOR"}
  , Record {uchar = '\10952', commands = [("base",""),("unicode","\\supsim")], category = Rel, comments = "SUPERSET OF ABOVE TILDE OPERATOR"}
  , Record {uchar = '\10953', commands = [("base",""),("unicode","\\subsetapprox")], category = Rel, comments = "SUBSET OF ABOVE ALMOST EQUAL TO"}
  , Record {uchar = '\10954', commands = [("base",""),("unicode","\\supsetapprox")], category = Rel, comments = "SUPERSET OF ABOVE ALMOST EQUAL TO"}
  , Record {uchar = '\10955', commands = [("amssymb","\\subsetneqq"),("unicode","\\subsetneqq")], category = Rel, comments = "SUBSET OF ABOVE NOT EQUAL TO"}
  , Record {uchar = '\10956', commands = [("amssymb","\\supsetneqq"),("unicode","\\supsetneqq")], category = Rel, comments = "SUPERSET OF ABOVE NOT EQUAL TO"}
  , Record {uchar = '\10957', commands = [("base",""),("unicode","\\lsqhook")], category = Rel, comments = "SQUARE LEFT OPEN BOX OPERATOR"}
  , Record {uchar = '\10958', commands = [("base",""),("unicode","\\rsqhook")], category = Rel, comments = "SQUARE RIGHT OPEN BOX OPERATOR"}
  , Record {uchar = '\10959', commands = [("base",""),("unicode","\\csub")], category = Rel, comments = "CLOSED SUBSET"}
  , Record {uchar = '\10960', commands = [("base",""),("unicode","\\csup")], category = Rel, comments = "CLOSED SUPERSET"}
  , Record {uchar = '\10961', commands = [("base",""),("unicode","\\csube")], category = Rel, comments = "CLOSED SUBSET OR EQUAL TO"}
  , Record {uchar = '\10962', commands = [("base",""),("unicode","\\csupe")], category = Rel, comments = "CLOSED SUPERSET OR EQUAL TO"}
  , Record {uchar = '\10963', commands = [("base",""),("unicode","\\subsup")], category = Rel, comments = "SUBSET ABOVE SUPERSET"}
  , Record {uchar = '\10964', commands = [("base",""),("unicode","\\supsub")], category = Rel, comments = "SUPERSET ABOVE SUBSET"}
  , Record {uchar = '\10965', commands = [("base",""),("unicode","\\subsub")], category = Rel, comments = "SUBSET ABOVE SUBSET"}
  , Record {uchar = '\10966', commands = [("base",""),("unicode","\\supsup")], category = Rel, comments = "SUPERSET ABOVE SUPERSET"}
  , Record {uchar = '\10967', commands = [("base",""),("unicode","\\suphsub")], category = Rel, comments = "SUPERSET BESIDE SUBSET"}
  , Record {uchar = '\10968', commands = [("base",""),("unicode","\\supdsub")], category = Rel, comments = "SUPERSET BESIDE AND JOINED BY DASH WITH SUBSET"}
  , Record {uchar = '\10969', commands = [("base",""),("unicode","\\forkv")], category = Rel, comments = "ELEMENT OF OPENING DOWNWARDS"}
  , Record {uchar = '\10970', commands = [("base",""),("unicode","\\topfork")], category = Rel, comments = "PITCHFORK WITH TEE TOP"}
  , Record {uchar = '\10971', commands = [("base",""),("unicode","\\mlcp")], category = Rel, comments = "TRANSVERSAL INTERSECTION"}
  , Record {uchar = '\10972', commands = [("base",""),("unicode","\\forks")], category = Rel, comments = "FORKING"}
  , Record {uchar = '\10973', commands = [("base",""),("unicode","\\forksnot")], category = Rel, comments = "NONFORKING"}
  , Record {uchar = '\10974', commands = [("base",""),("unicode","\\shortlefttack")], category = Rel, comments = "SHORT LEFT TACK"}
  , Record {uchar = '\10975', commands = [("base",""),("unicode","\\shortdowntack")], category = Rel, comments = "SHORT DOWN TACK"}
  , Record {uchar = '\10976', commands = [("base",""),("unicode","\\shortuptack")], category = Rel, comments = "SHORT UP TACK"}
  , Record {uchar = '\10977', commands = [("base",""),("unicode","\\perps")], category = Ord, comments = "PERPENDICULAR WITH S"}
  , Record {uchar = '\10978', commands = [("base",""),("unicode","\\vDdash")], category = Rel, comments = "VERTICAL BAR TRIPLE RIGHT TURNSTILE"}
  , Record {uchar = '\10979', commands = [("base",""),("unicode","\\dashV")], category = Rel, comments = "DOUBLE VERTICAL BAR LEFT TURNSTILE"}
  , Record {uchar = '\10980', commands = [("base",""),("unicode","\\Dashv")], category = Rel, comments = "VERTICAL BAR DOUBLE LEFT TURNSTILE"}
  , Record {uchar = '\10981', commands = [("base",""),("unicode","\\DashV")], category = Rel, comments = "DOUBLE VERTICAL BAR DOUBLE LEFT TURNSTILE"}
  , Record {uchar = '\10982', commands = [("base",""),("unicode","\\varVdash")], category = Rel, comments = "LONG DASH FROM LEFT MEMBER OF DOUBLE VERTICAL"}
  , Record {uchar = '\10983', commands = [("base",""),("unicode","\\Barv")], category = Rel, comments = "SHORT DOWN TACK WITH OVERBAR"}
  , Record {uchar = '\10984', commands = [("base",""),("unicode","\\vBar")], category = Rel, comments = "SHORT UP TACK WITH UNDERBAR"}
  , Record {uchar = '\10985', commands = [("base",""),("unicode","\\vBarv")], category = Rel, comments = "SHORT UP TACK ABOVE SHORT DOWN TACK"}
  , Record {uchar = '\10986', commands = [("txfonts","\\Top"),("unicode","\\barV")], category = Rel, comments = "DOUBLE DOWN TACK"}
  , Record {uchar = '\10987', commands = [("txfonts","\\Bot"),("txfonts","\\Perp"),("unicode","\\Vbar")], category = Rel, comments = "DOUBLE UP TACK"}
  , Record {uchar = '\10988', commands = [("base",""),("unicode","\\Not")], category = Rel, comments = "DOUBLE STROKE NOT SIGN"}
  , Record {uchar = '\10989', commands = [("base",""),("unicode","\\bNot")], category = Rel, comments = "REVERSED DOUBLE STROKE NOT SIGN"}
  , Record {uchar = '\10990', commands = [("base",""),("unicode","\\revnmid")], category = Rel, comments = "DOES NOT DIVIDE WITH REVERSED NEGATION SLASH"}
  , Record {uchar = '\10991', commands = [("base",""),("unicode","\\cirmid")], category = Rel, comments = "VERTICAL LINE WITH CIRCLE ABOVE"}
  , Record {uchar = '\10992', commands = [("base",""),("unicode","\\midcir")], category = Rel, comments = "VERTICAL LINE WITH CIRCLE BELOW"}
  , Record {uchar = '\10993', commands = [("base",""),("unicode","\\topcir")], category = Ord, comments = "DOWN TACK WITH CIRCLE BELOW"}
  , Record {uchar = '\10994', commands = [("base",""),("unicode","\\nhpar")], category = Rel, comments = "PARALLEL WITH HORIZONTAL STROKE"}
  , Record {uchar = '\10995', commands = [("base",""),("unicode","\\parsim")], category = Rel, comments = "PARALLEL WITH TILDE OPERATOR"}
  , Record {uchar = '\10996', commands = [("stmaryrd","\\interleave"),("unicode","\\interleave")], category = Bin, comments = "TRIPLE VERTICAL BAR BINARY RELATION"}
  , Record {uchar = '\10997', commands = [("base",""),("unicode","\\nhVvert")], category = Bin, comments = "TRIPLE VERTICAL BAR WITH HORIZONTAL STROKE"}
  , Record {uchar = '\10998', commands = [("base",""),("unicode","\\threedotcolon")], category = Bin, comments = "TRIPLE COLON OPERATOR"}
  , Record {uchar = '\10999', commands = [("base",""),("unicode","\\lllnest")], category = Rel, comments = "TRIPLE NESTED LESS-THAN"}
  , Record {uchar = '\11000', commands = [("base",""),("unicode","\\gggnest")], category = Rel, comments = "TRIPLE NESTED GREATER-THAN"}
  , Record {uchar = '\11001', commands = [("base",""),("unicode","\\leqqslant")], category = Rel, comments = "DOUBLE-LINE SLANTED LESS-THAN OR EQUAL TO"}
  , Record {uchar = '\11002', commands = [("base",""),("unicode","\\geqqslant")], category = Rel, comments = "DOUBLE-LINE SLANTED GREATER-THAN OR EQUAL TO"}
  , Record {uchar = '\11003', commands = [("base",""),("unicode","\\trslash")], category = Bin, comments = "TRIPLE SOLIDUS BINARY RELATION"}
  , Record {uchar = '\11004', commands = [("stmaryrd","\\biginterleave"),("unicode","\\biginterleave")], category = Op, comments = "LARGE TRIPLE VERTICAL BAR OPERATOR"}
  , Record {uchar = '\11005', commands = [("stmaryrd","\\sslash"),("txfonts","\\varparallel"),("unicode","\\sslash")], category = Bin, comments = "DOUBLE SOLIDUS OPERATOR"}
  , Record {uchar = '\11006', commands = [("stmaryrd","\\talloblong"),("unicode","\\talloblong")], category = Bin, comments = "WHITE VERTICAL BAR"}
  , Record {uchar = '\11007', commands = [("base",""),("unicode","\\bigtalloblong")], category = Op, comments = "N-ARY WHITE VERTICAL BAR"}
  , Record {uchar = '\11008', commands = [("base",""),("unicode","")], category = Ord, comments = "NORTH EAST WHITE ARROW"}
  , Record {uchar = '\11009', commands = [("base",""),("unicode","")], category = Ord, comments = "NORTH WEST WHITE ARROW"}
  , Record {uchar = '\11010', commands = [("base",""),("unicode","")], category = Ord, comments = "SOUTH EAST WHITE ARROW"}
  , Record {uchar = '\11011', commands = [("base",""),("unicode","")], category = Ord, comments = "SOUTH WEST WHITE ARROW"}
  , Record {uchar = '\11012', commands = [("base",""),("unicode","")], category = Ord, comments = "LEFT RIGHT WHITE ARROW"}
  , Record {uchar = '\11013', commands = [("base",""),("unicode","")], category = Ord, comments = "LEFTWARDS BLACK ARROW"}
  , Record {uchar = '\11014', commands = [("base",""),("unicode","")], category = Ord, comments = "UPWARDS BLACK ARROW"}
  , Record {uchar = '\11015', commands = [("base",""),("unicode","")], category = Ord, comments = "DOWNWARDS BLACK ARROW"}
  , Record {uchar = '\11016', commands = [("base",""),("unicode","")], category = Ord, comments = "NORTH EAST BLACK ARROW"}
  , Record {uchar = '\11017', commands = [("base",""),("unicode","")], category = Ord, comments = "NORTH WEST BLACK ARROW"}
  , Record {uchar = '\11018', commands = [("base",""),("unicode","")], category = Ord, comments = "SOUTH EAST BLACK ARROW"}
  , Record {uchar = '\11019', commands = [("base",""),("unicode","")], category = Ord, comments = "SOUTH WEST BLACK ARROW"}
  , Record {uchar = '\11020', commands = [("base",""),("unicode","")], category = Ord, comments = "LEFT RIGHT BLACK ARROW"}
  , Record {uchar = '\11021', commands = [("base",""),("unicode","")], category = Ord, comments = "UP DOWN BLACK ARROW"}
  , Record {uchar = '\11022', commands = [("base",""),("unicode","")], category = Ord, comments = "RIGHTWARDS ARROW WITH TIP DOWNWARDS"}
  , Record {uchar = '\11023', commands = [("base",""),("unicode","")], category = Ord, comments = "RIGHTWARDS ARROW WITH TIP UPWARDS"}
  , Record {uchar = '\11024', commands = [("base",""),("unicode","")], category = Ord, comments = "LEFTWARDS ARROW WITH TIP DOWNWARDS"}
  , Record {uchar = '\11025', commands = [("base",""),("unicode","")], category = Ord, comments = "LEFTWARDS ARROW WITH TIP UPWARDS"}
  , Record {uchar = '\11026', commands = [("base",""),("unicode","\\squaretopblack")], category = Ord, comments = "SQUARE WITH TOP HALF BLACK"}
  , Record {uchar = '\11027', commands = [("base",""),("unicode","\\squarebotblack")], category = Ord, comments = "SQUARE WITH BOTTOM HALF BLACK"}
  , Record {uchar = '\11028', commands = [("base",""),("unicode","\\squareurblack")], category = Ord, comments = "SQUARE WITH UPPER RIGHT DIAGONAL HALF BLACK"}
  , Record {uchar = '\11029', commands = [("base",""),("unicode","\\squarellblack")], category = Ord, comments = "SQUARE WITH LOWER LEFT DIAGONAL HALF BLACK"}
  , Record {uchar = '\11030', commands = [("base",""),("unicode","\\diamondleftblack")], category = Ord, comments = "DIAMOND WITH LEFT HALF BLACK"}
  , Record {uchar = '\11031', commands = [("base",""),("unicode","\\diamondrightblack")], category = Ord, comments = "DIAMOND WITH RIGHT HALF BLACK"}
  , Record {uchar = '\11032', commands = [("base",""),("unicode","\\diamondtopblack")], category = Ord, comments = "DIAMOND WITH TOP HALF BLACK"}
  , Record {uchar = '\11033', commands = [("base",""),("unicode","\\diamondbotblack")], category = Ord, comments = "DIAMOND WITH BOTTOM HALF BLACK"}
  , Record {uchar = '\11034', commands = [("base",""),("unicode","\\dottedsquare")], category = Ord, comments = "DOTTED SQUARE"}
  , Record {uchar = '\11035', commands = [("fourier","\\blacksquare"),("unicode","\\lgblksquare")], category = Ord, comments = "BLACK LARGE SQUARE"}
  , Record {uchar = '\11036', commands = [("fourier","\\square"),("unicode","\\lgwhtsquare")], category = Ord, comments = "WHITE LARGE SQUARE"}
  , Record {uchar = '\11037', commands = [("base",""),("amssymb","\\centerdot"),("unicode","\\vysmblksquare")], category = Ord, comments = "t \\Squaredot (marvosym), BLACK VERY SMALL SQUARE"}
  , Record {uchar = '\11038', commands = [("base",""),("unicode","\\vysmwhtsquare")], category = Ord, comments = "WHITE VERY SMALL SQUARE"}
  , Record {uchar = '\11039', commands = [("base",""),("unicode","\\pentagonblack")], category = Ord, comments = "BLACK PENTAGON"}
  , Record {uchar = '\11040', commands = [("base",""),("unicode","\\pentagon")], category = Ord, comments = "WHITE PENTAGON"}
  , Record {uchar = '\11041', commands = [("base",""),("unicode","\\varhexagon")], category = Ord, comments = "WHITE HEXAGON"}
  , Record {uchar = '\11042', commands = [("base",""),("unicode","\\varhexagonblack")], category = Ord, comments = "BLACK HEXAGON"}
  , Record {uchar = '\11043', commands = [("base",""),("unicode","\\hexagonblack")], category = Ord, comments = "HORIZONTAL BLACK HEXAGON"}
  , Record {uchar = '\11044', commands = [("base",""),("unicode","\\lgblkcircle")], category = Ord, comments = "BLACK LARGE CIRCLE"}
  , Record {uchar = '\11045', commands = [("base",""),("unicode","\\mdblkdiamond")], category = Ord, comments = "BLACK MEDIUM DIAMOND"}
  , Record {uchar = '\11046', commands = [("base",""),("unicode","\\mdwhtdiamond")], category = Ord, comments = "WHITE MEDIUM DIAMOND"}
  , Record {uchar = '\11047', commands = [("base",""),("amssymb","\\blacklozenge"),("unicode","\\mdblklozenge")], category = Ord, comments = "BLACK MEDIUM LOZENGE"}
  , Record {uchar = '\11048', commands = [("base",""),("amssymb","\\lozenge"),("unicode","\\mdwhtlozenge")], category = Ord, comments = "WHITE MEDIUM LOZENGE"}
  , Record {uchar = '\11049', commands = [("base",""),("unicode","\\smblkdiamond")], category = Ord, comments = "BLACK SMALL DIAMOND"}
  , Record {uchar = '\11050', commands = [("base",""),("unicode","\\smblklozenge")], category = Ord, comments = "BLACK SMALL LOZENGE"}
  , Record {uchar = '\11051', commands = [("base",""),("unicode","\\smwhtlozenge")], category = Ord, comments = "WHITE SMALL LOZENGE"}
  , Record {uchar = '\11052', commands = [("base",""),("unicode","\\blkhorzoval")], category = Ord, comments = "BLACK HORIZONTAL ELLIPSE"}
  , Record {uchar = '\11053', commands = [("base",""),("unicode","\\whthorzoval")], category = Ord, comments = "WHITE HORIZONTAL ELLIPSE"}
  , Record {uchar = '\11054', commands = [("base",""),("unicode","\\blkvertoval")], category = Ord, comments = "BLACK VERTICAL ELLIPSE"}
  , Record {uchar = '\11055', commands = [("base",""),("unicode","\\whtvertoval")], category = Ord, comments = "WHITE VERTICAL ELLIPSE"}
  , Record {uchar = '\11056', commands = [("base",""),("unicode","\\circleonleftarrow")], category = Rel, comments = "LEFT ARROW WITH SMALL CIRCLE"}
  , Record {uchar = '\11057', commands = [("base",""),("unicode","\\leftthreearrows")], category = Rel, comments = "THREE LEFTWARDS ARROWS"}
  , Record {uchar = '\11058', commands = [("base",""),("unicode","\\leftarrowonoplus")], category = Rel, comments = "LEFT ARROW WITH CIRCLED PLUS"}
  , Record {uchar = '\11059', commands = [("base",""),("unicode","\\longleftsquigarrow")], category = Rel, comments = "LONG LEFTWARDS SQUIGGLE ARROW"}
  , Record {uchar = '\11060', commands = [("base",""),("unicode","\\nvtwoheadleftarrow")], category = Rel, comments = "LEFTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE"}
  , Record {uchar = '\11061', commands = [("base",""),("unicode","\\nVtwoheadleftarrow")], category = Rel, comments = "LEFTWARDS TWO-HEADED ARROW WITH DOUBLE VERTICAL STROKE"}
  , Record {uchar = '\11062', commands = [("base",""),("unicode","\\twoheadmapsfrom")], category = Rel, comments = "LEFTWARDS TWO-HEADED ARROW FROM BAR"}
  , Record {uchar = '\11063', commands = [("base",""),("unicode","\\twoheadleftdbkarrow")], category = Rel, comments = "leftwards two-headed triple-dash arrow"}
  , Record {uchar = '\11064', commands = [("base",""),("unicode","\\leftdotarrow")], category = Rel, comments = "LEFTWARDS ARROW WITH DOTTED STEM"}
  , Record {uchar = '\11065', commands = [("base",""),("unicode","\\nvleftarrowtail")], category = Rel, comments = "LEFTWARDS ARROW WITH TAIL WITH VERTICAL STROKE"}
  , Record {uchar = '\11066', commands = [("base",""),("unicode","\\nVleftarrowtail")], category = Rel, comments = "LEFTWARDS ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE"}
  , Record {uchar = '\11067', commands = [("base",""),("unicode","\\twoheadleftarrowtail")], category = Rel, comments = "LEFTWARDS TWO-HEADED ARROW WITH TAIL"}
  , Record {uchar = '\11068', commands = [("base",""),("unicode","\\nvtwoheadleftarrowtail")], category = Rel, comments = "LEFTWARDS TWO-HEADED ARROW WITH TAIL WITH VERTICAL STROKE"}
  , Record {uchar = '\11069', commands = [("base",""),("unicode","\\nVtwoheadleftarrowtail")], category = Rel, comments = "LEFTWARDS TWO-HEADED ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE"}
  , Record {uchar = '\11070', commands = [("base",""),("unicode","\\leftarrowx")], category = Rel, comments = "LEFTWARDS ARROW THROUGH X"}
  , Record {uchar = '\11071', commands = [("base",""),("unicode","\\leftcurvedarrow")], category = Rel, comments = "WAVE ARROW POINTING DIRECTLY LEFT"}
  , Record {uchar = '\11072', commands = [("base",""),("unicode","\\equalleftarrow")], category = Rel, comments = "EQUALS SIGN ABOVE LEFTWARDS ARROW"}
  , Record {uchar = '\11073', commands = [("base",""),("unicode","\\bsimilarleftarrow")], category = Rel, comments = "REVERSE TILDE OPERATOR ABOVE LEFTWARDS ARROW"}
  , Record {uchar = '\11074', commands = [("base",""),("unicode","\\leftarrowbackapprox")], category = Rel, comments = "LEFTWARDS ARROW ABOVE REVERSE ALMOST EQUAL TO"}
  , Record {uchar = '\11075', commands = [("base",""),("unicode","\\rightarrowgtr")], category = Rel, comments = "rightwards arrow through less-than"}
  , Record {uchar = '\11076', commands = [("base",""),("unicode","\\rightarrowsupset")], category = Rel, comments = "rightwards arrow through subset"}
  , Record {uchar = '\11077', commands = [("base",""),("unicode","\\LLeftarrow")], category = Rel, comments = "LEFTWARDS QUADRUPLE ARROW"}
  , Record {uchar = '\11078', commands = [("base",""),("unicode","\\RRightarrow")], category = Rel, comments = "RIGHTWARDS QUADRUPLE ARROW"}
  , Record {uchar = '\11079', commands = [("base",""),("unicode","\\bsimilarrightarrow")], category = Rel, comments = "REVERSE TILDE OPERATOR ABOVE RIGHTWARDS ARROW"}
  , Record {uchar = '\11080', commands = [("base",""),("unicode","\\rightarrowbackapprox")], category = Rel, comments = "RIGHTWARDS ARROW ABOVE REVERSE ALMOST EQUAL TO"}
  , Record {uchar = '\11081', commands = [("base",""),("unicode","\\similarleftarrow")], category = Rel, comments = "TILDE OPERATOR ABOVE LEFTWARDS ARROW"}
  , Record {uchar = '\11082', commands = [("base",""),("unicode","\\leftarrowapprox")], category = Rel, comments = "LEFTWARDS ARROW ABOVE ALMOST EQUAL TO"}
  , Record {uchar = '\11083', commands = [("base",""),("unicode","\\leftarrowbsimilar")], category = Rel, comments = "LEFTWARDS ARROW ABOVE REVERSE TILDE OPERATOR"}
  , Record {uchar = '\11084', commands = [("base",""),("unicode","\\rightarrowbsimilar")], category = Rel, comments = "righttwards arrow above reverse tilde operator"}
  , Record {uchar = '\11088', commands = [("base",""),("unicode","\\medwhitestar")], category = Ord, comments = "WHITE MEDIUM STAR"}
  , Record {uchar = '\11089', commands = [("base",""),("unicode","\\medblackstar")], category = Ord, comments = "black medium star"}
  , Record {uchar = '\11090', commands = [("base",""),("unicode","\\smwhitestar")], category = Ord, comments = "WHITE SMALL STAR"}
  , Record {uchar = '\11091', commands = [("base",""),("unicode","\\rightpentagonblack")], category = Ord, comments = "BLACK RIGHT-POINTING PENTAGON"}
  , Record {uchar = '\11092', commands = [("base",""),("unicode","\\rightpentagon")], category = Ord, comments = "WHITE RIGHT-POINTING PENTAGON"}
  , Record {uchar = '\12296', commands = [("base",""),("base","\\langle"),("unicode","")], category = Open, comments = "LEFT ANGLE BRACKET (deprecated for math use)"}
  , Record {uchar = '\12297', commands = [("base",""),("base","\\rangle"),("unicode","")], category = Close, comments = "RIGHT ANGLE BRACKET (deprecated for math use)"}
  , Record {uchar = '\12306', commands = [("base",""),("unicode","\\postalmark")], category = Ord, comments = "POSTAL MARK"}
  , Record {uchar = '\12308', commands = [("base",""),("unicode","\\lbrbrak")], category = Open, comments = "left broken bracket"}
  , Record {uchar = '\12309', commands = [("base",""),("unicode","\\rbrbrak")], category = Close, comments = "right broken bracket"}
  , Record {uchar = '\12312', commands = [("base",""),("unicode","\\Lbrbrak")], category = Open, comments = "LEFT WHITE TORTOISE SHELL BRACKET"}
  , Record {uchar = '\12313', commands = [("base",""),("unicode","\\Rbrbrak")], category = Close, comments = "RIGHT WHITE TORTOISE SHELL BRACKET"}
  , Record {uchar = '\12314', commands = [("base",""),("stmaryrd","\\llbracket"),("unicode","")], category = Open, comments = "LEFT WHITE SQUARE BRACKET (deprecated for math use)"}
  , Record {uchar = '\12315', commands = [("base",""),("stmaryrd","\\rrbracket"),("unicode","")], category = Close, comments = "RIGHT WHITE SQUARE BRACKET (deprecated for math use)"}
  , Record {uchar = '\12336', commands = [("base",""),("unicode","\\hzigzag")], category = Ord, comments = "zigzag"}
  , Record {uchar = '\12398', commands = [("base",""),("unicode","")], category = Alpha, comments = "HIRAGANA LETTER NO"}
  , Record {uchar = '\64297', commands = [("base",""),("unicode","")], category = Ord, comments = "HEBREW LETTER ALTERNATIVE PLUS SIGN (doesn't have cross shape)"}
  , Record {uchar = '\65024', commands = [("base",""),("unicode","")], category = Accent, comments = "VARIATION SELECTOR-1"}
  , Record {uchar = '\65121', commands = [("base",""),("unicode","")], category = Ord, comments = "SMALL ASTERISK"}
  , Record {uchar = '\65122', commands = [("base",""),("unicode","")], category = Ord, comments = "SMALL PLUS SIGN"}
  , Record {uchar = '\65123', commands = [("base",""),("unicode","")], category = Ord, comments = "SMALL HYPHEN-MINUS"}
  , Record {uchar = '\65124', commands = [("base",""),("unicode","")], category = Ord, comments = "SMALL LESS-THAN SIGN"}
  , Record {uchar = '\65125', commands = [("base",""),("unicode","")], category = Ord, comments = "SMALL GREATER-THAN SIGN"}
  , Record {uchar = '\65126', commands = [("base",""),("unicode","")], category = Ord, comments = "SMALL EQUALS SIGN"}
  , Record {uchar = '\65128', commands = [("base",""),("unicode","")], category = Ord, comments = "SMALL REVERSE SOLIDUS"}
  , Record {uchar = '\65291', commands = [("base",""),("unicode","")], category = Ord, comments = "FULLWIDTH PLUS SIGN"}
  , Record {uchar = '\65308', commands = [("base",""),("unicode","")], category = Ord, comments = "FULLWIDTH LESS-THAN SIGN"}
  , Record {uchar = '\65309', commands = [("base",""),("unicode","")], category = Ord, comments = "FULLWIDTH EQUALS SIGN"}
  , Record {uchar = '\65310', commands = [("base",""),("unicode","")], category = Ord, comments = "FULLWIDTH GREATER-THAN SIGN"}
  , Record {uchar = '\65340', commands = [("base",""),("unicode","")], category = Ord, comments = "FULLWIDTH REVERSE SOLIDUS"}
  , Record {uchar = '\65342', commands = [("base",""),("unicode","")], category = Ord, comments = "FULLWIDTH CIRCUMFLEX ACCENT"}
  , Record {uchar = '\65372', commands = [("base",""),("unicode","")], category = Ord, comments = "FULLWIDTH VERTICAL LINE"}
  , Record {uchar = '\65374', commands = [("base",""),("unicode","")], category = Ord, comments = "FULLWIDTH TILDE"}
  , Record {uchar = '\65506', commands = [("base",""),("unicode","")], category = Ord, comments = "FULLWIDTH NOT SIGN"}
  , Record {uchar = '\65513', commands = [("base",""),("unicode","")], category = Ord, comments = "HALFWIDTH LEFTWARDS ARROW"}
  , Record {uchar = '\65514', commands = [("base",""),("unicode","")], category = Ord, comments = "HALFWIDTH UPWARDS ARROW"}
  , Record {uchar = '\65515', commands = [("base",""),("unicode","")], category = Ord, comments = "HALFWIDTH RIGHTWARDS ARROW"}
  , Record {uchar = '\65516', commands = [("base",""),("unicode","")], category = Ord, comments = "HALFWIDTH DOWNWARDS ARROW"}
  , Record {uchar = '\119808', commands = [("base","\\mathbf{A}"),("unicode","\\mbfA")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL A"}
  , Record {uchar = '\119809', commands = [("base","\\mathbf{B}"),("unicode","\\mbfB")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL B"}
  , Record {uchar = '\119810', commands = [("base","\\mathbf{C}"),("unicode","\\mbfC")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL C"}
  , Record {uchar = '\119811', commands = [("base","\\mathbf{D}"),("unicode","\\mbfD")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL D"}
  , Record {uchar = '\119812', commands = [("base","\\mathbf{E}"),("unicode","\\mbfE")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL E"}
  , Record {uchar = '\119813', commands = [("base","\\mathbf{F}"),("unicode","\\mbfF")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL F"}
  , Record {uchar = '\119814', commands = [("base","\\mathbf{G}"),("unicode","\\mbfG")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL G"}
  , Record {uchar = '\119815', commands = [("base","\\mathbf{H}"),("unicode","\\mbfH")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL H"}
  , Record {uchar = '\119816', commands = [("base","\\mathbf{I}"),("unicode","\\mbfI")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL I"}
  , Record {uchar = '\119817', commands = [("base","\\mathbf{J}"),("unicode","\\mbfJ")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL J"}
  , Record {uchar = '\119818', commands = [("base","\\mathbf{K}"),("unicode","\\mbfK")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL K"}
  , Record {uchar = '\119819', commands = [("base","\\mathbf{L}"),("unicode","\\mbfL")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL L"}
  , Record {uchar = '\119820', commands = [("base","\\mathbf{M}"),("unicode","\\mbfM")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL M"}
  , Record {uchar = '\119821', commands = [("base","\\mathbf{N}"),("unicode","\\mbfN")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL N"}
  , Record {uchar = '\119822', commands = [("base","\\mathbf{O}"),("unicode","\\mbfO")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL O"}
  , Record {uchar = '\119823', commands = [("base","\\mathbf{P}"),("unicode","\\mbfP")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL P"}
  , Record {uchar = '\119824', commands = [("base","\\mathbf{Q}"),("unicode","\\mbfQ")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL Q"}
  , Record {uchar = '\119825', commands = [("base","\\mathbf{R}"),("unicode","\\mbfR")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL R"}
  , Record {uchar = '\119826', commands = [("base","\\mathbf{S}"),("unicode","\\mbfS")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL S"}
  , Record {uchar = '\119827', commands = [("base","\\mathbf{T}"),("unicode","\\mbfT")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL T"}
  , Record {uchar = '\119828', commands = [("base","\\mathbf{U}"),("unicode","\\mbfU")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL U"}
  , Record {uchar = '\119829', commands = [("base","\\mathbf{V}"),("unicode","\\mbfV")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL V"}
  , Record {uchar = '\119830', commands = [("base","\\mathbf{W}"),("unicode","\\mbfW")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL W"}
  , Record {uchar = '\119831', commands = [("base","\\mathbf{X}"),("unicode","\\mbfX")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL X"}
  , Record {uchar = '\119832', commands = [("base","\\mathbf{Y}"),("unicode","\\mbfY")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL Y"}
  , Record {uchar = '\119833', commands = [("base","\\mathbf{Z}"),("unicode","\\mbfZ")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL Z"}
  , Record {uchar = '\119834', commands = [("base","\\mathbf{a}"),("unicode","\\mbfa")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL A"}
  , Record {uchar = '\119835', commands = [("base","\\mathbf{b}"),("unicode","\\mbfb")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL B"}
  , Record {uchar = '\119836', commands = [("base","\\mathbf{c}"),("unicode","\\mbfc")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL C"}
  , Record {uchar = '\119837', commands = [("base","\\mathbf{d}"),("unicode","\\mbfd")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL D"}
  , Record {uchar = '\119838', commands = [("base","\\mathbf{e}"),("unicode","\\mbfe")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL E"}
  , Record {uchar = '\119839', commands = [("base","\\mathbf{f}"),("unicode","\\mbff")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL F"}
  , Record {uchar = '\119840', commands = [("base","\\mathbf{g}"),("unicode","\\mbfg")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL G"}
  , Record {uchar = '\119841', commands = [("base","\\mathbf{h}"),("unicode","\\mbfh")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL H"}
  , Record {uchar = '\119842', commands = [("base","\\mathbf{i}"),("unicode","\\mbfi")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL I"}
  , Record {uchar = '\119843', commands = [("base","\\mathbf{j}"),("unicode","\\mbfj")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL J"}
  , Record {uchar = '\119844', commands = [("base","\\mathbf{k}"),("unicode","\\mbfk")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL K"}
  , Record {uchar = '\119845', commands = [("base","\\mathbf{l}"),("unicode","\\mbfl")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL L"}
  , Record {uchar = '\119846', commands = [("base","\\mathbf{m}"),("unicode","\\mbfm")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL M"}
  , Record {uchar = '\119847', commands = [("base","\\mathbf{n}"),("unicode","\\mbfn")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL N"}
  , Record {uchar = '\119848', commands = [("base","\\mathbf{o}"),("unicode","\\mbfo")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL O"}
  , Record {uchar = '\119849', commands = [("base","\\mathbf{p}"),("unicode","\\mbfp")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL P"}
  , Record {uchar = '\119850', commands = [("base","\\mathbf{q}"),("unicode","\\mbfq")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL Q"}
  , Record {uchar = '\119851', commands = [("base","\\mathbf{r}"),("unicode","\\mbfr")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL R"}
  , Record {uchar = '\119852', commands = [("base","\\mathbf{s}"),("unicode","\\mbfs")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL S"}
  , Record {uchar = '\119853', commands = [("base","\\mathbf{t}"),("unicode","\\mbft")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL T"}
  , Record {uchar = '\119854', commands = [("base","\\mathbf{u}"),("unicode","\\mbfu")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL U"}
  , Record {uchar = '\119855', commands = [("base","\\mathbf{v}"),("unicode","\\mbfv")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL V"}
  , Record {uchar = '\119856', commands = [("base","\\mathbf{w}"),("unicode","\\mbfw")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL W"}
  , Record {uchar = '\119857', commands = [("base","\\mathbf{x}"),("unicode","\\mbfx")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL X"}
  , Record {uchar = '\119858', commands = [("base","\\mathbf{y}"),("unicode","\\mbfy")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL Y"}
  , Record {uchar = '\119859', commands = [("base","\\mathbf{z}"),("unicode","\\mbfz")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL Z"}
  , Record {uchar = '\119860', commands = [("base","A"),("base","\\mathit{A}"),("unicode","\\mitA")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL A"}
  , Record {uchar = '\119861', commands = [("base","B"),("base","\\mathit{B}"),("unicode","\\mitB")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL B"}
  , Record {uchar = '\119862', commands = [("base","C"),("base","\\mathit{C}"),("unicode","\\mitC")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL C"}
  , Record {uchar = '\119863', commands = [("base","D"),("base","\\mathit{D}"),("unicode","\\mitD")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL D"}
  , Record {uchar = '\119864', commands = [("base","E"),("base","\\mathit{E}"),("unicode","\\mitE")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL E"}
  , Record {uchar = '\119865', commands = [("base","F"),("base","\\mathit{F}"),("unicode","\\mitF")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL F"}
  , Record {uchar = '\119866', commands = [("base","G"),("base","\\mathit{G}"),("unicode","\\mitG")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL G"}
  , Record {uchar = '\119867', commands = [("base","H"),("base","\\mathit{H}"),("unicode","\\mitH")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL H"}
  , Record {uchar = '\119868', commands = [("base","I"),("base","\\mathit{I}"),("unicode","\\mitI")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL I"}
  , Record {uchar = '\119869', commands = [("base","J"),("base","\\mathit{J}"),("unicode","\\mitJ")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL J"}
  , Record {uchar = '\119870', commands = [("base","K"),("base","\\mathit{K}"),("unicode","\\mitK")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL K"}
  , Record {uchar = '\119871', commands = [("base","L"),("base","\\mathit{L}"),("unicode","\\mitL")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL L"}
  , Record {uchar = '\119872', commands = [("base","M"),("base","\\mathit{M}"),("unicode","\\mitM")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL M"}
  , Record {uchar = '\119873', commands = [("base","N"),("base","\\mathit{N}"),("unicode","\\mitN")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL N"}
  , Record {uchar = '\119874', commands = [("base","O"),("base","\\mathit{O}"),("unicode","\\mitO")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL O"}
  , Record {uchar = '\119875', commands = [("base","P"),("base","\\mathit{P}"),("unicode","\\mitP")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL P"}
  , Record {uchar = '\119876', commands = [("base","Q"),("base","\\mathit{Q}"),("unicode","\\mitQ")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL Q"}
  , Record {uchar = '\119877', commands = [("base","R"),("base","\\mathit{R}"),("unicode","\\mitR")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL R"}
  , Record {uchar = '\119878', commands = [("base","S"),("base","\\mathit{S}"),("unicode","\\mitS")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL S"}
  , Record {uchar = '\119879', commands = [("base","T"),("base","\\mathit{T}"),("unicode","\\mitT")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL T"}
  , Record {uchar = '\119880', commands = [("base","U"),("base","\\mathit{U}"),("unicode","\\mitU")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL U"}
  , Record {uchar = '\119881', commands = [("base","V"),("base","\\mathit{V}"),("unicode","\\mitV")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL V"}
  , Record {uchar = '\119882', commands = [("base","W"),("base","\\mathit{W}"),("unicode","\\mitW")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL W"}
  , Record {uchar = '\119883', commands = [("base","X"),("base","\\mathit{X}"),("unicode","\\mitX")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL X"}
  , Record {uchar = '\119884', commands = [("base","Y"),("base","\\mathit{Y}"),("unicode","\\mitY")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL Y"}
  , Record {uchar = '\119885', commands = [("base","Z"),("base","\\mathit{Z}"),("unicode","\\mitZ")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL Z"}
  , Record {uchar = '\119886', commands = [("base","a"),("base","\\mathit{a}"),("unicode","\\mita")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL A"}
  , Record {uchar = '\119887', commands = [("base","b"),("base","\\mathit{b}"),("unicode","\\mitb")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL B"}
  , Record {uchar = '\119888', commands = [("base","c"),("base","\\mathit{c}"),("unicode","\\mitc")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL C"}
  , Record {uchar = '\119889', commands = [("base","d"),("base","\\mathit{d}"),("unicode","\\mitd")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL D"}
  , Record {uchar = '\119890', commands = [("base","e"),("base","\\mathit{e}"),("unicode","\\mite")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL E"}
  , Record {uchar = '\119891', commands = [("base","f"),("base","\\mathit{f}"),("unicode","\\mitf")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL F"}
  , Record {uchar = '\119892', commands = [("base","g"),("base","\\mathit{g}"),("unicode","\\mitg")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL G"}
  , Record {uchar = '\119894', commands = [("base","i"),("base","\\mathit{i}"),("unicode","\\miti")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL I"}
  , Record {uchar = '\119895', commands = [("base","j"),("base","\\mathit{j}"),("unicode","\\mitj")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL J"}
  , Record {uchar = '\119896', commands = [("base","k"),("base","\\mathit{k}"),("unicode","\\mitk")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL K"}
  , Record {uchar = '\119897', commands = [("base","l"),("base","\\mathit{l}"),("unicode","\\mitl")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL L"}
  , Record {uchar = '\119898', commands = [("base","m"),("base","\\mathit{m}"),("unicode","\\mitm")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL M"}
  , Record {uchar = '\119899', commands = [("base","n"),("base","\\mathit{n}"),("unicode","\\mitn")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL N"}
  , Record {uchar = '\119900', commands = [("base","o"),("base","\\mathit{o}"),("unicode","\\mito")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL O"}
  , Record {uchar = '\119901', commands = [("base","p"),("base","\\mathit{p}"),("unicode","\\mitp")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL P"}
  , Record {uchar = '\119902', commands = [("base","q"),("base","\\mathit{q}"),("unicode","\\mitq")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL Q"}
  , Record {uchar = '\119903', commands = [("base","r"),("base","\\mathit{r}"),("unicode","\\mitr")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL R"}
  , Record {uchar = '\119904', commands = [("base","s"),("base","\\mathit{s}"),("unicode","\\mits")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL S"}
  , Record {uchar = '\119905', commands = [("base","t"),("base","\\mathit{t}"),("unicode","\\mitt")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL T"}
  , Record {uchar = '\119906', commands = [("base","u"),("base","\\mathit{u}"),("unicode","\\mitu")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL U"}
  , Record {uchar = '\119907', commands = [("base","v"),("base","\\mathit{v}"),("unicode","\\mitv")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL V"}
  , Record {uchar = '\119908', commands = [("base","w"),("base","\\mathit{w}"),("unicode","\\mitw")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL W"}
  , Record {uchar = '\119909', commands = [("base","x"),("base","\\mathit{x}"),("unicode","\\mitx")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL X"}
  , Record {uchar = '\119910', commands = [("base","y"),("base","\\mathit{y}"),("unicode","\\mity")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL Y"}
  , Record {uchar = '\119911', commands = [("base","z"),("base","\\mathit{z}"),("unicode","\\mitz")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL Z"}
  , Record {uchar = '\119912', commands = [("isomath","\\mathbfit{A}"),("fixmath","\\mathbold{A}"),("unicode","\\mbfitA")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL A"}
  , Record {uchar = '\119913', commands = [("isomath","\\mathbfit{B}"),("fixmath","\\mathbold{B}"),("unicode","\\mbfitB")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL B"}
  , Record {uchar = '\119914', commands = [("isomath","\\mathbfit{C}"),("fixmath","\\mathbold{C}"),("unicode","\\mbfitC")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL C"}
  , Record {uchar = '\119915', commands = [("isomath","\\mathbfit{D}"),("fixmath","\\mathbold{D}"),("unicode","\\mbfitD")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL D"}
  , Record {uchar = '\119916', commands = [("isomath","\\mathbfit{E}"),("fixmath","\\mathbold{E}"),("unicode","\\mbfitE")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL E"}
  , Record {uchar = '\119917', commands = [("isomath","\\mathbfit{F}"),("fixmath","\\mathbold{F}"),("unicode","\\mbfitF")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL F"}
  , Record {uchar = '\119918', commands = [("isomath","\\mathbfit{G}"),("fixmath","\\mathbold{G}"),("unicode","\\mbfitG")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL G"}
  , Record {uchar = '\119919', commands = [("isomath","\\mathbfit{H}"),("fixmath","\\mathbold{H}"),("unicode","\\mbfitH")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL H"}
  , Record {uchar = '\119920', commands = [("isomath","\\mathbfit{I}"),("fixmath","\\mathbold{I}"),("unicode","\\mbfitI")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL I"}
  , Record {uchar = '\119921', commands = [("isomath","\\mathbfit{J}"),("fixmath","\\mathbold{J}"),("unicode","\\mbfitJ")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL J"}
  , Record {uchar = '\119922', commands = [("isomath","\\mathbfit{K}"),("fixmath","\\mathbold{K}"),("unicode","\\mbfitK")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL K"}
  , Record {uchar = '\119923', commands = [("isomath","\\mathbfit{L}"),("fixmath","\\mathbold{L}"),("unicode","\\mbfitL")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL L"}
  , Record {uchar = '\119924', commands = [("isomath","\\mathbfit{M}"),("fixmath","\\mathbold{M}"),("unicode","\\mbfitM")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL M"}
  , Record {uchar = '\119925', commands = [("isomath","\\mathbfit{N}"),("fixmath","\\mathbold{N}"),("unicode","\\mbfitN")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL N"}
  , Record {uchar = '\119926', commands = [("isomath","\\mathbfit{O}"),("fixmath","\\mathbold{O}"),("unicode","\\mbfitO")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL O"}
  , Record {uchar = '\119927', commands = [("isomath","\\mathbfit{P}"),("fixmath","\\mathbold{P}"),("unicode","\\mbfitP")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL P"}
  , Record {uchar = '\119928', commands = [("isomath","\\mathbfit{Q}"),("fixmath","\\mathbold{Q}"),("unicode","\\mbfitQ")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL Q"}
  , Record {uchar = '\119929', commands = [("isomath","\\mathbfit{R}"),("fixmath","\\mathbold{R}"),("unicode","\\mbfitR")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL R"}
  , Record {uchar = '\119930', commands = [("isomath","\\mathbfit{S}"),("fixmath","\\mathbold{S}"),("unicode","\\mbfitS")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL S"}
  , Record {uchar = '\119931', commands = [("isomath","\\mathbfit{T}"),("fixmath","\\mathbold{T}"),("unicode","\\mbfitT")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL T"}
  , Record {uchar = '\119932', commands = [("isomath","\\mathbfit{U}"),("fixmath","\\mathbold{U}"),("unicode","\\mbfitU")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL U"}
  , Record {uchar = '\119933', commands = [("isomath","\\mathbfit{V}"),("fixmath","\\mathbold{V}"),("unicode","\\mbfitV")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL V"}
  , Record {uchar = '\119934', commands = [("isomath","\\mathbfit{W}"),("fixmath","\\mathbold{W}"),("unicode","\\mbfitW")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL W"}
  , Record {uchar = '\119935', commands = [("isomath","\\mathbfit{X}"),("fixmath","\\mathbold{X}"),("unicode","\\mbfitX")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL X"}
  , Record {uchar = '\119936', commands = [("isomath","\\mathbfit{Y}"),("fixmath","\\mathbold{Y}"),("unicode","\\mbfitY")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL Y"}
  , Record {uchar = '\119937', commands = [("isomath","\\mathbfit{Z}"),("fixmath","\\mathbold{Z}"),("unicode","\\mbfitZ")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL Z"}
  , Record {uchar = '\119938', commands = [("isomath","\\mathbfit{a}"),("fixmath","\\mathbold{a}"),("unicode","\\mbfita")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL A"}
  , Record {uchar = '\119939', commands = [("isomath","\\mathbfit{b}"),("fixmath","\\mathbold{b}"),("unicode","\\mbfitb")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL B"}
  , Record {uchar = '\119940', commands = [("isomath","\\mathbfit{c}"),("fixmath","\\mathbold{c}"),("unicode","\\mbfitc")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL C"}
  , Record {uchar = '\119941', commands = [("isomath","\\mathbfit{d}"),("fixmath","\\mathbold{d}"),("unicode","\\mbfitd")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL D"}
  , Record {uchar = '\119942', commands = [("isomath","\\mathbfit{e}"),("fixmath","\\mathbold{e}"),("unicode","\\mbfite")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL E"}
  , Record {uchar = '\119943', commands = [("isomath","\\mathbfit{f}"),("fixmath","\\mathbold{f}"),("unicode","\\mbfitf")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL F"}
  , Record {uchar = '\119944', commands = [("isomath","\\mathbfit{g}"),("fixmath","\\mathbold{g}"),("unicode","\\mbfitg")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL G"}
  , Record {uchar = '\119945', commands = [("isomath","\\mathbfit{h}"),("fixmath","\\mathbold{h}"),("unicode","\\mbfith")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL H"}
  , Record {uchar = '\119946', commands = [("isomath","\\mathbfit{i}"),("fixmath","\\mathbold{i}"),("unicode","\\mbfiti")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL I"}
  , Record {uchar = '\119947', commands = [("isomath","\\mathbfit{j}"),("fixmath","\\mathbold{j}"),("unicode","\\mbfitj")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL J"}
  , Record {uchar = '\119948', commands = [("isomath","\\mathbfit{k}"),("fixmath","\\mathbold{k}"),("unicode","\\mbfitk")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL K"}
  , Record {uchar = '\119949', commands = [("isomath","\\mathbfit{l}"),("fixmath","\\mathbold{l}"),("unicode","\\mbfitl")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL L"}
  , Record {uchar = '\119950', commands = [("isomath","\\mathbfit{m}"),("fixmath","\\mathbold{m}"),("unicode","\\mbfitm")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL M"}
  , Record {uchar = '\119951', commands = [("isomath","\\mathbfit{n}"),("fixmath","\\mathbold{n}"),("unicode","\\mbfitn")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL N"}
  , Record {uchar = '\119952', commands = [("isomath","\\mathbfit{o}"),("fixmath","\\mathbold{o}"),("unicode","\\mbfito")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL O"}
  , Record {uchar = '\119953', commands = [("isomath","\\mathbfit{p}"),("fixmath","\\mathbold{p}"),("unicode","\\mbfitp")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL P"}
  , Record {uchar = '\119954', commands = [("isomath","\\mathbfit{q}"),("fixmath","\\mathbold{q}"),("unicode","\\mbfitq")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL Q"}
  , Record {uchar = '\119955', commands = [("isomath","\\mathbfit{r}"),("fixmath","\\mathbold{r}"),("unicode","\\mbfitr")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL R"}
  , Record {uchar = '\119956', commands = [("isomath","\\mathbfit{s}"),("fixmath","\\mathbold{s}"),("unicode","\\mbfits")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL S"}
  , Record {uchar = '\119957', commands = [("isomath","\\mathbfit{t}"),("fixmath","\\mathbold{t}"),("unicode","\\mbfitt")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL T"}
  , Record {uchar = '\119958', commands = [("isomath","\\mathbfit{u}"),("fixmath","\\mathbold{u}"),("unicode","\\mbfitu")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL U"}
  , Record {uchar = '\119959', commands = [("isomath","\\mathbfit{v}"),("fixmath","\\mathbold{v}"),("unicode","\\mbfitv")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL V"}
  , Record {uchar = '\119960', commands = [("isomath","\\mathbfit{w}"),("fixmath","\\mathbold{w}"),("unicode","\\mbfitw")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL W"}
  , Record {uchar = '\119961', commands = [("isomath","\\mathbfit{x}"),("fixmath","\\mathbold{x}"),("unicode","\\mbfitx")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL X"}
  , Record {uchar = '\119962', commands = [("isomath","\\mathbfit{y}"),("fixmath","\\mathbold{y}"),("unicode","\\mbfity")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL Y"}
  , Record {uchar = '\119963', commands = [("isomath","\\mathbfit{z}"),("fixmath","\\mathbold{z}"),("unicode","\\mbfitz")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL Z"}
  , Record {uchar = '\119964', commands = [("base","\\mathcal{A}"),("unicode","\\mscrA")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL A"}
  , Record {uchar = '\119966', commands = [("base","\\mathcal{C}"),("unicode","\\mscrC")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL C"}
  , Record {uchar = '\119967', commands = [("base","\\mathcal{D}"),("unicode","\\mscrD")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL D"}
  , Record {uchar = '\119970', commands = [("base","\\mathcal{G}"),("unicode","\\mscrG")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL G"}
  , Record {uchar = '\119973', commands = [("base","\\mathcal{J}"),("unicode","\\mscrJ")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL J"}
  , Record {uchar = '\119974', commands = [("base","\\mathcal{K}"),("unicode","\\mscrK")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL K"}
  , Record {uchar = '\119977', commands = [("base","\\mathcal{N}"),("unicode","\\mscrN")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL N"}
  , Record {uchar = '\119978', commands = [("base","\\mathcal{O}"),("unicode","\\mscrO")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL O"}
  , Record {uchar = '\119979', commands = [("base","\\mathcal{P}"),("unicode","\\mscrP")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL P"}
  , Record {uchar = '\119980', commands = [("base","\\mathcal{Q}"),("unicode","\\mscrQ")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL Q"}
  , Record {uchar = '\119982', commands = [("base","\\mathcal{S}"),("unicode","\\mscrS")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL S"}
  , Record {uchar = '\119983', commands = [("base","\\mathcal{T}"),("unicode","\\mscrT")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL T"}
  , Record {uchar = '\119984', commands = [("base","\\mathcal{U}"),("unicode","\\mscrU")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL U"}
  , Record {uchar = '\119985', commands = [("base","\\mathcal{V}"),("unicode","\\mscrV")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL V"}
  , Record {uchar = '\119986', commands = [("base","\\mathcal{W}"),("unicode","\\mscrW")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL W"}
  , Record {uchar = '\119987', commands = [("base","\\mathcal{X}"),("unicode","\\mscrX")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL X"}
  , Record {uchar = '\119988', commands = [("base","\\mathcal{Y}"),("unicode","\\mscrY")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL Y"}
  , Record {uchar = '\119989', commands = [("base","\\mathcal{Z}"),("unicode","\\mscrZ")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL Z"}
  , Record {uchar = '\119990', commands = [("urwchancal","\\mathcal{a}"),("unicode","\\mscra")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL A"}
  , Record {uchar = '\119991', commands = [("urwchancal","\\mathcal{b}"),("unicode","\\mscrb")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL B"}
  , Record {uchar = '\119992', commands = [("urwchancal","\\mathcal{c}"),("unicode","\\mscrc")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL C"}
  , Record {uchar = '\119993', commands = [("urwchancal","\\mathcal{d}"),("unicode","\\mscrd")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL D"}
  , Record {uchar = '\119995', commands = [("urwchancal","\\mathcal{f}"),("unicode","\\mscrf")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL F"}
  , Record {uchar = '\119997', commands = [("urwchancal","\\mathcal{h}"),("unicode","\\mscrh")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL H"}
  , Record {uchar = '\119998', commands = [("urwchancal","\\mathcal{i}"),("unicode","\\mscri")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL I"}
  , Record {uchar = '\119999', commands = [("urwchancal","\\mathcal{j}"),("unicode","\\mscrj")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL J"}
  , Record {uchar = '\120000', commands = [("urwchancal","\\mathcal{k}"),("unicode","\\mscrk")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL K"}
  , Record {uchar = '\120001', commands = [("urwchancal","\\mathcal{l}"),("unicode","\\mscrl")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL L"}
  , Record {uchar = '\120002', commands = [("urwchancal","\\mathcal{m}"),("unicode","\\mscrm")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL M"}
  , Record {uchar = '\120003', commands = [("urwchancal","\\mathcal{n}"),("unicode","\\mscrn")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL N"}
  , Record {uchar = '\120005', commands = [("urwchancal","\\mathcal{p}"),("unicode","\\mscrp")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL P"}
  , Record {uchar = '\120006', commands = [("urwchancal","\\mathcal{q}"),("unicode","\\mscrq")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL Q"}
  , Record {uchar = '\120007', commands = [("urwchancal","\\mathcal{r}"),("unicode","\\mscrr")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL R"}
  , Record {uchar = '\120008', commands = [("urwchancal","\\mathcal{s}"),("unicode","\\mscrs")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL S"}
  , Record {uchar = '\120009', commands = [("urwchancal","\\mathcal{t}"),("unicode","\\mscrt")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL T"}
  , Record {uchar = '\120010', commands = [("urwchancal","\\mathcal{u}"),("unicode","\\mscru")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL U"}
  , Record {uchar = '\120011', commands = [("urwchancal","\\mathcal{v}"),("unicode","\\mscrv")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL V"}
  , Record {uchar = '\120012', commands = [("urwchancal","\\mathcal{w}"),("unicode","\\mscrw")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL W"}
  , Record {uchar = '\120013', commands = [("urwchancal","\\mathcal{x}"),("unicode","\\mscrx")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL X"}
  , Record {uchar = '\120014', commands = [("urwchancal","\\mathcal{y}"),("unicode","\\mscry")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL Y"}
  , Record {uchar = '\120015', commands = [("urwchancal","\\mathcal{z}"),("unicode","\\mscrz")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL Z"}
  , Record {uchar = '\120016', commands = [("base",""),("unicode","\\mbfscrA")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL A"}
  , Record {uchar = '\120017', commands = [("base",""),("unicode","\\mbfscrB")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL B"}
  , Record {uchar = '\120018', commands = [("base",""),("unicode","\\mbfscrC")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL C"}
  , Record {uchar = '\120019', commands = [("base",""),("unicode","\\mbfscrD")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL D"}
  , Record {uchar = '\120020', commands = [("base",""),("unicode","\\mbfscrE")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL E"}
  , Record {uchar = '\120021', commands = [("base",""),("unicode","\\mbfscrF")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL F"}
  , Record {uchar = '\120022', commands = [("base",""),("unicode","\\mbfscrG")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL G"}
  , Record {uchar = '\120023', commands = [("base",""),("unicode","\\mbfscrH")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL H"}
  , Record {uchar = '\120024', commands = [("base",""),("unicode","\\mbfscrI")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL I"}
  , Record {uchar = '\120025', commands = [("base",""),("unicode","\\mbfscrJ")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL J"}
  , Record {uchar = '\120026', commands = [("base",""),("unicode","\\mbfscrK")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL K"}
  , Record {uchar = '\120027', commands = [("base",""),("unicode","\\mbfscrL")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL L"}
  , Record {uchar = '\120028', commands = [("base",""),("unicode","\\mbfscrM")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL M"}
  , Record {uchar = '\120029', commands = [("base",""),("unicode","\\mbfscrN")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL N"}
  , Record {uchar = '\120030', commands = [("base",""),("unicode","\\mbfscrO")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL O"}
  , Record {uchar = '\120031', commands = [("base",""),("unicode","\\mbfscrP")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL P"}
  , Record {uchar = '\120032', commands = [("base",""),("unicode","\\mbfscrQ")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL Q"}
  , Record {uchar = '\120033', commands = [("base",""),("unicode","\\mbfscrR")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL R"}
  , Record {uchar = '\120034', commands = [("base",""),("unicode","\\mbfscrS")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL S"}
  , Record {uchar = '\120035', commands = [("base",""),("unicode","\\mbfscrT")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL T"}
  , Record {uchar = '\120036', commands = [("base",""),("unicode","\\mbfscrU")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL U"}
  , Record {uchar = '\120037', commands = [("base",""),("unicode","\\mbfscrV")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL V"}
  , Record {uchar = '\120038', commands = [("base",""),("unicode","\\mbfscrW")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL W"}
  , Record {uchar = '\120039', commands = [("base",""),("unicode","\\mbfscrX")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL X"}
  , Record {uchar = '\120040', commands = [("base",""),("unicode","\\mbfscrY")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL Y"}
  , Record {uchar = '\120041', commands = [("base",""),("unicode","\\mbfscrZ")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL Z"}
  , Record {uchar = '\120042', commands = [("base",""),("unicode","\\mbfscra")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL A"}
  , Record {uchar = '\120043', commands = [("base",""),("unicode","\\mbfscrb")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL B"}
  , Record {uchar = '\120044', commands = [("base",""),("unicode","\\mbfscrc")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL C"}
  , Record {uchar = '\120045', commands = [("base",""),("unicode","\\mbfscrd")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL D"}
  , Record {uchar = '\120046', commands = [("base",""),("unicode","\\mbfscre")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL E"}
  , Record {uchar = '\120047', commands = [("base",""),("unicode","\\mbfscrf")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL F"}
  , Record {uchar = '\120048', commands = [("base",""),("unicode","\\mbfscrg")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL G"}
  , Record {uchar = '\120049', commands = [("base",""),("unicode","\\mbfscrh")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL H"}
  , Record {uchar = '\120050', commands = [("base",""),("unicode","\\mbfscri")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL I"}
  , Record {uchar = '\120051', commands = [("base",""),("unicode","\\mbfscrj")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL J"}
  , Record {uchar = '\120052', commands = [("base",""),("unicode","\\mbfscrk")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL K"}
  , Record {uchar = '\120053', commands = [("base",""),("unicode","\\mbfscrl")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL L"}
  , Record {uchar = '\120054', commands = [("base",""),("unicode","\\mbfscrm")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL M"}
  , Record {uchar = '\120055', commands = [("base",""),("unicode","\\mbfscrn")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL N"}
  , Record {uchar = '\120056', commands = [("base",""),("unicode","\\mbfscro")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL O"}
  , Record {uchar = '\120057', commands = [("base",""),("unicode","\\mbfscrp")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL P"}
  , Record {uchar = '\120058', commands = [("base",""),("unicode","\\mbfscrq")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL Q"}
  , Record {uchar = '\120059', commands = [("base",""),("unicode","\\mbfscrr")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL R"}
  , Record {uchar = '\120060', commands = [("base",""),("unicode","\\mbfscrs")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL S"}
  , Record {uchar = '\120061', commands = [("base",""),("unicode","\\mbfscrt")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL T"}
  , Record {uchar = '\120062', commands = [("base",""),("unicode","\\mbfscru")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL U"}
  , Record {uchar = '\120063', commands = [("base",""),("unicode","\\mbfscrv")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL V"}
  , Record {uchar = '\120064', commands = [("base",""),("unicode","\\mbfscrw")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL W"}
  , Record {uchar = '\120065', commands = [("base",""),("unicode","\\mbfscrx")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL X"}
  , Record {uchar = '\120066', commands = [("base",""),("unicode","\\mbfscry")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL Y"}
  , Record {uchar = '\120067', commands = [("base",""),("unicode","\\mbfscrz")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL Z"}
  , Record {uchar = '\120068', commands = [("eufrak","\\mathfrak{A}"),("unicode","\\mfrakA")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL A"}
  , Record {uchar = '\120069', commands = [("eufrak","\\mathfrak{B}"),("unicode","\\mfrakB")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL B"}
  , Record {uchar = '\120071', commands = [("eufrak","\\mathfrak{D}"),("unicode","\\mfrakD")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL D"}
  , Record {uchar = '\120072', commands = [("eufrak","\\mathfrak{E}"),("unicode","\\mfrakE")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL E"}
  , Record {uchar = '\120073', commands = [("eufrak","\\mathfrak{F}"),("unicode","\\mfrakF")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL F"}
  , Record {uchar = '\120074', commands = [("eufrak","\\mathfrak{G}"),("unicode","\\mfrakG")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL G"}
  , Record {uchar = '\120077', commands = [("eufrak","\\mathfrak{J}"),("unicode","\\mfrakJ")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL J"}
  , Record {uchar = '\120078', commands = [("eufrak","\\mathfrak{K}"),("unicode","\\mfrakK")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL K"}
  , Record {uchar = '\120079', commands = [("eufrak","\\mathfrak{L}"),("unicode","\\mfrakL")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL L"}
  , Record {uchar = '\120080', commands = [("eufrak","\\mathfrak{M}"),("unicode","\\mfrakM")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL M"}
  , Record {uchar = '\120081', commands = [("eufrak","\\mathfrak{N}"),("unicode","\\mfrakN")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL N"}
  , Record {uchar = '\120082', commands = [("eufrak","\\mathfrak{O}"),("unicode","\\mfrakO")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL O"}
  , Record {uchar = '\120083', commands = [("eufrak","\\mathfrak{P}"),("unicode","\\mfrakP")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL P"}
  , Record {uchar = '\120084', commands = [("eufrak","\\mathfrak{Q}"),("unicode","\\mfrakQ")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL Q"}
  , Record {uchar = '\120086', commands = [("eufrak","\\mathfrak{S}"),("unicode","\\mfrakS")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL S"}
  , Record {uchar = '\120087', commands = [("eufrak","\\mathfrak{T}"),("unicode","\\mfrakT")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL T"}
  , Record {uchar = '\120088', commands = [("eufrak","\\mathfrak{U}"),("unicode","\\mfrakU")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL U"}
  , Record {uchar = '\120089', commands = [("eufrak","\\mathfrak{V}"),("unicode","\\mfrakV")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL V"}
  , Record {uchar = '\120090', commands = [("eufrak","\\mathfrak{W}"),("unicode","\\mfrakW")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL W"}
  , Record {uchar = '\120091', commands = [("eufrak","\\mathfrak{X}"),("unicode","\\mfrakX")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL X"}
  , Record {uchar = '\120092', commands = [("eufrak","\\mathfrak{Y}"),("unicode","\\mfrakY")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL Y"}
  , Record {uchar = '\120094', commands = [("eufrak","\\mathfrak{a}"),("unicode","\\mfraka")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL A"}
  , Record {uchar = '\120095', commands = [("eufrak","\\mathfrak{b}"),("unicode","\\mfrakb")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL B"}
  , Record {uchar = '\120096', commands = [("eufrak","\\mathfrak{c}"),("unicode","\\mfrakc")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL C"}
  , Record {uchar = '\120097', commands = [("eufrak","\\mathfrak{d}"),("unicode","\\mfrakd")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL D"}
  , Record {uchar = '\120098', commands = [("eufrak","\\mathfrak{e}"),("unicode","\\mfrake")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL E"}
  , Record {uchar = '\120099', commands = [("eufrak","\\mathfrak{f}"),("unicode","\\mfrakf")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL F"}
  , Record {uchar = '\120100', commands = [("eufrak","\\mathfrak{g}"),("unicode","\\mfrakg")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL G"}
  , Record {uchar = '\120101', commands = [("eufrak","\\mathfrak{h}"),("unicode","\\mfrakh")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL H"}
  , Record {uchar = '\120102', commands = [("eufrak","\\mathfrak{i}"),("unicode","\\mfraki")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL I"}
  , Record {uchar = '\120103', commands = [("eufrak","\\mathfrak{j}"),("unicode","\\mfrakj")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL J"}
  , Record {uchar = '\120104', commands = [("eufrak","\\mathfrak{k}"),("unicode","\\mfrakk")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL K"}
  , Record {uchar = '\120105', commands = [("eufrak","\\mathfrak{l}"),("unicode","\\mfrakl")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL L"}
  , Record {uchar = '\120106', commands = [("eufrak","\\mathfrak{m}"),("unicode","\\mfrakm")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL M"}
  , Record {uchar = '\120107', commands = [("eufrak","\\mathfrak{n}"),("unicode","\\mfrakn")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL N"}
  , Record {uchar = '\120108', commands = [("eufrak","\\mathfrak{o}"),("unicode","\\mfrako")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL O"}
  , Record {uchar = '\120109', commands = [("eufrak","\\mathfrak{p}"),("unicode","\\mfrakp")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL P"}
  , Record {uchar = '\120110', commands = [("eufrak","\\mathfrak{q}"),("unicode","\\mfrakq")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL Q"}
  , Record {uchar = '\120111', commands = [("eufrak","\\mathfrak{r}"),("unicode","\\mfrakr")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL R"}
  , Record {uchar = '\120112', commands = [("eufrak","\\mathfrak{s}"),("unicode","\\mfraks")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL S"}
  , Record {uchar = '\120113', commands = [("eufrak","\\mathfrak{t}"),("unicode","\\mfrakt")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL T"}
  , Record {uchar = '\120114', commands = [("eufrak","\\mathfrak{u}"),("unicode","\\mfraku")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL U"}
  , Record {uchar = '\120115', commands = [("eufrak","\\mathfrak{v}"),("unicode","\\mfrakv")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL V"}
  , Record {uchar = '\120116', commands = [("eufrak","\\mathfrak{w}"),("unicode","\\mfrakw")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL W"}
  , Record {uchar = '\120117', commands = [("eufrak","\\mathfrak{x}"),("unicode","\\mfrakx")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL X"}
  , Record {uchar = '\120118', commands = [("eufrak","\\mathfrak{y}"),("unicode","\\mfraky")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL Y"}
  , Record {uchar = '\120119', commands = [("eufrak","\\mathfrak{z}"),("unicode","\\mfrakz")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL Z"}
  , Record {uchar = '\120120', commands = [("mathbb","\\mathbb{A}"),("dsfont","\\mathds{A}"),("unicode","\\BbbA")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK CAPITAL A"}
  , Record {uchar = '\120121', commands = [("mathbb","\\mathbb{B}"),("dsfont","\\mathds{B}"),("unicode","\\BbbB")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL B"}
  , Record {uchar = '\120123', commands = [("mathbb","\\mathbb{D}"),("dsfont","\\mathds{D}"),("unicode","\\BbbD")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL D"}
  , Record {uchar = '\120124', commands = [("mathbb","\\mathbb{E}"),("dsfont","\\mathds{E}"),("unicode","\\BbbE")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL E"}
  , Record {uchar = '\120125', commands = [("mathbb","\\mathbb{F}"),("dsfont","\\mathds{F}"),("unicode","\\BbbF")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL F"}
  , Record {uchar = '\120126', commands = [("mathbb","\\mathbb{G}"),("dsfont","\\mathds{G}"),("unicode","\\BbbG")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL G"}
  , Record {uchar = '\120128', commands = [("mathbb","\\mathbb{I}"),("dsfont","\\mathds{I}"),("unicode","\\BbbI")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL I"}
  , Record {uchar = '\120129', commands = [("mathbb","\\mathbb{J}"),("dsfont","\\mathds{J}"),("unicode","\\BbbJ")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL J"}
  , Record {uchar = '\120130', commands = [("mathbb","\\mathbb{K}"),("dsfont","\\mathds{K}"),("unicode","\\BbbK")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL K"}
  , Record {uchar = '\120131', commands = [("mathbb","\\mathbb{L}"),("dsfont","\\mathds{L}"),("unicode","\\BbbL")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL L"}
  , Record {uchar = '\120132', commands = [("mathbb","\\mathbb{M}"),("dsfont","\\mathds{M}"),("unicode","\\BbbM")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL M"}
  , Record {uchar = '\120134', commands = [("mathbb","\\mathbb{O}"),("dsfont","\\mathds{O}"),("unicode","\\BbbO")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL O"}
  , Record {uchar = '\120138', commands = [("mathbb","\\mathbb{S}"),("dsfont","\\mathds{S}"),("unicode","\\BbbS")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL S"}
  , Record {uchar = '\120139', commands = [("mathbb","\\mathbb{T}"),("dsfont","\\mathds{T}"),("unicode","\\BbbT")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL T"}
  , Record {uchar = '\120140', commands = [("mathbb","\\mathbb{U}"),("dsfont","\\mathds{U}"),("unicode","\\BbbU")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL U"}
  , Record {uchar = '\120141', commands = [("mathbb","\\mathbb{V}"),("dsfont","\\mathds{V}"),("unicode","\\BbbV")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL V"}
  , Record {uchar = '\120142', commands = [("mathbb","\\mathbb{W}"),("dsfont","\\mathds{W}"),("unicode","\\BbbW")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL W"}
  , Record {uchar = '\120143', commands = [("mathbb","\\mathbb{X}"),("dsfont","\\mathds{X}"),("unicode","\\BbbX")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL X"}
  , Record {uchar = '\120144', commands = [("mathbb","\\mathbb{Y}"),("dsfont","\\mathds{Y}"),("unicode","\\BbbY")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL Y"}
  , Record {uchar = '\120146', commands = [("bbold","\\mathbb{a}"),("unicode","\\Bbba")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL A"}
  , Record {uchar = '\120147', commands = [("bbold","\\mathbb{b}"),("unicode","\\Bbbb")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL B"}
  , Record {uchar = '\120148', commands = [("bbold","\\mathbb{c}"),("unicode","\\Bbbc")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL C"}
  , Record {uchar = '\120149', commands = [("bbold","\\mathbb{d}"),("unicode","\\Bbbd")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL D"}
  , Record {uchar = '\120150', commands = [("bbold","\\mathbb{e}"),("unicode","\\Bbbe")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL E"}
  , Record {uchar = '\120151', commands = [("bbold","\\mathbb{f}"),("unicode","\\Bbbf")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL F"}
  , Record {uchar = '\120152', commands = [("bbold","\\mathbb{g}"),("unicode","\\Bbbg")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL G"}
  , Record {uchar = '\120153', commands = [("bbold","\\mathbb{h}"),("unicode","\\Bbbh")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL H"}
  , Record {uchar = '\120154', commands = [("bbold","\\mathbb{i}"),("unicode","\\Bbbi")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL I"}
  , Record {uchar = '\120155', commands = [("bbold","\\mathbb{j}"),("unicode","\\Bbbj")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL J"}
  , Record {uchar = '\120156', commands = [("bbold","\\mathbb{k}"),("fourier","\\mathbb{k}"),("amssymb","\\Bbbk"),("unicode","\\Bbbk")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL K"}
  , Record {uchar = '\120157', commands = [("bbold","\\mathbb{l}"),("unicode","\\Bbbl")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL L"}
  , Record {uchar = '\120158', commands = [("bbold","\\mathbb{m}"),("unicode","\\Bbbm")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL M"}
  , Record {uchar = '\120159', commands = [("bbold","\\mathbb{n}"),("unicode","\\Bbbn")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL N"}
  , Record {uchar = '\120160', commands = [("bbold","\\mathbb{o}"),("unicode","\\Bbbo")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL O"}
  , Record {uchar = '\120161', commands = [("bbold","\\mathbb{p}"),("unicode","\\Bbbp")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL P"}
  , Record {uchar = '\120162', commands = [("bbold","\\mathbb{q}"),("unicode","\\Bbbq")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL Q"}
  , Record {uchar = '\120163', commands = [("bbold","\\mathbb{r}"),("unicode","\\Bbbr")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL R"}
  , Record {uchar = '\120164', commands = [("bbold","\\mathbb{s}"),("unicode","\\Bbbs")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL S"}
  , Record {uchar = '\120165', commands = [("bbold","\\mathbb{t}"),("unicode","\\Bbbt")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL T"}
  , Record {uchar = '\120166', commands = [("bbold","\\mathbb{u}"),("unicode","\\Bbbu")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL U"}
  , Record {uchar = '\120167', commands = [("bbold","\\mathbb{v}"),("unicode","\\Bbbv")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL V"}
  , Record {uchar = '\120168', commands = [("bbold","\\mathbb{w}"),("unicode","\\Bbbw")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL W"}
  , Record {uchar = '\120169', commands = [("bbold","\\mathbb{x}"),("unicode","\\Bbbx")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL X"}
  , Record {uchar = '\120170', commands = [("bbold","\\mathbb{y}"),("unicode","\\Bbby")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL Y"}
  , Record {uchar = '\120171', commands = [("bbold","\\mathbb{z}"),("unicode","\\Bbbz")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL Z"}
  , Record {uchar = '\120172', commands = [("base",""),("unicode","\\mbffrakA")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL A"}
  , Record {uchar = '\120173', commands = [("base",""),("unicode","\\mbffrakB")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL B"}
  , Record {uchar = '\120174', commands = [("base",""),("unicode","\\mbffrakC")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL C"}
  , Record {uchar = '\120175', commands = [("base",""),("unicode","\\mbffrakD")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL D"}
  , Record {uchar = '\120176', commands = [("base",""),("unicode","\\mbffrakE")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL E"}
  , Record {uchar = '\120177', commands = [("base",""),("unicode","\\mbffrakF")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL F"}
  , Record {uchar = '\120178', commands = [("base",""),("unicode","\\mbffrakG")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL G"}
  , Record {uchar = '\120179', commands = [("base",""),("unicode","\\mbffrakH")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL H"}
  , Record {uchar = '\120180', commands = [("base",""),("unicode","\\mbffrakI")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL I"}
  , Record {uchar = '\120181', commands = [("base",""),("unicode","\\mbffrakJ")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL J"}
  , Record {uchar = '\120182', commands = [("base",""),("unicode","\\mbffrakK")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL K"}
  , Record {uchar = '\120183', commands = [("base",""),("unicode","\\mbffrakL")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL L"}
  , Record {uchar = '\120184', commands = [("base",""),("unicode","\\mbffrakM")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL M"}
  , Record {uchar = '\120185', commands = [("base",""),("unicode","\\mbffrakN")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL N"}
  , Record {uchar = '\120186', commands = [("base",""),("unicode","\\mbffrakO")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL O"}
  , Record {uchar = '\120187', commands = [("base",""),("unicode","\\mbffrakP")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL P"}
  , Record {uchar = '\120188', commands = [("base",""),("unicode","\\mbffrakQ")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL Q"}
  , Record {uchar = '\120189', commands = [("base",""),("unicode","\\mbffrakR")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL R"}
  , Record {uchar = '\120190', commands = [("base",""),("unicode","\\mbffrakS")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL S"}
  , Record {uchar = '\120191', commands = [("base",""),("unicode","\\mbffrakT")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL T"}
  , Record {uchar = '\120192', commands = [("base",""),("unicode","\\mbffrakU")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL U"}
  , Record {uchar = '\120193', commands = [("base",""),("unicode","\\mbffrakV")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL V"}
  , Record {uchar = '\120194', commands = [("base",""),("unicode","\\mbffrakW")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL W"}
  , Record {uchar = '\120195', commands = [("base",""),("unicode","\\mbffrakX")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL X"}
  , Record {uchar = '\120196', commands = [("base",""),("unicode","\\mbffrakY")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL Y"}
  , Record {uchar = '\120197', commands = [("base",""),("unicode","\\mbffrakZ")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL Z"}
  , Record {uchar = '\120198', commands = [("base",""),("unicode","\\mbffraka")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL A"}
  , Record {uchar = '\120199', commands = [("base",""),("unicode","\\mbffrakb")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL B"}
  , Record {uchar = '\120200', commands = [("base",""),("unicode","\\mbffrakc")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL C"}
  , Record {uchar = '\120201', commands = [("base",""),("unicode","\\mbffrakd")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL D"}
  , Record {uchar = '\120202', commands = [("base",""),("unicode","\\mbffrake")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL E"}
  , Record {uchar = '\120203', commands = [("base",""),("unicode","\\mbffrakf")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL F"}
  , Record {uchar = '\120204', commands = [("base",""),("unicode","\\mbffrakg")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL G"}
  , Record {uchar = '\120205', commands = [("base",""),("unicode","\\mbffrakh")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL H"}
  , Record {uchar = '\120206', commands = [("base",""),("unicode","\\mbffraki")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL I"}
  , Record {uchar = '\120207', commands = [("base",""),("unicode","\\mbffrakj")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL J"}
  , Record {uchar = '\120208', commands = [("base",""),("unicode","\\mbffrakk")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL K"}
  , Record {uchar = '\120209', commands = [("base",""),("unicode","\\mbffrakl")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL L"}
  , Record {uchar = '\120210', commands = [("base",""),("unicode","\\mbffrakm")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL M"}
  , Record {uchar = '\120211', commands = [("base",""),("unicode","\\mbffrakn")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL N"}
  , Record {uchar = '\120212', commands = [("base",""),("unicode","\\mbffrako")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL O"}
  , Record {uchar = '\120213', commands = [("base",""),("unicode","\\mbffrakp")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL P"}
  , Record {uchar = '\120214', commands = [("base",""),("unicode","\\mbffrakq")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL Q"}
  , Record {uchar = '\120215', commands = [("base",""),("unicode","\\mbffrakr")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL R"}
  , Record {uchar = '\120216', commands = [("base",""),("unicode","\\mbffraks")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL S"}
  , Record {uchar = '\120217', commands = [("base",""),("unicode","\\mbffrakt")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL T"}
  , Record {uchar = '\120218', commands = [("base",""),("unicode","\\mbffraku")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL U"}
  , Record {uchar = '\120219', commands = [("base",""),("unicode","\\mbffrakv")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL V"}
  , Record {uchar = '\120220', commands = [("base",""),("unicode","\\mbffrakw")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL W"}
  , Record {uchar = '\120221', commands = [("base",""),("unicode","\\mbffrakx")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL X"}
  , Record {uchar = '\120222', commands = [("base",""),("unicode","\\mbffraky")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL Y"}
  , Record {uchar = '\120223', commands = [("base",""),("unicode","\\mbffrakz")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL Z"}
  , Record {uchar = '\120224', commands = [("base","\\mathsf{A}"),("unicode","\\msansA")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL A"}
  , Record {uchar = '\120225', commands = [("base","\\mathsf{B}"),("unicode","\\msansB")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL B"}
  , Record {uchar = '\120226', commands = [("base","\\mathsf{C}"),("unicode","\\msansC")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL C"}
  , Record {uchar = '\120227', commands = [("base","\\mathsf{D}"),("unicode","\\msansD")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL D"}
  , Record {uchar = '\120228', commands = [("base","\\mathsf{E}"),("unicode","\\msansE")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL E"}
  , Record {uchar = '\120229', commands = [("base","\\mathsf{F}"),("unicode","\\msansF")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL F"}
  , Record {uchar = '\120230', commands = [("base","\\mathsf{G}"),("unicode","\\msansG")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL G"}
  , Record {uchar = '\120231', commands = [("base","\\mathsf{H}"),("unicode","\\msansH")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL H"}
  , Record {uchar = '\120232', commands = [("base","\\mathsf{I}"),("unicode","\\msansI")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL I"}
  , Record {uchar = '\120233', commands = [("base","\\mathsf{J}"),("unicode","\\msansJ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL J"}
  , Record {uchar = '\120234', commands = [("base","\\mathsf{K}"),("unicode","\\msansK")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL K"}
  , Record {uchar = '\120235', commands = [("base","\\mathsf{L}"),("unicode","\\msansL")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL L"}
  , Record {uchar = '\120236', commands = [("base","\\mathsf{M}"),("unicode","\\msansM")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL M"}
  , Record {uchar = '\120237', commands = [("base","\\mathsf{N}"),("unicode","\\msansN")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL N"}
  , Record {uchar = '\120238', commands = [("base","\\mathsf{O}"),("unicode","\\msansO")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL O"}
  , Record {uchar = '\120239', commands = [("base","\\mathsf{P}"),("unicode","\\msansP")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL P"}
  , Record {uchar = '\120240', commands = [("base","\\mathsf{Q}"),("unicode","\\msansQ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL Q"}
  , Record {uchar = '\120241', commands = [("base","\\mathsf{R}"),("unicode","\\msansR")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL R"}
  , Record {uchar = '\120242', commands = [("base","\\mathsf{S}"),("unicode","\\msansS")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL S"}
  , Record {uchar = '\120243', commands = [("base","\\mathsf{T}"),("unicode","\\msansT")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL T"}
  , Record {uchar = '\120244', commands = [("base","\\mathsf{U}"),("unicode","\\msansU")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL U"}
  , Record {uchar = '\120245', commands = [("base","\\mathsf{V}"),("unicode","\\msansV")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL V"}
  , Record {uchar = '\120246', commands = [("base","\\mathsf{W}"),("unicode","\\msansW")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL W"}
  , Record {uchar = '\120247', commands = [("base","\\mathsf{X}"),("unicode","\\msansX")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL X"}
  , Record {uchar = '\120248', commands = [("base","\\mathsf{Y}"),("unicode","\\msansY")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL Y"}
  , Record {uchar = '\120249', commands = [("base","\\mathsf{Z}"),("unicode","\\msansZ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL Z"}
  , Record {uchar = '\120250', commands = [("base","\\mathsf{a}"),("unicode","\\msansa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL A"}
  , Record {uchar = '\120251', commands = [("base","\\mathsf{b}"),("unicode","\\msansb")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL B"}
  , Record {uchar = '\120252', commands = [("base","\\mathsf{c}"),("unicode","\\msansc")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL C"}
  , Record {uchar = '\120253', commands = [("base","\\mathsf{d}"),("unicode","\\msansd")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL D"}
  , Record {uchar = '\120254', commands = [("base","\\mathsf{e}"),("unicode","\\msanse")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL E"}
  , Record {uchar = '\120255', commands = [("base","\\mathsf{f}"),("unicode","\\msansf")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL F"}
  , Record {uchar = '\120256', commands = [("base","\\mathsf{g}"),("unicode","\\msansg")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL G"}
  , Record {uchar = '\120257', commands = [("base","\\mathsf{h}"),("unicode","\\msansh")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL H"}
  , Record {uchar = '\120258', commands = [("base","\\mathsf{i}"),("unicode","\\msansi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL I"}
  , Record {uchar = '\120259', commands = [("base","\\mathsf{j}"),("unicode","\\msansj")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL J"}
  , Record {uchar = '\120260', commands = [("base","\\mathsf{k}"),("unicode","\\msansk")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL K"}
  , Record {uchar = '\120261', commands = [("base","\\mathsf{l}"),("unicode","\\msansl")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL L"}
  , Record {uchar = '\120262', commands = [("base","\\mathsf{m}"),("unicode","\\msansm")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL M"}
  , Record {uchar = '\120263', commands = [("base","\\mathsf{n}"),("unicode","\\msansn")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL N"}
  , Record {uchar = '\120264', commands = [("base","\\mathsf{o}"),("unicode","\\msanso")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL O"}
  , Record {uchar = '\120265', commands = [("base","\\mathsf{p}"),("unicode","\\msansp")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL P"}
  , Record {uchar = '\120266', commands = [("base","\\mathsf{q}"),("unicode","\\msansq")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL Q"}
  , Record {uchar = '\120267', commands = [("base","\\mathsf{r}"),("unicode","\\msansr")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL R"}
  , Record {uchar = '\120268', commands = [("base","\\mathsf{s}"),("unicode","\\msanss")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL S"}
  , Record {uchar = '\120269', commands = [("base","\\mathsf{t}"),("unicode","\\msanst")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL T"}
  , Record {uchar = '\120270', commands = [("base","\\mathsf{u}"),("unicode","\\msansu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL U"}
  , Record {uchar = '\120271', commands = [("base","\\mathsf{v}"),("unicode","\\msansv")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL V"}
  , Record {uchar = '\120272', commands = [("base","\\mathsf{w}"),("unicode","\\msansw")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL W"}
  , Record {uchar = '\120273', commands = [("base","\\mathsf{x}"),("unicode","\\msansx")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL X"}
  , Record {uchar = '\120274', commands = [("base","\\mathsf{y}"),("unicode","\\msansy")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL Y"}
  , Record {uchar = '\120275', commands = [("base","\\mathsf{z}"),("unicode","\\msansz")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL Z"}
  , Record {uchar = '\120276', commands = [("mathsfbf","\\mathsfbf{A}"),("unicode","\\mbfsansA")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL A"}
  , Record {uchar = '\120277', commands = [("mathsfbf","\\mathsfbf{B}"),("unicode","\\mbfsansB")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL B"}
  , Record {uchar = '\120278', commands = [("mathsfbf","\\mathsfbf{C}"),("unicode","\\mbfsansC")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL C"}
  , Record {uchar = '\120279', commands = [("mathsfbf","\\mathsfbf{D}"),("unicode","\\mbfsansD")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL D"}
  , Record {uchar = '\120280', commands = [("mathsfbf","\\mathsfbf{E}"),("unicode","\\mbfsansE")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL E"}
  , Record {uchar = '\120281', commands = [("mathsfbf","\\mathsfbf{F}"),("unicode","\\mbfsansF")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL F"}
  , Record {uchar = '\120282', commands = [("mathsfbf","\\mathsfbf{G}"),("unicode","\\mbfsansG")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL G"}
  , Record {uchar = '\120283', commands = [("mathsfbf","\\mathsfbf{H}"),("unicode","\\mbfsansH")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL H"}
  , Record {uchar = '\120284', commands = [("mathsfbf","\\mathsfbf{I}"),("unicode","\\mbfsansI")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL I"}
  , Record {uchar = '\120285', commands = [("mathsfbf","\\mathsfbf{J}"),("unicode","\\mbfsansJ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL J"}
  , Record {uchar = '\120286', commands = [("mathsfbf","\\mathsfbf{K}"),("unicode","\\mbfsansK")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL K"}
  , Record {uchar = '\120287', commands = [("mathsfbf","\\mathsfbf{L}"),("unicode","\\mbfsansL")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL L"}
  , Record {uchar = '\120288', commands = [("mathsfbf","\\mathsfbf{M}"),("unicode","\\mbfsansM")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL M"}
  , Record {uchar = '\120289', commands = [("mathsfbf","\\mathsfbf{N}"),("unicode","\\mbfsansN")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL N"}
  , Record {uchar = '\120290', commands = [("mathsfbf","\\mathsfbf{O}"),("unicode","\\mbfsansO")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL O"}
  , Record {uchar = '\120291', commands = [("mathsfbf","\\mathsfbf{P}"),("unicode","\\mbfsansP")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL P"}
  , Record {uchar = '\120292', commands = [("mathsfbf","\\mathsfbf{Q}"),("unicode","\\mbfsansQ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL Q"}
  , Record {uchar = '\120293', commands = [("mathsfbf","\\mathsfbf{R}"),("unicode","\\mbfsansR")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL R"}
  , Record {uchar = '\120294', commands = [("mathsfbf","\\mathsfbf{S}"),("unicode","\\mbfsansS")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL S"}
  , Record {uchar = '\120295', commands = [("mathsfbf","\\mathsfbf{T}"),("unicode","\\mbfsansT")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL T"}
  , Record {uchar = '\120296', commands = [("mathsfbf","\\mathsfbf{U}"),("unicode","\\mbfsansU")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL U"}
  , Record {uchar = '\120297', commands = [("mathsfbf","\\mathsfbf{V}"),("unicode","\\mbfsansV")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL V"}
  , Record {uchar = '\120298', commands = [("mathsfbf","\\mathsfbf{W}"),("unicode","\\mbfsansW")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL W"}
  , Record {uchar = '\120299', commands = [("mathsfbf","\\mathsfbf{X}"),("unicode","\\mbfsansX")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL X"}
  , Record {uchar = '\120300', commands = [("mathsfbf","\\mathsfbf{Y}"),("unicode","\\mbfsansY")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL Y"}
  , Record {uchar = '\120301', commands = [("mathsfbf","\\mathsfbf{Z}"),("unicode","\\mbfsansZ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL Z"}
  , Record {uchar = '\120302', commands = [("mathsfbf","\\mathsfbf{a}"),("unicode","\\mbfsansa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL A"}
  , Record {uchar = '\120303', commands = [("mathsfbf","\\mathsfbf{b}"),("unicode","\\mbfsansb")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL B"}
  , Record {uchar = '\120304', commands = [("mathsfbf","\\mathsfbf{c}"),("unicode","\\mbfsansc")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL C"}
  , Record {uchar = '\120305', commands = [("mathsfbf","\\mathsfbf{d}"),("unicode","\\mbfsansd")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL D"}
  , Record {uchar = '\120306', commands = [("mathsfbf","\\mathsfbf{e}"),("unicode","\\mbfsanse")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL E"}
  , Record {uchar = '\120307', commands = [("mathsfbf","\\mathsfbf{f}"),("unicode","\\mbfsansf")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL F"}
  , Record {uchar = '\120308', commands = [("mathsfbf","\\mathsfbf{g}"),("unicode","\\mbfsansg")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL G"}
  , Record {uchar = '\120309', commands = [("mathsfbf","\\mathsfbf{h}"),("unicode","\\mbfsansh")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL H"}
  , Record {uchar = '\120310', commands = [("mathsfbf","\\mathsfbf{i}"),("unicode","\\mbfsansi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL I"}
  , Record {uchar = '\120311', commands = [("mathsfbf","\\mathsfbf{j}"),("unicode","\\mbfsansj")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL J"}
  , Record {uchar = '\120312', commands = [("mathsfbf","\\mathsfbf{k}"),("unicode","\\mbfsansk")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL K"}
  , Record {uchar = '\120313', commands = [("mathsfbf","\\mathsfbf{l}"),("unicode","\\mbfsansl")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL L"}
  , Record {uchar = '\120314', commands = [("mathsfbf","\\mathsfbf{m}"),("unicode","\\mbfsansm")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL M"}
  , Record {uchar = '\120315', commands = [("mathsfbf","\\mathsfbf{n}"),("unicode","\\mbfsansn")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL N"}
  , Record {uchar = '\120316', commands = [("mathsfbf","\\mathsfbf{o}"),("unicode","\\mbfsanso")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL O"}
  , Record {uchar = '\120317', commands = [("mathsfbf","\\mathsfbf{p}"),("unicode","\\mbfsansp")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL P"}
  , Record {uchar = '\120318', commands = [("mathsfbf","\\mathsfbf{q}"),("unicode","\\mbfsansq")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL Q"}
  , Record {uchar = '\120319', commands = [("mathsfbf","\\mathsfbf{r}"),("unicode","\\mbfsansr")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL R"}
  , Record {uchar = '\120320', commands = [("mathsfbf","\\mathsfbf{s}"),("unicode","\\mbfsanss")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL S"}
  , Record {uchar = '\120321', commands = [("mathsfbf","\\mathsfbf{t}"),("unicode","\\mbfsanst")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL T"}
  , Record {uchar = '\120322', commands = [("mathsfbf","\\mathsfbf{u}"),("unicode","\\mbfsansu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL U"}
  , Record {uchar = '\120323', commands = [("mathsfbf","\\mathsfbf{v}"),("unicode","\\mbfsansv")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL V"}
  , Record {uchar = '\120324', commands = [("mathsfbf","\\mathsfbf{w}"),("unicode","\\mbfsansw")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL W"}
  , Record {uchar = '\120325', commands = [("mathsfbf","\\mathsfbf{x}"),("unicode","\\mbfsansx")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL X"}
  , Record {uchar = '\120326', commands = [("mathsfbf","\\mathsfbf{y}"),("unicode","\\mbfsansy")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL Y"}
  , Record {uchar = '\120327', commands = [("mathsfbf","\\mathsfbf{z}"),("unicode","\\mbfsansz")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL Z"}
  , Record {uchar = '\120328', commands = [("omlmathsfit","\\mathsfit{A}"),("unicode","\\mitsansA")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL A"}
  , Record {uchar = '\120329', commands = [("omlmathsfit","\\mathsfit{B}"),("unicode","\\mitsansB")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL B"}
  , Record {uchar = '\120330', commands = [("omlmathsfit","\\mathsfit{C}"),("unicode","\\mitsansC")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL C"}
  , Record {uchar = '\120331', commands = [("omlmathsfit","\\mathsfit{D}"),("unicode","\\mitsansD")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL D"}
  , Record {uchar = '\120332', commands = [("omlmathsfit","\\mathsfit{E}"),("unicode","\\mitsansE")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL E"}
  , Record {uchar = '\120333', commands = [("omlmathsfit","\\mathsfit{F}"),("unicode","\\mitsansF")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL F"}
  , Record {uchar = '\120334', commands = [("omlmathsfit","\\mathsfit{G}"),("unicode","\\mitsansG")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL G"}
  , Record {uchar = '\120335', commands = [("omlmathsfit","\\mathsfit{H}"),("unicode","\\mitsansH")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL H"}
  , Record {uchar = '\120336', commands = [("omlmathsfit","\\mathsfit{I}"),("unicode","\\mitsansI")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL I"}
  , Record {uchar = '\120337', commands = [("omlmathsfit","\\mathsfit{J}"),("unicode","\\mitsansJ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL J"}
  , Record {uchar = '\120338', commands = [("omlmathsfit","\\mathsfit{K}"),("unicode","\\mitsansK")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL K"}
  , Record {uchar = '\120339', commands = [("omlmathsfit","\\mathsfit{L}"),("unicode","\\mitsansL")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL L"}
  , Record {uchar = '\120340', commands = [("omlmathsfit","\\mathsfit{M}"),("unicode","\\mitsansM")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL M"}
  , Record {uchar = '\120341', commands = [("omlmathsfit","\\mathsfit{N}"),("unicode","\\mitsansN")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL N"}
  , Record {uchar = '\120342', commands = [("omlmathsfit","\\mathsfit{O}"),("unicode","\\mitsansO")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL O"}
  , Record {uchar = '\120343', commands = [("omlmathsfit","\\mathsfit{P}"),("unicode","\\mitsansP")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL P"}
  , Record {uchar = '\120344', commands = [("omlmathsfit","\\mathsfit{Q}"),("unicode","\\mitsansQ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL Q"}
  , Record {uchar = '\120345', commands = [("omlmathsfit","\\mathsfit{R}"),("unicode","\\mitsansR")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL R"}
  , Record {uchar = '\120346', commands = [("omlmathsfit","\\mathsfit{S}"),("unicode","\\mitsansS")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL S"}
  , Record {uchar = '\120347', commands = [("omlmathsfit","\\mathsfit{T}"),("unicode","\\mitsansT")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL T"}
  , Record {uchar = '\120348', commands = [("omlmathsfit","\\mathsfit{U}"),("unicode","\\mitsansU")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL U"}
  , Record {uchar = '\120349', commands = [("omlmathsfit","\\mathsfit{V}"),("unicode","\\mitsansV")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL V"}
  , Record {uchar = '\120350', commands = [("omlmathsfit","\\mathsfit{W}"),("unicode","\\mitsansW")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL W"}
  , Record {uchar = '\120351', commands = [("omlmathsfit","\\mathsfit{X}"),("unicode","\\mitsansX")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL X"}
  , Record {uchar = '\120352', commands = [("omlmathsfit","\\mathsfit{Y}"),("unicode","\\mitsansY")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL Y"}
  , Record {uchar = '\120353', commands = [("omlmathsfit","\\mathsfit{Z}"),("unicode","\\mitsansZ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL Z"}
  , Record {uchar = '\120354', commands = [("omlmathsfit","\\mathsfit{a}"),("unicode","\\mitsansa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL A"}
  , Record {uchar = '\120355', commands = [("omlmathsfit","\\mathsfit{b}"),("unicode","\\mitsansb")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL B"}
  , Record {uchar = '\120356', commands = [("omlmathsfit","\\mathsfit{c}"),("unicode","\\mitsansc")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL C"}
  , Record {uchar = '\120357', commands = [("omlmathsfit","\\mathsfit{d}"),("unicode","\\mitsansd")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL D"}
  , Record {uchar = '\120358', commands = [("omlmathsfit","\\mathsfit{e}"),("unicode","\\mitsanse")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL E"}
  , Record {uchar = '\120359', commands = [("omlmathsfit","\\mathsfit{f}"),("unicode","\\mitsansf")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL F"}
  , Record {uchar = '\120360', commands = [("omlmathsfit","\\mathsfit{g}"),("unicode","\\mitsansg")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL G"}
  , Record {uchar = '\120361', commands = [("omlmathsfit","\\mathsfit{h}"),("unicode","\\mitsansh")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL H"}
  , Record {uchar = '\120362', commands = [("omlmathsfit","\\mathsfit{i}"),("unicode","\\mitsansi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL I"}
  , Record {uchar = '\120363', commands = [("omlmathsfit","\\mathsfit{j}"),("unicode","\\mitsansj")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL J"}
  , Record {uchar = '\120364', commands = [("omlmathsfit","\\mathsfit{k}"),("unicode","\\mitsansk")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL K"}
  , Record {uchar = '\120365', commands = [("omlmathsfit","\\mathsfit{l}"),("unicode","\\mitsansl")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL L"}
  , Record {uchar = '\120366', commands = [("omlmathsfit","\\mathsfit{m}"),("unicode","\\mitsansm")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL M"}
  , Record {uchar = '\120367', commands = [("omlmathsfit","\\mathsfit{n}"),("unicode","\\mitsansn")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL N"}
  , Record {uchar = '\120368', commands = [("omlmathsfit","\\mathsfit{o}"),("unicode","\\mitsanso")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL O"}
  , Record {uchar = '\120369', commands = [("omlmathsfit","\\mathsfit{p}"),("unicode","\\mitsansp")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL P"}
  , Record {uchar = '\120370', commands = [("omlmathsfit","\\mathsfit{q}"),("unicode","\\mitsansq")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL Q"}
  , Record {uchar = '\120371', commands = [("omlmathsfit","\\mathsfit{r}"),("unicode","\\mitsansr")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL R"}
  , Record {uchar = '\120372', commands = [("omlmathsfit","\\mathsfit{s}"),("unicode","\\mitsanss")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL S"}
  , Record {uchar = '\120373', commands = [("omlmathsfit","\\mathsfit{t}"),("unicode","\\mitsanst")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL T"}
  , Record {uchar = '\120374', commands = [("omlmathsfit","\\mathsfit{u}"),("unicode","\\mitsansu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL U"}
  , Record {uchar = '\120375', commands = [("omlmathsfit","\\mathsfit{v}"),("unicode","\\mitsansv")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL V"}
  , Record {uchar = '\120376', commands = [("omlmathsfit","\\mathsfit{w}"),("unicode","\\mitsansw")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL W"}
  , Record {uchar = '\120377', commands = [("omlmathsfit","\\mathsfit{x}"),("unicode","\\mitsansx")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL X"}
  , Record {uchar = '\120378', commands = [("omlmathsfit","\\mathsfit{y}"),("unicode","\\mitsansy")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL Y"}
  , Record {uchar = '\120379', commands = [("omlmathsfit","\\mathsfit{z}"),("unicode","\\mitsansz")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL Z"}
  , Record {uchar = '\120380', commands = [("isomath","\\mathsfbfit{A}"),("unicode","\\mbfitsansA")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL A"}
  , Record {uchar = '\120381', commands = [("isomath","\\mathsfbfit{B}"),("unicode","\\mbfitsansB")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL B"}
  , Record {uchar = '\120382', commands = [("isomath","\\mathsfbfit{C}"),("unicode","\\mbfitsansC")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL C"}
  , Record {uchar = '\120383', commands = [("isomath","\\mathsfbfit{D}"),("unicode","\\mbfitsansD")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL D"}
  , Record {uchar = '\120384', commands = [("isomath","\\mathsfbfit{E}"),("unicode","\\mbfitsansE")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL E"}
  , Record {uchar = '\120385', commands = [("isomath","\\mathsfbfit{F}"),("unicode","\\mbfitsansF")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL F"}
  , Record {uchar = '\120386', commands = [("isomath","\\mathsfbfit{G}"),("unicode","\\mbfitsansG")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL G"}
  , Record {uchar = '\120387', commands = [("isomath","\\mathsfbfit{H}"),("unicode","\\mbfitsansH")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL H"}
  , Record {uchar = '\120388', commands = [("isomath","\\mathsfbfit{I}"),("unicode","\\mbfitsansI")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL I"}
  , Record {uchar = '\120389', commands = [("isomath","\\mathsfbfit{J}"),("unicode","\\mbfitsansJ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL J"}
  , Record {uchar = '\120390', commands = [("isomath","\\mathsfbfit{K}"),("unicode","\\mbfitsansK")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL K"}
  , Record {uchar = '\120391', commands = [("isomath","\\mathsfbfit{L}"),("unicode","\\mbfitsansL")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL L"}
  , Record {uchar = '\120392', commands = [("isomath","\\mathsfbfit{M}"),("unicode","\\mbfitsansM")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL M"}
  , Record {uchar = '\120393', commands = [("isomath","\\mathsfbfit{N}"),("unicode","\\mbfitsansN")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL N"}
  , Record {uchar = '\120394', commands = [("isomath","\\mathsfbfit{O}"),("unicode","\\mbfitsansO")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL O"}
  , Record {uchar = '\120395', commands = [("isomath","\\mathsfbfit{P}"),("unicode","\\mbfitsansP")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL P"}
  , Record {uchar = '\120396', commands = [("isomath","\\mathsfbfit{Q}"),("unicode","\\mbfitsansQ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Q"}
  , Record {uchar = '\120397', commands = [("isomath","\\mathsfbfit{R}"),("unicode","\\mbfitsansR")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL R"}
  , Record {uchar = '\120398', commands = [("isomath","\\mathsfbfit{S}"),("unicode","\\mbfitsansS")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL S"}
  , Record {uchar = '\120399', commands = [("isomath","\\mathsfbfit{T}"),("unicode","\\mbfitsansT")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL T"}
  , Record {uchar = '\120400', commands = [("isomath","\\mathsfbfit{U}"),("unicode","\\mbfitsansU")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL U"}
  , Record {uchar = '\120401', commands = [("isomath","\\mathsfbfit{V}"),("unicode","\\mbfitsansV")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL V"}
  , Record {uchar = '\120402', commands = [("isomath","\\mathsfbfit{W}"),("unicode","\\mbfitsansW")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL W"}
  , Record {uchar = '\120403', commands = [("isomath","\\mathsfbfit{X}"),("unicode","\\mbfitsansX")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL X"}
  , Record {uchar = '\120404', commands = [("isomath","\\mathsfbfit{Y}"),("unicode","\\mbfitsansY")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Y"}
  , Record {uchar = '\120405', commands = [("isomath","\\mathsfbfit{Z}"),("unicode","\\mbfitsansZ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Z"}
  , Record {uchar = '\120406', commands = [("isomath","\\mathsfbfit{a}"),("unicode","\\mbfitsansa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL A"}
  , Record {uchar = '\120407', commands = [("isomath","\\mathsfbfit{b}"),("unicode","\\mbfitsansb")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL B"}
  , Record {uchar = '\120408', commands = [("isomath","\\mathsfbfit{c}"),("unicode","\\mbfitsansc")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL C"}
  , Record {uchar = '\120409', commands = [("isomath","\\mathsfbfit{d}"),("unicode","\\mbfitsansd")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL D"}
  , Record {uchar = '\120410', commands = [("isomath","\\mathsfbfit{e}"),("unicode","\\mbfitsanse")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL E"}
  , Record {uchar = '\120411', commands = [("isomath","\\mathsfbfit{f}"),("unicode","\\mbfitsansf")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL F"}
  , Record {uchar = '\120412', commands = [("isomath","\\mathsfbfit{g}"),("unicode","\\mbfitsansg")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL G"}
  , Record {uchar = '\120413', commands = [("isomath","\\mathsfbfit{h}"),("unicode","\\mbfitsansh")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL H"}
  , Record {uchar = '\120414', commands = [("isomath","\\mathsfbfit{i}"),("unicode","\\mbfitsansi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL I"}
  , Record {uchar = '\120415', commands = [("isomath","\\mathsfbfit{j}"),("unicode","\\mbfitsansj")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL J"}
  , Record {uchar = '\120416', commands = [("isomath","\\mathsfbfit{k}"),("unicode","\\mbfitsansk")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL K"}
  , Record {uchar = '\120417', commands = [("isomath","\\mathsfbfit{l}"),("unicode","\\mbfitsansl")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL L"}
  , Record {uchar = '\120418', commands = [("isomath","\\mathsfbfit{m}"),("unicode","\\mbfitsansm")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL M"}
  , Record {uchar = '\120419', commands = [("isomath","\\mathsfbfit{n}"),("unicode","\\mbfitsansn")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL N"}
  , Record {uchar = '\120420', commands = [("isomath","\\mathsfbfit{o}"),("unicode","\\mbfitsanso")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL O"}
  , Record {uchar = '\120421', commands = [("isomath","\\mathsfbfit{p}"),("unicode","\\mbfitsansp")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL P"}
  , Record {uchar = '\120422', commands = [("isomath","\\mathsfbfit{q}"),("unicode","\\mbfitsansq")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Q"}
  , Record {uchar = '\120423', commands = [("isomath","\\mathsfbfit{r}"),("unicode","\\mbfitsansr")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL R"}
  , Record {uchar = '\120424', commands = [("isomath","\\mathsfbfit{s}"),("unicode","\\mbfitsanss")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL S"}
  , Record {uchar = '\120425', commands = [("isomath","\\mathsfbfit{t}"),("unicode","\\mbfitsanst")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL T"}
  , Record {uchar = '\120426', commands = [("isomath","\\mathsfbfit{u}"),("unicode","\\mbfitsansu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL U"}
  , Record {uchar = '\120427', commands = [("isomath","\\mathsfbfit{v}"),("unicode","\\mbfitsansv")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL V"}
  , Record {uchar = '\120428', commands = [("isomath","\\mathsfbfit{w}"),("unicode","\\mbfitsansw")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL W"}
  , Record {uchar = '\120429', commands = [("isomath","\\mathsfbfit{x}"),("unicode","\\mbfitsansx")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL X"}
  , Record {uchar = '\120430', commands = [("isomath","\\mathsfbfit{y}"),("unicode","\\mbfitsansy")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Y"}
  , Record {uchar = '\120431', commands = [("isomath","\\mathsfbfit{z}"),("unicode","\\mbfitsansz")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Z"}
  , Record {uchar = '\120432', commands = [("base","\\mathtt{A}"),("unicode","\\mttA")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL A"}
  , Record {uchar = '\120433', commands = [("base","\\mathtt{B}"),("unicode","\\mttB")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL B"}
  , Record {uchar = '\120434', commands = [("base","\\mathtt{C}"),("unicode","\\mttC")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL C"}
  , Record {uchar = '\120435', commands = [("base","\\mathtt{D}"),("unicode","\\mttD")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL D"}
  , Record {uchar = '\120436', commands = [("base","\\mathtt{E}"),("unicode","\\mttE")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL E"}
  , Record {uchar = '\120437', commands = [("base","\\mathtt{F}"),("unicode","\\mttF")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL F"}
  , Record {uchar = '\120438', commands = [("base","\\mathtt{G}"),("unicode","\\mttG")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL G"}
  , Record {uchar = '\120439', commands = [("base","\\mathtt{H}"),("unicode","\\mttH")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL H"}
  , Record {uchar = '\120440', commands = [("base","\\mathtt{I}"),("unicode","\\mttI")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL I"}
  , Record {uchar = '\120441', commands = [("base","\\mathtt{J}"),("unicode","\\mttJ")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL J"}
  , Record {uchar = '\120442', commands = [("base","\\mathtt{K}"),("unicode","\\mttK")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL K"}
  , Record {uchar = '\120443', commands = [("base","\\mathtt{L}"),("unicode","\\mttL")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL L"}
  , Record {uchar = '\120444', commands = [("base","\\mathtt{M}"),("unicode","\\mttM")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL M"}
  , Record {uchar = '\120445', commands = [("base","\\mathtt{N}"),("unicode","\\mttN")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL N"}
  , Record {uchar = '\120446', commands = [("base","\\mathtt{O}"),("unicode","\\mttO")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL O"}
  , Record {uchar = '\120447', commands = [("base","\\mathtt{P}"),("unicode","\\mttP")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL P"}
  , Record {uchar = '\120448', commands = [("base","\\mathtt{Q}"),("unicode","\\mttQ")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL Q"}
  , Record {uchar = '\120449', commands = [("base","\\mathtt{R}"),("unicode","\\mttR")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL R"}
  , Record {uchar = '\120450', commands = [("base","\\mathtt{S}"),("unicode","\\mttS")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL S"}
  , Record {uchar = '\120451', commands = [("base","\\mathtt{T}"),("unicode","\\mttT")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL T"}
  , Record {uchar = '\120452', commands = [("base","\\mathtt{U}"),("unicode","\\mttU")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL U"}
  , Record {uchar = '\120453', commands = [("base","\\mathtt{V}"),("unicode","\\mttV")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL V"}
  , Record {uchar = '\120454', commands = [("base","\\mathtt{W}"),("unicode","\\mttW")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL W"}
  , Record {uchar = '\120455', commands = [("base","\\mathtt{X}"),("unicode","\\mttX")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL X"}
  , Record {uchar = '\120456', commands = [("base","\\mathtt{Y}"),("unicode","\\mttY")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL Y"}
  , Record {uchar = '\120457', commands = [("base","\\mathtt{Z}"),("unicode","\\mttZ")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL Z"}
  , Record {uchar = '\120458', commands = [("base","\\mathtt{a}"),("unicode","\\mtta")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL A"}
  , Record {uchar = '\120459', commands = [("base","\\mathtt{b}"),("unicode","\\mttb")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL B"}
  , Record {uchar = '\120460', commands = [("base","\\mathtt{c}"),("unicode","\\mttc")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL C"}
  , Record {uchar = '\120461', commands = [("base","\\mathtt{d}"),("unicode","\\mttd")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL D"}
  , Record {uchar = '\120462', commands = [("base","\\mathtt{e}"),("unicode","\\mtte")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL E"}
  , Record {uchar = '\120463', commands = [("base","\\mathtt{f}"),("unicode","\\mttf")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL F"}
  , Record {uchar = '\120464', commands = [("base","\\mathtt{g}"),("unicode","\\mttg")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL G"}
  , Record {uchar = '\120465', commands = [("base","\\mathtt{h}"),("unicode","\\mtth")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL H"}
  , Record {uchar = '\120466', commands = [("base","\\mathtt{i}"),("unicode","\\mtti")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL I"}
  , Record {uchar = '\120467', commands = [("base","\\mathtt{j}"),("unicode","\\mttj")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL J"}
  , Record {uchar = '\120468', commands = [("base","\\mathtt{k}"),("unicode","\\mttk")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL K"}
  , Record {uchar = '\120469', commands = [("base","\\mathtt{l}"),("unicode","\\mttl")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL L"}
  , Record {uchar = '\120470', commands = [("base","\\mathtt{m}"),("unicode","\\mttm")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL M"}
  , Record {uchar = '\120471', commands = [("base","\\mathtt{n}"),("unicode","\\mttn")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL N"}
  , Record {uchar = '\120472', commands = [("base","\\mathtt{o}"),("unicode","\\mtto")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL O"}
  , Record {uchar = '\120473', commands = [("base","\\mathtt{p}"),("unicode","\\mttp")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL P"}
  , Record {uchar = '\120474', commands = [("base","\\mathtt{q}"),("unicode","\\mttq")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL Q"}
  , Record {uchar = '\120475', commands = [("base","\\mathtt{r}"),("unicode","\\mttr")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL R"}
  , Record {uchar = '\120476', commands = [("base","\\mathtt{s}"),("unicode","\\mtts")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL S"}
  , Record {uchar = '\120477', commands = [("base","\\mathtt{t}"),("unicode","\\mttt")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL T"}
  , Record {uchar = '\120478', commands = [("base","\\mathtt{u}"),("unicode","\\mttu")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL U"}
  , Record {uchar = '\120479', commands = [("base","\\mathtt{v}"),("unicode","\\mttv")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL V"}
  , Record {uchar = '\120480', commands = [("base","\\mathtt{w}"),("unicode","\\mttw")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL W"}
  , Record {uchar = '\120481', commands = [("base","\\mathtt{x}"),("unicode","\\mttx")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL X"}
  , Record {uchar = '\120482', commands = [("base","\\mathtt{y}"),("unicode","\\mtty")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL Y"}
  , Record {uchar = '\120483', commands = [("base","\\mathtt{z}"),("unicode","\\mttz")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL Z"}
  , Record {uchar = '\120484', commands = [("base","\\imath"),("unicode","\\imath")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL DOTLESS I"}
  , Record {uchar = '\120485', commands = [("base","\\jmath"),("unicode","\\jmath")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL DOTLESS J"}
  , Record {uchar = '\120488', commands = [("base",""),("unicode","\\mbfAlpha")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL ALPHA"}
  , Record {uchar = '\120489', commands = [("base",""),("unicode","\\mbfBeta")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL BETA"}
  , Record {uchar = '\120490', commands = [("base","\\mathbf{\\Gamma}"),("unicode","\\mbfGamma")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL GAMMA"}
  , Record {uchar = '\120491', commands = [("base","\\mathbf{\\Delta}"),("unicode","\\mbfDelta")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL DELTA"}
  , Record {uchar = '\120492', commands = [("base",""),("unicode","\\mbfEpsilon")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL EPSILON"}
  , Record {uchar = '\120493', commands = [("base",""),("unicode","\\mbfZeta")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL ZETA"}
  , Record {uchar = '\120494', commands = [("base",""),("unicode","\\mbfEta")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL ETA"}
  , Record {uchar = '\120495', commands = [("base","\\mathbf{\\Theta}"),("unicode","\\mbfTheta")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL THETA"}
  , Record {uchar = '\120496', commands = [("base",""),("unicode","\\mbfIota")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL IOTA"}
  , Record {uchar = '\120497', commands = [("base",""),("unicode","\\mbfKappa")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL KAPPA"}
  , Record {uchar = '\120498', commands = [("base","\\mathbf{\\Lambda}"),("unicode","\\mbfLambda")], category = Alpha, comments = "mathematical bold capital lambda"}
  , Record {uchar = '\120499', commands = [("base",""),("unicode","\\mbfMu")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL MU"}
  , Record {uchar = '\120500', commands = [("base",""),("unicode","\\mbfNu")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL NU"}
  , Record {uchar = '\120501', commands = [("base","\\mathbf{\\Xi}"),("unicode","\\mbfXi")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL XI"}
  , Record {uchar = '\120502', commands = [("base",""),("unicode","\\mbfOmicron")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL OMICRON"}
  , Record {uchar = '\120503', commands = [("base","\\mathbf{\\Pi}"),("unicode","\\mbfPi")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL PI"}
  , Record {uchar = '\120504', commands = [("base",""),("unicode","\\mbfRho")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL RHO"}
  , Record {uchar = '\120505', commands = [("base",""),("unicode","\\mbfvarTheta")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL THETA SYMBOL"}
  , Record {uchar = '\120506', commands = [("base","\\mathbf{\\Sigma}"),("unicode","\\mbfSigma")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL SIGMA"}
  , Record {uchar = '\120507', commands = [("base",""),("unicode","\\mbfTau")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL TAU"}
  , Record {uchar = '\120508', commands = [("base","\\mathbf{\\Upsilon}"),("unicode","\\mbfUpsilon")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL UPSILON"}
  , Record {uchar = '\120509', commands = [("base","\\mathbf{\\Phi}"),("unicode","\\mbfPhi")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL PHI"}
  , Record {uchar = '\120510', commands = [("base",""),("unicode","\\mbfChi")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL CHI"}
  , Record {uchar = '\120511', commands = [("base","\\mathbf{\\Psi}"),("unicode","\\mbfPsi")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL PSI"}
  , Record {uchar = '\120512', commands = [("base","\\mathbf{\\Omega}"),("unicode","\\mbfOmega")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL OMEGA"}
  , Record {uchar = '\120513', commands = [("base",""),("unicode","\\mbfnabla")], category = Ord, comments = "MATHEMATICAL BOLD NABLA"}
  , Record {uchar = '\120514', commands = [("omlmathbf","\\mathbf{\\alpha}"),("unicode","\\mbfalpha")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL ALPHA"}
  , Record {uchar = '\120515', commands = [("omlmathbf","\\mathbf{\\beta}"),("unicode","\\mbfbeta")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL BETA"}
  , Record {uchar = '\120516', commands = [("omlmathbf","\\mathbf{\\gamma}"),("unicode","\\mbfgamma")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL GAMMA"}
  , Record {uchar = '\120517', commands = [("omlmathbf","\\mathbf{\\delta}"),("unicode","\\mbfdelta")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL DELTA"}
  , Record {uchar = '\120518', commands = [("omlmathbf","\\mathbf{\\varepsilon}"),("unicode","\\mbfepsilon")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL EPSILON"}
  , Record {uchar = '\120519', commands = [("omlmathbf","\\mathbf{\\zeta}"),("unicode","\\mbfzeta")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL ZETA"}
  , Record {uchar = '\120520', commands = [("omlmathbf","\\mathbf{\\eta}"),("unicode","\\mbfeta")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL ETA"}
  , Record {uchar = '\120521', commands = [("omlmathbf","\\mathbf{\\theta}"),("unicode","\\mbftheta")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL THETA"}
  , Record {uchar = '\120522', commands = [("omlmathbf","\\mathbf{\\iota}"),("unicode","\\mbfiota")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL IOTA"}
  , Record {uchar = '\120523', commands = [("omlmathbf","\\mathbf{\\kappa}"),("unicode","\\mbfkappa")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL KAPPA"}
  , Record {uchar = '\120524', commands = [("omlmathbf","\\mathbf{\\lambda}"),("unicode","\\mbflambda")], category = Alpha, comments = "mathematical bold small lambda"}
  , Record {uchar = '\120525', commands = [("omlmathbf","\\mathbf{\\mu}"),("unicode","\\mbfmu")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL MU"}
  , Record {uchar = '\120526', commands = [("omlmathbf","\\mathbf{\\nu}"),("unicode","\\mbfnu")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL NU"}
  , Record {uchar = '\120527', commands = [("omlmathbf","\\mathbf{\\xi}"),("unicode","\\mbfxi")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL XI"}
  , Record {uchar = '\120528', commands = [("base",""),("unicode","\\mbfomicron")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL OMICRON"}
  , Record {uchar = '\120529', commands = [("omlmathbf","\\mathbf{\\pi}"),("unicode","\\mbfpi")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL PI"}
  , Record {uchar = '\120530', commands = [("omlmathbf","\\mathbf{\\rho}"),("unicode","\\mbfrho")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL RHO"}
  , Record {uchar = '\120531', commands = [("omlmathbf","\\mathbf{\\varsigma}"),("unicode","\\mbfvarsigma")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL FINAL SIGMA"}
  , Record {uchar = '\120532', commands = [("omlmathbf","\\mathbf{\\sigma}"),("unicode","\\mbfsigma")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL SIGMA"}
  , Record {uchar = '\120533', commands = [("omlmathbf","\\mathbf{\\tau}"),("unicode","\\mbftau")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL TAU"}
  , Record {uchar = '\120534', commands = [("omlmathbf","\\mathbf{\\upsilon}"),("unicode","\\mbfupsilon")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL UPSILON"}
  , Record {uchar = '\120535', commands = [("omlmathbf","\\mathbf{\\varphi}"),("unicode","\\mbfvarphi")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL PHI"}
  , Record {uchar = '\120536', commands = [("omlmathbf","\\mathbf{\\chi}"),("unicode","\\mbfchi")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL CHI"}
  , Record {uchar = '\120537', commands = [("omlmathbf","\\mathbf{\\psi}"),("unicode","\\mbfpsi")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL PSI"}
  , Record {uchar = '\120538', commands = [("omlmathbf","\\mathbf{\\omega}"),("unicode","\\mbfomega")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL OMEGA"}
  , Record {uchar = '\120539', commands = [("base",""),("unicode","\\mbfpartial")], category = Ord, comments = "MATHEMATICAL BOLD PARTIAL DIFFERENTIAL"}
  , Record {uchar = '\120540', commands = [("omlmathbf","\\mathbf{\\epsilon}"),("unicode","\\mbfvarepsilon")], category = Alpha, comments = "MATHEMATICAL BOLD EPSILON SYMBOL"}
  , Record {uchar = '\120541', commands = [("omlmathbf","\\mathbf{\\vartheta}"),("unicode","\\mbfvartheta")], category = Alpha, comments = "MATHEMATICAL BOLD THETA SYMBOL"}
  , Record {uchar = '\120542', commands = [("base",""),("unicode","\\mbfvarkappa")], category = Alpha, comments = "MATHEMATICAL BOLD KAPPA SYMBOL"}
  , Record {uchar = '\120543', commands = [("omlmathbf","\\mathbf{\\phi}"),("unicode","\\mbfphi")], category = Alpha, comments = "MATHEMATICAL BOLD PHI SYMBOL"}
  , Record {uchar = '\120544', commands = [("omlmathbf","\\mathbf{\\varrho}"),("unicode","\\mbfvarrho")], category = Alpha, comments = "MATHEMATICAL BOLD RHO SYMBOL"}
  , Record {uchar = '\120545', commands = [("omlmathbf","\\mathbf{\\varpi}"),("unicode","\\mbfvarpi")], category = Alpha, comments = "MATHEMATICAL BOLD PI SYMBOL"}
  , Record {uchar = '\120546', commands = [("base",""),("unicode","\\mitAlpha")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL ALPHA"}
  , Record {uchar = '\120547', commands = [("base",""),("unicode","\\mitBeta")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL BETA"}
  , Record {uchar = '\120548', commands = [("slantedGreek","\\Gamma"),("-fourier","\\mathit{\\Gamma}"),("unicode","\\mitGamma")], category = Alpha, comments = "= \\varGamma (amsmath fourier), MATHEMATICAL ITALIC CAPITAL GAMMA"}
  , Record {uchar = '\120549', commands = [("slantedGreek","\\Delta"),("-fourier","\\mathit{\\Delta}"),("unicode","\\mitDelta")], category = Alpha, comments = "= \\varDelta (amsmath fourier), MATHEMATICAL ITALIC CAPITAL DELTA"}
  , Record {uchar = '\120550', commands = [("base",""),("unicode","\\mitEpsilon")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL EPSILON"}
  , Record {uchar = '\120551', commands = [("base",""),("unicode","\\mitZeta")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL ZETA"}
  , Record {uchar = '\120552', commands = [("base",""),("unicode","\\mitEta")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL ETA"}
  , Record {uchar = '\120553', commands = [("slantedGreek","\\Theta"),("-fourier","\\mathit{\\Theta}"),("unicode","\\mitTheta")], category = Alpha, comments = "= \\varTheta (amsmath fourier), MATHEMATICAL ITALIC CAPITAL THETA"}
  , Record {uchar = '\120554', commands = [("base",""),("unicode","\\mitIota")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL IOTA"}
  , Record {uchar = '\120555', commands = [("base",""),("unicode","\\mitKappa")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL KAPPA"}
  , Record {uchar = '\120556', commands = [("slantedGreek","\\Lambda"),("-fourier","\\mathit{\\Lambda}"),("unicode","\\mitLambda")], category = Alpha, comments = "= \\varLambda (amsmath fourier), mathematical italic capital lambda"}
  , Record {uchar = '\120557', commands = [("base",""),("unicode","\\mitMu")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL MU"}
  , Record {uchar = '\120558', commands = [("base",""),("unicode","\\mitNu")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL NU"}
  , Record {uchar = '\120559', commands = [("slantedGreek","\\Xi"),("-fourier","\\mathit{\\Xi}"),("unicode","\\mitXi")], category = Alpha, comments = "= \\varXi (amsmath fourier), MATHEMATICAL ITALIC CAPITAL XI"}
  , Record {uchar = '\120560', commands = [("base",""),("unicode","\\mitOmicron")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL OMICRON"}
  , Record {uchar = '\120561', commands = [("slantedGreek","\\Pi"),("-fourier","\\mathit{\\Pi}"),("unicode","\\mitPi")], category = Alpha, comments = "= \\varPi (amsmath fourier), MATHEMATICAL ITALIC CAPITAL PI"}
  , Record {uchar = '\120562', commands = [("base",""),("unicode","\\mitRho")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL RHO"}
  , Record {uchar = '\120563', commands = [("base",""),("unicode","\\mitvarTheta")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL THETA SYMBOL"}
  , Record {uchar = '\120564', commands = [("slantedGreek","\\Sigma"),("-fourier","\\mathit{\\Sigma}"),("unicode","\\mitSigma")], category = Alpha, comments = "= \\varSigma (amsmath fourier), MATHEMATICAL ITALIC CAPITAL SIGMA"}
  , Record {uchar = '\120565', commands = [("base",""),("unicode","\\mitTau")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL TAU"}
  , Record {uchar = '\120566', commands = [("slantedGreek","\\Upsilon"),("-fourier","\\mathit{\\Upsilon}"),("unicode","\\mitUpsilon")], category = Alpha, comments = "= \\varUpsilon (amsmath fourier), MATHEMATICAL ITALIC CAPITAL UPSILON"}
  , Record {uchar = '\120567', commands = [("slantedGreek","\\Phi"),("-fourier","\\mathit{\\Phi}"),("unicode","\\mitPhi")], category = Alpha, comments = "= \\varPhi (amsmath fourier), MATHEMATICAL ITALIC CAPITAL PHI"}
  , Record {uchar = '\120568', commands = [("base",""),("unicode","\\mitChi")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL CHI"}
  , Record {uchar = '\120569', commands = [("slantedGreek","\\Psi"),("-fourier","\\mathit{\\Psi}"),("unicode","\\mitPsi")], category = Alpha, comments = "= \\varPsi (amsmath fourier), MATHEMATICAL ITALIC CAPITAL PSI"}
  , Record {uchar = '\120570', commands = [("slantedGreek","\\Omega"),("-fourier","\\mathit{\\Omega}"),("unicode","\\mitOmega")], category = Alpha, comments = "= \\varOmega (amsmath fourier), MATHEMATICAL ITALIC CAPITAL OMEGA"}
  , Record {uchar = '\120571', commands = [("base",""),("unicode","\\mitnabla")], category = Ord, comments = "MATHEMATICAL ITALIC NABLA"}
  , Record {uchar = '\120572', commands = [("base","\\alpha"),("omlmathit","\\mathit{\\alpha}"),("unicode","\\mitalpha")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL ALPHA"}
  , Record {uchar = '\120573', commands = [("base","\\beta"),("omlmathit","\\mathit{\\beta}"),("unicode","\\mitbeta")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL BETA"}
  , Record {uchar = '\120574', commands = [("base","\\gamma"),("omlmathit","\\mathit{\\gamma}"),("unicode","\\mitgamma")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL GAMMA"}
  , Record {uchar = '\120575', commands = [("base","\\delta"),("omlmathit","\\mathit{\\delta}"),("unicode","\\mitdelta")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL DELTA"}
  , Record {uchar = '\120576', commands = [("base","\\varepsilon"),("omlmathit","\\mathit{\\varepsilon}"),("unicode","\\mitepsilon")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL EPSILON"}
  , Record {uchar = '\120577', commands = [("base","\\zeta"),("omlmathit","\\mathit{\\zeta}"),("unicode","\\mitzeta")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL ZETA"}
  , Record {uchar = '\120578', commands = [("base","\\eta"),("omlmathit","\\mathit{\\eta}"),("unicode","\\miteta")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL ETA"}
  , Record {uchar = '\120579', commands = [("base","\\theta"),("omlmathit","\\mathit{\\theta}"),("unicode","\\mittheta")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL THETA"}
  , Record {uchar = '\120580', commands = [("base","\\iota"),("omlmathit","\\mathit{\\iota}"),("unicode","\\mitiota")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL IOTA"}
  , Record {uchar = '\120581', commands = [("base","\\kappa"),("omlmathit","\\mathit{\\kappa}"),("unicode","\\mitkappa")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL KAPPA"}
  , Record {uchar = '\120582', commands = [("base","\\lambda"),("omlmathit","\\mathit{\\lambda}"),("unicode","\\mitlambda")], category = Alpha, comments = "mathematical italic small lambda"}
  , Record {uchar = '\120583', commands = [("base","\\mu"),("omlmathit","\\mathit{\\mu}"),("unicode","\\mitmu")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL MU"}
  , Record {uchar = '\120584', commands = [("base","\\nu"),("omlmathit","\\mathit{\\nu}"),("unicode","\\mitnu")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL NU"}
  , Record {uchar = '\120585', commands = [("base","\\xi"),("omlmathit","\\mathit{\\xi}"),("unicode","\\mitxi")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL XI"}
  , Record {uchar = '\120586', commands = [("base",""),("unicode","\\mitomicron")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL OMICRON"}
  , Record {uchar = '\120587', commands = [("base","\\pi"),("omlmathit","\\mathit{\\pi}"),("unicode","\\mitpi")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL PI"}
  , Record {uchar = '\120588', commands = [("base","\\rho"),("omlmathit","\\mathit{\\rho}"),("unicode","\\mitrho")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL RHO"}
  , Record {uchar = '\120589', commands = [("base","\\varsigma"),("omlmathit","\\mathit{\\varsigma}"),("unicode","\\mitvarsigma")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL FINAL SIGMA"}
  , Record {uchar = '\120590', commands = [("base","\\sigma"),("omlmathit","\\mathit{\\sigma}"),("unicode","\\mitsigma")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL SIGMA"}
  , Record {uchar = '\120591', commands = [("base","\\tau"),("omlmathit","\\mathit{\\tau}"),("unicode","\\mittau")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL TAU"}
  , Record {uchar = '\120592', commands = [("base","\\upsilon"),("omlmathit","\\mathit{\\upsilon}"),("unicode","\\mitupsilon")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL UPSILON"}
  , Record {uchar = '\120593', commands = [("base","\\varphi"),("omlmathit","\\mathit{\\varphi}"),("unicode","\\mitphi")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL PHI"}
  , Record {uchar = '\120594', commands = [("base","\\chi"),("omlmathit","\\mathit{\\chi}"),("unicode","\\mitchi")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL CHI"}
  , Record {uchar = '\120595', commands = [("base","\\psi"),("omlmathit","\\mathit{\\psi}"),("unicode","\\mitpsi")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL PSI"}
  , Record {uchar = '\120596', commands = [("base","\\omega"),("omlmathit","\\mathit{\\omega}"),("unicode","\\mitomega")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL OMEGA"}
  , Record {uchar = '\120597', commands = [("base","\\partial"),("omlmathit","\\mathit{\\partial}"),("unicode","\\mitpartial")], category = Ord, comments = "MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL"}
  , Record {uchar = '\120598', commands = [("base","\\epsilon"),("omlmathit","\\mathit{\\epsilon}"),("unicode","\\mitvarepsilon")], category = Alpha, comments = "MATHEMATICAL ITALIC EPSILON SYMBOL"}
  , Record {uchar = '\120599', commands = [("base","\\vartheta"),("omlmathit","\\mathit{\\vartheta}"),("unicode","\\mitvartheta")], category = Alpha, comments = "MATHEMATICAL ITALIC THETA SYMBOL"}
  , Record {uchar = '\120600', commands = [("amssymb","\\varkappa"),("unicode","\\mitvarkappa")], category = Alpha, comments = "MATHEMATICAL ITALIC KAPPA SYMBOL"}
  , Record {uchar = '\120601', commands = [("base","\\phi"),("omlmathit","\\mathit{\\phi}"),("unicode","\\mitvarphi")], category = Alpha, comments = "MATHEMATICAL ITALIC PHI SYMBOL"}
  , Record {uchar = '\120602', commands = [("base","\\varrho"),("omlmathit","\\mathit{\\varrho}"),("unicode","\\mitvarrho")], category = Alpha, comments = "MATHEMATICAL ITALIC RHO SYMBOL"}
  , Record {uchar = '\120603', commands = [("base","\\varpi"),("omlmathit","\\mathit{\\varpi}"),("unicode","\\mitvarpi")], category = Alpha, comments = "MATHEMATICAL ITALIC PI SYMBOL"}
  , Record {uchar = '\120604', commands = [("base",""),("unicode","\\mbfitAlpha")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL ALPHA"}
  , Record {uchar = '\120605', commands = [("base",""),("unicode","\\mbfitBeta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL BETA"}
  , Record {uchar = '\120606', commands = [("isomath","\\mathbfit{\\Gamma}"),("fixmath","\\mathbold{\\Gamma}"),("unicode","\\mbfitGamma")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL GAMMA"}
  , Record {uchar = '\120607', commands = [("isomath","\\mathbfit{\\Delta}"),("fixmath","\\mathbold{\\Delta}"),("unicode","\\mbfitDelta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL DELTA"}
  , Record {uchar = '\120608', commands = [("base",""),("unicode","\\mbfitEpsilon")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL EPSILON"}
  , Record {uchar = '\120609', commands = [("base",""),("unicode","\\mbfitZeta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL ZETA"}
  , Record {uchar = '\120610', commands = [("base",""),("unicode","\\mbfitEta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL ETA"}
  , Record {uchar = '\120611', commands = [("isomath","\\mathbfit{\\Theta}"),("fixmath","\\mathbold{\\Theta}"),("unicode","\\mbfitTheta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL THETA"}
  , Record {uchar = '\120612', commands = [("base",""),("unicode","\\mbfitIota")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL IOTA"}
  , Record {uchar = '\120613', commands = [("base",""),("unicode","\\mbfitKappa")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL KAPPA"}
  , Record {uchar = '\120614', commands = [("isomath","\\mathbfit{\\Lambda}"),("fixmath","\\mathbold{\\Lambda}"),("unicode","\\mbfitLambda")], category = Alpha, comments = "mathematical bold italic capital lambda"}
  , Record {uchar = '\120615', commands = [("base",""),("unicode","\\mbfitMu")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL MU"}
  , Record {uchar = '\120616', commands = [("base",""),("unicode","\\mbfitNu")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL NU"}
  , Record {uchar = '\120617', commands = [("isomath","\\mathbfit{\\Xi}"),("fixmath","\\mathbold{\\Xi}"),("unicode","\\mbfitXi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL XI"}
  , Record {uchar = '\120618', commands = [("base",""),("unicode","\\mbfitOmicron")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL OMICRON"}
  , Record {uchar = '\120619', commands = [("isomath","\\mathbfit{\\Pi}"),("fixmath","\\mathbold{\\Pi}"),("unicode","\\mbfitPi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL PI"}
  , Record {uchar = '\120620', commands = [("base",""),("unicode","\\mbfitRho")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL RHO"}
  , Record {uchar = '\120621', commands = [("base",""),("unicode","\\mbfitvarTheta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL THETA SYMBOL"}
  , Record {uchar = '\120622', commands = [("isomath","\\mathbfit{\\Sigma}"),("fixmath","\\mathbold{\\Sigma}"),("unicode","\\mbfitSigma")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL SIGMA"}
  , Record {uchar = '\120623', commands = [("base",""),("unicode","\\mbfitTau")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL TAU"}
  , Record {uchar = '\120624', commands = [("isomath","\\mathbfit{\\Upsilon}"),("fixmath","\\mathbold{\\Upsilon}"),("unicode","\\mbfitUpsilon")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL UPSILON"}
  , Record {uchar = '\120625', commands = [("isomath","\\mathbfit{\\Phi}"),("fixmath","\\mathbold{\\Phi}"),("unicode","\\mbfitPhi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL PHI"}
  , Record {uchar = '\120626', commands = [("base",""),("unicode","\\mbfitChi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL CHI"}
  , Record {uchar = '\120627', commands = [("isomath","\\mathbfit{\\Psi}"),("fixmath","\\mathbold{\\Psi}"),("unicode","\\mbfitPsi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL PSI"}
  , Record {uchar = '\120628', commands = [("isomath","\\mathbfit{\\Omega}"),("fixmath","\\mathbold{\\Omega}"),("unicode","\\mbfitOmega")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL OMEGA"}
  , Record {uchar = '\120629', commands = [("base",""),("unicode","\\mbfitnabla")], category = Ord, comments = "MATHEMATICAL BOLD ITALIC NABLA"}
  , Record {uchar = '\120630', commands = [("isomath","\\mathbfit{\\alpha}"),("fixmath","\\mathbold{\\alpha}"),("unicode","\\mbfitalpha")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL ALPHA"}
  , Record {uchar = '\120631', commands = [("isomath","\\mathbfit{\\beta}"),("fixmath","\\mathbold{\\beta}"),("unicode","\\mbfitbeta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL BETA"}
  , Record {uchar = '\120632', commands = [("isomath","\\mathbfit{\\gamma}"),("fixmath","\\mathbold{\\gamma}"),("unicode","\\mbfitgamma")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL GAMMA"}
  , Record {uchar = '\120633', commands = [("isomath","\\mathbfit{\\delta}"),("fixmath","\\mathbold{\\delta}"),("unicode","\\mbfitdelta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL DELTA"}
  , Record {uchar = '\120634', commands = [("isomath","\\mathbfit{\\varepsilon}"),("fixmath","\\mathbold{\\varepsilon}"),("unicode","\\mbfitepsilon")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL EPSILON"}
  , Record {uchar = '\120635', commands = [("isomath","\\mathbfit{\\zeta}"),("fixmath","\\mathbold{\\zeta}"),("unicode","\\mbfitzeta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL ZETA"}
  , Record {uchar = '\120636', commands = [("isomath","\\mathbfit{\\eta}"),("fixmath","\\mathbold{\\eta}"),("unicode","\\mbfiteta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL ETA"}
  , Record {uchar = '\120637', commands = [("isomath","\\mathbfit{\\theta}"),("fixmath","\\mathbold{\\theta}"),("unicode","\\mbfittheta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL THETA"}
  , Record {uchar = '\120638', commands = [("isomath","\\mathbfit{\\iota}"),("fixmath","\\mathbold{\\iota}"),("unicode","\\mbfitiota")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL IOTA"}
  , Record {uchar = '\120639', commands = [("isomath","\\mathbfit{\\kappa}"),("fixmath","\\mathbold{\\kappa}"),("unicode","\\mbfitkappa")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL KAPPA"}
  , Record {uchar = '\120640', commands = [("isomath","\\mathbfit{\\lambda}"),("fixmath","\\mathbold{\\lambda}"),("unicode","\\mbfitlambda")], category = Alpha, comments = "mathematical bold italic small lambda"}
  , Record {uchar = '\120641', commands = [("isomath","\\mathbfit{\\mu}"),("fixmath","\\mathbold{\\mu}"),("unicode","\\mbfitmu")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL MU"}
  , Record {uchar = '\120642', commands = [("isomath","\\mathbfit{\\nu}"),("fixmath","\\mathbold{\\nu}"),("unicode","\\mbfitnu")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL NU"}
  , Record {uchar = '\120643', commands = [("isomath","\\mathbfit{\\xi}"),("fixmath","\\mathbold{\\xi}"),("unicode","\\mbfitxi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL XI"}
  , Record {uchar = '\120644', commands = [("base",""),("unicode","\\mbfitomicron")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL OMICRON"}
  , Record {uchar = '\120645', commands = [("isomath","\\mathbfit{\\pi}"),("fixmath","\\mathbold{\\pi}"),("unicode","\\mbfitpi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL PI"}
  , Record {uchar = '\120646', commands = [("isomath","\\mathbfit{\\rho}"),("fixmath","\\mathbold{\\rho}"),("unicode","\\mbfitrho")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL RHO"}
  , Record {uchar = '\120647', commands = [("isomath","\\mathbfit{\\varsigma}"),("fixmath","\\mathbold{\\varsigma}"),("unicode","\\mbfitvarsigma")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL FINAL SIGMA"}
  , Record {uchar = '\120648', commands = [("isomath","\\mathbfit{\\sigma}"),("fixmath","\\mathbold{\\sigma}"),("unicode","\\mbfitsigma")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL SIGMA"}
  , Record {uchar = '\120649', commands = [("isomath","\\mathbfit{\\tau}"),("fixmath","\\mathbold{\\tau}"),("unicode","\\mbfittau")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL TAU"}
  , Record {uchar = '\120650', commands = [("isomath","\\mathbfit{\\upsilon}"),("fixmath","\\mathbold{\\upsilon}"),("unicode","\\mbfitupsilon")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL UPSILON"}
  , Record {uchar = '\120651', commands = [("isomath","\\mathbfit{\\varphi}"),("fixmath","\\mathbold{\\varphi}"),("unicode","\\mbfitphi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL PHI"}
  , Record {uchar = '\120652', commands = [("isomath","\\mathbfit{\\chi}"),("fixmath","\\mathbold{\\chi}"),("unicode","\\mbfitchi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL CHI"}
  , Record {uchar = '\120653', commands = [("isomath","\\mathbfit{\\psi}"),("fixmath","\\mathbold{\\psi}"),("unicode","\\mbfitpsi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL PSI"}
  , Record {uchar = '\120654', commands = [("isomath","\\mathbfit{\\omega}"),("fixmath","\\mathbold{\\omega}"),("unicode","\\mbfitomega")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL OMEGA"}
  , Record {uchar = '\120655', commands = [("base",""),("unicode","\\mbfitpartial")], category = Ord, comments = "MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL"}
  , Record {uchar = '\120656', commands = [("isomath","\\mathbfit{\\epsilon}"),("fixmath","\\mathbold{\\epsilon}"),("unicode","\\mbfitvarepsilon")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC EPSILON SYMBOL"}
  , Record {uchar = '\120657', commands = [("isomath","\\mathbfit{\\vartheta}"),("fixmath","\\mathbold{\\vartheta}"),("unicode","\\mbfitvartheta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC THETA SYMBOL"}
  , Record {uchar = '\120658', commands = [("base",""),("unicode","\\mbfitvarkappa")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC KAPPA SYMBOL"}
  , Record {uchar = '\120659', commands = [("isomath","\\mathbfit{\\phi}"),("fixmath","\\mathbold{\\phi}"),("unicode","\\mbfitvarphi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC PHI SYMBOL"}
  , Record {uchar = '\120660', commands = [("isomath","\\mathbfit{\\varrho}"),("fixmath","\\mathbold{\\varrho}"),("unicode","\\mbfitvarrho")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC RHO SYMBOL"}
  , Record {uchar = '\120661', commands = [("isomath","\\mathbfit{\\varpi}"),("fixmath","\\mathbold{\\varpi}"),("unicode","\\mbfitvarpi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC PI SYMBOL"}
  , Record {uchar = '\120662', commands = [("base",""),("unicode","\\mbfsansAlpha")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL ALPHA"}
  , Record {uchar = '\120663', commands = [("base",""),("unicode","\\mbfsansBeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL BETA"}
  , Record {uchar = '\120664', commands = [("mathsfbf","\\mathsfbf{\\Gamma}"),("unicode","\\mbfsansGamma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL GAMMA"}
  , Record {uchar = '\120665', commands = [("mathsfbf","\\mathsfbf{\\Delta}"),("unicode","\\mbfsansDelta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL DELTA"}
  , Record {uchar = '\120666', commands = [("base",""),("unicode","\\mbfsansEpsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL EPSILON"}
  , Record {uchar = '\120667', commands = [("base",""),("unicode","\\mbfsansZeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL ZETA"}
  , Record {uchar = '\120668', commands = [("base",""),("unicode","\\mbfsansEta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL ETA"}
  , Record {uchar = '\120669', commands = [("mathsfbf","\\mathsfbf{\\Theta}"),("unicode","\\mbfsansTheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA"}
  , Record {uchar = '\120670', commands = [("base",""),("unicode","\\mbfsansIota")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL IOTA"}
  , Record {uchar = '\120671', commands = [("base",""),("unicode","\\mbfsansKappa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL KAPPA"}
  , Record {uchar = '\120672', commands = [("mathsfbf","\\mathsfbf{\\Lambda}"),("unicode","\\mbfsansLambda")], category = Alpha, comments = "mathematical sans-serif bold capital lambda"}
  , Record {uchar = '\120673', commands = [("base",""),("unicode","\\mbfsansMu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL MU"}
  , Record {uchar = '\120674', commands = [("base",""),("unicode","\\mbfsansNu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL NU"}
  , Record {uchar = '\120675', commands = [("mathsfbf","\\mathsfbf{\\Xi}"),("unicode","\\mbfsansXi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL XI"}
  , Record {uchar = '\120676', commands = [("base",""),("unicode","\\mbfsansOmicron")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL OMICRON"}
  , Record {uchar = '\120677', commands = [("mathsfbf","\\mathsfbf{\\Pi}"),("unicode","\\mbfsansPi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL PI"}
  , Record {uchar = '\120678', commands = [("base",""),("unicode","\\mbfsansRho")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL RHO"}
  , Record {uchar = '\120679', commands = [("base",""),("unicode","\\mbfsansvarTheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA SYMBOL"}
  , Record {uchar = '\120680', commands = [("mathsfbf","\\mathsfbf{\\Sigma}"),("unicode","\\mbfsansSigma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL SIGMA"}
  , Record {uchar = '\120681', commands = [("base",""),("unicode","\\mbfsansTau")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL TAU"}
  , Record {uchar = '\120682', commands = [("mathsfbf","\\mathsfbf{\\Upsilon}"),("unicode","\\mbfsansUpsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL UPSILON"}
  , Record {uchar = '\120683', commands = [("mathsfbf","\\mathsfbf{\\Phi}"),("unicode","\\mbfsansPhi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL PHI"}
  , Record {uchar = '\120684', commands = [("base",""),("unicode","\\mbfsansChi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL CHI"}
  , Record {uchar = '\120685', commands = [("mathsfbf","\\mathsfbf{\\Psi}"),("unicode","\\mbfsansPsi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL PSI"}
  , Record {uchar = '\120686', commands = [("mathsfbf","\\mathsfbf{\\Omega}"),("unicode","\\mbfsansOmega")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA"}
  , Record {uchar = '\120687', commands = [("base",""),("unicode","\\mbfsansnabla")], category = Ord, comments = "MATHEMATICAL SANS-SERIF BOLD NABLA"}
  , Record {uchar = '\120688', commands = [("omlmathsfbf","\\mathsfbf{\\alpha}"),("unicode","\\mbfsansalpha")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA"}
  , Record {uchar = '\120689', commands = [("omlmathsfbf","\\mathsfbf{\\beta}"),("unicode","\\mbfsansbeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL BETA"}
  , Record {uchar = '\120690', commands = [("omlmathsfbf","\\mathsfbf{\\gamma}"),("unicode","\\mbfsansgamma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL GAMMA"}
  , Record {uchar = '\120691', commands = [("omlmathsfbf","\\mathsfbf{\\delta}"),("unicode","\\mbfsansdelta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL DELTA"}
  , Record {uchar = '\120692', commands = [("omlmathsfbf","\\mathsfbf{\\varepsilon}"),("unicode","\\mbfsansepsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL EPSILON"}
  , Record {uchar = '\120693', commands = [("omlmathsfbf","\\mathsfbf{\\zeta}"),("unicode","\\mbfsanszeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL ZETA"}
  , Record {uchar = '\120694', commands = [("omlmathsfbf","\\mathsfbf{\\eta}"),("unicode","\\mbfsanseta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL ETA"}
  , Record {uchar = '\120695', commands = [("omlmathsfbf","\\mathsfbf{\\theta}"),("unicode","\\mbfsanstheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL THETA"}
  , Record {uchar = '\120696', commands = [("omlmathsfbf","\\mathsfbf{\\iota}"),("unicode","\\mbfsansiota")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL IOTA"}
  , Record {uchar = '\120697', commands = [("omlmathsfbf","\\mathsfbf{\\kappa}"),("unicode","\\mbfsanskappa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL KAPPA"}
  , Record {uchar = '\120698', commands = [("omlmathsfbf","\\mathsfbf{\\lambda}"),("unicode","\\mbfsanslambda")], category = Alpha, comments = "mathematical sans-serif bold small lambda"}
  , Record {uchar = '\120699', commands = [("omlmathsfbf","\\mathsfbf{\\mu}"),("unicode","\\mbfsansmu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL MU"}
  , Record {uchar = '\120700', commands = [("omlmathsfbf","\\mathsfbf{\\nu}"),("unicode","\\mbfsansnu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL NU"}
  , Record {uchar = '\120701', commands = [("omlmathsfbf","\\mathsfbf{\\xi}"),("unicode","\\mbfsansxi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL XI"}
  , Record {uchar = '\120702', commands = [("base",""),("unicode","\\mbfsansomicron")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL OMICRON"}
  , Record {uchar = '\120703', commands = [("omlmathsfbf","\\mathsfbf{\\pi}"),("unicode","\\mbfsanspi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL PI"}
  , Record {uchar = '\120704', commands = [("omlmathsfbf","\\mathsfbf{\\rho}"),("unicode","\\mbfsansrho")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL RHO"}
  , Record {uchar = '\120705', commands = [("omlmathsfbf","\\mathsfbf{\\varsigma}"),("unicode","\\mbfsansvarsigma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL FINAL SIGMA"}
  , Record {uchar = '\120706', commands = [("omlmathsfbf","\\mathsfbf{\\sigma}"),("unicode","\\mbfsanssigma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL SIGMA"}
  , Record {uchar = '\120707', commands = [("omlmathsfbf","\\mathsfbf{\\tau}"),("unicode","\\mbfsanstau")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL TAU"}
  , Record {uchar = '\120708', commands = [("omlmathsfbf","\\mathsfbf{\\upsilon}"),("unicode","\\mbfsansupsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL UPSILON"}
  , Record {uchar = '\120709', commands = [("omlmathsfbf","\\mathsfbf{\\varphi}"),("unicode","\\mbfsansphi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL PHI"}
  , Record {uchar = '\120710', commands = [("omlmathsfbf","\\mathsfbf{\\chi}"),("unicode","\\mbfsanschi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL CHI"}
  , Record {uchar = '\120711', commands = [("omlmathsfbf","\\mathsfbf{\\psi}"),("unicode","\\mbfsanspsi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL PSI"}
  , Record {uchar = '\120712', commands = [("omlmathsfbf","\\mathsfbf{\\omega}"),("unicode","\\mbfsansomega")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA"}
  , Record {uchar = '\120713', commands = [("base",""),("unicode","\\mbfsanspartial")], category = Ord, comments = "MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL"}
  , Record {uchar = '\120714', commands = [("omlmathsfbf","\\mathsfbf{\\epsilon}"),("unicode","\\mbfsansvarepsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL"}
  , Record {uchar = '\120715', commands = [("omlmathsfbf","\\mathsfbf{\\vartheta}"),("unicode","\\mbfsansvartheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD THETA SYMBOL"}
  , Record {uchar = '\120716', commands = [("base",""),("unicode","\\mbfsansvarkappa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD KAPPA SYMBOL"}
  , Record {uchar = '\120717', commands = [("omlmathsfbf","\\mathsfbf{\\phi}"),("unicode","\\mbfsansvarphi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD PHI SYMBOL"}
  , Record {uchar = '\120718', commands = [("omlmathsfbf","\\mathsfbf{\\varrho}"),("unicode","\\mbfsansvarrho")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD RHO SYMBOL"}
  , Record {uchar = '\120719', commands = [("omlmathsfbf","\\mathsfbf{\\varpi}"),("unicode","\\mbfsansvarpi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD PI SYMBOL"}
  , Record {uchar = '\120720', commands = [("base",""),("unicode","\\mbfitsansAlpha")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ALPHA"}
  , Record {uchar = '\120721', commands = [("base",""),("unicode","\\mbfitsansBeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL BETA"}
  , Record {uchar = '\120722', commands = [("isomath","\\mathsfbfit{\\Gamma}"),("unicode","\\mbfitsansGamma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL GAMMA"}
  , Record {uchar = '\120723', commands = [("isomath","\\mathsfbfit{\\Delta}"),("unicode","\\mbfitsansDelta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL DELTA"}
  , Record {uchar = '\120724', commands = [("base",""),("unicode","\\mbfitsansEpsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL EPSILON"}
  , Record {uchar = '\120725', commands = [("base",""),("unicode","\\mbfitsansZeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ZETA"}
  , Record {uchar = '\120726', commands = [("base",""),("unicode","\\mbfitsansEta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ETA"}
  , Record {uchar = '\120727', commands = [("isomath","\\mathsfbfit{\\Theta}"),("unicode","\\mbfitsansTheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA"}
  , Record {uchar = '\120728', commands = [("base",""),("unicode","\\mbfitsansIota")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL IOTA"}
  , Record {uchar = '\120729', commands = [("base",""),("unicode","\\mbfitsansKappa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL KAPPA"}
  , Record {uchar = '\120730', commands = [("isomath","\\mathsfbfit{\\Lambda}"),("unicode","\\mbfitsansLambda")], category = Alpha, comments = "mathematical sans-serif bold italic capital lambda"}
  , Record {uchar = '\120731', commands = [("base",""),("unicode","\\mbfitsansMu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL MU"}
  , Record {uchar = '\120732', commands = [("base",""),("unicode","\\mbfitsansNu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL NU"}
  , Record {uchar = '\120733', commands = [("isomath","\\mathsfbfit{\\Xi}"),("unicode","\\mbfitsansXi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL XI"}
  , Record {uchar = '\120734', commands = [("base",""),("unicode","\\mbfitsansOmicron")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMICRON"}
  , Record {uchar = '\120735', commands = [("isomath","\\mathsfbfit{\\Pi}"),("unicode","\\mbfitsansPi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PI"}
  , Record {uchar = '\120736', commands = [("base",""),("unicode","\\mbfitsansRho")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL RHO"}
  , Record {uchar = '\120737', commands = [("base",""),("unicode","\\mbfitsansvarTheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA SYMBOL"}
  , Record {uchar = '\120738', commands = [("isomath","\\mathsfbfit{\\Sigma}"),("unicode","\\mbfitsansSigma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL SIGMA"}
  , Record {uchar = '\120739', commands = [("base",""),("unicode","\\mbfitsansTau")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL TAU"}
  , Record {uchar = '\120740', commands = [("isomath","\\mathsfbfit{\\Upsilon}"),("unicode","\\mbfitsansUpsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL UPSILON"}
  , Record {uchar = '\120741', commands = [("isomath","\\mathsfbfit{\\Phi}"),("unicode","\\mbfitsansPhi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PHI"}
  , Record {uchar = '\120742', commands = [("base",""),("unicode","\\mbfitsansChi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL CHI"}
  , Record {uchar = '\120743', commands = [("isomath","\\mathsfbfit{\\Psi}"),("unicode","\\mbfitsansPsi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PSI"}
  , Record {uchar = '\120744', commands = [("isomath","\\mathsfbfit{\\Omega}"),("unicode","\\mbfitsansOmega")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA"}
  , Record {uchar = '\120745', commands = [("base",""),("unicode","\\mbfitsansnabla")], category = Ord, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA"}
  , Record {uchar = '\120746', commands = [("isomath","\\mathsfbfit{\\alpha}"),("unicode","\\mbfitsansalpha")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA"}
  , Record {uchar = '\120747', commands = [("isomath","\\mathsfbfit{\\beta}"),("unicode","\\mbfitsansbeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL BETA"}
  , Record {uchar = '\120748', commands = [("isomath","\\mathsfbfit{\\gamma}"),("unicode","\\mbfitsansgamma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL GAMMA"}
  , Record {uchar = '\120749', commands = [("isomath","\\mathsfbfit{\\delta}"),("unicode","\\mbfitsansdelta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL DELTA"}
  , Record {uchar = '\120750', commands = [("isomath","\\mathsfbfit{\\varepsilon}"),("unicode","\\mbfitsansepsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL EPSILON"}
  , Record {uchar = '\120751', commands = [("isomath","\\mathsfbfit{\\zeta}"),("unicode","\\mbfitsanszeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ZETA"}
  , Record {uchar = '\120752', commands = [("isomath","\\mathsfbfit{\\eta}"),("unicode","\\mbfitsanseta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ETA"}
  , Record {uchar = '\120753', commands = [("isomath","\\mathsfbfit{\\theta}"),("unicode","\\mbfitsanstheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL THETA"}
  , Record {uchar = '\120754', commands = [("isomath","\\mathsfbfit{\\iota}"),("unicode","\\mbfitsansiota")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL IOTA"}
  , Record {uchar = '\120755', commands = [("isomath","\\mathsfbfit{\\kappa}"),("unicode","\\mbfitsanskappa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL KAPPA"}
  , Record {uchar = '\120756', commands = [("isomath","\\mathsfbfit{\\lambda}"),("unicode","\\mbfitsanslambda")], category = Alpha, comments = "mathematical sans-serif bold italic small lambda"}
  , Record {uchar = '\120757', commands = [("isomath","\\mathsfbfit{\\mu}"),("unicode","\\mbfitsansmu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL MU"}
  , Record {uchar = '\120758', commands = [("isomath","\\mathsfbfit{\\nu}"),("unicode","\\mbfitsansnu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL NU"}
  , Record {uchar = '\120759', commands = [("isomath","\\mathsfbfit{\\xi}"),("unicode","\\mbfitsansxi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL XI"}
  , Record {uchar = '\120760', commands = [("base",""),("unicode","\\mbfitsansomicron")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMICRON"}
  , Record {uchar = '\120761', commands = [("isomath","\\mathsfbfit{\\pi}"),("unicode","\\mbfitsanspi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PI"}
  , Record {uchar = '\120762', commands = [("isomath","\\mathsfbfit{\\rho}"),("unicode","\\mbfitsansrho")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL RHO"}
  , Record {uchar = '\120763', commands = [("isomath","\\mathsfbfit{\\varsigma}"),("unicode","\\mbfitsansvarsigma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL FINAL SIGMA"}
  , Record {uchar = '\120764', commands = [("isomath","\\mathsfbfit{\\sigma}"),("unicode","\\mbfitsanssigma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL SIGMA"}
  , Record {uchar = '\120765', commands = [("isomath","\\mathsfbfit{\\tau}"),("unicode","\\mbfitsanstau")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL TAU"}
  , Record {uchar = '\120766', commands = [("isomath","\\mathsfbfit{\\upsilon}"),("unicode","\\mbfitsansupsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL UPSILON"}
  , Record {uchar = '\120767', commands = [("isomath","\\mathsfbfit{\\varphi}"),("unicode","\\mbfitsansphi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PHI"}
  , Record {uchar = '\120768', commands = [("isomath","\\mathsfbfit{\\chi}"),("unicode","\\mbfitsanschi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL CHI"}
  , Record {uchar = '\120769', commands = [("isomath","\\mathsfbfit{\\psi}"),("unicode","\\mbfitsanspsi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PSI"}
  , Record {uchar = '\120770', commands = [("isomath","\\mathsfbfit{\\omega}"),("unicode","\\mbfitsansomega")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA"}
  , Record {uchar = '\120771', commands = [("base",""),("unicode","\\mbfitsanspartial")], category = Ord, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL"}
  , Record {uchar = '\120772', commands = [("isomath","\\mathsfbfit{\\epsilon}"),("unicode","\\mbfitsansvarepsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL"}
  , Record {uchar = '\120773', commands = [("isomath","\\mathsfbfit{\\vartheta}"),("unicode","\\mbfitsansvartheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC THETA SYMBOL"}
  , Record {uchar = '\120774', commands = [("base",""),("unicode","\\mbfitsansvarkappa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC KAPPA SYMBOL"}
  , Record {uchar = '\120775', commands = [("isomath","\\mathsfbfit{\\phi}"),("unicode","\\mbfitsansvarphi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC PHI SYMBOL"}
  , Record {uchar = '\120776', commands = [("isomath","\\mathsfbfit{\\varrho}"),("unicode","\\mbfitsansvarrho")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC RHO SYMBOL"}
  , Record {uchar = '\120777', commands = [("isomath","\\mathsfbfit{\\varpi}"),("unicode","\\mbfitsansvarpi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC PI SYMBOL"}
  , Record {uchar = '\120778', commands = [("base",""),("unicode","\\mbfDigamma")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL DIGAMMA"}
  , Record {uchar = '\120779', commands = [("base",""),("unicode","\\mbfdigamma")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL DIGAMMA"}
  , Record {uchar = '\120782', commands = [("base","\\mathbf{0}"),("unicode","")], category = Ord, comments = "mathematical bold digit 0"}
  , Record {uchar = '\120783', commands = [("base","\\mathbf{1}"),("unicode","")], category = Ord, comments = "mathematical bold digit 1"}
  , Record {uchar = '\120784', commands = [("base","\\mathbf{2}"),("unicode","")], category = Ord, comments = "mathematical bold digit 2"}
  , Record {uchar = '\120785', commands = [("base","\\mathbf{3}"),("unicode","")], category = Ord, comments = "mathematical bold digit 3"}
  , Record {uchar = '\120786', commands = [("base","\\mathbf{4}"),("unicode","")], category = Ord, comments = "mathematical bold digit 4"}
  , Record {uchar = '\120787', commands = [("base","\\mathbf{5}"),("unicode","")], category = Ord, comments = "mathematical bold digit 5"}
  , Record {uchar = '\120788', commands = [("base","\\mathbf{6}"),("unicode","")], category = Ord, comments = "mathematical bold digit 6"}
  , Record {uchar = '\120789', commands = [("base","\\mathbf{7}"),("unicode","")], category = Ord, comments = "mathematical bold digit 7"}
  , Record {uchar = '\120790', commands = [("base","\\mathbf{8}"),("unicode","")], category = Ord, comments = "mathematical bold digit 8"}
  , Record {uchar = '\120791', commands = [("base","\\mathbf{9}"),("unicode","")], category = Ord, comments = "mathematical bold digit 9"}
  , Record {uchar = '\120792', commands = [("bbold","\\mathbb{0}"),("unicode","\\Bbbzero")], category = Ord, comments = "mathematical double-struck digit 0"}
  , Record {uchar = '\120793', commands = [("bbold","\\mathbb{1}"),("fourier","\\mathbb{1}"),("dsfont","\\mathds{1}"),("unicode","\\Bbbone")], category = Ord, comments = "mathematical double-struck digit 1"}
  , Record {uchar = '\120794', commands = [("bbold","\\mathbb{2}"),("unicode","\\Bbbtwo")], category = Ord, comments = "mathematical double-struck digit 2"}
  , Record {uchar = '\120795', commands = [("bbold","\\mathbb{3}"),("unicode","\\Bbbthree")], category = Ord, comments = "mathematical double-struck digit 3"}
  , Record {uchar = '\120796', commands = [("bbold","\\mathbb{4}"),("unicode","\\Bbbfour")], category = Ord, comments = "mathematical double-struck digit 4"}
  , Record {uchar = '\120797', commands = [("bbold","\\mathbb{5}"),("unicode","\\Bbbfive")], category = Ord, comments = "mathematical double-struck digit 5"}
  , Record {uchar = '\120798', commands = [("bbold","\\mathbb{6}"),("unicode","\\Bbbsix")], category = Ord, comments = "mathematical double-struck digit 6"}
  , Record {uchar = '\120799', commands = [("bbold","\\mathbb{7}"),("unicode","\\Bbbseven")], category = Ord, comments = "mathematical double-struck digit 7"}
  , Record {uchar = '\120800', commands = [("bbold","\\mathbb{8}"),("unicode","\\Bbbeight")], category = Ord, comments = "mathematical double-struck digit 8"}
  , Record {uchar = '\120801', commands = [("bbold","\\mathbb{9}"),("unicode","\\Bbbnine")], category = Ord, comments = "mathematical double-struck digit 9"}
  , Record {uchar = '\120802', commands = [("base","\\mathsf{0}"),("unicode","\\msanszero")], category = Ord, comments = "mathematical sans-serif digit 0"}
  , Record {uchar = '\120803', commands = [("base","\\mathsf{1}"),("unicode","\\msansone")], category = Ord, comments = "mathematical sans-serif digit 1"}
  , Record {uchar = '\120804', commands = [("base","\\mathsf{2}"),("unicode","\\msanstwo")], category = Ord, comments = "mathematical sans-serif digit 2"}
  , Record {uchar = '\120805', commands = [("base","\\mathsf{3}"),("unicode","\\msansthree")], category = Ord, comments = "mathematical sans-serif digit 3"}
  , Record {uchar = '\120806', commands = [("base","\\mathsf{4}"),("unicode","\\msansfour")], category = Ord, comments = "mathematical sans-serif digit 4"}
  , Record {uchar = '\120807', commands = [("base","\\mathsf{5}"),("unicode","\\msansfive")], category = Ord, comments = "mathematical sans-serif digit 5"}
  , Record {uchar = '\120808', commands = [("base","\\mathsf{6}"),("unicode","\\msanssix")], category = Ord, comments = "mathematical sans-serif digit 6"}
  , Record {uchar = '\120809', commands = [("base","\\mathsf{7}"),("unicode","\\msansseven")], category = Ord, comments = "mathematical sans-serif digit 7"}
  , Record {uchar = '\120810', commands = [("base","\\mathsf{8}"),("unicode","\\msanseight")], category = Ord, comments = "mathematical sans-serif digit 8"}
  , Record {uchar = '\120811', commands = [("base","\\mathsf{9}"),("unicode","\\msansnine")], category = Ord, comments = "mathematical sans-serif digit 9"}
  , Record {uchar = '\120812', commands = [("mathsfbf","\\mathsfbf{0}"),("unicode","\\mbfsanszero")], category = Ord, comments = "mathematical sans-serif bold digit 0"}
  , Record {uchar = '\120813', commands = [("mathsfbf","\\mathsfbf{1}"),("unicode","\\mbfsansone")], category = Ord, comments = "mathematical sans-serif bold digit 1"}
  , Record {uchar = '\120814', commands = [("mathsfbf","\\mathsfbf{2}"),("unicode","\\mbfsanstwo")], category = Ord, comments = "mathematical sans-serif bold digit 2"}
  , Record {uchar = '\120815', commands = [("mathsfbf","\\mathsfbf{3}"),("unicode","\\mbfsansthree")], category = Ord, comments = "mathematical sans-serif bold digit 3"}
  , Record {uchar = '\120816', commands = [("mathsfbf","\\mathsfbf{4}"),("unicode","\\mbfsansfour")], category = Ord, comments = "mathematical sans-serif bold digit 4"}
  , Record {uchar = '\120817', commands = [("mathsfbf","\\mathsfbf{5}"),("unicode","\\mbfsansfive")], category = Ord, comments = "mathematical sans-serif bold digit 5"}
  , Record {uchar = '\120818', commands = [("mathsfbf","\\mathsfbf{6}"),("unicode","\\mbfsanssix")], category = Ord, comments = "mathematical sans-serif bold digit 6"}
  , Record {uchar = '\120819', commands = [("mathsfbf","\\mathsfbf{7}"),("unicode","\\mbfsansseven")], category = Ord, comments = "mathematical sans-serif bold digit 7"}
  , Record {uchar = '\120820', commands = [("mathsfbf","\\mathsfbf{8}"),("unicode","\\mbfsanseight")], category = Ord, comments = "mathematical sans-serif bold digit 8"}
  , Record {uchar = '\120821', commands = [("mathsfbf","\\mathsfbf{9}"),("unicode","\\mbfsansnine")], category = Ord, comments = "mathematical sans-serif bold digit 9"}
  , Record {uchar = '\120822', commands = [("base","\\mathtt{0}"),("unicode","\\mttzero")], category = Ord, comments = "mathematical monospace digit 0"}
  , Record {uchar = '\120823', commands = [("base","\\mathtt{1}"),("unicode","\\mttone")], category = Ord, comments = "mathematical monospace digit 1"}
  , Record {uchar = '\120824', commands = [("base","\\mathtt{2}"),("unicode","\\mtttwo")], category = Ord, comments = "mathematical monospace digit 2"}
  , Record {uchar = '\120825', commands = [("base","\\mathtt{3}"),("unicode","\\mttthree")], category = Ord, comments = "mathematical monospace digit 3"}
  , Record {uchar = '\120826', commands = [("base","\\mathtt{4}"),("unicode","\\mttfour")], category = Ord, comments = "mathematical monospace digit 4"}
  , Record {uchar = '\120827', commands = [("base","\\mathtt{5}"),("unicode","\\mttfive")], category = Ord, comments = "mathematical monospace digit 5"}
  , Record {uchar = '\120828', commands = [("base","\\mathtt{6}"),("unicode","\\mttsix")], category = Ord, comments = "mathematical monospace digit 6"}
  , Record {uchar = '\120829', commands = [("base","\\mathtt{7}"),("unicode","\\mttseven")], category = Ord, comments = "mathematical monospace digit 7"}
  , Record {uchar = '\120830', commands = [("base","\\mathtt{8}"),("unicode","\\mtteight")], category = Ord, comments = "mathematical monospace digit 8"}
  , Record {uchar = '\120831', commands = [("base","\\mathtt{9}"),("unicode","\\mttnine")], category = Ord, comments = "mathematical monospace digit 9"}]

