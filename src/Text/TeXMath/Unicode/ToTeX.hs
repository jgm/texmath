{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Text.TeXMath.Unicode.ToTeX ( getTeXMath
                                  , getSymbolType
                                  , symbolMap
                                  , records
                                  ) where

import qualified Data.Map as M
import qualified Data.Text as T
import Text.TeXMath.TeX
import Text.TeXMath.Types
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Control.Applicative hiding (optional)
import Text.TeXMath.Unicode.ToUnicode (fromUnicodeChar)
import qualified Text.TeXMath.Shared as S

-- | Converts a string of unicode characters into a strong of equivalent
-- TeXMath commands. An environment is a list of strings specifying which
-- additional packages are available.
getTeXMath :: T.Text -> Env -> [TeX]
getTeXMath s e = concatMap (charToString e) $ T.unpack s

-- Categories which require braces
commandTypes :: [TeXSymbolType]
commandTypes = [Accent, Rad, TOver, TUnder]

-- Guaranteed to return latex safe string
charToString :: Env -> Char -> [TeX]
charToString e c =
  fromMaybe [escapeLaTeX c]
    (charToLaTeXString e c <|> textConvert e c)

-- Takes a single character and attempts to convert it to a latex string
charToLaTeXString :: Env -> Char -> Maybe [TeX]
-- we ignore 65024 VARIATION SELECTOR 1 to avoid putting it
-- literally in the output ; it is used in mathml output.
charToLaTeXString _ '\65024' = Just []
charToLaTeXString environment c = do
  v <- M.lookup c recordsMap
  -- Required packages for the command
  let toLit cs = case T.uncons cs of
        Just (x, xs) -> if T.null xs then [Token x] else [Literal cs]
        Nothing      -> []
  let cmds = commands v
  raw <- lookup "base" cmds <|>
         listToMaybe (mapMaybe (flip lookup cmds) environment)
  let latexCommand = if isControlSeq raw
                        then [ControlSeq raw]
                        else toLit raw
  return $ if category v `elem` commandTypes
              then latexCommand ++ [Grouped []]
              else latexCommand

-- Convert special unicode characters not in the standard mapping
textConvert :: Env -> Char -> Maybe [TeX]
textConvert env c = do
  (ttype, v) <- fromUnicodeChar c
  return [ControlSeq (S.getLaTeXTextCommand env ttype), Grouped [Token v]]


recordsMap :: M.Map Char Record
recordsMap = M.fromList (map f records)
  where
    f r = (uchar r, r)

-- | Returns TeX symbol type corresponding to a unicode character.
getSymbolType :: Char -> TeXSymbolType
getSymbolType c = fromMaybe Ord (category <$> M.lookup c recordsMap)

-- | Mapping from TeX commands to Exp.
symbolMap :: M.Map T.Text Exp
symbolMap = foldr go mempty records
 where
   go r m =
     foldr (\(_,!c) ->
              if T.take 1 c == "\\" && not (T.any (=='{') c)
              then
                let !t = T.singleton $! uchar $! r
                 in M.insert c (ESymbol (category r) t)
              else id)
           m
           (commands r)


records :: [Record]
records =
  [ Record {uchar = '!', commands = [("base","!"),("unicode-math","\\exclam")], category = Pun, comments = "EXCLAMATION MARK"}
  , Record {uchar = '#', commands = [("base","\\#"),("oz","\\#"),("unicode-math","\\octothorpe")], category = Ord, comments = "NUMBER SIGN"}
  , Record {uchar = '$', commands = [("base","\\$"),("base","\\mathdollar"),("unicode-math","\\mathdollar")], category = Ord, comments = "DOLLAR SIGN"}
  , Record {uchar = '%', commands = [("base","\\%"),("unicode-math","\\percent")], category = Ord, comments = "PERCENT SIGN"}
  , Record {uchar = '&', commands = [("base","\\&"),("stmaryrd","\\binampersand"),("unicode-math","\\ampersand")], category = Ord, comments = ""}
  , Record {uchar = '(', commands = [("base","("),("unicode-math","\\lparen")], category = Open, comments = "LEFT PARENTHESIS"}
  , Record {uchar = ')', commands = [("base",")"),("unicode-math","\\rparen")], category = Close, comments = "RIGHT PARENTHESIS"}
  , Record {uchar = '*', commands = [("base","*"),("base","\\ast")], category = Ord, comments = "(high) ASTERISK, star"}
  , Record {uchar = '+', commands = [("base","+"),("unicode-math","\\plus")], category = Bin, comments = "PLUS SIGN"}
  , Record {uchar = ',', commands = [("base",","),("unicode-math","\\comma")], category = Pun, comments = "COMMA"}
  , Record {uchar = '-', commands = [("base","-")], category = Bin, comments = "t -, HYPHEN-MINUS (deprecated for math)"}
  , Record {uchar = '.', commands = [("base","."),("unicode-math","\\period")], category = Alpha, comments = "FULL STOP, period"}
  , Record {uchar = '/', commands = [("base","/"),("base","\\slash"),("unicode-math","\\mathslash")], category = Ord, comments = "SOLIDUS"}
  , Record {uchar = '0', commands = [("base","0")], category = Ord, comments = "DIGIT ZERO"}
  , Record {uchar = '1', commands = [("base","1")], category = Ord, comments = "DIGIT ONE"}
  , Record {uchar = '2', commands = [("base","2")], category = Ord, comments = "DIGIT TWO"}
  , Record {uchar = '3', commands = [("base","3")], category = Ord, comments = "DIGIT THREE"}
  , Record {uchar = '4', commands = [("base","4")], category = Ord, comments = "DIGIT FOUR"}
  , Record {uchar = '5', commands = [("base","5")], category = Ord, comments = "DIGIT FIVE"}
  , Record {uchar = '6', commands = [("base","6")], category = Ord, comments = "DIGIT SIX"}
  , Record {uchar = '7', commands = [("base","7")], category = Ord, comments = "DIGIT SEVEN"}
  , Record {uchar = '8', commands = [("base","8")], category = Ord, comments = "DIGIT EIGHT"}
  , Record {uchar = '9', commands = [("base","9")], category = Ord, comments = "DIGIT NINE"}
  , Record {uchar = ':', commands = [("base",":"),("literal","\\colon"),("unicode-math","\\mathcolon")], category = Pun, comments = "COLON (not ratio)"}
  , Record {uchar = ';', commands = [("base",";"),("unicode-math","\\semicolon")], category = Pun, comments = "SEMICOLON p:"}
  , Record {uchar = '<', commands = [("base","<"),("unicode-math","\\less")], category = Rel, comments = "LESS-THAN SIGN r:"}
  , Record {uchar = '=', commands = [("base","="),("unicode-math","\\equal")], category = Rel, comments = "EQUALS SIGN r:"}
  , Record {uchar = '>', commands = [("base",">"),("unicode-math","\\greater")], category = Rel, comments = "GREATER-THAN SIGN r:"}
  , Record {uchar = '?', commands = [("base","?"),("unicode-math","\\question")], category = Ord, comments = "QUESTION MARK"}
  , Record {uchar = '@', commands = [("base","@"),("unicode-math","\\atsign")], category = Ord, comments = "at"}
  , Record {uchar = 'A', commands = [("base","A"),("base","\\mathrm{A}")], category = Alpha, comments = "LATIN CAPITAL LETTER A"}
  , Record {uchar = 'B', commands = [("base","B"),("base","\\mathrm{B}")], category = Alpha, comments = "LATIN CAPITAL LETTER B"}
  , Record {uchar = 'C', commands = [("base","C"),("base","\\mathrm{C}")], category = Alpha, comments = "LATIN CAPITAL LETTER C"}
  , Record {uchar = 'D', commands = [("base","D"),("base","\\mathrm{D}")], category = Alpha, comments = "LATIN CAPITAL LETTER D"}
  , Record {uchar = 'E', commands = [("base","E"),("base","\\mathrm{E}")], category = Alpha, comments = "LATIN CAPITAL LETTER E"}
  , Record {uchar = 'F', commands = [("base","F"),("base","\\mathrm{F}")], category = Alpha, comments = "LATIN CAPITAL LETTER F"}
  , Record {uchar = 'G', commands = [("base","G"),("base","\\mathrm{G}")], category = Alpha, comments = "LATIN CAPITAL LETTER G"}
  , Record {uchar = 'H', commands = [("base","H"),("base","\\mathrm{H}")], category = Alpha, comments = "LATIN CAPITAL LETTER H"}
  , Record {uchar = 'I', commands = [("base","I"),("base","\\mathrm{I}")], category = Alpha, comments = "LATIN CAPITAL LETTER I"}
  , Record {uchar = 'J', commands = [("base","J"),("base","\\mathrm{J}")], category = Alpha, comments = "LATIN CAPITAL LETTER J"}
  , Record {uchar = 'K', commands = [("base","K"),("base","\\mathrm{K}")], category = Alpha, comments = "LATIN CAPITAL LETTER K"}
  , Record {uchar = 'L', commands = [("base","L"),("base","\\mathrm{L}")], category = Alpha, comments = "LATIN CAPITAL LETTER L"}
  , Record {uchar = 'M', commands = [("base","M"),("base","\\mathrm{M}")], category = Alpha, comments = "LATIN CAPITAL LETTER M"}
  , Record {uchar = 'N', commands = [("base","N"),("base","\\mathrm{N}")], category = Alpha, comments = "LATIN CAPITAL LETTER N"}
  , Record {uchar = 'O', commands = [("base","O"),("base","\\mathrm{O}")], category = Alpha, comments = "LATIN CAPITAL LETTER O"}
  , Record {uchar = 'P', commands = [("base","P"),("base","\\mathrm{P}")], category = Alpha, comments = "LATIN CAPITAL LETTER P"}
  , Record {uchar = 'Q', commands = [("base","Q"),("base","\\mathrm{Q}")], category = Alpha, comments = "LATIN CAPITAL LETTER Q"}
  , Record {uchar = 'R', commands = [("base","R"),("base","\\mathrm{R}")], category = Alpha, comments = "LATIN CAPITAL LETTER R"}
  , Record {uchar = 'S', commands = [("base","S"),("base","\\mathrm{S}")], category = Alpha, comments = "LATIN CAPITAL LETTER S"}
  , Record {uchar = 'T', commands = [("base","T"),("base","\\mathrm{T}")], category = Alpha, comments = "LATIN CAPITAL LETTER T"}
  , Record {uchar = 'U', commands = [("base","U"),("base","\\mathrm{U}")], category = Alpha, comments = "LATIN CAPITAL LETTER U"}
  , Record {uchar = 'V', commands = [("base","V"),("base","\\mathrm{V}")], category = Alpha, comments = "LATIN CAPITAL LETTER V"}
  , Record {uchar = 'W', commands = [("base","W"),("base","\\mathrm{W}")], category = Alpha, comments = "LATIN CAPITAL LETTER W"}
  , Record {uchar = 'X', commands = [("base","X"),("base","\\mathrm{X}")], category = Alpha, comments = "LATIN CAPITAL LETTER X"}
  , Record {uchar = 'Y', commands = [("base","Y"),("base","\\mathrm{Y}")], category = Alpha, comments = "LATIN CAPITAL LETTER Y"}
  , Record {uchar = 'Z', commands = [("base","Z"),("base","\\mathrm{Z}")], category = Alpha, comments = "LATIN CAPITAL LETTER Z"}
  , Record {uchar = '[', commands = [("base","\\lbrack"),("unicode-math","\\lbrack")], category = Open, comments = "LEFT SQUARE BRACKET"}
  , Record {uchar = '\\', commands = [("base","\\backslash"),("unicode-math","\\backslash")], category = Ord, comments = "REVERSE SOLIDUS"}
  , Record {uchar = ']', commands = [("base","\\rbrack"),("unicode-math","\\rbrack")], category = Close, comments = "RIGHT SQUARE BRACKET"}
  , Record {uchar = '^', commands = [("base","\\hat{}"),("unicode-math","\\sphat")], category = Ord, comments = "amsxtra^CIRCUMFLEX ACCENT, TeX superscript operator"}
  , Record {uchar = '_', commands = [("base","\\_")], category = Ord, comments = "LOW LINE, TeX subscript operator"}
  , Record {uchar = '`', commands = [], category = Ord, comments = "grave, alias for 0300"}
  , Record {uchar = 'a', commands = [("base","a"),("base","\\mathrm{a}")], category = Alpha, comments = "LATIN SMALL LETTER A"}
  , Record {uchar = 'b', commands = [("base","b"),("base","\\mathrm{b}")], category = Alpha, comments = "LATIN SMALL LETTER B"}
  , Record {uchar = 'c', commands = [("base","c"),("base","\\mathrm{c}")], category = Alpha, comments = "LATIN SMALL LETTER C"}
  , Record {uchar = 'd', commands = [("base","d"),("base","\\mathrm{d}")], category = Alpha, comments = "LATIN SMALL LETTER D"}
  , Record {uchar = 'e', commands = [("base","e"),("base","\\mathrm{e}")], category = Alpha, comments = "LATIN SMALL LETTER E"}
  , Record {uchar = 'f', commands = [("base","f"),("base","\\mathrm{f}")], category = Alpha, comments = "LATIN SMALL LETTER F"}
  , Record {uchar = 'g', commands = [("base","g"),("base","\\mathrm{g}")], category = Alpha, comments = "LATIN SMALL LETTER G"}
  , Record {uchar = 'h', commands = [("base","h"),("base","\\mathrm{h}")], category = Alpha, comments = "LATIN SMALL LETTER H"}
  , Record {uchar = 'i', commands = [("base","i"),("base","\\mathrm{i}")], category = Alpha, comments = "LATIN SMALL LETTER I"}
  , Record {uchar = 'j', commands = [("base","j"),("base","\\mathrm{j}")], category = Alpha, comments = "LATIN SMALL LETTER J"}
  , Record {uchar = 'k', commands = [("base","k"),("base","\\mathrm{k}")], category = Alpha, comments = "LATIN SMALL LETTER K"}
  , Record {uchar = 'l', commands = [("base","l"),("base","\\mathrm{l}")], category = Alpha, comments = "LATIN SMALL LETTER L"}
  , Record {uchar = 'm', commands = [("base","m"),("base","\\mathrm{m}")], category = Alpha, comments = "LATIN SMALL LETTER M"}
  , Record {uchar = 'n', commands = [("base","n"),("base","\\mathrm{n}")], category = Alpha, comments = "LATIN SMALL LETTER N"}
  , Record {uchar = 'o', commands = [("base","o"),("base","\\mathrm{o}")], category = Alpha, comments = "LATIN SMALL LETTER O"}
  , Record {uchar = 'p', commands = [("base","p"),("base","\\mathrm{p}")], category = Alpha, comments = "LATIN SMALL LETTER P"}
  , Record {uchar = 'q', commands = [("base","q"),("base","\\mathrm{q}")], category = Alpha, comments = "LATIN SMALL LETTER Q"}
  , Record {uchar = 'r', commands = [("base","r"),("base","\\mathrm{r}")], category = Alpha, comments = "LATIN SMALL LETTER R"}
  , Record {uchar = 's', commands = [("base","s"),("base","\\mathrm{s}")], category = Alpha, comments = "LATIN SMALL LETTER S"}
  , Record {uchar = 't', commands = [("base","t"),("base","\\mathrm{t}")], category = Alpha, comments = "LATIN SMALL LETTER T"}
  , Record {uchar = 'u', commands = [("base","u"),("base","\\mathrm{u}")], category = Alpha, comments = "LATIN SMALL LETTER U"}
  , Record {uchar = 'v', commands = [("base","v"),("base","\\mathrm{v}")], category = Alpha, comments = "LATIN SMALL LETTER V"}
  , Record {uchar = 'w', commands = [("base","w"),("base","\\mathrm{w}")], category = Alpha, comments = "LATIN SMALL LETTER W"}
  , Record {uchar = 'x', commands = [("base","x"),("base","\\mathrm{x}")], category = Alpha, comments = "LATIN SMALL LETTER X"}
  , Record {uchar = 'y', commands = [("base","y"),("base","\\mathrm{y}")], category = Alpha, comments = "LATIN SMALL LETTER Y"}
  , Record {uchar = 'z', commands = [("base","z"),("base","\\mathrm{z}")], category = Alpha, comments = "LATIN SMALL LETTER Z"}
  , Record {uchar = '{', commands = [("base","\\{"),("base","\\lbrace"),("unicode-math","\\lbrace")], category = Open, comments = "LEFT CURLY BRACKET"}
  , Record {uchar = '|', commands = [("base","|"),("base","\\vert"),("unicode-math","\\vert")], category = Fence, comments = "vertical bar"}
  , Record {uchar = '}', commands = [("base","\\}"),("base","\\rbrace"),("unicode-math","\\rbrace")], category = Close, comments = "RIGHT CURLY BRACKET"}
  , Record {uchar = '~', commands = [("amsxtra","\\sptilde"),("base","\\sim")], category = Ord, comments = "TILDE"}
  , Record {uchar = '\160', commands = [("base","~")], category = Ord, comments = "nbsp"}
  , Record {uchar = '\161', commands = [], category = Ord, comments = "iexcl"}
  , Record {uchar = '\162', commands = [("wasysym","\\cent"),("txfonts","\\mathcent")], category = Ord, comments = "cent"}
  , Record {uchar = '\163', commands = [("base","\\pounds"),("txfonts","\\mathsterling"),("unicode-math","\\sterling")], category = Ord, comments = "POUND SIGN, fourier prints a dollar sign"}
  , Record {uchar = '\164', commands = [], category = Ord, comments = "t \\currency (wasysym), curren"}
  , Record {uchar = '\165', commands = [("amsfonts","\\yen"),("unicode-math","\\yen")], category = Ord, comments = "YEN SIGN"}
  , Record {uchar = '\166', commands = [], category = Ord, comments = "brvbar (vertical)"}
  , Record {uchar = '\167', commands = [], category = Ord, comments = "sect"}
  , Record {uchar = '\168', commands = [("amsxtra","\\spddot")], category = Ord, comments = "Dot /die, alias for 0308"}
  , Record {uchar = '\172', commands = [("base","\\neg"),("base","\\lnot"),("unicode-math","\\neg")], category = Ord, comments = "NOT SIGN"}
  , Record {uchar = '\174', commands = [("amsfonts","\\circledR")], category = Ord, comments = "REGISTERED SIGN"}
  , Record {uchar = '\175', commands = [], category = Ord, comments = "macr, alias for 0304"}
  , Record {uchar = '\176', commands = [("base","{^\\circ}")], category = Ord, comments = "deg"}
  , Record {uchar = '\177', commands = [("base","\\pm"),("unicode-math","\\pm")], category = Bin, comments = "plus-or-minus sign"}
  , Record {uchar = '\178', commands = [], category = Ord, comments = "sup2"}
  , Record {uchar = '\179', commands = [], category = Ord, comments = "sup3"}
  , Record {uchar = '\180', commands = [], category = Ord, comments = "acute, alias for 0301"}
  , Record {uchar = '\181', commands = [("wrisym","\\Micro"),("mathcomp","\\tcmu")], category = Alpha, comments = "t \\textmu (textcomp), # \\mathrm{\\mu} (omlmathrm), # \\muup (kpfonts mathdesign), MICRO SIGN"}
  , Record {uchar = '\182', commands = [], category = Ord, comments = "para (paragraph sign, pilcrow)"}
  , Record {uchar = '\183', commands = [("base","\\cdot"),("unicode-math","\\cdotp")], category = Bin, comments = "x \\centerdot, b: MIDDLE DOT"}
  , Record {uchar = '\185', commands = [], category = Ord, comments = "sup1"}
  , Record {uchar = '\188', commands = [], category = Ord, comments = "frac14"}
  , Record {uchar = '\189', commands = [], category = Ord, comments = "frac12"}
  , Record {uchar = '\190', commands = [], category = Ord, comments = "frac34"}
  , Record {uchar = '\191', commands = [], category = Ord, comments = "iquest"}
  , Record {uchar = '\215', commands = [("base","\\times"),("unicode-math","\\times")], category = Bin, comments = "MULTIPLICATION SIGN, z notation Cartesian product"}
  , Record {uchar = '\240', commands = [("amssymb","\\eth"),("arevmath","\\eth"),("unicode-math","\\matheth")], category = Alpha, comments = "eth"}
  , Record {uchar = '\247', commands = [("base","\\div"),("unicode-math","\\div")], category = Bin, comments = "divide sign"}
  , Record {uchar = '\305', commands = [("base","\\imath")], category = Alpha, comments = "imath"}
  , Record {uchar = '\437', commands = [("unicode-math","\\Zbar")], category = Ord, comments = "impedance"}
  , Record {uchar = '\567', commands = [("base","\\jmath")], category = Alpha, comments = "jmath"}
  , Record {uchar = '\710', commands = [("base","\\hat{}")], category = Alpha, comments = "circ, alias for 0302"}
  , Record {uchar = '\711', commands = [], category = Alpha, comments = "CARON, alias for 030C"}
  , Record {uchar = '\728', commands = [], category = Ord, comments = "BREVE, alias for 0306"}
  , Record {uchar = '\729', commands = [], category = Ord, comments = "dot, alias for 0307"}
  , Record {uchar = '\730', commands = [], category = Ord, comments = "ring, alias for 030A"}
  , Record {uchar = '\732', commands = [], category = Ord, comments = "tilde, alias for 0303"}
  , Record {uchar = '\768', commands = [("base","\\grave"),("unicode-math","\\grave")], category = Accent, comments = "grave accent"}
  , Record {uchar = '\769', commands = [("base","\\acute"),("unicode-math","\\acute")], category = Accent, comments = "acute accent"}
  , Record {uchar = '\770', commands = [("base","\\hat"),("amssymb","\\widehat"),("unicode-math","\\hat")], category = Accent, comments = "circumflex accent"}
  , Record {uchar = '\771', commands = [("base","\\tilde"),("yhmath, fourier","\\widetilde"),("unicode-math","\\tilde")], category = Accent, comments = "tilde"}
  , Record {uchar = '\772', commands = [("base","\\bar"),("unicode-math","\\bar")], category = Accent, comments = "macron"}
  , Record {uchar = '\773', commands = [("base","\\overline"),("unicode-math","\\overbar")], category = Accent, comments = "overbar embellishment"}
  , Record {uchar = '\774', commands = [("base","\\breve"),("unicode-math","\\breve")], category = Accent, comments = "breve"}
  , Record {uchar = '\775', commands = [("base","\\dot"),("wrisym","\\Dot"),("unicode-math","\\dot")], category = Accent, comments = "dot above"}
  , Record {uchar = '\776', commands = [("base","\\ddot"),("wrisym","\\DDot"),("unicode-math","\\ddot")], category = Accent, comments = "dieresis"}
  , Record {uchar = '\777', commands = [("unicode-math","\\ovhook")], category = Accent, comments = "COMBINING HOOK ABOVE"}
  , Record {uchar = '\778', commands = [("amssymb","\\mathring"),("yhmath","\\ring"),("unicode-math","\\ocirc")], category = Accent, comments = "ring"}
  , Record {uchar = '\780', commands = [("base","\\check"),("unicode-math","\\check")], category = Accent, comments = "caron"}
  , Record {uchar = '\784', commands = [("unicode-math","\\candra")], category = Accent, comments = "candrabindu (non-spacing)"}
  , Record {uchar = '\785', commands = [], category = Accent, comments = "COMBINING INVERTED BREVE"}
  , Record {uchar = '\786', commands = [("unicode-math","\\oturnedcomma")], category = Accent, comments = "COMBINING TURNED COMMA ABOVE"}
  , Record {uchar = '\789', commands = [("unicode-math","\\ocommatopright")], category = Accent, comments = "COMBINING COMMA ABOVE RIGHT"}
  , Record {uchar = '\794', commands = [("unicode-math","\\droang")], category = Accent, comments = "left angle above (non-spacing)"}
  , Record {uchar = '\803', commands = [], category = Accent, comments = "COMBINING DOT BELOW"}
  , Record {uchar = '\812', commands = [], category = Accent, comments = "COMBINING CARON BELOW"}
  , Record {uchar = '\813', commands = [], category = Accent, comments = "COMBINING CIRCUMFLEX ACCENT BELOW"}
  , Record {uchar = '\814', commands = [], category = Accent, comments = "COMBINING BREVE BELOW"}
  , Record {uchar = '\815', commands = [], category = Accent, comments = "COMBINING INVERTED BREVE BELOW"}
  , Record {uchar = '\816', commands = [("undertilde","\\utilde"),("unicode-math","\\wideutilde")], category = Accent, comments = "under tilde accent (multiple characters and non-spacing)"}
  , Record {uchar = '\817', commands = [("base","\\underbar"),("unicode-math","\\underbar")], category = Accent, comments = "COMBINING MACRON BELOW"}
  , Record {uchar = '\818', commands = [("base","\\underline")], category = Accent, comments = "COMBINING LOW LINE"}
  , Record {uchar = '\819', commands = [], category = Accent, comments = "2lowbar"}
  , Record {uchar = '\824', commands = [("base","\\not"),("unicode-math","\\not")], category = Accent, comments = "COMBINING LONG SOLIDUS OVERLAY"}
  , Record {uchar = '\826', commands = [], category = Accent, comments = "COMBINING INVERTED BRIDGE BELOW"}
  , Record {uchar = '\831', commands = [], category = Accent, comments = "COMBINING DOUBLE OVERLINE"}
  , Record {uchar = '\838', commands = [], category = Accent, comments = "COMBINING BRIDGE ABOVE"}
  , Record {uchar = '\913', commands = [("unicode-math","\\upAlpha")], category = Alpha, comments = "capital alpha, greek"}
  , Record {uchar = '\914', commands = [("unicode-math","\\upBeta")], category = Alpha, comments = "capital beta, greek"}
  , Record {uchar = '\915', commands = [("base","\\Gamma"),("-slantedGreek","\\Gamma"),("unicode-math","\\upGamma")], category = Alpha, comments = "= \\mathrm{\\Gamma}, capital gamma, greek"}
  , Record {uchar = '\916', commands = [("base","\\Delta"),("-slantedGreek","\\Delta"),("unicode-math","\\upDelta")], category = Alpha, comments = "= \\mathrm{\\Delta}, capital delta, greek"}
  , Record {uchar = '\917', commands = [("unicode-math","\\upEpsilon")], category = Alpha, comments = "capital epsilon, greek"}
  , Record {uchar = '\918', commands = [("unicode-math","\\upZeta")], category = Alpha, comments = "capital zeta, greek"}
  , Record {uchar = '\919', commands = [("unicode-math","\\upEta")], category = Alpha, comments = "capital eta, greek"}
  , Record {uchar = '\920', commands = [("base","\\Theta"),("-slantedGreek","\\Theta"),("unicode-math","\\upTheta")], category = Alpha, comments = "= \\mathrm{\\Theta}, capital theta, greek"}
  , Record {uchar = '\921', commands = [("unicode-math","\\upIota")], category = Alpha, comments = "capital iota, greek"}
  , Record {uchar = '\922', commands = [("unicode-math","\\upKappa")], category = Alpha, comments = "capital kappa, greek"}
  , Record {uchar = '\923', commands = [("base","\\Lambda"),("-slantedGreek","\\Lambda"),("unicode-math","\\upLambda")], category = Alpha, comments = "= \\mathrm{\\Lambda}, capital lambda, greek"}
  , Record {uchar = '\924', commands = [("unicode-math","\\upMu")], category = Alpha, comments = "capital mu, greek"}
  , Record {uchar = '\925', commands = [("unicode-math","\\upNu")], category = Alpha, comments = "capital nu, greek"}
  , Record {uchar = '\926', commands = [("base","\\Xi"),("-slantedGreek","\\Xi"),("unicode-math","\\upXi")], category = Alpha, comments = "= \\mathrm{\\Xi}, capital xi, greek"}
  , Record {uchar = '\927', commands = [("unicode-math","\\upOmicron")], category = Alpha, comments = "capital omicron, greek"}
  , Record {uchar = '\928', commands = [("base","\\Pi"),("-slantedGreek","\\Pi"),("unicode-math","\\upPi")], category = Alpha, comments = "= \\mathrm{\\Pi}, capital pi, greek"}
  , Record {uchar = '\929', commands = [("unicode-math","\\upRho")], category = Alpha, comments = "capital rho, greek"}
  , Record {uchar = '\931', commands = [("base","\\Sigma"),("-slantedGreek","\\Sigma"),("unicode-math","\\upSigma")], category = Alpha, comments = "= \\mathrm{\\Sigma}, capital sigma, greek"}
  , Record {uchar = '\932', commands = [("unicode-math","\\upTau")], category = Alpha, comments = "capital tau, greek"}
  , Record {uchar = '\933', commands = [("base","\\Upsilon"),("-slantedGreek","\\Upsilon"),("unicode-math","\\upUpsilon")], category = Alpha, comments = "= \\mathrm{\\Upsilon}, capital upsilon, greek"}
  , Record {uchar = '\934', commands = [("base","\\Phi"),("-slantedGreek","\\Phi"),("unicode-math","\\upPhi")], category = Alpha, comments = "= \\mathrm{\\Phi}, capital phi, greek"}
  , Record {uchar = '\935', commands = [("unicode-math","\\upChi")], category = Alpha, comments = "capital chi, greek"}
  , Record {uchar = '\936', commands = [("base","\\Psi"),("-slantedGreek","\\Psi"),("unicode-math","\\upPsi")], category = Alpha, comments = "= \\mathrm{\\Psi}, capital psi, greek"}
  , Record {uchar = '\937', commands = [("base","\\Omega"),("-slantedGreek","\\Omega"),("unicode-math","\\upOmega")], category = Alpha, comments = "= \\mathrm{\\Omega}, capital omega, greek"}
  , Record {uchar = '\945', commands = [("base","\\alpha"),("omlmathrm","\\mathrm{\\alpha}"),("unicode-math","\\upalpha")], category = Alpha, comments = "= \\alphaup (kpfonts mathdesign), = \\upalpha (upgreek), alpha, greek"}
  , Record {uchar = '\946', commands = [("base","\\beta"),("omlmathrm","\\mathrm{\\beta}"),("unicode-math","\\upbeta")], category = Alpha, comments = "= \\betaup (kpfonts mathdesign), = \\upbeta (upgreek), beta, greek"}
  , Record {uchar = '\947', commands = [("base","\\gamma"),("omlmathrm","\\mathrm{\\gamma}"),("unicode-math","\\upgamma")], category = Alpha, comments = "= \\gammaup (kpfonts mathdesign), = \\upgamma (upgreek), gamma, greek"}
  , Record {uchar = '\948', commands = [("base","\\delta"),("omlmathrm","\\mathrm{\\delta}"),("unicode-math","\\updelta")], category = Alpha, comments = "= \\deltaup (kpfonts mathdesign), = \\updelta (upgreek), delta, greek"}
  , Record {uchar = '\949', commands = [("base","\\varepsilon"),("omlmathrm","\\mathrm{\\varepsilon}"),("unicode-math","\\upepsilon")], category = Alpha, comments = "= \\varepsilonup (kpfonts mathdesign), = \\upepsilon (upgreek), rounded epsilon, greek"}
  , Record {uchar = '\950', commands = [("base","\\zeta"),("omlmathrm","\\mathrm{\\zeta}"),("unicode-math","\\upzeta")], category = Alpha, comments = "= \\zetaup (kpfonts mathdesign), = \\upzeta (upgreek), zeta, greek"}
  , Record {uchar = '\951', commands = [("base","\\eta"),("omlmathrm","\\mathrm{\\eta}"),("unicode-math","\\upeta")], category = Alpha, comments = "= \\etaup (kpfonts mathdesign), = \\upeta (upgreek), eta, greek"}
  , Record {uchar = '\952', commands = [("base","\\theta"),("omlmathrm","\\mathrm{\\theta}"),("unicode-math","\\uptheta")], category = Alpha, comments = "= \\thetaup (kpfonts mathdesign), straight theta, = \\uptheta (upgreek), theta, greek"}
  , Record {uchar = '\953', commands = [("base","\\iota"),("omlmathrm","\\mathrm{\\iota}"),("unicode-math","\\upiota")], category = Alpha, comments = "= \\iotaup (kpfonts mathdesign), = \\upiota (upgreek), iota, greek"}
  , Record {uchar = '\954', commands = [("base","\\kappa"),("omlmathrm","\\mathrm{\\kappa}"),("unicode-math","\\upkappa")], category = Alpha, comments = "= \\kappaup (kpfonts mathdesign), = \\upkappa (upgreek), kappa, greek"}
  , Record {uchar = '\955', commands = [("base","\\lambda"),("omlmathrm","\\mathrm{\\lambda}"),("unicode-math","\\uplambda")], category = Alpha, comments = "= \\lambdaup (kpfonts mathdesign), = \\uplambda (upgreek), lambda, greek"}
  , Record {uchar = '\956', commands = [("base","\\mu"),("omlmathrm","\\mathrm{\\mu}"),("unicode-math","\\upmu")], category = Alpha, comments = "= \\muup (kpfonts mathdesign), = \\upmu (upgreek), mu, greek"}
  , Record {uchar = '\957', commands = [("base","\\nu"),("omlmathrm","\\mathrm{\\nu}"),("unicode-math","\\upnu")], category = Alpha, comments = "= \\nuup (kpfonts mathdesign), = \\upnu (upgreek), nu, greek"}
  , Record {uchar = '\958', commands = [("base","\\xi"),("omlmathrm","\\mathrm{\\xi}"),("unicode-math","\\upxi")], category = Alpha, comments = "= \\xiup (kpfonts mathdesign), = \\upxi (upgreek), xi, greek"}
  , Record {uchar = '\959', commands = [("unicode-math","\\upomicron")], category = Alpha, comments = "small omicron, greek"}
  , Record {uchar = '\960', commands = [("base","\\pi"),("omlmathrm","\\mathrm{\\pi}"),("unicode-math","\\uppi")], category = Alpha, comments = "= \\piup (kpfonts mathdesign), = \\uppi (upgreek), pi, greek"}
  , Record {uchar = '\961', commands = [("base","\\rho"),("omlmathrm","\\mathrm{\\rho}"),("unicode-math","\\uprho")], category = Alpha, comments = "= \\rhoup (kpfonts mathdesign), = \\uprho (upgreek), rho, greek"}
  , Record {uchar = '\962', commands = [("base","\\varsigma"),("omlmathrm","\\mathrm{\\varsigma}"),("unicode-math","\\upvarsigma")], category = Alpha, comments = "= \\varsigmaup (kpfonts mathdesign), = \\upvarsigma (upgreek), terminal sigma, greek"}
  , Record {uchar = '\963', commands = [("base","\\sigma"),("omlmathrm","\\mathrm{\\sigma}"),("unicode-math","\\upsigma")], category = Alpha, comments = "= \\sigmaup (kpfonts mathdesign), = \\upsigma (upgreek), sigma, greek"}
  , Record {uchar = '\964', commands = [("base","\\tau"),("omlmathrm","\\mathrm{\\tau}"),("unicode-math","\\uptau")], category = Alpha, comments = "= \\tauup (kpfonts mathdesign), = \\uptau (upgreek), tau, greek"}
  , Record {uchar = '\965', commands = [("base","\\upsilon"),("omlmathrm","\\mathrm{\\upsilon}"),("unicode-math","\\upupsilon")], category = Alpha, comments = "= \\upsilonup (kpfonts mathdesign), = \\upupsilon (upgreek), upsilon, greek"}
  , Record {uchar = '\966', commands = [("base","\\varphi"),("omlmathrm","\\mathrm{\\varphi}"),("unicode-math","\\upvarphi")], category = Alpha, comments = "= \\varphiup (kpfonts mathdesign), = \\upvarphi (upgreek), curly or open phi, greek"}
  , Record {uchar = '\967', commands = [("base","\\chi"),("omlmathrm","\\mathrm{\\chi}"),("unicode-math","\\upchi")], category = Alpha, comments = "= \\chiup (kpfonts mathdesign), = \\upchi (upgreek), chi, greek"}
  , Record {uchar = '\968', commands = [("base","\\psi"),("omlmathrm","\\mathrm{\\psi}"),("unicode-math","\\uppsi")], category = Alpha, comments = "= \\psiup (kpfonts mathdesign), = \\uppsi (upgreek), psi, greek"}
  , Record {uchar = '\969', commands = [("base","\\omega"),("omlmathrm","\\mathrm{\\omega}"),("unicode-math","\\upomega")], category = Alpha, comments = "= \\omegaup (kpfonts mathdesign), = \\upomega (upgreek), omega, greek"}
  , Record {uchar = '\976', commands = [("arevmath","\\varbeta"),("unicode-math","\\upvarbeta")], category = Alpha, comments = "rounded beta, greek"}
  , Record {uchar = '\977', commands = [("base","\\vartheta"),("omlmathrm","\\mathrm{\\vartheta}"),("unicode-math","\\upvartheta")], category = Alpha, comments = "= \\varthetaup (kpfonts mathdesign), curly or open theta"}
  , Record {uchar = '\978', commands = [("base","\\mathrm{\\Upsilon}"),("unicode-math","\\upUpsilon")], category = Alpha, comments = "GREEK UPSILON WITH HOOK SYMBOL"}
  , Record {uchar = '\981', commands = [("base","\\phi"),("omlmathrm","\\mathrm{\\phi}"),("unicode-math","\\upphi")], category = Alpha, comments = "= \\phiup (kpfonts mathdesign), GREEK PHI SYMBOL (straight)"}
  , Record {uchar = '\982', commands = [("base","\\varpi"),("omlmathrm","\\mathrm{\\varpi}"),("unicode-math","\\upvarpi")], category = Alpha, comments = "= \\varpiup (kpfonts mathdesign), GREEK PI SYMBOL (pomega)"}
  , Record {uchar = '\984', commands = [("arevmath","\\Qoppa"),("wrisym","\\Koppa"),("unicode-math","\\upoldKoppa")], category = Ord, comments = "t \\Qoppa (LGR), GREEK LETTER ARCHAIC KOPPA"}
  , Record {uchar = '\985', commands = [("arevmath","\\qoppa"),("wrisym","\\koppa"),("unicode-math","\\upoldkoppa")], category = Ord, comments = "t \\qoppa (LGR), GREEK SMALL LETTER ARCHAIC KOPPA"}
  , Record {uchar = '\986', commands = [("arevmath","\\Stigma"),("wrisym","\\Stigma"),("unicode-math","\\upStigma")], category = Alpha, comments = "capital stigma"}
  , Record {uchar = '\987', commands = [("arevmath","\\stigma"),("wrisym","\\stigma"),("unicode-math","\\upstigma")], category = Alpha, comments = "GREEK SMALL LETTER STIGMA"}
  , Record {uchar = '\988', commands = [("wrisym","\\Digamma"),("amssymb","\\digamma"),("unicode-math","\\upDigamma")], category = Alpha, comments = "capital digamma"}
  , Record {uchar = '\989', commands = [("arevmath","\\digamma"),("wrisym","\\digamma"),("unicode-math","\\updigamma")], category = Alpha, comments = "GREEK SMALL LETTER DIGAMMA"}
  , Record {uchar = '\990', commands = [("arevmath","\\Koppa"),("unicode-math","\\upKoppa")], category = Alpha, comments = "capital koppa"}
  , Record {uchar = '\991', commands = [("arevmath","\\koppa"),("unicode-math","\\upkoppa")], category = Alpha, comments = "GREEK SMALL LETTER KOPPA"}
  , Record {uchar = '\992', commands = [("arevmath","\\Sampi"),("wrisym","\\Sampi"),("unicode-math","\\upSampi")], category = Alpha, comments = "capital sampi"}
  , Record {uchar = '\993', commands = [("arevmath","\\sampi"),("wrisym","\\sampi"),("unicode-math","\\upsampi")], category = Alpha, comments = "GREEK SMALL LETTER SAMPI"}
  , Record {uchar = '\1008', commands = [("unicode-math","\\upvarkappa")], category = Alpha, comments = "GREEK KAPPA SYMBOL (round)"}
  , Record {uchar = '\1009', commands = [("base","\\varrho"),("omlmathrm","\\mathrm{\\varrho}"),("unicode-math","\\upvarrho")], category = Alpha, comments = "= \\varrhoup (kpfonts mathdesign), GREEK RHO SYMBOL (round)"}
  , Record {uchar = '\1012', commands = [("unicode-math","\\upvarTheta")], category = Alpha, comments = "x \\varTheta (amssymb), GREEK CAPITAL THETA SYMBOL"}
  , Record {uchar = '\1013', commands = [("base","\\epsilon"),("omlmathrm","\\mathrm{\\epsilon}"),("unicode-math","\\upvarepsilon")], category = Alpha, comments = "= \\epsilonup (kpfonts mathdesign), GREEK LUNATE EPSILON SYMBOL"}
  , Record {uchar = '\1014', commands = [("amssymb","\\backepsilon"),("wrisym","\\backepsilon"),("unicode-math","\\upbackepsilon")], category = Ord, comments = "GREEK REVERSED LUNATE EPSILON SYMBOL"}
  , Record {uchar = '\1064', commands = [], category = Alpha, comments = "t \\CYRSHHA (T2A), Shcy, CYRILLIC CAPITAL LETTER SHA"}
  , Record {uchar = '\8192', commands = [], category = Ord, comments = "enquad"}
  , Record {uchar = '\8193', commands = [("base","\\quad")], category = Ord, comments = "emquad"}
  , Record {uchar = '\8194', commands = [], category = Ord, comments = "ensp (half an em)"}
  , Record {uchar = '\8195', commands = [], category = Ord, comments = "emsp"}
  , Record {uchar = '\8196', commands = [], category = Ord, comments = "THREE-PER-EM SPACE"}
  , Record {uchar = '\8197', commands = [], category = Ord, comments = "FOUR-PER-EM SPACE, mid space"}
  , Record {uchar = '\8198', commands = [], category = Ord, comments = "SIX-PER-EM SPACE"}
  , Record {uchar = '\8199', commands = [], category = Ord, comments = "FIGURE SPACE"}
  , Record {uchar = '\8201', commands = [("base","\\,")], category = Ord, comments = "THIN SPACE"}
  , Record {uchar = '\8202', commands = [], category = Ord, comments = "HAIR SPACE"}
  , Record {uchar = '\8203', commands = [("base","\\hspace{0pt}")], category = Ord, comments = "zwsp"}
  , Record {uchar = '\8208', commands = [], category = Ord, comments = "HYPHEN (true graphic)"}
  , Record {uchar = '\8210', commands = [], category = Ord, comments = "dash"}
  , Record {uchar = '\8211', commands = [], category = Ord, comments = "ndash"}
  , Record {uchar = '\8212', commands = [], category = Ord, comments = "mdash"}
  , Record {uchar = '\8213', commands = [("unicode-math","\\horizbar")], category = Ord, comments = "HORIZONTAL BAR"}
  , Record {uchar = '\8214', commands = [("base","\\|"),("base","\\Vert"),("unicode-math","\\Vert")], category = Fence, comments = "double vertical bar"}
  , Record {uchar = '\8215', commands = [("unicode-math","\\twolowline")], category = Ord, comments = "DOUBLE LOW LINE (spacing)"}
  , Record {uchar = '\8220', commands = [("base","``")], category = Pun, comments = "Opening curly quote"}
  , Record {uchar = '\8221', commands = [("base","\"")], category = Pun, comments = "Closing curly quote"}
  , Record {uchar = '\8224', commands = [("base","\\dagger"),("unicode-math","\\dagger")], category = Bin, comments = "DAGGER relation"}
  , Record {uchar = '\8225', commands = [("base","\\ddagger"),("unicode-math","\\ddagger")], category = Bin, comments = "DOUBLE DAGGER relation"}
  , Record {uchar = '\8226', commands = [("base","\\bullet"),("unicode-math","\\smblkcircle")], category = Bin, comments = "b: round BULLET, filled"}
  , Record {uchar = '\8229', commands = [("unicode-math","\\enleadertwodots")], category = Ord, comments = "double baseline dot (en leader)"}
  , Record {uchar = '\8230', commands = [("base","\\ldots"),("unicode-math","\\unicodeellipsis")], category = Ord, comments = "ellipsis (horizontal)"}
  , Record {uchar = '\8242', commands = [("base","\\prime"),("unicode-math","\\prime")], category = Ord, comments = "PRIME or minute, not superscripted"}
  , Record {uchar = '\8243', commands = [("mathabx","\\second"),("unicode-math","\\dprime")], category = Ord, comments = "DOUBLE PRIME or second, not superscripted"}
  , Record {uchar = '\8244', commands = [("mathabx","\\third"),("unicode-math","\\trprime")], category = Ord, comments = "TRIPLE PRIME (not superscripted)"}
  , Record {uchar = '\8245', commands = [("amssymb","\\backprime"),("unicode-math","\\backprime")], category = Ord, comments = "reverse prime, not superscripted"}
  , Record {uchar = '\8246', commands = [("unicode-math","\\backdprime")], category = Ord, comments = "double reverse prime, not superscripted"}
  , Record {uchar = '\8247', commands = [("unicode-math","\\backtrprime")], category = Ord, comments = "triple reverse prime, not superscripted"}
  , Record {uchar = '\8248', commands = [("unicode-math","\\caretinsert")], category = Ord, comments = "CARET (insertion mark)"}
  , Record {uchar = '\8251', commands = [], category = Ord, comments = "REFERENCE MARK, Japanese kome jirushi"}
  , Record {uchar = '\8252', commands = [("base","!!"),("unicode-math","\\Exclam")], category = Ord, comments = "DOUBLE EXCLAMATION MARK"}
  , Record {uchar = '\8256', commands = [("oz","\\cat"),("unicode-math","\\tieconcat")], category = Bin, comments = "CHARACTER TIE, z notation sequence concatenation"}
  , Record {uchar = '\8259', commands = [("unicode-math","\\hyphenbullet")], category = Ord, comments = "rectangle, filled (HYPHEN BULLET)"}
  , Record {uchar = '\8260', commands = [("base","/"),("unicode-math","\\fracslash")], category = Bin, comments = "FRACTION SLASH"}
  , Record {uchar = '\8263', commands = [("base","??"),("unicode-math","\\Question")], category = Ord, comments = "DOUBLE QUESTION MARK"}
  , Record {uchar = '\8270', commands = [("base","\\ast")], category = Bin, comments = "lowast, LOW ASTERISK"}
  , Record {uchar = '\8271', commands = [], category = Ord, comments = "bsemi, REVERSED SEMICOLON"}
  , Record {uchar = '\8272', commands = [("unicode-math","\\closure")], category = Rel, comments = "CLOSE UP (editing mark)"}
  , Record {uchar = '\8273', commands = [], category = Ord, comments = "Ast"}
  , Record {uchar = '\8274', commands = [("base","./.")], category = Ord, comments = "COMMERCIAL MINUS SIGN"}
  , Record {uchar = '\8279', commands = [("mathabx","\\fourth"),("unicode-math","\\qprime")], category = Ord, comments = "QUADRUPLE PRIME, not superscripted"}
  , Record {uchar = '\8287', commands = [("base","\\:"),("amsmath","\\medspace")], category = Ord, comments = "MEDIUM MATHEMATICAL SPACE, four-eighteenths of an em"}
  , Record {uchar = '\8289', commands = [], category = Ord, comments = "FUNCTION APPLICATION"}
  , Record {uchar = '\8290', commands = [], category = Ord, comments = "INVISIBLE TIMES"}
  , Record {uchar = '\8291', commands = [], category = Ord, comments = "INVISIBLE SEPARATOR"}
  , Record {uchar = '\8292', commands = [], category = Ord, comments = "INVISIBLE PLUS"}
  , Record {uchar = '\8314', commands = [], category = Ord, comments = "SUPERSCRIPT PLUS SIGN subscript operators"}
  , Record {uchar = '\8315', commands = [], category = Ord, comments = "SUPERSCRIPT MINUS subscript operators"}
  , Record {uchar = '\8316', commands = [], category = Ord, comments = "SUPERSCRIPT EQUALS SIGN subscript operators"}
  , Record {uchar = '\8317', commands = [], category = Open, comments = "SUPERSCRIPT LEFT PARENTHESIS subscript operators"}
  , Record {uchar = '\8318', commands = [], category = Close, comments = "SUPERSCRIPT RIGHT PARENTHESIS subscript operators"}
  , Record {uchar = '\8330', commands = [], category = Ord, comments = "SUBSCRIPT PLUS SIGN superscript operators"}
  , Record {uchar = '\8331', commands = [], category = Ord, comments = "SUBSCRIPT MINUS superscript operators"}
  , Record {uchar = '\8332', commands = [], category = Ord, comments = "SUBSCRIPT EQUALS SIGN superscript operators"}
  , Record {uchar = '\8333', commands = [], category = Open, comments = "SUBSCRIPT LEFT PARENTHESIS superscript operators"}
  , Record {uchar = '\8334', commands = [], category = Close, comments = "SUBSCRIPT RIGHT PARENTHESIS superscript operators"}
  , Record {uchar = '\8364', commands = [("unicode-math","\\euro")], category = Ord, comments = "EURO SIGN"}
  , Record {uchar = '\8400', commands = [("wrisym","\\lvec"),("unicode-math","\\leftharpoonaccent")], category = Accent, comments = "COMBINING LEFT HARPOON ABOVE"}
  , Record {uchar = '\8401', commands = [("wrisym","\\vec"),("unicode-math","\\rightharpoonaccent")], category = Accent, comments = "COMBINING RIGHT HARPOON ABOVE"}
  , Record {uchar = '\8402', commands = [("unicode-math","\\vertoverlay")], category = Accent, comments = "COMBINING LONG VERTICAL LINE OVERLAY"}
  , Record {uchar = '\8403', commands = [], category = Accent, comments = "COMBINING SHORT VERTICAL LINE OVERLAY"}
  , Record {uchar = '\8404', commands = [], category = Accent, comments = "COMBINING ANTICLOCKWISE ARROW ABOVE"}
  , Record {uchar = '\8406', commands = [("wrisym","\\LVec"),("base","\\overleftarrow"),("unicode-math","\\overleftarrow")], category = Accent, comments = "COMBINING LEFT ARROW ABOVE"}
  , Record {uchar = '\8407', commands = [("base","\\vec"),("wrisym","\\Vec"),("unicode-math","\\vec")], category = Accent, comments = "# \\overrightarrow, COMBINING RIGHT ARROW ABOVE"}
  , Record {uchar = '\8408', commands = [], category = Accent, comments = "COMBINING RING OVERLAY"}
  , Record {uchar = '\8409', commands = [], category = Accent, comments = "COMBINING CLOCKWISE RING OVERLAY"}
  , Record {uchar = '\8410', commands = [], category = Accent, comments = "COMBINING ANTICLOCKWISE RING OVERLAY"}
  , Record {uchar = '\8411', commands = [("amsmath","\\dddot"),("wrisym","\\DDDot"),("unicode-math","\\dddot")], category = Accent, comments = "COMBINING THREE DOTS ABOVE"}
  , Record {uchar = '\8412', commands = [("amsmath","\\ddddot"),("unicode-math","\\ddddot")], category = Accent, comments = "COMBINING FOUR DOTS ABOVE"}
  , Record {uchar = '\8413', commands = [("unicode-math","\\enclosecircle")], category = Accent, comments = "COMBINING ENCLOSING CIRCLE"}
  , Record {uchar = '\8414', commands = [("unicode-math","\\enclosesquare")], category = Accent, comments = "COMBINING ENCLOSING SQUARE"}
  , Record {uchar = '\8415', commands = [("unicode-math","\\enclosediamond")], category = Accent, comments = "COMBINING ENCLOSING DIAMOND"}
  , Record {uchar = '\8417', commands = [("amsmath","\\overleftrightarrow"),("unicode-math","\\overleftrightarrow")], category = Accent, comments = "COMBINING LEFT RIGHT ARROW ABOVE"}
  , Record {uchar = '\8420', commands = [("unicode-math","\\enclosetriangle")], category = Accent, comments = "COMBINING ENCLOSING UPWARD POINTING TRIANGLE"}
  , Record {uchar = '\8421', commands = [], category = Accent, comments = "COMBINING REVERSE SOLIDUS OVERLAY"}
  , Record {uchar = '\8422', commands = [], category = Accent, comments = "COMBINING DOUBLE VERTICAL STROKE OVERLAY, z notation finite function diacritic"}
  , Record {uchar = '\8423', commands = [("unicode-math","\\annuity")], category = Accent, comments = "COMBINING ANNUITY SYMBOL"}
  , Record {uchar = '\8424', commands = [("unicode-math","\\threeunderdot")], category = Accent, comments = "COMBINING TRIPLE UNDERDOT"}
  , Record {uchar = '\8425', commands = [("unicode-math","\\widebridgeabove")], category = Accent, comments = "COMBINING WIDE BRIDGE ABOVE"}
  , Record {uchar = '\8426', commands = [], category = Accent, comments = "COMBINING LEFTWARDS ARROW OVERLAY"}
  , Record {uchar = '\8427', commands = [], category = Accent, comments = "COMBINING LONG DOUBLE SOLIDUS OVERLAY"}
  , Record {uchar = '\8428', commands = [("unicode-math","\\underrightharpoondown")], category = Accent, comments = "COMBINING RIGHTWARDS HARPOON WITH BARB DOWNWARDS"}
  , Record {uchar = '\8429', commands = [("unicode-math","\\underleftharpoondown")], category = Accent, comments = "COMBINING LEFTWARDS HARPOON WITH BARB DOWNWARDS"}
  , Record {uchar = '\8430', commands = [("amsmath","\\underleftarrow"),("unicode-math","\\underleftarrow")], category = Accent, comments = "COMBINING LEFT ARROW BELOW"}
  , Record {uchar = '\8431', commands = [("amsmath","\\underrightarrow"),("unicode-math","\\underrightarrow")], category = Accent, comments = "COMBINING RIGHT ARROW BELOW"}
  , Record {uchar = '\8432', commands = [("unicode-math","\\asteraccent")], category = Accent, comments = "COMBINING ASTERISK ABOVE"}
  , Record {uchar = '\8450', commands = [("mathbb","\\mathbb{C}"),("dsfont","\\mathds{C}"),("unicode-math","\\BbbC")], category = Alpha, comments = "open face C"}
  , Record {uchar = '\8455', commands = [("wrisym","\\Euler"),("unicode-math","\\Eulerconst")], category = Ord, comments = "EULER CONSTANT"}
  , Record {uchar = '\8458', commands = [("urwchancal","\\mathcal{g}"),("unicode-math","\\mscrg")], category = Alpha, comments = "/scr g, script small letter g"}
  , Record {uchar = '\8459', commands = [("base","\\mathcal{H}"),("unicode-math","\\mscrH")], category = Alpha, comments = "hamiltonian (script capital H)"}
  , Record {uchar = '\8460', commands = [("eufrak","\\mathfrak{H}"),("unicode-math","\\mfrakH")], category = Alpha, comments = "/frak H, black-letter capital H"}
  , Record {uchar = '\8461', commands = [("mathbb","\\mathbb{H}"),("dsfont","\\mathds{H}"),("unicode-math","\\BbbH")], category = Alpha, comments = "open face capital H"}
  , Record {uchar = '\8462', commands = [("base","h"),("unicode-math","\\Planckconst")], category = Ord, comments = "Planck constant"}
  , Record {uchar = '\8463', commands = [("amssymb","\\hslash"),("fourier","\\hslash"),("arevmath","\\hslash"),("wrisym","\\HBar"),("unicode-math","\\hslash")], category = Alpha, comments = "#\\hbar, Planck's h over 2pi"}
  , Record {uchar = '\8464', commands = [("base","\\mathcal{I}"),("unicode-math","\\mscrI")], category = Alpha, comments = "/scr I, script capital I"}
  , Record {uchar = '\8465', commands = [("base","\\Im"),("eufrak","\\mathfrak{I}"),("unicode-math","\\Im")], category = Alpha, comments = "imaginary part"}
  , Record {uchar = '\8466', commands = [("base","\\mathcal{L}"),("unicode-math","\\mscrL")], category = Alpha, comments = "lagrangian (script capital L)"}
  , Record {uchar = '\8467', commands = [("base","\\ell"),("unicode-math","\\ell")], category = Alpha, comments = "cursive small l"}
  , Record {uchar = '\8469', commands = [("mathbb","\\mathbb{N}"),("dsfont","\\mathds{N}"),("unicode-math","\\BbbN")], category = Alpha, comments = "open face N"}
  , Record {uchar = '\8472', commands = [("amssymb","\\wp"),("unicode-math","\\wp")], category = Alpha, comments = "weierstrass p"}
  , Record {uchar = '\8473', commands = [("mathbb","\\mathbb{P}"),("dsfont","\\mathds{P}"),("unicode-math","\\BbbP")], category = Alpha, comments = "open face P"}
  , Record {uchar = '\8474', commands = [("mathbb","\\mathbb{Q}"),("dsfont","\\mathds{Q}"),("unicode-math","\\BbbQ")], category = Alpha, comments = "open face Q"}
  , Record {uchar = '\8475', commands = [("base","\\mathcal{R}"),("unicode-math","\\mscrR")], category = Alpha, comments = "/scr R, script capital R"}
  , Record {uchar = '\8476', commands = [("base","\\Re"),("eufrak","\\mathfrak{R}"),("unicode-math","\\Re")], category = Alpha, comments = "real part"}
  , Record {uchar = '\8477', commands = [("mathbb","\\mathbb{R}"),("dsfont","\\mathds{R}"),("unicode-math","\\BbbR")], category = Alpha, comments = "open face R"}
  , Record {uchar = '\8484', commands = [("mathbb","\\mathbb{Z}"),("dsfont","\\mathds{Z}"),("unicode-math","\\BbbZ")], category = Alpha, comments = "open face Z"}
  , Record {uchar = '\8486', commands = [("mathcomp","\\tcohm"),("base","\\mathrm{\\Omega}")], category = Alpha, comments = "ohm (deprecated in math, use greek letter)"}
  , Record {uchar = '\8487', commands = [("amsfonts","\\mho"),("arevmath","\\mho"),("wrisym","\\Mho"),("unicode-math","\\mho")], category = Ord, comments = "t \\agemO (wasysym), conductance"}
  , Record {uchar = '\8488', commands = [("eufrak","\\mathfrak{Z}"),("unicode-math","\\mfrakZ")], category = Alpha, comments = "/frak Z, black-letter capital Z"}
  , Record {uchar = '\8489', commands = [("unicode-math","\\turnediota")], category = Alpha, comments = "turned iota"}
  , Record {uchar = '\8491', commands = [("wrisym","\\Angstroem"),("base","\\mathring{\\mathrm{A}}"),("unicode-math","\\Angstrom")], category = Alpha, comments = "\197ngstr\246m capital A with ring"}
  , Record {uchar = '\8492', commands = [("base","\\mathcal{B}"),("unicode-math","\\mscrB")], category = Alpha, comments = "bernoulli function (script capital B)"}
  , Record {uchar = '\8493', commands = [("eufrak","\\mathfrak{C}"),("unicode-math","\\mfrakC")], category = Alpha, comments = "black-letter capital C"}
  , Record {uchar = '\8495', commands = [("urwchancal","\\mathcal{e}"),("unicode-math","\\mscre")], category = Alpha, comments = "/scr e, script small letter e"}
  , Record {uchar = '\8496', commands = [("base","\\mathcal{E}"),("unicode-math","\\mscrE")], category = Alpha, comments = "/scr E, script capital E"}
  , Record {uchar = '\8497', commands = [("base","\\mathcal{F}"),("unicode-math","\\mscrF")], category = Alpha, comments = "/scr F, script capital F"}
  , Record {uchar = '\8498', commands = [("amssymb","\\Finv"),("unicode-math","\\Finv")], category = Ord, comments = "TURNED CAPITAL F"}
  , Record {uchar = '\8499', commands = [("base","\\mathcal{M}"),("unicode-math","\\mscrM")], category = Alpha, comments = "physics m-matrix (SCRIPT CAPITAL M)"}
  , Record {uchar = '\8500', commands = [("urwchancal","\\mathcal{o}"),("unicode-math","\\mscro")], category = Alpha, comments = "order of (SCRIPT SMALL O)"}
  , Record {uchar = '\8501', commands = [("base","\\aleph"),("unicode-math","\\aleph")], category = Alpha, comments = "aleph, hebrew"}
  , Record {uchar = '\8502', commands = [("amssymb","\\beth"),("wrisym","\\beth"),("unicode-math","\\beth")], category = Alpha, comments = "beth, hebrew"}
  , Record {uchar = '\8503', commands = [("amssymb","\\gimel"),("wrisym","\\gimel"),("unicode-math","\\gimel")], category = Alpha, comments = "gimel, hebrew"}
  , Record {uchar = '\8504', commands = [("amssymb","\\daleth"),("wrisym","\\daleth"),("unicode-math","\\daleth")], category = Alpha, comments = "daleth, hebrew"}
  , Record {uchar = '\8508', commands = [("bbold","\\mathbb{\\pi}"),("unicode-math","\\Bbbpi")], category = Ord, comments = "\\DoublePi (wrisym), DOUBLE-STRUCK SMALL PI"}
  , Record {uchar = '\8509', commands = [("bbold","\\mathbb{\\gamma}"),("unicode-math","\\Bbbgamma")], category = Alpha, comments = "\\EulerGamma (wrisym), DOUBLE-STRUCK SMALL GAMMA"}
  , Record {uchar = '\8510', commands = [("bbold","\\mathbb{\\Gamma}"),("unicode-math","\\BbbGamma")], category = Alpha, comments = "DOUBLE-STRUCK CAPITAL GAMMA"}
  , Record {uchar = '\8511', commands = [("bbold","\\mathbb{\\Pi}"),("unicode-math","\\BbbPi")], category = Alpha, comments = "DOUBLE-STRUCK CAPITAL PI"}
  , Record {uchar = '\8512', commands = [("bbold","\\mathbb{\\Sigma}"),("unicode-math","\\Bbbsum")], category = Op, comments = "DOUBLE-STRUCK N-ARY SUMMATION"}
  , Record {uchar = '\8513', commands = [("amssymb","\\Game"),("unicode-math","\\Game")], category = Ord, comments = "TURNED SANS-SERIF CAPITAL G (amssymb has mirrored G)"}
  , Record {uchar = '\8514', commands = [("unicode-math","\\sansLturned")], category = Ord, comments = "TURNED SANS-SERIF CAPITAL L"}
  , Record {uchar = '\8515', commands = [("unicode-math","\\sansLmirrored")], category = Ord, comments = "REVERSED SANS-SERIF CAPITAL L"}
  , Record {uchar = '\8516', commands = [("stmaryrd","\\Yup"),("unicode-math","\\Yup")], category = Ord, comments = "TURNED SANS-SERIF CAPITAL Y"}
  , Record {uchar = '\8517', commands = [("wrisym","\\CapitalDifferentialD"),("wrisym","\\DD"),("unicode-math","\\mitBbbD")], category = Ord, comments = "DOUBLE-STRUCK ITALIC CAPITAL D"}
  , Record {uchar = '\8518', commands = [("wrisym","\\DifferentialD"),("wrisym","\\dd"),("unicode-math","\\mitBbbd")], category = Ord, comments = "DOUBLE-STRUCK ITALIC SMALL D"}
  , Record {uchar = '\8519', commands = [("wrisym","\\ExponetialE"),("wrisym","\\ee"),("unicode-math","\\mitBbbe")], category = Ord, comments = "DOUBLE-STRUCK ITALIC SMALL E"}
  , Record {uchar = '\8520', commands = [("wrisym","\\ComplexI"),("wrisym","\\ii"),("unicode-math","\\mitBbbi")], category = Ord, comments = "DOUBLE-STRUCK ITALIC SMALL I"}
  , Record {uchar = '\8521', commands = [("wrisym","\\ComplexJ"),("wrisym","\\jj"),("unicode-math","\\mitBbbj")], category = Ord, comments = "DOUBLE-STRUCK ITALIC SMALL J"}
  , Record {uchar = '\8522', commands = [("unicode-math","\\PropertyLine")], category = Ord, comments = "PROPERTY LINE"}
  , Record {uchar = '\8523', commands = [("txfonts","\\invamp"),("stmaryrd","\\bindnasrepma"),("unicode-math","\\upand")], category = Bin, comments = "TURNED AMPERSAND"}
  , Record {uchar = '\8592', commands = [("base","\\leftarrow"),("base","\\gets"),("unicode-math","\\leftarrow")], category = Rel, comments = "a: leftward arrow"}
  , Record {uchar = '\8593', commands = [("base","\\uparrow"),("unicode-math","\\uparrow")], category = Rel, comments = "upward arrow"}
  , Record {uchar = '\8594', commands = [("base","\\rightarrow"),("base","\\to"),("unicode-math","\\rightarrow")], category = Rel, comments = "= \\tfun (oz), = \\fun (oz), rightward arrow, z notation total function"}
  , Record {uchar = '\8595', commands = [("base","\\downarrow"),("unicode-math","\\downarrow")], category = Rel, comments = "downward arrow"}
  , Record {uchar = '\8596', commands = [("base","\\leftrightarrow"),("oz","\\rel"),("unicode-math","\\leftrightarrow")], category = Rel, comments = "LEFT RIGHT ARROW, z notation relation"}
  , Record {uchar = '\8597', commands = [("base","\\updownarrow"),("unicode-math","\\updownarrow")], category = Rel, comments = "up and down arrow"}
  , Record {uchar = '\8598', commands = [("amssymb","\\nwarrow"),("unicode-math","\\nwarrow")], category = Rel, comments = "nw pointing arrow"}
  , Record {uchar = '\8599', commands = [("base","\\nearrow"),("unicode-math","\\nearrow")], category = Rel, comments = "ne pointing arrow"}
  , Record {uchar = '\8600', commands = [("base","\\searrow"),("unicode-math","\\searrow")], category = Rel, comments = "se pointing arrow"}
  , Record {uchar = '\8601', commands = [("base","\\swarrow"),("unicode-math","\\swarrow")], category = Rel, comments = "sw pointing arrow"}
  , Record {uchar = '\8602', commands = [("amssymb","\\nleftarrow"),("unicode-math","\\nleftarrow")], category = Rel, comments = "not left arrow"}
  , Record {uchar = '\8603', commands = [("amssymb","\\nrightarrow"),("unicode-math","\\nrightarrow")], category = Rel, comments = "not right arrow"}
  , Record {uchar = '\8604', commands = [("unicode-math","\\leftwavearrow")], category = Rel, comments = "left arrow-wavy"}
  , Record {uchar = '\8605', commands = [("unicode-math","\\rightwavearrow")], category = Rel, comments = "right arrow-wavy"}
  , Record {uchar = '\8606', commands = [("amssymb","\\twoheadleftarrow"),("unicode-math","\\twoheadleftarrow")], category = Rel, comments = "left two-headed arrow"}
  , Record {uchar = '\8607', commands = [("unicode-math","\\twoheaduparrow")], category = Rel, comments = "up two-headed arrow"}
  , Record {uchar = '\8608', commands = [("amssymb","\\twoheadrightarrow"),("oz","\\tsur"),("unicode-math","\\twoheadrightarrow")], category = Rel, comments = "= \\surj (oz), right two-headed arrow, z notation total surjection"}
  , Record {uchar = '\8609', commands = [("unicode-math","\\twoheaddownarrow")], category = Rel, comments = "down two-headed arrow"}
  , Record {uchar = '\8610', commands = [("amssymb","\\leftarrowtail"),("unicode-math","\\leftarrowtail")], category = Rel, comments = "left arrow-tailed"}
  , Record {uchar = '\8611', commands = [("amssymb","\\rightarrowtail"),("oz","\\tinj"),("unicode-math","\\rightarrowtail")], category = Rel, comments = "= \\inj (oz), right arrow-tailed, z notation total injection"}
  , Record {uchar = '\8612', commands = [("stmaryrd","\\mapsfrom"),("kpfonts","\\mappedfrom"),("unicode-math","\\mapsfrom")], category = Rel, comments = "maps to, leftward"}
  , Record {uchar = '\8613', commands = [("wrisym","\\MapsUp"),("unicode-math","\\mapsup")], category = Rel, comments = "maps to, upward"}
  , Record {uchar = '\8614', commands = [("base","\\mapsto"),("unicode-math","\\mapsto")], category = Rel, comments = "maps to, rightward, z notation maplet"}
  , Record {uchar = '\8615', commands = [("wrisym","\\MapsDown"),("unicode-math","\\mapsdown")], category = Rel, comments = "maps to, downward"}
  , Record {uchar = '\8616', commands = [("unicode-math","\\updownarrowbar")], category = Ord, comments = "UP DOWN ARROW WITH BASE (perpendicular)"}
  , Record {uchar = '\8617', commands = [("base","\\hookleftarrow"),("unicode-math","\\hookleftarrow")], category = Rel, comments = "left arrow-hooked"}
  , Record {uchar = '\8618', commands = [("base","\\hookrightarrow"),("unicode-math","\\hookrightarrow")], category = Rel, comments = "right arrow-hooked"}
  , Record {uchar = '\8619', commands = [("amssymb","\\looparrowleft"),("unicode-math","\\looparrowleft")], category = Rel, comments = "left arrow-looped"}
  , Record {uchar = '\8620', commands = [("amssymb","\\looparrowright"),("unicode-math","\\looparrowright")], category = Rel, comments = "right arrow-looped"}
  , Record {uchar = '\8621', commands = [("amssymb","\\leftrightsquigarrow"),("unicode-math","\\leftrightsquigarrow")], category = Rel, comments = "left and right arr-wavy"}
  , Record {uchar = '\8622', commands = [("amssymb","\\nleftrightarrow"),("unicode-math","\\nleftrightarrow")], category = Rel, comments = "not left and right arrow"}
  , Record {uchar = '\8623', commands = [("stmaryrd","\\lightning"),("unicode-math","\\downzigzagarrow")], category = Rel, comments = "t \\Lightning (marvosym), DOWNWARDS ZIGZAG ARROW"}
  , Record {uchar = '\8624', commands = [("amssymb","\\Lsh"),("unicode-math","\\Lsh")], category = Rel, comments = "a: UPWARDS ARROW WITH TIP LEFTWARDS"}
  , Record {uchar = '\8625', commands = [("amssymb","\\Rsh"),("unicode-math","\\Rsh")], category = Rel, comments = "a: UPWARDS ARROW WITH TIP RIGHTWARDS"}
  , Record {uchar = '\8626', commands = [("mathabx","\\dlsh"),("unicode-math","\\Ldsh")], category = Rel, comments = "left down angled arrow"}
  , Record {uchar = '\8627', commands = [("mathabx","\\drsh"),("unicode-math","\\Rdsh")], category = Rel, comments = "right down angled arrow"}
  , Record {uchar = '\8628', commands = [("unicode-math","\\linefeed")], category = Ord, comments = "RIGHTWARDS ARROW WITH CORNER DOWNWARDS"}
  , Record {uchar = '\8629', commands = [("unicode-math","\\carriagereturn")], category = Ord, comments = "downwards arrow with corner leftward = carriage return"}
  , Record {uchar = '\8630', commands = [("amssymb","\\curvearrowleft"),("fourier","\\curvearrowleft"),("unicode-math","\\curvearrowleft")], category = Rel, comments = "left curved arrow"}
  , Record {uchar = '\8631', commands = [("amssymb","\\curvearrowright"),("fourier","\\curvearrowright"),("unicode-math","\\curvearrowright")], category = Rel, comments = "right curved arrow"}
  , Record {uchar = '\8632', commands = [("unicode-math","\\barovernorthwestarrow")], category = Ord, comments = "NORTH WEST ARROW TO LONG BAR"}
  , Record {uchar = '\8633', commands = [("unicode-math","\\barleftarrowrightarrowba")], category = Ord, comments = "LEFTWARDS ARROW TO BAR OVER RIGHTWARDS ARROW TO BAR"}
  , Record {uchar = '\8634', commands = [("amssymb","\\circlearrowleft"),("wasysym","\\leftturn"),("unicode-math","\\acwopencirclearrow")], category = Ord, comments = "ANTICLOCKWISE OPEN CIRCLE ARROW"}
  , Record {uchar = '\8635', commands = [("amssymb","\\circlearrowright"),("wasysym","\\rightturn"),("unicode-math","\\cwopencirclearrow")], category = Ord, comments = "CLOCKWISE OPEN CIRCLE ARROW"}
  , Record {uchar = '\8636', commands = [("base","\\leftharpoonup"),("unicode-math","\\leftharpoonup")], category = Rel, comments = "left harpoon-up"}
  , Record {uchar = '\8637', commands = [("base","\\leftharpoondown"),("unicode-math","\\leftharpoondown")], category = Rel, comments = "left harpoon-down"}
  , Record {uchar = '\8638', commands = [("amssymb","\\upharpoonright"),("amssymb","\\restriction"),("unicode-math","\\upharpoonright")], category = Rel, comments = "= \\upharpoonrightup (wrisym), a: up harpoon-right"}
  , Record {uchar = '\8639', commands = [("amssymb","\\upharpoonleft"),("wrisym","\\upharpoonleftup"),("unicode-math","\\upharpoonleft")], category = Rel, comments = "up harpoon-left"}
  , Record {uchar = '\8640', commands = [("base","\\rightharpoonup"),("unicode-math","\\rightharpoonup")], category = Rel, comments = "right harpoon-up"}
  , Record {uchar = '\8641', commands = [("base","\\rightharpoondown"),("unicode-math","\\rightharpoondown")], category = Rel, comments = "right harpoon-down"}
  , Record {uchar = '\8642', commands = [("amssymb","\\downharpoonright"),("wrisym","\\upharpoonrightdown"),("unicode-math","\\downharpoonright")], category = Rel, comments = "down harpoon-right"}
  , Record {uchar = '\8643', commands = [("amssymb","\\downharpoonleft"),("wrisym","\\upharpoonleftdown"),("unicode-math","\\downharpoonleft")], category = Rel, comments = "down harpoon-left"}
  , Record {uchar = '\8644', commands = [("amssymb","\\rightleftarrows"),("wrisym","\\rightleftarrow"),("unicode-math","\\rightleftarrows")], category = Rel, comments = "right arrow over left arrow"}
  , Record {uchar = '\8645', commands = [("mathabx","\\updownarrows"),("wrisym","\\uparrowdownarrow"),("unicode-math","\\updownarrows")], category = Rel, comments = "up arrow, down arrow"}
  , Record {uchar = '\8646', commands = [("amssymb","\\leftrightarrows"),("wrisym","\\leftrightarrow"),("unicode-math","\\leftrightarrows")], category = Rel, comments = "left arrow over right arrow"}
  , Record {uchar = '\8647', commands = [("amssymb","\\leftleftarrows"),("fourier","\\leftleftarrows"),("unicode-math","\\leftleftarrows")], category = Rel, comments = "two left arrows"}
  , Record {uchar = '\8648', commands = [("amssymb","\\upuparrows"),("unicode-math","\\upuparrows")], category = Rel, comments = "two up arrows"}
  , Record {uchar = '\8649', commands = [("amssymb","\\rightrightarrows"),("fourier","\\rightrightarrows"),("unicode-math","\\rightrightarrows")], category = Rel, comments = "two right arrows"}
  , Record {uchar = '\8650', commands = [("amssymb","\\downdownarrows"),("unicode-math","\\downdownarrows")], category = Rel, comments = "two down arrows"}
  , Record {uchar = '\8651', commands = [("amssymb","\\leftrightharpoons"),("wrisym","\\revequilibrium"),("unicode-math","\\leftrightharpoons")], category = Rel, comments = "left harpoon over right"}
  , Record {uchar = '\8652', commands = [("base","\\rightleftharpoons"),("wrisym","\\equilibrium"),("unicode-math","\\rightleftharpoons")], category = Rel, comments = "right harpoon over left"}
  , Record {uchar = '\8653', commands = [("amssymb","\\nLeftarrow"),("unicode-math","\\nLeftarrow")], category = Rel, comments = "not implied by"}
  , Record {uchar = '\8654', commands = [("amssymb","\\nLeftrightarrow"),("unicode-math","\\nLeftrightarrow")], category = Rel, comments = "not left and right double arrows"}
  , Record {uchar = '\8655', commands = [("amssymb","\\nRightarrow"),("unicode-math","\\nRightarrow")], category = Rel, comments = "not implies"}
  , Record {uchar = '\8656', commands = [("base","\\Leftarrow"),("unicode-math","\\Leftarrow")], category = Rel, comments = "left double arrow"}
  , Record {uchar = '\8657', commands = [("base","\\Uparrow"),("unicode-math","\\Uparrow")], category = Rel, comments = "up double arrow"}
  , Record {uchar = '\8658', commands = [("base","\\Rightarrow"),("unicode-math","\\Rightarrow")], category = Rel, comments = "right double arrow"}
  , Record {uchar = '\8659', commands = [("base","\\Downarrow"),("unicode-math","\\Downarrow")], category = Rel, comments = "down double arrow"}
  , Record {uchar = '\8660', commands = [("base","\\Leftrightarrow"),("unicode-math","\\Leftrightarrow")], category = Rel, comments = "left and right double arrow"}
  , Record {uchar = '\8661', commands = [("base","\\Updownarrow"),("unicode-math","\\Updownarrow")], category = Rel, comments = "up and down double arrow"}
  , Record {uchar = '\8662', commands = [("txfonts","\\Nwarrow"),("unicode-math","\\Nwarrow")], category = Rel, comments = "nw pointing double arrow"}
  , Record {uchar = '\8663', commands = [("txfonts","\\Nearrow"),("unicode-math","\\Nearrow")], category = Rel, comments = "ne pointing double arrow"}
  , Record {uchar = '\8664', commands = [("txfonts","\\Searrow"),("unicode-math","\\Searrow")], category = Rel, comments = "se pointing double arrow"}
  , Record {uchar = '\8665', commands = [("txfonts","\\Swarrow"),("unicode-math","\\Swarrow")], category = Rel, comments = "sw pointing double arrow"}
  , Record {uchar = '\8666', commands = [("amssymb","\\Lleftarrow"),("unicode-math","\\Lleftarrow")], category = Rel, comments = "left triple arrow"}
  , Record {uchar = '\8667', commands = [("amssymb","\\Rrightarrow"),("unicode-math","\\Rrightarrow")], category = Rel, comments = "right triple arrow"}
  , Record {uchar = '\8668', commands = [("mathabx","\\leftsquigarrow"),("txfonts","\\leftsquigarrow"),("unicode-math","\\leftsquigarrow")], category = Rel, comments = "LEFTWARDS SQUIGGLE ARROW"}
  , Record {uchar = '\8669', commands = [("amssymb","\\rightsquigarrow"),("unicode-math","\\rightsquigarrow")], category = Rel, comments = "RIGHTWARDS SQUIGGLE ARROW"}
  , Record {uchar = '\8670', commands = [("unicode-math","\\nHuparrow")], category = Ord, comments = "UPWARDS ARROW WITH DOUBLE STROKE"}
  , Record {uchar = '\8671', commands = [("unicode-math","\\nHdownarrow")], category = Ord, comments = "DOWNWARDS ARROW WITH DOUBLE STROKE"}
  , Record {uchar = '\8672', commands = [("amsfonts","\\dashleftarrow"),("unicode-math","\\leftdasharrow")], category = Ord, comments = "LEFTWARDS DASHED ARROW"}
  , Record {uchar = '\8673', commands = [("unicode-math","\\updasharrow")], category = Ord, comments = "UPWARDS DASHED ARROW"}
  , Record {uchar = '\8674', commands = [("amsfonts","\\dashrightarrow"),("amsfonts","\\dasharrow"),("unicode-math","\\rightdasharrow")], category = Ord, comments = "RIGHTWARDS DASHED ARROW"}
  , Record {uchar = '\8675', commands = [("unicode-math","\\downdasharrow")], category = Ord, comments = "DOWNWARDS DASHED ARROW"}
  , Record {uchar = '\8676', commands = [("wrisym","\\LeftArrowBar"),("unicode-math","\\barleftarrow")], category = Rel, comments = "LEFTWARDS ARROW TO BAR"}
  , Record {uchar = '\8677', commands = [("wrisym","\\RightArrowBar"),("unicode-math","\\rightarrowbar")], category = Rel, comments = "RIGHTWARDS ARROW TO BAR"}
  , Record {uchar = '\8678', commands = [("unicode-math","\\leftwhitearrow")], category = Ord, comments = "LEFTWARDS WHITE ARROW"}
  , Record {uchar = '\8679', commands = [("unicode-math","\\upwhitearrow")], category = Ord, comments = "UPWARDS WHITE ARROW"}
  , Record {uchar = '\8680', commands = [("unicode-math","\\rightwhitearrow")], category = Ord, comments = "RIGHTWARDS WHITE ARROW"}
  , Record {uchar = '\8681', commands = [("unicode-math","\\downwhitearrow")], category = Ord, comments = "DOWNWARDS WHITE ARROW"}
  , Record {uchar = '\8682', commands = [("unicode-math","\\whitearrowupfrombar")], category = Ord, comments = "UPWARDS WHITE ARROW FROM BAR"}
  , Record {uchar = '\8683', commands = [], category = Ord, comments = "UPWARDS WHITE ARROW ON PEDESTAL"}
  , Record {uchar = '\8684', commands = [], category = Ord, comments = "UPWARDS WHITE ARROW ON PEDESTAL WITH HORIZONTAL BAR"}
  , Record {uchar = '\8685', commands = [], category = Ord, comments = "UPWARDS WHITE ARROW ON PEDESTAL WITH VERTICAL BAR"}
  , Record {uchar = '\8686', commands = [], category = Ord, comments = "UPWARDS WHITE DOUBLE ARROW"}
  , Record {uchar = '\8687', commands = [], category = Ord, comments = "UPWARDS WHITE DOUBLE ARROW ON PEDESTAL"}
  , Record {uchar = '\8688', commands = [], category = Ord, comments = "RIGHTWARDS WHITE ARROW FROM WALL"}
  , Record {uchar = '\8689', commands = [], category = Ord, comments = "NORTH WEST ARROW TO CORNER"}
  , Record {uchar = '\8690', commands = [], category = Ord, comments = "SOUTH EAST ARROW TO CORNER"}
  , Record {uchar = '\8691', commands = [], category = Ord, comments = "UP DOWN WHITE ARROW"}
  , Record {uchar = '\8692', commands = [("unicode-math","\\circleonrightarrow")], category = Rel, comments = "RIGHT ARROW WITH SMALL CIRCLE"}
  , Record {uchar = '\8693', commands = [("mathabx","\\downuparrows"),("wrisym","\\downarrowuparrow"),("unicode-math","\\downuparrows")], category = Rel, comments = "DOWNWARDS ARROW LEFTWARDS OF UPWARDS ARROW"}
  , Record {uchar = '\8694', commands = [("unicode-math","\\rightthreearrows")], category = Rel, comments = "THREE RIGHTWARDS ARROWS"}
  , Record {uchar = '\8695', commands = [("unicode-math","\\nvleftarrow")], category = Rel, comments = "LEFTWARDS ARROW WITH VERTICAL STROKE"}
  , Record {uchar = '\8696', commands = [("oz","\\pfun"),("unicode-math","\\nvrightarrow")], category = Rel, comments = "RIGHTWARDS ARROW WITH VERTICAL STROKE, z notation partial function"}
  , Record {uchar = '\8697', commands = [("unicode-math","\\nvleftrightarrow")], category = Rel, comments = "LEFT RIGHT ARROW WITH VERTICAL STROKE, z notation partial relation"}
  , Record {uchar = '\8698', commands = [("unicode-math","\\nVleftarrow")], category = Rel, comments = "LEFTWARDS ARROW WITH DOUBLE VERTICAL STROKE"}
  , Record {uchar = '\8699', commands = [("oz","\\ffun"),("unicode-math","\\nVrightarrow")], category = Rel, comments = "RIGHTWARDS ARROW WITH DOUBLE VERTICAL STROKE, z notation finite function"}
  , Record {uchar = '\8700', commands = [("unicode-math","\\nVleftrightarrow")], category = Rel, comments = "LEFT RIGHT ARROW WITH DOUBLE VERTICAL STROKE, z notation finite relation"}
  , Record {uchar = '\8701', commands = [("stmaryrd","\\leftarrowtriangle"),("unicode-math","\\leftarrowtriangle")], category = Rel, comments = "LEFTWARDS OPEN-HEADED ARROW"}
  , Record {uchar = '\8702', commands = [("stmaryrd","\\rightarrowtriangle"),("unicode-math","\\rightarrowtriangle")], category = Rel, comments = "RIGHTWARDS OPEN-HEADED ARROW"}
  , Record {uchar = '\8703', commands = [("stmaryrd","\\leftrightarrowtriangle"),("unicode-math","\\leftrightarrowtriangle")], category = Rel, comments = "LEFT RIGHT OPEN-HEADED ARROW"}
  , Record {uchar = '\8704', commands = [("base","\\forall"),("unicode-math","\\forall")], category = Ord, comments = "FOR ALL"}
  , Record {uchar = '\8705', commands = [("amssymb","\\complement"),("fourier","\\complement"),("unicode-math","\\complement")], category = Ord, comments = "COMPLEMENT sign"}
  , Record {uchar = '\8706', commands = [("base","\\partial"),("kpfonts","\\partialup"),("unicode-math","\\partial")], category = Ord, comments = "PARTIAL DIFFERENTIAL"}
  , Record {uchar = '\8707', commands = [("base","\\exists"),("oz","\\exi"),("unicode-math","\\exists")], category = Ord, comments = "at least one exists"}
  , Record {uchar = '\8708', commands = [("amssymb","\\nexists"),("fourier","\\nexists"),("oz","\\nexi"),("unicode-math","\\nexists")], category = Ord, comments = "negated exists"}
  , Record {uchar = '\8709', commands = [("amssymb","\\varnothing"),("unicode-math","\\varnothing")], category = Ord, comments = "circle, slash"}
  , Record {uchar = '\8710', commands = [("base","\\mathrm{\\Delta}"),("unicode-math","\\increment")], category = Ord, comments = "laplacian (Delta; nabla square)"}
  , Record {uchar = '\8711', commands = [("base","\\nabla"),("unicode-math","\\nabla")], category = Ord, comments = "NABLA, del, hamilton operator"}
  , Record {uchar = '\8712', commands = [("base","\\in"),("unicode-math","\\in")], category = Rel, comments = "set membership, variant"}
  , Record {uchar = '\8713', commands = [("base","\\notin"),("wrisym","\\nin"),("unicode-math","\\notin")], category = Rel, comments = "negated set membership"}
  , Record {uchar = '\8714', commands = [("base","\\in"),("unicode-math","\\smallin")], category = Rel, comments = "set membership (small set membership)"}
  , Record {uchar = '\8715', commands = [("base","\\ni"),("base","\\owns"),("unicode-math","\\ni")], category = Rel, comments = "contains, variant"}
  , Record {uchar = '\8716', commands = [("wrisym","\\nni"),("txfonts","\\notni"),("unicode-math","\\nni")], category = Rel, comments = "= \\notowner (mathabx), = \\notowns (fourier), negated contains, variant"}
  , Record {uchar = '\8717', commands = [("base","\\ni"), ("unicode-math","\\smallni")], category = Rel, comments = "r: contains (SMALL CONTAINS AS MEMBER)"}
  , Record {uchar = '\8718', commands = [("amssymb","\\blacksquare"),("unicode-math","\\QED")], category = Ord, comments = "END OF PROOF"}
  , Record {uchar = '\8719', commands = [("base","\\prod"),("unicode-math","\\prod")], category = Op, comments = "product operator"}
  , Record {uchar = '\8720', commands = [("base","\\coprod"),("unicode-math","\\coprod")], category = Op, comments = "coproduct operator"}
  , Record {uchar = '\8721', commands = [("base","\\sum"),("unicode-math","\\sum")], category = Op, comments = "summation operator"}
  , Record {uchar = '\8722', commands = [("base","-"),("unicode-math","\\minus")], category = Bin, comments = "MINUS SIGN"}
  , Record {uchar = '\8723', commands = [("base","\\mp"),("unicode-math","\\mp")], category = Bin, comments = "MINUS-OR-PLUS SIGN"}
  , Record {uchar = '\8724', commands = [("amssymb","\\dotplus"),("unicode-math","\\dotplus")], category = Bin, comments = "plus sign, dot above"}
  , Record {uchar = '\8725', commands = [("base","\\slash"),("unicode-math","\\divslash")], category = Bin, comments = "DIVISION SLASH"}
  , Record {uchar = '\8726', commands = [("amssymb","\\smallsetminus"),("fourier","\\smallsetminus"),("unicode-math","\\smallsetminus")], category = Bin, comments = "small SET MINUS (cf. reverse solidus)"}
  , Record {uchar = '\8727', commands = [("base","\\ast"),("unicode-math","\\ast")], category = Bin, comments = "ASTERISK OPERATOR (Hodge star operator)"}
  , Record {uchar = '\8728', commands = [("base","\\circ"),("unicode-math","\\vysmwhtcircle")], category = Bin, comments = "composite function (small circle)"}
  , Record {uchar = '\8729', commands = [("base","\\bullet"),("unicode-math","\\vysmblkcircle")], category = Bin, comments = "BULLET OPERATOR"}
  , Record {uchar = '\8730', commands = [("base","\\sqrt"),("unicode-math","\\sqrt")], category = Rad, comments = "radical"}
  , Record {uchar = '\8731', commands = [("base","\\sqrt[3]"),("unicode-math","\\cuberoot")], category = Rad, comments = "CUBE ROOT"}
  , Record {uchar = '\8732', commands = [("base","\\sqrt[4]"),("unicode-math","\\fourthroot")], category = Rad, comments = "FOURTH ROOT"}
  , Record {uchar = '\8733', commands = [("base","\\propto"),("amssymb","\\varpropto"),("unicode-math","\\propto")], category = Rel, comments = "is PROPORTIONAL TO"}
  , Record {uchar = '\8734', commands = [("base","\\infty"),("unicode-math","\\infty")], category = Ord, comments = "INFINITY"}
  , Record {uchar = '\8735', commands = [("wrisym","\\rightangle"),("unicode-math","\\rightangle")], category = Ord, comments = "right (90 degree) angle"}
  , Record {uchar = '\8736', commands = [("base","\\angle"),("unicode-math","\\angle")], category = Ord, comments = "ANGLE"}
  , Record {uchar = '\8737', commands = [("amssymb","\\measuredangle"),("wrisym","\\measuredangle"),("unicode-math","\\measuredangle")], category = Ord, comments = "MEASURED ANGLE"}
  , Record {uchar = '\8738', commands = [("amssymb","\\sphericalangle"),("wrisym","\\sphericalangle"),("unicode-math","\\sphericalangle")], category = Ord, comments = "SPHERICAL ANGLE"}
  , Record {uchar = '\8739', commands = [("base","\\mid"),("unicode-math","\\mid")], category = Rel, comments = "r: DIVIDES"}
  , Record {uchar = '\8740', commands = [("amssymb","\\nmid"),("unicode-math","\\nmid")], category = Rel, comments = "negated mid, DOES NOT DIVIDE"}
  , Record {uchar = '\8741', commands = [("base","\\parallel"),("unicode-math","\\parallel")], category = Rel, comments = "parallel"}
  , Record {uchar = '\8742', commands = [("amssymb","\\nparallel"),("fourier","\\nparallel"),("unicode-math","\\nparallel")], category = Rel, comments = "not parallel"}
  , Record {uchar = '\8743', commands = [("amssymb","\\wedge"),("base","\\land"),("unicode-math","\\wedge")], category = Bin, comments = "b: LOGICAL AND"}
  , Record {uchar = '\8744', commands = [("base","\\vee"),("base","\\lor"),("unicode-math","\\vee")], category = Bin, comments = "b: LOGICAL OR"}
  , Record {uchar = '\8745', commands = [("base","\\cap"),("unicode-math","\\cap")], category = Bin, comments = "INTERSECTION"}
  , Record {uchar = '\8746', commands = [("base","\\cup"),("unicode-math","\\cup")], category = Bin, comments = "UNION or logical sum"}
  , Record {uchar = '\8747', commands = [("base","\\int"),("unicode-math","\\int")], category = Op, comments = "INTEGRAL operator"}
  , Record {uchar = '\8748', commands = [("amsmath","\\iint"),("fourier","\\iint"),("esint","\\iint"),("wasysym","\\iint"),("unicode-math","\\iint")], category = Op, comments = "DOUBLE INTEGRAL operator"}
  , Record {uchar = '\8749', commands = [("amsmath","\\iiint"),("fourier","\\iiint"),("esint","\\iiint"),("wasysym","\\iiint"),("unicode-math","\\iiint")], category = Op, comments = "TRIPLE INTEGRAL operator"}
  , Record {uchar = '\8750', commands = [("base","\\oint"),("unicode-math","\\oint")], category = Op, comments = "CONTOUR INTEGRAL operator"}
  , Record {uchar = '\8751', commands = [("esint","\\oiint"),("wasysym","\\oiint"),("fourier","\\oiint"),("wrisym","\\dbloint"),("unicode-math","\\oiint")], category = Op, comments = "double contour integral operator"}
  , Record {uchar = '\8752', commands = [("txfonts","\\oiiint"),("fourier","\\oiiint"),("unicode-math","\\oiiint")], category = Op, comments = "triple contour integral operator"}
  , Record {uchar = '\8753', commands = [("unicode-math","\\intclockwise")], category = Op, comments = "CLOCKWISE INTEGRAL"}
  , Record {uchar = '\8754', commands = [("esint","\\varointclockwise"),("wrisym","\\clockoint"),("unicode-math","\\varointclockwise")], category = Op, comments = "contour integral, clockwise"}
  , Record {uchar = '\8755', commands = [("esint","\\ointctrclockwise"),("wrisym","\\cntclockoint"),("unicode-math","\\ointctrclockwise")], category = Op, comments = "contour integral, anticlockwise"}
  , Record {uchar = '\8756', commands = [("amssymb","\\therefore"),("wrisym","\\therefore"),("wasysym","\\wasytherefore"),("unicode-math","\\therefore")], category = Ord, comments = "THEREFORE"}
  , Record {uchar = '\8757', commands = [("amssymb","\\because"),("wrisym","\\because"),("unicode-math","\\because")], category = Ord, comments = "BECAUSE"}
  , Record {uchar = '\8758', commands = [("base",":"),("unicode-math","\\mathratio")], category = Rel, comments = "x \\colon, RATIO"}
  , Record {uchar = '\8759', commands = [("wrisym","\\Proportion"),("base","::"),("unicode-math","\\Colon")], category = Rel, comments = "two colons"}
  , Record {uchar = '\8760', commands = [("unicode-math","\\dotminus")], category = Bin, comments = "minus sign, dot above"}
  , Record {uchar = '\8761', commands = [("txfonts","\\eqcolon"),("base","-:"),("unicode-math","\\dashcolon")], category = Rel, comments = "EXCESS"}
  , Record {uchar = '\8762', commands = [("unicode-math","\\dotsminusdots")], category = Rel, comments = "minus with four dots, GEOMETRIC PROPORTION"}
  , Record {uchar = '\8763', commands = [("unicode-math","\\kernelcontraction")], category = Rel, comments = "HOMOTHETIC"}
  , Record {uchar = '\8764', commands = [("base","\\sim"),("unicode-math","\\sim")], category = Rel, comments = "similar to, TILDE OPERATOR"}
  , Record {uchar = '\8765', commands = [("amssymb","\\backsim"),("unicode-math","\\backsim")], category = Rel, comments = "reverse similar"}
  , Record {uchar = '\8766', commands = [("unicode-math","\\invlazys")], category = Bin, comments = "most positive, INVERTED LAZY S"}
  , Record {uchar = '\8767', commands = [("wasysym","\\AC"),("unicode-math","\\sinewave")], category = Ord, comments = "SINE WAVE, alternating current"}
  , Record {uchar = '\8768', commands = [("amssymb","\\wr"),("unicode-math","\\wr")], category = Bin, comments = "WREATH PRODUCT"}
  , Record {uchar = '\8769', commands = [("amssymb","\\nsim"),("wrisym","\\nsim"),("unicode-math","\\nsim")], category = Rel, comments = "not similar"}
  , Record {uchar = '\8770', commands = [("amssymb","\\eqsim"),("unicode-math","\\eqsim")], category = Rel, comments = "equals, similar"}
  , Record {uchar = '\8771', commands = [("base","\\simeq"),("unicode-math","\\simeq")], category = Rel, comments = "similar, equals"}
  , Record {uchar = '\8772', commands = [("txfonts","\\nsimeq"),("unicode-math","\\nsime")], category = Rel, comments = "not similar, equals"}
  , Record {uchar = '\8773', commands = [("base","\\cong"),("unicode-math","\\cong")], category = Rel, comments = "congruent with"}
  , Record {uchar = '\8774', commands = [("unicode-math","\\simneqq")], category = Rel, comments = "similar, not equals [vert only for 9573 entity]"}
  , Record {uchar = '\8775', commands = [("amssymb","\\ncong"),("wrisym","\\ncong"),("unicode-math","\\ncong")], category = Rel, comments = "not congruent with"}
  , Record {uchar = '\8776', commands = [("base","\\approx"),("unicode-math","\\approx")], category = Rel, comments = "approximate"}
  , Record {uchar = '\8777', commands = [("wrisym","\\napprox"),("unicode-math","\\napprox")], category = Rel, comments = "not approximate"}
  , Record {uchar = '\8778', commands = [("amssymb","\\approxeq"),("unicode-math","\\approxeq")], category = Rel, comments = "approximate, equals"}
  , Record {uchar = '\8779', commands = [("unicode-math","\\approxident")], category = Rel, comments = "approximately identical to"}
  , Record {uchar = '\8780', commands = [("unicode-math","\\backcong")], category = Rel, comments = "ALL EQUAL TO"}
  , Record {uchar = '\8781', commands = [("base","\\asymp"),("unicode-math","\\asymp")], category = Rel, comments = "asymptotically equal to"}
  , Record {uchar = '\8782', commands = [("amssymb","\\Bumpeq"),("wrisym","\\Bumpeq"),("unicode-math","\\Bumpeq")], category = Rel, comments = "bumpy equals"}
  , Record {uchar = '\8783', commands = [("amssymb","\\bumpeq"),("wrisym","\\bumpeq"),("unicode-math","\\bumpeq")], category = Rel, comments = "bumpy equals, equals"}
  , Record {uchar = '\8784', commands = [("base","\\doteq"),("wrisym","\\dotequal"),("unicode-math","\\doteq")], category = Rel, comments = "equals, single dot above"}
  , Record {uchar = '\8785', commands = [("amssymb","\\Doteq"),("amssymb","\\doteqdot"),("unicode-math","\\Doteq")], category = Rel, comments = "/doteq r: equals, even dots"}
  , Record {uchar = '\8786', commands = [("amssymb","\\fallingdotseq"),("unicode-math","\\fallingdotseq")], category = Rel, comments = "equals, falling dots"}
  , Record {uchar = '\8787', commands = [("amssymb","\\risingdotseq"),("unicode-math","\\risingdotseq")], category = Rel, comments = "equals, rising dots"}
  , Record {uchar = '\8788', commands = [("mathabx","\\coloneq"),("txfonts","\\coloneqq"),("unicode-math","\\coloneq")], category = Rel, comments = "= \\SetDelayed (wrisym), # := colon, equals"}
  , Record {uchar = '\8789', commands = [("mathabx","\\eqcolon"),("txfonts","\\eqqcolon"),("unicode-math","\\eqcolon")], category = Rel, comments = "# =:, equals, colon"}
  , Record {uchar = '\8790', commands = [("amssymb","\\eqcirc"),("unicode-math","\\eqcirc")], category = Rel, comments = "circle on equals sign"}
  , Record {uchar = '\8791', commands = [("amssymb","\\circeq"),("unicode-math","\\circeq")], category = Rel, comments = "circle, equals"}
  , Record {uchar = '\8792', commands = [("unicode-math","\\arceq")], category = Rel, comments = "arc, equals; CORRESPONDS TO"}
  , Record {uchar = '\8793', commands = [("mathabx","\\corresponds"),("oz","\\sdef"),("unicode-math","\\wedgeq")], category = Rel, comments = "t \\Corresponds (marvosym), corresponds to (wedge over equals)"}
  , Record {uchar = '\8794', commands = [("unicode-math","\\veeeq")], category = Rel, comments = "logical or, equals"}
  , Record {uchar = '\8795', commands = [("unicode-math","\\stareq")], category = Rel, comments = "STAR EQUALS"}
  , Record {uchar = '\8796', commands = [("amssymb","\\triangleq"),("oz","\\varsdef"),("unicode-math","\\triangleq")], category = Rel, comments = "triangle, equals"}
  , Record {uchar = '\8797', commands = [("unicode-math","\\eqdef")], category = Rel, comments = "equals by definition"}
  , Record {uchar = '\8798', commands = [("unicode-math","\\measeq")], category = Rel, comments = "MEASURED BY (m over equals)"}
  , Record {uchar = '\8799', commands = [("unicode-math","\\questeq")], category = Rel, comments = "equal with questionmark"}
  , Record {uchar = '\8800', commands = [("base","\\neq"),("base","\\ne"),("unicode-math","\\ne")], category = Rel, comments = "r: not equal"}
  , Record {uchar = '\8801', commands = [("base","\\equiv"),("unicode-math","\\equiv")], category = Rel, comments = "identical with"}
  , Record {uchar = '\8802', commands = [("wrisym","\\nequiv"),("unicode-math","\\nequiv")], category = Rel, comments = "not identical with"}
  , Record {uchar = '\8803', commands = [("unicode-math","\\Equiv")], category = Rel, comments = "strict equivalence (4 lines)"}
  , Record {uchar = '\8804', commands = [("base","\\leq"),("base","\\le"),("unicode-math","\\leq")], category = Rel, comments = "r: less-than-or-equal"}
  , Record {uchar = '\8805', commands = [("base","\\geq"),("base","\\ge"),("unicode-math","\\geq")], category = Rel, comments = "r: greater-than-or-equal"}
  , Record {uchar = '\8806', commands = [("amssymb","\\leqq"),("unicode-math","\\leqq")], category = Rel, comments = "less, double equals"}
  , Record {uchar = '\8807', commands = [("amssymb","\\geqq"),("unicode-math","\\geqq")], category = Rel, comments = "greater, double equals"}
  , Record {uchar = '\8808', commands = [("amssymb","\\lneqq"),("unicode-math","\\lneqq")], category = Rel, comments = "less, not double equals"}
  , Record {uchar = '\8809', commands = [("amssymb","\\gneqq"),("unicode-math","\\gneqq")], category = Rel, comments = "greater, not double equals"}
  , Record {uchar = '\8810', commands = [("base","\\ll"),("unicode-math","\\ll")], category = Rel, comments = "much less than, type 2"}
  , Record {uchar = '\8811', commands = [("base","\\gg"),("unicode-math","\\gg")], category = Rel, comments = "much greater than, type 2"}
  , Record {uchar = '\8812', commands = [("amssymb","\\between"),("unicode-math","\\between")], category = Rel, comments = "BETWEEN"}
  , Record {uchar = '\8813', commands = [("mathabx","\\notasymp"),("wrisym","\\nasymp"),("unicode-math","\\nasymp")], category = Rel, comments = "not asymptotically equal to"}
  , Record {uchar = '\8814', commands = [("amssymb","\\nless"),("unicode-math","\\nless")], category = Rel, comments = "NOT LESS-THAN"}
  , Record {uchar = '\8815', commands = [("amssymb","\\ngtr"),("unicode-math","\\ngtr")], category = Rel, comments = "NOT GREATER-THAN"}
  , Record {uchar = '\8816', commands = [("amssymb","\\nleq"),("wrisym","\\nleq"),("fourier","\\nleqslant"),("unicode-math","\\nleq")], category = Rel, comments = "not less-than-or-equal"}
  , Record {uchar = '\8817', commands = [("amssymb","\\ngeq"),("wrisym","\\ngeq"),("fourier","\\ngeqslant"),("unicode-math","\\ngeq")], category = Rel, comments = "not greater-than-or-equal"}
  , Record {uchar = '\8818', commands = [("amssymb","\\lesssim"),("wasysym","\\apprle"),("unicode-math","\\lesssim")], category = Rel, comments = "= \\LessTilde (wrisym), less, similar"}
  , Record {uchar = '\8819', commands = [("amssymb","\\gtrsim"),("wasysym","\\apprge"),("unicode-math","\\gtrsim")], category = Rel, comments = "= \\GreaterTilde (wrisym), greater, similar"}
  , Record {uchar = '\8820', commands = [("wrisym","\\NotLessTilde"),("unicode-math","\\nlesssim")], category = Rel, comments = "not less, similar"}
  , Record {uchar = '\8821', commands = [("wrisym","\\NotGreaterTilde"),("unicode-math","\\ngtrsim")], category = Rel, comments = "not greater, similar"}
  , Record {uchar = '\8822', commands = [("amssymb","\\lessgtr"),("unicode-math","\\lessgtr")], category = Rel, comments = "less, greater"}
  , Record {uchar = '\8823', commands = [("amssymb","\\gtrless"),("wrisym","\\GreaterLess"),("unicode-math","\\gtrless")], category = Rel, comments = "greater, less"}
  , Record {uchar = '\8824', commands = [("unicode-math","\\nlessgtr")], category = Rel, comments = "not less, greater"}
  , Record {uchar = '\8825', commands = [("wrisym","\\NotGreaterLess"),("unicode-math","\\ngtrless")], category = Rel, comments = "not greater, less"}
  , Record {uchar = '\8826', commands = [("base","\\prec"),("unicode-math","\\prec")], category = Rel, comments = "PRECEDES"}
  , Record {uchar = '\8827', commands = [("base","\\succ"),("unicode-math","\\succ")], category = Rel, comments = "SUCCEEDS"}
  , Record {uchar = '\8828', commands = [("amssymb","\\preccurlyeq"),("wrisym","\\PrecedesSlantEqual"),("unicode-math","\\preccurlyeq")], category = Rel, comments = "precedes, curly equals"}
  , Record {uchar = '\8829', commands = [("amssymb","\\succcurlyeq"),("wrisym","\\SucceedsSlantEqual"),("unicode-math","\\succcurlyeq")], category = Rel, comments = "succeeds, curly equals"}
  , Record {uchar = '\8830', commands = [("amssymb","\\precsim"),("wrisym","\\PrecedesTilde"),("unicode-math","\\precsim")], category = Rel, comments = "precedes, similar"}
  , Record {uchar = '\8831', commands = [("amssymb","\\succsim"),("wrisym","\\SucceedsTilde"),("unicode-math","\\succsim")], category = Rel, comments = "succeeds, similar"}
  , Record {uchar = '\8832', commands = [("amssymb","\\nprec"),("wrisym","\\nprec"),("unicode-math","\\nprec")], category = Rel, comments = "not precedes"}
  , Record {uchar = '\8833', commands = [("amssymb","\\nsucc"),("wrisym","\\nsucc"),("unicode-math","\\nsucc")], category = Rel, comments = "not succeeds"}
  , Record {uchar = '\8834', commands = [("base","\\subset"),("unicode-math","\\subset")], category = Rel, comments = "subset or is implied by"}
  , Record {uchar = '\8835', commands = [("base","\\supset"),("unicode-math","\\supset")], category = Rel, comments = "superset or implies"}
  , Record {uchar = '\8836', commands = [("wrisym","\\nsubset"),("unicode-math","\\nsubset")], category = Rel, comments = "not subset, variant [slash negation]"}
  , Record {uchar = '\8837', commands = [("wrisym","\\nsupset"),("unicode-math","\\nsupset")], category = Rel, comments = "not superset, variant [slash negation]"}
  , Record {uchar = '\8838', commands = [("base","\\subseteq"),("unicode-math","\\subseteq")], category = Rel, comments = "subset, equals"}
  , Record {uchar = '\8839', commands = [("base","\\supseteq"),("unicode-math","\\supseteq")], category = Rel, comments = "superset, equals"}
  , Record {uchar = '\8840', commands = [("amssymb","\\nsubseteq"),("wrisym","\\nsubseteq"),("unicode-math","\\nsubseteq")], category = Rel, comments = "not subset, equals"}
  , Record {uchar = '\8841', commands = [("amssymb","\\nsupseteq"),("wrisym","\\nsupseteq"),("unicode-math","\\nsupseteq")], category = Rel, comments = "not superset, equals"}
  , Record {uchar = '\8842', commands = [("amssymb","\\subsetneq"),("fourier","\\varsubsetneq"),("unicode-math","\\subsetneq")], category = Rel, comments = "subset, not equals"}
  , Record {uchar = '\8843', commands = [("amssymb","\\supsetneq"),("unicode-math","\\supsetneq")], category = Rel, comments = "superset, not equals"}
  , Record {uchar = '\8844', commands = [("unicode-math","\\cupleftarrow")], category = Bin, comments = "MULTISET"}
  , Record {uchar = '\8845', commands = [("unicode-math","\\cupdot")], category = Bin, comments = "union, with dot"}
  , Record {uchar = '\8846', commands = [("base","\\uplus"),("oz","\\buni"),("unicode-math","\\uplus")], category = Bin, comments = "plus sign in union"}
  , Record {uchar = '\8847', commands = [("amsfonts","\\sqsubset"),("unicode-math","\\sqsubset")], category = Rel, comments = "square subset"}
  , Record {uchar = '\8848', commands = [("amsfonts","\\sqsupset"),("unicode-math","\\sqsupset")], category = Rel, comments = "square superset"}
  , Record {uchar = '\8849', commands = [("base","\\sqsubseteq"),("unicode-math","\\sqsubseteq")], category = Rel, comments = "square subset, equals"}
  , Record {uchar = '\8850', commands = [("base","\\sqsupseteq"),("unicode-math","\\sqsupseteq")], category = Rel, comments = "square superset, equals"}
  , Record {uchar = '\8851', commands = [("base","\\sqcap"),("unicode-math","\\sqcap")], category = Bin, comments = "square intersection"}
  , Record {uchar = '\8852', commands = [("base","\\sqcup"),("unicode-math","\\sqcup")], category = Bin, comments = "square union"}
  , Record {uchar = '\8853', commands = [("base","\\oplus"),("unicode-math","\\oplus")], category = Bin, comments = "plus sign in circle"}
  , Record {uchar = '\8854', commands = [("base","\\ominus"),("unicode-math","\\ominus")], category = Bin, comments = "minus sign in circle"}
  , Record {uchar = '\8855', commands = [("base","\\otimes"),("unicode-math","\\otimes")], category = Bin, comments = "multiply sign in circle"}
  , Record {uchar = '\8856', commands = [("base","\\oslash"),("unicode-math","\\oslash")], category = Bin, comments = "solidus in circle"}
  , Record {uchar = '\8857', commands = [("base","\\odot"),("unicode-math","\\odot")], category = Bin, comments = "middle dot in circle"}
  , Record {uchar = '\8858', commands = [("amssymb","\\circledcirc"),("unicode-math","\\circledcirc")], category = Bin, comments = "small circle in circle"}
  , Record {uchar = '\8859', commands = [("amssymb","\\circledast"),("unicode-math","\\circledast")], category = Bin, comments = "asterisk in circle"}
  , Record {uchar = '\8860', commands = [("unicode-math","\\circledequal")], category = Bin, comments = "equal in circle"}
  , Record {uchar = '\8861', commands = [("amssymb","\\circleddash"),("unicode-math","\\circleddash")], category = Bin, comments = "hyphen in circle"}
  , Record {uchar = '\8862', commands = [("amssymb","\\boxplus"),("unicode-math","\\boxplus")], category = Bin, comments = "plus sign in box"}
  , Record {uchar = '\8863', commands = [("amssymb","\\boxminus"),("unicode-math","\\boxminus")], category = Bin, comments = "minus sign in box"}
  , Record {uchar = '\8864', commands = [("amssymb","\\boxtimes"),("unicode-math","\\boxtimes")], category = Bin, comments = "multiply sign in box"}
  , Record {uchar = '\8865', commands = [("amssymb","\\boxdot"),("stmaryrd","\\boxdot"),("unicode-math","\\boxdot")], category = Bin, comments = "/dotsquare /boxdot b: small dot in box"}
  , Record {uchar = '\8866', commands = [("base","\\vdash"),("unicode-math","\\vdash")], category = Rel, comments = "RIGHT TACK, proves, implies, yields, (vertical, dash)"}
  , Record {uchar = '\8867', commands = [("amssymb","\\dashv"),("unicode-math","\\dashv")], category = Rel, comments = "LEFT TACK, non-theorem, does not yield, (dash, vertical)"}
  , Record {uchar = '\8868', commands = [("base","\\top"),("unicode-math","\\top")], category = Ord, comments = "DOWN TACK, top"}
  , Record {uchar = '\8869', commands = [("base","\\bot"),("unicode-math","\\bot")], category = Ord, comments = "UP TACK, bottom"}
  , Record {uchar = '\8870', commands = [("base","\\vdash"),("unicode-math","\\assert")], category = Rel, comments = "ASSERTION (vertical, short dash)"}
  , Record {uchar = '\8871', commands = [("base","\\models"),("unicode-math","\\models")], category = Rel, comments = "MODELS (vertical, short double dash)"}
  , Record {uchar = '\8872', commands = [("amssymb","\\vDash"),("fourier","\\vDash"),("unicode-math","\\vDash")], category = Rel, comments = "TRUE (vertical, double dash)"}
  , Record {uchar = '\8873', commands = [("amssymb","\\Vdash"),("unicode-math","\\Vdash")], category = Rel, comments = "double vertical, dash"}
  , Record {uchar = '\8874', commands = [("amssymb","\\Vvdash"),("unicode-math","\\Vvdash")], category = Rel, comments = "triple vertical, dash"}
  , Record {uchar = '\8875', commands = [("mathabx","\\VDash"),("txfonts","\\VDash"),("unicode-math","\\VDash")], category = Rel, comments = "double vert, double dash"}
  , Record {uchar = '\8876', commands = [("amssymb","\\nvdash"),("unicode-math","\\nvdash")], category = Rel, comments = "not vertical, dash"}
  , Record {uchar = '\8877', commands = [("amssymb","\\nvDash"),("fourier","\\nvDash"),("unicode-math","\\nvDash")], category = Rel, comments = "not vertical, double dash"}
  , Record {uchar = '\8878', commands = [("amssymb","\\nVdash"),("unicode-math","\\nVdash")], category = Rel, comments = "not double vertical, dash"}
  , Record {uchar = '\8879', commands = [("amssymb","\\nVDash"),("unicode-math","\\nVDash")], category = Rel, comments = "not double vert, double dash"}
  , Record {uchar = '\8880', commands = [("unicode-math","\\prurel")], category = Rel, comments = "element PRECEDES UNDER RELATION"}
  , Record {uchar = '\8881', commands = [("unicode-math","\\scurel")], category = Rel, comments = "SUCCEEDS UNDER RELATION"}
  , Record {uchar = '\8882', commands = [("amssymb","\\vartriangleleft"),("unicode-math","\\vartriangleleft")], category = Rel, comments = "left triangle, open, variant"}
  , Record {uchar = '\8883', commands = [("amssymb","\\vartriangleright"),("unicode-math","\\vartriangleright")], category = Rel, comments = "right triangle, open, variant"}
  , Record {uchar = '\8884', commands = [("amssymb","\\trianglelefteq"),("wrisym","\\unlhd"),("unicode-math","\\trianglelefteq")], category = Rel, comments = "left triangle, equals"}
  , Record {uchar = '\8885', commands = [("amssymb","\\trianglerighteq"),("wrisym","\\unrhd"),("unicode-math","\\trianglerighteq")], category = Rel, comments = "right triangle, equals"}
  , Record {uchar = '\8886', commands = [("txfonts","\\multimapdotbothA"),("unicode-math","\\origof")], category = Rel, comments = "ORIGINAL OF"}
  , Record {uchar = '\8887', commands = [("txfonts","\\multimapdotbothB"),("unicode-math","\\imageof")], category = Rel, comments = "IMAGE OF"}
  , Record {uchar = '\8888', commands = [("amssymb","\\multimap"),("unicode-math","\\multimap")], category = Rel, comments = "/MULTIMAP a:"}
  , Record {uchar = '\8889', commands = [("unicode-math","\\hermitmatrix")], category = Ord, comments = "HERMITIAN CONJUGATE MATRIX"}
  , Record {uchar = '\8890', commands = [("amssymb","\\intercal"),("fourier","\\intercal"),("unicode-math","\\intercal")], category = Bin, comments = "intercal"}
  , Record {uchar = '\8891', commands = [("amssymb","\\veebar"),("unicode-math","\\veebar")], category = Bin, comments = "logical or, bar below (large vee); exclusive disjunction"}
  , Record {uchar = '\8892', commands = [("amssymb","\\barwedge"),("unicode-math","\\barwedge")], category = Bin, comments = "logical NAND (bar over wedge)"}
  , Record {uchar = '\8893', commands = [("unicode-math","\\barvee")], category = Bin, comments = "bar, vee (large vee)"}
  , Record {uchar = '\8894', commands = [("unicode-math","\\measuredrightangle")], category = Ord, comments = "right angle-measured [with arc]"}
  , Record {uchar = '\8895', commands = [("unicode-math","\\varlrtriangle")], category = Ord, comments = "RIGHT TRIANGLE"}
  , Record {uchar = '\8896', commands = [("base","\\bigwedge"),("unicode-math","\\bigwedge")], category = Op, comments = "logical or operator"}
  , Record {uchar = '\8897', commands = [("base","\\bigvee"),("unicode-math","\\bigvee")], category = Op, comments = "logical and operator"}
  , Record {uchar = '\8898', commands = [("base","\\bigcap"),("oz","\\dint"),("unicode-math","\\bigcap")], category = Op, comments = "\\dinter (oz), intersection operator"}
  , Record {uchar = '\8899', commands = [("base","\\bigcup"),("oz","\\duni"),("unicode-math","\\bigcup")], category = Op, comments = "\\dunion (oz), union operator"}
  , Record {uchar = '\8900', commands = [("base","\\diamond"),("unicode-math","\\smwhtdiamond")], category = Bin, comments = "DIAMOND OPERATOR (white diamond)"}
  , Record {uchar = '\8901', commands = [("base","\\cdot"),("unicode-math","\\cdot")], category = Bin, comments = "DOT OPERATOR (small middle dot)"}
  , Record {uchar = '\8902', commands = [("base","\\star"),("unicode-math","\\star")], category = Bin, comments = "small star, filled, low"}
  , Record {uchar = '\8903', commands = [("amssymb","\\divideontimes"),("unicode-math","\\divideontimes")], category = Bin, comments = "division on times"}
  , Record {uchar = '\8904', commands = [("base","\\bowtie"),("txfonts","\\lrtimes"),("unicode-math","\\bowtie")], category = Rel, comments = "BOWTIE"}
  , Record {uchar = '\8905', commands = [("amssymb","\\ltimes"),("unicode-math","\\ltimes")], category = Bin, comments = "times sign, left closed"}
  , Record {uchar = '\8906', commands = [("amssymb","\\rtimes"),("unicode-math","\\rtimes")], category = Bin, comments = "times sign, right closed"}
  , Record {uchar = '\8907', commands = [("amssymb","\\leftthreetimes"),("unicode-math","\\leftthreetimes")], category = Bin, comments = "LEFT SEMIDIRECT PRODUCT"}
  , Record {uchar = '\8908', commands = [("amssymb","\\rightthreetimes"),("unicode-math","\\rightthreetimes")], category = Bin, comments = "RIGHT SEMIDIRECT PRODUCT"}
  , Record {uchar = '\8909', commands = [("amssymb","\\backsimeq"),("unicode-math","\\backsimeq")], category = Rel, comments = "reverse similar, equals"}
  , Record {uchar = '\8910', commands = [("amssymb","\\curlyvee"),("unicode-math","\\curlyvee")], category = Bin, comments = "CURLY LOGICAL OR"}
  , Record {uchar = '\8911', commands = [("amssymb","\\curlywedge"),("unicode-math","\\curlywedge")], category = Bin, comments = "CURLY LOGICAL AND"}
  , Record {uchar = '\8912', commands = [("amssymb","\\Subset"),("unicode-math","\\Subset")], category = Rel, comments = "DOUBLE SUBSET"}
  , Record {uchar = '\8913', commands = [("amssymb","\\Supset"),("unicode-math","\\Supset")], category = Rel, comments = "DOUBLE SUPERSET"}
  , Record {uchar = '\8914', commands = [("amssymb","\\Cap"),("unicode-math","\\Cap")], category = Bin, comments = "/cap /doublecap b: DOUBLE INTERSECTION"}
  , Record {uchar = '\8915', commands = [("amssymb","\\Cup"),("unicode-math","\\Cup")], category = Bin, comments = "/cup /doublecup b: DOUBLE UNION"}
  , Record {uchar = '\8916', commands = [("amssymb","\\pitchfork"),("unicode-math","\\pitchfork")], category = Rel, comments = "PITCHFORK"}
  , Record {uchar = '\8917', commands = [("mathabx","\\hash"),("unicode-math","\\equalparallel")], category = Rel, comments = "parallel, equal; equal or parallel"}
  , Record {uchar = '\8918', commands = [("amssymb","\\lessdot"),("unicode-math","\\lessdot")], category = Rel, comments = "less than, with dot"}
  , Record {uchar = '\8919', commands = [("amssymb","\\gtrdot"),("unicode-math","\\gtrdot")], category = Rel, comments = "greater than, with dot"}
  , Record {uchar = '\8920', commands = [("amssymb","\\lll"),("unicode-math","\\lll")], category = Rel, comments = "triple less-than"}
  , Record {uchar = '\8921', commands = [("amssymb","\\ggg"),("unicode-math","\\ggg")], category = Rel, comments = "triple greater-than"}
  , Record {uchar = '\8922', commands = [("amssymb","\\lesseqgtr"),("unicode-math","\\lesseqgtr")], category = Rel, comments = "less, equals, greater"}
  , Record {uchar = '\8923', commands = [("amssymb","\\gtreqless"),("unicode-math","\\gtreqless")], category = Rel, comments = "greater, equals, less"}
  , Record {uchar = '\8924', commands = [("unicode-math","\\eqless")], category = Rel, comments = "equal-or-less"}
  , Record {uchar = '\8925', commands = [("unicode-math","\\eqgtr")], category = Rel, comments = "equal-or-greater"}
  , Record {uchar = '\8926', commands = [("amssymb","\\curlyeqprec"),("unicode-math","\\curlyeqprec")], category = Rel, comments = "curly equals, precedes"}
  , Record {uchar = '\8927', commands = [("amssymb","\\curlyeqsucc"),("unicode-math","\\curlyeqsucc")], category = Rel, comments = "curly equals, succeeds"}
  , Record {uchar = '\8928', commands = [("amssymb","\\npreceq"),("wrisym","\\npreceq"),("unicode-math","\\npreccurlyeq")], category = Rel, comments = "DOES NOT PRECEDE OR EQUAL"}
  , Record {uchar = '\8929', commands = [("amssymb","\\nsucceq"),("wrisym","\\nsucceq"),("unicode-math","\\nsucccurlyeq")], category = Rel, comments = "not succeeds, curly equals"}
  , Record {uchar = '\8930', commands = [("wrisym","\\nsqsubseteq"),("unicode-math","\\nsqsubseteq")], category = Rel, comments = "not, square subset, equals"}
  , Record {uchar = '\8931', commands = [("wrisym","\\nsqsupseteq"),("unicode-math","\\nsqsupseteq")], category = Rel, comments = "not, square superset, equals"}
  , Record {uchar = '\8932', commands = [("unicode-math","\\sqsubsetneq")], category = Rel, comments = "square subset, not equals"}
  , Record {uchar = '\8933', commands = [("unicode-math","\\sqsupsetneq")], category = Rel, comments = "square superset, not equals"}
  , Record {uchar = '\8934', commands = [("amssymb","\\lnsim"),("unicode-math","\\lnsim")], category = Rel, comments = "less, not similar"}
  , Record {uchar = '\8935', commands = [("amssymb","\\gnsim"),("unicode-math","\\gnsim")], category = Rel, comments = "greater, not similar"}
  , Record {uchar = '\8936', commands = [("amssymb","\\precnsim"),("unicode-math","\\precnsim")], category = Rel, comments = "precedes, not similar"}
  , Record {uchar = '\8937', commands = [("amssymb","\\succnsim"),("unicode-math","\\succnsim")], category = Rel, comments = "succeeds, not similar"}
  , Record {uchar = '\8938', commands = [("amssymb","\\ntriangleleft"),("wrisym","\\NotLeftTriangle"),("unicode-math","\\ntriangleleft")], category = Rel, comments = "not left triangle"}
  , Record {uchar = '\8939', commands = [("amssymb","\\ntriangleright"),("wrisym","\\NotRightTriangle"),("unicode-math","\\ntriangleright")], category = Rel, comments = "not right triangle"}
  , Record {uchar = '\8940', commands = [("amssymb","\\ntrianglelefteq"),("wrisym","\\nunlhd"),("unicode-math","\\ntrianglelefteq")], category = Rel, comments = "not left triangle, equals"}
  , Record {uchar = '\8941', commands = [("amssymb","\\ntrianglerighteq"),("wrisym","\\nunrhd"),("unicode-math","\\ntrianglerighteq")], category = Rel, comments = "not right triangle, equals"}
  , Record {uchar = '\8942', commands = [("base","\\vdots"),("unicode-math","\\vdots")], category = Rel, comments = "VERTICAL ELLIPSIS"}
  , Record {uchar = '\8943', commands = [("base","\\cdots"),("unicode-math","\\unicodecdots")], category = Ord, comments = "three dots, centered"}
  , Record {uchar = '\8944', commands = [("mathdots","\\iddots"),("yhmath","\\adots"),("unicode-math","\\adots")], category = Rel, comments = "three dots, ascending"}
  , Record {uchar = '\8945', commands = [("base","\\ddots"),("unicode-math","\\ddots")], category = Rel, comments = "three dots, descending"}
  , Record {uchar = '\8946', commands = [("unicode-math","\\disin")], category = Rel, comments = "ELEMENT OF WITH LONG HORIZONTAL STROKE"}
  , Record {uchar = '\8947', commands = [("unicode-math","\\varisins")], category = Rel, comments = "ELEMENT OF WITH VERTICAL BAR AT END OF HORIZONTAL STROKE"}
  , Record {uchar = '\8948', commands = [("unicode-math","\\isins")], category = Rel, comments = "SMALL ELEMENT OF WITH VERTICAL BAR AT END OF HORIZONTAL STROKE"}
  , Record {uchar = '\8949', commands = [("unicode-math","\\isindot")], category = Rel, comments = "ELEMENT OF WITH DOT ABOVE"}
  , Record {uchar = '\8950', commands = [("mathabx","\\barin"),("unicode-math","\\varisinobar")], category = Rel, comments = "ELEMENT OF WITH OVERBAR"}
  , Record {uchar = '\8951', commands = [("unicode-math","\\isinobar")], category = Rel, comments = "SMALL ELEMENT OF WITH OVERBAR"}
  , Record {uchar = '\8952', commands = [("unicode-math","\\isinvb")], category = Rel, comments = "ELEMENT OF WITH UNDERBAR"}
  , Record {uchar = '\8953', commands = [("unicode-math","\\isinE")], category = Rel, comments = "ELEMENT OF WITH TWO HORIZONTAL STROKES"}
  , Record {uchar = '\8954', commands = [("unicode-math","\\nisd")], category = Rel, comments = "CONTAINS WITH LONG HORIZONTAL STROKE"}
  , Record {uchar = '\8955', commands = [("unicode-math","\\varnis")], category = Rel, comments = "CONTAINS WITH VERTICAL BAR AT END OF HORIZONTAL STROKE"}
  , Record {uchar = '\8956', commands = [("unicode-math","\\nis")], category = Rel, comments = "SMALL CONTAINS WITH VERTICAL BAR AT END OF HORIZONTAL STROKE"}
  , Record {uchar = '\8957', commands = [("unicode-math","\\varniobar")], category = Rel, comments = "CONTAINS WITH OVERBAR"}
  , Record {uchar = '\8958', commands = [("unicode-math","\\niobar")], category = Rel, comments = "SMALL CONTAINS WITH OVERBAR"}
  , Record {uchar = '\8959', commands = [("base","\\mathsf{E}"),("unicode-math","\\bagmember")], category = Rel, comments = "Z NOTATION BAG MEMBERSHIP"}
  , Record {uchar = '\8960', commands = [("mathabx","\\diameter"),("amssymb","\\varnothing"),("unicode-math","\\diameter")], category = Ord, comments = "DIAMETER SIGN"}
  , Record {uchar = '\8962', commands = [("unicode-math","\\house")], category = Ord, comments = "HOUSE"}
  , Record {uchar = '\8965', commands = [("amssymb","\\barwedge"),("unicode-math","\\varbarwedge")], category = Bin, comments = "PROJECTIVE (bar over small wedge) not nand"}
  , Record {uchar = '\8966', commands = [("amssymb","\\doublebarwedge"),("unicode-math","\\vardoublebarwedge")], category = Bin, comments = "PERSPECTIVE (double bar over small wedge)"}
  , Record {uchar = '\8968', commands = [("base","\\lceil"),("unicode-math","\\lceil")], category = Open, comments = "LEFT CEILING"}
  , Record {uchar = '\8969', commands = [("base","\\rceil"),("unicode-math","\\rceil")], category = Close, comments = "RIGHT CEILING"}
  , Record {uchar = '\8970', commands = [("base","\\lfloor"),("unicode-math","\\lfloor")], category = Open, comments = "LEFT FLOOR"}
  , Record {uchar = '\8971', commands = [("base","\\rfloor"),("unicode-math","\\rfloor")], category = Close, comments = "RIGHT FLOOR"}
  , Record {uchar = '\8976', commands = [("wasysym","\\invneg"),("unicode-math","\\invnot")], category = Ord, comments = "reverse not"}
  , Record {uchar = '\8977', commands = [("wasysym","\\wasylozenge"),("unicode-math","\\sqlozenge")], category = Ord, comments = "SQUARE LOZENGE"}
  , Record {uchar = '\8978', commands = [("unicode-math","\\profline")], category = Ord, comments = "profile of a line"}
  , Record {uchar = '\8979', commands = [("unicode-math","\\profsurf")], category = Ord, comments = "profile of a surface"}
  , Record {uchar = '\8983', commands = [("unicode-math","\\viewdata")], category = Ord, comments = "VIEWDATA SQUARE"}
  , Record {uchar = '\8985', commands = [("unicode-math","\\turnednot")], category = Ord, comments = "TURNED NOT SIGN"}
  , Record {uchar = '\8988', commands = [("amsfonts","\\ulcorner"),("unicode-math","\\ulcorner")], category = Open, comments = "upper left corner"}
  , Record {uchar = '\8989', commands = [("amsfonts","\\urcorner"),("unicode-math","\\urcorner")], category = Close, comments = "upper right corner"}
  , Record {uchar = '\8990', commands = [("amsfonts","\\llcorner"),("unicode-math","\\llcorner")], category = Open, comments = "lower left corner"}
  , Record {uchar = '\8991', commands = [("amsfonts","\\lrcorner"),("unicode-math","\\lrcorner")], category = Close, comments = "lower right corner"}
  , Record {uchar = '\8992', commands = [("unicode-math","\\inttop")], category = Ord, comments = "TOP HALF INTEGRAL"}
  , Record {uchar = '\8993', commands = [("unicode-math","\\intbottom")], category = Ord, comments = "BOTTOM HALF INTEGRAL"}
  , Record {uchar = '\8994', commands = [("base","\\frown"),("base","\\smallfrown"),("unicode-math","\\frown")], category = Rel, comments = "FROWN (down curve)"}
  , Record {uchar = '\8995', commands = [("base","\\smile"),("base","\\smallsmile"),("unicode-math","\\smile")], category = Rel, comments = "SMILE (up curve)"}
  , Record {uchar = '\9001', commands = [("base","\\langle"),("unicode","\\langle")], category = Open, comments = "Left angle bracket"}
  , Record {uchar = '\9002', commands = [("base","\\rangle"),("unicode","\\rangle")], category = Close, comments = "Right angle bracket"}
  , Record {uchar = '\9004', commands = [("unicode-math","\\varhexagonlrbonds")], category = Ord, comments = "six carbon ring, corner down, double bonds lower right etc"}
  , Record {uchar = '\9010', commands = [("unicode-math","\\conictaper")], category = Ord, comments = "CONICAL TAPER"}
  , Record {uchar = '\9014', commands = [("unicode-math","\\topbot")], category = Ord, comments = "APL FUNCTIONAL SYMBOL I-BEAM, top and bottom"}
  , Record {uchar = '\9015', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL SQUISH QUAD"}
  , Record {uchar = '\9016', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD EQUAL"}
  , Record {uchar = '\9017', commands = [("wasysym","\\APLinv")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD DIVIDE"}
  , Record {uchar = '\9018', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD DIAMOND"}
  , Record {uchar = '\9019', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD JOT"}
  , Record {uchar = '\9020', commands = [("wasysym","\\APLcirc{\\APLbox}")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD CIRCLE"}
  , Record {uchar = '\9021', commands = [("wasysym","\\APLvert{\\Circle}"),("unicode-math","\\obar")], category = Bin, comments = "x \\obar (stmaryrd), APL FUNCTIONAL SYMBOL CIRCLE STILE, circle with vertical bar"}
  , Record {uchar = '\9022', commands = [("wasysym","\\APLcirc{\\Circle}")], category = Ord, comments = "APL FUNCTIONAL SYMBOL CIRCLE JOT"}
  , Record {uchar = '\9023', commands = [("wasysym","\\notslash"),("unicode-math","\\APLnotslash")], category = Rel, comments = "APL FUNCTIONAL SYMBOL SLASH BAR, solidus, bar through"}
  , Record {uchar = '\9024', commands = [("wasysym","\\notbackslash"),("unicode-math","\\APLnotbackslash")], category = Ord, comments = "APL FUNCTIONAL SYMBOL BACKSLASH BAR"}
  , Record {uchar = '\9025', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD SLASH"}
  , Record {uchar = '\9026', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD BACKSLASH"}
  , Record {uchar = '\9027', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD LESS-THAN"}
  , Record {uchar = '\9028', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD GREATER-THAN"}
  , Record {uchar = '\9029', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL LEFTWARDS VANE"}
  , Record {uchar = '\9030', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL RIGHTWARDS VANE"}
  , Record {uchar = '\9031', commands = [("wasysym","\\APLleftarrowbox")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD LEFTWARDS ARROW"}
  , Record {uchar = '\9032', commands = [("wasysym","\\APLrightarrowbox")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD RIGHTWARDS ARROW"}
  , Record {uchar = '\9033', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL CIRCLE BACKSLASH"}
  , Record {uchar = '\9034', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL DOWN TACK UNDERBAR"}
  , Record {uchar = '\9035', commands = [("wasysym","\\APLvert{\\APLup}")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DELTA STILE"}
  , Record {uchar = '\9036', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD DOWN CARET"}
  , Record {uchar = '\9037', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD DELTA"}
  , Record {uchar = '\9038', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL DOWN TACK JOT"}
  , Record {uchar = '\9039', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL UPWARDS VANE"}
  , Record {uchar = '\9040', commands = [("wasysym","\\APLuparrowbox")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD UPWARDS ARROW"}
  , Record {uchar = '\9041', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL UP TACK OVERBAR"}
  , Record {uchar = '\9042', commands = [("wasysym","\\APLvert{\\APLdown}")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DEL STILE"}
  , Record {uchar = '\9043', commands = [("unicode-math","\\APLboxupcaret")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD UP CARET"}
  , Record {uchar = '\9044', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD DEL"}
  , Record {uchar = '\9045', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL UP TACK JOT"}
  , Record {uchar = '\9046', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL DOWNWARDS VANE"}
  , Record {uchar = '\9047', commands = [("wasysym","\\APLdownarrowbox")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD DOWNWARDS ARROW"}
  , Record {uchar = '\9048', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUOTE UNDERBAR"}
  , Record {uchar = '\9049', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL DELTA UNDERBAR"}
  , Record {uchar = '\9050', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL DIAMOND UNDERBAR"}
  , Record {uchar = '\9051', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL JOT UNDERBAR"}
  , Record {uchar = '\9052', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL CIRCLE UNDERBAR"}
  , Record {uchar = '\9053', commands = [("wasysym","\\APLcomment")], category = Ord, comments = "APL FUNCTIONAL SYMBOL UP SHOE JOT"}
  , Record {uchar = '\9054', commands = [("wasysym","\\APLinput")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUOTE QUAD"}
  , Record {uchar = '\9055', commands = [("wasysym","\\APLlog")], category = Ord, comments = "APL FUNCTIONAL SYMBOL CIRCLE STAR"}
  , Record {uchar = '\9056', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD COLON"}
  , Record {uchar = '\9057', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL UP TACK DIAERESIS"}
  , Record {uchar = '\9058', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL DEL DIAERESIS"}
  , Record {uchar = '\9059', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL STAR DIAERESIS"}
  , Record {uchar = '\9060', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL JOT DIAERESIS"}
  , Record {uchar = '\9061', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL CIRCLE DIAERESIS"}
  , Record {uchar = '\9062', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL DOWN SHOE STILE"}
  , Record {uchar = '\9063', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL LEFT SHOE STILE"}
  , Record {uchar = '\9064', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL TILDE DIAERESIS"}
  , Record {uchar = '\9065', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL GREATER-THAN DIAERESIS"}
  , Record {uchar = '\9066', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL COMMA BAR"}
  , Record {uchar = '\9067', commands = [("wasysym","\\APLnot{\\APLdown}")], category = Ord, comments = "APL FUNCTIONAL SYMBOL DEL TILDE"}
  , Record {uchar = '\9068', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL ZILDE"}
  , Record {uchar = '\9069', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL STILE TILDE"}
  , Record {uchar = '\9070', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL SEMICOLON UNDERBAR"}
  , Record {uchar = '\9071', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD NOT EQUAL"}
  , Record {uchar = '\9072', commands = [("unicode-math","\\APLboxquestion")], category = Ord, comments = "APL FUNCTIONAL SYMBOL QUAD QUESTION"}
  , Record {uchar = '\9073', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL DOWN CARET TILDE"}
  , Record {uchar = '\9074', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL UP CARET TILDE"}
  , Record {uchar = '\9075', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL IOTA"}
  , Record {uchar = '\9076', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL RHO"}
  , Record {uchar = '\9077', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL OMEGA"}
  , Record {uchar = '\9078', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL ALPHA UNDERBAR"}
  , Record {uchar = '\9079', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL EPSILON UNDERBAR"}
  , Record {uchar = '\9080', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL IOTA UNDERBAR"}
  , Record {uchar = '\9081', commands = [], category = Ord, comments = "APL FUNCTIONAL SYMBOL OMEGA UNDERBAR"}
  , Record {uchar = '\9084', commands = [("unicode-math","\\rangledownzigzagarrow")], category = Ord, comments = "RIGHT ANGLE WITH DOWNWARDS ZIGZAG ARROW"}
  , Record {uchar = '\9108', commands = [("unicode-math","\\hexagon")], category = Ord, comments = "horizontal benzene ring [hexagon flat open]"}
  , Record {uchar = '\9115', commands = [("unicode-math","\\lparenuend")], category = Ord, comments = "LEFT PARENTHESIS UPPER HOOK"}
  , Record {uchar = '\9116', commands = [("unicode-math","\\lparenextender")], category = Ord, comments = "LEFT PARENTHESIS EXTENSION"}
  , Record {uchar = '\9117', commands = [("unicode-math","\\lparenlend")], category = Ord, comments = "LEFT PARENTHESIS LOWER HOOK"}
  , Record {uchar = '\9118', commands = [("unicode-math","\\rparenuend")], category = Ord, comments = "RIGHT PARENTHESIS UPPER HOOK"}
  , Record {uchar = '\9119', commands = [("unicode-math","\\rparenextender")], category = Ord, comments = "RIGHT PARENTHESIS EXTENSION"}
  , Record {uchar = '\9120', commands = [("unicode-math","\\rparenlend")], category = Ord, comments = "RIGHT PARENTHESIS LOWER HOOK"}
  , Record {uchar = '\9121', commands = [("unicode-math","\\lbrackuend")], category = Ord, comments = "LEFT SQUARE BRACKET UPPER CORNER"}
  , Record {uchar = '\9122', commands = [("unicode-math","\\lbrackextender")], category = Ord, comments = "LEFT SQUARE BRACKET EXTENSION"}
  , Record {uchar = '\9123', commands = [("unicode-math","\\lbracklend")], category = Ord, comments = "LEFT SQUARE BRACKET LOWER CORNER"}
  , Record {uchar = '\9124', commands = [("unicode-math","\\rbrackuend")], category = Ord, comments = "RIGHT SQUARE BRACKET UPPER CORNER"}
  , Record {uchar = '\9125', commands = [("unicode-math","\\rbrackextender")], category = Ord, comments = "RIGHT SQUARE BRACKET EXTENSION"}
  , Record {uchar = '\9126', commands = [("unicode-math","\\rbracklend")], category = Ord, comments = "RIGHT SQUARE BRACKET LOWER CORNER"}
  , Record {uchar = '\9127', commands = [("unicode-math","\\lbraceuend")], category = Ord, comments = "LEFT CURLY BRACKET UPPER HOOK"}
  , Record {uchar = '\9128', commands = [("unicode-math","\\lbracemid")], category = Ord, comments = "LEFT CURLY BRACKET MIDDLE PIECE"}
  , Record {uchar = '\9129', commands = [("unicode-math","\\lbracelend")], category = Ord, comments = "LEFT CURLY BRACKET LOWER HOOK"}
  , Record {uchar = '\9130', commands = [("unicode-math","\\vbraceextender")], category = Ord, comments = "CURLY BRACKET EXTENSION"}
  , Record {uchar = '\9131', commands = [("unicode-math","\\rbraceuend")], category = Ord, comments = "RIGHT CURLY BRACKET UPPER HOOK"}
  , Record {uchar = '\9132', commands = [("unicode-math","\\rbracemid")], category = Ord, comments = "RIGHT CURLY BRACKET MIDDLE PIECE"}
  , Record {uchar = '\9133', commands = [("unicode-math","\\rbracelend")], category = Ord, comments = "RIGHT CURLY BRACKET LOWER HOOK"}
  , Record {uchar = '\9134', commands = [("unicode-math","\\intextender")], category = Ord, comments = "INTEGRAL EXTENSION"}
  , Record {uchar = '\9135', commands = [("unicode-math","\\harrowextender")], category = Ord, comments = "HORIZONTAL LINE EXTENSION (used to extend arrows)"}
  , Record {uchar = '\9136', commands = [("unicode-math","\\lmoustache")], category = Ord, comments = "? \\lmoustache, UPPER LEFT OR LOWER RIGHT CURLY BRACKET SECTION"}
  , Record {uchar = '\9137', commands = [("unicode-math","\\rmoustache")], category = Ord, comments = "? \\rmoustache, UPPER RIGHT OR LOWER LEFT CURLY BRACKET SECTION"}
  , Record {uchar = '\9138', commands = [("unicode-math","\\sumtop")], category = Ord, comments = "SUMMATION TOP"}
  , Record {uchar = '\9139', commands = [("unicode-math","\\sumbottom")], category = Ord, comments = "SUMMATION BOTTOM"}
  , Record {uchar = '\9140', commands = [("unicode-math","\\overbracket")], category = TOver, comments = "TOP SQUARE BRACKET"}
  , Record {uchar = '\9141', commands = [("unicode-math","\\underbracket")], category = TUnder, comments = "BOTTOM SQUARE BRACKET"}
  , Record {uchar = '\9142', commands = [("unicode-math","\\bbrktbrk")], category = Ord, comments = "BOTTOM SQUARE BRACKET OVER TOP SQUARE BRACKET"}
  , Record {uchar = '\9143', commands = [("unicode-math","\\sqrtbottom")], category = Ord, comments = "RADICAL SYMBOL BOTTOM"}
  , Record {uchar = '\9144', commands = [("unicode-math","\\lvboxline")], category = Ord, comments = "LEFT VERTICAL BOX LINE"}
  , Record {uchar = '\9145', commands = [("unicode-math","\\rvboxline")], category = Ord, comments = "RIGHT VERTICAL BOX LINE"}
  , Record {uchar = '\9166', commands = [("unicode-math","\\varcarriagereturn")], category = Ord, comments = "RETURN SYMBOL"}
  , Record {uchar = '\9168', commands = [], category = Ord, comments = "VERTICAL LINE EXTENSION (VERTICAL LINE EXTENSION)"}
  , Record {uchar = '\9180', commands = [("wrisym","\\overparen"),("yhmath mathabx fourier","\\wideparen"),("unicode-math","\\overparen")], category = TOver, comments = "TOP PARENTHESIS (mathematical use)"}
  , Record {uchar = '\9181', commands = [("wrisym","\\underparen"),("unicode-math","\\underparen")], category = TUnder, comments = "BOTTOM PARENTHESIS (mathematical use)"}
  , Record {uchar = '\9182', commands = [("base","\\overbrace"),("unicode-math","\\overbrace")], category = TOver, comments = "TOP CURLY BRACKET (mathematical use)"}
  , Record {uchar = '\9183', commands = [("base","\\underbrace"),("unicode-math","\\underbrace")], category = TUnder, comments = "BOTTOM CURLY BRACKET (mathematical use)"}
  , Record {uchar = '\9184', commands = [("unicode-math","\\obrbrak")], category = Ord, comments = "TOP TORTOISE SHELL BRACKET (mathematical use)"}
  , Record {uchar = '\9185', commands = [("unicode-math","\\ubrbrak")], category = Ord, comments = "BOTTOM TORTOISE SHELL BRACKET (mathematical use)"}
  , Record {uchar = '\9186', commands = [("unicode-math","\\trapezium")], category = Ord, comments = "WHITE TRAPEZIUM"}
  , Record {uchar = '\9187', commands = [("unicode-math","\\benzenr")], category = Ord, comments = "BENZENE RING WITH CIRCLE"}
  , Record {uchar = '\9188', commands = [("unicode-math","\\strns")], category = Ord, comments = "STRAIGHTNESS"}
  , Record {uchar = '\9189', commands = [("unicode-math","\\fltns")], category = Ord, comments = "FLATNESS"}
  , Record {uchar = '\9190', commands = [("wasysym","\\AC"),("unicode-math","\\accurrent")], category = Ord, comments = "AC CURRENT"}
  , Record {uchar = '\9191', commands = [("unicode-math","\\elinters")], category = Ord, comments = "ELECTRICAL INTERSECTION"}
  , Record {uchar = '\9416', commands = [], category = Ord, comments = "oS capital S in circle"}
  , Record {uchar = '\9478', commands = [("unicode-math","\\bdtriplevdash")], category = Ord, comments = "doubly broken vert"}
  , Record {uchar = '\9600', commands = [("unicode-math","\\blockuphalf")], category = Ord, comments = "UPPER HALF BLOCK"}
  , Record {uchar = '\9604', commands = [("unicode-math","\\blocklowhalf")], category = Ord, comments = "LOWER HALF BLOCK"}
  , Record {uchar = '\9608', commands = [("unicode-math","\\blockfull")], category = Ord, comments = "FULL BLOCK"}
  , Record {uchar = '\9612', commands = [("unicode-math","\\blocklefthalf")], category = Ord, comments = "LEFT HALF BLOCK"}
  , Record {uchar = '\9616', commands = [("unicode-math","\\blockrighthalf")], category = Ord, comments = "RIGHT HALF BLOCK"}
  , Record {uchar = '\9617', commands = [("unicode-math","\\blockqtrshaded")], category = Ord, comments = "25\\% shaded block"}
  , Record {uchar = '\9618', commands = [("unicode-math","\\blockhalfshaded")], category = Ord, comments = "50\\% shaded block"}
  , Record {uchar = '\9619', commands = [("unicode-math","\\blockthreeqtrshaded")], category = Ord, comments = "75\\% shaded block"}
  , Record {uchar = '\9632', commands = [("base","\\blacksquare"),("unicode-math","\\mdlgblksquare")], category = Ord, comments = "square, filled"}
  , Record {uchar = '\9633', commands = [("base","\\square"),("unicode-math","\\mdlgwhtsquare")], category = Ord, comments = "square, open"}
  , Record {uchar = '\9634', commands = [("unicode-math","\\squoval")], category = Ord, comments = "WHITE SQUARE WITH ROUNDED CORNERS"}
  , Record {uchar = '\9635', commands = [("unicode-math","\\blackinwhitesquare")], category = Ord, comments = "WHITE SQUARE CONTAINING BLACK SMALL SQUARE"}
  , Record {uchar = '\9636', commands = [("unicode-math","\\squarehfill")], category = Ord, comments = "square, horizontal rule filled"}
  , Record {uchar = '\9637', commands = [("unicode-math","\\squarevfill")], category = Ord, comments = "square, vertical rule filled"}
  , Record {uchar = '\9638', commands = [("unicode-math","\\squarehvfill")], category = Ord, comments = "SQUARE WITH ORTHOGONAL CROSSHATCH FILL"}
  , Record {uchar = '\9639', commands = [("unicode-math","\\squarenwsefill")], category = Ord, comments = "square, nw-to-se rule filled"}
  , Record {uchar = '\9640', commands = [("unicode-math","\\squareneswfill")], category = Ord, comments = "square, ne-to-sw rule filled"}
  , Record {uchar = '\9641', commands = [("unicode-math","\\squarecrossfill")], category = Ord, comments = "SQUARE WITH DIAGONAL CROSSHATCH FILL"}
  , Record {uchar = '\9642', commands = [("unicode-math","\\smblksquare")], category = Ord, comments = "sq bullet, filled"}
  , Record {uchar = '\9643', commands = [("unicode-math","\\smwhtsquare")], category = Ord, comments = "WHITE SMALL SQUARE"}
  , Record {uchar = '\9644', commands = [("unicode-math","\\hrectangleblack")], category = Ord, comments = "BLACK RECTANGLE"}
  , Record {uchar = '\9645', commands = [("unicode-math","\\hrectangle")], category = Ord, comments = "horizontal rectangle, open"}
  , Record {uchar = '\9646', commands = [("unicode-math","\\vrectangleblack")], category = Ord, comments = "BLACK VERTICAL RECTANGLE"}
  , Record {uchar = '\9647', commands = [("unicode-math","\\vrectangle")], category = Ord, comments = "rectangle, white (vertical)"}
  , Record {uchar = '\9648', commands = [("unicode-math","\\parallelogramblack")], category = Ord, comments = "BLACK PARALLELOGRAM"}
  , Record {uchar = '\9649', commands = [("unicode-math","\\parallelogram")], category = Ord, comments = "parallelogram, open"}
  , Record {uchar = '\9650', commands = [("unicode-math","\\bigblacktriangleup")], category = Ord, comments = "BLACK UP-POINTING TRIANGLE"}
  , Record {uchar = '\9651', commands = [("base","\\bigtriangleup"),("amsfonts","\\triangle"),("unicode-math","\\bigtriangleup")], category = Bin, comments = "# \\vartriangle (amssymb), big up triangle, open"}
  , Record {uchar = '\9652', commands = [("mathabx","\\blacktriangleup"),("unicode-math","\\blacktriangle")], category = Bin, comments = "up triangle, filled"}
  , Record {uchar = '\9653', commands = [("mathabx","\\smalltriangleup"),("amssymb","\\vartriangle"),("unicode-math","\\vartriangle")], category = Bin, comments = "small up triangle, open"}
  , Record {uchar = '\9654', commands = [("wasysym","\\RHD"),("fourier -mathabx","\\blacktriangleright"),("unicode-math","\\blacktriangleright")], category = Bin, comments = "(large) right triangle, filled"}
  , Record {uchar = '\9655', commands = [("amssymb","\\rhd"),("wasysym","\\rhd"),("oz","\\rres"),("unicode-math","\\triangleright")], category = Bin, comments = "= \\RightTriangle (wrisym), (large) right triangle, open; z notation range restriction"}
  , Record {uchar = '\9656', commands = [("mathabx","\\blacktriangleright"),("unicode-math","\\smallblacktriangleright")], category = Bin, comments = "right triangle, filled"}
  , Record {uchar = '\9657', commands = [("mathabx","\\smalltriangleright"),("base","\\triangleright"),("unicode-math","\\smalltriangleright")], category = Bin, comments = "x \\triangleright (mathabx), right triangle, open"}
  , Record {uchar = '\9658', commands = [("unicode-math","\\blackpointerright")], category = Ord, comments = "BLACK RIGHT-POINTING POINTER"}
  , Record {uchar = '\9659', commands = [("mathabx","\\triangleright"),("unicode-math","\\whitepointerright")], category = Ord, comments = "WHITE RIGHT-POINTING POINTER"}
  , Record {uchar = '\9660', commands = [("unicode-math","\\bigblacktriangledown")], category = Ord, comments = "big down triangle, filled"}
  , Record {uchar = '\9661', commands = [("base","\\bigtriangledown"),("unicode-math","\\bigtriangledown")], category = Bin, comments = "big down triangle, open"}
  , Record {uchar = '\9662', commands = [("mathabx","\\blacktriangledown"),("unicode-math","\\blacktriangledown")], category = Bin, comments = "BLACK DOWN-POINTING SMALL TRIANGLE"}
  , Record {uchar = '\9663', commands = [("mathabx","\\smalltriangledown"),("amssymb","\\triangledown"),("unicode-math","\\triangledown")], category = Bin, comments = "WHITE DOWN-POINTING SMALL TRIANGLE"}
  , Record {uchar = '\9664', commands = [("wasysym","\\LHD"),("fourier -mathabx","\\blacktriangleleft"),("unicode-math","\\blacktriangleleft")], category = Bin, comments = "(large) left triangle, filled"}
  , Record {uchar = '\9665', commands = [("amssymb","\\lhd"),("wasysym","\\lhd"),("oz","\\dres"),("unicode-math","\\triangleleft")], category = Bin, comments = "= \\LeftTriangle (wrisym), (large) left triangle, open; z notation domain restriction"}
  , Record {uchar = '\9666', commands = [("mathabx","\\blacktriangleleft"),("unicode-math","\\smallblacktriangleleft")], category = Bin, comments = "left triangle, filled"}
  , Record {uchar = '\9667', commands = [("mathabx","\\smalltriangleleft"),("base","\\triangleleft"),("unicode-math","\\smalltriangleleft")], category = Bin, comments = "x \\triangleleft (mathabx), left triangle, open"}
  , Record {uchar = '\9668', commands = [("unicode-math","\\blackpointerleft")], category = Ord, comments = "BLACK LEFT-POINTING POINTER"}
  , Record {uchar = '\9669', commands = [("mathabx","\\triangleleft"),("unicode-math","\\whitepointerleft")], category = Ord, comments = "WHITE LEFT-POINTING POINTER"}
  , Record {uchar = '\9670', commands = [("txfonts","\\Diamondblack"),("unicode-math","\\mdlgblkdiamond")], category = Ord, comments = "BLACK DIAMOND"}
  , Record {uchar = '\9671', commands = [("amssymb","\\Diamond"),("unicode-math","\\mdlgwhtdiamond")], category = Ord, comments = "WHITE DIAMOND; diamond, open"}
  , Record {uchar = '\9672', commands = [("unicode-math","\\blackinwhitediamond")], category = Ord, comments = "WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND"}
  , Record {uchar = '\9673', commands = [("unicode-math","\\fisheye")], category = Ord, comments = "FISHEYE"}
  , Record {uchar = '\9674', commands = [("amssymb","\\lozenge"),("unicode-math","\\mdlgwhtlozenge")], category = Ord, comments = "LOZENGE or total mark"}
  , Record {uchar = '\9675', commands = [("wasysym","\\Circle"),("unicode-math","\\mdlgwhtcircle")], category = Bin, comments = "medium large circle"}
  , Record {uchar = '\9676', commands = [("unicode-math","\\dottedcircle")], category = Ord, comments = "DOTTED CIRCLE"}
  , Record {uchar = '\9677', commands = [("unicode-math","\\circlevertfill")], category = Ord, comments = "CIRCLE WITH VERTICAL FILL"}
  , Record {uchar = '\9678', commands = [("amssymb","\\circledcirc"),("unicode-math","\\bullseye")], category = Ord, comments = "BULLSEYE"}
  , Record {uchar = '\9679', commands = [("wasysym","\\CIRCLE"),("unicode-math","\\mdlgblkcircle")], category = Ord, comments = "circle, filled"}
  , Record {uchar = '\9680', commands = [("wasysym","\\LEFTcircle"),("unicode-math","\\circlelefthalfblack")], category = Ord, comments = "circle, filled left half [harvey ball]"}
  , Record {uchar = '\9681', commands = [("wasysym","\\RIGHTcircle"),("unicode-math","\\circlerighthalfblack")], category = Ord, comments = "circle, filled right half"}
  , Record {uchar = '\9682', commands = [("unicode-math","\\circlebottomhalfblack")], category = Ord, comments = "circle, filled bottom half"}
  , Record {uchar = '\9683', commands = [("unicode-math","\\circletophalfblack")], category = Ord, comments = "circle, filled top half"}
  , Record {uchar = '\9684', commands = [("unicode-math","\\circleurquadblack")], category = Ord, comments = "CIRCLE WITH UPPER RIGHT QUADRANT BLACK"}
  , Record {uchar = '\9685', commands = [("unicode-math","\\blackcircleulquadwhite")], category = Ord, comments = "CIRCLE WITH ALL BUT UPPER LEFT QUADRANT BLACK"}
  , Record {uchar = '\9686', commands = [("wasysym","\\LEFTCIRCLE"),("unicode-math","\\blacklefthalfcircle")], category = Ord, comments = "LEFT HALF BLACK CIRCLE"}
  , Record {uchar = '\9687', commands = [("wasysym","\\RIGHTCIRCLE"),("unicode-math","\\blackrighthalfcircle")], category = Ord, comments = "RIGHT HALF BLACK CIRCLE"}
  , Record {uchar = '\9688', commands = [("unicode-math","\\inversebullet")], category = Ord, comments = "INVERSE BULLET"}
  , Record {uchar = '\9689', commands = [("unicode-math","\\inversewhitecircle")], category = Ord, comments = "INVERSE WHITE CIRCLE"}
  , Record {uchar = '\9690', commands = [("unicode-math","\\invwhiteupperhalfcircle")], category = Ord, comments = "UPPER HALF INVERSE WHITE CIRCLE"}
  , Record {uchar = '\9691', commands = [("unicode-math","\\invwhitelowerhalfcircle")], category = Ord, comments = "LOWER HALF INVERSE WHITE CIRCLE"}
  , Record {uchar = '\9692', commands = [("unicode-math","\\ularc")], category = Ord, comments = "UPPER LEFT QUADRANT CIRCULAR ARC"}
  , Record {uchar = '\9693', commands = [("unicode-math","\\urarc")], category = Ord, comments = "UPPER RIGHT QUADRANT CIRCULAR ARC"}
  , Record {uchar = '\9694', commands = [("unicode-math","\\lrarc")], category = Ord, comments = "LOWER RIGHT QUADRANT CIRCULAR ARC"}
  , Record {uchar = '\9695', commands = [("unicode-math","\\llarc")], category = Ord, comments = "LOWER LEFT QUADRANT CIRCULAR ARC"}
  , Record {uchar = '\9696', commands = [("unicode-math","\\topsemicircle")], category = Ord, comments = "UPPER HALF CIRCLE"}
  , Record {uchar = '\9697', commands = [("unicode-math","\\botsemicircle")], category = Ord, comments = "LOWER HALF CIRCLE"}
  , Record {uchar = '\9698', commands = [("unicode-math","\\lrblacktriangle")], category = Ord, comments = "lower right triangle, filled"}
  , Record {uchar = '\9699', commands = [("unicode-math","\\llblacktriangle")], category = Ord, comments = "lower left triangle, filled"}
  , Record {uchar = '\9700', commands = [("unicode-math","\\ulblacktriangle")], category = Ord, comments = "upper left triangle, filled"}
  , Record {uchar = '\9701', commands = [("unicode-math","\\urblacktriangle")], category = Ord, comments = "upper right triangle, filled"}
  , Record {uchar = '\9702', commands = [("unicode-math","\\smwhtcircle")], category = Ord, comments = "WHITE BULLET"}
  , Record {uchar = '\9703', commands = [("unicode-math","\\squareleftblack")], category = Ord, comments = "square, filled left half"}
  , Record {uchar = '\9704', commands = [("unicode-math","\\squarerightblack")], category = Ord, comments = "square, filled right half"}
  , Record {uchar = '\9705', commands = [("unicode-math","\\squareulblack")], category = Ord, comments = "square, filled top left corner"}
  , Record {uchar = '\9706', commands = [("unicode-math","\\squarelrblack")], category = Ord, comments = "square, filled bottom right corner"}
  , Record {uchar = '\9707', commands = [("stmaryrd","\\boxbar"),("txfonts","\\boxbar"),("unicode-math","\\boxbar")], category = Bin, comments = "vertical bar in box"}
  , Record {uchar = '\9708', commands = [("unicode-math","\\trianglecdot")], category = Ord, comments = "triangle with centered dot"}
  , Record {uchar = '\9709', commands = [("unicode-math","\\triangleleftblack")], category = Ord, comments = "UP-POINTING TRIANGLE WITH LEFT HALF BLACK"}
  , Record {uchar = '\9710', commands = [("unicode-math","\\trianglerightblack")], category = Ord, comments = "UP-POINTING TRIANGLE WITH RIGHT HALF BLACK"}
  , Record {uchar = '\9711', commands = [("unicode-math","\\lgwhtcircle")], category = Ord, comments = "LARGE CIRCLE"}
  , Record {uchar = '\9712', commands = [("unicode-math","\\squareulquad")], category = Ord, comments = "WHITE SQUARE WITH UPPER LEFT QUADRANT"}
  , Record {uchar = '\9713', commands = [("unicode-math","\\squarellquad")], category = Ord, comments = "WHITE SQUARE WITH LOWER LEFT QUADRANT"}
  , Record {uchar = '\9714', commands = [("unicode-math","\\squarelrquad")], category = Ord, comments = "WHITE SQUARE WITH LOWER RIGHT QUADRANT"}
  , Record {uchar = '\9715', commands = [("unicode-math","\\squareurquad")], category = Ord, comments = "WHITE SQUARE WITH UPPER RIGHT QUADRANT"}
  , Record {uchar = '\9716', commands = [("unicode-math","\\circleulquad")], category = Ord, comments = "WHITE CIRCLE WITH UPPER LEFT QUADRANT"}
  , Record {uchar = '\9717', commands = [("unicode-math","\\circlellquad")], category = Ord, comments = "WHITE CIRCLE WITH LOWER LEFT QUADRANT"}
  , Record {uchar = '\9718', commands = [("unicode-math","\\circlelrquad")], category = Ord, comments = "WHITE CIRCLE WITH LOWER RIGHT QUADRANT"}
  , Record {uchar = '\9719', commands = [("unicode-math","\\circleurquad")], category = Ord, comments = "WHITE CIRCLE WITH UPPER RIGHT QUADRANT"}
  , Record {uchar = '\9720', commands = [("unicode-math","\\ultriangle")], category = Ord, comments = "UPPER LEFT TRIANGLE"}
  , Record {uchar = '\9721', commands = [("unicode-math","\\urtriangle")], category = Ord, comments = "UPPER RIGHT TRIANGLE"}
  , Record {uchar = '\9722', commands = [("unicode-math","\\lltriangle")], category = Ord, comments = "LOWER LEFT TRIANGLE"}
  , Record {uchar = '\9723', commands = [("amssymb","\\square"),("unicode-math","\\mdwhtsquare")], category = Ord, comments = "WHITE MEDIUM SQUARE"}
  , Record {uchar = '\9724', commands = [("amssymb","\\blacksquare"),("unicode-math","\\mdblksquare")], category = Ord, comments = "BLACK MEDIUM SQUARE"}
  , Record {uchar = '\9725', commands = [("unicode-math","\\mdsmwhtsquare")], category = Ord, comments = "WHITE MEDIUM SMALL SQUARE"}
  , Record {uchar = '\9726', commands = [("unicode-math","\\mdsmblksquare")], category = Ord, comments = "BLACK MEDIUM SMALL SQUARE"}
  , Record {uchar = '\9727', commands = [("unicode-math","\\lrtriangle")], category = Ord, comments = "LOWER RIGHT TRIANGLE"}
  , Record {uchar = '\9733', commands = [("amssymb","\\bigstar"),("unicode-math","\\bigstar")], category = Ord, comments = "star, filled"}
  , Record {uchar = '\9734', commands = [("unicode-math","\\bigwhitestar")], category = Ord, comments = "star, open"}
  , Record {uchar = '\9737', commands = [("mathabx","\\Sun"),("unicode-math","\\astrosun")], category = Ord, comments = "SUN"}
  , Record {uchar = '\9740', commands = [], category = Ord, comments = "text \\CONJUNCTION (wasysym), CONJUNCTION"}
  , Record {uchar = '\9744', commands = [("wasysym","\\Square")], category = Ord, comments = "BALLOT BOX"}
  , Record {uchar = '\9745', commands = [("wasysym","\\CheckedBox")], category = Ord, comments = "t \\Checkedbox (marvosym), BALLOT BOX WITH CHECK"}
  , Record {uchar = '\9746', commands = [("wasysym","\\XBox")], category = Ord, comments = "t \\Crossedbox (marvosym), BALLOT BOX WITH X"}
  , Record {uchar = '\9749', commands = [("arevmath","\\steaming")], category = Ord, comments = "HOT BEVERAGE"}
  , Record {uchar = '\9758', commands = [("arevmath","\\pointright")], category = Ord, comments = "WHITE RIGHT POINTING INDEX"}
  , Record {uchar = '\9760', commands = [("arevmath","\\skull")], category = Ord, comments = "SKULL AND CROSSBONES"}
  , Record {uchar = '\9761', commands = [("unicode-math","\\danger")], category = Ord, comments = "CAUTION SIGN, dangerous bend"}
  , Record {uchar = '\9762', commands = [("arevmath","\\radiation")], category = Ord, comments = "RADIOACTIVE SIGN"}
  , Record {uchar = '\9763', commands = [("arevmath","\\biohazard")], category = Ord, comments = "BIOHAZARD SIGN"}
  , Record {uchar = '\9775', commands = [("arevmath","\\yinyang")], category = Ord, comments = "YIN YANG"}
  , Record {uchar = '\9785', commands = [("wasysym","\\frownie"),("arevmath","\\sadface")], category = Ord, comments = "WHITE FROWNING FACE"}
  , Record {uchar = '\9786', commands = [("wasysym","\\smiley"),("arevmath","\\smileface")], category = Ord, comments = "WHITE SMILING FACE"}
  , Record {uchar = '\9787', commands = [("wasysym","\\blacksmiley"),("arevmath","\\invsmileface"),("unicode-math","\\blacksmiley")], category = Ord, comments = "BLACK SMILING FACE"}
  , Record {uchar = '\9788', commands = [("wasysym","\\sun"),("unicode-math","\\sun")], category = Ord, comments = "WHITE SUN WITH RAYS"}
  , Record {uchar = '\9789', commands = [("wasysym","\\rightmoon"),("mathabx","\\rightmoon"),("unicode-math","\\rightmoon")], category = Ord, comments = "FIRST QUARTER MOON"}
  , Record {uchar = '\9790', commands = [("wasysym","\\leftmoon"),("mathabx","\\leftmoon"),("unicode-math","\\leftmoon")], category = Ord, comments = "LAST QUARTER MOON"}
  , Record {uchar = '\9791', commands = [("wasysym","\\mercury"),("mathabx","\\Mercury")], category = Ord, comments = "MERCURY"}
  , Record {uchar = '\9792', commands = [("wasysym","\\female"),("mathabx","\\Venus"),("unicode-math","\\female")], category = Ord, comments = "= \\girl (mathabx), venus, female"}
  , Record {uchar = '\9793', commands = [("wasysym","\\earth"),("mathabx","\\varEarth")], category = Ord, comments = "EARTH"}
  , Record {uchar = '\9794', commands = [("wasysym","\\male"),("mathabx","\\Mars"),("unicode-math","\\male")], category = Ord, comments = "= \\boy (mathabx), mars, male"}
  , Record {uchar = '\9795', commands = [("wasysym","\\jupiter"),("mathabx","\\Jupiter")], category = Ord, comments = "JUPITER"}
  , Record {uchar = '\9796', commands = [("wasysym","\\saturn"),("mathabx","\\Saturn")], category = Ord, comments = "SATURN"}
  , Record {uchar = '\9797', commands = [("wasysym","\\uranus"),("mathabx","\\Uranus")], category = Ord, comments = "URANUS"}
  , Record {uchar = '\9798', commands = [("wasysym","\\neptune"),("mathabx","\\Neptune")], category = Ord, comments = "NEPTUNE"}
  , Record {uchar = '\9799', commands = [("wasysym","\\pluto"),("mathabx","\\Pluto")], category = Ord, comments = "PLUTO"}
  , Record {uchar = '\9800', commands = [("wasysym","\\aries"),("mathabx","\\Aries")], category = Ord, comments = "ARIES"}
  , Record {uchar = '\9801', commands = [("wasysym","\\taurus"),("mathabx","\\Taurus")], category = Ord, comments = "TAURUS"}
  , Record {uchar = '\9802', commands = [("wasysym","\\gemini"),("mathabx","\\Gemini")], category = Ord, comments = "GEMINI"}
  , Record {uchar = '\9803', commands = [("wasysym","\\cancer")], category = Ord, comments = "CANCER"}
  , Record {uchar = '\9804', commands = [("wasysym","\\leo"),("mathabx","\\Leo")], category = Ord, comments = "LEO"}
  , Record {uchar = '\9805', commands = [("wasysym","\\virgo")], category = Ord, comments = "VIRGO"}
  , Record {uchar = '\9806', commands = [("wasysym","\\libra"),("mathabx","\\Libra")], category = Ord, comments = "LIBRA"}
  , Record {uchar = '\9807', commands = [("wasysym","\\scorpio"),("mathabx","\\Scorpio")], category = Ord, comments = "SCORPIUS"}
  , Record {uchar = '\9808', commands = [("wasysym","\\sagittarius")], category = Ord, comments = "SAGITTARIUS"}
  , Record {uchar = '\9809', commands = [("wasysym","\\capricornus")], category = Ord, comments = "CAPRICORN"}
  , Record {uchar = '\9810', commands = [("wasysym","\\aquarius")], category = Ord, comments = "AQUARIUS"}
  , Record {uchar = '\9811', commands = [("wasysym","\\pisces")], category = Ord, comments = "PISCES"}
  , Record {uchar = '\9824', commands = [("base","\\spadesuit"),("unicode-math","\\spadesuit")], category = Ord, comments = "spades suit symbol"}
  , Record {uchar = '\9825', commands = [("base","\\heartsuit"),("unicode-math","\\heartsuit")], category = Ord, comments = "heart suit symbol"}
  , Record {uchar = '\9826', commands = [("base","\\diamondsuit"),("unicode-math","\\diamondsuit")], category = Ord, comments = "diamond suit symbol"}
  , Record {uchar = '\9827', commands = [("base","\\clubsuit"),("unicode-math","\\clubsuit")], category = Ord, comments = "club suit symbol"}
  , Record {uchar = '\9828', commands = [("txfonts","\\varspadesuit"),("arevmath","\\varspade"),("unicode-math","\\varspadesuit")], category = Ord, comments = "spade, white (card suit)"}
  , Record {uchar = '\9829', commands = [("txfonts","\\varheartsuit"),("arevmath","\\varheart"),("unicode-math","\\varheartsuit")], category = Ord, comments = "filled heart (card suit)"}
  , Record {uchar = '\9830', commands = [("txfonts","\\vardiamondsuit"),("arevmath","\\vardiamond"),("unicode-math","\\vardiamondsuit")], category = Ord, comments = "filled diamond (card suit)"}
  , Record {uchar = '\9831', commands = [("txfonts","\\varclubsuit"),("arevmath","\\varclub"),("unicode-math","\\varclubsuit")], category = Ord, comments = "club, white (card suit)"}
  , Record {uchar = '\9833', commands = [("arevmath","\\quarternote"),("wasysym","\\quarternote"),("unicode-math","\\quarternote")], category = Ord, comments = "music note (sung text sign)"}
  , Record {uchar = '\9834', commands = [("arevmath","\\eighthnote"),("unicode-math","\\eighthnote")], category = Ord, comments = "EIGHTH NOTE"}
  , Record {uchar = '\9835', commands = [("wasysym","\\twonotes"),("unicode-math","\\twonotes")], category = Ord, comments = "BEAMED EIGHTH NOTES"}
  , Record {uchar = '\9836', commands = [("arevmath","\\sixteenthnote")], category = Ord, comments = "BEAMED SIXTEENTH NOTES"}
  , Record {uchar = '\9837', commands = [("base","\\flat"),("unicode-math","\\flat")], category = Ord, comments = "musical flat"}
  , Record {uchar = '\9838', commands = [("base","\\natural"),("unicode-math","\\natural")], category = Ord, comments = "music natural"}
  , Record {uchar = '\9839', commands = [("base","\\sharp"),("oz","\\#"),("unicode-math","\\sharp")], category = Ord, comments = "MUSIC SHARP SIGN, z notation infix bag count"}
  , Record {uchar = '\9851', commands = [("arevmath","\\recycle")], category = Ord, comments = "BLACK UNIVERSAL RECYCLING SYMBOL"}
  , Record {uchar = '\9854', commands = [("unicode-math","\\acidfree")], category = Ord, comments = "PERMANENT PAPER SIGN"}
  , Record {uchar = '\9856', commands = [("unicode-math","\\dicei")], category = Ord, comments = "DIE FACE-1"}
  , Record {uchar = '\9857', commands = [("unicode-math","\\diceii")], category = Ord, comments = "DIE FACE-2"}
  , Record {uchar = '\9858', commands = [("unicode-math","\\diceiii")], category = Ord, comments = "DIE FACE-3"}
  , Record {uchar = '\9859', commands = [("unicode-math","\\diceiv")], category = Ord, comments = "DIE FACE-4"}
  , Record {uchar = '\9860', commands = [("unicode-math","\\dicev")], category = Ord, comments = "DIE FACE-5"}
  , Record {uchar = '\9861', commands = [("unicode-math","\\dicevi")], category = Ord, comments = "DIE FACE-6"}
  , Record {uchar = '\9862', commands = [("unicode-math","\\circledrightdot")], category = Ord, comments = "WHITE CIRCLE WITH DOT RIGHT"}
  , Record {uchar = '\9863', commands = [("unicode-math","\\circledtwodots")], category = Ord, comments = "WHITE CIRCLE WITH TWO DOTS"}
  , Record {uchar = '\9864', commands = [("unicode-math","\\blackcircledrightdot")], category = Ord, comments = "BLACK CIRCLE WITH WHITE DOT RIGHT"}
  , Record {uchar = '\9865', commands = [("unicode-math","\\blackcircledtwodots")], category = Ord, comments = "BLACK CIRCLE WITH TWO WHITE DOTS"}
  , Record {uchar = '\9875', commands = [("arevmath","\\anchor")], category = Ord, comments = "ANCHOR"}
  , Record {uchar = '\9876', commands = [("arevmath","\\swords")], category = Ord, comments = "CROSSED SWORDS"}
  , Record {uchar = '\9888', commands = [("arevmath","\\warning")], category = Ord, comments = "WARNING SIGN"}
  , Record {uchar = '\9893', commands = [("unicode-math","\\Hermaphrodite")], category = Ord, comments = "MALE AND FEMALE SIGN"}
  , Record {uchar = '\9898', commands = [("txfonts","\\medcirc"),("unicode-math","\\mdwhtcircle")], category = Ord, comments = "MEDIUM WHITE CIRCLE"}
  , Record {uchar = '\9899', commands = [("txfonts","\\medbullet"),("unicode-math","\\mdblkcircle")], category = Ord, comments = "MEDIUM BLACK CIRCLE"}
  , Record {uchar = '\9900', commands = [("unicode-math","\\mdsmwhtcircle")], category = Ord, comments = "MEDIUM SMALL WHITE CIRCLE"}
  , Record {uchar = '\9906', commands = [("unicode-math","\\neuter")], category = Ord, comments = "NEUTER"}
  , Record {uchar = '\9998', commands = [("arevmath","\\pencil")], category = Ord, comments = "LOWER RIGHT PENCIL"}
  , Record {uchar = '\10003', commands = [("amsfonts","\\checkmark"),("arevmath","\\ballotcheck"),("unicode-math","\\checkmark")], category = Ord, comments = "tick, CHECK MARK"}
  , Record {uchar = '\10007', commands = [("arevmath","\\ballotx")], category = Ord, comments = "BALLOT X"}
  , Record {uchar = '\10016', commands = [("amsfonts","\\maltese"),("unicode-math","\\maltese")], category = Ord, comments = "MALTESE CROSS"}
  , Record {uchar = '\10026', commands = [("unicode-math","\\circledstar")], category = Ord, comments = "CIRCLED WHITE STAR"}
  , Record {uchar = '\10038', commands = [("unicode-math","\\varstar")], category = Ord, comments = "SIX POINTED BLACK STAR"}
  , Record {uchar = '\10045', commands = [("unicode-math","\\dingasterisk")], category = Ord, comments = "HEAVY TEARDROP-SPOKED ASTERISK"}
  , Record {uchar = '\10098', commands = [("unicode-math","\\lbrbrak")], category = Open, comments = "LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT"}
  , Record {uchar = '\10099', commands = [("unicode-math","\\rbrbrak")], category = Close, comments = "LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT"}
  , Record {uchar = '\10139', commands = [("unicode-math","\\draftingarrow")], category = Ord, comments = "right arrow with bold head (drafting)"}
  , Record {uchar = '\10146', commands = [("arevmath","\\arrowbullet")], category = Ord, comments = "THREE-D TOP-LIGHTED RIGHTWARDS ARROWHEAD"}
  , Record {uchar = '\10176', commands = [("unicode-math","\\threedangle")], category = Ord, comments = "THREE DIMENSIONAL ANGLE"}
  , Record {uchar = '\10177', commands = [("unicode-math","\\whiteinwhitetriangle")], category = Ord, comments = "WHITE TRIANGLE CONTAINING SMALL WHITE TRIANGLE"}
  , Record {uchar = '\10178', commands = [("base","\\perp"),("unicode-math","\\perp")], category = Rel, comments = "PERPENDICULAR"}
  , Record {uchar = '\10179', commands = [("unicode-math","\\subsetcirc")], category = Ord, comments = "OPEN SUBSET"}
  , Record {uchar = '\10180', commands = [("unicode-math","\\supsetcirc")], category = Ord, comments = "OPEN SUPERSET"}
  , Record {uchar = '\10181', commands = [("stmaryrd","\\Lbag"),("txfonts","\\Lbag"),("stmaryrd -oz","\\lbag"),("unicode-math","\\lbag")], category = Open, comments = "LEFT S-SHAPED BAG DELIMITER"}
  , Record {uchar = '\10182', commands = [("stmaryrd","\\Rbag"),("txfonts","\\Rbag"),("stmaryrd -oz","\\rbag"),("unicode-math","\\rbag")], category = Close, comments = "RIGHT S-SHAPED BAG DELIMITER"}
  , Record {uchar = '\10183', commands = [("unicode-math","\\veedot")], category = Bin, comments = "OR WITH DOT INSIDE"}
  , Record {uchar = '\10184', commands = [("unicode-math","\\bsolhsub")], category = Rel, comments = "REVERSE SOLIDUS PRECEDING SUBSET"}
  , Record {uchar = '\10185', commands = [("unicode-math","\\suphsol")], category = Rel, comments = "SUPERSET PRECEDING SOLIDUS"}
  , Record {uchar = '\10188', commands = [("unicode-math","\\longdivision")], category = Open, comments = "LONG DIVISION"}
  , Record {uchar = '\10192', commands = [("txfonts","\\Diamonddot"),("unicode-math","\\diamondcdot")], category = Ord, comments = "WHITE DIAMOND WITH CENTRED DOT"}
  , Record {uchar = '\10193', commands = [("unicode-math","\\wedgedot")], category = Bin, comments = "AND WITH DOT"}
  , Record {uchar = '\10194', commands = [("unicode-math","\\upin")], category = Rel, comments = "ELEMENT OF OPENING UPWARDS"}
  , Record {uchar = '\10195', commands = [("unicode-math","\\pullback")], category = Rel, comments = "LOWER RIGHT CORNER WITH DOT"}
  , Record {uchar = '\10196', commands = [("unicode-math","\\pushout")], category = Rel, comments = "UPPER LEFT CORNER WITH DOT"}
  , Record {uchar = '\10197', commands = [("unicode-math","\\leftouterjoin")], category = Op, comments = "LEFT OUTER JOIN"}
  , Record {uchar = '\10198', commands = [("unicode-math","\\rightouterjoin")], category = Op, comments = "RIGHT OUTER JOIN"}
  , Record {uchar = '\10199', commands = [("unicode-math","\\fullouterjoin")], category = Op, comments = "FULL OUTER JOIN"}
  , Record {uchar = '\10200', commands = [("unicode-math","\\bigbot")], category = Op, comments = "LARGE UP TACK"}
  , Record {uchar = '\10201', commands = [("unicode-math","\\bigtop")], category = Op, comments = "LARGE DOWN TACK"}
  , Record {uchar = '\10202', commands = [("unicode-math","\\DashVDash")], category = Rel, comments = "LEFT AND RIGHT DOUBLE TURNSTILE"}
  , Record {uchar = '\10203', commands = [("unicode-math","\\dashVdash")], category = Rel, comments = "LEFT AND RIGHT TACK"}
  , Record {uchar = '\10204', commands = [("txfonts","\\multimapinv"),("unicode-math","\\multimapinv")], category = Rel, comments = "LEFT MULTIMAP"}
  , Record {uchar = '\10205', commands = [("unicode-math","\\vlongdash")], category = Rel, comments = "long left tack"}
  , Record {uchar = '\10206', commands = [("unicode-math","\\longdashv")], category = Rel, comments = "long right tack"}
  , Record {uchar = '\10207', commands = [("unicode-math","\\cirbot")], category = Rel, comments = "UP TACK WITH CIRCLE ABOVE"}
  , Record {uchar = '\10208', commands = [("unicode-math","\\lozengeminus")], category = Bin, comments = "LOZENGE DIVIDED BY HORIZONTAL RULE"}
  , Record {uchar = '\10209', commands = [("unicode-math","\\concavediamond")], category = Bin, comments = "WHITE CONCAVE-SIDED DIAMOND"}
  , Record {uchar = '\10210', commands = [("unicode-math","\\concavediamondtickleft")], category = Bin, comments = "WHITE CONCAVE-SIDED DIAMOND WITH LEFTWARDS TICK"}
  , Record {uchar = '\10211', commands = [("unicode-math","\\concavediamondtickright")], category = Bin, comments = "WHITE CONCAVE-SIDED DIAMOND WITH RIGHTWARDS TICK"}
  , Record {uchar = '\10212', commands = [("unicode-math","\\whitesquaretickleft")], category = Bin, comments = "WHITE SQUARE WITH LEFTWARDS TICK"}
  , Record {uchar = '\10213', commands = [("unicode-math","\\whitesquaretickright")], category = Bin, comments = "WHITE SQUARE WITH RIGHTWARDS TICK"}
  , Record {uchar = '\10214', commands = [("stmaryrd","\\llbracket"),("wrisym","\\llbracket"),("kpfonts","\\llbracket"),("fourier","\\llbracket"),("mathbbol","\\Lbrack"),("unicode-math","\\lBrack")], category = Open, comments = "= \\lbag (oz -stmaryrd), MATHEMATICAL LEFT WHITE SQUARE BRACKET"}
  , Record {uchar = '\10215', commands = [("stmaryrd","\\rrbracket"),("wrisym","\\rrbracket"),("kpfonts","\\rrbracket"),("fourier","\\rrbracket"),("mathbbol","\\Rbrack"),("unicode-math","\\rBrack")], category = Close, comments = "= \\rbag (oz -stmaryrd), MATHEMATICAL RIGHT WHITE SQUARE BRACKET"}
  , Record {uchar = '\10216', commands = [("base","\\langle"),("unicode-math","\\langle")], category = Open, comments = "MATHEMATICAL LEFT ANGLE BRACKET"}
  , Record {uchar = '\10217', commands = [("base","\\rangle"),("unicode-math","\\rangle")], category = Close, comments = "MATHEMATICAL RIGHT ANGLE BRACKET"}
  , Record {uchar = '\10218', commands = [("oz","\\lang"),("unicode-math","\\lAngle")], category = Open, comments = "MATHEMATICAL LEFT DOUBLE ANGLE BRACKET, z notation left chevron bracket"}
  , Record {uchar = '\10219', commands = [("oz","\\rang"),("unicode-math","\\rAngle")], category = Close, comments = "MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET, z notation right chevron bracket"}
  , Record {uchar = '\10220', commands = [("unicode-math","\\Lbrbrak")], category = Open, comments = "MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET"}
  , Record {uchar = '\10221', commands = [("unicode-math","\\Rbrbrak")], category = Close, comments = "MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET"}
  , Record {uchar = '\10222', commands = [("base","\\lgroup")], category = Open, comments = "MATHEMATICAL LEFT FLATTENED PARENTHESIS"}
  , Record {uchar = '\10223', commands = [("base","\\rgroup")], category = Close, comments = "MATHEMATICAL RIGHT FLATTENED PARENTHESIS"}
  , Record {uchar = '\10224', commands = [("unicode-math","\\UUparrow")], category = Rel, comments = "UPWARDS QUADRUPLE ARROW"}
  , Record {uchar = '\10225', commands = [("unicode-math","\\DDownarrow")], category = Rel, comments = "DOWNWARDS QUADRUPLE ARROW"}
  , Record {uchar = '\10226', commands = [("unicode-math","\\acwgapcirclearrow")], category = Rel, comments = "ANTICLOCKWISE GAPPED CIRCLE ARROW"}
  , Record {uchar = '\10227', commands = [("unicode-math","\\cwgapcirclearrow")], category = Rel, comments = "CLOCKWISE GAPPED CIRCLE ARROW"}
  , Record {uchar = '\10228', commands = [("unicode-math","\\rightarrowonoplus")], category = Rel, comments = "RIGHT ARROW WITH CIRCLED PLUS"}
  , Record {uchar = '\10229', commands = [("base","\\longleftarrow"),("unicode-math","\\longleftarrow")], category = Rel, comments = "LONG LEFTWARDS ARROW"}
  , Record {uchar = '\10230', commands = [("base","\\longrightarrow"),("unicode-math","\\longrightarrow")], category = Rel, comments = "LONG RIGHTWARDS ARROW"}
  , Record {uchar = '\10231', commands = [("base","\\longleftrightarrow"),("unicode-math","\\longleftrightarrow")], category = Rel, comments = "LONG LEFT RIGHT ARROW"}
  , Record {uchar = '\10232', commands = [("base","\\Longleftarrow"),("amsmath","\\impliedby"),("unicode-math","\\Longleftarrow")], category = Rel, comments = "LONG LEFTWARDS DOUBLE ARROW"}
  , Record {uchar = '\10233', commands = [("base","\\Longrightarrow"),("amsmath","\\implies"),("unicode-math","\\Longrightarrow")], category = Rel, comments = "LONG RIGHTWARDS DOUBLE ARROW"}
  , Record {uchar = '\10234', commands = [("base","\\Longleftrightarrow"),("oz","\\iff"),("unicode-math","\\Longleftrightarrow")], category = Rel, comments = "LONG LEFT RIGHT DOUBLE ARROW"}
  , Record {uchar = '\10235', commands = [("stmaryrd","\\longmapsfrom"),("kpfonts","\\longmappedfrom"),("unicode-math","\\longmapsfrom")], category = Rel, comments = "LONG LEFTWARDS ARROW FROM BAR"}
  , Record {uchar = '\10236', commands = [("base","\\longmapsto"),("unicode-math","\\longmapsto")], category = Rel, comments = "LONG RIGHTWARDS ARROW FROM BAR"}
  , Record {uchar = '\10237', commands = [("stmaryrd","\\Longmapsfrom"),("kpfonts","\\Longmappedfrom"),("unicode-math","\\Longmapsfrom")], category = Rel, comments = "LONG LEFTWARDS DOUBLE ARROW FROM BAR"}
  , Record {uchar = '\10238', commands = [("stmaryrd","\\Longmapsto"),("unicode-math","\\Longmapsto")], category = Rel, comments = "LONG RIGHTWARDS DOUBLE ARROW FROM BAR"}
  , Record {uchar = '\10239', commands = [("unicode-math","\\longrightsquigarrow")], category = Rel, comments = "LONG RIGHTWARDS SQUIGGLE ARROW"}
  , Record {uchar = '\10496', commands = [("oz","\\psur"),("oz","\\psurj"),("unicode-math","\\nvtwoheadrightarrow")], category = Rel, comments = "RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE, z notation partial surjection"}
  , Record {uchar = '\10497', commands = [("unicode-math","\\nVtwoheadrightarrow")], category = Rel, comments = "RIGHTWARDS TWO-HEADED ARROW WITH DOUBLE VERTICAL STROKE, z notation finite surjection"}
  , Record {uchar = '\10498', commands = [("unicode-math","\\nvLeftarrow")], category = Rel, comments = "LEFTWARDS DOUBLE ARROW WITH VERTICAL STROKE"}
  , Record {uchar = '\10499', commands = [("unicode-math","\\nvRightarrow")], category = Rel, comments = "RIGHTWARDS DOUBLE ARROW WITH VERTICAL STROKE"}
  , Record {uchar = '\10500', commands = [("unicode-math","\\nvLeftrightarrow")], category = Rel, comments = "LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE"}
  , Record {uchar = '\10501', commands = [("unicode-math","\\twoheadmapsto")], category = Rel, comments = "RIGHTWARDS TWO-HEADED ARROW FROM BAR"}
  , Record {uchar = '\10502', commands = [("stmaryrd","\\Mapsfrom"),("kpfonts","\\Mappedfrom"),("unicode-math","\\Mapsfrom")], category = Rel, comments = "LEFTWARDS DOUBLE ARROW FROM BAR"}
  , Record {uchar = '\10503', commands = [("stmaryrd","\\Mapsto"),("unicode-math","\\Mapsto")], category = Rel, comments = "RIGHTWARDS DOUBLE ARROW FROM BAR"}
  , Record {uchar = '\10504', commands = [("unicode-math","\\downarrowbarred")], category = Rel, comments = "DOWNWARDS ARROW WITH HORIZONTAL STROKE"}
  , Record {uchar = '\10505', commands = [("unicode-math","\\uparrowbarred")], category = Rel, comments = "UPWARDS ARROW WITH HORIZONTAL STROKE"}
  , Record {uchar = '\10506', commands = [("unicode-math","\\Uuparrow")], category = Rel, comments = "UPWARDS TRIPLE ARROW"}
  , Record {uchar = '\10507', commands = [("unicode-math","\\Ddownarrow")], category = Rel, comments = "DOWNWARDS TRIPLE ARROW"}
  , Record {uchar = '\10508', commands = [("unicode-math","\\leftbkarrow")], category = Rel, comments = "LEFTWARDS DOUBLE DASH ARROW"}
  , Record {uchar = '\10509', commands = [("unicode-math","\\rightbkarrow")], category = Rel, comments = "RIGHTWARDS DOUBLE DASH ARROW"}
  , Record {uchar = '\10510', commands = [("unicode-math","\\leftdbkarrow")], category = Rel, comments = "LEFTWARDS TRIPLE DASH ARROW"}
  , Record {uchar = '\10511', commands = [("unicode-math","\\dbkarow")], category = Rel, comments = "RIGHTWARDS TRIPLE DASH ARROW"}
  , Record {uchar = '\10512', commands = [("unicode-math","\\drbkarow")], category = Rel, comments = "RIGHTWARDS TWO-HEADED TRIPLE DASH ARROW"}
  , Record {uchar = '\10513', commands = [("unicode-math","\\rightdotarrow")], category = Rel, comments = "RIGHTWARDS ARROW WITH DOTTED STEM"}
  , Record {uchar = '\10514', commands = [("wrisym","\\UpArrowBar"),("unicode-math","\\baruparrow")], category = Rel, comments = "UPWARDS ARROW TO BAR"}
  , Record {uchar = '\10515', commands = [("wrisym","\\DownArrowBar"),("unicode-math","\\downarrowbar")], category = Rel, comments = "DOWNWARDS ARROW TO BAR"}
  , Record {uchar = '\10516', commands = [("oz","\\pinj"),("unicode-math","\\nvrightarrowtail")], category = Rel, comments = "RIGHTWARDS ARROW WITH TAIL WITH VERTICAL STROKE, z notation partial injection"}
  , Record {uchar = '\10517', commands = [("oz","\\finj"),("unicode-math","\\nVrightarrowtail")], category = Rel, comments = "RIGHTWARDS ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE, z notation finite injection"}
  , Record {uchar = '\10518', commands = [("oz","\\bij"),("unicode-math","\\twoheadrightarrowtail")], category = Rel, comments = "RIGHTWARDS TWO-HEADED ARROW WITH TAIL, z notation bijection"}
  , Record {uchar = '\10519', commands = [("unicode-math","\\nvtwoheadrightarrowtail")], category = Rel, comments = "RIGHTWARDS TWO-HEADED ARROW WITH TAIL WITH VERTICAL STROKE, z notation surjective injection"}
  , Record {uchar = '\10520', commands = [("unicode-math","\\nVtwoheadrightarrowtail")], category = Rel, comments = "RIGHTWARDS TWO-HEADED ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE, z notation finite surjective injection"}
  , Record {uchar = '\10521', commands = [("unicode-math","\\lefttail")], category = Rel, comments = "LEFTWARDS ARROW-TAIL"}
  , Record {uchar = '\10522', commands = [("unicode-math","\\righttail")], category = Rel, comments = "RIGHTWARDS ARROW-TAIL"}
  , Record {uchar = '\10523', commands = [("unicode-math","\\leftdbltail")], category = Rel, comments = "LEFTWARDS DOUBLE ARROW-TAIL"}
  , Record {uchar = '\10524', commands = [("unicode-math","\\rightdbltail")], category = Rel, comments = "RIGHTWARDS DOUBLE ARROW-TAIL"}
  , Record {uchar = '\10525', commands = [("unicode-math","\\diamondleftarrow")], category = Rel, comments = "LEFTWARDS ARROW TO BLACK DIAMOND"}
  , Record {uchar = '\10526', commands = [("unicode-math","\\rightarrowdiamond")], category = Rel, comments = "RIGHTWARDS ARROW TO BLACK DIAMOND"}
  , Record {uchar = '\10527', commands = [("unicode-math","\\diamondleftarrowbar")], category = Rel, comments = "LEFTWARDS ARROW FROM BAR TO BLACK DIAMOND"}
  , Record {uchar = '\10528', commands = [("unicode-math","\\barrightarrowdiamond")], category = Rel, comments = "RIGHTWARDS ARROW FROM BAR TO BLACK DIAMOND"}
  , Record {uchar = '\10529', commands = [("unicode-math","\\nwsearrow")], category = Rel, comments = "NORTH WEST AND SOUTH EAST ARROW"}
  , Record {uchar = '\10530', commands = [("unicode-math","\\neswarrow")], category = Rel, comments = "NORTH EAST AND SOUTH WEST ARROW"}
  , Record {uchar = '\10531', commands = [("unicode-math","\\hknwarrow")], category = Rel, comments = "NORTH WEST ARROW WITH HOOK"}
  , Record {uchar = '\10532', commands = [("unicode-math","\\hknearrow")], category = Rel, comments = "NORTH EAST ARROW WITH HOOK"}
  , Record {uchar = '\10533', commands = [("unicode-math","\\hksearow")], category = Rel, comments = "SOUTH EAST ARROW WITH HOOK"}
  , Record {uchar = '\10534', commands = [("unicode-math","\\hkswarow")], category = Rel, comments = "SOUTH WEST ARROW WITH HOOK"}
  , Record {uchar = '\10535', commands = [("unicode-math","\\tona")], category = Rel, comments = "NORTH WEST ARROW AND NORTH EAST ARROW"}
  , Record {uchar = '\10536', commands = [("unicode-math","\\toea")], category = Rel, comments = "NORTH EAST ARROW AND SOUTH EAST ARROW"}
  , Record {uchar = '\10537', commands = [("unicode-math","\\tosa")], category = Rel, comments = "SOUTH EAST ARROW AND SOUTH WEST ARROW"}
  , Record {uchar = '\10538', commands = [("unicode-math","\\towa")], category = Rel, comments = "SOUTH WEST ARROW AND NORTH WEST ARROW"}
  , Record {uchar = '\10539', commands = [("unicode-math","\\rdiagovfdiag")], category = Ord, comments = "RISING DIAGONAL CROSSING FALLING DIAGONAL"}
  , Record {uchar = '\10540', commands = [("unicode-math","\\fdiagovrdiag")], category = Ord, comments = "FALLING DIAGONAL CROSSING RISING DIAGONAL"}
  , Record {uchar = '\10541', commands = [("unicode-math","\\seovnearrow")], category = Ord, comments = "SOUTH EAST ARROW CROSSING NORTH EAST ARROW"}
  , Record {uchar = '\10542', commands = [("unicode-math","\\neovsearrow")], category = Ord, comments = "NORTH EAST ARROW CROSSING SOUTH EAST ARROW"}
  , Record {uchar = '\10543', commands = [("unicode-math","\\fdiagovnearrow")], category = Ord, comments = "FALLING DIAGONAL CROSSING NORTH EAST ARROW"}
  , Record {uchar = '\10544', commands = [("unicode-math","\\rdiagovsearrow")], category = Ord, comments = "RISING DIAGONAL CROSSING SOUTH EAST ARROW"}
  , Record {uchar = '\10545', commands = [("unicode-math","\\neovnwarrow")], category = Ord, comments = "NORTH EAST ARROW CROSSING NORTH WEST ARROW"}
  , Record {uchar = '\10546', commands = [("unicode-math","\\nwovnearrow")], category = Ord, comments = "NORTH WEST ARROW CROSSING NORTH EAST ARROW"}
  , Record {uchar = '\10547', commands = [("txfonts","\\leadsto"),("unicode-math","\\rightcurvedarrow")], category = Rel, comments = "WAVE ARROW POINTING DIRECTLY RIGHT"}
  , Record {uchar = '\10548', commands = [("unicode-math","\\uprightcurvearrow")], category = Ord, comments = "ARROW POINTING RIGHTWARDS THEN CURVING UPWARDS"}
  , Record {uchar = '\10549', commands = [("unicode-math","\\downrightcurvedarrow")], category = Ord, comments = "ARROW POINTING RIGHTWARDS THEN CURVING DOWNWARDS"}
  , Record {uchar = '\10550', commands = [("unicode-math","\\leftdowncurvedarrow")], category = Rel, comments = "ARROW POINTING DOWNWARDS THEN CURVING LEFTWARDS"}
  , Record {uchar = '\10551', commands = [("unicode-math","\\rightdowncurvedarrow")], category = Rel, comments = "ARROW POINTING DOWNWARDS THEN CURVING RIGHTWARDS"}
  , Record {uchar = '\10552', commands = [("unicode-math","\\cwrightarcarrow")], category = Rel, comments = "RIGHT-SIDE ARC CLOCKWISE ARROW"}
  , Record {uchar = '\10553', commands = [("unicode-math","\\acwleftarcarrow")], category = Rel, comments = "LEFT-SIDE ARC ANTICLOCKWISE ARROW"}
  , Record {uchar = '\10554', commands = [("unicode-math","\\acwoverarcarrow")], category = Rel, comments = "TOP ARC ANTICLOCKWISE ARROW"}
  , Record {uchar = '\10555', commands = [("unicode-math","\\acwunderarcarrow")], category = Rel, comments = "BOTTOM ARC ANTICLOCKWISE ARROW"}
  , Record {uchar = '\10556', commands = [("unicode-math","\\curvearrowrightminus")], category = Rel, comments = "TOP ARC CLOCKWISE ARROW WITH MINUS"}
  , Record {uchar = '\10557', commands = [("unicode-math","\\curvearrowleftplus")], category = Rel, comments = "TOP ARC ANTICLOCKWISE ARROW WITH PLUS"}
  , Record {uchar = '\10558', commands = [("unicode-math","\\cwundercurvearrow")], category = Rel, comments = "LOWER RIGHT SEMICIRCULAR CLOCKWISE ARROW"}
  , Record {uchar = '\10559', commands = [("unicode-math","\\ccwundercurvearrow")], category = Rel, comments = "LOWER LEFT SEMICIRCULAR ANTICLOCKWISE ARROW"}
  , Record {uchar = '\10560', commands = [("unicode-math","\\acwcirclearrow")], category = Rel, comments = "ANTICLOCKWISE CLOSED CIRCLE ARROW"}
  , Record {uchar = '\10561', commands = [("unicode-math","\\cwcirclearrow")], category = Rel, comments = "CLOCKWISE CLOSED CIRCLE ARROW"}
  , Record {uchar = '\10562', commands = [("unicode-math","\\rightarrowshortleftarrow")], category = Rel, comments = "RIGHTWARDS ARROW ABOVE SHORT LEFTWARDS ARROW"}
  , Record {uchar = '\10563', commands = [("unicode-math","\\leftarrowshortrightarrow")], category = Rel, comments = "LEFTWARDS ARROW ABOVE SHORT RIGHTWARDS ARROW"}
  , Record {uchar = '\10564', commands = [("unicode-math","\\shortrightarrowleftarrow")], category = Rel, comments = "SHORT RIGHTWARDS ARROW ABOVE LEFTWARDS ARROW"}
  , Record {uchar = '\10565', commands = [("unicode-math","\\rightarrowplus")], category = Rel, comments = "RIGHTWARDS ARROW WITH PLUS BELOW"}
  , Record {uchar = '\10566', commands = [("unicode-math","\\leftarrowplus")], category = Rel, comments = "LEFTWARDS ARROW WITH PLUS BELOW"}
  , Record {uchar = '\10567', commands = [("unicode-math","\\rightarrowx")], category = Rel, comments = "RIGHTWARDS ARROW THROUGH X"}
  , Record {uchar = '\10568', commands = [("unicode-math","\\leftrightarrowcircle")], category = Rel, comments = "LEFT RIGHT ARROW THROUGH SMALL CIRCLE"}
  , Record {uchar = '\10569', commands = [("unicode-math","\\twoheaduparrowcircle")], category = Rel, comments = "UPWARDS TWO-HEADED ARROW FROM SMALL CIRCLE"}
  , Record {uchar = '\10570', commands = [("mathabx","\\leftrightharpoon"),("unicode-math","\\leftrightharpoonupdown")], category = Rel, comments = "LEFT BARB UP RIGHT BARB DOWN HARPOON"}
  , Record {uchar = '\10571', commands = [("mathabx","\\rightleftharpoon"),("unicode-math","\\leftrightharpoondownup")], category = Rel, comments = "LEFT BARB DOWN RIGHT BARB UP HARPOON"}
  , Record {uchar = '\10572', commands = [("unicode-math","\\updownharpoonrightleft")], category = Rel, comments = "UP BARB RIGHT DOWN BARB LEFT HARPOON"}
  , Record {uchar = '\10573', commands = [("unicode-math","\\updownharpoonleftright")], category = Rel, comments = "UP BARB LEFT DOWN BARB RIGHT HARPOON"}
  , Record {uchar = '\10574', commands = [("wrisym","\\leftrightharpoonup"),("unicode-math","\\leftrightharpoonupup")], category = Rel, comments = "LEFT BARB UP RIGHT BARB UP HARPOON"}
  , Record {uchar = '\10575', commands = [("wrisym","\\rightupdownharpoon"),("unicode-math","\\updownharpoonrightright")], category = Rel, comments = "UP BARB RIGHT DOWN BARB RIGHT HARPOON"}
  , Record {uchar = '\10576', commands = [("wrisym","\\leftrightharpoondown"),("unicode-math","\\leftrightharpoondowndown")], category = Rel, comments = "LEFT BARB DOWN RIGHT BARB DOWN HARPOON"}
  , Record {uchar = '\10577', commands = [("wrisym","\\leftupdownharpoon"),("unicode-math","\\updownharpoonleftleft")], category = Rel, comments = "UP BARB LEFT DOWN BARB LEFT HARPOON"}
  , Record {uchar = '\10578', commands = [("wrisym","\\LeftVectorBar"),("unicode-math","\\barleftharpoonup")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB UP TO BAR"}
  , Record {uchar = '\10579', commands = [("wrisym","\\RightVectorBar"),("unicode-math","\\rightharpoonupbar")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB UP TO BAR"}
  , Record {uchar = '\10580', commands = [("wrisym","\\RightUpVectorBar"),("unicode-math","\\barupharpoonright")], category = Rel, comments = "UPWARDS HARPOON WITH BARB RIGHT TO BAR"}
  , Record {uchar = '\10581', commands = [("wrisym","\\RightDownVectorBar"),("unicode-math","\\downharpoonrightbar")], category = Rel, comments = "DOWNWARDS HARPOON WITH BARB RIGHT TO BAR"}
  , Record {uchar = '\10582', commands = [("wrisym","\\DownLeftVectorBar"),("unicode-math","\\barleftharpoondown")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB DOWN TO BAR"}
  , Record {uchar = '\10583', commands = [("wrisym","\\DownRightVectorBar"),("unicode-math","\\rightharpoondownbar")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB DOWN TO BAR"}
  , Record {uchar = '\10584', commands = [("wrisym","\\LeftUpVectorBar"),("unicode-math","\\barupharpoonleft")], category = Rel, comments = "UPWARDS HARPOON WITH BARB LEFT TO BAR"}
  , Record {uchar = '\10585', commands = [("wrisym","\\LeftDownVectorBar"),("unicode-math","\\downharpoonleftbar")], category = Rel, comments = "DOWNWARDS HARPOON WITH BARB LEFT TO BAR"}
  , Record {uchar = '\10586', commands = [("wrisym","\\LeftTeeVector"),("unicode-math","\\leftharpoonupbar")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB UP FROM BAR"}
  , Record {uchar = '\10587', commands = [("wrisym","\\RightTeeVector"),("unicode-math","\\barrightharpoonup")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB UP FROM BAR"}
  , Record {uchar = '\10588', commands = [("wrisym","\\RightUpTeeVector"),("unicode-math","\\upharpoonrightbar")], category = Rel, comments = "UPWARDS HARPOON WITH BARB RIGHT FROM BAR"}
  , Record {uchar = '\10589', commands = [("wrisym","\\RightDownTeeVector"),("unicode-math","\\bardownharpoonright")], category = Rel, comments = "DOWNWARDS HARPOON WITH BARB RIGHT FROM BAR"}
  , Record {uchar = '\10590', commands = [("wrisym","\\DownLeftTeeVector"),("unicode-math","\\leftharpoondownbar")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB DOWN FROM BAR"}
  , Record {uchar = '\10591', commands = [("wrisym","\\DownRightTeeVector"),("unicode-math","\\barrightharpoondown")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB DOWN FROM BAR"}
  , Record {uchar = '\10592', commands = [("wrisym","\\LeftUpTeeVector"),("unicode-math","\\upharpoonleftbar")], category = Rel, comments = "UPWARDS HARPOON WITH BARB LEFT FROM BAR"}
  , Record {uchar = '\10593', commands = [("wrisym","\\LeftDownTeeVector"),("unicode-math","\\bardownharpoonleft")], category = Rel, comments = "DOWNWARDS HARPOON WITH BARB LEFT FROM BAR"}
  , Record {uchar = '\10594', commands = [("mathabx","\\leftleftharpoons"),("unicode-math","\\leftharpoonsupdown")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB UP ABOVE LEFTWARDS HARPOON WITH BARB DOWN"}
  , Record {uchar = '\10595', commands = [("mathabx","\\upupharpoons"),("unicode-math","\\upharpoonsleftright")], category = Rel, comments = "UPWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT"}
  , Record {uchar = '\10596', commands = [("mathabx","\\rightrightharpoons"),("unicode-math","\\rightharpoonsupdown")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB UP ABOVE RIGHTWARDS HARPOON WITH BARB DOWN"}
  , Record {uchar = '\10597', commands = [("mathabx","\\downdownharpoons"),("unicode-math","\\downharpoonsleftright")], category = Rel, comments = "DOWNWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT"}
  , Record {uchar = '\10598', commands = [("unicode-math","\\leftrightharpoonsup")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB UP ABOVE RIGHTWARDS HARPOON WITH BARB UP"}
  , Record {uchar = '\10599', commands = [("unicode-math","\\leftrightharpoonsdown")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB DOWN ABOVE RIGHTWARDS HARPOON WITH BARB DOWN"}
  , Record {uchar = '\10600', commands = [("unicode-math","\\rightleftharpoonsup")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB UP ABOVE LEFTWARDS HARPOON WITH BARB UP"}
  , Record {uchar = '\10601', commands = [("unicode-math","\\rightleftharpoonsdown")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB DOWN ABOVE LEFTWARDS HARPOON WITH BARB DOWN"}
  , Record {uchar = '\10602', commands = [("mathabx","\\leftbarharpoon"),("unicode-math","\\leftharpoonupdash")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB UP ABOVE LONG DASH"}
  , Record {uchar = '\10603', commands = [("mathabx","\\barleftharpoon"),("unicode-math","\\dashleftharpoondown")], category = Rel, comments = "LEFTWARDS HARPOON WITH BARB DOWN BELOW LONG DASH"}
  , Record {uchar = '\10604', commands = [("mathabx","\\rightbarharpoon"),("unicode-math","\\rightharpoonupdash")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB UP ABOVE LONG DASH"}
  , Record {uchar = '\10605', commands = [("mathabx","\\barrightharpoon"),("unicode-math","\\dashrightharpoondown")], category = Rel, comments = "RIGHTWARDS HARPOON WITH BARB DOWN BELOW LONG DASH"}
  , Record {uchar = '\10606', commands = [("mathabx","\\updownharpoons"),("wrisym","\\upequilibrium"),("unicode-math","\\updownharpoonsleftright")], category = Rel, comments = "UPWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT"}
  , Record {uchar = '\10607', commands = [("mathabx","\\downupharpoons"),("wrisym","\\uprevequilibrium"),("unicode-math","\\downupharpoonsleftright")], category = Rel, comments = "DOWNWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT"}
  , Record {uchar = '\10608', commands = [("unicode-math","\\rightimply")], category = Rel, comments = "RIGHT DOUBLE ARROW WITH ROUNDED HEAD"}
  , Record {uchar = '\10609', commands = [("unicode-math","\\equalrightarrow")], category = Rel, comments = "EQUALS SIGN ABOVE RIGHTWARDS ARROW"}
  , Record {uchar = '\10610', commands = [("unicode-math","\\similarrightarrow")], category = Rel, comments = "TILDE OPERATOR ABOVE RIGHTWARDS ARROW"}
  , Record {uchar = '\10611', commands = [("unicode-math","\\leftarrowsimilar")], category = Rel, comments = "LEFTWARDS ARROW ABOVE TILDE OPERATOR"}
  , Record {uchar = '\10612', commands = [("unicode-math","\\rightarrowsimilar")], category = Rel, comments = "RIGHTWARDS ARROW ABOVE TILDE OPERATOR"}
  , Record {uchar = '\10613', commands = [("unicode-math","\\rightarrowapprox")], category = Rel, comments = "RIGHTWARDS ARROW ABOVE ALMOST EQUAL TO"}
  , Record {uchar = '\10614', commands = [("unicode-math","\\ltlarr")], category = Rel, comments = "LESS-THAN ABOVE LEFTWARDS ARROW"}
  , Record {uchar = '\10615', commands = [("unicode-math","\\leftarrowless")], category = Rel, comments = "LEFTWARDS ARROW THROUGH LESS-THAN"}
  , Record {uchar = '\10616', commands = [("unicode-math","\\gtrarr")], category = Rel, comments = "GREATER-THAN ABOVE RIGHTWARDS ARROW"}
  , Record {uchar = '\10617', commands = [("unicode-math","\\subrarr")], category = Rel, comments = "SUBSET ABOVE RIGHTWARDS ARROW"}
  , Record {uchar = '\10618', commands = [("unicode-math","\\leftarrowsubset")], category = Rel, comments = "LEFTWARDS ARROW THROUGH SUBSET"}
  , Record {uchar = '\10619', commands = [("unicode-math","\\suplarr")], category = Rel, comments = "SUPERSET ABOVE LEFTWARDS ARROW"}
  , Record {uchar = '\10620', commands = [("txfonts","\\strictfi"),("unicode-math","\\leftfishtail")], category = Rel, comments = "LEFT FISH TAIL"}
  , Record {uchar = '\10621', commands = [("txfonts","\\strictif"),("unicode-math","\\rightfishtail")], category = Rel, comments = "RIGHT FISH TAIL"}
  , Record {uchar = '\10622', commands = [("unicode-math","\\upfishtail")], category = Rel, comments = "UP FISH TAIL"}
  , Record {uchar = '\10623', commands = [("unicode-math","\\downfishtail")], category = Rel, comments = "DOWN FISH TAIL"}
  , Record {uchar = '\10624', commands = [("fourier","\\VERT"),("unicode-math","\\Vvert")], category = Fence, comments = "TRIPLE VERTICAL BAR DELIMITER"}
  , Record {uchar = '\10625', commands = [("oz","\\spot"),("oz","\\dot"),("unicode-math","\\mdsmblkcircle")], category = Ord, comments = "Z NOTATION SPOT"}
  , Record {uchar = '\10626', commands = [("unicode-math","\\typecolon")], category = Bin, comments = "Z NOTATION TYPE COLON, (present in bbold font but no command)"}
  , Record {uchar = '\10627', commands = [("unicode-math","\\lBrace")], category = Open, comments = "LEFT WHITE CURLY BRACKET"}
  , Record {uchar = '\10628', commands = [("unicode-math","\\rBrace")], category = Close, comments = "RIGHT WHITE CURLY BRACKET"}
  , Record {uchar = '\10629', commands = [("mathbbol","\\Lparen"),("unicode-math","\\lParen")], category = Open, comments = "LEFT WHITE PARENTHESIS"}
  , Record {uchar = '\10630', commands = [("mathbbol","\\Rparen"),("unicode-math","\\rParen")], category = Close, comments = "RIGHT WHITE PARENTHESIS"}
  , Record {uchar = '\10631', commands = [("oz","\\limg"),("stmaryrd","\\llparenthesis"),("unicode-math","\\llparenthesis")], category = Open, comments = "Z NOTATION LEFT IMAGE BRACKET"}
  , Record {uchar = '\10632', commands = [("oz","\\rimg"),("stmaryrd","\\rrparenthesis"),("unicode-math","\\rrparenthesis")], category = Close, comments = "Z NOTATION RIGHT IMAGE BRACKET"}
  , Record {uchar = '\10633', commands = [("oz","\\lblot"),("unicode-math","\\llangle")], category = Open, comments = "Z NOTATION LEFT BINDING BRACKET"}
  , Record {uchar = '\10634', commands = [("oz","\\rblot"),("unicode-math","\\rrangle")], category = Close, comments = "Z NOTATION RIGHT BINDING BRACKET"}
  , Record {uchar = '\10635', commands = [("unicode-math","\\lbrackubar")], category = Open, comments = "LEFT SQUARE BRACKET WITH UNDERBAR"}
  , Record {uchar = '\10636', commands = [("unicode-math","\\rbrackubar")], category = Close, comments = "RIGHT SQUARE BRACKET WITH UNDERBAR"}
  , Record {uchar = '\10637', commands = [("unicode-math","\\lbrackultick")], category = Open, comments = "LEFT SQUARE BRACKET WITH TICK IN TOP CORNER"}
  , Record {uchar = '\10638', commands = [("unicode-math","\\rbracklrtick")], category = Close, comments = "RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER"}
  , Record {uchar = '\10639', commands = [("unicode-math","\\lbracklltick")], category = Open, comments = "LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER"}
  , Record {uchar = '\10640', commands = [("unicode-math","\\rbrackurtick")], category = Close, comments = "RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER"}
  , Record {uchar = '\10641', commands = [("unicode-math","\\langledot")], category = Open, comments = "LEFT ANGLE BRACKET WITH DOT"}
  , Record {uchar = '\10642', commands = [("unicode-math","\\rangledot")], category = Close, comments = "RIGHT ANGLE BRACKET WITH DOT"}
  , Record {uchar = '\10643', commands = [("unicode-math","\\lparenless")], category = Open, comments = "LEFT ARC LESS-THAN BRACKET"}
  , Record {uchar = '\10644', commands = [("unicode-math","\\rparengtr")], category = Close, comments = "RIGHT ARC GREATER-THAN BRACKET"}
  , Record {uchar = '\10645', commands = [("unicode-math","\\Lparengtr")], category = Open, comments = "DOUBLE LEFT ARC GREATER-THAN BRACKET"}
  , Record {uchar = '\10646', commands = [("unicode-math","\\Rparenless")], category = Close, comments = "DOUBLE RIGHT ARC LESS-THAN BRACKET"}
  , Record {uchar = '\10647', commands = [("unicode-math","\\lblkbrbrak")], category = Open, comments = "LEFT BLACK TORTOISE SHELL BRACKET"}
  , Record {uchar = '\10648', commands = [("unicode-math","\\rblkbrbrak")], category = Close, comments = "RIGHT BLACK TORTOISE SHELL BRACKET"}
  , Record {uchar = '\10649', commands = [("unicode-math","\\fourvdots")], category = Ord, comments = "DOTTED FENCE"}
  , Record {uchar = '\10650', commands = [("unicode-math","\\vzigzag")], category = Ord, comments = "VERTICAL ZIGZAG LINE"}
  , Record {uchar = '\10651', commands = [("unicode-math","\\measuredangleleft")], category = Ord, comments = "MEASURED ANGLE OPENING LEFT"}
  , Record {uchar = '\10652', commands = [("unicode-math","\\rightanglesqr")], category = Ord, comments = "RIGHT ANGLE VARIANT WITH SQUARE"}
  , Record {uchar = '\10653', commands = [("unicode-math","\\rightanglemdot")], category = Ord, comments = "MEASURED RIGHT ANGLE WITH DOT"}
  , Record {uchar = '\10654', commands = [("unicode-math","\\angles")], category = Ord, comments = "ANGLE WITH S INSIDE"}
  , Record {uchar = '\10655', commands = [("unicode-math","\\angdnr")], category = Ord, comments = "ACUTE ANGLE"}
  , Record {uchar = '\10656', commands = [("unicode-math","\\gtlpar")], category = Ord, comments = "SPHERICAL ANGLE OPENING LEFT"}
  , Record {uchar = '\10657', commands = [("unicode-math","\\sphericalangleup")], category = Ord, comments = "SPHERICAL ANGLE OPENING UP"}
  , Record {uchar = '\10658', commands = [("unicode-math","\\turnangle")], category = Ord, comments = "TURNED ANGLE"}
  , Record {uchar = '\10659', commands = [("unicode-math","\\revangle")], category = Ord, comments = "REVERSED ANGLE"}
  , Record {uchar = '\10660', commands = [("unicode-math","\\angleubar")], category = Ord, comments = "ANGLE WITH UNDERBAR"}
  , Record {uchar = '\10661', commands = [("unicode-math","\\revangleubar")], category = Ord, comments = "REVERSED ANGLE WITH UNDERBAR"}
  , Record {uchar = '\10662', commands = [("unicode-math","\\wideangledown")], category = Ord, comments = "OBLIQUE ANGLE OPENING UP"}
  , Record {uchar = '\10663', commands = [("unicode-math","\\wideangleup")], category = Ord, comments = "OBLIQUE ANGLE OPENING DOWN"}
  , Record {uchar = '\10664', commands = [("unicode-math","\\measanglerutone")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING UP AND RIGHT"}
  , Record {uchar = '\10665', commands = [("unicode-math","\\measanglelutonw")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING UP AND LEFT"}
  , Record {uchar = '\10666', commands = [("unicode-math","\\measanglerdtose")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING DOWN AND RIGHT"}
  , Record {uchar = '\10667', commands = [("unicode-math","\\measangleldtosw")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING DOWN AND LEFT"}
  , Record {uchar = '\10668', commands = [("unicode-math","\\measangleurtone")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING RIGHT AND UP"}
  , Record {uchar = '\10669', commands = [("unicode-math","\\measangleultonw")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING LEFT AND UP"}
  , Record {uchar = '\10670', commands = [("unicode-math","\\measangledrtose")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING RIGHT AND DOWN"}
  , Record {uchar = '\10671', commands = [("unicode-math","\\measangledltosw")], category = Ord, comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING LEFT AND DOWN"}
  , Record {uchar = '\10672', commands = [("unicode-math","\\revemptyset")], category = Ord, comments = "REVERSED EMPTY SET"}
  , Record {uchar = '\10673', commands = [("unicode-math","\\emptysetobar")], category = Ord, comments = "EMPTY SET WITH OVERBAR"}
  , Record {uchar = '\10674', commands = [("unicode-math","\\emptysetocirc")], category = Ord, comments = "EMPTY SET WITH SMALL CIRCLE ABOVE"}
  , Record {uchar = '\10675', commands = [("unicode-math","\\emptysetoarr")], category = Ord, comments = "EMPTY SET WITH RIGHT ARROW ABOVE"}
  , Record {uchar = '\10676', commands = [("unicode-math","\\emptysetoarrl")], category = Ord, comments = "EMPTY SET WITH LEFT ARROW ABOVE"}
  , Record {uchar = '\10677', commands = [("unicode-math","\\circlehbar")], category = Bin, comments = "CIRCLE WITH HORIZONTAL BAR"}
  , Record {uchar = '\10678', commands = [("unicode-math","\\circledvert")], category = Bin, comments = "CIRCLED VERTICAL BAR"}
  , Record {uchar = '\10679', commands = [("unicode-math","\\circledparallel")], category = Bin, comments = "CIRCLED PARALLEL"}
  , Record {uchar = '\10680', commands = [("txfonts","\\circledbslash"),("unicode-math","\\obslash")], category = Bin, comments = "CIRCLED REVERSE SOLIDUS"}
  , Record {uchar = '\10681', commands = [("unicode-math","\\operp")], category = Bin, comments = "CIRCLED PERPENDICULAR"}
  , Record {uchar = '\10682', commands = [("unicode-math","\\obot")], category = Ord, comments = "CIRCLE DIVIDED BY HORIZONTAL BAR AND TOP HALF DIVIDED BY VERTICAL BAR"}
  , Record {uchar = '\10683', commands = [("unicode-math","\\olcross")], category = Ord, comments = "CIRCLE WITH SUPERIMPOSED X"}
  , Record {uchar = '\10684', commands = [("unicode-math","\\odotslashdot")], category = Ord, comments = "CIRCLED ANTICLOCKWISE-ROTATED DIVISION SIGN"}
  , Record {uchar = '\10685', commands = [("unicode-math","\\uparrowoncircle")], category = Ord, comments = "UP ARROW THROUGH CIRCLE"}
  , Record {uchar = '\10686', commands = [("unicode-math","\\circledwhitebullet")], category = Ord, comments = "CIRCLED WHITE BULLET"}
  , Record {uchar = '\10687', commands = [("unicode-math","\\circledbullet")], category = Ord, comments = "CIRCLED BULLET"}
  , Record {uchar = '\10688', commands = [("txfonts","\\circledless"),("unicode-math","\\olessthan")], category = Bin, comments = "CIRCLED LESS-THAN"}
  , Record {uchar = '\10689', commands = [("txfonts","\\circledgtr"),("unicode-math","\\ogreaterthan")], category = Bin, comments = "CIRCLED GREATER-THAN"}
  , Record {uchar = '\10690', commands = [("unicode-math","\\cirscir")], category = Ord, comments = "CIRCLE WITH SMALL CIRCLE TO THE RIGHT"}
  , Record {uchar = '\10691', commands = [("unicode-math","\\cirE")], category = Ord, comments = "CIRCLE WITH TWO HORIZONTAL STROKES TO THE RIGHT"}
  , Record {uchar = '\10692', commands = [("stmaryrd","\\boxslash"),("txfonts","\\boxslash"),("unicode-math","\\boxdiag")], category = Bin, comments = "SQUARED RISING DIAGONAL SLASH"}
  , Record {uchar = '\10693', commands = [("stmaryrd","\\boxbslash"),("txfonts","\\boxbslash"),("unicode-math","\\boxbslash")], category = Bin, comments = "SQUARED FALLING DIAGONAL SLASH"}
  , Record {uchar = '\10694', commands = [("stmaryrd","\\boxast"),("txfonts","\\boxast"),("unicode-math","\\boxast")], category = Bin, comments = "SQUARED ASTERISK"}
  , Record {uchar = '\10695', commands = [("stmaryrd","\\boxcircle"),("unicode-math","\\boxcircle")], category = Bin, comments = "SQUARED SMALL CIRCLE"}
  , Record {uchar = '\10696', commands = [("stmaryrd","\\boxbox"),("unicode-math","\\boxbox")], category = Bin, comments = "SQUARED SQUARE"}
  , Record {uchar = '\10697', commands = [("unicode-math","\\boxonbox")], category = Ord, comments = "TWO JOINED SQUARES"}
  , Record {uchar = '\10698', commands = [("unicode-math","\\triangleodot")], category = Ord, comments = "TRIANGLE WITH DOT ABOVE"}
  , Record {uchar = '\10699', commands = [("unicode-math","\\triangleubar")], category = Ord, comments = "TRIANGLE WITH UNDERBAR"}
  , Record {uchar = '\10700', commands = [("unicode-math","\\triangles")], category = Ord, comments = "S IN TRIANGLE"}
  , Record {uchar = '\10701', commands = [("unicode-math","\\triangleserifs")], category = Bin, comments = "TRIANGLE WITH SERIFS AT BOTTOM"}
  , Record {uchar = '\10702', commands = [("unicode-math","\\rtriltri")], category = Rel, comments = "RIGHT TRIANGLE ABOVE LEFT TRIANGLE"}
  , Record {uchar = '\10703', commands = [("wrisym","\\LeftTriangleBar"),("unicode-math","\\ltrivb")], category = Rel, comments = "LEFT TRIANGLE BESIDE VERTICAL BAR"}
  , Record {uchar = '\10704', commands = [("wrisym","\\RightTriangleBar"),("unicode-math","\\vbrtri")], category = Rel, comments = "VERTICAL BAR BESIDE RIGHT TRIANGLE"}
  , Record {uchar = '\10705', commands = [("unicode-math","\\lfbowtie")], category = Rel, comments = "left black bowtie"}
  , Record {uchar = '\10706', commands = [("unicode-math","\\rfbowtie")], category = Rel, comments = "right black bowtie"}
  , Record {uchar = '\10707', commands = [("unicode-math","\\fbowtie")], category = Rel, comments = "BLACK BOWTIE"}
  , Record {uchar = '\10708', commands = [("unicode-math","\\lftimes")], category = Rel, comments = "left black times"}
  , Record {uchar = '\10709', commands = [("unicode-math","\\rftimes")], category = Rel, comments = "right black times"}
  , Record {uchar = '\10710', commands = [("unicode-math","\\hourglass")], category = Bin, comments = "WHITE HOURGLASS"}
  , Record {uchar = '\10711', commands = [("unicode-math","\\blackhourglass")], category = Bin, comments = "BLACK HOURGLASS"}
  , Record {uchar = '\10712', commands = [("unicode-math","\\lvzigzag")], category = Open, comments = "LEFT WIGGLY FENCE"}
  , Record {uchar = '\10713', commands = [("unicode-math","\\rvzigzag")], category = Close, comments = "RIGHT WIGGLY FENCE"}
  , Record {uchar = '\10714', commands = [("unicode-math","\\Lvzigzag")], category = Open, comments = "LEFT DOUBLE WIGGLY FENCE"}
  , Record {uchar = '\10715', commands = [("unicode-math","\\Rvzigzag")], category = Close, comments = "RIGHT DOUBLE WIGGLY FENCE"}
  , Record {uchar = '\10716', commands = [("unicode-math","\\iinfin")], category = Ord, comments = "INCOMPLETE INFINITY"}
  , Record {uchar = '\10717', commands = [("unicode-math","\\tieinfty")], category = Ord, comments = "TIE OVER INFINITY"}
  , Record {uchar = '\10718', commands = [("unicode-math","\\nvinfty")], category = Ord, comments = "INFINITY NEGATED WITH VERTICAL BAR"}
  , Record {uchar = '\10719', commands = [("txfonts","\\multimapboth"),("unicode-math","\\dualmap")], category = Rel, comments = "DOUBLE-ENDED MULTIMAP"}
  , Record {uchar = '\10720', commands = [("unicode-math","\\laplac")], category = Ord, comments = "SQUARE WITH CONTOURED OUTLINE"}
  , Record {uchar = '\10721', commands = [("unicode-math","\\lrtriangleeq")], category = Rel, comments = "INCREASES AS"}
  , Record {uchar = '\10722', commands = [("unicode-math","\\shuffle")], category = Bin, comments = "SHUFFLE PRODUCT"}
  , Record {uchar = '\10723', commands = [("unicode-math","\\eparsl")], category = Rel, comments = "EQUALS SIGN AND SLANTED PARALLEL"}
  , Record {uchar = '\10724', commands = [("unicode-math","\\smeparsl")], category = Rel, comments = "EQUALS SIGN AND SLANTED PARALLEL WITH TILDE ABOVE"}
  , Record {uchar = '\10725', commands = [("unicode-math","\\eqvparsl")], category = Rel, comments = "IDENTICAL TO AND SLANTED PARALLEL"}
  , Record {uchar = '\10726', commands = [("unicode-math","\\gleichstark")], category = Rel, comments = "GLEICH STARK"}
  , Record {uchar = '\10727', commands = [("unicode-math","\\thermod")], category = Ord, comments = "THERMODYNAMIC"}
  , Record {uchar = '\10728', commands = [("unicode-math","\\downtriangleleftblack")], category = Ord, comments = "DOWN-POINTING TRIANGLE WITH LEFT HALF BLACK"}
  , Record {uchar = '\10729', commands = [("unicode-math","\\downtrianglerightblack")], category = Ord, comments = "DOWN-POINTING TRIANGLE WITH RIGHT HALF BLACK"}
  , Record {uchar = '\10730', commands = [("unicode-math","\\blackdiamonddownarrow")], category = Ord, comments = "BLACK DIAMOND WITH DOWN ARROW"}
  , Record {uchar = '\10731', commands = [("amssymb","\\blacklozenge"),("unicode-math","\\mdlgblklozenge")], category = Bin, comments = "BLACK LOZENGE"}
  , Record {uchar = '\10732', commands = [("unicode-math","\\circledownarrow")], category = Ord, comments = "WHITE CIRCLE WITH DOWN ARROW"}
  , Record {uchar = '\10733', commands = [("unicode-math","\\blackcircledownarrow")], category = Ord, comments = "BLACK CIRCLE WITH DOWN ARROW"}
  , Record {uchar = '\10734', commands = [("unicode-math","\\errbarsquare")], category = Ord, comments = "ERROR-BARRED WHITE SQUARE"}
  , Record {uchar = '\10735', commands = [("unicode-math","\\errbarblacksquare")], category = Ord, comments = "ERROR-BARRED BLACK SQUARE"}
  , Record {uchar = '\10736', commands = [("unicode-math","\\errbardiamond")], category = Ord, comments = "ERROR-BARRED WHITE DIAMOND"}
  , Record {uchar = '\10737', commands = [("unicode-math","\\errbarblackdiamond")], category = Ord, comments = "ERROR-BARRED BLACK DIAMOND"}
  , Record {uchar = '\10738', commands = [("unicode-math","\\errbarcircle")], category = Ord, comments = "ERROR-BARRED WHITE CIRCLE"}
  , Record {uchar = '\10739', commands = [("unicode-math","\\errbarblackcircle")], category = Ord, comments = "ERROR-BARRED BLACK CIRCLE"}
  , Record {uchar = '\10740', commands = [("unicode-math","\\ruledelayed")], category = Rel, comments = "RULE-DELAYED"}
  , Record {uchar = '\10741', commands = [("base","\\setminus"),("unicode-math","\\setminus")], category = Bin, comments = "REVERSE SOLIDUS OPERATOR"}
  , Record {uchar = '\10742', commands = [("unicode-math","\\dsol")], category = Bin, comments = "SOLIDUS WITH OVERBAR"}
  , Record {uchar = '\10743', commands = [("unicode-math","\\rsolbar")], category = Bin, comments = "REVERSE SOLIDUS WITH HORIZONTAL STROKE"}
  , Record {uchar = '\10744', commands = [("unicode-math","\\xsol")], category = Op, comments = "BIG SOLIDUS"}
  , Record {uchar = '\10745', commands = [("oz","\\zhide"),("oz","\\hide"),("unicode-math","\\xbsol")], category = Op, comments = "BIG REVERSE SOLIDUS, z notation schema hiding"}
  , Record {uchar = '\10746', commands = [("unicode-math","\\doubleplus")], category = Bin, comments = "DOUBLE PLUS"}
  , Record {uchar = '\10747', commands = [("unicode-math","\\tripleplus")], category = Bin, comments = "TRIPLE PLUS"}
  , Record {uchar = '\10748', commands = [("unicode-math","\\lcurvyangle")], category = Open, comments = "left pointing curved angle bracket"}
  , Record {uchar = '\10749', commands = [("unicode-math","\\rcurvyangle")], category = Close, comments = "right pointing curved angle bracket"}
  , Record {uchar = '\10750', commands = [("unicode-math","\\tplus")], category = Bin, comments = "TINY"}
  , Record {uchar = '\10751', commands = [("unicode-math","\\tminus")], category = Bin, comments = "MINY"}
  , Record {uchar = '\10752', commands = [("base","\\bigodot"),("unicode-math","\\bigodot")], category = Op, comments = "N-ARY CIRCLED DOT OPERATOR"}
  , Record {uchar = '\10753', commands = [("base","\\bigoplus"),("unicode-math","\\bigoplus")], category = Op, comments = "N-ARY CIRCLED PLUS OPERATOR"}
  , Record {uchar = '\10754', commands = [("base","\\bigotimes"),("unicode-math","\\bigotimes")], category = Op, comments = "N-ARY CIRCLED TIMES OPERATOR"}
  , Record {uchar = '\10755', commands = [("unicode-math","\\bigcupdot")], category = Op, comments = "N-ARY UNION OPERATOR WITH DOT"}
  , Record {uchar = '\10756', commands = [("base","\\biguplus"),("unicode-math","\\biguplus")], category = Op, comments = "N-ARY UNION OPERATOR WITH PLUS"}
  , Record {uchar = '\10757', commands = [("txfonts","\\bigsqcap"),("unicode-math","\\bigsqcap")], category = Op, comments = "N-ARY SQUARE INTERSECTION OPERATOR"}
  , Record {uchar = '\10758', commands = [("base","\\bigsqcup"),("unicode-math","\\bigsqcup")], category = Op, comments = "N-ARY SQUARE UNION OPERATOR"}
  , Record {uchar = '\10759', commands = [("unicode-math","\\conjquant")], category = Op, comments = "TWO LOGICAL AND OPERATOR"}
  , Record {uchar = '\10760', commands = [("unicode-math","\\disjquant")], category = Op, comments = "TWO LOGICAL OR OPERATOR"}
  , Record {uchar = '\10761', commands = [("txfonts","\\varprod"),("unicode-math","\\bigtimes")], category = Op, comments = "N-ARY TIMES OPERATOR"}
  , Record {uchar = '\10762', commands = [("unicode-math","\\modtwosum")], category = Ord, comments = "MODULO TWO SUM"}
  , Record {uchar = '\10763', commands = [("unicode-math","\\sumint")], category = Op, comments = "SUMMATION WITH INTEGRAL"}
  , Record {uchar = '\10764', commands = [("amsmath","\\iiiint"),("esint","\\iiiint"),("unicode-math","\\iiiint")], category = Op, comments = "QUADRUPLE INTEGRAL OPERATOR"}
  , Record {uchar = '\10765', commands = [("unicode-math","\\intbar")], category = Op, comments = "FINITE PART INTEGRAL"}
  , Record {uchar = '\10766', commands = [("unicode-math","\\intBar")], category = Op, comments = "INTEGRAL WITH DOUBLE STROKE"}
  , Record {uchar = '\10767', commands = [("esint","\\fint"),("wrisym","\\fint"),("unicode-math","\\fint")], category = Op, comments = "INTEGRAL AVERAGE WITH SLASH"}
  , Record {uchar = '\10768', commands = [("unicode-math","\\cirfnint")], category = Op, comments = "CIRCULATION FUNCTION"}
  , Record {uchar = '\10769', commands = [("unicode-math","\\awint")], category = Op, comments = "ANTICLOCKWISE INTEGRATION"}
  , Record {uchar = '\10770', commands = [("unicode-math","\\rppolint")], category = Op, comments = "LINE INTEGRATION WITH RECTANGULAR PATH AROUND POLE"}
  , Record {uchar = '\10771', commands = [("unicode-math","\\scpolint")], category = Op, comments = "LINE INTEGRATION WITH SEMICIRCULAR PATH AROUND POLE"}
  , Record {uchar = '\10772', commands = [("unicode-math","\\npolint")], category = Op, comments = "LINE INTEGRATION NOT INCLUDING THE POLE"}
  , Record {uchar = '\10773', commands = [("unicode-math","\\pointint")], category = Op, comments = "INTEGRAL AROUND A POINT OPERATOR"}
  , Record {uchar = '\10774', commands = [("esint","\\sqint"),("wrisym","\\sqrint"),("unicode-math","\\sqint")], category = Op, comments = "QUATERNION INTEGRAL OPERATOR"}
  , Record {uchar = '\10775', commands = [("unicode-math","\\intlarhk")], category = Op, comments = "INTEGRAL WITH LEFTWARDS ARROW WITH HOOK"}
  , Record {uchar = '\10776', commands = [("unicode-math","\\intx")], category = Op, comments = "INTEGRAL WITH TIMES SIGN"}
  , Record {uchar = '\10777', commands = [("unicode-math","\\intcap")], category = Op, comments = "INTEGRAL WITH INTERSECTION"}
  , Record {uchar = '\10778', commands = [("unicode-math","\\intcup")], category = Op, comments = "INTEGRAL WITH UNION"}
  , Record {uchar = '\10779', commands = [("unicode-math","\\upint")], category = Op, comments = "INTEGRAL WITH OVERBAR"}
  , Record {uchar = '\10780', commands = [("unicode-math","\\lowint")], category = Op, comments = "INTEGRAL WITH UNDERBAR"}
  , Record {uchar = '\10781', commands = [("amssymb","\\Join"),("unicode-math","\\Join")], category = Op, comments = "JOIN"}
  , Record {uchar = '\10782', commands = [("unicode-math","\\bigtriangleleft")], category = Op, comments = "LARGE LEFT TRIANGLE OPERATOR"}
  , Record {uchar = '\10783', commands = [("oz","\\zcmp"),("oz","\\semi"),("unicode-math","\\zcmp")], category = Op, comments = "= \\fatsemi (stmaryrd), Z NOTATION SCHEMA COMPOSITION"}
  , Record {uchar = '\10784', commands = [("oz","\\zpipe"),("unicode-math","\\zpipe")], category = Op, comments = "Z NOTATION SCHEMA PIPING"}
  , Record {uchar = '\10785', commands = [("oz","\\zproject"),("oz","\\project"),("unicode-math","\\zproject")], category = Op, comments = "Z NOTATION SCHEMA PROJECTION"}
  , Record {uchar = '\10786', commands = [("unicode-math","\\ringplus")], category = Bin, comments = "PLUS SIGN WITH SMALL CIRCLE ABOVE"}
  , Record {uchar = '\10787', commands = [("unicode-math","\\plushat")], category = Bin, comments = "PLUS SIGN WITH CIRCUMFLEX ACCENT ABOVE"}
  , Record {uchar = '\10788', commands = [("unicode-math","\\simplus")], category = Bin, comments = "PLUS SIGN WITH TILDE ABOVE"}
  , Record {uchar = '\10789', commands = [("unicode-math","\\plusdot")], category = Bin, comments = "PLUS SIGN WITH DOT BELOW"}
  , Record {uchar = '\10790', commands = [("unicode-math","\\plussim")], category = Bin, comments = "PLUS SIGN WITH TILDE BELOW"}
  , Record {uchar = '\10791', commands = [("unicode-math","\\plussubtwo")], category = Bin, comments = "PLUS SIGN WITH SUBSCRIPT TWO"}
  , Record {uchar = '\10792', commands = [("unicode-math","\\plustrif")], category = Bin, comments = "PLUS SIGN WITH BLACK TRIANGLE"}
  , Record {uchar = '\10793', commands = [("unicode-math","\\commaminus")], category = Bin, comments = "MINUS SIGN WITH COMMA ABOVE"}
  , Record {uchar = '\10794', commands = [("unicode-math","\\minusdot")], category = Bin, comments = "MINUS SIGN WITH DOT BELOW"}
  , Record {uchar = '\10795', commands = [("unicode-math","\\minusfdots")], category = Bin, comments = "MINUS SIGN WITH FALLING DOTS"}
  , Record {uchar = '\10796', commands = [("unicode-math","\\minusrdots")], category = Bin, comments = "MINUS SIGN WITH RISING DOTS"}
  , Record {uchar = '\10797', commands = [("unicode-math","\\opluslhrim")], category = Bin, comments = "PLUS SIGN IN LEFT HALF CIRCLE"}
  , Record {uchar = '\10798', commands = [("unicode-math","\\oplusrhrim")], category = Bin, comments = "PLUS SIGN IN RIGHT HALF CIRCLE"}
  , Record {uchar = '\10799', commands = [("base","\\times"),("unicode-math","\\vectimes")], category = Bin, comments = "VECTOR OR CROSS PRODUCT"}
  , Record {uchar = '\10800', commands = [("unicode-math","\\dottimes")], category = Bin, comments = "MULTIPLICATION SIGN WITH DOT ABOVE"}
  , Record {uchar = '\10801', commands = [("unicode-math","\\timesbar")], category = Bin, comments = "MULTIPLICATION SIGN WITH UNDERBAR"}
  , Record {uchar = '\10802', commands = [("unicode-math","\\btimes")], category = Bin, comments = "SEMIDIRECT PRODUCT WITH BOTTOM CLOSED"}
  , Record {uchar = '\10803', commands = [("unicode-math","\\smashtimes")], category = Bin, comments = "SMASH PRODUCT"}
  , Record {uchar = '\10804', commands = [("unicode-math","\\otimeslhrim")], category = Bin, comments = "MULTIPLICATION SIGN IN LEFT HALF CIRCLE"}
  , Record {uchar = '\10805', commands = [("unicode-math","\\otimesrhrim")], category = Bin, comments = "MULTIPLICATION SIGN IN RIGHT HALF CIRCLE"}
  , Record {uchar = '\10806', commands = [("unicode-math","\\otimeshat")], category = Bin, comments = "CIRCLED MULTIPLICATION SIGN WITH CIRCUMFLEX ACCENT"}
  , Record {uchar = '\10807', commands = [("unicode-math","\\Otimes")], category = Bin, comments = "MULTIPLICATION SIGN IN DOUBLE CIRCLE"}
  , Record {uchar = '\10808', commands = [("unicode-math","\\odiv")], category = Bin, comments = "CIRCLED DIVISION SIGN"}
  , Record {uchar = '\10809', commands = [("unicode-math","\\triangleplus")], category = Bin, comments = "PLUS SIGN IN TRIANGLE"}
  , Record {uchar = '\10810', commands = [("unicode-math","\\triangleminus")], category = Bin, comments = "MINUS SIGN IN TRIANGLE"}
  , Record {uchar = '\10811', commands = [("unicode-math","\\triangletimes")], category = Bin, comments = "MULTIPLICATION SIGN IN TRIANGLE"}
  , Record {uchar = '\10812', commands = [("unicode-math","\\intprod")], category = Bin, comments = "INTERIOR PRODUCT"}
  , Record {uchar = '\10813', commands = [("unicode-math","\\intprodr")], category = Bin, comments = "RIGHTHAND INTERIOR PRODUCT"}
  , Record {uchar = '\10814', commands = [("oz","\\fcmp"),("oz","\\comp"),("unicode-math","\\fcmp")], category = Bin, comments = "Z NOTATION RELATIONAL COMPOSITION"}
  , Record {uchar = '\10815', commands = [("base","\\amalg"),("unicode-math","\\amalg")], category = Bin, comments = "AMALGAMATION OR COPRODUCT"}
  , Record {uchar = '\10816', commands = [("unicode-math","\\capdot")], category = Bin, comments = "INTERSECTION WITH DOT"}
  , Record {uchar = '\10817', commands = [("unicode-math","\\uminus")], category = Bin, comments = "UNION WITH MINUS SIGN, z notation bag subtraction"}
  , Record {uchar = '\10818', commands = [("unicode-math","\\barcup")], category = Bin, comments = "UNION WITH OVERBAR"}
  , Record {uchar = '\10819', commands = [("unicode-math","\\barcap")], category = Bin, comments = "INTERSECTION WITH OVERBAR"}
  , Record {uchar = '\10820', commands = [("unicode-math","\\capwedge")], category = Bin, comments = "INTERSECTION WITH LOGICAL AND"}
  , Record {uchar = '\10821', commands = [("unicode-math","\\cupvee")], category = Bin, comments = "UNION WITH LOGICAL OR"}
  , Record {uchar = '\10822', commands = [("unicode-math","\\cupovercap")], category = Bin, comments = "UNION ABOVE INTERSECTION"}
  , Record {uchar = '\10823', commands = [("unicode-math","\\capovercup")], category = Bin, comments = "INTERSECTION ABOVE UNION"}
  , Record {uchar = '\10824', commands = [("unicode-math","\\cupbarcap")], category = Bin, comments = "UNION ABOVE BAR ABOVE INTERSECTION"}
  , Record {uchar = '\10825', commands = [("unicode-math","\\capbarcup")], category = Bin, comments = "INTERSECTION ABOVE BAR ABOVE UNION"}
  , Record {uchar = '\10826', commands = [("unicode-math","\\twocups")], category = Bin, comments = "UNION BESIDE AND JOINED WITH UNION"}
  , Record {uchar = '\10827', commands = [("unicode-math","\\twocaps")], category = Bin, comments = "INTERSECTION BESIDE AND JOINED WITH INTERSECTION"}
  , Record {uchar = '\10828', commands = [("unicode-math","\\closedvarcup")], category = Bin, comments = "CLOSED UNION WITH SERIFS"}
  , Record {uchar = '\10829', commands = [("unicode-math","\\closedvarcap")], category = Bin, comments = "CLOSED INTERSECTION WITH SERIFS"}
  , Record {uchar = '\10830', commands = [("unicode-math","\\Sqcap")], category = Bin, comments = "DOUBLE SQUARE INTERSECTION"}
  , Record {uchar = '\10831', commands = [("unicode-math","\\Sqcup")], category = Bin, comments = "DOUBLE SQUARE UNION"}
  , Record {uchar = '\10832', commands = [("unicode-math","\\closedvarcupsmashprod")], category = Bin, comments = "CLOSED UNION WITH SERIFS AND SMASH PRODUCT"}
  , Record {uchar = '\10833', commands = [("unicode-math","\\wedgeodot")], category = Bin, comments = "LOGICAL AND WITH DOT ABOVE"}
  , Record {uchar = '\10834', commands = [("unicode-math","\\veeodot")], category = Bin, comments = "LOGICAL OR WITH DOT ABOVE"}
  , Record {uchar = '\10835', commands = [("unicode-math","\\Wedge")], category = Bin, comments = "DOUBLE LOGICAL AND"}
  , Record {uchar = '\10836', commands = [("unicode-math","\\Vee")], category = Bin, comments = "DOUBLE LOGICAL OR"}
  , Record {uchar = '\10837', commands = [("unicode-math","\\wedgeonwedge")], category = Bin, comments = "TWO INTERSECTING LOGICAL AND"}
  , Record {uchar = '\10838', commands = [("unicode-math","\\veeonvee")], category = Bin, comments = "TWO INTERSECTING LOGICAL OR"}
  , Record {uchar = '\10839', commands = [("unicode-math","\\bigslopedvee")], category = Bin, comments = "SLOPING LARGE OR"}
  , Record {uchar = '\10840', commands = [("unicode-math","\\bigslopedwedge")], category = Bin, comments = "SLOPING LARGE AND"}
  , Record {uchar = '\10841', commands = [("unicode-math","\\veeonwedge")], category = Rel, comments = "LOGICAL OR OVERLAPPING LOGICAL AND"}
  , Record {uchar = '\10842', commands = [("unicode-math","\\wedgemidvert")], category = Bin, comments = "LOGICAL AND WITH MIDDLE STEM"}
  , Record {uchar = '\10843', commands = [("unicode-math","\\veemidvert")], category = Bin, comments = "LOGICAL OR WITH MIDDLE STEM"}
  , Record {uchar = '\10844', commands = [("unicode-math","\\midbarwedge")], category = Bin, comments = "ogical and with horizontal dash"}
  , Record {uchar = '\10845', commands = [("unicode-math","\\midbarvee")], category = Bin, comments = "LOGICAL OR WITH HORIZONTAL DASH"}
  , Record {uchar = '\10846', commands = [("amssymb","\\doublebarwedge"),("unicode-math","\\doublebarwedge")], category = Bin, comments = "LOGICAL AND WITH DOUBLE OVERBAR"}
  , Record {uchar = '\10847', commands = [("unicode-math","\\wedgebar")], category = Bin, comments = "LOGICAL AND WITH UNDERBAR"}
  , Record {uchar = '\10848', commands = [("unicode-math","\\wedgedoublebar")], category = Bin, comments = "LOGICAL AND WITH DOUBLE UNDERBAR"}
  , Record {uchar = '\10849', commands = [("unicode-math","\\varveebar")], category = Bin, comments = "SMALL VEE WITH UNDERBAR"}
  , Record {uchar = '\10850', commands = [("unicode-math","\\doublebarvee")], category = Bin, comments = "LOGICAL OR WITH DOUBLE OVERBAR"}
  , Record {uchar = '\10851', commands = [("unicode-math","\\veedoublebar")], category = Bin, comments = "LOGICAL OR WITH DOUBLE UNDERBAR"}
  , Record {uchar = '\10852', commands = [("oz","\\dsub"),("oz","\\ndres"),("unicode-math","\\dsub")], category = Bin, comments = "Z NOTATION DOMAIN ANTIRESTRICTION"}
  , Record {uchar = '\10853', commands = [("oz","\\rsub"),("oz","\\nrres"),("unicode-math","\\rsub")], category = Bin, comments = "Z NOTATION RANGE ANTIRESTRICTION"}
  , Record {uchar = '\10854', commands = [("unicode-math","\\eqdot")], category = Rel, comments = "EQUALS SIGN WITH DOT BELOW"}
  , Record {uchar = '\10855', commands = [("unicode-math","\\dotequiv")], category = Rel, comments = "IDENTICAL WITH DOT ABOVE"}
  , Record {uchar = '\10856', commands = [("unicode-math","\\equivVert")], category = Rel, comments = "TRIPLE HORIZONTAL BAR WITH DOUBLE VERTICAL STROKE"}
  , Record {uchar = '\10857', commands = [("unicode-math","\\equivVvert")], category = Rel, comments = "TRIPLE HORIZONTAL BAR WITH TRIPLE VERTICAL STROKE"}
  , Record {uchar = '\10858', commands = [("unicode-math","\\dotsim")], category = Rel, comments = "TILDE OPERATOR WITH DOT ABOVE"}
  , Record {uchar = '\10859', commands = [("unicode-math","\\simrdots")], category = Rel, comments = "TILDE OPERATOR WITH RISING DOTS"}
  , Record {uchar = '\10860', commands = [("unicode-math","\\simminussim")], category = Rel, comments = "SIMILAR MINUS SIMILAR"}
  , Record {uchar = '\10861', commands = [("unicode-math","\\congdot")], category = Rel, comments = "CONGRUENT WITH DOT ABOVE"}
  , Record {uchar = '\10862', commands = [("unicode-math","\\asteq")], category = Rel, comments = "EQUALS WITH ASTERISK"}
  , Record {uchar = '\10863', commands = [("unicode-math","\\hatapprox")], category = Rel, comments = "ALMOST EQUAL TO WITH CIRCUMFLEX ACCENT"}
  , Record {uchar = '\10864', commands = [("unicode-math","\\approxeqq")], category = Rel, comments = "APPROXIMATELY EQUAL OR EQUAL TO"}
  , Record {uchar = '\10865', commands = [("unicode-math","\\eqqplus")], category = Bin, comments = "EQUALS SIGN ABOVE PLUS SIGN"}
  , Record {uchar = '\10866', commands = [("unicode-math","\\pluseqq")], category = Bin, comments = "PLUS SIGN ABOVE EQUALS SIGN"}
  , Record {uchar = '\10867', commands = [("unicode-math","\\eqqsim")], category = Rel, comments = "EQUALS SIGN ABOVE TILDE OPERATOR"}
  , Record {uchar = '\10868', commands = [("txfonts","\\Coloneqq"),("base","::="),("unicode-math","\\Coloneq")], category = Rel, comments = "x \\Coloneq (txfonts), DOUBLE COLON EQUAL"}
  , Record {uchar = '\10869', commands = [("wrisym","\\Equal"),("base","=="),("unicode-math","\\eqeq")], category = Rel, comments = "TWO CONSECUTIVE EQUALS SIGNS"}
  , Record {uchar = '\10870', commands = [("wrisym","\\Same"),("base","==="),("unicode-math","\\eqeqeq")], category = Rel, comments = "THREE CONSECUTIVE EQUALS SIGNS"}
  , Record {uchar = '\10871', commands = [("unicode-math","\\ddotseq")], category = Rel, comments = "EQUALS SIGN WITH TWO DOTS ABOVE AND TWO DOTS BELOW"}
  , Record {uchar = '\10872', commands = [("unicode-math","\\equivDD")], category = Rel, comments = "EQUIVALENT WITH FOUR DOTS ABOVE"}
  , Record {uchar = '\10873', commands = [("unicode-math","\\ltcir")], category = Rel, comments = "LESS-THAN WITH CIRCLE INSIDE"}
  , Record {uchar = '\10874', commands = [("unicode-math","\\gtcir")], category = Rel, comments = "GREATER-THAN WITH CIRCLE INSIDE"}
  , Record {uchar = '\10875', commands = [("unicode-math","\\ltquest")], category = Rel, comments = "LESS-THAN WITH QUESTION MARK ABOVE"}
  , Record {uchar = '\10876', commands = [("unicode-math","\\gtquest")], category = Rel, comments = "GREATER-THAN WITH QUESTION MARK ABOVE"}
  , Record {uchar = '\10877', commands = [("amssymb","\\leqslant"),("fourier","\\leqslant"),("unicode-math","\\leqslant")], category = Rel, comments = "LESS-THAN OR SLANTED EQUAL TO"}
  , Record {uchar = '\10878', commands = [("amssymb","\\geqslant"),("fourier","\\geqslant"),("unicode-math","\\geqslant")], category = Rel, comments = "GREATER-THAN OR SLANTED EQUAL TO"}
  , Record {uchar = '\10879', commands = [("unicode-math","\\lesdot")], category = Rel, comments = "LESS-THAN OR SLANTED EQUAL TO WITH DOT INSIDE"}
  , Record {uchar = '\10880', commands = [("unicode-math","\\gesdot")], category = Rel, comments = "GREATER-THAN OR SLANTED EQUAL TO WITH DOT INSIDE"}
  , Record {uchar = '\10881', commands = [("unicode-math","\\lesdoto")], category = Rel, comments = "LESS-THAN OR SLANTED EQUAL TO WITH DOT ABOVE"}
  , Record {uchar = '\10882', commands = [("unicode-math","\\gesdoto")], category = Rel, comments = "GREATER-THAN OR SLANTED EQUAL TO WITH DOT ABOVE"}
  , Record {uchar = '\10883', commands = [("unicode-math","\\lesdotor")], category = Rel, comments = "LESS-THAN OR SLANTED EQUAL TO WITH DOT ABOVE RIGHT"}
  , Record {uchar = '\10884', commands = [("unicode-math","\\gesdotol")], category = Rel, comments = "GREATER-THAN OR SLANTED EQUAL TO WITH DOT ABOVE LEFT"}
  , Record {uchar = '\10885', commands = [("amssymb","\\lessapprox"),("unicode-math","\\lessapprox")], category = Rel, comments = "LESS-THAN OR APPROXIMATE"}
  , Record {uchar = '\10886', commands = [("amssymb","\\gtrapprox"),("unicode-math","\\gtrapprox")], category = Rel, comments = "GREATER-THAN OR APPROXIMATE"}
  , Record {uchar = '\10887', commands = [("amssymb","\\lneq"),("unicode-math","\\lneq")], category = Rel, comments = "LESS-THAN AND SINGLE-LINE NOT EQUAL TO"}
  , Record {uchar = '\10888', commands = [("amssymb","\\gneq"),("unicode-math","\\gneq")], category = Rel, comments = "GREATER-THAN AND SINGLE-LINE NOT EQUAL TO"}
  , Record {uchar = '\10889', commands = [("amssymb","\\lnapprox"),("unicode-math","\\lnapprox")], category = Rel, comments = "LESS-THAN AND NOT APPROXIMATE"}
  , Record {uchar = '\10890', commands = [("amssymb","\\gnapprox"),("unicode-math","\\gnapprox")], category = Rel, comments = "GREATER-THAN AND NOT APPROXIMATE"}
  , Record {uchar = '\10891', commands = [("amssymb","\\lesseqqgtr"),("unicode-math","\\lesseqqgtr")], category = Rel, comments = "LESS-THAN ABOVE DOUBLE-LINE EQUAL ABOVE GREATER-THAN"}
  , Record {uchar = '\10892', commands = [("amssymb","\\gtreqqless"),("unicode-math","\\gtreqqless")], category = Rel, comments = "GREATER-THAN ABOVE DOUBLE-LINE EQUAL ABOVE LESS-THAN"}
  , Record {uchar = '\10893', commands = [("unicode-math","\\lsime")], category = Rel, comments = "LESS-THAN ABOVE SIMILAR OR EQUAL"}
  , Record {uchar = '\10894', commands = [("unicode-math","\\gsime")], category = Rel, comments = "GREATER-THAN ABOVE SIMILAR OR EQUAL"}
  , Record {uchar = '\10895', commands = [("unicode-math","\\lsimg")], category = Rel, comments = "LESS-THAN ABOVE SIMILAR ABOVE GREATER-THAN"}
  , Record {uchar = '\10896', commands = [("unicode-math","\\gsiml")], category = Rel, comments = "GREATER-THAN ABOVE SIMILAR ABOVE LESS-THAN"}
  , Record {uchar = '\10897', commands = [("unicode-math","\\lgE")], category = Rel, comments = "LESS-THAN ABOVE GREATER-THAN ABOVE DOUBLE-LINE EQUAL"}
  , Record {uchar = '\10898', commands = [("unicode-math","\\glE")], category = Rel, comments = "GREATER-THAN ABOVE LESS-THAN ABOVE DOUBLE-LINE EQUAL"}
  , Record {uchar = '\10899', commands = [("unicode-math","\\lesges")], category = Rel, comments = "LESS-THAN ABOVE SLANTED EQUAL ABOVE GREATER-THAN ABOVE SLANTED EQUAL"}
  , Record {uchar = '\10900', commands = [("unicode-math","\\gesles")], category = Rel, comments = "GREATER-THAN ABOVE SLANTED EQUAL ABOVE LESS-THAN ABOVE SLANTED EQUAL"}
  , Record {uchar = '\10901', commands = [("amssymb","\\eqslantless"),("unicode-math","\\eqslantless")], category = Rel, comments = "SLANTED EQUAL TO OR LESS-THAN"}
  , Record {uchar = '\10902', commands = [("amssymb","\\eqslantgtr"),("unicode-math","\\eqslantgtr")], category = Rel, comments = "SLANTED EQUAL TO OR GREATER-THAN"}
  , Record {uchar = '\10903', commands = [("unicode-math","\\elsdot")], category = Rel, comments = "SLANTED EQUAL TO OR LESS-THAN WITH DOT INSIDE"}
  , Record {uchar = '\10904', commands = [("unicode-math","\\egsdot")], category = Rel, comments = "SLANTED EQUAL TO OR GREATER-THAN WITH DOT INSIDE"}
  , Record {uchar = '\10905', commands = [("unicode-math","\\eqqless")], category = Rel, comments = "DOUBLE-LINE EQUAL TO OR LESS-THAN"}
  , Record {uchar = '\10906', commands = [("unicode-math","\\eqqgtr")], category = Rel, comments = "DOUBLE-LINE EQUAL TO OR GREATER-THAN"}
  , Record {uchar = '\10907', commands = [("unicode-math","\\eqqslantless")], category = Rel, comments = "DOUBLE-LINE SLANTED EQUAL TO OR LESS-THAN"}
  , Record {uchar = '\10908', commands = [("unicode-math","\\eqqslantgtr")], category = Rel, comments = "DOUBLE-LINE SLANTED EQUAL TO OR GREATER-THAN"}
  , Record {uchar = '\10909', commands = [("unicode-math","\\simless")], category = Rel, comments = "SIMILAR OR LESS-THAN"}
  , Record {uchar = '\10910', commands = [("unicode-math","\\simgtr")], category = Rel, comments = "SIMILAR OR GREATER-THAN"}
  , Record {uchar = '\10911', commands = [("unicode-math","\\simlE")], category = Rel, comments = "SIMILAR ABOVE LESS-THAN ABOVE EQUALS SIGN"}
  , Record {uchar = '\10912', commands = [("unicode-math","\\simgE")], category = Rel, comments = "SIMILAR ABOVE GREATER-THAN ABOVE EQUALS SIGN"}
  , Record {uchar = '\10913', commands = [("wrisym","\\NestedLessLess"),("mathabx -amssymb","\\lll"),("unicode-math","\\Lt")], category = Rel, comments = "DOUBLE NESTED LESS-THAN"}
  , Record {uchar = '\10914', commands = [("wrisym","\\NestedGreaterGreater"),("mathabx -amssymb","\\ggg"),("unicode-math","\\Gt")], category = Rel, comments = "DOUBLE NESTED GREATER-THAN"}
  , Record {uchar = '\10915', commands = [("unicode-math","\\partialmeetcontraction")], category = Rel, comments = "double less-than with underbar"}
  , Record {uchar = '\10916', commands = [("unicode-math","\\glj")], category = Rel, comments = "GREATER-THAN OVERLAPPING LESS-THAN"}
  , Record {uchar = '\10917', commands = [("unicode-math","\\gla")], category = Rel, comments = "GREATER-THAN BESIDE LESS-THAN"}
  , Record {uchar = '\10918', commands = [("stmaryrd","\\leftslice"),("unicode-math","\\ltcc")], category = Rel, comments = "LESS-THAN CLOSED BY CURVE"}
  , Record {uchar = '\10919', commands = [("stmaryrd","\\rightslice"),("unicode-math","\\gtcc")], category = Rel, comments = "GREATER-THAN CLOSED BY CURVE"}
  , Record {uchar = '\10920', commands = [("unicode-math","\\lescc")], category = Rel, comments = "LESS-THAN CLOSED BY CURVE ABOVE SLANTED EQUAL"}
  , Record {uchar = '\10921', commands = [("unicode-math","\\gescc")], category = Rel, comments = "GREATER-THAN CLOSED BY CURVE ABOVE SLANTED EQUAL"}
  , Record {uchar = '\10922', commands = [("unicode-math","\\smt")], category = Rel, comments = "SMALLER THAN"}
  , Record {uchar = '\10923', commands = [("unicode-math","\\lat")], category = Rel, comments = "LARGER THAN"}
  , Record {uchar = '\10924', commands = [("unicode-math","\\smte")], category = Rel, comments = "SMALLER THAN OR EQUAL TO"}
  , Record {uchar = '\10925', commands = [("unicode-math","\\late")], category = Rel, comments = "LARGER THAN OR EQUAL TO"}
  , Record {uchar = '\10926', commands = [("unicode-math","\\bumpeqq")], category = Rel, comments = "EQUALS SIGN WITH BUMPY ABOVE"}
  , Record {uchar = '\10927', commands = [("base","\\preceq"),("unicode-math","\\preceq")], category = Rel, comments = "PRECEDES ABOVE SINGLE-LINE EQUALS SIGN"}
  , Record {uchar = '\10928', commands = [("base","\\succeq"),("unicode-math","\\succeq")], category = Rel, comments = "SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN"}
  , Record {uchar = '\10929', commands = [("unicode-math","\\precneq")], category = Rel, comments = "PRECEDES ABOVE SINGLE-LINE NOT EQUAL TO"}
  , Record {uchar = '\10930', commands = [("unicode-math","\\succneq")], category = Rel, comments = "SUCCEEDS ABOVE SINGLE-LINE NOT EQUAL TO"}
  , Record {uchar = '\10931', commands = [("txfonts","\\preceqq"),("unicode-math","\\preceqq")], category = Rel, comments = "PRECEDES ABOVE EQUALS SIGN"}
  , Record {uchar = '\10932', commands = [("txfonts","\\succeqq"),("unicode-math","\\succeqq")], category = Rel, comments = "SUCCEEDS ABOVE EQUALS SIGN"}
  , Record {uchar = '\10933', commands = [("unicode-math","\\precneqq")], category = Rel, comments = "PRECEDES ABOVE NOT EQUAL TO"}
  , Record {uchar = '\10934', commands = [("unicode-math","\\succneqq")], category = Rel, comments = "SUCCEEDS ABOVE NOT EQUAL TO"}
  , Record {uchar = '\10935', commands = [("amssymb","\\precapprox"),("unicode-math","\\precapprox")], category = Rel, comments = "PRECEDES ABOVE ALMOST EQUAL TO"}
  , Record {uchar = '\10936', commands = [("amssymb","\\succapprox"),("unicode-math","\\succapprox")], category = Rel, comments = "SUCCEEDS ABOVE ALMOST EQUAL TO"}
  , Record {uchar = '\10937', commands = [("amssymb","\\precnapprox"),("unicode-math","\\precnapprox")], category = Rel, comments = "PRECEDES ABOVE NOT ALMOST EQUAL TO"}
  , Record {uchar = '\10938', commands = [("amssymb","\\succnapprox"),("unicode-math","\\succnapprox")], category = Rel, comments = "SUCCEEDS ABOVE NOT ALMOST EQUAL TO"}
  , Record {uchar = '\10939', commands = [("mathabx","\\llcurly"),("unicode-math","\\Prec")], category = Rel, comments = "DOUBLE PRECEDES"}
  , Record {uchar = '\10940', commands = [("mathabx","\\ggcurly"),("unicode-math","\\Succ")], category = Rel, comments = "DOUBLE SUCCEEDS"}
  , Record {uchar = '\10941', commands = [("unicode-math","\\subsetdot")], category = Rel, comments = "SUBSET WITH DOT"}
  , Record {uchar = '\10942', commands = [("unicode-math","\\supsetdot")], category = Rel, comments = "SUPERSET WITH DOT"}
  , Record {uchar = '\10943', commands = [("unicode-math","\\subsetplus")], category = Rel, comments = "SUBSET WITH PLUS SIGN BELOW"}
  , Record {uchar = '\10944', commands = [("unicode-math","\\supsetplus")], category = Rel, comments = "SUPERSET WITH PLUS SIGN BELOW"}
  , Record {uchar = '\10945', commands = [("unicode-math","\\submult")], category = Rel, comments = "SUBSET WITH MULTIPLICATION SIGN BELOW"}
  , Record {uchar = '\10946', commands = [("unicode-math","\\supmult")], category = Rel, comments = "SUPERSET WITH MULTIPLICATION SIGN BELOW"}
  , Record {uchar = '\10947', commands = [("unicode-math","\\subedot")], category = Rel, comments = "SUBSET OF OR EQUAL TO WITH DOT ABOVE"}
  , Record {uchar = '\10948', commands = [("unicode-math","\\supedot")], category = Rel, comments = "SUPERSET OF OR EQUAL TO WITH DOT ABOVE"}
  , Record {uchar = '\10949', commands = [("amssymb","\\subseteqq"),("unicode-math","\\subseteqq")], category = Rel, comments = "SUBSET OF ABOVE EQUALS SIGN"}
  , Record {uchar = '\10950', commands = [("amssymb","\\supseteqq"),("unicode-math","\\supseteqq")], category = Rel, comments = "SUPERSET OF ABOVE EQUALS SIGN"}
  , Record {uchar = '\10951', commands = [("unicode-math","\\subsim")], category = Rel, comments = "SUBSET OF ABOVE TILDE OPERATOR"}
  , Record {uchar = '\10952', commands = [("unicode-math","\\supsim")], category = Rel, comments = "SUPERSET OF ABOVE TILDE OPERATOR"}
  , Record {uchar = '\10953', commands = [("unicode-math","\\subsetapprox")], category = Rel, comments = "SUBSET OF ABOVE ALMOST EQUAL TO"}
  , Record {uchar = '\10954', commands = [("unicode-math","\\supsetapprox")], category = Rel, comments = "SUPERSET OF ABOVE ALMOST EQUAL TO"}
  , Record {uchar = '\10955', commands = [("amssymb","\\subsetneqq"),("unicode-math","\\subsetneqq")], category = Rel, comments = "SUBSET OF ABOVE NOT EQUAL TO"}
  , Record {uchar = '\10956', commands = [("amssymb","\\supsetneqq"),("unicode-math","\\supsetneqq")], category = Rel, comments = "SUPERSET OF ABOVE NOT EQUAL TO"}
  , Record {uchar = '\10957', commands = [("unicode-math","\\lsqhook")], category = Rel, comments = "SQUARE LEFT OPEN BOX OPERATOR"}
  , Record {uchar = '\10958', commands = [("unicode-math","\\rsqhook")], category = Rel, comments = "SQUARE RIGHT OPEN BOX OPERATOR"}
  , Record {uchar = '\10959', commands = [("unicode-math","\\csub")], category = Rel, comments = "CLOSED SUBSET"}
  , Record {uchar = '\10960', commands = [("unicode-math","\\csup")], category = Rel, comments = "CLOSED SUPERSET"}
  , Record {uchar = '\10961', commands = [("unicode-math","\\csube")], category = Rel, comments = "CLOSED SUBSET OR EQUAL TO"}
  , Record {uchar = '\10962', commands = [("unicode-math","\\csupe")], category = Rel, comments = "CLOSED SUPERSET OR EQUAL TO"}
  , Record {uchar = '\10963', commands = [("unicode-math","\\subsup")], category = Rel, comments = "SUBSET ABOVE SUPERSET"}
  , Record {uchar = '\10964', commands = [("unicode-math","\\supsub")], category = Rel, comments = "SUPERSET ABOVE SUBSET"}
  , Record {uchar = '\10965', commands = [("unicode-math","\\subsub")], category = Rel, comments = "SUBSET ABOVE SUBSET"}
  , Record {uchar = '\10966', commands = [("unicode-math","\\supsup")], category = Rel, comments = "SUPERSET ABOVE SUPERSET"}
  , Record {uchar = '\10967', commands = [("unicode-math","\\suphsub")], category = Rel, comments = "SUPERSET BESIDE SUBSET"}
  , Record {uchar = '\10968', commands = [("unicode-math","\\supdsub")], category = Rel, comments = "SUPERSET BESIDE AND JOINED BY DASH WITH SUBSET"}
  , Record {uchar = '\10969', commands = [("unicode-math","\\forkv")], category = Rel, comments = "ELEMENT OF OPENING DOWNWARDS"}
  , Record {uchar = '\10970', commands = [("unicode-math","\\topfork")], category = Rel, comments = "PITCHFORK WITH TEE TOP"}
  , Record {uchar = '\10971', commands = [("unicode-math","\\mlcp")], category = Rel, comments = "TRANSVERSAL INTERSECTION"}
  , Record {uchar = '\10972', commands = [("unicode-math","\\forks")], category = Rel, comments = "FORKING"}
  , Record {uchar = '\10973', commands = [("unicode-math","\\forksnot")], category = Rel, comments = "NONFORKING"}
  , Record {uchar = '\10974', commands = [("unicode-math","\\shortlefttack")], category = Rel, comments = "SHORT LEFT TACK"}
  , Record {uchar = '\10975', commands = [("unicode-math","\\shortdowntack")], category = Rel, comments = "SHORT DOWN TACK"}
  , Record {uchar = '\10976', commands = [("unicode-math","\\shortuptack")], category = Rel, comments = "SHORT UP TACK"}
  , Record {uchar = '\10977', commands = [("unicode-math","\\perps")], category = Ord, comments = "PERPENDICULAR WITH S"}
  , Record {uchar = '\10978', commands = [("unicode-math","\\vDdash")], category = Rel, comments = "VERTICAL BAR TRIPLE RIGHT TURNSTILE"}
  , Record {uchar = '\10979', commands = [("unicode-math","\\dashV")], category = Rel, comments = "DOUBLE VERTICAL BAR LEFT TURNSTILE"}
  , Record {uchar = '\10980', commands = [("unicode-math","\\Dashv")], category = Rel, comments = "VERTICAL BAR DOUBLE LEFT TURNSTILE"}
  , Record {uchar = '\10981', commands = [("unicode-math","\\DashV")], category = Rel, comments = "DOUBLE VERTICAL BAR DOUBLE LEFT TURNSTILE"}
  , Record {uchar = '\10982', commands = [("unicode-math","\\varVdash")], category = Rel, comments = "LONG DASH FROM LEFT MEMBER OF DOUBLE VERTICAL"}
  , Record {uchar = '\10983', commands = [("unicode-math","\\Barv")], category = Rel, comments = "SHORT DOWN TACK WITH OVERBAR"}
  , Record {uchar = '\10984', commands = [("unicode-math","\\vBar")], category = Rel, comments = "SHORT UP TACK WITH UNDERBAR"}
  , Record {uchar = '\10985', commands = [("unicode-math","\\vBarv")], category = Rel, comments = "SHORT UP TACK ABOVE SHORT DOWN TACK"}
  , Record {uchar = '\10986', commands = [("txfonts","\\Top"),("unicode-math","\\barV")], category = Rel, comments = "DOUBLE DOWN TACK"}
  , Record {uchar = '\10987', commands = [("txfonts","\\Bot"),("txfonts","\\Perp"),("unicode-math","\\Vbar")], category = Rel, comments = "DOUBLE UP TACK"}
  , Record {uchar = '\10988', commands = [("unicode-math","\\Not")], category = Rel, comments = "DOUBLE STROKE NOT SIGN"}
  , Record {uchar = '\10989', commands = [("unicode-math","\\bNot")], category = Rel, comments = "REVERSED DOUBLE STROKE NOT SIGN"}
  , Record {uchar = '\10990', commands = [("unicode-math","\\revnmid")], category = Rel, comments = "DOES NOT DIVIDE WITH REVERSED NEGATION SLASH"}
  , Record {uchar = '\10991', commands = [("unicode-math","\\cirmid")], category = Rel, comments = "VERTICAL LINE WITH CIRCLE ABOVE"}
  , Record {uchar = '\10992', commands = [("unicode-math","\\midcir")], category = Rel, comments = "VERTICAL LINE WITH CIRCLE BELOW"}
  , Record {uchar = '\10993', commands = [("unicode-math","\\topcir")], category = Ord, comments = "DOWN TACK WITH CIRCLE BELOW"}
  , Record {uchar = '\10994', commands = [("unicode-math","\\nhpar")], category = Rel, comments = "PARALLEL WITH HORIZONTAL STROKE"}
  , Record {uchar = '\10995', commands = [("unicode-math","\\parsim")], category = Rel, comments = "PARALLEL WITH TILDE OPERATOR"}
  , Record {uchar = '\10996', commands = [("stmaryrd","\\interleave"),("unicode-math","\\interleave")], category = Bin, comments = "TRIPLE VERTICAL BAR BINARY RELATION"}
  , Record {uchar = '\10997', commands = [("unicode-math","\\nhVvert")], category = Bin, comments = "TRIPLE VERTICAL BAR WITH HORIZONTAL STROKE"}
  , Record {uchar = '\10998', commands = [("unicode-math","\\threedotcolon")], category = Bin, comments = "TRIPLE COLON OPERATOR"}
  , Record {uchar = '\10999', commands = [("unicode-math","\\lllnest")], category = Rel, comments = "TRIPLE NESTED LESS-THAN"}
  , Record {uchar = '\11000', commands = [("unicode-math","\\gggnest")], category = Rel, comments = "TRIPLE NESTED GREATER-THAN"}
  , Record {uchar = '\11001', commands = [("unicode-math","\\leqqslant")], category = Rel, comments = "DOUBLE-LINE SLANTED LESS-THAN OR EQUAL TO"}
  , Record {uchar = '\11002', commands = [("unicode-math","\\geqqslant")], category = Rel, comments = "DOUBLE-LINE SLANTED GREATER-THAN OR EQUAL TO"}
  , Record {uchar = '\11003', commands = [("unicode-math","\\trslash")], category = Bin, comments = "TRIPLE SOLIDUS BINARY RELATION"}
  , Record {uchar = '\11004', commands = [("stmaryrd","\\biginterleave"),("unicode-math","\\biginterleave")], category = Op, comments = "LARGE TRIPLE VERTICAL BAR OPERATOR"}
  , Record {uchar = '\11005', commands = [("stmaryrd","\\sslash"),("txfonts","\\varparallel"),("unicode-math","\\sslash")], category = Bin, comments = "DOUBLE SOLIDUS OPERATOR"}
  , Record {uchar = '\11006', commands = [("stmaryrd","\\talloblong"),("unicode-math","\\talloblong")], category = Bin, comments = "WHITE VERTICAL BAR"}
  , Record {uchar = '\11007', commands = [("unicode-math","\\bigtalloblong")], category = Op, comments = "N-ARY WHITE VERTICAL BAR"}
  , Record {uchar = '\11008', commands = [], category = Ord, comments = "NORTH EAST WHITE ARROW"}
  , Record {uchar = '\11009', commands = [], category = Ord, comments = "NORTH WEST WHITE ARROW"}
  , Record {uchar = '\11010', commands = [], category = Ord, comments = "SOUTH EAST WHITE ARROW"}
  , Record {uchar = '\11011', commands = [], category = Ord, comments = "SOUTH WEST WHITE ARROW"}
  , Record {uchar = '\11012', commands = [], category = Ord, comments = "LEFT RIGHT WHITE ARROW"}
  , Record {uchar = '\11013', commands = [], category = Ord, comments = "LEFTWARDS BLACK ARROW"}
  , Record {uchar = '\11014', commands = [], category = Ord, comments = "UPWARDS BLACK ARROW"}
  , Record {uchar = '\11015', commands = [], category = Ord, comments = "DOWNWARDS BLACK ARROW"}
  , Record {uchar = '\11016', commands = [], category = Ord, comments = "NORTH EAST BLACK ARROW"}
  , Record {uchar = '\11017', commands = [], category = Ord, comments = "NORTH WEST BLACK ARROW"}
  , Record {uchar = '\11018', commands = [], category = Ord, comments = "SOUTH EAST BLACK ARROW"}
  , Record {uchar = '\11019', commands = [], category = Ord, comments = "SOUTH WEST BLACK ARROW"}
  , Record {uchar = '\11020', commands = [], category = Ord, comments = "LEFT RIGHT BLACK ARROW"}
  , Record {uchar = '\11021', commands = [], category = Ord, comments = "UP DOWN BLACK ARROW"}
  , Record {uchar = '\11022', commands = [], category = Ord, comments = "RIGHTWARDS ARROW WITH TIP DOWNWARDS"}
  , Record {uchar = '\11023', commands = [], category = Ord, comments = "RIGHTWARDS ARROW WITH TIP UPWARDS"}
  , Record {uchar = '\11024', commands = [], category = Ord, comments = "LEFTWARDS ARROW WITH TIP DOWNWARDS"}
  , Record {uchar = '\11025', commands = [], category = Ord, comments = "LEFTWARDS ARROW WITH TIP UPWARDS"}
  , Record {uchar = '\11026', commands = [("unicode-math","\\squaretopblack")], category = Ord, comments = "SQUARE WITH TOP HALF BLACK"}
  , Record {uchar = '\11027', commands = [("unicode-math","\\squarebotblack")], category = Ord, comments = "SQUARE WITH BOTTOM HALF BLACK"}
  , Record {uchar = '\11028', commands = [("unicode-math","\\squareurblack")], category = Ord, comments = "SQUARE WITH UPPER RIGHT DIAGONAL HALF BLACK"}
  , Record {uchar = '\11029', commands = [("unicode-math","\\squarellblack")], category = Ord, comments = "SQUARE WITH LOWER LEFT DIAGONAL HALF BLACK"}
  , Record {uchar = '\11030', commands = [("unicode-math","\\diamondleftblack")], category = Ord, comments = "DIAMOND WITH LEFT HALF BLACK"}
  , Record {uchar = '\11031', commands = [("unicode-math","\\diamondrightblack")], category = Ord, comments = "DIAMOND WITH RIGHT HALF BLACK"}
  , Record {uchar = '\11032', commands = [("unicode-math","\\diamondtopblack")], category = Ord, comments = "DIAMOND WITH TOP HALF BLACK"}
  , Record {uchar = '\11033', commands = [("unicode-math","\\diamondbotblack")], category = Ord, comments = "DIAMOND WITH BOTTOM HALF BLACK"}
  , Record {uchar = '\11034', commands = [("unicode-math","\\dottedsquare")], category = Ord, comments = "DOTTED SQUARE"}
  , Record {uchar = '\11035', commands = [("fourier","\\blacksquare"),("unicode-math","\\lgblksquare")], category = Ord, comments = "BLACK LARGE SQUARE"}
  , Record {uchar = '\11036', commands = [("fourier","\\square"),("unicode-math","\\lgwhtsquare")], category = Ord, comments = "WHITE LARGE SQUARE"}
  , Record {uchar = '\11037', commands = [("amssymb","\\centerdot"),("unicode-math","\\vysmblksquare")], category = Ord, comments = "t \\Squaredot (marvosym), BLACK VERY SMALL SQUARE"}
  , Record {uchar = '\11038', commands = [("unicode-math","\\vysmwhtsquare")], category = Ord, comments = "WHITE VERY SMALL SQUARE"}
  , Record {uchar = '\11039', commands = [("unicode-math","\\pentagonblack")], category = Ord, comments = "BLACK PENTAGON"}
  , Record {uchar = '\11040', commands = [("unicode-math","\\pentagon")], category = Ord, comments = "WHITE PENTAGON"}
  , Record {uchar = '\11041', commands = [("unicode-math","\\varhexagon")], category = Ord, comments = "WHITE HEXAGON"}
  , Record {uchar = '\11042', commands = [("unicode-math","\\varhexagonblack")], category = Ord, comments = "BLACK HEXAGON"}
  , Record {uchar = '\11043', commands = [("unicode-math","\\hexagonblack")], category = Ord, comments = "HORIZONTAL BLACK HEXAGON"}
  , Record {uchar = '\11044', commands = [("unicode-math","\\lgblkcircle")], category = Ord, comments = "BLACK LARGE CIRCLE"}
  , Record {uchar = '\11045', commands = [("unicode-math","\\mdblkdiamond")], category = Ord, comments = "BLACK MEDIUM DIAMOND"}
  , Record {uchar = '\11046', commands = [("unicode-math","\\mdwhtdiamond")], category = Ord, comments = "WHITE MEDIUM DIAMOND"}
  , Record {uchar = '\11047', commands = [("amssymb","\\blacklozenge"),("unicode-math","\\mdblklozenge")], category = Ord, comments = "BLACK MEDIUM LOZENGE"}
  , Record {uchar = '\11048', commands = [("amssymb","\\lozenge"),("unicode-math","\\mdwhtlozenge")], category = Ord, comments = "WHITE MEDIUM LOZENGE"}
  , Record {uchar = '\11049', commands = [("unicode-math","\\smblkdiamond")], category = Ord, comments = "BLACK SMALL DIAMOND"}
  , Record {uchar = '\11050', commands = [("unicode-math","\\smblklozenge")], category = Ord, comments = "BLACK SMALL LOZENGE"}
  , Record {uchar = '\11051', commands = [("unicode-math","\\smwhtlozenge")], category = Ord, comments = "WHITE SMALL LOZENGE"}
  , Record {uchar = '\11052', commands = [("unicode-math","\\blkhorzoval")], category = Ord, comments = "BLACK HORIZONTAL ELLIPSE"}
  , Record {uchar = '\11053', commands = [("unicode-math","\\whthorzoval")], category = Ord, comments = "WHITE HORIZONTAL ELLIPSE"}
  , Record {uchar = '\11054', commands = [("unicode-math","\\blkvertoval")], category = Ord, comments = "BLACK VERTICAL ELLIPSE"}
  , Record {uchar = '\11055', commands = [("unicode-math","\\whtvertoval")], category = Ord, comments = "WHITE VERTICAL ELLIPSE"}
  , Record {uchar = '\11056', commands = [("unicode-math","\\circleonleftarrow")], category = Rel, comments = "LEFT ARROW WITH SMALL CIRCLE"}
  , Record {uchar = '\11057', commands = [("unicode-math","\\leftthreearrows")], category = Rel, comments = "THREE LEFTWARDS ARROWS"}
  , Record {uchar = '\11058', commands = [("unicode-math","\\leftarrowonoplus")], category = Rel, comments = "LEFT ARROW WITH CIRCLED PLUS"}
  , Record {uchar = '\11059', commands = [("unicode-math","\\longleftsquigarrow")], category = Rel, comments = "LONG LEFTWARDS SQUIGGLE ARROW"}
  , Record {uchar = '\11060', commands = [("unicode-math","\\nvtwoheadleftarrow")], category = Rel, comments = "LEFTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE"}
  , Record {uchar = '\11061', commands = [("unicode-math","\\nVtwoheadleftarrow")], category = Rel, comments = "LEFTWARDS TWO-HEADED ARROW WITH DOUBLE VERTICAL STROKE"}
  , Record {uchar = '\11062', commands = [("unicode-math","\\twoheadmapsfrom")], category = Rel, comments = "LEFTWARDS TWO-HEADED ARROW FROM BAR"}
  , Record {uchar = '\11063', commands = [("unicode-math","\\twoheadleftdbkarrow")], category = Rel, comments = "leftwards two-headed triple-dash arrow"}
  , Record {uchar = '\11064', commands = [("unicode-math","\\leftdotarrow")], category = Rel, comments = "LEFTWARDS ARROW WITH DOTTED STEM"}
  , Record {uchar = '\11065', commands = [("unicode-math","\\nvleftarrowtail")], category = Rel, comments = "LEFTWARDS ARROW WITH TAIL WITH VERTICAL STROKE"}
  , Record {uchar = '\11066', commands = [("unicode-math","\\nVleftarrowtail")], category = Rel, comments = "LEFTWARDS ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE"}
  , Record {uchar = '\11067', commands = [("unicode-math","\\twoheadleftarrowtail")], category = Rel, comments = "LEFTWARDS TWO-HEADED ARROW WITH TAIL"}
  , Record {uchar = '\11068', commands = [("unicode-math","\\nvtwoheadleftarrowtail")], category = Rel, comments = "LEFTWARDS TWO-HEADED ARROW WITH TAIL WITH VERTICAL STROKE"}
  , Record {uchar = '\11069', commands = [("unicode-math","\\nVtwoheadleftarrowtail")], category = Rel, comments = "LEFTWARDS TWO-HEADED ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE"}
  , Record {uchar = '\11070', commands = [("unicode-math","\\leftarrowx")], category = Rel, comments = "LEFTWARDS ARROW THROUGH X"}
  , Record {uchar = '\11071', commands = [("unicode-math","\\leftcurvedarrow")], category = Rel, comments = "WAVE ARROW POINTING DIRECTLY LEFT"}
  , Record {uchar = '\11072', commands = [("unicode-math","\\equalleftarrow")], category = Rel, comments = "EQUALS SIGN ABOVE LEFTWARDS ARROW"}
  , Record {uchar = '\11073', commands = [("unicode-math","\\bsimilarleftarrow")], category = Rel, comments = "REVERSE TILDE OPERATOR ABOVE LEFTWARDS ARROW"}
  , Record {uchar = '\11074', commands = [("unicode-math","\\leftarrowbackapprox")], category = Rel, comments = "LEFTWARDS ARROW ABOVE REVERSE ALMOST EQUAL TO"}
  , Record {uchar = '\11075', commands = [("unicode-math","\\rightarrowgtr")], category = Rel, comments = "rightwards arrow through less-than"}
  , Record {uchar = '\11076', commands = [("unicode-math","\\rightarrowsupset")], category = Rel, comments = "rightwards arrow through subset"}
  , Record {uchar = '\11077', commands = [("unicode-math","\\LLeftarrow")], category = Rel, comments = "LEFTWARDS QUADRUPLE ARROW"}
  , Record {uchar = '\11078', commands = [("unicode-math","\\RRightarrow")], category = Rel, comments = "RIGHTWARDS QUADRUPLE ARROW"}
  , Record {uchar = '\11079', commands = [("unicode-math","\\bsimilarrightarrow")], category = Rel, comments = "REVERSE TILDE OPERATOR ABOVE RIGHTWARDS ARROW"}
  , Record {uchar = '\11080', commands = [("unicode-math","\\rightarrowbackapprox")], category = Rel, comments = "RIGHTWARDS ARROW ABOVE REVERSE ALMOST EQUAL TO"}
  , Record {uchar = '\11081', commands = [("unicode-math","\\similarleftarrow")], category = Rel, comments = "TILDE OPERATOR ABOVE LEFTWARDS ARROW"}
  , Record {uchar = '\11082', commands = [("unicode-math","\\leftarrowapprox")], category = Rel, comments = "LEFTWARDS ARROW ABOVE ALMOST EQUAL TO"}
  , Record {uchar = '\11083', commands = [("unicode-math","\\leftarrowbsimilar")], category = Rel, comments = "LEFTWARDS ARROW ABOVE REVERSE TILDE OPERATOR"}
  , Record {uchar = '\11084', commands = [("unicode-math","\\rightarrowbsimilar")], category = Rel, comments = "righttwards arrow above reverse tilde operator"}
  , Record {uchar = '\11088', commands = [("unicode-math","\\medwhitestar")], category = Ord, comments = "WHITE MEDIUM STAR"}
  , Record {uchar = '\11089', commands = [("unicode-math","\\medblackstar")], category = Ord, comments = "black medium star"}
  , Record {uchar = '\11090', commands = [("unicode-math","\\smwhitestar")], category = Ord, comments = "WHITE SMALL STAR"}
  , Record {uchar = '\11091', commands = [("unicode-math","\\rightpentagonblack")], category = Ord, comments = "BLACK RIGHT-POINTING PENTAGON"}
  , Record {uchar = '\11092', commands = [("unicode-math","\\rightpentagon")], category = Ord, comments = "WHITE RIGHT-POINTING PENTAGON"}
  , Record {uchar = '\12296', commands = [("base","\\langle")], category = Open, comments = "LEFT ANGLE BRACKET (deprecated for math use)"}
  , Record {uchar = '\12297', commands = [("base","\\rangle")], category = Close, comments = "RIGHT ANGLE BRACKET (deprecated for math use)"}
  , Record {uchar = '\12306', commands = [("unicode-math","\\postalmark")], category = Ord, comments = "POSTAL MARK"}
  , Record {uchar = '\12308', commands = [("unicode-math","\\lbrbrak")], category = Open, comments = "left broken bracket"}
  , Record {uchar = '\12309', commands = [("unicode-math","\\rbrbrak")], category = Close, comments = "right broken bracket"}
  , Record {uchar = '\12312', commands = [("unicode-math","\\Lbrbrak")], category = Open, comments = "LEFT WHITE TORTOISE SHELL BRACKET"}
  , Record {uchar = '\12313', commands = [("unicode-math","\\Rbrbrak")], category = Close, comments = "RIGHT WHITE TORTOISE SHELL BRACKET"}
  , Record {uchar = '\12314', commands = [("stmaryrd","\\llbracket")], category = Open, comments = "LEFT WHITE SQUARE BRACKET (deprecated for math use)"}
  , Record {uchar = '\12315', commands = [("stmaryrd","\\rrbracket")], category = Close, comments = "RIGHT WHITE SQUARE BRACKET (deprecated for math use)"}
  , Record {uchar = '\12336', commands = [("unicode-math","\\hzigzag")], category = Ord, comments = "zigzag"}
  , Record {uchar = '\12398', commands = [], category = Alpha, comments = "HIRAGANA LETTER NO"}
  , Record {uchar = '\64297', commands = [], category = Ord, comments = "HEBREW LETTER ALTERNATIVE PLUS SIGN (doesn't have cross shape)"}
  , Record {uchar = '\65024', commands = [], category = Accent, comments = "VARIATION SELECTOR-1"}
  , Record {uchar = '\65121', commands = [], category = Ord, comments = "SMALL ASTERISK"}
  , Record {uchar = '\65122', commands = [], category = Ord, comments = "SMALL PLUS SIGN"}
  , Record {uchar = '\65123', commands = [], category = Ord, comments = "SMALL HYPHEN-MINUS"}
  , Record {uchar = '\65124', commands = [], category = Ord, comments = "SMALL LESS-THAN SIGN"}
  , Record {uchar = '\65125', commands = [], category = Ord, comments = "SMALL GREATER-THAN SIGN"}
  , Record {uchar = '\65126', commands = [], category = Ord, comments = "SMALL EQUALS SIGN"}
  , Record {uchar = '\65128', commands = [], category = Ord, comments = "SMALL REVERSE SOLIDUS"}
  , Record {uchar = '\65291', commands = [], category = Ord, comments = "FULLWIDTH PLUS SIGN"}
  , Record {uchar = '\65308', commands = [], category = Ord, comments = "FULLWIDTH LESS-THAN SIGN"}
  , Record {uchar = '\65309', commands = [], category = Ord, comments = "FULLWIDTH EQUALS SIGN"}
  , Record {uchar = '\65310', commands = [], category = Ord, comments = "FULLWIDTH GREATER-THAN SIGN"}
  , Record {uchar = '\65340', commands = [], category = Ord, comments = "FULLWIDTH REVERSE SOLIDUS"}
  , Record {uchar = '\65342', commands = [], category = Ord, comments = "FULLWIDTH CIRCUMFLEX ACCENT"}
  , Record {uchar = '\65372', commands = [], category = Ord, comments = "FULLWIDTH VERTICAL LINE"}
  , Record {uchar = '\65374', commands = [], category = Ord, comments = "FULLWIDTH TILDE"}
  , Record {uchar = '\65506', commands = [], category = Ord, comments = "FULLWIDTH NOT SIGN"}
  , Record {uchar = '\65513', commands = [], category = Ord, comments = "HALFWIDTH LEFTWARDS ARROW"}
  , Record {uchar = '\65514', commands = [], category = Ord, comments = "HALFWIDTH UPWARDS ARROW"}
  , Record {uchar = '\65515', commands = [], category = Ord, comments = "HALFWIDTH RIGHTWARDS ARROW"}
  , Record {uchar = '\65516', commands = [], category = Ord, comments = "HALFWIDTH DOWNWARDS ARROW"}
  , Record {uchar = '\119808', commands = [("base","\\mathbf{A}"),("unicode-math","\\mbfA")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL A"}
  , Record {uchar = '\119809', commands = [("base","\\mathbf{B}"),("unicode-math","\\mbfB")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL B"}
  , Record {uchar = '\119810', commands = [("base","\\mathbf{C}"),("unicode-math","\\mbfC")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL C"}
  , Record {uchar = '\119811', commands = [("base","\\mathbf{D}"),("unicode-math","\\mbfD")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL D"}
  , Record {uchar = '\119812', commands = [("base","\\mathbf{E}"),("unicode-math","\\mbfE")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL E"}
  , Record {uchar = '\119813', commands = [("base","\\mathbf{F}"),("unicode-math","\\mbfF")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL F"}
  , Record {uchar = '\119814', commands = [("base","\\mathbf{G}"),("unicode-math","\\mbfG")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL G"}
  , Record {uchar = '\119815', commands = [("base","\\mathbf{H}"),("unicode-math","\\mbfH")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL H"}
  , Record {uchar = '\119816', commands = [("base","\\mathbf{I}"),("unicode-math","\\mbfI")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL I"}
  , Record {uchar = '\119817', commands = [("base","\\mathbf{J}"),("unicode-math","\\mbfJ")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL J"}
  , Record {uchar = '\119818', commands = [("base","\\mathbf{K}"),("unicode-math","\\mbfK")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL K"}
  , Record {uchar = '\119819', commands = [("base","\\mathbf{L}"),("unicode-math","\\mbfL")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL L"}
  , Record {uchar = '\119820', commands = [("base","\\mathbf{M}"),("unicode-math","\\mbfM")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL M"}
  , Record {uchar = '\119821', commands = [("base","\\mathbf{N}"),("unicode-math","\\mbfN")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL N"}
  , Record {uchar = '\119822', commands = [("base","\\mathbf{O}"),("unicode-math","\\mbfO")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL O"}
  , Record {uchar = '\119823', commands = [("base","\\mathbf{P}"),("unicode-math","\\mbfP")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL P"}
  , Record {uchar = '\119824', commands = [("base","\\mathbf{Q}"),("unicode-math","\\mbfQ")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL Q"}
  , Record {uchar = '\119825', commands = [("base","\\mathbf{R}"),("unicode-math","\\mbfR")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL R"}
  , Record {uchar = '\119826', commands = [("base","\\mathbf{S}"),("unicode-math","\\mbfS")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL S"}
  , Record {uchar = '\119827', commands = [("base","\\mathbf{T}"),("unicode-math","\\mbfT")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL T"}
  , Record {uchar = '\119828', commands = [("base","\\mathbf{U}"),("unicode-math","\\mbfU")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL U"}
  , Record {uchar = '\119829', commands = [("base","\\mathbf{V}"),("unicode-math","\\mbfV")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL V"}
  , Record {uchar = '\119830', commands = [("base","\\mathbf{W}"),("unicode-math","\\mbfW")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL W"}
  , Record {uchar = '\119831', commands = [("base","\\mathbf{X}"),("unicode-math","\\mbfX")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL X"}
  , Record {uchar = '\119832', commands = [("base","\\mathbf{Y}"),("unicode-math","\\mbfY")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL Y"}
  , Record {uchar = '\119833', commands = [("base","\\mathbf{Z}"),("unicode-math","\\mbfZ")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL Z"}
  , Record {uchar = '\119834', commands = [("base","\\mathbf{a}"),("unicode-math","\\mbfa")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL A"}
  , Record {uchar = '\119835', commands = [("base","\\mathbf{b}"),("unicode-math","\\mbfb")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL B"}
  , Record {uchar = '\119836', commands = [("base","\\mathbf{c}"),("unicode-math","\\mbfc")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL C"}
  , Record {uchar = '\119837', commands = [("base","\\mathbf{d}"),("unicode-math","\\mbfd")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL D"}
  , Record {uchar = '\119838', commands = [("base","\\mathbf{e}"),("unicode-math","\\mbfe")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL E"}
  , Record {uchar = '\119839', commands = [("base","\\mathbf{f}"),("unicode-math","\\mbff")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL F"}
  , Record {uchar = '\119840', commands = [("base","\\mathbf{g}"),("unicode-math","\\mbfg")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL G"}
  , Record {uchar = '\119841', commands = [("base","\\mathbf{h}"),("unicode-math","\\mbfh")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL H"}
  , Record {uchar = '\119842', commands = [("base","\\mathbf{i}"),("unicode-math","\\mbfi")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL I"}
  , Record {uchar = '\119843', commands = [("base","\\mathbf{j}"),("unicode-math","\\mbfj")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL J"}
  , Record {uchar = '\119844', commands = [("base","\\mathbf{k}"),("unicode-math","\\mbfk")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL K"}
  , Record {uchar = '\119845', commands = [("base","\\mathbf{l}"),("unicode-math","\\mbfl")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL L"}
  , Record {uchar = '\119846', commands = [("base","\\mathbf{m}"),("unicode-math","\\mbfm")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL M"}
  , Record {uchar = '\119847', commands = [("base","\\mathbf{n}"),("unicode-math","\\mbfn")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL N"}
  , Record {uchar = '\119848', commands = [("base","\\mathbf{o}"),("unicode-math","\\mbfo")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL O"}
  , Record {uchar = '\119849', commands = [("base","\\mathbf{p}"),("unicode-math","\\mbfp")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL P"}
  , Record {uchar = '\119850', commands = [("base","\\mathbf{q}"),("unicode-math","\\mbfq")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL Q"}
  , Record {uchar = '\119851', commands = [("base","\\mathbf{r}"),("unicode-math","\\mbfr")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL R"}
  , Record {uchar = '\119852', commands = [("base","\\mathbf{s}"),("unicode-math","\\mbfs")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL S"}
  , Record {uchar = '\119853', commands = [("base","\\mathbf{t}"),("unicode-math","\\mbft")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL T"}
  , Record {uchar = '\119854', commands = [("base","\\mathbf{u}"),("unicode-math","\\mbfu")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL U"}
  , Record {uchar = '\119855', commands = [("base","\\mathbf{v}"),("unicode-math","\\mbfv")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL V"}
  , Record {uchar = '\119856', commands = [("base","\\mathbf{w}"),("unicode-math","\\mbfw")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL W"}
  , Record {uchar = '\119857', commands = [("base","\\mathbf{x}"),("unicode-math","\\mbfx")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL X"}
  , Record {uchar = '\119858', commands = [("base","\\mathbf{y}"),("unicode-math","\\mbfy")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL Y"}
  , Record {uchar = '\119859', commands = [("base","\\mathbf{z}"),("unicode-math","\\mbfz")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL Z"}
  , Record {uchar = '\119860', commands = [("base","A"),("base","\\mathit{A}"),("unicode-math","\\mitA")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL A"}
  , Record {uchar = '\119861', commands = [("base","B"),("base","\\mathit{B}"),("unicode-math","\\mitB")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL B"}
  , Record {uchar = '\119862', commands = [("base","C"),("base","\\mathit{C}"),("unicode-math","\\mitC")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL C"}
  , Record {uchar = '\119863', commands = [("base","D"),("base","\\mathit{D}"),("unicode-math","\\mitD")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL D"}
  , Record {uchar = '\119864', commands = [("base","E"),("base","\\mathit{E}"),("unicode-math","\\mitE")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL E"}
  , Record {uchar = '\119865', commands = [("base","F"),("base","\\mathit{F}"),("unicode-math","\\mitF")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL F"}
  , Record {uchar = '\119866', commands = [("base","G"),("base","\\mathit{G}"),("unicode-math","\\mitG")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL G"}
  , Record {uchar = '\119867', commands = [("base","H"),("base","\\mathit{H}"),("unicode-math","\\mitH")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL H"}
  , Record {uchar = '\119868', commands = [("base","I"),("base","\\mathit{I}"),("unicode-math","\\mitI")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL I"}
  , Record {uchar = '\119869', commands = [("base","J"),("base","\\mathit{J}"),("unicode-math","\\mitJ")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL J"}
  , Record {uchar = '\119870', commands = [("base","K"),("base","\\mathit{K}"),("unicode-math","\\mitK")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL K"}
  , Record {uchar = '\119871', commands = [("base","L"),("base","\\mathit{L}"),("unicode-math","\\mitL")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL L"}
  , Record {uchar = '\119872', commands = [("base","M"),("base","\\mathit{M}"),("unicode-math","\\mitM")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL M"}
  , Record {uchar = '\119873', commands = [("base","N"),("base","\\mathit{N}"),("unicode-math","\\mitN")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL N"}
  , Record {uchar = '\119874', commands = [("base","O"),("base","\\mathit{O}"),("unicode-math","\\mitO")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL O"}
  , Record {uchar = '\119875', commands = [("base","P"),("base","\\mathit{P}"),("unicode-math","\\mitP")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL P"}
  , Record {uchar = '\119876', commands = [("base","Q"),("base","\\mathit{Q}"),("unicode-math","\\mitQ")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL Q"}
  , Record {uchar = '\119877', commands = [("base","R"),("base","\\mathit{R}"),("unicode-math","\\mitR")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL R"}
  , Record {uchar = '\119878', commands = [("base","S"),("base","\\mathit{S}"),("unicode-math","\\mitS")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL S"}
  , Record {uchar = '\119879', commands = [("base","T"),("base","\\mathit{T}"),("unicode-math","\\mitT")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL T"}
  , Record {uchar = '\119880', commands = [("base","U"),("base","\\mathit{U}"),("unicode-math","\\mitU")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL U"}
  , Record {uchar = '\119881', commands = [("base","V"),("base","\\mathit{V}"),("unicode-math","\\mitV")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL V"}
  , Record {uchar = '\119882', commands = [("base","W"),("base","\\mathit{W}"),("unicode-math","\\mitW")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL W"}
  , Record {uchar = '\119883', commands = [("base","X"),("base","\\mathit{X}"),("unicode-math","\\mitX")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL X"}
  , Record {uchar = '\119884', commands = [("base","Y"),("base","\\mathit{Y}"),("unicode-math","\\mitY")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL Y"}
  , Record {uchar = '\119885', commands = [("base","Z"),("base","\\mathit{Z}"),("unicode-math","\\mitZ")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL Z"}
  , Record {uchar = '\119886', commands = [("base","a"),("base","\\mathit{a}"),("unicode-math","\\mita")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL A"}
  , Record {uchar = '\119887', commands = [("base","b"),("base","\\mathit{b}"),("unicode-math","\\mitb")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL B"}
  , Record {uchar = '\119888', commands = [("base","c"),("base","\\mathit{c}"),("unicode-math","\\mitc")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL C"}
  , Record {uchar = '\119889', commands = [("base","d"),("base","\\mathit{d}"),("unicode-math","\\mitd")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL D"}
  , Record {uchar = '\119890', commands = [("base","e"),("base","\\mathit{e}"),("unicode-math","\\mite")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL E"}
  , Record {uchar = '\119891', commands = [("base","f"),("base","\\mathit{f}"),("unicode-math","\\mitf")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL F"}
  , Record {uchar = '\119892', commands = [("base","g"),("base","\\mathit{g}"),("unicode-math","\\mitg")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL G"}
  , Record {uchar = '\119894', commands = [("base","i"),("base","\\mathit{i}"),("unicode-math","\\miti")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL I"}
  , Record {uchar = '\119895', commands = [("base","j"),("base","\\mathit{j}"),("unicode-math","\\mitj")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL J"}
  , Record {uchar = '\119896', commands = [("base","k"),("base","\\mathit{k}"),("unicode-math","\\mitk")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL K"}
  , Record {uchar = '\119897', commands = [("base","l"),("base","\\mathit{l}"),("unicode-math","\\mitl")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL L"}
  , Record {uchar = '\119898', commands = [("base","m"),("base","\\mathit{m}"),("unicode-math","\\mitm")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL M"}
  , Record {uchar = '\119899', commands = [("base","n"),("base","\\mathit{n}"),("unicode-math","\\mitn")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL N"}
  , Record {uchar = '\119900', commands = [("base","o"),("base","\\mathit{o}"),("unicode-math","\\mito")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL O"}
  , Record {uchar = '\119901', commands = [("base","p"),("base","\\mathit{p}"),("unicode-math","\\mitp")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL P"}
  , Record {uchar = '\119902', commands = [("base","q"),("base","\\mathit{q}"),("unicode-math","\\mitq")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL Q"}
  , Record {uchar = '\119903', commands = [("base","r"),("base","\\mathit{r}"),("unicode-math","\\mitr")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL R"}
  , Record {uchar = '\119904', commands = [("base","s"),("base","\\mathit{s}"),("unicode-math","\\mits")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL S"}
  , Record {uchar = '\119905', commands = [("base","t"),("base","\\mathit{t}"),("unicode-math","\\mitt")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL T"}
  , Record {uchar = '\119906', commands = [("base","u"),("base","\\mathit{u}"),("unicode-math","\\mitu")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL U"}
  , Record {uchar = '\119907', commands = [("base","v"),("base","\\mathit{v}"),("unicode-math","\\mitv")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL V"}
  , Record {uchar = '\119908', commands = [("base","w"),("base","\\mathit{w}"),("unicode-math","\\mitw")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL W"}
  , Record {uchar = '\119909', commands = [("base","x"),("base","\\mathit{x}"),("unicode-math","\\mitx")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL X"}
  , Record {uchar = '\119910', commands = [("base","y"),("base","\\mathit{y}"),("unicode-math","\\mity")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL Y"}
  , Record {uchar = '\119911', commands = [("base","z"),("base","\\mathit{z}"),("unicode-math","\\mitz")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL Z"}
  , Record {uchar = '\119912', commands = [("isomath","\\mathbfit{A}"),("fixmath","\\mathbold{A}"),("unicode-math","\\mbfitA")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL A"}
  , Record {uchar = '\119913', commands = [("isomath","\\mathbfit{B}"),("fixmath","\\mathbold{B}"),("unicode-math","\\mbfitB")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL B"}
  , Record {uchar = '\119914', commands = [("isomath","\\mathbfit{C}"),("fixmath","\\mathbold{C}"),("unicode-math","\\mbfitC")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL C"}
  , Record {uchar = '\119915', commands = [("isomath","\\mathbfit{D}"),("fixmath","\\mathbold{D}"),("unicode-math","\\mbfitD")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL D"}
  , Record {uchar = '\119916', commands = [("isomath","\\mathbfit{E}"),("fixmath","\\mathbold{E}"),("unicode-math","\\mbfitE")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL E"}
  , Record {uchar = '\119917', commands = [("isomath","\\mathbfit{F}"),("fixmath","\\mathbold{F}"),("unicode-math","\\mbfitF")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL F"}
  , Record {uchar = '\119918', commands = [("isomath","\\mathbfit{G}"),("fixmath","\\mathbold{G}"),("unicode-math","\\mbfitG")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL G"}
  , Record {uchar = '\119919', commands = [("isomath","\\mathbfit{H}"),("fixmath","\\mathbold{H}"),("unicode-math","\\mbfitH")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL H"}
  , Record {uchar = '\119920', commands = [("isomath","\\mathbfit{I}"),("fixmath","\\mathbold{I}"),("unicode-math","\\mbfitI")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL I"}
  , Record {uchar = '\119921', commands = [("isomath","\\mathbfit{J}"),("fixmath","\\mathbold{J}"),("unicode-math","\\mbfitJ")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL J"}
  , Record {uchar = '\119922', commands = [("isomath","\\mathbfit{K}"),("fixmath","\\mathbold{K}"),("unicode-math","\\mbfitK")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL K"}
  , Record {uchar = '\119923', commands = [("isomath","\\mathbfit{L}"),("fixmath","\\mathbold{L}"),("unicode-math","\\mbfitL")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL L"}
  , Record {uchar = '\119924', commands = [("isomath","\\mathbfit{M}"),("fixmath","\\mathbold{M}"),("unicode-math","\\mbfitM")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL M"}
  , Record {uchar = '\119925', commands = [("isomath","\\mathbfit{N}"),("fixmath","\\mathbold{N}"),("unicode-math","\\mbfitN")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL N"}
  , Record {uchar = '\119926', commands = [("isomath","\\mathbfit{O}"),("fixmath","\\mathbold{O}"),("unicode-math","\\mbfitO")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL O"}
  , Record {uchar = '\119927', commands = [("isomath","\\mathbfit{P}"),("fixmath","\\mathbold{P}"),("unicode-math","\\mbfitP")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL P"}
  , Record {uchar = '\119928', commands = [("isomath","\\mathbfit{Q}"),("fixmath","\\mathbold{Q}"),("unicode-math","\\mbfitQ")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL Q"}
  , Record {uchar = '\119929', commands = [("isomath","\\mathbfit{R}"),("fixmath","\\mathbold{R}"),("unicode-math","\\mbfitR")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL R"}
  , Record {uchar = '\119930', commands = [("isomath","\\mathbfit{S}"),("fixmath","\\mathbold{S}"),("unicode-math","\\mbfitS")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL S"}
  , Record {uchar = '\119931', commands = [("isomath","\\mathbfit{T}"),("fixmath","\\mathbold{T}"),("unicode-math","\\mbfitT")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL T"}
  , Record {uchar = '\119932', commands = [("isomath","\\mathbfit{U}"),("fixmath","\\mathbold{U}"),("unicode-math","\\mbfitU")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL U"}
  , Record {uchar = '\119933', commands = [("isomath","\\mathbfit{V}"),("fixmath","\\mathbold{V}"),("unicode-math","\\mbfitV")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL V"}
  , Record {uchar = '\119934', commands = [("isomath","\\mathbfit{W}"),("fixmath","\\mathbold{W}"),("unicode-math","\\mbfitW")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL W"}
  , Record {uchar = '\119935', commands = [("isomath","\\mathbfit{X}"),("fixmath","\\mathbold{X}"),("unicode-math","\\mbfitX")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL X"}
  , Record {uchar = '\119936', commands = [("isomath","\\mathbfit{Y}"),("fixmath","\\mathbold{Y}"),("unicode-math","\\mbfitY")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL Y"}
  , Record {uchar = '\119937', commands = [("isomath","\\mathbfit{Z}"),("fixmath","\\mathbold{Z}"),("unicode-math","\\mbfitZ")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL Z"}
  , Record {uchar = '\119938', commands = [("isomath","\\mathbfit{a}"),("fixmath","\\mathbold{a}"),("unicode-math","\\mbfita")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL A"}
  , Record {uchar = '\119939', commands = [("isomath","\\mathbfit{b}"),("fixmath","\\mathbold{b}"),("unicode-math","\\mbfitb")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL B"}
  , Record {uchar = '\119940', commands = [("isomath","\\mathbfit{c}"),("fixmath","\\mathbold{c}"),("unicode-math","\\mbfitc")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL C"}
  , Record {uchar = '\119941', commands = [("isomath","\\mathbfit{d}"),("fixmath","\\mathbold{d}"),("unicode-math","\\mbfitd")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL D"}
  , Record {uchar = '\119942', commands = [("isomath","\\mathbfit{e}"),("fixmath","\\mathbold{e}"),("unicode-math","\\mbfite")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL E"}
  , Record {uchar = '\119943', commands = [("isomath","\\mathbfit{f}"),("fixmath","\\mathbold{f}"),("unicode-math","\\mbfitf")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL F"}
  , Record {uchar = '\119944', commands = [("isomath","\\mathbfit{g}"),("fixmath","\\mathbold{g}"),("unicode-math","\\mbfitg")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL G"}
  , Record {uchar = '\119945', commands = [("isomath","\\mathbfit{h}"),("fixmath","\\mathbold{h}"),("unicode-math","\\mbfith")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL H"}
  , Record {uchar = '\119946', commands = [("isomath","\\mathbfit{i}"),("fixmath","\\mathbold{i}"),("unicode-math","\\mbfiti")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL I"}
  , Record {uchar = '\119947', commands = [("isomath","\\mathbfit{j}"),("fixmath","\\mathbold{j}"),("unicode-math","\\mbfitj")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL J"}
  , Record {uchar = '\119948', commands = [("isomath","\\mathbfit{k}"),("fixmath","\\mathbold{k}"),("unicode-math","\\mbfitk")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL K"}
  , Record {uchar = '\119949', commands = [("isomath","\\mathbfit{l}"),("fixmath","\\mathbold{l}"),("unicode-math","\\mbfitl")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL L"}
  , Record {uchar = '\119950', commands = [("isomath","\\mathbfit{m}"),("fixmath","\\mathbold{m}"),("unicode-math","\\mbfitm")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL M"}
  , Record {uchar = '\119951', commands = [("isomath","\\mathbfit{n}"),("fixmath","\\mathbold{n}"),("unicode-math","\\mbfitn")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL N"}
  , Record {uchar = '\119952', commands = [("isomath","\\mathbfit{o}"),("fixmath","\\mathbold{o}"),("unicode-math","\\mbfito")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL O"}
  , Record {uchar = '\119953', commands = [("isomath","\\mathbfit{p}"),("fixmath","\\mathbold{p}"),("unicode-math","\\mbfitp")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL P"}
  , Record {uchar = '\119954', commands = [("isomath","\\mathbfit{q}"),("fixmath","\\mathbold{q}"),("unicode-math","\\mbfitq")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL Q"}
  , Record {uchar = '\119955', commands = [("isomath","\\mathbfit{r}"),("fixmath","\\mathbold{r}"),("unicode-math","\\mbfitr")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL R"}
  , Record {uchar = '\119956', commands = [("isomath","\\mathbfit{s}"),("fixmath","\\mathbold{s}"),("unicode-math","\\mbfits")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL S"}
  , Record {uchar = '\119957', commands = [("isomath","\\mathbfit{t}"),("fixmath","\\mathbold{t}"),("unicode-math","\\mbfitt")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL T"}
  , Record {uchar = '\119958', commands = [("isomath","\\mathbfit{u}"),("fixmath","\\mathbold{u}"),("unicode-math","\\mbfitu")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL U"}
  , Record {uchar = '\119959', commands = [("isomath","\\mathbfit{v}"),("fixmath","\\mathbold{v}"),("unicode-math","\\mbfitv")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL V"}
  , Record {uchar = '\119960', commands = [("isomath","\\mathbfit{w}"),("fixmath","\\mathbold{w}"),("unicode-math","\\mbfitw")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL W"}
  , Record {uchar = '\119961', commands = [("isomath","\\mathbfit{x}"),("fixmath","\\mathbold{x}"),("unicode-math","\\mbfitx")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL X"}
  , Record {uchar = '\119962', commands = [("isomath","\\mathbfit{y}"),("fixmath","\\mathbold{y}"),("unicode-math","\\mbfity")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL Y"}
  , Record {uchar = '\119963', commands = [("isomath","\\mathbfit{z}"),("fixmath","\\mathbold{z}"),("unicode-math","\\mbfitz")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL Z"}
  , Record {uchar = '\119964', commands = [("base","\\mathcal{A}"),("unicode-math","\\mscrA")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL A"}
  , Record {uchar = '\119966', commands = [("base","\\mathcal{C}"),("unicode-math","\\mscrC")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL C"}
  , Record {uchar = '\119967', commands = [("base","\\mathcal{D}"),("unicode-math","\\mscrD")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL D"}
  , Record {uchar = '\119970', commands = [("base","\\mathcal{G}"),("unicode-math","\\mscrG")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL G"}
  , Record {uchar = '\119973', commands = [("base","\\mathcal{J}"),("unicode-math","\\mscrJ")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL J"}
  , Record {uchar = '\119974', commands = [("base","\\mathcal{K}"),("unicode-math","\\mscrK")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL K"}
  , Record {uchar = '\119977', commands = [("base","\\mathcal{N}"),("unicode-math","\\mscrN")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL N"}
  , Record {uchar = '\119978', commands = [("base","\\mathcal{O}"),("unicode-math","\\mscrO")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL O"}
  , Record {uchar = '\119979', commands = [("base","\\mathcal{P}"),("unicode-math","\\mscrP")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL P"}
  , Record {uchar = '\119980', commands = [("base","\\mathcal{Q}"),("unicode-math","\\mscrQ")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL Q"}
  , Record {uchar = '\119982', commands = [("base","\\mathcal{S}"),("unicode-math","\\mscrS")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL S"}
  , Record {uchar = '\119983', commands = [("base","\\mathcal{T}"),("unicode-math","\\mscrT")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL T"}
  , Record {uchar = '\119984', commands = [("base","\\mathcal{U}"),("unicode-math","\\mscrU")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL U"}
  , Record {uchar = '\119985', commands = [("base","\\mathcal{V}"),("unicode-math","\\mscrV")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL V"}
  , Record {uchar = '\119986', commands = [("base","\\mathcal{W}"),("unicode-math","\\mscrW")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL W"}
  , Record {uchar = '\119987', commands = [("base","\\mathcal{X}"),("unicode-math","\\mscrX")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL X"}
  , Record {uchar = '\119988', commands = [("base","\\mathcal{Y}"),("unicode-math","\\mscrY")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL Y"}
  , Record {uchar = '\119989', commands = [("base","\\mathcal{Z}"),("unicode-math","\\mscrZ")], category = Alpha, comments = "MATHEMATICAL SCRIPT CAPITAL Z"}
  , Record {uchar = '\119990', commands = [("urwchancal","\\mathcal{a}"),("unicode-math","\\mscra")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL A"}
  , Record {uchar = '\119991', commands = [("urwchancal","\\mathcal{b}"),("unicode-math","\\mscrb")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL B"}
  , Record {uchar = '\119992', commands = [("urwchancal","\\mathcal{c}"),("unicode-math","\\mscrc")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL C"}
  , Record {uchar = '\119993', commands = [("urwchancal","\\mathcal{d}"),("unicode-math","\\mscrd")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL D"}
  , Record {uchar = '\119995', commands = [("urwchancal","\\mathcal{f}"),("unicode-math","\\mscrf")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL F"}
  , Record {uchar = '\119997', commands = [("urwchancal","\\mathcal{h}"),("unicode-math","\\mscrh")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL H"}
  , Record {uchar = '\119998', commands = [("urwchancal","\\mathcal{i}"),("unicode-math","\\mscri")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL I"}
  , Record {uchar = '\119999', commands = [("urwchancal","\\mathcal{j}"),("unicode-math","\\mscrj")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL J"}
  , Record {uchar = '\120000', commands = [("urwchancal","\\mathcal{k}"),("unicode-math","\\mscrk")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL K"}
  , Record {uchar = '\120001', commands = [("urwchancal","\\mathcal{l}"),("unicode-math","\\mscrl")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL L"}
  , Record {uchar = '\120002', commands = [("urwchancal","\\mathcal{m}"),("unicode-math","\\mscrm")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL M"}
  , Record {uchar = '\120003', commands = [("urwchancal","\\mathcal{n}"),("unicode-math","\\mscrn")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL N"}
  , Record {uchar = '\120005', commands = [("urwchancal","\\mathcal{p}"),("unicode-math","\\mscrp")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL P"}
  , Record {uchar = '\120006', commands = [("urwchancal","\\mathcal{q}"),("unicode-math","\\mscrq")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL Q"}
  , Record {uchar = '\120007', commands = [("urwchancal","\\mathcal{r}"),("unicode-math","\\mscrr")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL R"}
  , Record {uchar = '\120008', commands = [("urwchancal","\\mathcal{s}"),("unicode-math","\\mscrs")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL S"}
  , Record {uchar = '\120009', commands = [("urwchancal","\\mathcal{t}"),("unicode-math","\\mscrt")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL T"}
  , Record {uchar = '\120010', commands = [("urwchancal","\\mathcal{u}"),("unicode-math","\\mscru")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL U"}
  , Record {uchar = '\120011', commands = [("urwchancal","\\mathcal{v}"),("unicode-math","\\mscrv")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL V"}
  , Record {uchar = '\120012', commands = [("urwchancal","\\mathcal{w}"),("unicode-math","\\mscrw")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL W"}
  , Record {uchar = '\120013', commands = [("urwchancal","\\mathcal{x}"),("unicode-math","\\mscrx")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL X"}
  , Record {uchar = '\120014', commands = [("urwchancal","\\mathcal{y}"),("unicode-math","\\mscry")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL Y"}
  , Record {uchar = '\120015', commands = [("urwchancal","\\mathcal{z}"),("unicode-math","\\mscrz")], category = Alpha, comments = "MATHEMATICAL SCRIPT SMALL Z"}
  , Record {uchar = '\120016', commands = [("unicode-math","\\mbfscrA")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL A"}
  , Record {uchar = '\120017', commands = [("unicode-math","\\mbfscrB")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL B"}
  , Record {uchar = '\120018', commands = [("unicode-math","\\mbfscrC")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL C"}
  , Record {uchar = '\120019', commands = [("unicode-math","\\mbfscrD")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL D"}
  , Record {uchar = '\120020', commands = [("unicode-math","\\mbfscrE")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL E"}
  , Record {uchar = '\120021', commands = [("unicode-math","\\mbfscrF")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL F"}
  , Record {uchar = '\120022', commands = [("unicode-math","\\mbfscrG")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL G"}
  , Record {uchar = '\120023', commands = [("unicode-math","\\mbfscrH")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL H"}
  , Record {uchar = '\120024', commands = [("unicode-math","\\mbfscrI")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL I"}
  , Record {uchar = '\120025', commands = [("unicode-math","\\mbfscrJ")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL J"}
  , Record {uchar = '\120026', commands = [("unicode-math","\\mbfscrK")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL K"}
  , Record {uchar = '\120027', commands = [("unicode-math","\\mbfscrL")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL L"}
  , Record {uchar = '\120028', commands = [("unicode-math","\\mbfscrM")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL M"}
  , Record {uchar = '\120029', commands = [("unicode-math","\\mbfscrN")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL N"}
  , Record {uchar = '\120030', commands = [("unicode-math","\\mbfscrO")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL O"}
  , Record {uchar = '\120031', commands = [("unicode-math","\\mbfscrP")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL P"}
  , Record {uchar = '\120032', commands = [("unicode-math","\\mbfscrQ")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL Q"}
  , Record {uchar = '\120033', commands = [("unicode-math","\\mbfscrR")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL R"}
  , Record {uchar = '\120034', commands = [("unicode-math","\\mbfscrS")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL S"}
  , Record {uchar = '\120035', commands = [("unicode-math","\\mbfscrT")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL T"}
  , Record {uchar = '\120036', commands = [("unicode-math","\\mbfscrU")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL U"}
  , Record {uchar = '\120037', commands = [("unicode-math","\\mbfscrV")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL V"}
  , Record {uchar = '\120038', commands = [("unicode-math","\\mbfscrW")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL W"}
  , Record {uchar = '\120039', commands = [("unicode-math","\\mbfscrX")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL X"}
  , Record {uchar = '\120040', commands = [("unicode-math","\\mbfscrY")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL Y"}
  , Record {uchar = '\120041', commands = [("unicode-math","\\mbfscrZ")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT CAPITAL Z"}
  , Record {uchar = '\120042', commands = [("unicode-math","\\mbfscra")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL A"}
  , Record {uchar = '\120043', commands = [("unicode-math","\\mbfscrb")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL B"}
  , Record {uchar = '\120044', commands = [("unicode-math","\\mbfscrc")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL C"}
  , Record {uchar = '\120045', commands = [("unicode-math","\\mbfscrd")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL D"}
  , Record {uchar = '\120046', commands = [("unicode-math","\\mbfscre")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL E"}
  , Record {uchar = '\120047', commands = [("unicode-math","\\mbfscrf")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL F"}
  , Record {uchar = '\120048', commands = [("unicode-math","\\mbfscrg")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL G"}
  , Record {uchar = '\120049', commands = [("unicode-math","\\mbfscrh")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL H"}
  , Record {uchar = '\120050', commands = [("unicode-math","\\mbfscri")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL I"}
  , Record {uchar = '\120051', commands = [("unicode-math","\\mbfscrj")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL J"}
  , Record {uchar = '\120052', commands = [("unicode-math","\\mbfscrk")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL K"}
  , Record {uchar = '\120053', commands = [("unicode-math","\\mbfscrl")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL L"}
  , Record {uchar = '\120054', commands = [("unicode-math","\\mbfscrm")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL M"}
  , Record {uchar = '\120055', commands = [("unicode-math","\\mbfscrn")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL N"}
  , Record {uchar = '\120056', commands = [("unicode-math","\\mbfscro")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL O"}
  , Record {uchar = '\120057', commands = [("unicode-math","\\mbfscrp")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL P"}
  , Record {uchar = '\120058', commands = [("unicode-math","\\mbfscrq")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL Q"}
  , Record {uchar = '\120059', commands = [("unicode-math","\\mbfscrr")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL R"}
  , Record {uchar = '\120060', commands = [("unicode-math","\\mbfscrs")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL S"}
  , Record {uchar = '\120061', commands = [("unicode-math","\\mbfscrt")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL T"}
  , Record {uchar = '\120062', commands = [("unicode-math","\\mbfscru")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL U"}
  , Record {uchar = '\120063', commands = [("unicode-math","\\mbfscrv")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL V"}
  , Record {uchar = '\120064', commands = [("unicode-math","\\mbfscrw")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL W"}
  , Record {uchar = '\120065', commands = [("unicode-math","\\mbfscrx")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL X"}
  , Record {uchar = '\120066', commands = [("unicode-math","\\mbfscry")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL Y"}
  , Record {uchar = '\120067', commands = [("unicode-math","\\mbfscrz")], category = Alpha, comments = "MATHEMATICAL BOLD SCRIPT SMALL Z"}
  , Record {uchar = '\120068', commands = [("eufrak","\\mathfrak{A}"),("unicode-math","\\mfrakA")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL A"}
  , Record {uchar = '\120069', commands = [("eufrak","\\mathfrak{B}"),("unicode-math","\\mfrakB")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL B"}
  , Record {uchar = '\120071', commands = [("eufrak","\\mathfrak{D}"),("unicode-math","\\mfrakD")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL D"}
  , Record {uchar = '\120072', commands = [("eufrak","\\mathfrak{E}"),("unicode-math","\\mfrakE")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL E"}
  , Record {uchar = '\120073', commands = [("eufrak","\\mathfrak{F}"),("unicode-math","\\mfrakF")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL F"}
  , Record {uchar = '\120074', commands = [("eufrak","\\mathfrak{G}"),("unicode-math","\\mfrakG")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL G"}
  , Record {uchar = '\120077', commands = [("eufrak","\\mathfrak{J}"),("unicode-math","\\mfrakJ")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL J"}
  , Record {uchar = '\120078', commands = [("eufrak","\\mathfrak{K}"),("unicode-math","\\mfrakK")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL K"}
  , Record {uchar = '\120079', commands = [("eufrak","\\mathfrak{L}"),("unicode-math","\\mfrakL")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL L"}
  , Record {uchar = '\120080', commands = [("eufrak","\\mathfrak{M}"),("unicode-math","\\mfrakM")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL M"}
  , Record {uchar = '\120081', commands = [("eufrak","\\mathfrak{N}"),("unicode-math","\\mfrakN")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL N"}
  , Record {uchar = '\120082', commands = [("eufrak","\\mathfrak{O}"),("unicode-math","\\mfrakO")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL O"}
  , Record {uchar = '\120083', commands = [("eufrak","\\mathfrak{P}"),("unicode-math","\\mfrakP")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL P"}
  , Record {uchar = '\120084', commands = [("eufrak","\\mathfrak{Q}"),("unicode-math","\\mfrakQ")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL Q"}
  , Record {uchar = '\120086', commands = [("eufrak","\\mathfrak{S}"),("unicode-math","\\mfrakS")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL S"}
  , Record {uchar = '\120087', commands = [("eufrak","\\mathfrak{T}"),("unicode-math","\\mfrakT")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL T"}
  , Record {uchar = '\120088', commands = [("eufrak","\\mathfrak{U}"),("unicode-math","\\mfrakU")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL U"}
  , Record {uchar = '\120089', commands = [("eufrak","\\mathfrak{V}"),("unicode-math","\\mfrakV")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL V"}
  , Record {uchar = '\120090', commands = [("eufrak","\\mathfrak{W}"),("unicode-math","\\mfrakW")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL W"}
  , Record {uchar = '\120091', commands = [("eufrak","\\mathfrak{X}"),("unicode-math","\\mfrakX")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL X"}
  , Record {uchar = '\120092', commands = [("eufrak","\\mathfrak{Y}"),("unicode-math","\\mfrakY")], category = Alpha, comments = "MATHEMATICAL FRAKTUR CAPITAL Y"}
  , Record {uchar = '\120094', commands = [("eufrak","\\mathfrak{a}"),("unicode-math","\\mfraka")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL A"}
  , Record {uchar = '\120095', commands = [("eufrak","\\mathfrak{b}"),("unicode-math","\\mfrakb")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL B"}
  , Record {uchar = '\120096', commands = [("eufrak","\\mathfrak{c}"),("unicode-math","\\mfrakc")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL C"}
  , Record {uchar = '\120097', commands = [("eufrak","\\mathfrak{d}"),("unicode-math","\\mfrakd")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL D"}
  , Record {uchar = '\120098', commands = [("eufrak","\\mathfrak{e}"),("unicode-math","\\mfrake")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL E"}
  , Record {uchar = '\120099', commands = [("eufrak","\\mathfrak{f}"),("unicode-math","\\mfrakf")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL F"}
  , Record {uchar = '\120100', commands = [("eufrak","\\mathfrak{g}"),("unicode-math","\\mfrakg")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL G"}
  , Record {uchar = '\120101', commands = [("eufrak","\\mathfrak{h}"),("unicode-math","\\mfrakh")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL H"}
  , Record {uchar = '\120102', commands = [("eufrak","\\mathfrak{i}"),("unicode-math","\\mfraki")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL I"}
  , Record {uchar = '\120103', commands = [("eufrak","\\mathfrak{j}"),("unicode-math","\\mfrakj")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL J"}
  , Record {uchar = '\120104', commands = [("eufrak","\\mathfrak{k}"),("unicode-math","\\mfrakk")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL K"}
  , Record {uchar = '\120105', commands = [("eufrak","\\mathfrak{l}"),("unicode-math","\\mfrakl")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL L"}
  , Record {uchar = '\120106', commands = [("eufrak","\\mathfrak{m}"),("unicode-math","\\mfrakm")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL M"}
  , Record {uchar = '\120107', commands = [("eufrak","\\mathfrak{n}"),("unicode-math","\\mfrakn")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL N"}
  , Record {uchar = '\120108', commands = [("eufrak","\\mathfrak{o}"),("unicode-math","\\mfrako")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL O"}
  , Record {uchar = '\120109', commands = [("eufrak","\\mathfrak{p}"),("unicode-math","\\mfrakp")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL P"}
  , Record {uchar = '\120110', commands = [("eufrak","\\mathfrak{q}"),("unicode-math","\\mfrakq")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL Q"}
  , Record {uchar = '\120111', commands = [("eufrak","\\mathfrak{r}"),("unicode-math","\\mfrakr")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL R"}
  , Record {uchar = '\120112', commands = [("eufrak","\\mathfrak{s}"),("unicode-math","\\mfraks")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL S"}
  , Record {uchar = '\120113', commands = [("eufrak","\\mathfrak{t}"),("unicode-math","\\mfrakt")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL T"}
  , Record {uchar = '\120114', commands = [("eufrak","\\mathfrak{u}"),("unicode-math","\\mfraku")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL U"}
  , Record {uchar = '\120115', commands = [("eufrak","\\mathfrak{v}"),("unicode-math","\\mfrakv")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL V"}
  , Record {uchar = '\120116', commands = [("eufrak","\\mathfrak{w}"),("unicode-math","\\mfrakw")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL W"}
  , Record {uchar = '\120117', commands = [("eufrak","\\mathfrak{x}"),("unicode-math","\\mfrakx")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL X"}
  , Record {uchar = '\120118', commands = [("eufrak","\\mathfrak{y}"),("unicode-math","\\mfraky")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL Y"}
  , Record {uchar = '\120119', commands = [("eufrak","\\mathfrak{z}"),("unicode-math","\\mfrakz")], category = Alpha, comments = "MATHEMATICAL FRAKTUR SMALL Z"}
  , Record {uchar = '\120120', commands = [("mathbb","\\mathbb{A}"),("dsfont","\\mathds{A}"),("unicode-math","\\BbbA")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK CAPITAL A"}
  , Record {uchar = '\120121', commands = [("mathbb","\\mathbb{B}"),("dsfont","\\mathds{B}"),("unicode-math","\\BbbB")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL B"}
  , Record {uchar = '\120123', commands = [("mathbb","\\mathbb{D}"),("dsfont","\\mathds{D}"),("unicode-math","\\BbbD")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL D"}
  , Record {uchar = '\120124', commands = [("mathbb","\\mathbb{E}"),("dsfont","\\mathds{E}"),("unicode-math","\\BbbE")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL E"}
  , Record {uchar = '\120125', commands = [("mathbb","\\mathbb{F}"),("dsfont","\\mathds{F}"),("unicode-math","\\BbbF")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL F"}
  , Record {uchar = '\120126', commands = [("mathbb","\\mathbb{G}"),("dsfont","\\mathds{G}"),("unicode-math","\\BbbG")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL G"}
  , Record {uchar = '\120128', commands = [("mathbb","\\mathbb{I}"),("dsfont","\\mathds{I}"),("unicode-math","\\BbbI")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL I"}
  , Record {uchar = '\120129', commands = [("mathbb","\\mathbb{J}"),("dsfont","\\mathds{J}"),("unicode-math","\\BbbJ")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL J"}
  , Record {uchar = '\120130', commands = [("mathbb","\\mathbb{K}"),("dsfont","\\mathds{K}"),("unicode-math","\\BbbK")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL K"}
  , Record {uchar = '\120131', commands = [("mathbb","\\mathbb{L}"),("dsfont","\\mathds{L}"),("unicode-math","\\BbbL")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL L"}
  , Record {uchar = '\120132', commands = [("mathbb","\\mathbb{M}"),("dsfont","\\mathds{M}"),("unicode-math","\\BbbM")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL M"}
  , Record {uchar = '\120134', commands = [("mathbb","\\mathbb{O}"),("dsfont","\\mathds{O}"),("unicode-math","\\BbbO")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL O"}
  , Record {uchar = '\120138', commands = [("mathbb","\\mathbb{S}"),("dsfont","\\mathds{S}"),("unicode-math","\\BbbS")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL S"}
  , Record {uchar = '\120139', commands = [("mathbb","\\mathbb{T}"),("dsfont","\\mathds{T}"),("unicode-math","\\BbbT")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL T"}
  , Record {uchar = '\120140', commands = [("mathbb","\\mathbb{U}"),("dsfont","\\mathds{U}"),("unicode-math","\\BbbU")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL U"}
  , Record {uchar = '\120141', commands = [("mathbb","\\mathbb{V}"),("dsfont","\\mathds{V}"),("unicode-math","\\BbbV")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL V"}
  , Record {uchar = '\120142', commands = [("mathbb","\\mathbb{W}"),("dsfont","\\mathds{W}"),("unicode-math","\\BbbW")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL W"}
  , Record {uchar = '\120143', commands = [("mathbb","\\mathbb{X}"),("dsfont","\\mathds{X}"),("unicode-math","\\BbbX")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL X"}
  , Record {uchar = '\120144', commands = [("mathbb","\\mathbb{Y}"),("dsfont","\\mathds{Y}"),("unicode-math","\\BbbY")], category = Alpha, comments = "matMATHEMATICAL DOUBLE-STRUCK CAPITAL Y"}
  , Record {uchar = '\120146', commands = [("bbold","\\mathbb{a}"),("unicode-math","\\Bbba")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL A"}
  , Record {uchar = '\120147', commands = [("bbold","\\mathbb{b}"),("unicode-math","\\Bbbb")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL B"}
  , Record {uchar = '\120148', commands = [("bbold","\\mathbb{c}"),("unicode-math","\\Bbbc")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL C"}
  , Record {uchar = '\120149', commands = [("bbold","\\mathbb{d}"),("unicode-math","\\Bbbd")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL D"}
  , Record {uchar = '\120150', commands = [("bbold","\\mathbb{e}"),("unicode-math","\\Bbbe")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL E"}
  , Record {uchar = '\120151', commands = [("bbold","\\mathbb{f}"),("unicode-math","\\Bbbf")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL F"}
  , Record {uchar = '\120152', commands = [("bbold","\\mathbb{g}"),("unicode-math","\\Bbbg")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL G"}
  , Record {uchar = '\120153', commands = [("bbold","\\mathbb{h}"),("unicode-math","\\Bbbh")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL H"}
  , Record {uchar = '\120154', commands = [("bbold","\\mathbb{i}"),("unicode-math","\\Bbbi")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL I"}
  , Record {uchar = '\120155', commands = [("bbold","\\mathbb{j}"),("unicode-math","\\Bbbj")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL J"}
  , Record {uchar = '\120156', commands = [("bbold","\\mathbb{k}"),("fourier","\\mathbb{k}"),("amssymb","\\Bbbk"),("unicode-math","\\Bbbk")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL K"}
  , Record {uchar = '\120157', commands = [("bbold","\\mathbb{l}"),("unicode-math","\\Bbbl")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL L"}
  , Record {uchar = '\120158', commands = [("bbold","\\mathbb{m}"),("unicode-math","\\Bbbm")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL M"}
  , Record {uchar = '\120159', commands = [("bbold","\\mathbb{n}"),("unicode-math","\\Bbbn")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL N"}
  , Record {uchar = '\120160', commands = [("bbold","\\mathbb{o}"),("unicode-math","\\Bbbo")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL O"}
  , Record {uchar = '\120161', commands = [("bbold","\\mathbb{p}"),("unicode-math","\\Bbbp")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL P"}
  , Record {uchar = '\120162', commands = [("bbold","\\mathbb{q}"),("unicode-math","\\Bbbq")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL Q"}
  , Record {uchar = '\120163', commands = [("bbold","\\mathbb{r}"),("unicode-math","\\Bbbr")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL R"}
  , Record {uchar = '\120164', commands = [("bbold","\\mathbb{s}"),("unicode-math","\\Bbbs")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL S"}
  , Record {uchar = '\120165', commands = [("bbold","\\mathbb{t}"),("unicode-math","\\Bbbt")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL T"}
  , Record {uchar = '\120166', commands = [("bbold","\\mathbb{u}"),("unicode-math","\\Bbbu")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL U"}
  , Record {uchar = '\120167', commands = [("bbold","\\mathbb{v}"),("unicode-math","\\Bbbv")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL V"}
  , Record {uchar = '\120168', commands = [("bbold","\\mathbb{w}"),("unicode-math","\\Bbbw")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL W"}
  , Record {uchar = '\120169', commands = [("bbold","\\mathbb{x}"),("unicode-math","\\Bbbx")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL X"}
  , Record {uchar = '\120170', commands = [("bbold","\\mathbb{y}"),("unicode-math","\\Bbby")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL Y"}
  , Record {uchar = '\120171', commands = [("bbold","\\mathbb{z}"),("unicode-math","\\Bbbz")], category = Alpha, comments = "MATHEMATICAL DOUBLE-STRUCK SMALL Z"}
  , Record {uchar = '\120172', commands = [("unicode-math","\\mbffrakA")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL A"}
  , Record {uchar = '\120173', commands = [("unicode-math","\\mbffrakB")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL B"}
  , Record {uchar = '\120174', commands = [("unicode-math","\\mbffrakC")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL C"}
  , Record {uchar = '\120175', commands = [("unicode-math","\\mbffrakD")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL D"}
  , Record {uchar = '\120176', commands = [("unicode-math","\\mbffrakE")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL E"}
  , Record {uchar = '\120177', commands = [("unicode-math","\\mbffrakF")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL F"}
  , Record {uchar = '\120178', commands = [("unicode-math","\\mbffrakG")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL G"}
  , Record {uchar = '\120179', commands = [("unicode-math","\\mbffrakH")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL H"}
  , Record {uchar = '\120180', commands = [("unicode-math","\\mbffrakI")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL I"}
  , Record {uchar = '\120181', commands = [("unicode-math","\\mbffrakJ")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL J"}
  , Record {uchar = '\120182', commands = [("unicode-math","\\mbffrakK")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL K"}
  , Record {uchar = '\120183', commands = [("unicode-math","\\mbffrakL")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL L"}
  , Record {uchar = '\120184', commands = [("unicode-math","\\mbffrakM")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL M"}
  , Record {uchar = '\120185', commands = [("unicode-math","\\mbffrakN")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL N"}
  , Record {uchar = '\120186', commands = [("unicode-math","\\mbffrakO")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL O"}
  , Record {uchar = '\120187', commands = [("unicode-math","\\mbffrakP")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL P"}
  , Record {uchar = '\120188', commands = [("unicode-math","\\mbffrakQ")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL Q"}
  , Record {uchar = '\120189', commands = [("unicode-math","\\mbffrakR")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL R"}
  , Record {uchar = '\120190', commands = [("unicode-math","\\mbffrakS")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL S"}
  , Record {uchar = '\120191', commands = [("unicode-math","\\mbffrakT")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL T"}
  , Record {uchar = '\120192', commands = [("unicode-math","\\mbffrakU")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL U"}
  , Record {uchar = '\120193', commands = [("unicode-math","\\mbffrakV")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL V"}
  , Record {uchar = '\120194', commands = [("unicode-math","\\mbffrakW")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL W"}
  , Record {uchar = '\120195', commands = [("unicode-math","\\mbffrakX")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL X"}
  , Record {uchar = '\120196', commands = [("unicode-math","\\mbffrakY")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL Y"}
  , Record {uchar = '\120197', commands = [("unicode-math","\\mbffrakZ")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL Z"}
  , Record {uchar = '\120198', commands = [("unicode-math","\\mbffraka")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL A"}
  , Record {uchar = '\120199', commands = [("unicode-math","\\mbffrakb")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL B"}
  , Record {uchar = '\120200', commands = [("unicode-math","\\mbffrakc")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL C"}
  , Record {uchar = '\120201', commands = [("unicode-math","\\mbffrakd")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL D"}
  , Record {uchar = '\120202', commands = [("unicode-math","\\mbffrake")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL E"}
  , Record {uchar = '\120203', commands = [("unicode-math","\\mbffrakf")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL F"}
  , Record {uchar = '\120204', commands = [("unicode-math","\\mbffrakg")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL G"}
  , Record {uchar = '\120205', commands = [("unicode-math","\\mbffrakh")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL H"}
  , Record {uchar = '\120206', commands = [("unicode-math","\\mbffraki")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL I"}
  , Record {uchar = '\120207', commands = [("unicode-math","\\mbffrakj")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL J"}
  , Record {uchar = '\120208', commands = [("unicode-math","\\mbffrakk")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL K"}
  , Record {uchar = '\120209', commands = [("unicode-math","\\mbffrakl")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL L"}
  , Record {uchar = '\120210', commands = [("unicode-math","\\mbffrakm")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL M"}
  , Record {uchar = '\120211', commands = [("unicode-math","\\mbffrakn")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL N"}
  , Record {uchar = '\120212', commands = [("unicode-math","\\mbffrako")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL O"}
  , Record {uchar = '\120213', commands = [("unicode-math","\\mbffrakp")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL P"}
  , Record {uchar = '\120214', commands = [("unicode-math","\\mbffrakq")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL Q"}
  , Record {uchar = '\120215', commands = [("unicode-math","\\mbffrakr")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL R"}
  , Record {uchar = '\120216', commands = [("unicode-math","\\mbffraks")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL S"}
  , Record {uchar = '\120217', commands = [("unicode-math","\\mbffrakt")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL T"}
  , Record {uchar = '\120218', commands = [("unicode-math","\\mbffraku")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL U"}
  , Record {uchar = '\120219', commands = [("unicode-math","\\mbffrakv")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL V"}
  , Record {uchar = '\120220', commands = [("unicode-math","\\mbffrakw")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL W"}
  , Record {uchar = '\120221', commands = [("unicode-math","\\mbffrakx")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL X"}
  , Record {uchar = '\120222', commands = [("unicode-math","\\mbffraky")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL Y"}
  , Record {uchar = '\120223', commands = [("unicode-math","\\mbffrakz")], category = Alpha, comments = "MATHEMATICAL BOLD FRAKTUR SMALL Z"}
  , Record {uchar = '\120224', commands = [("base","\\mathsf{A}"),("unicode-math","\\msansA")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL A"}
  , Record {uchar = '\120225', commands = [("base","\\mathsf{B}"),("unicode-math","\\msansB")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL B"}
  , Record {uchar = '\120226', commands = [("base","\\mathsf{C}"),("unicode-math","\\msansC")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL C"}
  , Record {uchar = '\120227', commands = [("base","\\mathsf{D}"),("unicode-math","\\msansD")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL D"}
  , Record {uchar = '\120228', commands = [("base","\\mathsf{E}"),("unicode-math","\\msansE")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL E"}
  , Record {uchar = '\120229', commands = [("base","\\mathsf{F}"),("unicode-math","\\msansF")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL F"}
  , Record {uchar = '\120230', commands = [("base","\\mathsf{G}"),("unicode-math","\\msansG")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL G"}
  , Record {uchar = '\120231', commands = [("base","\\mathsf{H}"),("unicode-math","\\msansH")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL H"}
  , Record {uchar = '\120232', commands = [("base","\\mathsf{I}"),("unicode-math","\\msansI")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL I"}
  , Record {uchar = '\120233', commands = [("base","\\mathsf{J}"),("unicode-math","\\msansJ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL J"}
  , Record {uchar = '\120234', commands = [("base","\\mathsf{K}"),("unicode-math","\\msansK")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL K"}
  , Record {uchar = '\120235', commands = [("base","\\mathsf{L}"),("unicode-math","\\msansL")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL L"}
  , Record {uchar = '\120236', commands = [("base","\\mathsf{M}"),("unicode-math","\\msansM")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL M"}
  , Record {uchar = '\120237', commands = [("base","\\mathsf{N}"),("unicode-math","\\msansN")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL N"}
  , Record {uchar = '\120238', commands = [("base","\\mathsf{O}"),("unicode-math","\\msansO")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL O"}
  , Record {uchar = '\120239', commands = [("base","\\mathsf{P}"),("unicode-math","\\msansP")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL P"}
  , Record {uchar = '\120240', commands = [("base","\\mathsf{Q}"),("unicode-math","\\msansQ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL Q"}
  , Record {uchar = '\120241', commands = [("base","\\mathsf{R}"),("unicode-math","\\msansR")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL R"}
  , Record {uchar = '\120242', commands = [("base","\\mathsf{S}"),("unicode-math","\\msansS")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL S"}
  , Record {uchar = '\120243', commands = [("base","\\mathsf{T}"),("unicode-math","\\msansT")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL T"}
  , Record {uchar = '\120244', commands = [("base","\\mathsf{U}"),("unicode-math","\\msansU")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL U"}
  , Record {uchar = '\120245', commands = [("base","\\mathsf{V}"),("unicode-math","\\msansV")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL V"}
  , Record {uchar = '\120246', commands = [("base","\\mathsf{W}"),("unicode-math","\\msansW")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL W"}
  , Record {uchar = '\120247', commands = [("base","\\mathsf{X}"),("unicode-math","\\msansX")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL X"}
  , Record {uchar = '\120248', commands = [("base","\\mathsf{Y}"),("unicode-math","\\msansY")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL Y"}
  , Record {uchar = '\120249', commands = [("base","\\mathsf{Z}"),("unicode-math","\\msansZ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF CAPITAL Z"}
  , Record {uchar = '\120250', commands = [("base","\\mathsf{a}"),("unicode-math","\\msansa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL A"}
  , Record {uchar = '\120251', commands = [("base","\\mathsf{b}"),("unicode-math","\\msansb")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL B"}
  , Record {uchar = '\120252', commands = [("base","\\mathsf{c}"),("unicode-math","\\msansc")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL C"}
  , Record {uchar = '\120253', commands = [("base","\\mathsf{d}"),("unicode-math","\\msansd")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL D"}
  , Record {uchar = '\120254', commands = [("base","\\mathsf{e}"),("unicode-math","\\msanse")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL E"}
  , Record {uchar = '\120255', commands = [("base","\\mathsf{f}"),("unicode-math","\\msansf")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL F"}
  , Record {uchar = '\120256', commands = [("base","\\mathsf{g}"),("unicode-math","\\msansg")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL G"}
  , Record {uchar = '\120257', commands = [("base","\\mathsf{h}"),("unicode-math","\\msansh")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL H"}
  , Record {uchar = '\120258', commands = [("base","\\mathsf{i}"),("unicode-math","\\msansi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL I"}
  , Record {uchar = '\120259', commands = [("base","\\mathsf{j}"),("unicode-math","\\msansj")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL J"}
  , Record {uchar = '\120260', commands = [("base","\\mathsf{k}"),("unicode-math","\\msansk")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL K"}
  , Record {uchar = '\120261', commands = [("base","\\mathsf{l}"),("unicode-math","\\msansl")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL L"}
  , Record {uchar = '\120262', commands = [("base","\\mathsf{m}"),("unicode-math","\\msansm")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL M"}
  , Record {uchar = '\120263', commands = [("base","\\mathsf{n}"),("unicode-math","\\msansn")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL N"}
  , Record {uchar = '\120264', commands = [("base","\\mathsf{o}"),("unicode-math","\\msanso")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL O"}
  , Record {uchar = '\120265', commands = [("base","\\mathsf{p}"),("unicode-math","\\msansp")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL P"}
  , Record {uchar = '\120266', commands = [("base","\\mathsf{q}"),("unicode-math","\\msansq")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL Q"}
  , Record {uchar = '\120267', commands = [("base","\\mathsf{r}"),("unicode-math","\\msansr")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL R"}
  , Record {uchar = '\120268', commands = [("base","\\mathsf{s}"),("unicode-math","\\msanss")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL S"}
  , Record {uchar = '\120269', commands = [("base","\\mathsf{t}"),("unicode-math","\\msanst")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL T"}
  , Record {uchar = '\120270', commands = [("base","\\mathsf{u}"),("unicode-math","\\msansu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL U"}
  , Record {uchar = '\120271', commands = [("base","\\mathsf{v}"),("unicode-math","\\msansv")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL V"}
  , Record {uchar = '\120272', commands = [("base","\\mathsf{w}"),("unicode-math","\\msansw")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL W"}
  , Record {uchar = '\120273', commands = [("base","\\mathsf{x}"),("unicode-math","\\msansx")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL X"}
  , Record {uchar = '\120274', commands = [("base","\\mathsf{y}"),("unicode-math","\\msansy")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL Y"}
  , Record {uchar = '\120275', commands = [("base","\\mathsf{z}"),("unicode-math","\\msansz")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF SMALL Z"}
  , Record {uchar = '\120276', commands = [("mathsfbf","\\mathsfbf{A}"),("unicode-math","\\mbfsansA")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL A"}
  , Record {uchar = '\120277', commands = [("mathsfbf","\\mathsfbf{B}"),("unicode-math","\\mbfsansB")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL B"}
  , Record {uchar = '\120278', commands = [("mathsfbf","\\mathsfbf{C}"),("unicode-math","\\mbfsansC")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL C"}
  , Record {uchar = '\120279', commands = [("mathsfbf","\\mathsfbf{D}"),("unicode-math","\\mbfsansD")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL D"}
  , Record {uchar = '\120280', commands = [("mathsfbf","\\mathsfbf{E}"),("unicode-math","\\mbfsansE")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL E"}
  , Record {uchar = '\120281', commands = [("mathsfbf","\\mathsfbf{F}"),("unicode-math","\\mbfsansF")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL F"}
  , Record {uchar = '\120282', commands = [("mathsfbf","\\mathsfbf{G}"),("unicode-math","\\mbfsansG")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL G"}
  , Record {uchar = '\120283', commands = [("mathsfbf","\\mathsfbf{H}"),("unicode-math","\\mbfsansH")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL H"}
  , Record {uchar = '\120284', commands = [("mathsfbf","\\mathsfbf{I}"),("unicode-math","\\mbfsansI")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL I"}
  , Record {uchar = '\120285', commands = [("mathsfbf","\\mathsfbf{J}"),("unicode-math","\\mbfsansJ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL J"}
  , Record {uchar = '\120286', commands = [("mathsfbf","\\mathsfbf{K}"),("unicode-math","\\mbfsansK")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL K"}
  , Record {uchar = '\120287', commands = [("mathsfbf","\\mathsfbf{L}"),("unicode-math","\\mbfsansL")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL L"}
  , Record {uchar = '\120288', commands = [("mathsfbf","\\mathsfbf{M}"),("unicode-math","\\mbfsansM")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL M"}
  , Record {uchar = '\120289', commands = [("mathsfbf","\\mathsfbf{N}"),("unicode-math","\\mbfsansN")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL N"}
  , Record {uchar = '\120290', commands = [("mathsfbf","\\mathsfbf{O}"),("unicode-math","\\mbfsansO")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL O"}
  , Record {uchar = '\120291', commands = [("mathsfbf","\\mathsfbf{P}"),("unicode-math","\\mbfsansP")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL P"}
  , Record {uchar = '\120292', commands = [("mathsfbf","\\mathsfbf{Q}"),("unicode-math","\\mbfsansQ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL Q"}
  , Record {uchar = '\120293', commands = [("mathsfbf","\\mathsfbf{R}"),("unicode-math","\\mbfsansR")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL R"}
  , Record {uchar = '\120294', commands = [("mathsfbf","\\mathsfbf{S}"),("unicode-math","\\mbfsansS")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL S"}
  , Record {uchar = '\120295', commands = [("mathsfbf","\\mathsfbf{T}"),("unicode-math","\\mbfsansT")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL T"}
  , Record {uchar = '\120296', commands = [("mathsfbf","\\mathsfbf{U}"),("unicode-math","\\mbfsansU")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL U"}
  , Record {uchar = '\120297', commands = [("mathsfbf","\\mathsfbf{V}"),("unicode-math","\\mbfsansV")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL V"}
  , Record {uchar = '\120298', commands = [("mathsfbf","\\mathsfbf{W}"),("unicode-math","\\mbfsansW")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL W"}
  , Record {uchar = '\120299', commands = [("mathsfbf","\\mathsfbf{X}"),("unicode-math","\\mbfsansX")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL X"}
  , Record {uchar = '\120300', commands = [("mathsfbf","\\mathsfbf{Y}"),("unicode-math","\\mbfsansY")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL Y"}
  , Record {uchar = '\120301', commands = [("mathsfbf","\\mathsfbf{Z}"),("unicode-math","\\mbfsansZ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL Z"}
  , Record {uchar = '\120302', commands = [("mathsfbf","\\mathsfbf{a}"),("unicode-math","\\mbfsansa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL A"}
  , Record {uchar = '\120303', commands = [("mathsfbf","\\mathsfbf{b}"),("unicode-math","\\mbfsansb")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL B"}
  , Record {uchar = '\120304', commands = [("mathsfbf","\\mathsfbf{c}"),("unicode-math","\\mbfsansc")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL C"}
  , Record {uchar = '\120305', commands = [("mathsfbf","\\mathsfbf{d}"),("unicode-math","\\mbfsansd")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL D"}
  , Record {uchar = '\120306', commands = [("mathsfbf","\\mathsfbf{e}"),("unicode-math","\\mbfsanse")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL E"}
  , Record {uchar = '\120307', commands = [("mathsfbf","\\mathsfbf{f}"),("unicode-math","\\mbfsansf")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL F"}
  , Record {uchar = '\120308', commands = [("mathsfbf","\\mathsfbf{g}"),("unicode-math","\\mbfsansg")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL G"}
  , Record {uchar = '\120309', commands = [("mathsfbf","\\mathsfbf{h}"),("unicode-math","\\mbfsansh")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL H"}
  , Record {uchar = '\120310', commands = [("mathsfbf","\\mathsfbf{i}"),("unicode-math","\\mbfsansi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL I"}
  , Record {uchar = '\120311', commands = [("mathsfbf","\\mathsfbf{j}"),("unicode-math","\\mbfsansj")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL J"}
  , Record {uchar = '\120312', commands = [("mathsfbf","\\mathsfbf{k}"),("unicode-math","\\mbfsansk")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL K"}
  , Record {uchar = '\120313', commands = [("mathsfbf","\\mathsfbf{l}"),("unicode-math","\\mbfsansl")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL L"}
  , Record {uchar = '\120314', commands = [("mathsfbf","\\mathsfbf{m}"),("unicode-math","\\mbfsansm")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL M"}
  , Record {uchar = '\120315', commands = [("mathsfbf","\\mathsfbf{n}"),("unicode-math","\\mbfsansn")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL N"}
  , Record {uchar = '\120316', commands = [("mathsfbf","\\mathsfbf{o}"),("unicode-math","\\mbfsanso")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL O"}
  , Record {uchar = '\120317', commands = [("mathsfbf","\\mathsfbf{p}"),("unicode-math","\\mbfsansp")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL P"}
  , Record {uchar = '\120318', commands = [("mathsfbf","\\mathsfbf{q}"),("unicode-math","\\mbfsansq")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL Q"}
  , Record {uchar = '\120319', commands = [("mathsfbf","\\mathsfbf{r}"),("unicode-math","\\mbfsansr")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL R"}
  , Record {uchar = '\120320', commands = [("mathsfbf","\\mathsfbf{s}"),("unicode-math","\\mbfsanss")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL S"}
  , Record {uchar = '\120321', commands = [("mathsfbf","\\mathsfbf{t}"),("unicode-math","\\mbfsanst")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL T"}
  , Record {uchar = '\120322', commands = [("mathsfbf","\\mathsfbf{u}"),("unicode-math","\\mbfsansu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL U"}
  , Record {uchar = '\120323', commands = [("mathsfbf","\\mathsfbf{v}"),("unicode-math","\\mbfsansv")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL V"}
  , Record {uchar = '\120324', commands = [("mathsfbf","\\mathsfbf{w}"),("unicode-math","\\mbfsansw")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL W"}
  , Record {uchar = '\120325', commands = [("mathsfbf","\\mathsfbf{x}"),("unicode-math","\\mbfsansx")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL X"}
  , Record {uchar = '\120326', commands = [("mathsfbf","\\mathsfbf{y}"),("unicode-math","\\mbfsansy")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL Y"}
  , Record {uchar = '\120327', commands = [("mathsfbf","\\mathsfbf{z}"),("unicode-math","\\mbfsansz")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL Z"}
  , Record {uchar = '\120328', commands = [("omlmathsfit","\\mathsfit{A}"),("unicode-math","\\mitsansA")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL A"}
  , Record {uchar = '\120329', commands = [("omlmathsfit","\\mathsfit{B}"),("unicode-math","\\mitsansB")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL B"}
  , Record {uchar = '\120330', commands = [("omlmathsfit","\\mathsfit{C}"),("unicode-math","\\mitsansC")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL C"}
  , Record {uchar = '\120331', commands = [("omlmathsfit","\\mathsfit{D}"),("unicode-math","\\mitsansD")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL D"}
  , Record {uchar = '\120332', commands = [("omlmathsfit","\\mathsfit{E}"),("unicode-math","\\mitsansE")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL E"}
  , Record {uchar = '\120333', commands = [("omlmathsfit","\\mathsfit{F}"),("unicode-math","\\mitsansF")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL F"}
  , Record {uchar = '\120334', commands = [("omlmathsfit","\\mathsfit{G}"),("unicode-math","\\mitsansG")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL G"}
  , Record {uchar = '\120335', commands = [("omlmathsfit","\\mathsfit{H}"),("unicode-math","\\mitsansH")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL H"}
  , Record {uchar = '\120336', commands = [("omlmathsfit","\\mathsfit{I}"),("unicode-math","\\mitsansI")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL I"}
  , Record {uchar = '\120337', commands = [("omlmathsfit","\\mathsfit{J}"),("unicode-math","\\mitsansJ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL J"}
  , Record {uchar = '\120338', commands = [("omlmathsfit","\\mathsfit{K}"),("unicode-math","\\mitsansK")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL K"}
  , Record {uchar = '\120339', commands = [("omlmathsfit","\\mathsfit{L}"),("unicode-math","\\mitsansL")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL L"}
  , Record {uchar = '\120340', commands = [("omlmathsfit","\\mathsfit{M}"),("unicode-math","\\mitsansM")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL M"}
  , Record {uchar = '\120341', commands = [("omlmathsfit","\\mathsfit{N}"),("unicode-math","\\mitsansN")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL N"}
  , Record {uchar = '\120342', commands = [("omlmathsfit","\\mathsfit{O}"),("unicode-math","\\mitsansO")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL O"}
  , Record {uchar = '\120343', commands = [("omlmathsfit","\\mathsfit{P}"),("unicode-math","\\mitsansP")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL P"}
  , Record {uchar = '\120344', commands = [("omlmathsfit","\\mathsfit{Q}"),("unicode-math","\\mitsansQ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL Q"}
  , Record {uchar = '\120345', commands = [("omlmathsfit","\\mathsfit{R}"),("unicode-math","\\mitsansR")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL R"}
  , Record {uchar = '\120346', commands = [("omlmathsfit","\\mathsfit{S}"),("unicode-math","\\mitsansS")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL S"}
  , Record {uchar = '\120347', commands = [("omlmathsfit","\\mathsfit{T}"),("unicode-math","\\mitsansT")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL T"}
  , Record {uchar = '\120348', commands = [("omlmathsfit","\\mathsfit{U}"),("unicode-math","\\mitsansU")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL U"}
  , Record {uchar = '\120349', commands = [("omlmathsfit","\\mathsfit{V}"),("unicode-math","\\mitsansV")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL V"}
  , Record {uchar = '\120350', commands = [("omlmathsfit","\\mathsfit{W}"),("unicode-math","\\mitsansW")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL W"}
  , Record {uchar = '\120351', commands = [("omlmathsfit","\\mathsfit{X}"),("unicode-math","\\mitsansX")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL X"}
  , Record {uchar = '\120352', commands = [("omlmathsfit","\\mathsfit{Y}"),("unicode-math","\\mitsansY")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL Y"}
  , Record {uchar = '\120353', commands = [("omlmathsfit","\\mathsfit{Z}"),("unicode-math","\\mitsansZ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL Z"}
  , Record {uchar = '\120354', commands = [("omlmathsfit","\\mathsfit{a}"),("unicode-math","\\mitsansa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL A"}
  , Record {uchar = '\120355', commands = [("omlmathsfit","\\mathsfit{b}"),("unicode-math","\\mitsansb")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL B"}
  , Record {uchar = '\120356', commands = [("omlmathsfit","\\mathsfit{c}"),("unicode-math","\\mitsansc")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL C"}
  , Record {uchar = '\120357', commands = [("omlmathsfit","\\mathsfit{d}"),("unicode-math","\\mitsansd")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL D"}
  , Record {uchar = '\120358', commands = [("omlmathsfit","\\mathsfit{e}"),("unicode-math","\\mitsanse")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL E"}
  , Record {uchar = '\120359', commands = [("omlmathsfit","\\mathsfit{f}"),("unicode-math","\\mitsansf")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL F"}
  , Record {uchar = '\120360', commands = [("omlmathsfit","\\mathsfit{g}"),("unicode-math","\\mitsansg")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL G"}
  , Record {uchar = '\120361', commands = [("omlmathsfit","\\mathsfit{h}"),("unicode-math","\\mitsansh")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL H"}
  , Record {uchar = '\120362', commands = [("omlmathsfit","\\mathsfit{i}"),("unicode-math","\\mitsansi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL I"}
  , Record {uchar = '\120363', commands = [("omlmathsfit","\\mathsfit{j}"),("unicode-math","\\mitsansj")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL J"}
  , Record {uchar = '\120364', commands = [("omlmathsfit","\\mathsfit{k}"),("unicode-math","\\mitsansk")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL K"}
  , Record {uchar = '\120365', commands = [("omlmathsfit","\\mathsfit{l}"),("unicode-math","\\mitsansl")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL L"}
  , Record {uchar = '\120366', commands = [("omlmathsfit","\\mathsfit{m}"),("unicode-math","\\mitsansm")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL M"}
  , Record {uchar = '\120367', commands = [("omlmathsfit","\\mathsfit{n}"),("unicode-math","\\mitsansn")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL N"}
  , Record {uchar = '\120368', commands = [("omlmathsfit","\\mathsfit{o}"),("unicode-math","\\mitsanso")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL O"}
  , Record {uchar = '\120369', commands = [("omlmathsfit","\\mathsfit{p}"),("unicode-math","\\mitsansp")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL P"}
  , Record {uchar = '\120370', commands = [("omlmathsfit","\\mathsfit{q}"),("unicode-math","\\mitsansq")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL Q"}
  , Record {uchar = '\120371', commands = [("omlmathsfit","\\mathsfit{r}"),("unicode-math","\\mitsansr")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL R"}
  , Record {uchar = '\120372', commands = [("omlmathsfit","\\mathsfit{s}"),("unicode-math","\\mitsanss")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL S"}
  , Record {uchar = '\120373', commands = [("omlmathsfit","\\mathsfit{t}"),("unicode-math","\\mitsanst")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL T"}
  , Record {uchar = '\120374', commands = [("omlmathsfit","\\mathsfit{u}"),("unicode-math","\\mitsansu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL U"}
  , Record {uchar = '\120375', commands = [("omlmathsfit","\\mathsfit{v}"),("unicode-math","\\mitsansv")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL V"}
  , Record {uchar = '\120376', commands = [("omlmathsfit","\\mathsfit{w}"),("unicode-math","\\mitsansw")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL W"}
  , Record {uchar = '\120377', commands = [("omlmathsfit","\\mathsfit{x}"),("unicode-math","\\mitsansx")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL X"}
  , Record {uchar = '\120378', commands = [("omlmathsfit","\\mathsfit{y}"),("unicode-math","\\mitsansy")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL Y"}
  , Record {uchar = '\120379', commands = [("omlmathsfit","\\mathsfit{z}"),("unicode-math","\\mitsansz")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL Z"}
  , Record {uchar = '\120380', commands = [("isomath","\\mathsfbfit{A}"),("unicode-math","\\mbfitsansA")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL A"}
  , Record {uchar = '\120381', commands = [("isomath","\\mathsfbfit{B}"),("unicode-math","\\mbfitsansB")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL B"}
  , Record {uchar = '\120382', commands = [("isomath","\\mathsfbfit{C}"),("unicode-math","\\mbfitsansC")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL C"}
  , Record {uchar = '\120383', commands = [("isomath","\\mathsfbfit{D}"),("unicode-math","\\mbfitsansD")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL D"}
  , Record {uchar = '\120384', commands = [("isomath","\\mathsfbfit{E}"),("unicode-math","\\mbfitsansE")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL E"}
  , Record {uchar = '\120385', commands = [("isomath","\\mathsfbfit{F}"),("unicode-math","\\mbfitsansF")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL F"}
  , Record {uchar = '\120386', commands = [("isomath","\\mathsfbfit{G}"),("unicode-math","\\mbfitsansG")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL G"}
  , Record {uchar = '\120387', commands = [("isomath","\\mathsfbfit{H}"),("unicode-math","\\mbfitsansH")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL H"}
  , Record {uchar = '\120388', commands = [("isomath","\\mathsfbfit{I}"),("unicode-math","\\mbfitsansI")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL I"}
  , Record {uchar = '\120389', commands = [("isomath","\\mathsfbfit{J}"),("unicode-math","\\mbfitsansJ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL J"}
  , Record {uchar = '\120390', commands = [("isomath","\\mathsfbfit{K}"),("unicode-math","\\mbfitsansK")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL K"}
  , Record {uchar = '\120391', commands = [("isomath","\\mathsfbfit{L}"),("unicode-math","\\mbfitsansL")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL L"}
  , Record {uchar = '\120392', commands = [("isomath","\\mathsfbfit{M}"),("unicode-math","\\mbfitsansM")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL M"}
  , Record {uchar = '\120393', commands = [("isomath","\\mathsfbfit{N}"),("unicode-math","\\mbfitsansN")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL N"}
  , Record {uchar = '\120394', commands = [("isomath","\\mathsfbfit{O}"),("unicode-math","\\mbfitsansO")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL O"}
  , Record {uchar = '\120395', commands = [("isomath","\\mathsfbfit{P}"),("unicode-math","\\mbfitsansP")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL P"}
  , Record {uchar = '\120396', commands = [("isomath","\\mathsfbfit{Q}"),("unicode-math","\\mbfitsansQ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Q"}
  , Record {uchar = '\120397', commands = [("isomath","\\mathsfbfit{R}"),("unicode-math","\\mbfitsansR")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL R"}
  , Record {uchar = '\120398', commands = [("isomath","\\mathsfbfit{S}"),("unicode-math","\\mbfitsansS")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL S"}
  , Record {uchar = '\120399', commands = [("isomath","\\mathsfbfit{T}"),("unicode-math","\\mbfitsansT")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL T"}
  , Record {uchar = '\120400', commands = [("isomath","\\mathsfbfit{U}"),("unicode-math","\\mbfitsansU")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL U"}
  , Record {uchar = '\120401', commands = [("isomath","\\mathsfbfit{V}"),("unicode-math","\\mbfitsansV")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL V"}
  , Record {uchar = '\120402', commands = [("isomath","\\mathsfbfit{W}"),("unicode-math","\\mbfitsansW")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL W"}
  , Record {uchar = '\120403', commands = [("isomath","\\mathsfbfit{X}"),("unicode-math","\\mbfitsansX")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL X"}
  , Record {uchar = '\120404', commands = [("isomath","\\mathsfbfit{Y}"),("unicode-math","\\mbfitsansY")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Y"}
  , Record {uchar = '\120405', commands = [("isomath","\\mathsfbfit{Z}"),("unicode-math","\\mbfitsansZ")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Z"}
  , Record {uchar = '\120406', commands = [("isomath","\\mathsfbfit{a}"),("unicode-math","\\mbfitsansa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL A"}
  , Record {uchar = '\120407', commands = [("isomath","\\mathsfbfit{b}"),("unicode-math","\\mbfitsansb")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL B"}
  , Record {uchar = '\120408', commands = [("isomath","\\mathsfbfit{c}"),("unicode-math","\\mbfitsansc")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL C"}
  , Record {uchar = '\120409', commands = [("isomath","\\mathsfbfit{d}"),("unicode-math","\\mbfitsansd")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL D"}
  , Record {uchar = '\120410', commands = [("isomath","\\mathsfbfit{e}"),("unicode-math","\\mbfitsanse")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL E"}
  , Record {uchar = '\120411', commands = [("isomath","\\mathsfbfit{f}"),("unicode-math","\\mbfitsansf")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL F"}
  , Record {uchar = '\120412', commands = [("isomath","\\mathsfbfit{g}"),("unicode-math","\\mbfitsansg")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL G"}
  , Record {uchar = '\120413', commands = [("isomath","\\mathsfbfit{h}"),("unicode-math","\\mbfitsansh")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL H"}
  , Record {uchar = '\120414', commands = [("isomath","\\mathsfbfit{i}"),("unicode-math","\\mbfitsansi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL I"}
  , Record {uchar = '\120415', commands = [("isomath","\\mathsfbfit{j}"),("unicode-math","\\mbfitsansj")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL J"}
  , Record {uchar = '\120416', commands = [("isomath","\\mathsfbfit{k}"),("unicode-math","\\mbfitsansk")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL K"}
  , Record {uchar = '\120417', commands = [("isomath","\\mathsfbfit{l}"),("unicode-math","\\mbfitsansl")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL L"}
  , Record {uchar = '\120418', commands = [("isomath","\\mathsfbfit{m}"),("unicode-math","\\mbfitsansm")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL M"}
  , Record {uchar = '\120419', commands = [("isomath","\\mathsfbfit{n}"),("unicode-math","\\mbfitsansn")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL N"}
  , Record {uchar = '\120420', commands = [("isomath","\\mathsfbfit{o}"),("unicode-math","\\mbfitsanso")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL O"}
  , Record {uchar = '\120421', commands = [("isomath","\\mathsfbfit{p}"),("unicode-math","\\mbfitsansp")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL P"}
  , Record {uchar = '\120422', commands = [("isomath","\\mathsfbfit{q}"),("unicode-math","\\mbfitsansq")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Q"}
  , Record {uchar = '\120423', commands = [("isomath","\\mathsfbfit{r}"),("unicode-math","\\mbfitsansr")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL R"}
  , Record {uchar = '\120424', commands = [("isomath","\\mathsfbfit{s}"),("unicode-math","\\mbfitsanss")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL S"}
  , Record {uchar = '\120425', commands = [("isomath","\\mathsfbfit{t}"),("unicode-math","\\mbfitsanst")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL T"}
  , Record {uchar = '\120426', commands = [("isomath","\\mathsfbfit{u}"),("unicode-math","\\mbfitsansu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL U"}
  , Record {uchar = '\120427', commands = [("isomath","\\mathsfbfit{v}"),("unicode-math","\\mbfitsansv")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL V"}
  , Record {uchar = '\120428', commands = [("isomath","\\mathsfbfit{w}"),("unicode-math","\\mbfitsansw")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL W"}
  , Record {uchar = '\120429', commands = [("isomath","\\mathsfbfit{x}"),("unicode-math","\\mbfitsansx")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL X"}
  , Record {uchar = '\120430', commands = [("isomath","\\mathsfbfit{y}"),("unicode-math","\\mbfitsansy")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Y"}
  , Record {uchar = '\120431', commands = [("isomath","\\mathsfbfit{z}"),("unicode-math","\\mbfitsansz")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Z"}
  , Record {uchar = '\120432', commands = [("base","\\mathtt{A}"),("unicode-math","\\mttA")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL A"}
  , Record {uchar = '\120433', commands = [("base","\\mathtt{B}"),("unicode-math","\\mttB")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL B"}
  , Record {uchar = '\120434', commands = [("base","\\mathtt{C}"),("unicode-math","\\mttC")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL C"}
  , Record {uchar = '\120435', commands = [("base","\\mathtt{D}"),("unicode-math","\\mttD")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL D"}
  , Record {uchar = '\120436', commands = [("base","\\mathtt{E}"),("unicode-math","\\mttE")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL E"}
  , Record {uchar = '\120437', commands = [("base","\\mathtt{F}"),("unicode-math","\\mttF")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL F"}
  , Record {uchar = '\120438', commands = [("base","\\mathtt{G}"),("unicode-math","\\mttG")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL G"}
  , Record {uchar = '\120439', commands = [("base","\\mathtt{H}"),("unicode-math","\\mttH")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL H"}
  , Record {uchar = '\120440', commands = [("base","\\mathtt{I}"),("unicode-math","\\mttI")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL I"}
  , Record {uchar = '\120441', commands = [("base","\\mathtt{J}"),("unicode-math","\\mttJ")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL J"}
  , Record {uchar = '\120442', commands = [("base","\\mathtt{K}"),("unicode-math","\\mttK")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL K"}
  , Record {uchar = '\120443', commands = [("base","\\mathtt{L}"),("unicode-math","\\mttL")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL L"}
  , Record {uchar = '\120444', commands = [("base","\\mathtt{M}"),("unicode-math","\\mttM")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL M"}
  , Record {uchar = '\120445', commands = [("base","\\mathtt{N}"),("unicode-math","\\mttN")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL N"}
  , Record {uchar = '\120446', commands = [("base","\\mathtt{O}"),("unicode-math","\\mttO")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL O"}
  , Record {uchar = '\120447', commands = [("base","\\mathtt{P}"),("unicode-math","\\mttP")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL P"}
  , Record {uchar = '\120448', commands = [("base","\\mathtt{Q}"),("unicode-math","\\mttQ")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL Q"}
  , Record {uchar = '\120449', commands = [("base","\\mathtt{R}"),("unicode-math","\\mttR")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL R"}
  , Record {uchar = '\120450', commands = [("base","\\mathtt{S}"),("unicode-math","\\mttS")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL S"}
  , Record {uchar = '\120451', commands = [("base","\\mathtt{T}"),("unicode-math","\\mttT")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL T"}
  , Record {uchar = '\120452', commands = [("base","\\mathtt{U}"),("unicode-math","\\mttU")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL U"}
  , Record {uchar = '\120453', commands = [("base","\\mathtt{V}"),("unicode-math","\\mttV")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL V"}
  , Record {uchar = '\120454', commands = [("base","\\mathtt{W}"),("unicode-math","\\mttW")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL W"}
  , Record {uchar = '\120455', commands = [("base","\\mathtt{X}"),("unicode-math","\\mttX")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL X"}
  , Record {uchar = '\120456', commands = [("base","\\mathtt{Y}"),("unicode-math","\\mttY")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL Y"}
  , Record {uchar = '\120457', commands = [("base","\\mathtt{Z}"),("unicode-math","\\mttZ")], category = Alpha, comments = "MATHEMATICAL MONOSPACE CAPITAL Z"}
  , Record {uchar = '\120458', commands = [("base","\\mathtt{a}"),("unicode-math","\\mtta")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL A"}
  , Record {uchar = '\120459', commands = [("base","\\mathtt{b}"),("unicode-math","\\mttb")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL B"}
  , Record {uchar = '\120460', commands = [("base","\\mathtt{c}"),("unicode-math","\\mttc")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL C"}
  , Record {uchar = '\120461', commands = [("base","\\mathtt{d}"),("unicode-math","\\mttd")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL D"}
  , Record {uchar = '\120462', commands = [("base","\\mathtt{e}"),("unicode-math","\\mtte")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL E"}
  , Record {uchar = '\120463', commands = [("base","\\mathtt{f}"),("unicode-math","\\mttf")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL F"}
  , Record {uchar = '\120464', commands = [("base","\\mathtt{g}"),("unicode-math","\\mttg")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL G"}
  , Record {uchar = '\120465', commands = [("base","\\mathtt{h}"),("unicode-math","\\mtth")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL H"}
  , Record {uchar = '\120466', commands = [("base","\\mathtt{i}"),("unicode-math","\\mtti")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL I"}
  , Record {uchar = '\120467', commands = [("base","\\mathtt{j}"),("unicode-math","\\mttj")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL J"}
  , Record {uchar = '\120468', commands = [("base","\\mathtt{k}"),("unicode-math","\\mttk")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL K"}
  , Record {uchar = '\120469', commands = [("base","\\mathtt{l}"),("unicode-math","\\mttl")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL L"}
  , Record {uchar = '\120470', commands = [("base","\\mathtt{m}"),("unicode-math","\\mttm")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL M"}
  , Record {uchar = '\120471', commands = [("base","\\mathtt{n}"),("unicode-math","\\mttn")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL N"}
  , Record {uchar = '\120472', commands = [("base","\\mathtt{o}"),("unicode-math","\\mtto")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL O"}
  , Record {uchar = '\120473', commands = [("base","\\mathtt{p}"),("unicode-math","\\mttp")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL P"}
  , Record {uchar = '\120474', commands = [("base","\\mathtt{q}"),("unicode-math","\\mttq")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL Q"}
  , Record {uchar = '\120475', commands = [("base","\\mathtt{r}"),("unicode-math","\\mttr")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL R"}
  , Record {uchar = '\120476', commands = [("base","\\mathtt{s}"),("unicode-math","\\mtts")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL S"}
  , Record {uchar = '\120477', commands = [("base","\\mathtt{t}"),("unicode-math","\\mttt")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL T"}
  , Record {uchar = '\120478', commands = [("base","\\mathtt{u}"),("unicode-math","\\mttu")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL U"}
  , Record {uchar = '\120479', commands = [("base","\\mathtt{v}"),("unicode-math","\\mttv")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL V"}
  , Record {uchar = '\120480', commands = [("base","\\mathtt{w}"),("unicode-math","\\mttw")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL W"}
  , Record {uchar = '\120481', commands = [("base","\\mathtt{x}"),("unicode-math","\\mttx")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL X"}
  , Record {uchar = '\120482', commands = [("base","\\mathtt{y}"),("unicode-math","\\mtty")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL Y"}
  , Record {uchar = '\120483', commands = [("base","\\mathtt{z}"),("unicode-math","\\mttz")], category = Alpha, comments = "MATHEMATICAL MONOSPACE SMALL Z"}
  , Record {uchar = '\120484', commands = [("base","\\imath"),("unicode-math","\\imath")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL DOTLESS I"}
  , Record {uchar = '\120485', commands = [("base","\\jmath"),("unicode-math","\\jmath")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL DOTLESS J"}
  , Record {uchar = '\120488', commands = [("unicode-math","\\mbfAlpha")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL ALPHA"}
  , Record {uchar = '\120489', commands = [("unicode-math","\\mbfBeta")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL BETA"}
  , Record {uchar = '\120490', commands = [("base","\\mathbf{\\Gamma}"),("unicode-math","\\mbfGamma")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL GAMMA"}
  , Record {uchar = '\120491', commands = [("base","\\mathbf{\\Delta}"),("unicode-math","\\mbfDelta")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL DELTA"}
  , Record {uchar = '\120492', commands = [("unicode-math","\\mbfEpsilon")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL EPSILON"}
  , Record {uchar = '\120493', commands = [("unicode-math","\\mbfZeta")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL ZETA"}
  , Record {uchar = '\120494', commands = [("unicode-math","\\mbfEta")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL ETA"}
  , Record {uchar = '\120495', commands = [("base","\\mathbf{\\Theta}"),("unicode-math","\\mbfTheta")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL THETA"}
  , Record {uchar = '\120496', commands = [("unicode-math","\\mbfIota")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL IOTA"}
  , Record {uchar = '\120497', commands = [("unicode-math","\\mbfKappa")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL KAPPA"}
  , Record {uchar = '\120498', commands = [("base","\\mathbf{\\Lambda}"),("unicode-math","\\mbfLambda")], category = Alpha, comments = "mathematical bold capital lambda"}
  , Record {uchar = '\120499', commands = [("unicode-math","\\mbfMu")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL MU"}
  , Record {uchar = '\120500', commands = [("unicode-math","\\mbfNu")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL NU"}
  , Record {uchar = '\120501', commands = [("base","\\mathbf{\\Xi}"),("unicode-math","\\mbfXi")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL XI"}
  , Record {uchar = '\120502', commands = [("unicode-math","\\mbfOmicron")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL OMICRON"}
  , Record {uchar = '\120503', commands = [("base","\\mathbf{\\Pi}"),("unicode-math","\\mbfPi")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL PI"}
  , Record {uchar = '\120504', commands = [("unicode-math","\\mbfRho")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL RHO"}
  , Record {uchar = '\120505', commands = [("unicode-math","\\mbfvarTheta")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL THETA SYMBOL"}
  , Record {uchar = '\120506', commands = [("base","\\mathbf{\\Sigma}"),("unicode-math","\\mbfSigma")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL SIGMA"}
  , Record {uchar = '\120507', commands = [("unicode-math","\\mbfTau")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL TAU"}
  , Record {uchar = '\120508', commands = [("base","\\mathbf{\\Upsilon}"),("unicode-math","\\mbfUpsilon")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL UPSILON"}
  , Record {uchar = '\120509', commands = [("base","\\mathbf{\\Phi}"),("unicode-math","\\mbfPhi")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL PHI"}
  , Record {uchar = '\120510', commands = [("unicode-math","\\mbfChi")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL CHI"}
  , Record {uchar = '\120511', commands = [("base","\\mathbf{\\Psi}"),("unicode-math","\\mbfPsi")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL PSI"}
  , Record {uchar = '\120512', commands = [("base","\\mathbf{\\Omega}"),("unicode-math","\\mbfOmega")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL OMEGA"}
  , Record {uchar = '\120513', commands = [("unicode-math","\\mbfnabla")], category = Ord, comments = "MATHEMATICAL BOLD NABLA"}
  , Record {uchar = '\120514', commands = [("omlmathbf","\\mathbf{\\alpha}"),("unicode-math","\\mbfalpha")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL ALPHA"}
  , Record {uchar = '\120515', commands = [("omlmathbf","\\mathbf{\\beta}"),("unicode-math","\\mbfbeta")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL BETA"}
  , Record {uchar = '\120516', commands = [("omlmathbf","\\mathbf{\\gamma}"),("unicode-math","\\mbfgamma")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL GAMMA"}
  , Record {uchar = '\120517', commands = [("omlmathbf","\\mathbf{\\delta}"),("unicode-math","\\mbfdelta")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL DELTA"}
  , Record {uchar = '\120518', commands = [("omlmathbf","\\mathbf{\\varepsilon}"),("unicode-math","\\mbfepsilon")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL EPSILON"}
  , Record {uchar = '\120519', commands = [("omlmathbf","\\mathbf{\\zeta}"),("unicode-math","\\mbfzeta")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL ZETA"}
  , Record {uchar = '\120520', commands = [("omlmathbf","\\mathbf{\\eta}"),("unicode-math","\\mbfeta")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL ETA"}
  , Record {uchar = '\120521', commands = [("omlmathbf","\\mathbf{\\theta}"),("unicode-math","\\mbftheta")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL THETA"}
  , Record {uchar = '\120522', commands = [("omlmathbf","\\mathbf{\\iota}"),("unicode-math","\\mbfiota")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL IOTA"}
  , Record {uchar = '\120523', commands = [("omlmathbf","\\mathbf{\\kappa}"),("unicode-math","\\mbfkappa")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL KAPPA"}
  , Record {uchar = '\120524', commands = [("omlmathbf","\\mathbf{\\lambda}"),("unicode-math","\\mbflambda")], category = Alpha, comments = "mathematical bold small lambda"}
  , Record {uchar = '\120525', commands = [("omlmathbf","\\mathbf{\\mu}"),("unicode-math","\\mbfmu")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL MU"}
  , Record {uchar = '\120526', commands = [("omlmathbf","\\mathbf{\\nu}"),("unicode-math","\\mbfnu")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL NU"}
  , Record {uchar = '\120527', commands = [("omlmathbf","\\mathbf{\\xi}"),("unicode-math","\\mbfxi")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL XI"}
  , Record {uchar = '\120528', commands = [("unicode-math","\\mbfomicron")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL OMICRON"}
  , Record {uchar = '\120529', commands = [("omlmathbf","\\mathbf{\\pi}"),("unicode-math","\\mbfpi")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL PI"}
  , Record {uchar = '\120530', commands = [("omlmathbf","\\mathbf{\\rho}"),("unicode-math","\\mbfrho")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL RHO"}
  , Record {uchar = '\120531', commands = [("omlmathbf","\\mathbf{\\varsigma}"),("unicode-math","\\mbfvarsigma")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL FINAL SIGMA"}
  , Record {uchar = '\120532', commands = [("omlmathbf","\\mathbf{\\sigma}"),("unicode-math","\\mbfsigma")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL SIGMA"}
  , Record {uchar = '\120533', commands = [("omlmathbf","\\mathbf{\\tau}"),("unicode-math","\\mbftau")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL TAU"}
  , Record {uchar = '\120534', commands = [("omlmathbf","\\mathbf{\\upsilon}"),("unicode-math","\\mbfupsilon")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL UPSILON"}
  , Record {uchar = '\120535', commands = [("omlmathbf","\\mathbf{\\varphi}"),("unicode-math","\\mbfvarphi")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL PHI"}
  , Record {uchar = '\120536', commands = [("omlmathbf","\\mathbf{\\chi}"),("unicode-math","\\mbfchi")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL CHI"}
  , Record {uchar = '\120537', commands = [("omlmathbf","\\mathbf{\\psi}"),("unicode-math","\\mbfpsi")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL PSI"}
  , Record {uchar = '\120538', commands = [("omlmathbf","\\mathbf{\\omega}"),("unicode-math","\\mbfomega")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL OMEGA"}
  , Record {uchar = '\120539', commands = [("unicode-math","\\mbfpartial")], category = Ord, comments = "MATHEMATICAL BOLD PARTIAL DIFFERENTIAL"}
  , Record {uchar = '\120540', commands = [("omlmathbf","\\mathbf{\\epsilon}"),("unicode-math","\\mbfvarepsilon")], category = Alpha, comments = "MATHEMATICAL BOLD EPSILON SYMBOL"}
  , Record {uchar = '\120541', commands = [("omlmathbf","\\mathbf{\\vartheta}"),("unicode-math","\\mbfvartheta")], category = Alpha, comments = "MATHEMATICAL BOLD THETA SYMBOL"}
  , Record {uchar = '\120542', commands = [("unicode-math","\\mbfvarkappa")], category = Alpha, comments = "MATHEMATICAL BOLD KAPPA SYMBOL"}
  , Record {uchar = '\120543', commands = [("omlmathbf","\\mathbf{\\phi}"),("unicode-math","\\mbfphi")], category = Alpha, comments = "MATHEMATICAL BOLD PHI SYMBOL"}
  , Record {uchar = '\120544', commands = [("omlmathbf","\\mathbf{\\varrho}"),("unicode-math","\\mbfvarrho")], category = Alpha, comments = "MATHEMATICAL BOLD RHO SYMBOL"}
  , Record {uchar = '\120545', commands = [("omlmathbf","\\mathbf{\\varpi}"),("unicode-math","\\mbfvarpi")], category = Alpha, comments = "MATHEMATICAL BOLD PI SYMBOL"}
  , Record {uchar = '\120546', commands = [("unicode-math","\\mitAlpha")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL ALPHA"}
  , Record {uchar = '\120547', commands = [("unicode-math","\\mitBeta")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL BETA"}
  , Record {uchar = '\120548', commands = [("slantedGreek","\\Gamma"),("-fourier","\\mathit{\\Gamma}"),("unicode-math","\\mitGamma")], category = Alpha, comments = "= \\varGamma (amsmath fourier), MATHEMATICAL ITALIC CAPITAL GAMMA"}
  , Record {uchar = '\120549', commands = [("slantedGreek","\\Delta"),("-fourier","\\mathit{\\Delta}"),("unicode-math","\\mitDelta")], category = Alpha, comments = "= \\varDelta (amsmath fourier), MATHEMATICAL ITALIC CAPITAL DELTA"}
  , Record {uchar = '\120550', commands = [("unicode-math","\\mitEpsilon")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL EPSILON"}
  , Record {uchar = '\120551', commands = [("unicode-math","\\mitZeta")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL ZETA"}
  , Record {uchar = '\120552', commands = [("unicode-math","\\mitEta")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL ETA"}
  , Record {uchar = '\120553', commands = [("slantedGreek","\\Theta"),("-fourier","\\mathit{\\Theta}"),("unicode-math","\\mitTheta")], category = Alpha, comments = "= \\varTheta (amsmath fourier), MATHEMATICAL ITALIC CAPITAL THETA"}
  , Record {uchar = '\120554', commands = [("unicode-math","\\mitIota")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL IOTA"}
  , Record {uchar = '\120555', commands = [("unicode-math","\\mitKappa")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL KAPPA"}
  , Record {uchar = '\120556', commands = [("slantedGreek","\\Lambda"),("-fourier","\\mathit{\\Lambda}"),("unicode-math","\\mitLambda")], category = Alpha, comments = "= \\varLambda (amsmath fourier), mathematical italic capital lambda"}
  , Record {uchar = '\120557', commands = [("unicode-math","\\mitMu")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL MU"}
  , Record {uchar = '\120558', commands = [("unicode-math","\\mitNu")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL NU"}
  , Record {uchar = '\120559', commands = [("slantedGreek","\\Xi"),("-fourier","\\mathit{\\Xi}"),("unicode-math","\\mitXi")], category = Alpha, comments = "= \\varXi (amsmath fourier), MATHEMATICAL ITALIC CAPITAL XI"}
  , Record {uchar = '\120560', commands = [("unicode-math","\\mitOmicron")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL OMICRON"}
  , Record {uchar = '\120561', commands = [("slantedGreek","\\Pi"),("-fourier","\\mathit{\\Pi}"),("unicode-math","\\mitPi")], category = Alpha, comments = "= \\varPi (amsmath fourier), MATHEMATICAL ITALIC CAPITAL PI"}
  , Record {uchar = '\120562', commands = [("unicode-math","\\mitRho")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL RHO"}
  , Record {uchar = '\120563', commands = [("unicode-math","\\mitvarTheta")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL THETA SYMBOL"}
  , Record {uchar = '\120564', commands = [("slantedGreek","\\Sigma"),("-fourier","\\mathit{\\Sigma}"),("unicode-math","\\mitSigma")], category = Alpha, comments = "= \\varSigma (amsmath fourier), MATHEMATICAL ITALIC CAPITAL SIGMA"}
  , Record {uchar = '\120565', commands = [("unicode-math","\\mitTau")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL TAU"}
  , Record {uchar = '\120566', commands = [("slantedGreek","\\Upsilon"),("-fourier","\\mathit{\\Upsilon}"),("unicode-math","\\mitUpsilon")], category = Alpha, comments = "= \\varUpsilon (amsmath fourier), MATHEMATICAL ITALIC CAPITAL UPSILON"}
  , Record {uchar = '\120567', commands = [("slantedGreek","\\Phi"),("-fourier","\\mathit{\\Phi}"),("unicode-math","\\mitPhi")], category = Alpha, comments = "= \\varPhi (amsmath fourier), MATHEMATICAL ITALIC CAPITAL PHI"}
  , Record {uchar = '\120568', commands = [("unicode-math","\\mitChi")], category = Alpha, comments = "MATHEMATICAL ITALIC CAPITAL CHI"}
  , Record {uchar = '\120569', commands = [("slantedGreek","\\Psi"),("-fourier","\\mathit{\\Psi}"),("unicode-math","\\mitPsi")], category = Alpha, comments = "= \\varPsi (amsmath fourier), MATHEMATICAL ITALIC CAPITAL PSI"}
  , Record {uchar = '\120570', commands = [("slantedGreek","\\Omega"),("-fourier","\\mathit{\\Omega}"),("unicode-math","\\mitOmega")], category = Alpha, comments = "= \\varOmega (amsmath fourier), MATHEMATICAL ITALIC CAPITAL OMEGA"}
  , Record {uchar = '\120571', commands = [("unicode-math","\\mitnabla")], category = Ord, comments = "MATHEMATICAL ITALIC NABLA"}
  , Record {uchar = '\120572', commands = [("base","\\alpha"),("omlmathit","\\mathit{\\alpha}"),("unicode-math","\\mitalpha")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL ALPHA"}
  , Record {uchar = '\120573', commands = [("base","\\beta"),("omlmathit","\\mathit{\\beta}"),("unicode-math","\\mitbeta")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL BETA"}
  , Record {uchar = '\120574', commands = [("base","\\gamma"),("omlmathit","\\mathit{\\gamma}"),("unicode-math","\\mitgamma")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL GAMMA"}
  , Record {uchar = '\120575', commands = [("base","\\delta"),("omlmathit","\\mathit{\\delta}"),("unicode-math","\\mitdelta")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL DELTA"}
  , Record {uchar = '\120576', commands = [("base","\\varepsilon"),("omlmathit","\\mathit{\\varepsilon}"),("unicode-math","\\mitepsilon")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL EPSILON"}
  , Record {uchar = '\120577', commands = [("base","\\zeta"),("omlmathit","\\mathit{\\zeta}"),("unicode-math","\\mitzeta")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL ZETA"}
  , Record {uchar = '\120578', commands = [("base","\\eta"),("omlmathit","\\mathit{\\eta}"),("unicode-math","\\miteta")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL ETA"}
  , Record {uchar = '\120579', commands = [("base","\\theta"),("omlmathit","\\mathit{\\theta}"),("unicode-math","\\mittheta")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL THETA"}
  , Record {uchar = '\120580', commands = [("base","\\iota"),("omlmathit","\\mathit{\\iota}"),("unicode-math","\\mitiota")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL IOTA"}
  , Record {uchar = '\120581', commands = [("base","\\kappa"),("omlmathit","\\mathit{\\kappa}"),("unicode-math","\\mitkappa")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL KAPPA"}
  , Record {uchar = '\120582', commands = [("base","\\lambda"),("omlmathit","\\mathit{\\lambda}"),("unicode-math","\\mitlambda")], category = Alpha, comments = "mathematical italic small lambda"}
  , Record {uchar = '\120583', commands = [("base","\\mu"),("omlmathit","\\mathit{\\mu}"),("unicode-math","\\mitmu")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL MU"}
  , Record {uchar = '\120584', commands = [("base","\\nu"),("omlmathit","\\mathit{\\nu}"),("unicode-math","\\mitnu")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL NU"}
  , Record {uchar = '\120585', commands = [("base","\\xi"),("omlmathit","\\mathit{\\xi}"),("unicode-math","\\mitxi")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL XI"}
  , Record {uchar = '\120586', commands = [("unicode-math","\\mitomicron")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL OMICRON"}
  , Record {uchar = '\120587', commands = [("base","\\pi"),("omlmathit","\\mathit{\\pi}"),("unicode-math","\\mitpi")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL PI"}
  , Record {uchar = '\120588', commands = [("base","\\rho"),("omlmathit","\\mathit{\\rho}"),("unicode-math","\\mitrho")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL RHO"}
  , Record {uchar = '\120589', commands = [("base","\\varsigma"),("omlmathit","\\mathit{\\varsigma}"),("unicode-math","\\mitvarsigma")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL FINAL SIGMA"}
  , Record {uchar = '\120590', commands = [("base","\\sigma"),("omlmathit","\\mathit{\\sigma}"),("unicode-math","\\mitsigma")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL SIGMA"}
  , Record {uchar = '\120591', commands = [("base","\\tau"),("omlmathit","\\mathit{\\tau}"),("unicode-math","\\mittau")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL TAU"}
  , Record {uchar = '\120592', commands = [("base","\\upsilon"),("omlmathit","\\mathit{\\upsilon}"),("unicode-math","\\mitupsilon")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL UPSILON"}
  , Record {uchar = '\120593', commands = [("base","\\varphi"),("omlmathit","\\mathit{\\varphi}"),("unicode-math","\\mitphi")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL PHI"}
  , Record {uchar = '\120594', commands = [("base","\\chi"),("omlmathit","\\mathit{\\chi}"),("unicode-math","\\mitchi")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL CHI"}
  , Record {uchar = '\120595', commands = [("base","\\psi"),("omlmathit","\\mathit{\\psi}"),("unicode-math","\\mitpsi")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL PSI"}
  , Record {uchar = '\120596', commands = [("base","\\omega"),("omlmathit","\\mathit{\\omega}"),("unicode-math","\\mitomega")], category = Alpha, comments = "MATHEMATICAL ITALIC SMALL OMEGA"}
  , Record {uchar = '\120597', commands = [("base","\\partial"),("omlmathit","\\mathit{\\partial}"),("unicode-math","\\mitpartial")], category = Ord, comments = "MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL"}
  , Record {uchar = '\120598', commands = [("base","\\epsilon"),("omlmathit","\\mathit{\\epsilon}"),("unicode-math","\\mitvarepsilon")], category = Alpha, comments = "MATHEMATICAL ITALIC EPSILON SYMBOL"}
  , Record {uchar = '\120599', commands = [("base","\\vartheta"),("omlmathit","\\mathit{\\vartheta}"),("unicode-math","\\mitvartheta")], category = Alpha, comments = "MATHEMATICAL ITALIC THETA SYMBOL"}
  , Record {uchar = '\120600', commands = [("amssymb","\\varkappa"),("unicode-math","\\mitvarkappa")], category = Alpha, comments = "MATHEMATICAL ITALIC KAPPA SYMBOL"}
  , Record {uchar = '\120601', commands = [("base","\\phi"),("omlmathit","\\mathit{\\phi}"),("unicode-math","\\mitvarphi")], category = Alpha, comments = "MATHEMATICAL ITALIC PHI SYMBOL"}
  , Record {uchar = '\120602', commands = [("base","\\varrho"),("omlmathit","\\mathit{\\varrho}"),("unicode-math","\\mitvarrho")], category = Alpha, comments = "MATHEMATICAL ITALIC RHO SYMBOL"}
  , Record {uchar = '\120603', commands = [("base","\\varpi"),("omlmathit","\\mathit{\\varpi}"),("unicode-math","\\mitvarpi")], category = Alpha, comments = "MATHEMATICAL ITALIC PI SYMBOL"}
  , Record {uchar = '\120604', commands = [("unicode-math","\\mbfitAlpha")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL ALPHA"}
  , Record {uchar = '\120605', commands = [("unicode-math","\\mbfitBeta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL BETA"}
  , Record {uchar = '\120606', commands = [("isomath","\\mathbfit{\\Gamma}"),("fixmath","\\mathbold{\\Gamma}"),("unicode-math","\\mbfitGamma")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL GAMMA"}
  , Record {uchar = '\120607', commands = [("isomath","\\mathbfit{\\Delta}"),("fixmath","\\mathbold{\\Delta}"),("unicode-math","\\mbfitDelta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL DELTA"}
  , Record {uchar = '\120608', commands = [("unicode-math","\\mbfitEpsilon")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL EPSILON"}
  , Record {uchar = '\120609', commands = [("unicode-math","\\mbfitZeta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL ZETA"}
  , Record {uchar = '\120610', commands = [("unicode-math","\\mbfitEta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL ETA"}
  , Record {uchar = '\120611', commands = [("isomath","\\mathbfit{\\Theta}"),("fixmath","\\mathbold{\\Theta}"),("unicode-math","\\mbfitTheta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL THETA"}
  , Record {uchar = '\120612', commands = [("unicode-math","\\mbfitIota")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL IOTA"}
  , Record {uchar = '\120613', commands = [("unicode-math","\\mbfitKappa")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL KAPPA"}
  , Record {uchar = '\120614', commands = [("isomath","\\mathbfit{\\Lambda}"),("fixmath","\\mathbold{\\Lambda}"),("unicode-math","\\mbfitLambda")], category = Alpha, comments = "mathematical bold italic capital lambda"}
  , Record {uchar = '\120615', commands = [("unicode-math","\\mbfitMu")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL MU"}
  , Record {uchar = '\120616', commands = [("unicode-math","\\mbfitNu")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL NU"}
  , Record {uchar = '\120617', commands = [("isomath","\\mathbfit{\\Xi}"),("fixmath","\\mathbold{\\Xi}"),("unicode-math","\\mbfitXi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL XI"}
  , Record {uchar = '\120618', commands = [("unicode-math","\\mbfitOmicron")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL OMICRON"}
  , Record {uchar = '\120619', commands = [("isomath","\\mathbfit{\\Pi}"),("fixmath","\\mathbold{\\Pi}"),("unicode-math","\\mbfitPi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL PI"}
  , Record {uchar = '\120620', commands = [("unicode-math","\\mbfitRho")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL RHO"}
  , Record {uchar = '\120621', commands = [("unicode-math","\\mbfitvarTheta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL THETA SYMBOL"}
  , Record {uchar = '\120622', commands = [("isomath","\\mathbfit{\\Sigma}"),("fixmath","\\mathbold{\\Sigma}"),("unicode-math","\\mbfitSigma")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL SIGMA"}
  , Record {uchar = '\120623', commands = [("unicode-math","\\mbfitTau")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL TAU"}
  , Record {uchar = '\120624', commands = [("isomath","\\mathbfit{\\Upsilon}"),("fixmath","\\mathbold{\\Upsilon}"),("unicode-math","\\mbfitUpsilon")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL UPSILON"}
  , Record {uchar = '\120625', commands = [("isomath","\\mathbfit{\\Phi}"),("fixmath","\\mathbold{\\Phi}"),("unicode-math","\\mbfitPhi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL PHI"}
  , Record {uchar = '\120626', commands = [("unicode-math","\\mbfitChi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL CHI"}
  , Record {uchar = '\120627', commands = [("isomath","\\mathbfit{\\Psi}"),("fixmath","\\mathbold{\\Psi}"),("unicode-math","\\mbfitPsi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL PSI"}
  , Record {uchar = '\120628', commands = [("isomath","\\mathbfit{\\Omega}"),("fixmath","\\mathbold{\\Omega}"),("unicode-math","\\mbfitOmega")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC CAPITAL OMEGA"}
  , Record {uchar = '\120629', commands = [("unicode-math","\\mbfitnabla")], category = Ord, comments = "MATHEMATICAL BOLD ITALIC NABLA"}
  , Record {uchar = '\120630', commands = [("isomath","\\mathbfit{\\alpha}"),("fixmath","\\mathbold{\\alpha}"),("unicode-math","\\mbfitalpha")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL ALPHA"}
  , Record {uchar = '\120631', commands = [("isomath","\\mathbfit{\\beta}"),("fixmath","\\mathbold{\\beta}"),("unicode-math","\\mbfitbeta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL BETA"}
  , Record {uchar = '\120632', commands = [("isomath","\\mathbfit{\\gamma}"),("fixmath","\\mathbold{\\gamma}"),("unicode-math","\\mbfitgamma")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL GAMMA"}
  , Record {uchar = '\120633', commands = [("isomath","\\mathbfit{\\delta}"),("fixmath","\\mathbold{\\delta}"),("unicode-math","\\mbfitdelta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL DELTA"}
  , Record {uchar = '\120634', commands = [("isomath","\\mathbfit{\\varepsilon}"),("fixmath","\\mathbold{\\varepsilon}"),("unicode-math","\\mbfitepsilon")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL EPSILON"}
  , Record {uchar = '\120635', commands = [("isomath","\\mathbfit{\\zeta}"),("fixmath","\\mathbold{\\zeta}"),("unicode-math","\\mbfitzeta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL ZETA"}
  , Record {uchar = '\120636', commands = [("isomath","\\mathbfit{\\eta}"),("fixmath","\\mathbold{\\eta}"),("unicode-math","\\mbfiteta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL ETA"}
  , Record {uchar = '\120637', commands = [("isomath","\\mathbfit{\\theta}"),("fixmath","\\mathbold{\\theta}"),("unicode-math","\\mbfittheta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL THETA"}
  , Record {uchar = '\120638', commands = [("isomath","\\mathbfit{\\iota}"),("fixmath","\\mathbold{\\iota}"),("unicode-math","\\mbfitiota")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL IOTA"}
  , Record {uchar = '\120639', commands = [("isomath","\\mathbfit{\\kappa}"),("fixmath","\\mathbold{\\kappa}"),("unicode-math","\\mbfitkappa")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL KAPPA"}
  , Record {uchar = '\120640', commands = [("isomath","\\mathbfit{\\lambda}"),("fixmath","\\mathbold{\\lambda}"),("unicode-math","\\mbfitlambda")], category = Alpha, comments = "mathematical bold italic small lambda"}
  , Record {uchar = '\120641', commands = [("isomath","\\mathbfit{\\mu}"),("fixmath","\\mathbold{\\mu}"),("unicode-math","\\mbfitmu")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL MU"}
  , Record {uchar = '\120642', commands = [("isomath","\\mathbfit{\\nu}"),("fixmath","\\mathbold{\\nu}"),("unicode-math","\\mbfitnu")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL NU"}
  , Record {uchar = '\120643', commands = [("isomath","\\mathbfit{\\xi}"),("fixmath","\\mathbold{\\xi}"),("unicode-math","\\mbfitxi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL XI"}
  , Record {uchar = '\120644', commands = [("unicode-math","\\mbfitomicron")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL OMICRON"}
  , Record {uchar = '\120645', commands = [("isomath","\\mathbfit{\\pi}"),("fixmath","\\mathbold{\\pi}"),("unicode-math","\\mbfitpi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL PI"}
  , Record {uchar = '\120646', commands = [("isomath","\\mathbfit{\\rho}"),("fixmath","\\mathbold{\\rho}"),("unicode-math","\\mbfitrho")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL RHO"}
  , Record {uchar = '\120647', commands = [("isomath","\\mathbfit{\\varsigma}"),("fixmath","\\mathbold{\\varsigma}"),("unicode-math","\\mbfitvarsigma")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL FINAL SIGMA"}
  , Record {uchar = '\120648', commands = [("isomath","\\mathbfit{\\sigma}"),("fixmath","\\mathbold{\\sigma}"),("unicode-math","\\mbfitsigma")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL SIGMA"}
  , Record {uchar = '\120649', commands = [("isomath","\\mathbfit{\\tau}"),("fixmath","\\mathbold{\\tau}"),("unicode-math","\\mbfittau")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL TAU"}
  , Record {uchar = '\120650', commands = [("isomath","\\mathbfit{\\upsilon}"),("fixmath","\\mathbold{\\upsilon}"),("unicode-math","\\mbfitupsilon")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL UPSILON"}
  , Record {uchar = '\120651', commands = [("isomath","\\mathbfit{\\varphi}"),("fixmath","\\mathbold{\\varphi}"),("unicode-math","\\mbfitphi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL PHI"}
  , Record {uchar = '\120652', commands = [("isomath","\\mathbfit{\\chi}"),("fixmath","\\mathbold{\\chi}"),("unicode-math","\\mbfitchi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL CHI"}
  , Record {uchar = '\120653', commands = [("isomath","\\mathbfit{\\psi}"),("fixmath","\\mathbold{\\psi}"),("unicode-math","\\mbfitpsi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL PSI"}
  , Record {uchar = '\120654', commands = [("isomath","\\mathbfit{\\omega}"),("fixmath","\\mathbold{\\omega}"),("unicode-math","\\mbfitomega")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC SMALL OMEGA"}
  , Record {uchar = '\120655', commands = [("unicode-math","\\mbfitpartial")], category = Ord, comments = "MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL"}
  , Record {uchar = '\120656', commands = [("isomath","\\mathbfit{\\epsilon}"),("fixmath","\\mathbold{\\epsilon}"),("unicode-math","\\mbfitvarepsilon")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC EPSILON SYMBOL"}
  , Record {uchar = '\120657', commands = [("isomath","\\mathbfit{\\vartheta}"),("fixmath","\\mathbold{\\vartheta}"),("unicode-math","\\mbfitvartheta")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC THETA SYMBOL"}
  , Record {uchar = '\120658', commands = [("unicode-math","\\mbfitvarkappa")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC KAPPA SYMBOL"}
  , Record {uchar = '\120659', commands = [("isomath","\\mathbfit{\\phi}"),("fixmath","\\mathbold{\\phi}"),("unicode-math","\\mbfitvarphi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC PHI SYMBOL"}
  , Record {uchar = '\120660', commands = [("isomath","\\mathbfit{\\varrho}"),("fixmath","\\mathbold{\\varrho}"),("unicode-math","\\mbfitvarrho")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC RHO SYMBOL"}
  , Record {uchar = '\120661', commands = [("isomath","\\mathbfit{\\varpi}"),("fixmath","\\mathbold{\\varpi}"),("unicode-math","\\mbfitvarpi")], category = Alpha, comments = "MATHEMATICAL BOLD ITALIC PI SYMBOL"}
  , Record {uchar = '\120662', commands = [("unicode-math","\\mbfsansAlpha")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL ALPHA"}
  , Record {uchar = '\120663', commands = [("unicode-math","\\mbfsansBeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL BETA"}
  , Record {uchar = '\120664', commands = [("mathsfbf","\\mathsfbf{\\Gamma}"),("unicode-math","\\mbfsansGamma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL GAMMA"}
  , Record {uchar = '\120665', commands = [("mathsfbf","\\mathsfbf{\\Delta}"),("unicode-math","\\mbfsansDelta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL DELTA"}
  , Record {uchar = '\120666', commands = [("unicode-math","\\mbfsansEpsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL EPSILON"}
  , Record {uchar = '\120667', commands = [("unicode-math","\\mbfsansZeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL ZETA"}
  , Record {uchar = '\120668', commands = [("unicode-math","\\mbfsansEta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL ETA"}
  , Record {uchar = '\120669', commands = [("mathsfbf","\\mathsfbf{\\Theta}"),("unicode-math","\\mbfsansTheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA"}
  , Record {uchar = '\120670', commands = [("unicode-math","\\mbfsansIota")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL IOTA"}
  , Record {uchar = '\120671', commands = [("unicode-math","\\mbfsansKappa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL KAPPA"}
  , Record {uchar = '\120672', commands = [("mathsfbf","\\mathsfbf{\\Lambda}"),("unicode-math","\\mbfsansLambda")], category = Alpha, comments = "mathematical sans-serif bold capital lambda"}
  , Record {uchar = '\120673', commands = [("unicode-math","\\mbfsansMu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL MU"}
  , Record {uchar = '\120674', commands = [("unicode-math","\\mbfsansNu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL NU"}
  , Record {uchar = '\120675', commands = [("mathsfbf","\\mathsfbf{\\Xi}"),("unicode-math","\\mbfsansXi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL XI"}
  , Record {uchar = '\120676', commands = [("unicode-math","\\mbfsansOmicron")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL OMICRON"}
  , Record {uchar = '\120677', commands = [("mathsfbf","\\mathsfbf{\\Pi}"),("unicode-math","\\mbfsansPi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL PI"}
  , Record {uchar = '\120678', commands = [("unicode-math","\\mbfsansRho")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL RHO"}
  , Record {uchar = '\120679', commands = [("unicode-math","\\mbfsansvarTheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA SYMBOL"}
  , Record {uchar = '\120680', commands = [("mathsfbf","\\mathsfbf{\\Sigma}"),("unicode-math","\\mbfsansSigma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL SIGMA"}
  , Record {uchar = '\120681', commands = [("unicode-math","\\mbfsansTau")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL TAU"}
  , Record {uchar = '\120682', commands = [("mathsfbf","\\mathsfbf{\\Upsilon}"),("unicode-math","\\mbfsansUpsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL UPSILON"}
  , Record {uchar = '\120683', commands = [("mathsfbf","\\mathsfbf{\\Phi}"),("unicode-math","\\mbfsansPhi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL PHI"}
  , Record {uchar = '\120684', commands = [("unicode-math","\\mbfsansChi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL CHI"}
  , Record {uchar = '\120685', commands = [("mathsfbf","\\mathsfbf{\\Psi}"),("unicode-math","\\mbfsansPsi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL PSI"}
  , Record {uchar = '\120686', commands = [("mathsfbf","\\mathsfbf{\\Omega}"),("unicode-math","\\mbfsansOmega")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA"}
  , Record {uchar = '\120687', commands = [("unicode-math","\\mbfsansnabla")], category = Ord, comments = "MATHEMATICAL SANS-SERIF BOLD NABLA"}
  , Record {uchar = '\120688', commands = [("omlmathsfbf","\\mathsfbf{\\alpha}"),("unicode-math","\\mbfsansalpha")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA"}
  , Record {uchar = '\120689', commands = [("omlmathsfbf","\\mathsfbf{\\beta}"),("unicode-math","\\mbfsansbeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL BETA"}
  , Record {uchar = '\120690', commands = [("omlmathsfbf","\\mathsfbf{\\gamma}"),("unicode-math","\\mbfsansgamma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL GAMMA"}
  , Record {uchar = '\120691', commands = [("omlmathsfbf","\\mathsfbf{\\delta}"),("unicode-math","\\mbfsansdelta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL DELTA"}
  , Record {uchar = '\120692', commands = [("omlmathsfbf","\\mathsfbf{\\varepsilon}"),("unicode-math","\\mbfsansepsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL EPSILON"}
  , Record {uchar = '\120693', commands = [("omlmathsfbf","\\mathsfbf{\\zeta}"),("unicode-math","\\mbfsanszeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL ZETA"}
  , Record {uchar = '\120694', commands = [("omlmathsfbf","\\mathsfbf{\\eta}"),("unicode-math","\\mbfsanseta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL ETA"}
  , Record {uchar = '\120695', commands = [("omlmathsfbf","\\mathsfbf{\\theta}"),("unicode-math","\\mbfsanstheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL THETA"}
  , Record {uchar = '\120696', commands = [("omlmathsfbf","\\mathsfbf{\\iota}"),("unicode-math","\\mbfsansiota")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL IOTA"}
  , Record {uchar = '\120697', commands = [("omlmathsfbf","\\mathsfbf{\\kappa}"),("unicode-math","\\mbfsanskappa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL KAPPA"}
  , Record {uchar = '\120698', commands = [("omlmathsfbf","\\mathsfbf{\\lambda}"),("unicode-math","\\mbfsanslambda")], category = Alpha, comments = "mathematical sans-serif bold small lambda"}
  , Record {uchar = '\120699', commands = [("omlmathsfbf","\\mathsfbf{\\mu}"),("unicode-math","\\mbfsansmu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL MU"}
  , Record {uchar = '\120700', commands = [("omlmathsfbf","\\mathsfbf{\\nu}"),("unicode-math","\\mbfsansnu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL NU"}
  , Record {uchar = '\120701', commands = [("omlmathsfbf","\\mathsfbf{\\xi}"),("unicode-math","\\mbfsansxi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL XI"}
  , Record {uchar = '\120702', commands = [("unicode-math","\\mbfsansomicron")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL OMICRON"}
  , Record {uchar = '\120703', commands = [("omlmathsfbf","\\mathsfbf{\\pi}"),("unicode-math","\\mbfsanspi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL PI"}
  , Record {uchar = '\120704', commands = [("omlmathsfbf","\\mathsfbf{\\rho}"),("unicode-math","\\mbfsansrho")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL RHO"}
  , Record {uchar = '\120705', commands = [("omlmathsfbf","\\mathsfbf{\\varsigma}"),("unicode-math","\\mbfsansvarsigma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL FINAL SIGMA"}
  , Record {uchar = '\120706', commands = [("omlmathsfbf","\\mathsfbf{\\sigma}"),("unicode-math","\\mbfsanssigma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL SIGMA"}
  , Record {uchar = '\120707', commands = [("omlmathsfbf","\\mathsfbf{\\tau}"),("unicode-math","\\mbfsanstau")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL TAU"}
  , Record {uchar = '\120708', commands = [("omlmathsfbf","\\mathsfbf{\\upsilon}"),("unicode-math","\\mbfsansupsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL UPSILON"}
  , Record {uchar = '\120709', commands = [("omlmathsfbf","\\mathsfbf{\\varphi}"),("unicode-math","\\mbfsansphi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL PHI"}
  , Record {uchar = '\120710', commands = [("omlmathsfbf","\\mathsfbf{\\chi}"),("unicode-math","\\mbfsanschi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL CHI"}
  , Record {uchar = '\120711', commands = [("omlmathsfbf","\\mathsfbf{\\psi}"),("unicode-math","\\mbfsanspsi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL PSI"}
  , Record {uchar = '\120712', commands = [("omlmathsfbf","\\mathsfbf{\\omega}"),("unicode-math","\\mbfsansomega")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA"}
  , Record {uchar = '\120713', commands = [("unicode-math","\\mbfsanspartial")], category = Ord, comments = "MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL"}
  , Record {uchar = '\120714', commands = [("omlmathsfbf","\\mathsfbf{\\epsilon}"),("unicode-math","\\mbfsansvarepsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL"}
  , Record {uchar = '\120715', commands = [("omlmathsfbf","\\mathsfbf{\\vartheta}"),("unicode-math","\\mbfsansvartheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD THETA SYMBOL"}
  , Record {uchar = '\120716', commands = [("unicode-math","\\mbfsansvarkappa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD KAPPA SYMBOL"}
  , Record {uchar = '\120717', commands = [("omlmathsfbf","\\mathsfbf{\\phi}"),("unicode-math","\\mbfsansvarphi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD PHI SYMBOL"}
  , Record {uchar = '\120718', commands = [("omlmathsfbf","\\mathsfbf{\\varrho}"),("unicode-math","\\mbfsansvarrho")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD RHO SYMBOL"}
  , Record {uchar = '\120719', commands = [("omlmathsfbf","\\mathsfbf{\\varpi}"),("unicode-math","\\mbfsansvarpi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD PI SYMBOL"}
  , Record {uchar = '\120720', commands = [("unicode-math","\\mbfitsansAlpha")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ALPHA"}
  , Record {uchar = '\120721', commands = [("unicode-math","\\mbfitsansBeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL BETA"}
  , Record {uchar = '\120722', commands = [("isomath","\\mathsfbfit{\\Gamma}"),("unicode-math","\\mbfitsansGamma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL GAMMA"}
  , Record {uchar = '\120723', commands = [("isomath","\\mathsfbfit{\\Delta}"),("unicode-math","\\mbfitsansDelta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL DELTA"}
  , Record {uchar = '\120724', commands = [("unicode-math","\\mbfitsansEpsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL EPSILON"}
  , Record {uchar = '\120725', commands = [("unicode-math","\\mbfitsansZeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ZETA"}
  , Record {uchar = '\120726', commands = [("unicode-math","\\mbfitsansEta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ETA"}
  , Record {uchar = '\120727', commands = [("isomath","\\mathsfbfit{\\Theta}"),("unicode-math","\\mbfitsansTheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA"}
  , Record {uchar = '\120728', commands = [("unicode-math","\\mbfitsansIota")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL IOTA"}
  , Record {uchar = '\120729', commands = [("unicode-math","\\mbfitsansKappa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL KAPPA"}
  , Record {uchar = '\120730', commands = [("isomath","\\mathsfbfit{\\Lambda}"),("unicode-math","\\mbfitsansLambda")], category = Alpha, comments = "mathematical sans-serif bold italic capital lambda"}
  , Record {uchar = '\120731', commands = [("unicode-math","\\mbfitsansMu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL MU"}
  , Record {uchar = '\120732', commands = [("unicode-math","\\mbfitsansNu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL NU"}
  , Record {uchar = '\120733', commands = [("isomath","\\mathsfbfit{\\Xi}"),("unicode-math","\\mbfitsansXi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL XI"}
  , Record {uchar = '\120734', commands = [("unicode-math","\\mbfitsansOmicron")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMICRON"}
  , Record {uchar = '\120735', commands = [("isomath","\\mathsfbfit{\\Pi}"),("unicode-math","\\mbfitsansPi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PI"}
  , Record {uchar = '\120736', commands = [("unicode-math","\\mbfitsansRho")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL RHO"}
  , Record {uchar = '\120737', commands = [("unicode-math","\\mbfitsansvarTheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA SYMBOL"}
  , Record {uchar = '\120738', commands = [("isomath","\\mathsfbfit{\\Sigma}"),("unicode-math","\\mbfitsansSigma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL SIGMA"}
  , Record {uchar = '\120739', commands = [("unicode-math","\\mbfitsansTau")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL TAU"}
  , Record {uchar = '\120740', commands = [("isomath","\\mathsfbfit{\\Upsilon}"),("unicode-math","\\mbfitsansUpsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL UPSILON"}
  , Record {uchar = '\120741', commands = [("isomath","\\mathsfbfit{\\Phi}"),("unicode-math","\\mbfitsansPhi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PHI"}
  , Record {uchar = '\120742', commands = [("unicode-math","\\mbfitsansChi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL CHI"}
  , Record {uchar = '\120743', commands = [("isomath","\\mathsfbfit{\\Psi}"),("unicode-math","\\mbfitsansPsi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PSI"}
  , Record {uchar = '\120744', commands = [("isomath","\\mathsfbfit{\\Omega}"),("unicode-math","\\mbfitsansOmega")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA"}
  , Record {uchar = '\120745', commands = [("unicode-math","\\mbfitsansnabla")], category = Ord, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA"}
  , Record {uchar = '\120746', commands = [("isomath","\\mathsfbfit{\\alpha}"),("unicode-math","\\mbfitsansalpha")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA"}
  , Record {uchar = '\120747', commands = [("isomath","\\mathsfbfit{\\beta}"),("unicode-math","\\mbfitsansbeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL BETA"}
  , Record {uchar = '\120748', commands = [("isomath","\\mathsfbfit{\\gamma}"),("unicode-math","\\mbfitsansgamma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL GAMMA"}
  , Record {uchar = '\120749', commands = [("isomath","\\mathsfbfit{\\delta}"),("unicode-math","\\mbfitsansdelta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL DELTA"}
  , Record {uchar = '\120750', commands = [("isomath","\\mathsfbfit{\\varepsilon}"),("unicode-math","\\mbfitsansepsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL EPSILON"}
  , Record {uchar = '\120751', commands = [("isomath","\\mathsfbfit{\\zeta}"),("unicode-math","\\mbfitsanszeta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ZETA"}
  , Record {uchar = '\120752', commands = [("isomath","\\mathsfbfit{\\eta}"),("unicode-math","\\mbfitsanseta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ETA"}
  , Record {uchar = '\120753', commands = [("isomath","\\mathsfbfit{\\theta}"),("unicode-math","\\mbfitsanstheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL THETA"}
  , Record {uchar = '\120754', commands = [("isomath","\\mathsfbfit{\\iota}"),("unicode-math","\\mbfitsansiota")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL IOTA"}
  , Record {uchar = '\120755', commands = [("isomath","\\mathsfbfit{\\kappa}"),("unicode-math","\\mbfitsanskappa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL KAPPA"}
  , Record {uchar = '\120756', commands = [("isomath","\\mathsfbfit{\\lambda}"),("unicode-math","\\mbfitsanslambda")], category = Alpha, comments = "mathematical sans-serif bold italic small lambda"}
  , Record {uchar = '\120757', commands = [("isomath","\\mathsfbfit{\\mu}"),("unicode-math","\\mbfitsansmu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL MU"}
  , Record {uchar = '\120758', commands = [("isomath","\\mathsfbfit{\\nu}"),("unicode-math","\\mbfitsansnu")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL NU"}
  , Record {uchar = '\120759', commands = [("isomath","\\mathsfbfit{\\xi}"),("unicode-math","\\mbfitsansxi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL XI"}
  , Record {uchar = '\120760', commands = [("unicode-math","\\mbfitsansomicron")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMICRON"}
  , Record {uchar = '\120761', commands = [("isomath","\\mathsfbfit{\\pi}"),("unicode-math","\\mbfitsanspi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PI"}
  , Record {uchar = '\120762', commands = [("isomath","\\mathsfbfit{\\rho}"),("unicode-math","\\mbfitsansrho")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL RHO"}
  , Record {uchar = '\120763', commands = [("isomath","\\mathsfbfit{\\varsigma}"),("unicode-math","\\mbfitsansvarsigma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL FINAL SIGMA"}
  , Record {uchar = '\120764', commands = [("isomath","\\mathsfbfit{\\sigma}"),("unicode-math","\\mbfitsanssigma")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL SIGMA"}
  , Record {uchar = '\120765', commands = [("isomath","\\mathsfbfit{\\tau}"),("unicode-math","\\mbfitsanstau")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL TAU"}
  , Record {uchar = '\120766', commands = [("isomath","\\mathsfbfit{\\upsilon}"),("unicode-math","\\mbfitsansupsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL UPSILON"}
  , Record {uchar = '\120767', commands = [("isomath","\\mathsfbfit{\\varphi}"),("unicode-math","\\mbfitsansphi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PHI"}
  , Record {uchar = '\120768', commands = [("isomath","\\mathsfbfit{\\chi}"),("unicode-math","\\mbfitsanschi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL CHI"}
  , Record {uchar = '\120769', commands = [("isomath","\\mathsfbfit{\\psi}"),("unicode-math","\\mbfitsanspsi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PSI"}
  , Record {uchar = '\120770', commands = [("isomath","\\mathsfbfit{\\omega}"),("unicode-math","\\mbfitsansomega")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA"}
  , Record {uchar = '\120771', commands = [("unicode-math","\\mbfitsanspartial")], category = Ord, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL"}
  , Record {uchar = '\120772', commands = [("isomath","\\mathsfbfit{\\epsilon}"),("unicode-math","\\mbfitsansvarepsilon")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL"}
  , Record {uchar = '\120773', commands = [("isomath","\\mathsfbfit{\\vartheta}"),("unicode-math","\\mbfitsansvartheta")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC THETA SYMBOL"}
  , Record {uchar = '\120774', commands = [("unicode-math","\\mbfitsansvarkappa")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC KAPPA SYMBOL"}
  , Record {uchar = '\120775', commands = [("isomath","\\mathsfbfit{\\phi}"),("unicode-math","\\mbfitsansvarphi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC PHI SYMBOL"}
  , Record {uchar = '\120776', commands = [("isomath","\\mathsfbfit{\\varrho}"),("unicode-math","\\mbfitsansvarrho")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC RHO SYMBOL"}
  , Record {uchar = '\120777', commands = [("isomath","\\mathsfbfit{\\varpi}"),("unicode-math","\\mbfitsansvarpi")], category = Alpha, comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC PI SYMBOL"}
  , Record {uchar = '\120778', commands = [("unicode-math","\\mbfDigamma")], category = Alpha, comments = "MATHEMATICAL BOLD CAPITAL DIGAMMA"}
  , Record {uchar = '\120779', commands = [("unicode-math","\\mbfdigamma")], category = Alpha, comments = "MATHEMATICAL BOLD SMALL DIGAMMA"}
  , Record {uchar = '\120782', commands = [("base","\\mathbf{0}")], category = Ord, comments = "mathematical bold digit 0"}
  , Record {uchar = '\120783', commands = [("base","\\mathbf{1}")], category = Ord, comments = "mathematical bold digit 1"}
  , Record {uchar = '\120784', commands = [("base","\\mathbf{2}")], category = Ord, comments = "mathematical bold digit 2"}
  , Record {uchar = '\120785', commands = [("base","\\mathbf{3}")], category = Ord, comments = "mathematical bold digit 3"}
  , Record {uchar = '\120786', commands = [("base","\\mathbf{4}")], category = Ord, comments = "mathematical bold digit 4"}
  , Record {uchar = '\120787', commands = [("base","\\mathbf{5}")], category = Ord, comments = "mathematical bold digit 5"}
  , Record {uchar = '\120788', commands = [("base","\\mathbf{6}")], category = Ord, comments = "mathematical bold digit 6"}
  , Record {uchar = '\120789', commands = [("base","\\mathbf{7}")], category = Ord, comments = "mathematical bold digit 7"}
  , Record {uchar = '\120790', commands = [("base","\\mathbf{8}")], category = Ord, comments = "mathematical bold digit 8"}
  , Record {uchar = '\120791', commands = [("base","\\mathbf{9}")], category = Ord, comments = "mathematical bold digit 9"}
  , Record {uchar = '\120792', commands = [("bbold","\\mathbb{0}"),("unicode-math","\\Bbbzero")], category = Ord, comments = "mathematical double-struck digit 0"}
  , Record {uchar = '\120793', commands = [("bbold","\\mathbb{1}"),("fourier","\\mathbb{1}"),("dsfont","\\mathds{1}"),("unicode-math","\\Bbbone")], category = Ord, comments = "mathematical double-struck digit 1"}
  , Record {uchar = '\120794', commands = [("bbold","\\mathbb{2}"),("unicode-math","\\Bbbtwo")], category = Ord, comments = "mathematical double-struck digit 2"}
  , Record {uchar = '\120795', commands = [("bbold","\\mathbb{3}"),("unicode-math","\\Bbbthree")], category = Ord, comments = "mathematical double-struck digit 3"}
  , Record {uchar = '\120796', commands = [("bbold","\\mathbb{4}"),("unicode-math","\\Bbbfour")], category = Ord, comments = "mathematical double-struck digit 4"}
  , Record {uchar = '\120797', commands = [("bbold","\\mathbb{5}"),("unicode-math","\\Bbbfive")], category = Ord, comments = "mathematical double-struck digit 5"}
  , Record {uchar = '\120798', commands = [("bbold","\\mathbb{6}"),("unicode-math","\\Bbbsix")], category = Ord, comments = "mathematical double-struck digit 6"}
  , Record {uchar = '\120799', commands = [("bbold","\\mathbb{7}"),("unicode-math","\\Bbbseven")], category = Ord, comments = "mathematical double-struck digit 7"}
  , Record {uchar = '\120800', commands = [("bbold","\\mathbb{8}"),("unicode-math","\\Bbbeight")], category = Ord, comments = "mathematical double-struck digit 8"}
  , Record {uchar = '\120801', commands = [("bbold","\\mathbb{9}"),("unicode-math","\\Bbbnine")], category = Ord, comments = "mathematical double-struck digit 9"}
  , Record {uchar = '\120802', commands = [("base","\\mathsf{0}"),("unicode-math","\\msanszero")], category = Ord, comments = "mathematical sans-serif digit 0"}
  , Record {uchar = '\120803', commands = [("base","\\mathsf{1}"),("unicode-math","\\msansone")], category = Ord, comments = "mathematical sans-serif digit 1"}
  , Record {uchar = '\120804', commands = [("base","\\mathsf{2}"),("unicode-math","\\msanstwo")], category = Ord, comments = "mathematical sans-serif digit 2"}
  , Record {uchar = '\120805', commands = [("base","\\mathsf{3}"),("unicode-math","\\msansthree")], category = Ord, comments = "mathematical sans-serif digit 3"}
  , Record {uchar = '\120806', commands = [("base","\\mathsf{4}"),("unicode-math","\\msansfour")], category = Ord, comments = "mathematical sans-serif digit 4"}
  , Record {uchar = '\120807', commands = [("base","\\mathsf{5}"),("unicode-math","\\msansfive")], category = Ord, comments = "mathematical sans-serif digit 5"}
  , Record {uchar = '\120808', commands = [("base","\\mathsf{6}"),("unicode-math","\\msanssix")], category = Ord, comments = "mathematical sans-serif digit 6"}
  , Record {uchar = '\120809', commands = [("base","\\mathsf{7}"),("unicode-math","\\msansseven")], category = Ord, comments = "mathematical sans-serif digit 7"}
  , Record {uchar = '\120810', commands = [("base","\\mathsf{8}"),("unicode-math","\\msanseight")], category = Ord, comments = "mathematical sans-serif digit 8"}
  , Record {uchar = '\120811', commands = [("base","\\mathsf{9}"),("unicode-math","\\msansnine")], category = Ord, comments = "mathematical sans-serif digit 9"}
  , Record {uchar = '\120812', commands = [("mathsfbf","\\mathsfbf{0}"),("unicode-math","\\mbfsanszero")], category = Ord, comments = "mathematical sans-serif bold digit 0"}
  , Record {uchar = '\120813', commands = [("mathsfbf","\\mathsfbf{1}"),("unicode-math","\\mbfsansone")], category = Ord, comments = "mathematical sans-serif bold digit 1"}
  , Record {uchar = '\120814', commands = [("mathsfbf","\\mathsfbf{2}"),("unicode-math","\\mbfsanstwo")], category = Ord, comments = "mathematical sans-serif bold digit 2"}
  , Record {uchar = '\120815', commands = [("mathsfbf","\\mathsfbf{3}"),("unicode-math","\\mbfsansthree")], category = Ord, comments = "mathematical sans-serif bold digit 3"}
  , Record {uchar = '\120816', commands = [("mathsfbf","\\mathsfbf{4}"),("unicode-math","\\mbfsansfour")], category = Ord, comments = "mathematical sans-serif bold digit 4"}
  , Record {uchar = '\120817', commands = [("mathsfbf","\\mathsfbf{5}"),("unicode-math","\\mbfsansfive")], category = Ord, comments = "mathematical sans-serif bold digit 5"}
  , Record {uchar = '\120818', commands = [("mathsfbf","\\mathsfbf{6}"),("unicode-math","\\mbfsanssix")], category = Ord, comments = "mathematical sans-serif bold digit 6"}
  , Record {uchar = '\120819', commands = [("mathsfbf","\\mathsfbf{7}"),("unicode-math","\\mbfsansseven")], category = Ord, comments = "mathematical sans-serif bold digit 7"}
  , Record {uchar = '\120820', commands = [("mathsfbf","\\mathsfbf{8}"),("unicode-math","\\mbfsanseight")], category = Ord, comments = "mathematical sans-serif bold digit 8"}
  , Record {uchar = '\120821', commands = [("mathsfbf","\\mathsfbf{9}"),("unicode-math","\\mbfsansnine")], category = Ord, comments = "mathematical sans-serif bold digit 9"}
  , Record {uchar = '\120822', commands = [("base","\\mathtt{0}"),("unicode-math","\\mttzero")], category = Ord, comments = "mathematical monospace digit 0"}
  , Record {uchar = '\120823', commands = [("base","\\mathtt{1}"),("unicode-math","\\mttone")], category = Ord, comments = "mathematical monospace digit 1"}
  , Record {uchar = '\120824', commands = [("base","\\mathtt{2}"),("unicode-math","\\mtttwo")], category = Ord, comments = "mathematical monospace digit 2"}
  , Record {uchar = '\120825', commands = [("base","\\mathtt{3}"),("unicode-math","\\mttthree")], category = Ord, comments = "mathematical monospace digit 3"}
  , Record {uchar = '\120826', commands = [("base","\\mathtt{4}"),("unicode-math","\\mttfour")], category = Ord, comments = "mathematical monospace digit 4"}
  , Record {uchar = '\120827', commands = [("base","\\mathtt{5}"),("unicode-math","\\mttfive")], category = Ord, comments = "mathematical monospace digit 5"}
  , Record {uchar = '\120828', commands = [("base","\\mathtt{6}"),("unicode-math","\\mttsix")], category = Ord, comments = "mathematical monospace digit 6"}
  , Record {uchar = '\120829', commands = [("base","\\mathtt{7}"),("unicode-math","\\mttseven")], category = Ord, comments = "mathematical monospace digit 7"}
  , Record {uchar = '\120830', commands = [("base","\\mathtt{8}"),("unicode-math","\\mtteight")], category = Ord, comments = "mathematical monospace digit 8"}
  , Record {uchar = '\120831', commands = [("base","\\mathtt{9}"),("unicode-math","\\mttnine")], category = Ord, comments = "mathematical monospace digit 9"}]
