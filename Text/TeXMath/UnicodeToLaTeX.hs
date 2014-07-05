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
{-

This module is derived from the list of unicode to LaTeX mappings 
compiled by Günter Milde. All request for support should be sent to the
current maintainer of this module and NOT the aforementioned original author.

The work was originally licensed under the LaTeX Project Public License.

An unmodified original copy of this work can be obtained from the following
webpage.

http://milde.users.sourceforge.net/LUCR/Math/

Changes to the work can be seen via the git commit history to this module. 

Whilst distributed under the GPL in conformance with clause 10a, all 
deriviations of this work must also conform with clause 6 of the the
LaTeX Project Public License.

-} 

module Text.TeXMath.UnicodeToLaTeX (escapeLaTeX, convertText, getLaTeX) where

import qualified Data.Map as M
import Numeric (readHex)
import Text.TeXMath.Types
import Data.Char (ord)
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Control.Applicative hiding (optional)
import Text.Parsec hiding ((<|>))
import Text.TeXMath.Unidecode (getASCII)
import Text.TeXMath.ToUnicode (fromUnicode)
import qualified Text.TeXMath.Shared as S

env :: [String]
env = ["amsmath", "amssymb", ""]

-- Categories which require braces
commands :: [String]
commands = ["mathaccent", "mathradical", "mathover", "mathunder"]

escapeLaTeX :: Char -> String
escapeLaTeX c
  | c `elem` "#$%&_{}" = "\\" ++ [c] 
  | c == '~' = "\\textasciitilde"
  | c == '^' = "\\textasciicircum"
  | c == '\\' = "\\textbackslash"
  | otherwise = [c]

-- ugly
getLaTeX ::  String -> String
getLaTeX s = (concatMap (\x -> let a = getASCII x in 
                         fromMaybe a -- this will never actually happen 
                          (f x <|> textConvert x <|> (concat <$> mapM (\y -> padCommand <$> (f y <|> return [y])) a ))) s)
  where
    f i = do
      v <- M.lookup (ord i) recordsMap
      let r = filter (\z -> head z /= '-') $ (words . requirements)  v
      let Right alts = parse parseComment "" (comments v)
      ret <- if null r || or (map (`elem` r) env) 
                then Just $ latex v
                  else 
                    listToMaybe $ catMaybes (map (flip lookup alts) env) 
      let ret' = 
            case category v `elem` commands of
              True -> ret ++ "{}"
              False -> ret
      return $ padCommand ret'

padCommand :: String -> String 
padCommand s@('\\':_) = s ++ " "
padCommand s = s 

textConvert :: Char -> Maybe String
textConvert c = do
  (ttype, v) <- fromUnicode c 
  return $ S.getLaTeXTextCommand ttype ++ "{"++[v]++"}"

convertText :: String -> String
convertText = concatMap getSpaceLaTeX 
        
getSpaceLaTeX :: Char -> String 
getSpaceLaTeX c =  
  let cat = fromMaybe "" (cls <$> M.lookup (ord c) recordsMap) in
  if ('S' `elem` cat) then
    getLaTeX [c]
    else
      concatMap escapeLaTeX (getASCII c)
        
parseComment :: Parsec String () [(String, String)]
parseComment  = catMaybes <$> sepBy command (char ',')
 
command :: Parsec String () (Maybe (String, String))
command = do
  first <- anyChar
  case first of 
    '='-> Just <$> cmd
    '#'-> Just <$> cmd
    'x'-> Nothing <$ skip
    't'-> Nothing <$ skip
    _ -> Nothing <$ skip 

cmd :: Parsec String () (String, String)
cmd = do
  optional spaces
  alt <- manyTill anyChar (lookAhead (char ',') <|> space)
  optional spaces
  package <- option "" (between (char '(') (char ')') (many1 (notFollowedBy (char ')') *> anyChar)))
  optional spaces
  return (package, alt)

skip :: Parsec String () ()
skip = skipMany (notFollowedBy (char ',') *> anyChar)
     
recordsMap :: M.Map Int Record
recordsMap = M.fromList (map f records)
  where
    f r = (fst $ head (readHex $ point r), r)

records :: [Record]
records = [Record {point = "00021", uchar = "!", latex = "!", unicodemath = "\\exclam", cls = "N", category = "mathpunct", requirements = "", comments = "EXCLAMATION MARK"}
  , Record {point = "00023", uchar = "#", latex = "\\#", unicodemath = "\\octothorpe", cls = "N", category = "mathord", requirements = "-oz", comments = "# \\# (oz), NUMBER SIGN"}
  , Record {point = "00024", uchar = "$", latex = "\\$", unicodemath = "\\mathdollar", cls = "N", category = "mathord", requirements = "", comments = "= \\mathdollar, DOLLAR SIGN"}
  , Record {point = "00025", uchar = "%", latex = "\\%", unicodemath = "\\percent", cls = "N", category = "mathord", requirements = "", comments = "PERCENT SIGN"}
  , Record {point = "00026", uchar = "&", latex = "\\&", unicodemath = "\\ampersand", cls = "N", category = "mathord", requirements = "", comments = "# \\binampersand (stmaryrd)"}
  , Record {point = "00028", uchar = "(", latex = "(", unicodemath = "\\lparen", cls = "O", category = "mathopen", requirements = "", comments = "LEFT PARENTHESIS"}
  , Record {point = "00029", uchar = ")", latex = ")", unicodemath = "\\rparen", cls = "C", category = "mathclose", requirements = "", comments = "RIGHT PARENTHESIS"}
  , Record {point = "0002A", uchar = "*", latex = "*", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "# \\ast, (high) ASTERISK, star"}
  , Record {point = "0002B", uchar = "+", latex = "+", unicodemath = "\\plus", cls = "V", category = "mathbin", requirements = "", comments = "PLUS SIGN"}
  , Record {point = "0002C", uchar = ",", latex = ",", unicodemath = "\\comma", cls = "P", category = "mathpunct", requirements = "", comments = "COMMA"}
  , Record {point = "0002D", uchar = "-", latex = "-", unicodemath = "-", cls = "N", category = "mathbin", requirements = "", comments = "t -, HYPHEN-MINUS (deprecated for math)"}
  , Record {point = "0002E", uchar = ".", latex = ".", unicodemath = "\\period", cls = "P", category = "mathalpha", requirements = "", comments = "FULL STOP, period"}
  , Record {point = "0002F", uchar = "/", latex = "/", unicodemath = "\\mathslash", cls = "B", category = "mathord", requirements = "", comments = "# \\slash, SOLIDUS"}
  , Record {point = "00030", uchar = "0", latex = "0", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "DIGIT ZERO"}
  , Record {point = "00031", uchar = "1", latex = "1", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "DIGIT ONE"}
  , Record {point = "00032", uchar = "2", latex = "2", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "DIGIT TWO"}
  , Record {point = "00033", uchar = "3", latex = "3", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "DIGIT THREE"}
  , Record {point = "00034", uchar = "4", latex = "4", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "DIGIT FOUR"}
  , Record {point = "00035", uchar = "5", latex = "5", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "DIGIT FIVE"}
  , Record {point = "00036", uchar = "6", latex = "6", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "DIGIT SIX"}
  , Record {point = "00037", uchar = "7", latex = "7", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "DIGIT SEVEN"}
  , Record {point = "00038", uchar = "8", latex = "8", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "DIGIT EIGHT"}
  , Record {point = "00039", uchar = "9", latex = "9", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "DIGIT NINE"}
  , Record {point = "0003A", uchar = ":", latex = ":", unicodemath = "\\mathcolon", cls = "P", category = "mathpunct", requirements = "-literal", comments = "= \\colon (literal), COLON (not ratio)"}
  , Record {point = "0003B", uchar = ";", latex = ";", unicodemath = "\\semicolon", cls = "P", category = "mathpunct", requirements = "", comments = "SEMICOLON p:"}
  , Record {point = "0003C", uchar = "<", latex = "<", unicodemath = "\\less", cls = "R", category = "mathrel", requirements = "", comments = "LESS-THAN SIGN r:"}
  , Record {point = "0003D", uchar = "=", latex = "=", unicodemath = "\\equal", cls = "R", category = "mathrel", requirements = "", comments = "EQUALS SIGN r:"}
  , Record {point = "0003E", uchar = ">", latex = ">", unicodemath = "\\greater", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN SIGN r:"}
  , Record {point = "0003F", uchar = "?", latex = "?", unicodemath = "\\question", cls = "P", category = "mathord", requirements = "", comments = "QUESTION MARK"}
  , Record {point = "00040", uchar = "@", latex = "@", unicodemath = "\\atsign", cls = "N", category = "mathord", requirements = "", comments = "at"}
  , Record {point = "00041", uchar = "A", latex = "A", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{A}, LATIN CAPITAL LETTER A"}
  , Record {point = "00042", uchar = "B", latex = "B", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{B}, LATIN CAPITAL LETTER B"}
  , Record {point = "00043", uchar = "C", latex = "C", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{C}, LATIN CAPITAL LETTER C"}
  , Record {point = "00044", uchar = "D", latex = "D", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{D}, LATIN CAPITAL LETTER D"}
  , Record {point = "00045", uchar = "E", latex = "E", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{E}, LATIN CAPITAL LETTER E"}
  , Record {point = "00046", uchar = "F", latex = "F", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{F}, LATIN CAPITAL LETTER F"}
  , Record {point = "00047", uchar = "G", latex = "G", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{G}, LATIN CAPITAL LETTER G"}
  , Record {point = "00048", uchar = "H", latex = "H", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{H}, LATIN CAPITAL LETTER H"}
  , Record {point = "00049", uchar = "I", latex = "I", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{I}, LATIN CAPITAL LETTER I"}
  , Record {point = "0004A", uchar = "J", latex = "J", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{J}, LATIN CAPITAL LETTER J"}
  , Record {point = "0004B", uchar = "K", latex = "K", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{K}, LATIN CAPITAL LETTER K"}
  , Record {point = "0004C", uchar = "L", latex = "L", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{L}, LATIN CAPITAL LETTER L"}
  , Record {point = "0004D", uchar = "M", latex = "M", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{M}, LATIN CAPITAL LETTER M"}
  , Record {point = "0004E", uchar = "N", latex = "N", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{N}, LATIN CAPITAL LETTER N"}
  , Record {point = "0004F", uchar = "O", latex = "O", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{O}, LATIN CAPITAL LETTER O"}
  , Record {point = "00050", uchar = "P", latex = "P", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{P}, LATIN CAPITAL LETTER P"}
  , Record {point = "00051", uchar = "Q", latex = "Q", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{Q}, LATIN CAPITAL LETTER Q"}
  , Record {point = "00052", uchar = "R", latex = "R", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{R}, LATIN CAPITAL LETTER R"}
  , Record {point = "00053", uchar = "S", latex = "S", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{S}, LATIN CAPITAL LETTER S"}
  , Record {point = "00054", uchar = "T", latex = "T", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{T}, LATIN CAPITAL LETTER T"}
  , Record {point = "00055", uchar = "U", latex = "U", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{U}, LATIN CAPITAL LETTER U"}
  , Record {point = "00056", uchar = "V", latex = "V", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{V}, LATIN CAPITAL LETTER V"}
  , Record {point = "00057", uchar = "W", latex = "W", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{W}, LATIN CAPITAL LETTER W"}
  , Record {point = "00058", uchar = "X", latex = "X", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{X}, LATIN CAPITAL LETTER X"}
  , Record {point = "00059", uchar = "Y", latex = "Y", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{Y}, LATIN CAPITAL LETTER Y"}
  , Record {point = "0005A", uchar = "Z", latex = "Z", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{Z}, LATIN CAPITAL LETTER Z"}
  , Record {point = "0005B", uchar = "[", latex = "\\lbrack", unicodemath = "\\lbrack", cls = "O", category = "mathopen", requirements = "", comments = "LEFT SQUARE BRACKET"}
  , Record {point = "0005C", uchar = "\\", latex = "\\backslash", unicodemath = "\\backslash", cls = "B", category = "mathord", requirements = "", comments = "REVERSE SOLIDUS"}
  , Record {point = "0005D", uchar = "]", latex = "\\rbrack", unicodemath = "\\rbrack", cls = "C", category = "mathclose", requirements = "", comments = "RIGHT SQUARE BRACKET"}
  , Record {point = "0005E", uchar = "", latex = "\\sphat", unicodemath = "", cls = "N", category = "mathord", requirements = "amsxtra", comments = "= \\hat{}, CIRCUMFLEX ACCENT, TeX superscript operator"}
  , Record {point = "0005F", uchar = "_", latex = "\\_", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "LOW LINE, TeX subscript operator"}
  , Record {point = "00060", uchar = "`", latex = "", unicodemath = "", cls = "D", category = "mathord", requirements = "", comments = "grave, alias for 0300"}
  , Record {point = "00061", uchar = "a", latex = "a", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{a}, LATIN SMALL LETTER A"}
  , Record {point = "00062", uchar = "b", latex = "b", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{b}, LATIN SMALL LETTER B"}
  , Record {point = "00063", uchar = "c", latex = "c", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{c}, LATIN SMALL LETTER C"}
  , Record {point = "00064", uchar = "d", latex = "d", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{d}, LATIN SMALL LETTER D"}
  , Record {point = "00065", uchar = "e", latex = "e", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{e}, LATIN SMALL LETTER E"}
  , Record {point = "00066", uchar = "f", latex = "f", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{f}, LATIN SMALL LETTER F"}
  , Record {point = "00067", uchar = "g", latex = "g", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{g}, LATIN SMALL LETTER G"}
  , Record {point = "00068", uchar = "h", latex = "h", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{h}, LATIN SMALL LETTER H"}
  , Record {point = "00069", uchar = "i", latex = "i", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{i}, LATIN SMALL LETTER I"}
  , Record {point = "0006A", uchar = "j", latex = "j", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{j}, LATIN SMALL LETTER J"}
  , Record {point = "0006B", uchar = "k", latex = "k", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{k}, LATIN SMALL LETTER K"}
  , Record {point = "0006C", uchar = "l", latex = "l", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{l}, LATIN SMALL LETTER L"}
  , Record {point = "0006D", uchar = "m", latex = "m", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{m}, LATIN SMALL LETTER M"}
  , Record {point = "0006E", uchar = "n", latex = "n", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{n}, LATIN SMALL LETTER N"}
  , Record {point = "0006F", uchar = "o", latex = "o", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{o}, LATIN SMALL LETTER O"}
  , Record {point = "00070", uchar = "p", latex = "p", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{p}, LATIN SMALL LETTER P"}
  , Record {point = "00071", uchar = "q", latex = "q", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{q}, LATIN SMALL LETTER Q"}
  , Record {point = "00072", uchar = "r", latex = "r", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{r}, LATIN SMALL LETTER R"}
  , Record {point = "00073", uchar = "s", latex = "s", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{s}, LATIN SMALL LETTER S"}
  , Record {point = "00074", uchar = "t", latex = "t", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{t}, LATIN SMALL LETTER T"}
  , Record {point = "00075", uchar = "u", latex = "u", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{u}, LATIN SMALL LETTER U"}
  , Record {point = "00076", uchar = "v", latex = "v", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{v}, LATIN SMALL LETTER V"}
  , Record {point = "00077", uchar = "w", latex = "w", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{w}, LATIN SMALL LETTER W"}
  , Record {point = "00078", uchar = "x", latex = "x", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{x}, LATIN SMALL LETTER X"}
  , Record {point = "00079", uchar = "y", latex = "y", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{y}, LATIN SMALL LETTER Y"}
  , Record {point = "0007A", uchar = "z", latex = "z", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{z}, LATIN SMALL LETTER Z"}
  , Record {point = "0007B", uchar = "{", latex = "\\{", unicodemath = "\\lbrace", cls = "O", category = "mathopen", requirements = "", comments = "= \\lbrace, LEFT CURLY BRACKET"}
  , Record {point = "0007C", uchar = "|", latex = "|", unicodemath = "\\vert", cls = "F", category = "mathfence", requirements = "", comments = "= \\vert, vertical bar"}
  , Record {point = "0007D", uchar = "}", latex = "\\}", unicodemath = "\\rbrace", cls = "C", category = "mathclose", requirements = "", comments = "= \\rbrace, RIGHT CURLY BRACKET"}
  , Record {point = "0007E", uchar = "~", latex = "\\sptilde", unicodemath = "", cls = "N", category = "mathord", requirements = "amsxtra", comments = "# \\sim, TILDE"}
  , Record {point = "000A0", uchar = "\160", latex = "~", unicodemath = "", cls = "S", category = "", requirements = "", comments = "nbsp"}
  , Record {point = "000A1", uchar = "\161", latex = "", unicodemath = "", cls = "P", category = "", requirements = "", comments = "iexcl"}
  , Record {point = "000A2", uchar = "\162", latex = "\\cent", unicodemath = "", cls = "N", category = "mathord", requirements = "wasysym", comments = "= \\mathcent (txfonts), cent"}
  , Record {point = "000A3", uchar = "\163", latex = "\\pounds", unicodemath = "\\sterling", cls = "N", category = "mathord", requirements = "-fourier -omlmathit", comments = "= \\mathsterling (txfonts), POUND SIGN, fourier prints a dollar sign"}
  , Record {point = "000A4", uchar = "\164", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "t \\currency (wasysym), curren"}
  , Record {point = "000A5", uchar = "\165", latex = "\\yen", unicodemath = "\\yen", cls = "N", category = "mathord", requirements = "amsfonts", comments = "YEN SIGN"}
  , Record {point = "000A6", uchar = "\166", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "brvbar (vertical)"}
  , Record {point = "000A7", uchar = "\167", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "sect"}
  , Record {point = "000A8", uchar = "\168", latex = "\\spddot", unicodemath = "", cls = "D", category = "mathord", requirements = "amsxtra", comments = "Dot /die, alias for 0308"}
  , Record {point = "000AC", uchar = "\172", latex = "\\neg", unicodemath = "\\neg", cls = "U", category = "mathord", requirements = "", comments = "= \\lnot, NOT SIGN"}
  , Record {point = "000AE", uchar = "\174", latex = "\\circledR", unicodemath = "", cls = "X", category = "mathord", requirements = "amsfonts", comments = "REGISTERED SIGN"}
  , Record {point = "000AF", uchar = "\175", latex = "", unicodemath = "", cls = "D", category = "mathord", requirements = "", comments = "macr, alias for 0304"}
  , Record {point = "000B0", uchar = "\176", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "deg"}
  , Record {point = "000B1", uchar = "\177", latex = "\\pm", unicodemath = "\\pm", cls = "V", category = "mathbin", requirements = "", comments = "plus-or-minus sign"}
  , Record {point = "000B2", uchar = "\178", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "sup2"}
  , Record {point = "000B3", uchar = "\179", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "sup3"}
  , Record {point = "000B4", uchar = "\180", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "acute, alias for 0301"}
  , Record {point = "000B5", uchar = "\181", latex = "\\Micro", unicodemath = "", cls = "N", category = "mathalpha", requirements = "wrisym", comments = "= \\tcmu (mathcomp), t \\textmu (textcomp), # \\mathrm{\\mu} (omlmathrm), # \\muup (kpfonts mathdesign), MICRO SIGN"}
  , Record {point = "000B6", uchar = "\182", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "para (paragraph sign, pilcrow)"}
  , Record {point = "000B7", uchar = "\183", latex = "", unicodemath = "\\cdotp", cls = "B", category = "mathbin", requirements = "", comments = "# \\cdot, x \\centerdot, b: MIDDLE DOT"}
  , Record {point = "000B9", uchar = "\185", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "sup1"}
  , Record {point = "000BC", uchar = "\188", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "frac14"}
  , Record {point = "000BD", uchar = "\189", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "frac12"}
  , Record {point = "000BE", uchar = "\190", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "frac34"}
  , Record {point = "000BF", uchar = "\191", latex = "", unicodemath = "", cls = "P", category = "", requirements = "", comments = "iquest"}
  , Record {point = "000D7", uchar = "\215", latex = "\\times", unicodemath = "\\times", cls = "B", category = "mathbin", requirements = "", comments = "MULTIPLICATION SIGN, z notation Cartesian product"}
  , Record {point = "000F0", uchar = "\240", latex = "\\eth", unicodemath = "\\matheth", cls = "", category = "mathalpha", requirements = "amssymb arevmath", comments = "eth"}
  , Record {point = "000F7", uchar = "\247", latex = "\\div", unicodemath = "\\div", cls = "B", category = "mathbin", requirements = "", comments = "divide sign"}
  , Record {point = "00131", uchar = "\305", latex = "\\imath", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "imath"}
  , Record {point = "001B5", uchar = "\437", latex = "", unicodemath = "\\Zbar", cls = "", category = "mathord", requirements = "", comments = "impedance"}
  , Record {point = "00237", uchar = "\567", latex = "\\jmath", unicodemath = "", cls = "A", category = "mathalpha", requirements = "-literal", comments = "jmath"}
  , Record {point = "002C6", uchar = "\710", latex = "\\hat{}", unicodemath = "\\hat{}", cls = "D", category = "mathalpha", requirements = "", comments = "circ, alias for 0302"}
  , Record {point = "002C7", uchar = "\711", latex = "", unicodemath = "", cls = "D", category = "mathalpha", requirements = "", comments = "CARON, alias for 030C"}
  , Record {point = "002D8", uchar = "\728", latex = "", unicodemath = "", cls = "D", category = "mathord", requirements = "", comments = "BREVE, alias for 0306"}
  , Record {point = "002D9", uchar = "\729", latex = "", unicodemath = "", cls = "D", category = "mathord", requirements = "", comments = "dot, alias for 0307"}
  , Record {point = "002DA", uchar = "\730", latex = "", unicodemath = "", cls = "D", category = "mathord", requirements = "", comments = "ring, alias for 030A"}
  , Record {point = "002DC", uchar = "\732", latex = "", unicodemath = "", cls = "D", category = "mathord", requirements = "", comments = "tilde, alias for 0303"}
  , Record {point = "00300", uchar = "x\768", latex = "\\grave", unicodemath = "\\grave", cls = "D", category = "mathaccent", requirements = "", comments = "grave accent"}
  , Record {point = "00301", uchar = "x\769", latex = "\\acute", unicodemath = "\\acute", cls = "D", category = "mathaccent", requirements = "", comments = "acute accent"}
  , Record {point = "00302", uchar = "x\770", latex = "\\hat", unicodemath = "\\hat", cls = "D", category = "mathaccent", requirements = "", comments = "# \\widehat (amssymb), circumflex accent"}
  , Record {point = "00303", uchar = "x\771", latex = "\\tilde", unicodemath = "\\tilde", cls = "D", category = "mathaccent", requirements = "", comments = "# \\widetilde (yhmath, fourier), tilde"}
  , Record {point = "00304", uchar = "x\772", latex = "\\bar", unicodemath = "\\bar", cls = "D", category = "mathaccent", requirements = "", comments = "macron"}
  , Record {point = "00305", uchar = "x\773", latex = "\\overline", unicodemath = "\\overbar", cls = "D", category = "mathaccent", requirements = "", comments = "overbar embellishment"}
  , Record {point = "00306", uchar = "x\774", latex = "\\breve", unicodemath = "\\breve", cls = "D", category = "mathaccent", requirements = "", comments = "breve"}
  , Record {point = "00307", uchar = "x\775", latex = "\\dot", unicodemath = "\\dot", cls = "D", category = "mathaccent", requirements = "-oz", comments = "= \\Dot (wrisym), dot above"}
  , Record {point = "00308", uchar = "x\776", latex = "\\ddot", unicodemath = "\\ddot", cls = "D", category = "mathaccent", requirements = "", comments = "= \\DDot (wrisym), dieresis"}
  , Record {point = "00309", uchar = "x\777", latex = "", unicodemath = "\\ovhook", cls = "", category = "mathaccent", requirements = "", comments = "COMBINING HOOK ABOVE"}
  , Record {point = "0030A", uchar = "x\778", latex = "\\mathring", unicodemath = "\\ocirc", cls = "D", category = "mathaccent", requirements = "amssymb", comments = "= \\ring (yhmath), ring"}
  , Record {point = "0030C", uchar = "x\780", latex = "\\check", unicodemath = "\\check", cls = "D", category = "mathaccent", requirements = "", comments = "caron"}
  , Record {point = "00310", uchar = "x\784", latex = "", unicodemath = "\\candra", cls = "", category = "mathaccent", requirements = "", comments = "candrabindu (non-spacing)"}
  , Record {point = "00311", uchar = "x\785", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING INVERTED BREVE"}
  , Record {point = "00312", uchar = "x\786", latex = "", unicodemath = "\\oturnedcomma", cls = "", category = "mathaccent", requirements = "", comments = "COMBINING TURNED COMMA ABOVE"}
  , Record {point = "00315", uchar = "x\789", latex = "", unicodemath = "\\ocommatopright", cls = "", category = "mathaccent", requirements = "", comments = "COMBINING COMMA ABOVE RIGHT"}
  , Record {point = "0031A", uchar = "x\794", latex = "", unicodemath = "\\droang", cls = "", category = "mathaccent", requirements = "", comments = "left angle above (non-spacing)"}
  , Record {point = "00323", uchar = "x\803", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING DOT BELOW"}
  , Record {point = "0032C", uchar = "x\812", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING CARON BELOW"}
  , Record {point = "0032D", uchar = "x\813", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING CIRCUMFLEX ACCENT BELOW"}
  , Record {point = "0032E", uchar = "x\814", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING BREVE BELOW"}
  , Record {point = "0032F", uchar = "x\815", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING INVERTED BREVE BELOW"}
  , Record {point = "00330", uchar = "x\816", latex = "\\utilde", unicodemath = "\\wideutilde", cls = "D", category = "mathaccent", requirements = "undertilde", comments = "under tilde accent (multiple characters and non-spacing)"}
  , Record {point = "00331", uchar = "x\817", latex = "\\underbar", unicodemath = "\\underbar", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING MACRON BELOW"}
  , Record {point = "00332", uchar = "x\818", latex = "\\underline", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING LOW LINE"}
  , Record {point = "00333", uchar = "x\819", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "2lowbar"}
  , Record {point = "00338", uchar = "x\824", latex = "\\not", unicodemath = "\\not", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING LONG SOLIDUS OVERLAY"}
  , Record {point = "0033A", uchar = "x\826", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING INVERTED BRIDGE BELOW"}
  , Record {point = "0033F", uchar = "x\831", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING DOUBLE OVERLINE"}
  , Record {point = "00346", uchar = "x\838", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING BRIDGE ABOVE"}
  , Record {point = "00391", uchar = "\913", latex = "", unicodemath = "\\upAlpha", cls = "A", category = "mathalpha", requirements = "", comments = "capital alpha, greek"}
  , Record {point = "00392", uchar = "\914", latex = "", unicodemath = "\\upBeta", cls = "A", category = "mathalpha", requirements = "", comments = "capital beta, greek"}
  , Record {point = "00393", uchar = "\915", latex = "\\Gamma", unicodemath = "\\upGamma", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\Gamma (-slantedGreek), = \\mathrm{\\Gamma}, capital gamma, greek"}
  , Record {point = "00394", uchar = "\916", latex = "\\Delta", unicodemath = "\\upDelta", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\Delta (-slantedGreek), = \\mathrm{\\Delta}, capital delta, greek"}
  , Record {point = "00395", uchar = "\917", latex = "", unicodemath = "\\upEpsilon", cls = "A", category = "mathalpha", requirements = "", comments = "capital epsilon, greek"}
  , Record {point = "00396", uchar = "\918", latex = "", unicodemath = "\\upZeta", cls = "A", category = "mathalpha", requirements = "", comments = "capital zeta, greek"}
  , Record {point = "00397", uchar = "\919", latex = "", unicodemath = "\\upEta", cls = "A", category = "mathalpha", requirements = "", comments = "capital eta, greek"}
  , Record {point = "00398", uchar = "\920", latex = "\\Theta", unicodemath = "\\upTheta", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\Theta (-slantedGreek), = \\mathrm{\\Theta}, capital theta, greek"}
  , Record {point = "00399", uchar = "\921", latex = "", unicodemath = "\\upIota", cls = "A", category = "mathalpha", requirements = "", comments = "capital iota, greek"}
  , Record {point = "0039A", uchar = "\922", latex = "", unicodemath = "\\upKappa", cls = "A", category = "mathalpha", requirements = "", comments = "capital kappa, greek"}
  , Record {point = "0039B", uchar = "\923", latex = "\\Lambda", unicodemath = "\\upLambda", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\Lambda (-slantedGreek), = \\mathrm{\\Lambda}, capital lambda, greek"}
  , Record {point = "0039C", uchar = "\924", latex = "", unicodemath = "\\upMu", cls = "A", category = "mathalpha", requirements = "", comments = "capital mu, greek"}
  , Record {point = "0039D", uchar = "\925", latex = "", unicodemath = "\\upNu", cls = "A", category = "mathalpha", requirements = "", comments = "capital nu, greek"}
  , Record {point = "0039E", uchar = "\926", latex = "\\Xi", unicodemath = "\\upXi", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\Xi (-slantedGreek), = \\mathrm{\\Xi}, capital xi, greek"}
  , Record {point = "0039F", uchar = "\927", latex = "", unicodemath = "\\upOmicron", cls = "A", category = "mathalpha", requirements = "", comments = "capital omicron, greek"}
  , Record {point = "003A0", uchar = "\928", latex = "\\Pi", unicodemath = "\\upPi", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\Pi (-slantedGreek), = \\mathrm{\\Pi}, capital pi, greek"}
  , Record {point = "003A1", uchar = "\929", latex = "", unicodemath = "\\upRho", cls = "A", category = "mathalpha", requirements = "", comments = "capital rho, greek"}
  , Record {point = "003A3", uchar = "\931", latex = "\\Sigma", unicodemath = "\\upSigma", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\Sigma (-slantedGreek), = \\mathrm{\\Sigma}, capital sigma, greek"}
  , Record {point = "003A4", uchar = "\932", latex = "", unicodemath = "\\upTau", cls = "A", category = "mathalpha", requirements = "", comments = "capital tau, greek"}
  , Record {point = "003A5", uchar = "\933", latex = "\\Upsilon", unicodemath = "\\upUpsilon", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\Upsilon (-slantedGreek), = \\mathrm{\\Upsilon}, capital upsilon, greek"}
  , Record {point = "003A6", uchar = "\934", latex = "\\Phi", unicodemath = "\\upPhi", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\Phi (-slantedGreek), = \\mathrm{\\Phi}, capital phi, greek"}
  , Record {point = "003A7", uchar = "\935", latex = "", unicodemath = "\\upChi", cls = "A", category = "mathalpha", requirements = "", comments = "capital chi, greek"}
  , Record {point = "003A8", uchar = "\936", latex = "\\Psi", unicodemath = "\\upPsi", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\Psi (-slantedGreek), = \\mathrm{\\Psi}, capital psi, greek"}
  , Record {point = "003A9", uchar = "\937", latex = "\\Omega", unicodemath = "\\upOmega", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\Omega (-slantedGreek), = \\mathrm{\\Omega}, capital omega, greek"}
  , Record {point = "003B1", uchar = "\945", latex = "\\alpha", unicodemath = "\\upalpha", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\alpha} (omlmathrm), = \\alphaup (kpfonts mathdesign), = \\upalpha (upgreek), alpha, greek"}
  , Record {point = "003B2", uchar = "\946", latex = "\\beta", unicodemath = "\\upbeta", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\beta} (omlmathrm), = \\betaup (kpfonts mathdesign), = \\upbeta (upgreek), beta, greek"}
  , Record {point = "003B3", uchar = "\947", latex = "\\gamma", unicodemath = "\\upgamma", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\gamma} (omlmathrm), = \\gammaup (kpfonts mathdesign), = \\upgamma (upgreek), gamma, greek"}
  , Record {point = "003B4", uchar = "\948", latex = "\\delta", unicodemath = "\\updelta", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\delta} (omlmathrm), = \\deltaup (kpfonts mathdesign), = \\updelta (upgreek), delta, greek"}
  , Record {point = "003B5", uchar = "\949", latex = "\\varepsilon", unicodemath = "\\upepsilon", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\varepsilon} (omlmathrm), = \\varepsilonup (kpfonts mathdesign), = \\upepsilon (upgreek), rounded epsilon, greek"}
  , Record {point = "003B6", uchar = "\950", latex = "\\zeta", unicodemath = "\\upzeta", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\zeta} (omlmathrm), = \\zetaup (kpfonts mathdesign), = \\upzeta (upgreek), zeta, greek"}
  , Record {point = "003B7", uchar = "\951", latex = "\\eta", unicodemath = "\\upeta", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\eta} (omlmathrm), = \\etaup (kpfonts mathdesign), = \\upeta (upgreek), eta, greek"}
  , Record {point = "003B8", uchar = "\952", latex = "\\theta", unicodemath = "\\uptheta", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\theta} (omlmathrm), = \\thetaup (kpfonts mathdesign), straight theta, = \\uptheta (upgreek), theta, greek"}
  , Record {point = "003B9", uchar = "\953", latex = "\\iota", unicodemath = "\\upiota", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\iota} (omlmathrm), = \\iotaup (kpfonts mathdesign), = \\upiota (upgreek), iota, greek"}
  , Record {point = "003BA", uchar = "\954", latex = "\\kappa", unicodemath = "\\upkappa", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\kappa} (omlmathrm), = \\kappaup (kpfonts mathdesign), = \\upkappa (upgreek), kappa, greek"}
  , Record {point = "003BB", uchar = "\955", latex = "\\lambda", unicodemath = "\\uplambda", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\lambda} (omlmathrm), = \\lambdaup (kpfonts mathdesign), = \\uplambda (upgreek), lambda, greek"}
  , Record {point = "003BC", uchar = "\956", latex = "\\mu", unicodemath = "\\upmu", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\mu} (omlmathrm), = \\muup (kpfonts mathdesign), = \\upmu (upgreek), mu, greek"}
  , Record {point = "003BD", uchar = "\957", latex = "\\nu", unicodemath = "\\upnu", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\nu} (omlmathrm), = \\nuup (kpfonts mathdesign), = \\upnu (upgreek), nu, greek"}
  , Record {point = "003BE", uchar = "\958", latex = "\\xi", unicodemath = "\\upxi", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\xi} (omlmathrm), = \\xiup (kpfonts mathdesign), = \\upxi (upgreek), xi, greek"}
  , Record {point = "003BF", uchar = "\959", latex = "", unicodemath = "\\upomicron", cls = "A", category = "mathalpha", requirements = "", comments = "small omicron, greek"}
  , Record {point = "003C0", uchar = "\960", latex = "\\pi", unicodemath = "\\uppi", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\pi} (omlmathrm), = \\piup (kpfonts mathdesign), = \\uppi (upgreek), pi, greek"}
  , Record {point = "003C1", uchar = "\961", latex = "\\rho", unicodemath = "\\uprho", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\rho} (omlmathrm), = \\rhoup (kpfonts mathdesign), = \\uprho (upgreek), rho, greek"}
  , Record {point = "003C2", uchar = "\962", latex = "\\varsigma", unicodemath = "\\upvarsigma", cls = "", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\varsigma} (omlmathrm), = \\varsigmaup (kpfonts mathdesign), = \\upvarsigma (upgreek), terminal sigma, greek"}
  , Record {point = "003C3", uchar = "\963", latex = "\\sigma", unicodemath = "\\upsigma", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\sigma} (omlmathrm), = \\sigmaup (kpfonts mathdesign), = \\upsigma (upgreek), sigma, greek"}
  , Record {point = "003C4", uchar = "\964", latex = "\\tau", unicodemath = "\\uptau", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\tau} (omlmathrm), = \\tauup (kpfonts mathdesign), = \\uptau (upgreek), tau, greek"}
  , Record {point = "003C5", uchar = "\965", latex = "\\upsilon", unicodemath = "\\upupsilon", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\upsilon} (omlmathrm), = \\upsilonup (kpfonts mathdesign), = \\upupsilon (upgreek), upsilon, greek"}
  , Record {point = "003C6", uchar = "\966", latex = "\\varphi", unicodemath = "\\upvarphi", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\varphi} (omlmathrm), = \\varphiup (kpfonts mathdesign), = \\upvarphi (upgreek), curly or open phi, greek"}
  , Record {point = "003C7", uchar = "\967", latex = "\\chi", unicodemath = "\\upchi", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\chi} (omlmathrm), = \\chiup (kpfonts mathdesign), = \\upchi (upgreek), chi, greek"}
  , Record {point = "003C8", uchar = "\968", latex = "\\psi", unicodemath = "\\uppsi", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\psi} (omlmathrm), = \\psiup (kpfonts mathdesign), = \\uppsi (upgreek), psi, greek"}
  , Record {point = "003C9", uchar = "\969", latex = "\\omega", unicodemath = "\\upomega", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\omega} (omlmathrm), = \\omegaup (kpfonts mathdesign), = \\upomega (upgreek), omega, greek"}
  , Record {point = "003D0", uchar = "\976", latex = "\\varbeta", unicodemath = "\\upvarbeta", cls = "A", category = "mathalpha", requirements = "arevmath", comments = "rounded beta, greek"}
  , Record {point = "003D1", uchar = "\977", latex = "\\vartheta", unicodemath = "\\upvartheta", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\vartheta} (omlmathrm), = \\varthetaup (kpfonts mathdesign), curly or open theta"}
  , Record {point = "003D2", uchar = "\978", latex = "", unicodemath = "\\upUpsilon", cls = "A", category = "mathalpha", requirements = "", comments = "# \\mathrm{\\Upsilon}, GREEK UPSILON WITH HOOK SYMBOL"}
  , Record {point = "003D5", uchar = "\981", latex = "\\phi", unicodemath = "\\upphi", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\phi} (omlmathrm), = \\phiup (kpfonts mathdesign), GREEK PHI SYMBOL (straight)"}
  , Record {point = "003D6", uchar = "\982", latex = "\\varpi", unicodemath = "\\upvarpi", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\varpi} (omlmathrm), = \\varpiup (kpfonts mathdesign), GREEK PI SYMBOL (pomega)"}
  , Record {point = "003D8", uchar = "\984", latex = "\\Qoppa", unicodemath = "\\upoldKoppa", cls = "N", category = "mathord", requirements = "arevmath", comments = "= \\Koppa (wrisym), t \\Qoppa (LGR), GREEK LETTER ARCHAIC KOPPA"}
  , Record {point = "003D9", uchar = "\985", latex = "\\qoppa", unicodemath = "\\upoldkoppa", cls = "N", category = "mathord", requirements = "arevmath", comments = "= \\koppa (wrisym), t \\qoppa (LGR), GREEK SMALL LETTER ARCHAIC KOPPA"}
  , Record {point = "003DA", uchar = "\986", latex = "\\Stigma", unicodemath = "\\upStigma", cls = "A", category = "mathalpha", requirements = "arevmath wrisym", comments = "capital stigma"}
  , Record {point = "003DB", uchar = "\987", latex = "\\stigma", unicodemath = "\\upstigma", cls = "A", category = "mathalpha", requirements = "arevmath wrisym", comments = "GREEK SMALL LETTER STIGMA"}
  , Record {point = "003DC", uchar = "\988", latex = "\\Digamma", unicodemath = "\\upDigamma", cls = "A", category = "mathalpha", requirements = "wrisym -amssymb", comments = "= \\digamma (amssymb), capital digamma"}
  , Record {point = "003DD", uchar = "\989", latex = "\\digamma", unicodemath = "\\updigamma", cls = "A", category = "mathalpha", requirements = "arevmath wrisym", comments = "GREEK SMALL LETTER DIGAMMA"}
  , Record {point = "003DE", uchar = "\990", latex = "\\Koppa", unicodemath = "\\upKoppa", cls = "", category = "mathalpha", requirements = "arevmath", comments = "capital koppa"}
  , Record {point = "003DF", uchar = "\991", latex = "\\koppa", unicodemath = "\\upkoppa", cls = "", category = "mathalpha", requirements = "arevmath", comments = "GREEK SMALL LETTER KOPPA"}
  , Record {point = "003E0", uchar = "\992", latex = "\\Sampi", unicodemath = "\\upSampi", cls = "A", category = "mathalpha", requirements = "arevmath wrisym", comments = "capital sampi"}
  , Record {point = "003E1", uchar = "\993", latex = "\\sampi", unicodemath = "\\upsampi", cls = "A", category = "mathalpha", requirements = "arevmath", comments = "# \\sampi (wrisym), GREEK SMALL LETTER SAMPI"}
  , Record {point = "003F0", uchar = "\1008", latex = "", unicodemath = "\\upvarkappa", cls = "A", category = "mathalpha", requirements = "", comments = "GREEK KAPPA SYMBOL (round)"}
  , Record {point = "003F1", uchar = "\1009", latex = "\\varrho", unicodemath = "\\upvarrho", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\varrho} (omlmathrm), = \\varrhoup (kpfonts mathdesign), GREEK RHO SYMBOL (round)"}
  , Record {point = "003F4", uchar = "\1012", latex = "", unicodemath = "\\upvarTheta", cls = "A", category = "mathalpha", requirements = "", comments = "x \\varTheta (amssymb), GREEK CAPITAL THETA SYMBOL"}
  , Record {point = "003F5", uchar = "\1013", latex = "\\epsilon", unicodemath = "\\upvarepsilon", cls = "A", category = "mathalpha", requirements = "-literal", comments = "= \\mathrm{\\epsilon} (omlmathrm), = \\epsilonup (kpfonts mathdesign), GREEK LUNATE EPSILON SYMBOL"}
  , Record {point = "003F6", uchar = "\1014", latex = "\\backepsilon", unicodemath = "\\upbackepsilon", cls = "N", category = "mathord", requirements = "amssymb wrisym", comments = "GREEK REVERSED LUNATE EPSILON SYMBOL"}
  , Record {point = "00428", uchar = "\1064", latex = "", unicodemath = "", cls = "A", category = "mathalpha", requirements = "", comments = "t \\CYRSHHA (T2A), Shcy, CYRILLIC CAPITAL LETTER SHA"}
  , Record {point = "02000", uchar = "\8192", latex = "", unicodemath = "", cls = "S", category = "", requirements = "", comments = "enquad"}
  , Record {point = "02001", uchar = "\8193", latex = "\\quad", unicodemath = "", cls = "S", category = "", requirements = "", comments = "emquad"}
  , Record {point = "02002", uchar = "\8194", latex = "", unicodemath = "", cls = "S", category = "", requirements = "", comments = "ensp (half an em)"}
  , Record {point = "02003", uchar = "\8195", latex = "", unicodemath = "", cls = "S", category = "", requirements = "", comments = "emsp"}
  , Record {point = "02004", uchar = "\8196", latex = "", unicodemath = "", cls = "S", category = "", requirements = "", comments = "THREE-PER-EM SPACE"}
  , Record {point = "02005", uchar = "\8197", latex = "", unicodemath = "", cls = "S", category = "", requirements = "", comments = "FOUR-PER-EM SPACE, mid space"}
  , Record {point = "02006", uchar = "\8198", latex = "", unicodemath = "", cls = "S", category = "", requirements = "", comments = "SIX-PER-EM SPACE"}
  , Record {point = "02007", uchar = "\8199", latex = "", unicodemath = "", cls = "S", category = "", requirements = "", comments = "FIGURE SPACE"}
  , Record {point = "02009", uchar = "\8201", latex = "\\,", unicodemath = "", cls = "S", category = "", requirements = "", comments = "THIN SPACE"}
  , Record {point = "0200A", uchar = "\8202", latex = "", unicodemath = "", cls = "S", category = "", requirements = "", comments = "HAIR SPACE"}
  , Record {point = "0200B", uchar = "\8203", latex = "", unicodemath = "", cls = "S", category = "", requirements = "", comments = "# \\hspace{0pt}, zwsp"}
  , Record {point = "02010", uchar = "\8208", latex = "", unicodemath = "", cls = "P", category = "mathord", requirements = "", comments = "HYPHEN (true graphic)"}
  , Record {point = "02012", uchar = "\8210", latex = "", unicodemath = "", cls = "P", category = "mathord", requirements = "", comments = "dash"}
  , Record {point = "02013", uchar = "\8211", latex = "", unicodemath = "", cls = "P", category = "mathord", requirements = "", comments = "ndash"}
  , Record {point = "02014", uchar = "\8212", latex = "", unicodemath = "", cls = "P", category = "mathord", requirements = "", comments = "mdash"}
  , Record {point = "02015", uchar = "\8213", latex = "", unicodemath = "\\horizbar", cls = "", category = "mathord", requirements = "", comments = "HORIZONTAL BAR"}
  , Record {point = "02016", uchar = "\8214", latex = "\\|", unicodemath = "\\Vert", cls = "F", category = "mathfence", requirements = "", comments = "= \\Vert, double vertical bar"}
  , Record {point = "02017", uchar = "\8215", latex = "", unicodemath = "\\twolowline", cls = "", category = "mathord", requirements = "", comments = "DOUBLE LOW LINE (spacing)"}
  , Record {point = "0201C", uchar = "\8220", latex = "``", unicodemath = "``", cls = "O", category = "mathpunct", requirements = "", comments = "Left curly quote" }
  , Record {point = "0201D", uchar = "\8221", latex = "\"", unicodemath = "\"", cls = "O", category = "mathpunct", requirements = "", comments = "Right curly quote" }
  , Record {point = "02020", uchar = "\8224", latex = "\\dagger", unicodemath = "\\dagger", cls = "N", category = "mathbin", requirements = "", comments = "DAGGER relation"}
  , Record {point = "02021", uchar = "\8225", latex = "\\ddagger", unicodemath = "\\ddagger", cls = "N", category = "mathbin", requirements = "", comments = "DOUBLE DAGGER relation"}
  , Record {point = "02022", uchar = "\8226", latex = "", unicodemath = "\\smblkcircle", cls = "B", category = "mathbin", requirements = "", comments = "# \\bullet, b: round BULLET, filled"}
  , Record {point = "02025", uchar = "\8229", latex = "", unicodemath = "\\enleadertwodots", cls = "", category = "mathord", requirements = "", comments = "double baseline dot (en leader)"}
  , Record {point = "02026", uchar = "\8230", latex = "\\ldots", unicodemath = "\\unicodeellipsis", cls = "N", category = "mathord", requirements = "", comments = "ellipsis (horizontal)"}
  , Record {point = "02032", uchar = "\8242", latex = "\\prime", unicodemath = "\\prime", cls = "N", category = "mathord", requirements = "", comments = "PRIME or minute, not superscripted"}
  , Record {point = "02033", uchar = "\8243", latex = "\\second", unicodemath = "\\dprime", cls = "N", category = "mathord", requirements = "mathabx", comments = "DOUBLE PRIME or second, not superscripted"}
  , Record {point = "02034", uchar = "\8244", latex = "\\third", unicodemath = "\\trprime", cls = "N", category = "mathord", requirements = "mathabx", comments = "TRIPLE PRIME (not superscripted)"}
  , Record {point = "02035", uchar = "\8245", latex = "\\backprime", unicodemath = "\\backprime", cls = "N", category = "mathord", requirements = "amssymb", comments = "reverse prime, not superscripted"}
  , Record {point = "02036", uchar = "\8246", latex = "", unicodemath = "\\backdprime", cls = "N", category = "mathord", requirements = "", comments = "double reverse prime, not superscripted"}
  , Record {point = "02037", uchar = "\8247", latex = "", unicodemath = "\\backtrprime", cls = "N", category = "mathord", requirements = "", comments = "triple reverse prime, not superscripted"}
  , Record {point = "02038", uchar = "\8248", latex = "", unicodemath = "\\caretinsert", cls = "", category = "mathord", requirements = "", comments = "CARET (insertion mark)"}
  , Record {point = "0203B", uchar = "\8251", latex = "", unicodemath = "", cls = "N", category = "", requirements = "", comments = "REFERENCE MARK, Japanese kome jirushi"}
  , Record {point = "0203C", uchar = "\8252", latex = "", unicodemath = "\\Exclam", cls = "N", category = "mathord", requirements = "", comments = "# !!, DOUBLE EXCLAMATION MARK"}
  , Record {point = "02040", uchar = "\8256", latex = "\\cat", unicodemath = "\\tieconcat", cls = "B", category = "mathbin", requirements = "oz", comments = "CHARACTER TIE, z notation sequence concatenation"}
  , Record {point = "02043", uchar = "\8259", latex = "", unicodemath = "\\hyphenbullet", cls = "", category = "mathord", requirements = "", comments = "rectangle, filled (HYPHEN BULLET)"}
  , Record {point = "02044", uchar = "\8260", latex = "", unicodemath = "\\fracslash", cls = "B", category = "mathbin", requirements = "", comments = "# /, FRACTION SLASH"}
  , Record {point = "02047", uchar = "\8263", latex = "", unicodemath = "\\Question", cls = "", category = "mathord", requirements = "", comments = "# ??, DOUBLE QUESTION MARK"}
  , Record {point = "0204E", uchar = "\8270", latex = "", unicodemath = "", cls = "B", category = "mathbin", requirements = "", comments = "# \\ast, lowast, LOW ASTERISK"}
  , Record {point = "0204F", uchar = "\8271", latex = "", unicodemath = "", cls = "R", category = "", requirements = "", comments = "bsemi, REVERSED SEMICOLON"}
  , Record {point = "02050", uchar = "\8272", latex = "", unicodemath = "\\closure", cls = "R", category = "mathrel", requirements = "", comments = "CLOSE UP (editing mark)"}
  , Record {point = "02051", uchar = "\8273", latex = "", unicodemath = "", cls = "N", category = "", requirements = "", comments = "Ast"}
  , Record {point = "02052", uchar = "\8274", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "# ./., COMMERCIAL MINUS SIGN"}
  , Record {point = "02057", uchar = "\8279", latex = "\\fourth", unicodemath = "\\qprime", cls = "N", category = "mathord", requirements = "mathabx", comments = "QUADRUPLE PRIME, not superscripted"}
  , Record {point = "0205F", uchar = "\8287", latex = "\\:", unicodemath = "", cls = "S", category = "", requirements = "", comments = "= \\medspace (amsmath), MEDIUM MATHEMATICAL SPACE, four-eighteenths of an em"}
  , Record {point = "02061", uchar = "\8289", latex = "", unicodemath = "", cls = "B", category = "", requirements = "", comments = "FUNCTION APPLICATION"}
  , Record {point = "02062", uchar = "\8290", latex = "", unicodemath = "", cls = "B", category = "", requirements = "", comments = "INVISIBLE TIMES"}
  , Record {point = "02063", uchar = "\8291", latex = "", unicodemath = "", cls = "P", category = "", requirements = "", comments = "INVISIBLE SEPARATOR"}
  , Record {point = "02064", uchar = "\8292", latex = "", unicodemath = "", cls = "X", category = "", requirements = "", comments = "INVISIBLE PLUS"}
  , Record {point = "0207A", uchar = "\8314", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "SUPERSCRIPT PLUS SIGN subscript operators"}
  , Record {point = "0207B", uchar = "\8315", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "SUPERSCRIPT MINUS subscript operators"}
  , Record {point = "0207C", uchar = "\8316", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "SUPERSCRIPT EQUALS SIGN subscript operators"}
  , Record {point = "0207D", uchar = "\8317", latex = "", unicodemath = "", cls = "N", category = "mathopen", requirements = "", comments = "SUPERSCRIPT LEFT PARENTHESIS subscript operators"}
  , Record {point = "0207E", uchar = "\8318", latex = "", unicodemath = "", cls = "N", category = "mathclose", requirements = "", comments = "SUPERSCRIPT RIGHT PARENTHESIS subscript operators"}
  , Record {point = "0208A", uchar = "\8330", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "SUBSCRIPT PLUS SIGN superscript operators"}
  , Record {point = "0208B", uchar = "\8331", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "SUBSCRIPT MINUS superscript operators"}
  , Record {point = "0208C", uchar = "\8332", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "SUBSCRIPT EQUALS SIGN superscript operators"}
  , Record {point = "0208D", uchar = "\8333", latex = "", unicodemath = "", cls = "N", category = "mathopen", requirements = "", comments = "SUBSCRIPT LEFT PARENTHESIS superscript operators"}
  , Record {point = "0208E", uchar = "\8334", latex = "", unicodemath = "", cls = "N", category = "mathclose", requirements = "", comments = "SUBSCRIPT RIGHT PARENTHESIS superscript operators"}
  , Record {point = "020AC", uchar = "\8364", latex = "", unicodemath = "\\euro", cls = "", category = "mathord", requirements = "", comments = "EURO SIGN"}
  , Record {point = "020D0", uchar = "x\8400", latex = "\\lvec", unicodemath = "\\leftharpoonaccent", cls = "D", category = "mathaccent", requirements = "wrisym", comments = "COMBINING LEFT HARPOON ABOVE"}
  , Record {point = "020D1", uchar = "x\8401", latex = "\\vec", unicodemath = "\\rightharpoonaccent", cls = "D", category = "mathaccent", requirements = "wrisym", comments = "COMBINING RIGHT HARPOON ABOVE"}
  , Record {point = "020D2", uchar = "x\8402", latex = "", unicodemath = "\\vertoverlay", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING LONG VERTICAL LINE OVERLAY"}
  , Record {point = "020D3", uchar = "x\8403", latex = "", unicodemath = "", cls = "X", category = "mathaccent", requirements = "", comments = "COMBINING SHORT VERTICAL LINE OVERLAY"}
  , Record {point = "020D4", uchar = "x\8404", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING ANTICLOCKWISE ARROW ABOVE"}
  , Record {point = "020D6", uchar = "x\8406", latex = "\\LVec", unicodemath = "\\overleftarrow", cls = "D", category = "mathaccent", requirements = "wrisym", comments = "# \\overleftarrow, COMBINING LEFT ARROW ABOVE"}
  , Record {point = "020D7", uchar = "x\8407", latex = "\\vec", unicodemath = "\\vec", cls = "D", category = "mathaccent", requirements = "-wrisym", comments = "= \\Vec (wrisym), # \\overrightarrow, COMBINING RIGHT ARROW ABOVE"}
  , Record {point = "020D8", uchar = "x\8408", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING RING OVERLAY"}
  , Record {point = "020D9", uchar = "x\8409", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING CLOCKWISE RING OVERLAY"}
  , Record {point = "020DA", uchar = "x\8410", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING ANTICLOCKWISE RING OVERLAY"}
  , Record {point = "020DB", uchar = "x\8411", latex = "\\dddot", unicodemath = "\\dddot", cls = "D", category = "mathaccent", requirements = "amsmath", comments = "= \\DDDot (wrisym), COMBINING THREE DOTS ABOVE"}
  , Record {point = "020DC", uchar = "x\8412", latex = "\\ddddot", unicodemath = "\\ddddot", cls = "D", category = "mathaccent", requirements = "amsmath", comments = "COMBINING FOUR DOTS ABOVE"}
  , Record {point = "020DD", uchar = "x\8413", latex = "", unicodemath = "\\enclosecircle", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING ENCLOSING CIRCLE"}
  , Record {point = "020DE", uchar = "x\8414", latex = "", unicodemath = "\\enclosesquare", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING ENCLOSING SQUARE"}
  , Record {point = "020DF", uchar = "x\8415", latex = "", unicodemath = "\\enclosediamond", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING ENCLOSING DIAMOND"}
  , Record {point = "020E1", uchar = "x\8417", latex = "\\overleftrightarrow", unicodemath = "\\overleftrightarrow", cls = "D", category = "mathaccent", requirements = "amsmath", comments = "COMBINING LEFT RIGHT ARROW ABOVE"}
  , Record {point = "020E4", uchar = "x\8420", latex = "", unicodemath = "\\enclosetriangle", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING ENCLOSING UPWARD POINTING TRIANGLE"}
  , Record {point = "020E5", uchar = "x\8421", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING REVERSE SOLIDUS OVERLAY"}
  , Record {point = "020E6", uchar = "x\8422", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING DOUBLE VERTICAL STROKE OVERLAY, z notation finite function diacritic"}
  , Record {point = "020E7", uchar = "x\8423", latex = "", unicodemath = "\\annuity", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING ANNUITY SYMBOL"}
  , Record {point = "020E8", uchar = "x\8424", latex = "", unicodemath = "\\threeunderdot", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING TRIPLE UNDERDOT"}
  , Record {point = "020E9", uchar = "x\8425", latex = "", unicodemath = "\\widebridgeabove", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING WIDE BRIDGE ABOVE"}
  , Record {point = "020EA", uchar = "x\8426", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING LEFTWARDS ARROW OVERLAY"}
  , Record {point = "020EB", uchar = "x\8427", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING LONG DOUBLE SOLIDUS OVERLAY"}
  , Record {point = "020EC", uchar = "x\8428", latex = "", unicodemath = "\\underrightharpoondown", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING RIGHTWARDS HARPOON WITH BARB DOWNWARDS"}
  , Record {point = "020ED", uchar = "x\8429", latex = "", unicodemath = "\\underleftharpoondown", cls = "D", category = "mathaccent", requirements = "", comments = "COMBINING LEFTWARDS HARPOON WITH BARB DOWNWARDS"}
  , Record {point = "020EE", uchar = "x\8430", latex = "\\underleftarrow", unicodemath = "\\underleftarrow", cls = "D", category = "mathaccent", requirements = "amsmath", comments = "COMBINING LEFT ARROW BELOW"}
  , Record {point = "020EF", uchar = "x\8431", latex = "\\underrightarrow", unicodemath = "\\underrightarrow", cls = "D", category = "mathaccent", requirements = "amsmath", comments = "COMBINING RIGHT ARROW BELOW"}
  , Record {point = "020F0", uchar = "x\8432", latex = "", unicodemath = "\\asteraccent", cls = "", category = "mathaccent", requirements = "", comments = "COMBINING ASTERISK ABOVE"}
  , Record {point = "02102", uchar = "\8450", latex = "\\mathbb{C}", unicodemath = "\\BbbC", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{C} (dsfont), open face C"}
  , Record {point = "02107", uchar = "\8455", latex = "\\Euler", unicodemath = "\\Eulerconst", cls = "N", category = "mathord", requirements = "wrisym", comments = "EULER CONSTANT"}
  , Record {point = "0210A", uchar = "\8458", latex = "\\mathcal{g}", unicodemath = "\\mscrg", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "/scr g, script small letter g"}
  , Record {point = "0210B", uchar = "\8459", latex = "\\mathcal{H}", unicodemath = "\\mscrH", cls = "A", category = "mathalpha", requirements = "", comments = "hamiltonian (script capital H)"}
  , Record {point = "0210C", uchar = "\8460", latex = "\\mathfrak{H}", unicodemath = "\\mfrakH", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "/frak H, black-letter capital H"}
  , Record {point = "0210D", uchar = "\8461", latex = "\\mathbb{H}", unicodemath = "\\BbbH", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{H} (dsfont), open face capital H"}
  , Record {point = "0210E", uchar = "\8462", latex = "", unicodemath = "\\Planckconst", cls = "N", category = "mathord", requirements = "", comments = "# h, Planck constant"}
  , Record {point = "0210F", uchar = "\8463", latex = "\\hslash", unicodemath = "\\hslash", cls = "N", category = "mathalpha", requirements = "amssymb fourier arevmath", comments = "=\\HBar (wrisym), #\\hbar, Planck's h over 2pi"}
  , Record {point = "02110", uchar = "\8464", latex = "\\mathcal{I}", unicodemath = "\\mscrI", cls = "A", category = "mathalpha", requirements = "", comments = "/scr I, script capital I"}
  , Record {point = "02111", uchar = "\8465", latex = "\\Im", unicodemath = "\\Im", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathfrak{I} (eufrak), imaginary part"}
  , Record {point = "02112", uchar = "\8466", latex = "\\mathcal{L}", unicodemath = "\\mscrL", cls = "A", category = "mathalpha", requirements = "", comments = "lagrangian (script capital L)"}
  , Record {point = "02113", uchar = "\8467", latex = "\\ell", unicodemath = "\\ell", cls = "A", category = "mathalpha", requirements = "", comments = "cursive small l"}
  , Record {point = "02115", uchar = "\8469", latex = "\\mathbb{N}", unicodemath = "\\BbbN", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{N} (dsfont), open face N"}
  , Record {point = "02118", uchar = "\8472", latex = "\\wp", unicodemath = "\\wp", cls = "A", category = "mathalpha", requirements = "amssymb", comments = "weierstrass p"}
  , Record {point = "02119", uchar = "\8473", latex = "\\mathbb{P}", unicodemath = "\\BbbP", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{P} (dsfont), open face P"}
  , Record {point = "0211A", uchar = "\8474", latex = "\\mathbb{Q}", unicodemath = "\\BbbQ", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{Q} (dsfont), open face Q"}
  , Record {point = "0211B", uchar = "\8475", latex = "\\mathcal{R}", unicodemath = "\\mscrR", cls = "A", category = "mathalpha", requirements = "", comments = "/scr R, script capital R"}
  , Record {point = "0211C", uchar = "\8476", latex = "\\Re", unicodemath = "\\Re", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathfrak{R} (eufrak), real part"}
  , Record {point = "0211D", uchar = "\8477", latex = "\\mathbb{R}", unicodemath = "\\BbbR", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{R} (dsfont), open face R"}
  , Record {point = "02124", uchar = "\8484", latex = "\\mathbb{Z}", unicodemath = "\\BbbZ", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{Z} (dsfont), open face Z"}
  , Record {point = "02126", uchar = "\8486", latex = "\\tcohm", unicodemath = "", cls = "N", category = "mathalpha", requirements = "mathcomp", comments = "# \\mathrm{\\Omega}, ohm (deprecated in math, use greek letter)"}
  , Record {point = "02127", uchar = "\8487", latex = "\\mho", unicodemath = "\\mho", cls = "N", category = "mathord", requirements = "amsfonts arevmath", comments = "= \\Mho (wrisym), t \\agemO (wasysym), conductance"}
  , Record {point = "02128", uchar = "\8488", latex = "\\mathfrak{Z}", unicodemath = "\\mfrakZ", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "/frak Z, black-letter capital Z"}
  , Record {point = "02129", uchar = "\8489", latex = "", unicodemath = "\\turnediota", cls = "N", category = "mathalpha", requirements = "", comments = "turned iota"}
  , Record {point = "0212B", uchar = "\8491", latex = "\\Angstroem", unicodemath = "\\Angstrom", cls = "A", category = "mathalpha", requirements = "wrisym", comments = "# \\mathring{\\mathrm{A}}, \197ngstr\246m capital A with ring"}
  , Record {point = "0212C", uchar = "\8492", latex = "\\mathcal{B}", unicodemath = "\\mscrB", cls = "A", category = "mathalpha", requirements = "", comments = "bernoulli function (script capital B)"}
  , Record {point = "0212D", uchar = "\8493", latex = "\\mathfrak{C}", unicodemath = "\\mfrakC", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "black-letter capital C"}
  , Record {point = "0212F", uchar = "\8495", latex = "\\mathcal{e}", unicodemath = "\\mscre", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "/scr e, script small letter e"}
  , Record {point = "02130", uchar = "\8496", latex = "\\mathcal{E}", unicodemath = "\\mscrE", cls = "A", category = "mathalpha", requirements = "", comments = "/scr E, script capital E"}
  , Record {point = "02131", uchar = "\8497", latex = "\\mathcal{F}", unicodemath = "\\mscrF", cls = "A", category = "mathalpha", requirements = "", comments = "/scr F, script capital F"}
  , Record {point = "02132", uchar = "\8498", latex = "\\Finv", unicodemath = "\\Finv", cls = "N", category = "mathord", requirements = "amssymb", comments = "TURNED CAPITAL F"}
  , Record {point = "02133", uchar = "\8499", latex = "\\mathcal{M}", unicodemath = "\\mscrM", cls = "A", category = "mathalpha", requirements = "", comments = "physics m-matrix (SCRIPT CAPITAL M)"}
  , Record {point = "02134", uchar = "\8500", latex = "\\mathcal{o}", unicodemath = "\\mscro", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "order of (SCRIPT SMALL O)"}
  , Record {point = "02135", uchar = "\8501", latex = "\\aleph", unicodemath = "\\aleph", cls = "A", category = "mathalpha", requirements = "", comments = "aleph, hebrew"}
  , Record {point = "02136", uchar = "\8502", latex = "\\beth", unicodemath = "\\beth", cls = "A", category = "mathalpha", requirements = "amssymb wrisym", comments = "beth, hebrew"}
  , Record {point = "02137", uchar = "\8503", latex = "\\gimel", unicodemath = "\\gimel", cls = "A", category = "mathalpha", requirements = "amssymb wrisym", comments = "gimel, hebrew"}
  , Record {point = "02138", uchar = "\8504", latex = "\\daleth", unicodemath = "\\daleth", cls = "A", category = "mathalpha", requirements = "amssymb wrisym", comments = "daleth, hebrew"}
  , Record {point = "0213C", uchar = "\8508", latex = "\\mathbb{\\pi}", unicodemath = "\\Bbbpi", cls = "A", category = "mathord", requirements = "bbold", comments = "\\DoublePi (wrisym), DOUBLE-STRUCK SMALL PI"}
  , Record {point = "0213D", uchar = "\8509", latex = "\\mathbb{\\gamma}", unicodemath = "\\Bbbgamma", cls = "A", category = "mathalpha", requirements = "bbold", comments = "\\EulerGamma (wrisym), DOUBLE-STRUCK SMALL GAMMA"}
  , Record {point = "0213E", uchar = "\8510", latex = "\\mathbb{\\Gamma}", unicodemath = "\\BbbGamma", cls = "N", category = "mathalpha", requirements = "bbold", comments = "DOUBLE-STRUCK CAPITAL GAMMA"}
  , Record {point = "0213F", uchar = "\8511", latex = "\\mathbb{\\Pi}", unicodemath = "\\BbbPi", cls = "A", category = "mathalpha", requirements = "bbold", comments = "DOUBLE-STRUCK CAPITAL PI"}
  , Record {point = "02140", uchar = "\8512", latex = "\\mathbb{\\Sigma}", unicodemath = "\\Bbbsum", cls = "L", category = "mathop", requirements = "bbold", comments = "DOUBLE-STRUCK N-ARY SUMMATION"}
  , Record {point = "02141", uchar = "\8513", latex = "", unicodemath = "\\Game", cls = "N", category = "mathord", requirements = "", comments = "# \\Game (amssymb), TURNED SANS-SERIF CAPITAL G (amssymb has mirrored G)"}
  , Record {point = "02142", uchar = "\8514", latex = "", unicodemath = "\\sansLturned", cls = "N", category = "mathord", requirements = "", comments = "TURNED SANS-SERIF CAPITAL L"}
  , Record {point = "02143", uchar = "\8515", latex = "", unicodemath = "\\sansLmirrored", cls = "N", category = "mathord", requirements = "", comments = "REVERSED SANS-SERIF CAPITAL L"}
  , Record {point = "02144", uchar = "\8516", latex = "\\Yup", unicodemath = "\\Yup", cls = "N", category = "mathord", requirements = "stmaryrd", comments = "TURNED SANS-SERIF CAPITAL Y"}
  , Record {point = "02145", uchar = "\8517", latex = "\\CapitalDifferentialD", unicodemath = "\\mitBbbD", cls = "N", category = "mathord", requirements = "wrisym", comments = "= \\DD (wrisym), DOUBLE-STRUCK ITALIC CAPITAL D"}
  , Record {point = "02146", uchar = "\8518", latex = "\\DifferentialD", unicodemath = "\\mitBbbd", cls = "N", category = "mathord", requirements = "wrisym", comments = "= \\dd (wrisym), DOUBLE-STRUCK ITALIC SMALL D"}
  , Record {point = "02147", uchar = "\8519", latex = "\\ExponetialE", unicodemath = "\\mitBbbe", cls = "N", category = "mathord", requirements = "wrisym", comments = "= \\ee (wrisym), DOUBLE-STRUCK ITALIC SMALL E"}
  , Record {point = "02148", uchar = "\8520", latex = "\\ComplexI", unicodemath = "\\mitBbbi", cls = "N", category = "mathord", requirements = "wrisym", comments = "= \\ii (wrisym), DOUBLE-STRUCK ITALIC SMALL I"}
  , Record {point = "02149", uchar = "\8521", latex = "\\ComplexJ", unicodemath = "\\mitBbbj", cls = "N", category = "mathord", requirements = "wrisym", comments = "= \\jj (wrisym), DOUBLE-STRUCK ITALIC SMALL J"}
  , Record {point = "0214A", uchar = "\8522", latex = "", unicodemath = "\\PropertyLine", cls = "", category = "mathord", requirements = "", comments = "PROPERTY LINE"}
  , Record {point = "0214B", uchar = "\8523", latex = "\\invamp", unicodemath = "\\upand", cls = "N", category = "mathbin", requirements = "txfonts", comments = "# \\bindnasrepma (stmaryrd), TURNED AMPERSAND"}
  , Record {point = "02190", uchar = "\8592", latex = "\\leftarrow", unicodemath = "\\leftarrow", cls = "R", category = "mathrel", requirements = "", comments = "= \\gets, a: leftward arrow"}
  , Record {point = "02191", uchar = "\8593", latex = "\\uparrow", unicodemath = "\\uparrow", cls = "R", category = "mathrel", requirements = "", comments = "upward arrow"}
  , Record {point = "02192", uchar = "\8594", latex = "\\rightarrow", unicodemath = "\\rightarrow", cls = "R", category = "mathrel", requirements = "", comments = "= \\to, = \\tfun (oz), = \\fun (oz), rightward arrow, z notation total function"}
  , Record {point = "02193", uchar = "\8595", latex = "\\downarrow", unicodemath = "\\downarrow", cls = "R", category = "mathrel", requirements = "", comments = "downward arrow"}
  , Record {point = "02194", uchar = "\8596", latex = "\\leftrightarrow", unicodemath = "\\leftrightarrow", cls = "R", category = "mathrel", requirements = "-wrisym", comments = "= \\rel (oz), LEFT RIGHT ARROW, z notation relation"}
  , Record {point = "02195", uchar = "\8597", latex = "\\updownarrow", unicodemath = "\\updownarrow", cls = "R", category = "mathrel", requirements = "", comments = "up and down arrow"}
  , Record {point = "02196", uchar = "\8598", latex = "\\nwarrow", unicodemath = "\\nwarrow", cls = "R", category = "mathrel", requirements = "amssymb", comments = "nw pointing arrow"}
  , Record {point = "02197", uchar = "\8599", latex = "\\nearrow", unicodemath = "\\nearrow", cls = "R", category = "mathrel", requirements = "", comments = "ne pointing arrow"}
  , Record {point = "02198", uchar = "\8600", latex = "\\searrow", unicodemath = "\\searrow", cls = "R", category = "mathrel", requirements = "", comments = "se pointing arrow"}
  , Record {point = "02199", uchar = "\8601", latex = "\\swarrow", unicodemath = "\\swarrow", cls = "R", category = "mathrel", requirements = "", comments = "sw pointing arrow"}
  , Record {point = "0219A", uchar = "\8602", latex = "\\nleftarrow", unicodemath = "\\nleftarrow", cls = "R", category = "mathrel", requirements = "amssymb", comments = "not left arrow"}
  , Record {point = "0219B", uchar = "\8603", latex = "\\nrightarrow", unicodemath = "\\nrightarrow", cls = "R", category = "mathrel", requirements = "amssymb", comments = "not right arrow"}
  , Record {point = "0219C", uchar = "\8604", latex = "", unicodemath = "\\leftwavearrow", cls = "R", category = "mathrel", requirements = "", comments = "left arrow-wavy"}
  , Record {point = "0219D", uchar = "\8605", latex = "", unicodemath = "\\rightwavearrow", cls = "R", category = "mathrel", requirements = "", comments = "right arrow-wavy"}
  , Record {point = "0219E", uchar = "\8606", latex = "\\twoheadleftarrow", unicodemath = "\\twoheadleftarrow", cls = "R", category = "mathrel", requirements = "amssymb", comments = "left two-headed arrow"}
  , Record {point = "0219F", uchar = "\8607", latex = "", unicodemath = "\\twoheaduparrow", cls = "R", category = "mathrel", requirements = "", comments = "up two-headed arrow"}
  , Record {point = "021A0", uchar = "\8608", latex = "\\twoheadrightarrow", unicodemath = "\\twoheadrightarrow", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\tsur (oz), = \\surj (oz), right two-headed arrow, z notation total surjection"}
  , Record {point = "021A1", uchar = "\8609", latex = "", unicodemath = "\\twoheaddownarrow", cls = "R", category = "mathrel", requirements = "", comments = "down two-headed arrow"}
  , Record {point = "021A2", uchar = "\8610", latex = "\\leftarrowtail", unicodemath = "\\leftarrowtail", cls = "R", category = "mathrel", requirements = "amssymb", comments = "left arrow-tailed"}
  , Record {point = "021A3", uchar = "\8611", latex = "\\rightarrowtail", unicodemath = "\\rightarrowtail", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\tinj (oz), = \\inj (oz), right arrow-tailed, z notation total injection"}
  , Record {point = "021A4", uchar = "\8612", latex = "\\mapsfrom", unicodemath = "\\mapsfrom", cls = "R", category = "mathrel", requirements = "stmaryrd", comments = "= \\mappedfrom (kpfonts), maps to, leftward"}
  , Record {point = "021A5", uchar = "\8613", latex = "\\MapsUp", unicodemath = "\\mapsup", cls = "R", category = "mathrel", requirements = "wrisym", comments = "maps to, upward"}
  , Record {point = "021A6", uchar = "\8614", latex = "\\mapsto", unicodemath = "\\mapsto", cls = "R", category = "mathrel", requirements = "", comments = "maps to, rightward, z notation maplet"}
  , Record {point = "021A7", uchar = "\8615", latex = "\\MapsDown", unicodemath = "\\mapsdown", cls = "R", category = "mathrel", requirements = "wrisym", comments = "maps to, downward"}
  , Record {point = "021A8", uchar = "\8616", latex = "", unicodemath = "\\updownarrowbar", cls = "R", category = "mathord", requirements = "", comments = "UP DOWN ARROW WITH BASE (perpendicular)"}
  , Record {point = "021A9", uchar = "\8617", latex = "\\hookleftarrow", unicodemath = "\\hookleftarrow", cls = "R", category = "mathrel", requirements = "", comments = "left arrow-hooked"}
  , Record {point = "021AA", uchar = "\8618", latex = "\\hookrightarrow", unicodemath = "\\hookrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "right arrow-hooked"}
  , Record {point = "021AB", uchar = "\8619", latex = "\\looparrowleft", unicodemath = "\\looparrowleft", cls = "R", category = "mathrel", requirements = "amssymb", comments = "left arrow-looped"}
  , Record {point = "021AC", uchar = "\8620", latex = "\\looparrowright", unicodemath = "\\looparrowright", cls = "R", category = "mathrel", requirements = "amssymb", comments = "right arrow-looped"}
  , Record {point = "021AD", uchar = "\8621", latex = "\\leftrightsquigarrow", unicodemath = "\\leftrightsquigarrow", cls = "R", category = "mathrel", requirements = "amssymb", comments = "left and right arr-wavy"}
  , Record {point = "021AE", uchar = "\8622", latex = "\\nleftrightarrow", unicodemath = "\\nleftrightarrow", cls = "R", category = "mathrel", requirements = "amssymb", comments = "not left and right arrow"}
  , Record {point = "021AF", uchar = "\8623", latex = "\\lightning", unicodemath = "\\downzigzagarrow", cls = "R", category = "mathrel", requirements = "stmaryrd", comments = "t \\Lightning (marvosym), DOWNWARDS ZIGZAG ARROW"}
  , Record {point = "021B0", uchar = "\8624", latex = "\\Lsh", unicodemath = "\\Lsh", cls = "R", category = "mathrel", requirements = "amssymb", comments = "a: UPWARDS ARROW WITH TIP LEFTWARDS"}
  , Record {point = "021B1", uchar = "\8625", latex = "\\Rsh", unicodemath = "\\Rsh", cls = "R", category = "mathrel", requirements = "amssymb", comments = "a: UPWARDS ARROW WITH TIP RIGHTWARDS"}
  , Record {point = "021B2", uchar = "\8626", latex = "\\dlsh", unicodemath = "\\Ldsh", cls = "R", category = "mathrel", requirements = "mathabx", comments = "left down angled arrow"}
  , Record {point = "021B3", uchar = "\8627", latex = "\\drsh", unicodemath = "\\Rdsh", cls = "R", category = "mathrel", requirements = "mathabx", comments = "right down angled arrow"}
  , Record {point = "021B4", uchar = "\8628", latex = "", unicodemath = "\\linefeed", cls = "", category = "mathord", requirements = "", comments = "RIGHTWARDS ARROW WITH CORNER DOWNWARDS"}
  , Record {point = "021B5", uchar = "\8629", latex = "", unicodemath = "\\carriagereturn", cls = "", category = "mathord", requirements = "", comments = "downwards arrow with corner leftward = carriage return"}
  , Record {point = "021B6", uchar = "\8630", latex = "\\curvearrowleft", unicodemath = "\\curvearrowleft", cls = "R", category = "mathrel", requirements = "amssymb fourier", comments = "left curved arrow"}
  , Record {point = "021B7", uchar = "\8631", latex = "\\curvearrowright", unicodemath = "\\curvearrowright", cls = "R", category = "mathrel", requirements = "amssymb fourier", comments = "right curved arrow"}
  , Record {point = "021B8", uchar = "\8632", latex = "", unicodemath = "\\barovernorthwestarrow", cls = "", category = "mathord", requirements = "", comments = "NORTH WEST ARROW TO LONG BAR"}
  , Record {point = "021B9", uchar = "\8633", latex = "", unicodemath = "\\barleftarrowrightarrowba", cls = "", category = "mathord", requirements = "", comments = "LEFTWARDS ARROW TO BAR OVER RIGHTWARDS ARROW TO BAR"}
  , Record {point = "021BA", uchar = "\8634", latex = "\\circlearrowleft", unicodemath = "\\acwopencirclearrow", cls = "R", category = "mathord", requirements = "amssymb", comments = "= \\leftturn (wasysym), ANTICLOCKWISE OPEN CIRCLE ARROW"}
  , Record {point = "021BB", uchar = "\8635", latex = "\\circlearrowright", unicodemath = "\\cwopencirclearrow", cls = "R", category = "mathord", requirements = "amssymb", comments = "= \\rightturn (wasysym), CLOCKWISE OPEN CIRCLE ARROW"}
  , Record {point = "021BC", uchar = "\8636", latex = "\\leftharpoonup", unicodemath = "\\leftharpoonup", cls = "R", category = "mathrel", requirements = "", comments = "left harpoon-up"}
  , Record {point = "021BD", uchar = "\8637", latex = "\\leftharpoondown", unicodemath = "\\leftharpoondown", cls = "R", category = "mathrel", requirements = "", comments = "left harpoon-down"}
  , Record {point = "021BE", uchar = "\8638", latex = "\\upharpoonright", unicodemath = "\\upharpoonright", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\restriction (amssymb), = \\upharpoonrightup (wrisym), a: up harpoon-right"}
  , Record {point = "021BF", uchar = "\8639", latex = "\\upharpoonleft", unicodemath = "\\upharpoonleft", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\upharpoonleftup (wrisym), up harpoon-left"}
  , Record {point = "021C0", uchar = "\8640", latex = "\\rightharpoonup", unicodemath = "\\rightharpoonup", cls = "R", category = "mathrel", requirements = "", comments = "right harpoon-up"}
  , Record {point = "021C1", uchar = "\8641", latex = "\\rightharpoondown", unicodemath = "\\rightharpoondown", cls = "R", category = "mathrel", requirements = "", comments = "right harpoon-down"}
  , Record {point = "021C2", uchar = "\8642", latex = "\\downharpoonright", unicodemath = "\\downharpoonright", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\upharpoonrightdown (wrisym), down harpoon-right"}
  , Record {point = "021C3", uchar = "\8643", latex = "\\downharpoonleft", unicodemath = "\\downharpoonleft", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\upharpoonleftdown (wrisym), down harpoon-left"}
  , Record {point = "021C4", uchar = "\8644", latex = "\\rightleftarrows", unicodemath = "\\rightleftarrows", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\rightleftarrow (wrisym), right arrow over left arrow"}
  , Record {point = "021C5", uchar = "\8645", latex = "\\updownarrows", unicodemath = "\\updownarrows", cls = "R", category = "mathrel", requirements = "mathabx", comments = "= \\uparrowdownarrow (wrisym), up arrow, down arrow"}
  , Record {point = "021C6", uchar = "\8646", latex = "\\leftrightarrows", unicodemath = "\\leftrightarrows", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\leftrightarrow (wrisym), left arrow over right arrow"}
  , Record {point = "021C7", uchar = "\8647", latex = "\\leftleftarrows", unicodemath = "\\leftleftarrows", cls = "R", category = "mathrel", requirements = "amssymb fourier", comments = "two left arrows"}
  , Record {point = "021C8", uchar = "\8648", latex = "\\upuparrows", unicodemath = "\\upuparrows", cls = "R", category = "mathrel", requirements = "amssymb", comments = "two up arrows"}
  , Record {point = "021C9", uchar = "\8649", latex = "\\rightrightarrows", unicodemath = "\\rightrightarrows", cls = "R", category = "mathrel", requirements = "amssymb fourier", comments = "two right arrows"}
  , Record {point = "021CA", uchar = "\8650", latex = "\\downdownarrows", unicodemath = "\\downdownarrows", cls = "R", category = "mathrel", requirements = "amssymb", comments = "two down arrows"}
  , Record {point = "021CB", uchar = "\8651", latex = "\\leftrightharpoons", unicodemath = "\\leftrightharpoons", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\revequilibrium (wrisym), left harpoon over right"}
  , Record {point = "021CC", uchar = "\8652", latex = "\\rightleftharpoons", unicodemath = "\\rightleftharpoons", cls = "R", category = "mathrel", requirements = "", comments = "= \\equilibrium (wrisym), right harpoon over left"}
  , Record {point = "021CD", uchar = "\8653", latex = "\\nLeftarrow", unicodemath = "\\nLeftarrow", cls = "R", category = "mathrel", requirements = "amssymb", comments = "not implied by"}
  , Record {point = "021CE", uchar = "\8654", latex = "\\nLeftrightarrow", unicodemath = "\\nLeftrightarrow", cls = "R", category = "mathrel", requirements = "amssymb", comments = "not left and right double arrows"}
  , Record {point = "021CF", uchar = "\8655", latex = "\\nRightarrow", unicodemath = "\\nRightarrow", cls = "R", category = "mathrel", requirements = "amssymb", comments = "not implies"}
  , Record {point = "021D0", uchar = "\8656", latex = "\\Leftarrow", unicodemath = "\\Leftarrow", cls = "R", category = "mathrel", requirements = "", comments = "left double arrow"}
  , Record {point = "021D1", uchar = "\8657", latex = "\\Uparrow", unicodemath = "\\Uparrow", cls = "R", category = "mathrel", requirements = "", comments = "up double arrow"}
  , Record {point = "021D2", uchar = "\8658", latex = "\\Rightarrow", unicodemath = "\\Rightarrow", cls = "R", category = "mathrel", requirements = "-marvosym", comments = "right double arrow"}
  , Record {point = "021D3", uchar = "\8659", latex = "\\Downarrow", unicodemath = "\\Downarrow", cls = "R", category = "mathrel", requirements = "", comments = "down double arrow"}
  , Record {point = "021D4", uchar = "\8660", latex = "\\Leftrightarrow", unicodemath = "\\Leftrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "left and right double arrow"}
  , Record {point = "021D5", uchar = "\8661", latex = "\\Updownarrow", unicodemath = "\\Updownarrow", cls = "R", category = "mathrel", requirements = "", comments = "up and down double arrow"}
  , Record {point = "021D6", uchar = "\8662", latex = "\\Nwarrow", unicodemath = "\\Nwarrow", cls = "R", category = "mathrel", requirements = "txfonts", comments = "nw pointing double arrow"}
  , Record {point = "021D7", uchar = "\8663", latex = "\\Nearrow", unicodemath = "\\Nearrow", cls = "R", category = "mathrel", requirements = "txfonts", comments = "ne pointing double arrow"}
  , Record {point = "021D8", uchar = "\8664", latex = "\\Searrow", unicodemath = "\\Searrow", cls = "R", category = "mathrel", requirements = "txfonts", comments = "se pointing double arrow"}
  , Record {point = "021D9", uchar = "\8665", latex = "\\Swarrow", unicodemath = "\\Swarrow", cls = "R", category = "mathrel", requirements = "txfonts", comments = "sw pointing double arrow"}
  , Record {point = "021DA", uchar = "\8666", latex = "\\Lleftarrow", unicodemath = "\\Lleftarrow", cls = "R", category = "mathrel", requirements = "amssymb", comments = "left triple arrow"}
  , Record {point = "021DB", uchar = "\8667", latex = "\\Rrightarrow", unicodemath = "\\Rrightarrow", cls = "R", category = "mathrel", requirements = "amssymb", comments = "right triple arrow"}
  , Record {point = "021DC", uchar = "\8668", latex = "\\leftsquigarrow", unicodemath = "\\leftsquigarrow", cls = "R", category = "mathrel", requirements = "mathabx txfonts", comments = "LEFTWARDS SQUIGGLE ARROW"}
  , Record {point = "021DD", uchar = "\8669", latex = "\\rightsquigarrow", unicodemath = "\\rightsquigarrow", cls = "R", category = "mathrel", requirements = "amssymb", comments = "RIGHTWARDS SQUIGGLE ARROW"}
  , Record {point = "021DE", uchar = "\8670", latex = "", unicodemath = "\\nHuparrow", cls = "R", category = "mathord", requirements = "", comments = "UPWARDS ARROW WITH DOUBLE STROKE"}
  , Record {point = "021DF", uchar = "\8671", latex = "", unicodemath = "\\nHdownarrow", cls = "R", category = "mathord", requirements = "", comments = "DOWNWARDS ARROW WITH DOUBLE STROKE"}
  , Record {point = "021E0", uchar = "\8672", latex = "\\dashleftarrow", unicodemath = "\\leftdasharrow", cls = "R", category = "mathord", requirements = "amsfonts", comments = "LEFTWARDS DASHED ARROW"}
  , Record {point = "021E1", uchar = "\8673", latex = "", unicodemath = "\\updasharrow", cls = "R", category = "mathord", requirements = "", comments = "UPWARDS DASHED ARROW"}
  , Record {point = "021E2", uchar = "\8674", latex = "\\dashrightarrow", unicodemath = "\\rightdasharrow", cls = "R", category = "mathord", requirements = "amsfonts", comments = "= \\dasharrow (amsfonts), RIGHTWARDS DASHED ARROW"}
  , Record {point = "021E3", uchar = "\8675", latex = "", unicodemath = "\\downdasharrow", cls = "R", category = "mathord", requirements = "", comments = "DOWNWARDS DASHED ARROW"}
  , Record {point = "021E4", uchar = "\8676", latex = "\\LeftArrowBar", unicodemath = "\\barleftarrow", cls = "R", category = "mathrel", requirements = "wrisym", comments = "LEFTWARDS ARROW TO BAR"}
  , Record {point = "021E5", uchar = "\8677", latex = "\\RightArrowBar", unicodemath = "\\rightarrowbar", cls = "R", category = "mathrel", requirements = "wrisym", comments = "RIGHTWARDS ARROW TO BAR"}
  , Record {point = "021E6", uchar = "\8678", latex = "", unicodemath = "\\leftwhitearrow", cls = "R", category = "mathord", requirements = "", comments = "LEFTWARDS WHITE ARROW"}
  , Record {point = "021E7", uchar = "\8679", latex = "", unicodemath = "\\upwhitearrow", cls = "R", category = "mathord", requirements = "", comments = "UPWARDS WHITE ARROW"}
  , Record {point = "021E8", uchar = "\8680", latex = "", unicodemath = "\\rightwhitearrow", cls = "R", category = "mathord", requirements = "", comments = "RIGHTWARDS WHITE ARROW"}
  , Record {point = "021E9", uchar = "\8681", latex = "", unicodemath = "\\downwhitearrow", cls = "R", category = "mathord", requirements = "", comments = "DOWNWARDS WHITE ARROW"}
  , Record {point = "021EA", uchar = "\8682", latex = "", unicodemath = "\\whitearrowupfrombar", cls = "", category = "mathord", requirements = "", comments = "UPWARDS WHITE ARROW FROM BAR"}
  , Record {point = "021EB", uchar = "\8683", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "UPWARDS WHITE ARROW ON PEDESTAL"}
  , Record {point = "021EC", uchar = "\8684", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "UPWARDS WHITE ARROW ON PEDESTAL WITH HORIZONTAL BAR"}
  , Record {point = "021ED", uchar = "\8685", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "UPWARDS WHITE ARROW ON PEDESTAL WITH VERTICAL BAR"}
  , Record {point = "021EE", uchar = "\8686", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "UPWARDS WHITE DOUBLE ARROW"}
  , Record {point = "021EF", uchar = "\8687", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "UPWARDS WHITE DOUBLE ARROW ON PEDESTAL"}
  , Record {point = "021F0", uchar = "\8688", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "RIGHTWARDS WHITE ARROW FROM WALL"}
  , Record {point = "021F1", uchar = "\8689", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "NORTH WEST ARROW TO CORNER"}
  , Record {point = "021F2", uchar = "\8690", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "SOUTH EAST ARROW TO CORNER"}
  , Record {point = "021F3", uchar = "\8691", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "UP DOWN WHITE ARROW"}
  , Record {point = "021F4", uchar = "\8692", latex = "", unicodemath = "\\circleonrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "RIGHT ARROW WITH SMALL CIRCLE"}
  , Record {point = "021F5", uchar = "\8693", latex = "\\downuparrows", unicodemath = "\\downuparrows", cls = "R", category = "mathrel", requirements = "mathabx", comments = "= \\downarrowuparrow (wrisym), DOWNWARDS ARROW LEFTWARDS OF UPWARDS ARROW"}
  , Record {point = "021F6", uchar = "\8694", latex = "", unicodemath = "\\rightthreearrows", cls = "R", category = "mathrel", requirements = "", comments = "THREE RIGHTWARDS ARROWS"}
  , Record {point = "021F7", uchar = "\8695", latex = "", unicodemath = "\\nvleftarrow", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW WITH VERTICAL STROKE"}
  , Record {point = "021F8", uchar = "\8696", latex = "\\pfun", unicodemath = "\\nvrightarrow", cls = "R", category = "mathrel", requirements = "oz", comments = "RIGHTWARDS ARROW WITH VERTICAL STROKE, z notation partial function"}
  , Record {point = "021F9", uchar = "\8697", latex = "", unicodemath = "\\nvleftrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "LEFT RIGHT ARROW WITH VERTICAL STROKE, z notation partial relation"}
  , Record {point = "021FA", uchar = "\8698", latex = "", unicodemath = "\\nVleftarrow", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW WITH DOUBLE VERTICAL STROKE"}
  , Record {point = "021FB", uchar = "\8699", latex = "\\ffun", unicodemath = "\\nVrightarrow", cls = "R", category = "mathrel", requirements = "oz", comments = "RIGHTWARDS ARROW WITH DOUBLE VERTICAL STROKE, z notation finite function"}
  , Record {point = "021FC", uchar = "\8700", latex = "", unicodemath = "\\nVleftrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "LEFT RIGHT ARROW WITH DOUBLE VERTICAL STROKE, z notation finite relation"}
  , Record {point = "021FD", uchar = "\8701", latex = "\\leftarrowtriangle", unicodemath = "\\leftarrowtriangle", cls = "R", category = "mathrel", requirements = "stmaryrd", comments = "LEFTWARDS OPEN-HEADED ARROW"}
  , Record {point = "021FE", uchar = "\8702", latex = "\\rightarrowtriangle", unicodemath = "\\rightarrowtriangle", cls = "R", category = "mathrel", requirements = "stmaryrd", comments = "RIGHTWARDS OPEN-HEADED ARROW"}
  , Record {point = "021FF", uchar = "\8703", latex = "\\leftrightarrowtriangle", unicodemath = "\\leftrightarrowtriangle", cls = "R", category = "mathrel", requirements = "stmaryrd", comments = "LEFT RIGHT OPEN-HEADED ARROW"}
  , Record {point = "02200", uchar = "\8704", latex = "\\forall", unicodemath = "\\forall", cls = "U", category = "mathord", requirements = "", comments = "FOR ALL"}
  , Record {point = "02201", uchar = "\8705", latex = "\\complement", unicodemath = "\\complement", cls = "U", category = "mathord", requirements = "amssymb fourier", comments = "COMPLEMENT sign"}
  , Record {point = "02202", uchar = "\8706", latex = "\\partial", unicodemath = "\\partial", cls = "N", category = "mathord", requirements = "-literal", comments = "= \\partialup (kpfonts), PARTIAL DIFFERENTIAL"}
  , Record {point = "02203", uchar = "\8707", latex = "\\exists", unicodemath = "\\exists", cls = "U", category = "mathord", requirements = "", comments = "= \\exi (oz), at least one exists"}
  , Record {point = "02204", uchar = "\8708", latex = "\\nexists", unicodemath = "\\nexists", cls = "U", category = "mathord", requirements = "amssymb fourier", comments = "= \\nexi (oz), negated exists"}
  , Record {point = "02205", uchar = "\8709", latex = "\\varnothing", unicodemath = "\\varnothing", cls = "N", category = "mathord", requirements = "amssymb", comments = "circle, slash"}
  , Record {point = "02206", uchar = "\8710", latex = "", unicodemath = "\\increment", cls = "U", category = "mathord", requirements = "", comments = "# \\mathrm{\\Delta}, laplacian (Delta; nabla square)"}
  , Record {point = "02207", uchar = "\8711", latex = "\\nabla", unicodemath = "\\nabla", cls = "U", category = "mathord", requirements = "", comments = "NABLA, del, hamilton operator"}
  , Record {point = "02208", uchar = "\8712", latex = "\\in", unicodemath = "\\in", cls = "R", category = "mathrel", requirements = "", comments = "set membership, variant"}
  , Record {point = "02209", uchar = "\8713", latex = "\\notin", unicodemath = "\\notin", cls = "R", category = "mathrel", requirements = "", comments = "= \\nin (wrisym), negated set membership"}
  , Record {point = "0220A", uchar = "\8714", latex = "", unicodemath = "\\smallin", cls = "R", category = "mathrel", requirements = "", comments = "set membership (small set membership)"}
  , Record {point = "0220B", uchar = "\8715", latex = "\\ni", unicodemath = "\\ni", cls = "R", category = "mathrel", requirements = "", comments = "= \\owns, contains, variant"}
  , Record {point = "0220C", uchar = "\8716", latex = "\\nni", unicodemath = "\\nni", cls = "R", category = "mathrel", requirements = "wrisym", comments = "= \\notni (txfonts), = \\notowner (mathabx), = \\notowns (fourier), negated contains, variant"}
  , Record {point = "0220D", uchar = "\8717", latex = "", unicodemath = "\\smallni", cls = "R", category = "mathrel", requirements = "", comments = "r: contains (SMALL CONTAINS AS MEMBER)"}
  , Record {point = "0220E", uchar = "\8718", latex = "", unicodemath = "\\QED", cls = "N", category = "mathord", requirements = "", comments = "# \\blacksquare (amssymb), END OF PROOF"}
  , Record {point = "0220F", uchar = "\8719", latex = "\\prod", unicodemath = "\\prod", cls = "L", category = "mathop", requirements = "", comments = "product operator"}
  , Record {point = "02210", uchar = "\8720", latex = "\\coprod", unicodemath = "\\coprod", cls = "L", category = "mathop", requirements = "", comments = "coproduct operator"}
  , Record {point = "02211", uchar = "\8721", latex = "\\sum", unicodemath = "\\sum", cls = "L", category = "mathop", requirements = "", comments = "summation operator"}
  , Record {point = "02212", uchar = "\8722", latex = "-", unicodemath = "\\minus", cls = "V", category = "mathbin", requirements = "", comments = "MINUS SIGN"}
  , Record {point = "02213", uchar = "\8723", latex = "\\mp", unicodemath = "\\mp", cls = "V", category = "mathbin", requirements = "", comments = "MINUS-OR-PLUS SIGN"}
  , Record {point = "02214", uchar = "\8724", latex = "\\dotplus", unicodemath = "\\dotplus", cls = "B", category = "mathbin", requirements = "amssymb", comments = "plus sign, dot above"}
  , Record {point = "02215", uchar = "\8725", latex = "\\slash", unicodemath = "\\divslash", cls = "B", category = "mathbin", requirements = "", comments = "DIVISION SLASH"}
  , Record {point = "02216", uchar = "\8726", latex = "\\smallsetminus", unicodemath = "\\smallsetminus", cls = "B", category = "mathbin", requirements = "amssymb fourier", comments = "small SET MINUS (cf. reverse solidus)"}
  , Record {point = "02217", uchar = "\8727", latex = "\\ast", unicodemath = "\\ast", cls = "B", category = "mathbin", requirements = "", comments = "ASTERISK OPERATOR (Hodge star operator)"}
  , Record {point = "02218", uchar = "\8728", latex = "\\circ", unicodemath = "\\vysmwhtcircle", cls = "B", category = "mathbin", requirements = "", comments = "composite function (small circle)"}
  , Record {point = "02219", uchar = "\8729", latex = "\\bullet", unicodemath = "\\vysmblkcircle", cls = "B", category = "mathbin", requirements = "", comments = "BULLET OPERATOR"}
  , Record {point = "0221A", uchar = "\8730", latex = "\\sqrt", unicodemath = "\\sqrt", cls = "L", category = "mathradical", requirements = "", comments = "radical"}
  , Record {point = "0221B", uchar = "\8731", latex = "\\sqrt[3]", unicodemath = "\\cuberoot", cls = "L", category = "mathradical", requirements = "", comments = "CUBE ROOT"}
  , Record {point = "0221C", uchar = "\8732", latex = "\\sqrt[4]", unicodemath = "\\fourthroot", cls = "L", category = "mathradical", requirements = "", comments = "FOURTH ROOT"}
  , Record {point = "0221D", uchar = "\8733", latex = "\\propto", unicodemath = "\\propto", cls = "R", category = "mathrel", requirements = "", comments = "# \\varpropto (amssymb), is PROPORTIONAL TO"}
  , Record {point = "0221E", uchar = "\8734", latex = "\\infty", unicodemath = "\\infty", cls = "N", category = "mathord", requirements = "", comments = "INFINITY"}
  , Record {point = "0221F", uchar = "\8735", latex = "\\rightangle", unicodemath = "\\rightangle", cls = "N", category = "mathord", requirements = "wrisym", comments = "right (90 degree) angle"}
  , Record {point = "02220", uchar = "\8736", latex = "\\angle", unicodemath = "\\angle", cls = "N", category = "mathord", requirements = "", comments = "ANGLE"}
  , Record {point = "02221", uchar = "\8737", latex = "\\measuredangle", unicodemath = "\\measuredangle", cls = "N", category = "mathord", requirements = "amssymb wrisym", comments = "MEASURED ANGLE"}
  , Record {point = "02222", uchar = "\8738", latex = "\\sphericalangle", unicodemath = "\\sphericalangle", cls = "N", category = "mathord", requirements = "amssymb wrisym", comments = "SPHERICAL ANGLE"}
  , Record {point = "02223", uchar = "\8739", latex = "\\mid", unicodemath = "\\mid", cls = "R", category = "mathrel", requirements = "", comments = "r: DIVIDES"}
  , Record {point = "02224", uchar = "\8740", latex = "\\nmid", unicodemath = "\\nmid", cls = "R", category = "mathrel", requirements = "amssymb", comments = "negated mid, DOES NOT DIVIDE"}
  , Record {point = "02225", uchar = "\8741", latex = "\\parallel", unicodemath = "\\parallel", cls = "R", category = "mathrel", requirements = "", comments = "parallel"}
  , Record {point = "02226", uchar = "\8742", latex = "\\nparallel", unicodemath = "\\nparallel", cls = "R", category = "mathrel", requirements = "amssymb fourier", comments = "not parallel"}
  , Record {point = "02227", uchar = "\8743", latex = "\\wedge", unicodemath = "\\wedge", cls = "B", category = "mathbin", requirements = "amssymb", comments = "= \\land, b: LOGICAL AND"}
  , Record {point = "02228", uchar = "\8744", latex = "\\vee", unicodemath = "\\vee", cls = "B", category = "mathbin", requirements = "", comments = "= \\lor, b: LOGICAL OR"}
  , Record {point = "02229", uchar = "\8745", latex = "\\cap", unicodemath = "\\cap", cls = "B", category = "mathbin", requirements = "", comments = "INTERSECTION"}
  , Record {point = "0222A", uchar = "\8746", latex = "\\cup", unicodemath = "\\cup", cls = "B", category = "mathbin", requirements = "", comments = "UNION or logical sum"}
  , Record {point = "0222B", uchar = "\8747", latex = "\\int", unicodemath = "\\int", cls = "L", category = "mathop", requirements = "", comments = "INTEGRAL operator"}
  , Record {point = "0222C", uchar = "\8748", latex = "\\iint", unicodemath = "\\iint", cls = "L", category = "mathop", requirements = "amsmath fourier esint wasysym", comments = "DOUBLE INTEGRAL operator"}
  , Record {point = "0222D", uchar = "\8749", latex = "\\iiint", unicodemath = "\\iiint", cls = "L", category = "mathop", requirements = "amsmath fourier esint wasysym", comments = "TRIPLE INTEGRAL operator"}
  , Record {point = "0222E", uchar = "\8750", latex = "\\oint", unicodemath = "\\oint", cls = "L", category = "mathop", requirements = "", comments = "CONTOUR INTEGRAL operator"}
  , Record {point = "0222F", uchar = "\8751", latex = "\\oiint", unicodemath = "\\oiint", cls = "L", category = "mathop", requirements = "esint wasysym fourier", comments = "= \\dbloint (wrisym), double contour integral operator"}
  , Record {point = "02230", uchar = "\8752", latex = "\\oiiint", unicodemath = "\\oiiint", cls = "L", category = "mathop", requirements = "txfonts fourier", comments = "triple contour integral operator"}
  , Record {point = "02231", uchar = "\8753", latex = "", unicodemath = "\\intclockwise", cls = "L", category = "mathop", requirements = "", comments = "CLOCKWISE INTEGRAL"}
  , Record {point = "02232", uchar = "\8754", latex = "\\varointclockwise", unicodemath = "\\varointclockwise", cls = "L", category = "mathop", requirements = "esint", comments = "= \\clockoint (wrisym), contour integral, clockwise"}
  , Record {point = "02233", uchar = "\8755", latex = "\\ointctrclockwise", unicodemath = "\\ointctrclockwise", cls = "L", category = "mathop", requirements = "esint", comments = "= \\cntclockoint (wrisym), contour integral, anticlockwise"}
  , Record {point = "02234", uchar = "\8756", latex = "\\therefore", unicodemath = "\\therefore", cls = "R", category = "mathord", requirements = "amssymb wrisym", comments = "= \\wasytherefore (wasysym), THEREFORE"}
  , Record {point = "02235", uchar = "\8757", latex = "\\because", unicodemath = "\\because", cls = "R", category = "mathord", requirements = "amssymb wrisym", comments = "BECAUSE"}
  , Record {point = "02236", uchar = "\8758", latex = ":", unicodemath = "\\mathratio", cls = "R", category = "mathrel", requirements = "", comments = "x \\colon, RATIO"}
  , Record {point = "02237", uchar = "\8759", latex = "\\Proportion", unicodemath = "\\Colon", cls = "R", category = "mathrel", requirements = "wrisym", comments = "# ::, two colons"}
  , Record {point = "02238", uchar = "\8760", latex = "", unicodemath = "\\dotminus", cls = "B", category = "mathbin", requirements = "", comments = "minus sign, dot above"}
  , Record {point = "02239", uchar = "\8761", latex = "\\eqcolon", unicodemath = "\\dashcolon", cls = "R", category = "mathrel", requirements = "txfonts -mathabx", comments = "# -: ,EXCESS"}
  , Record {point = "0223A", uchar = "\8762", latex = "", unicodemath = "\\dotsminusdots", cls = "R", category = "mathrel", requirements = "", comments = "minus with four dots, GEOMETRIC PROPORTION"}
  , Record {point = "0223B", uchar = "\8763", latex = "", unicodemath = "\\kernelcontraction", cls = "R", category = "mathrel", requirements = "", comments = "HOMOTHETIC"}
  , Record {point = "0223C", uchar = "\8764", latex = "\\sim", unicodemath = "\\sim", cls = "R", category = "mathrel", requirements = "", comments = "similar to, TILDE OPERATOR"}
  , Record {point = "0223D", uchar = "\8765", latex = "\\backsim", unicodemath = "\\backsim", cls = "R", category = "mathrel", requirements = "amssymb", comments = "reverse similar"}
  , Record {point = "0223E", uchar = "\8766", latex = "", unicodemath = "\\invlazys", cls = "B", category = "mathbin", requirements = "", comments = "most positive, INVERTED LAZY S"}
  , Record {point = "0223F", uchar = "\8767", latex = "\\AC", unicodemath = "\\sinewave", cls = "N", category = "mathord", requirements = "wasysym", comments = "SINE WAVE, alternating current"}
  , Record {point = "02240", uchar = "\8768", latex = "\\wr", unicodemath = "\\wr", cls = "B", category = "mathbin", requirements = "amssymb", comments = "WREATH PRODUCT"}
  , Record {point = "02241", uchar = "\8769", latex = "\\nsim", unicodemath = "\\nsim", cls = "R", category = "mathrel", requirements = "amssymb wrisym", comments = "not similar"}
  , Record {point = "02242", uchar = "\8770", latex = "\\eqsim", unicodemath = "\\eqsim", cls = "R", category = "mathrel", requirements = "amssymb", comments = "equals, similar"}
  , Record {point = "02243", uchar = "\8771", latex = "\\simeq", unicodemath = "\\simeq", cls = "R", category = "mathrel", requirements = "", comments = "similar, equals"}
  , Record {point = "02244", uchar = "\8772", latex = "\\nsimeq", unicodemath = "\\nsime", cls = "R", category = "mathrel", requirements = "txfonts", comments = "not similar, equals"}
  , Record {point = "02245", uchar = "\8773", latex = "\\cong", unicodemath = "\\cong", cls = "R", category = "mathrel", requirements = "", comments = "congruent with"}
  , Record {point = "02246", uchar = "\8774", latex = "", unicodemath = "\\simneqq", cls = "R", category = "mathrel", requirements = "", comments = "similar, not equals [vert only for 9573 entity]"}
  , Record {point = "02247", uchar = "\8775", latex = "\\ncong", unicodemath = "\\ncong", cls = "R", category = "mathrel", requirements = "amssymb wrisym", comments = "not congruent with"}
  , Record {point = "02248", uchar = "\8776", latex = "\\approx", unicodemath = "\\approx", cls = "R", category = "mathrel", requirements = "", comments = "approximate"}
  , Record {point = "02249", uchar = "\8777", latex = "\\napprox", unicodemath = "\\napprox", cls = "R", category = "mathrel", requirements = "wrisym", comments = "not approximate"}
  , Record {point = "0224A", uchar = "\8778", latex = "\\approxeq", unicodemath = "\\approxeq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "approximate, equals"}
  , Record {point = "0224B", uchar = "\8779", latex = "", unicodemath = "\\approxident", cls = "R", category = "mathrel", requirements = "", comments = "approximately identical to"}
  , Record {point = "0224C", uchar = "\8780", latex = "", unicodemath = "\\backcong", cls = "R", category = "mathrel", requirements = "", comments = "ALL EQUAL TO"}
  , Record {point = "0224D", uchar = "\8781", latex = "\\asymp", unicodemath = "\\asymp", cls = "R", category = "mathrel", requirements = "", comments = "asymptotically equal to"}
  , Record {point = "0224E", uchar = "\8782", latex = "\\Bumpeq", unicodemath = "\\Bumpeq", cls = "R", category = "mathrel", requirements = "amssymb wrisym", comments = "bumpy equals"}
  , Record {point = "0224F", uchar = "\8783", latex = "\\bumpeq", unicodemath = "\\bumpeq", cls = "R", category = "mathrel", requirements = "amssymb wrisym", comments = "bumpy equals, equals"}
  , Record {point = "02250", uchar = "\8784", latex = "\\doteq", unicodemath = "\\doteq", cls = "R", category = "mathrel", requirements = "", comments = "= \\dotequal (wrisym), equals, single dot above"}
  , Record {point = "02251", uchar = "\8785", latex = "\\Doteq", unicodemath = "\\Doteq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\doteqdot (amssymb), /doteq r: equals, even dots"}
  , Record {point = "02252", uchar = "\8786", latex = "\\fallingdotseq", unicodemath = "\\fallingdotseq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "equals, falling dots"}
  , Record {point = "02253", uchar = "\8787", latex = "\\risingdotseq", unicodemath = "\\risingdotseq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "equals, rising dots"}
  , Record {point = "02254", uchar = "\8788", latex = "\\coloneq", unicodemath = "\\coloneq", cls = "R", category = "mathrel", requirements = "mathabx -txfonts", comments = "= \\coloneqq (txfonts), = \\SetDelayed (wrisym), # := colon, equals"}
  , Record {point = "02255", uchar = "\8789", latex = "\\eqcolon", unicodemath = "\\eqcolon", cls = "R", category = "mathrel", requirements = "mathabx -txfonts", comments = "= \\eqqcolon (txfonts), # =:, equals, colon"}
  , Record {point = "02256", uchar = "\8790", latex = "\\eqcirc", unicodemath = "\\eqcirc", cls = "R", category = "mathrel", requirements = "amssymb", comments = "circle on equals sign"}
  , Record {point = "02257", uchar = "\8791", latex = "\\circeq", unicodemath = "\\circeq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "circle, equals"}
  , Record {point = "02258", uchar = "\8792", latex = "", unicodemath = "\\arceq", cls = "R", category = "mathrel", requirements = "", comments = "arc, equals; CORRESPONDS TO"}
  , Record {point = "02259", uchar = "\8793", latex = "\\corresponds", unicodemath = "\\wedgeq", cls = "R", category = "mathrel", requirements = "mathabx", comments = "= \\sdef (oz), t \\Corresponds (marvosym), corresponds to (wedge over equals)"}
  , Record {point = "0225A", uchar = "\8794", latex = "", unicodemath = "\\veeeq", cls = "R", category = "mathrel", requirements = "", comments = "logical or, equals"}
  , Record {point = "0225B", uchar = "\8795", latex = "", unicodemath = "\\stareq", cls = "R", category = "mathrel", requirements = "", comments = "STAR EQUALS"}
  , Record {point = "0225C", uchar = "\8796", latex = "\\triangleq", unicodemath = "\\triangleq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\varsdef (oz), triangle, equals"}
  , Record {point = "0225D", uchar = "\8797", latex = "", unicodemath = "\\eqdef", cls = "R", category = "mathrel", requirements = "", comments = "equals by definition"}
  , Record {point = "0225E", uchar = "\8798", latex = "", unicodemath = "\\measeq", cls = "R", category = "mathrel", requirements = "", comments = "MEASURED BY (m over equals)"}
  , Record {point = "0225F", uchar = "\8799", latex = "", unicodemath = "\\questeq", cls = "R", category = "mathrel", requirements = "", comments = "equal with questionmark"}
  , Record {point = "02260", uchar = "\8800", latex = "\\neq", unicodemath = "\\ne", cls = "R", category = "mathrel", requirements = "", comments = "= \\ne, r: not equal"}
  , Record {point = "02261", uchar = "\8801", latex = "\\equiv", unicodemath = "\\equiv", cls = "R", category = "mathrel", requirements = "", comments = "identical with"}
  , Record {point = "02262", uchar = "\8802", latex = "\\nequiv", unicodemath = "\\nequiv", cls = "R", category = "mathrel", requirements = "wrisym", comments = "not identical with"}
  , Record {point = "02263", uchar = "\8803", latex = "", unicodemath = "\\Equiv", cls = "R", category = "mathrel", requirements = "", comments = "strict equivalence (4 lines)"}
  , Record {point = "02264", uchar = "\8804", latex = "\\leq", unicodemath = "\\leq", cls = "R", category = "mathrel", requirements = "", comments = "= \\le, r: less-than-or-equal"}
  , Record {point = "02265", uchar = "\8805", latex = "\\geq", unicodemath = "\\geq", cls = "R", category = "mathrel", requirements = "", comments = "= \\ge, r: greater-than-or-equal"}
  , Record {point = "02266", uchar = "\8806", latex = "\\leqq", unicodemath = "\\leqq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "less, double equals"}
  , Record {point = "02267", uchar = "\8807", latex = "\\geqq", unicodemath = "\\geqq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "greater, double equals"}
  , Record {point = "02268", uchar = "\8808", latex = "\\lneqq", unicodemath = "\\lneqq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "less, not double equals"}
  , Record {point = "02269", uchar = "\8809", latex = "\\gneqq", unicodemath = "\\gneqq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "greater, not double equals"}
  , Record {point = "0226A", uchar = "\8810", latex = "\\ll", unicodemath = "\\ll", cls = "R", category = "mathrel", requirements = "", comments = "much less than, type 2"}
  , Record {point = "0226B", uchar = "\8811", latex = "\\gg", unicodemath = "\\gg", cls = "R", category = "mathrel", requirements = "", comments = "much greater than, type 2"}
  , Record {point = "0226C", uchar = "\8812", latex = "\\between", unicodemath = "\\between", cls = "R", category = "mathrel", requirements = "amssymb", comments = "BETWEEN"}
  , Record {point = "0226D", uchar = "\8813", latex = "\\notasymp", unicodemath = "\\nasymp", cls = "R", category = "mathrel", requirements = "mathabx", comments = "= \\nasymp (wrisym), not asymptotically equal to"}
  , Record {point = "0226E", uchar = "\8814", latex = "\\nless", unicodemath = "\\nless", cls = "R", category = "mathrel", requirements = "amssymb", comments = "NOT LESS-THAN"}
  , Record {point = "0226F", uchar = "\8815", latex = "\\ngtr", unicodemath = "\\ngtr", cls = "R", category = "mathrel", requirements = "amssymb", comments = "NOT GREATER-THAN"}
  , Record {point = "02270", uchar = "\8816", latex = "\\nleq", unicodemath = "\\nleq", cls = "R", category = "mathrel", requirements = "amssymb wrisym", comments = "= \\nleqslant (fourier), not less-than-or-equal"}
  , Record {point = "02271", uchar = "\8817", latex = "\\ngeq", unicodemath = "\\ngeq", cls = "R", category = "mathrel", requirements = "amssymb wrisym", comments = "= \\ngeqslant (fourier), not greater-than-or-equal"}
  , Record {point = "02272", uchar = "\8818", latex = "\\lesssim", unicodemath = "\\lesssim", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\apprle (wasysym), = \\LessTilde (wrisym), less, similar"}
  , Record {point = "02273", uchar = "\8819", latex = "\\gtrsim", unicodemath = "\\gtrsim", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\apprge (wasysym), = \\GreaterTilde (wrisym), greater, similar"}
  , Record {point = "02274", uchar = "\8820", latex = "\\NotLessTilde", unicodemath = "\\nlesssim", cls = "R", category = "mathrel", requirements = "wrisym", comments = "not less, similar"}
  , Record {point = "02275", uchar = "\8821", latex = "\\NotGreaterTilde", unicodemath = "\\ngtrsim", cls = "R", category = "mathrel", requirements = "wrisym", comments = "not greater, similar"}
  , Record {point = "02276", uchar = "\8822", latex = "\\lessgtr", unicodemath = "\\lessgtr", cls = "R", category = "mathrel", requirements = "amssymb", comments = "less, greater"}
  , Record {point = "02277", uchar = "\8823", latex = "\\gtrless", unicodemath = "\\gtrless", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\GreaterLess (wrisym), greater, less"}
  , Record {point = "02278", uchar = "\8824", latex = "", unicodemath = "\\nlessgtr", cls = "R", category = "mathrel", requirements = "wrisym", comments = "not less, greater"}
  , Record {point = "02279", uchar = "\8825", latex = "\\NotGreaterLess", unicodemath = "\\ngtrless", cls = "R", category = "mathrel", requirements = "wrisym", comments = "not greater, less"}
  , Record {point = "0227A", uchar = "\8826", latex = "\\prec", unicodemath = "\\prec", cls = "R", category = "mathrel", requirements = "", comments = "PRECEDES"}
  , Record {point = "0227B", uchar = "\8827", latex = "\\succ", unicodemath = "\\succ", cls = "R", category = "mathrel", requirements = "", comments = "SUCCEEDS"}
  , Record {point = "0227C", uchar = "\8828", latex = "\\preccurlyeq", unicodemath = "\\preccurlyeq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\PrecedesSlantEqual (wrisym), precedes, curly equals"}
  , Record {point = "0227D", uchar = "\8829", latex = "\\succcurlyeq", unicodemath = "\\succcurlyeq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\SucceedsSlantEqual (wrisym), succeeds, curly equals"}
  , Record {point = "0227E", uchar = "\8830", latex = "\\precsim", unicodemath = "\\precsim", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\PrecedesTilde (wrisym), precedes, similar"}
  , Record {point = "0227F", uchar = "\8831", latex = "\\succsim", unicodemath = "\\succsim", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\SucceedsTilde (wrisym), succeeds, similar"}
  , Record {point = "02280", uchar = "\8832", latex = "\\nprec", unicodemath = "\\nprec", cls = "R", category = "mathrel", requirements = "amssymb wrisym", comments = "not precedes"}
  , Record {point = "02281", uchar = "\8833", latex = "\\nsucc", unicodemath = "\\nsucc", cls = "R", category = "mathrel", requirements = "amssymb wrisym", comments = "not succeeds"}
  , Record {point = "02282", uchar = "\8834", latex = "\\subset", unicodemath = "\\subset", cls = "R", category = "mathrel", requirements = "", comments = "subset or is implied by"}
  , Record {point = "02283", uchar = "\8835", latex = "\\supset", unicodemath = "\\supset", cls = "R", category = "mathrel", requirements = "", comments = "superset or implies"}
  , Record {point = "02284", uchar = "\8836", latex = "\\nsubset", unicodemath = "\\nsubset", cls = "R", category = "mathrel", requirements = "wrisym", comments = "not subset, variant [slash negation]"}
  , Record {point = "02285", uchar = "\8837", latex = "\\nsupset", unicodemath = "\\nsupset", cls = "R", category = "mathrel", requirements = "wrisym", comments = "not superset, variant [slash negation]"}
  , Record {point = "02286", uchar = "\8838", latex = "\\subseteq", unicodemath = "\\subseteq", cls = "R", category = "mathrel", requirements = "", comments = "subset, equals"}
  , Record {point = "02287", uchar = "\8839", latex = "\\supseteq", unicodemath = "\\supseteq", cls = "R", category = "mathrel", requirements = "", comments = "superset, equals"}
  , Record {point = "02288", uchar = "\8840", latex = "\\nsubseteq", unicodemath = "\\nsubseteq", cls = "R", category = "mathrel", requirements = "amssymb wrisym", comments = "not subset, equals"}
  , Record {point = "02289", uchar = "\8841", latex = "\\nsupseteq", unicodemath = "\\nsupseteq", cls = "R", category = "mathrel", requirements = "amssymb wrisym", comments = "not superset, equals"}
  , Record {point = "0228A", uchar = "\8842", latex = "\\subsetneq", unicodemath = "\\subsetneq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\varsubsetneq (fourier), subset, not equals"}
  , Record {point = "0228B", uchar = "\8843", latex = "\\supsetneq", unicodemath = "\\supsetneq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "superset, not equals"}
  , Record {point = "0228C", uchar = "\8844", latex = "", unicodemath = "\\cupleftarrow", cls = "B", category = "mathbin", requirements = "", comments = "MULTISET"}
  , Record {point = "0228D", uchar = "\8845", latex = "", unicodemath = "\\cupdot", cls = "B", category = "mathbin", requirements = "", comments = "union, with dot"}
  , Record {point = "0228E", uchar = "\8846", latex = "\\uplus", unicodemath = "\\uplus", cls = "B", category = "mathbin", requirements = "", comments = "= \\buni (oz), plus sign in union"}
  , Record {point = "0228F", uchar = "\8847", latex = "\\sqsubset", unicodemath = "\\sqsubset", cls = "R", category = "mathrel", requirements = "amsfonts", comments = "square subset"}
  , Record {point = "02290", uchar = "\8848", latex = "\\sqsupset", unicodemath = "\\sqsupset", cls = "R", category = "mathrel", requirements = "amsfonts", comments = "square superset"}
  , Record {point = "02291", uchar = "\8849", latex = "\\sqsubseteq", unicodemath = "\\sqsubseteq", cls = "R", category = "mathrel", requirements = "", comments = "square subset, equals"}
  , Record {point = "02292", uchar = "\8850", latex = "\\sqsupseteq", unicodemath = "\\sqsupseteq", cls = "R", category = "mathrel", requirements = "", comments = "square superset, equals"}
  , Record {point = "02293", uchar = "\8851", latex = "\\sqcap", unicodemath = "\\sqcap", cls = "B", category = "mathbin", requirements = "", comments = "square intersection"}
  , Record {point = "02294", uchar = "\8852", latex = "\\sqcup", unicodemath = "\\sqcup", cls = "B", category = "mathbin", requirements = "", comments = "square union"}
  , Record {point = "02295", uchar = "\8853", latex = "\\oplus", unicodemath = "\\oplus", cls = "B", category = "mathbin", requirements = "", comments = "plus sign in circle"}
  , Record {point = "02296", uchar = "\8854", latex = "\\ominus", unicodemath = "\\ominus", cls = "B", category = "mathbin", requirements = "", comments = "minus sign in circle"}
  , Record {point = "02297", uchar = "\8855", latex = "\\otimes", unicodemath = "\\otimes", cls = "B", category = "mathbin", requirements = "", comments = "multiply sign in circle"}
  , Record {point = "02298", uchar = "\8856", latex = "\\oslash", unicodemath = "\\oslash", cls = "B", category = "mathbin", requirements = "", comments = "solidus in circle"}
  , Record {point = "02299", uchar = "\8857", latex = "\\odot", unicodemath = "\\odot", cls = "B", category = "mathbin", requirements = "", comments = "middle dot in circle"}
  , Record {point = "0229A", uchar = "\8858", latex = "\\circledcirc", unicodemath = "\\circledcirc", cls = "B", category = "mathbin", requirements = "amssymb", comments = "small circle in circle"}
  , Record {point = "0229B", uchar = "\8859", latex = "\\circledast", unicodemath = "\\circledast", cls = "B", category = "mathbin", requirements = "amssymb", comments = "asterisk in circle"}
  , Record {point = "0229C", uchar = "\8860", latex = "", unicodemath = "\\circledequal", cls = "B", category = "mathbin", requirements = "", comments = "equal in circle"}
  , Record {point = "0229D", uchar = "\8861", latex = "\\circleddash", unicodemath = "\\circleddash", cls = "B", category = "mathbin", requirements = "amssymb", comments = "hyphen in circle"}
  , Record {point = "0229E", uchar = "\8862", latex = "\\boxplus", unicodemath = "\\boxplus", cls = "B", category = "mathbin", requirements = "amssymb", comments = "plus sign in box"}
  , Record {point = "0229F", uchar = "\8863", latex = "\\boxminus", unicodemath = "\\boxminus", cls = "B", category = "mathbin", requirements = "amssymb", comments = "minus sign in box"}
  , Record {point = "022A0", uchar = "\8864", latex = "\\boxtimes", unicodemath = "\\boxtimes", cls = "B", category = "mathbin", requirements = "amssymb", comments = "multiply sign in box"}
  , Record {point = "022A1", uchar = "\8865", latex = "\\boxdot", unicodemath = "\\boxdot", cls = "B", category = "mathbin", requirements = "amssymb stmaryrd", comments = "/dotsquare /boxdot b: small dot in box"}
  , Record {point = "022A2", uchar = "\8866", latex = "\\vdash", unicodemath = "\\vdash", cls = "R", category = "mathrel", requirements = "", comments = "RIGHT TACK, proves, implies, yields, (vertical, dash)"}
  , Record {point = "022A3", uchar = "\8867", latex = "\\dashv", unicodemath = "\\dashv", cls = "R", category = "mathrel", requirements = "amssymb", comments = "LEFT TACK, non-theorem, does not yield, (dash, vertical)"}
  , Record {point = "022A4", uchar = "\8868", latex = "\\top", unicodemath = "\\top", cls = "N", category = "mathord", requirements = "", comments = "DOWN TACK, top"}
  , Record {point = "022A5", uchar = "\8869", latex = "\\bot", unicodemath = "\\bot", cls = "R", category = "mathord", requirements = "", comments = "UP TACK, bottom"}
  , Record {point = "022A6", uchar = "\8870", latex = "", unicodemath = "\\assert", cls = "R", category = "mathrel", requirements = "", comments = "# \\vdash, ASSERTION (vertical, short dash)"}
  , Record {point = "022A7", uchar = "\8871", latex = "\\models", unicodemath = "\\models", cls = "R", category = "mathrel", requirements = "", comments = "MODELS (vertical, short double dash)"}
  , Record {point = "022A8", uchar = "\8872", latex = "\\vDash", unicodemath = "\\vDash", cls = "R", category = "mathrel", requirements = "amssymb fourier", comments = "TRUE (vertical, double dash)"}
  , Record {point = "022A9", uchar = "\8873", latex = "\\Vdash", unicodemath = "\\Vdash", cls = "R", category = "mathrel", requirements = "amssymb", comments = "double vertical, dash"}
  , Record {point = "022AA", uchar = "\8874", latex = "\\Vvdash", unicodemath = "\\Vvdash", cls = "R", category = "mathrel", requirements = "amssymb", comments = "triple vertical, dash"}
  , Record {point = "022AB", uchar = "\8875", latex = "\\VDash", unicodemath = "\\VDash", cls = "R", category = "mathrel", requirements = "mathabx txfonts", comments = "double vert, double dash"}
  , Record {point = "022AC", uchar = "\8876", latex = "\\nvdash", unicodemath = "\\nvdash", cls = "R", category = "mathrel", requirements = "amssymb", comments = "not vertical, dash"}
  , Record {point = "022AD", uchar = "\8877", latex = "\\nvDash", unicodemath = "\\nvDash", cls = "R", category = "mathrel", requirements = "amssymb fourier", comments = "not vertical, double dash"}
  , Record {point = "022AE", uchar = "\8878", latex = "\\nVdash", unicodemath = "\\nVdash", cls = "R", category = "mathrel", requirements = "amssymb", comments = "not double vertical, dash"}
  , Record {point = "022AF", uchar = "\8879", latex = "\\nVDash", unicodemath = "\\nVDash", cls = "R", category = "mathrel", requirements = "amssymb", comments = "not double vert, double dash"}
  , Record {point = "022B0", uchar = "\8880", latex = "", unicodemath = "\\prurel", cls = "R", category = "mathrel", requirements = "", comments = "element PRECEDES UNDER RELATION"}
  , Record {point = "022B1", uchar = "\8881", latex = "", unicodemath = "\\scurel", cls = "R", category = "mathrel", requirements = "", comments = "SUCCEEDS UNDER RELATION"}
  , Record {point = "022B2", uchar = "\8882", latex = "\\vartriangleleft", unicodemath = "\\vartriangleleft", cls = "R", category = "mathrel", requirements = "amssymb", comments = "left triangle, open, variant"}
  , Record {point = "022B3", uchar = "\8883", latex = "\\vartriangleright", unicodemath = "\\vartriangleright", cls = "R", category = "mathrel", requirements = "amssymb", comments = "right triangle, open, variant"}
  , Record {point = "022B4", uchar = "\8884", latex = "\\trianglelefteq", unicodemath = "\\trianglelefteq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\unlhd (wrisym), left triangle, equals"}
  , Record {point = "022B5", uchar = "\8885", latex = "\\trianglerighteq", unicodemath = "\\trianglerighteq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\unrhd (wrisym), right triangle, equals"}
  , Record {point = "022B6", uchar = "\8886", latex = "\\multimapdotbothA", unicodemath = "\\origof", cls = "R", category = "mathrel", requirements = "txfonts", comments = "ORIGINAL OF"}
  , Record {point = "022B7", uchar = "\8887", latex = "\\multimapdotbothB", unicodemath = "\\imageof", cls = "R", category = "mathrel", requirements = "txfonts", comments = "IMAGE OF"}
  , Record {point = "022B8", uchar = "\8888", latex = "\\multimap", unicodemath = "\\multimap", cls = "R", category = "mathrel", requirements = "amssymb", comments = "/MULTIMAP a:"}
  , Record {point = "022B9", uchar = "\8889", latex = "", unicodemath = "\\hermitmatrix", cls = "B", category = "mathord", requirements = "", comments = "HERMITIAN CONJUGATE MATRIX"}
  , Record {point = "022BA", uchar = "\8890", latex = "\\intercal", unicodemath = "\\intercal", cls = "B", category = "mathbin", requirements = "amssymb fourier", comments = "intercal"}
  , Record {point = "022BB", uchar = "\8891", latex = "\\veebar", unicodemath = "\\veebar", cls = "B", category = "mathbin", requirements = "amssymb", comments = "logical or, bar below (large vee); exclusive disjunction"}
  , Record {point = "022BC", uchar = "\8892", latex = "\\barwedge", unicodemath = "\\barwedge", cls = "B", category = "mathbin", requirements = "amssymb", comments = "logical NAND (bar over wedge)"}
  , Record {point = "022BD", uchar = "\8893", latex = "", unicodemath = "\\barvee", cls = "B", category = "mathbin", requirements = "", comments = "bar, vee (large vee)"}
  , Record {point = "022BE", uchar = "\8894", latex = "", unicodemath = "\\measuredrightangle", cls = "N", category = "mathord", requirements = "", comments = "right angle-measured [with arc]"}
  , Record {point = "022BF", uchar = "\8895", latex = "", unicodemath = "\\varlrtriangle", cls = "N", category = "mathord", requirements = "", comments = "RIGHT TRIANGLE"}
  , Record {point = "022C0", uchar = "\8896", latex = "\\bigwedge", unicodemath = "\\bigwedge", cls = "L", category = "mathop", requirements = "", comments = "logical or operator"}
  , Record {point = "022C1", uchar = "\8897", latex = "\\bigvee", unicodemath = "\\bigvee", cls = "L", category = "mathop", requirements = "", comments = "logical and operator"}
  , Record {point = "022C2", uchar = "\8898", latex = "\\bigcap", unicodemath = "\\bigcap", cls = "L", category = "mathop", requirements = "", comments = "= \\dint (oz), \\dinter (oz), intersection operator"}
  , Record {point = "022C3", uchar = "\8899", latex = "\\bigcup", unicodemath = "\\bigcup", cls = "L", category = "mathop", requirements = "", comments = "= \\duni (oz), \\dunion (oz), union operator"}
  , Record {point = "022C4", uchar = "\8900", latex = "\\diamond", unicodemath = "\\smwhtdiamond", cls = "B", category = "mathbin", requirements = "", comments = "DIAMOND OPERATOR (white diamond)"}
  , Record {point = "022C5", uchar = "\8901", latex = "\\cdot", unicodemath = "\\cdot", cls = "B", category = "mathbin", requirements = "", comments = "DOT OPERATOR (small middle dot)"}
  , Record {point = "022C6", uchar = "\8902", latex = "\\star", unicodemath = "\\star", cls = "B", category = "mathbin", requirements = "", comments = "small star, filled, low"}
  , Record {point = "022C7", uchar = "\8903", latex = "\\divideontimes", unicodemath = "\\divideontimes", cls = "B", category = "mathbin", requirements = "amssymb", comments = "division on times"}
  , Record {point = "022C8", uchar = "\8904", latex = "\\bowtie", unicodemath = "\\bowtie", cls = "R", category = "mathrel", requirements = "", comments = "= \\lrtimes (txfonts), BOWTIE"}
  , Record {point = "022C9", uchar = "\8905", latex = "\\ltimes", unicodemath = "\\ltimes", cls = "B", category = "mathbin", requirements = "amssymb", comments = "times sign, left closed"}
  , Record {point = "022CA", uchar = "\8906", latex = "\\rtimes", unicodemath = "\\rtimes", cls = "B", category = "mathbin", requirements = "amssymb", comments = "times sign, right closed"}
  , Record {point = "022CB", uchar = "\8907", latex = "\\leftthreetimes", unicodemath = "\\leftthreetimes", cls = "B", category = "mathbin", requirements = "amssymb", comments = "LEFT SEMIDIRECT PRODUCT"}
  , Record {point = "022CC", uchar = "\8908", latex = "\\rightthreetimes", unicodemath = "\\rightthreetimes", cls = "B", category = "mathbin", requirements = "amssymb", comments = "RIGHT SEMIDIRECT PRODUCT"}
  , Record {point = "022CD", uchar = "\8909", latex = "\\backsimeq", unicodemath = "\\backsimeq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "reverse similar, equals"}
  , Record {point = "022CE", uchar = "\8910", latex = "\\curlyvee", unicodemath = "\\curlyvee", cls = "B", category = "mathbin", requirements = "amssymb", comments = "CURLY LOGICAL OR"}
  , Record {point = "022CF", uchar = "\8911", latex = "\\curlywedge", unicodemath = "\\curlywedge", cls = "B", category = "mathbin", requirements = "amssymb", comments = "CURLY LOGICAL AND"}
  , Record {point = "022D0", uchar = "\8912", latex = "\\Subset", unicodemath = "\\Subset", cls = "R", category = "mathrel", requirements = "amssymb", comments = "DOUBLE SUBSET"}
  , Record {point = "022D1", uchar = "\8913", latex = "\\Supset", unicodemath = "\\Supset", cls = "R", category = "mathrel", requirements = "amssymb", comments = "DOUBLE SUPERSET"}
  , Record {point = "022D2", uchar = "\8914", latex = "\\Cap", unicodemath = "\\Cap", cls = "B", category = "mathbin", requirements = "amssymb", comments = "/cap /doublecap b: DOUBLE INTERSECTION"}
  , Record {point = "022D3", uchar = "\8915", latex = "\\Cup", unicodemath = "\\Cup", cls = "B", category = "mathbin", requirements = "amssymb", comments = "/cup /doublecup b: DOUBLE UNION"}
  , Record {point = "022D4", uchar = "\8916", latex = "\\pitchfork", unicodemath = "\\pitchfork", cls = "R", category = "mathrel", requirements = "amssymb", comments = "PITCHFORK"}
  , Record {point = "022D5", uchar = "\8917", latex = "\\hash", unicodemath = "\\equalparallel", cls = "R", category = "mathrel", requirements = "mathabx", comments = "parallel, equal; equal or parallel"}
  , Record {point = "022D6", uchar = "\8918", latex = "\\lessdot", unicodemath = "\\lessdot", cls = "R", category = "mathrel", requirements = "amssymb", comments = "less than, with dot"}
  , Record {point = "022D7", uchar = "\8919", latex = "\\gtrdot", unicodemath = "\\gtrdot", cls = "R", category = "mathrel", requirements = "amssymb", comments = "greater than, with dot"}
  , Record {point = "022D8", uchar = "\8920", latex = "\\lll", unicodemath = "\\lll", cls = "R", category = "mathrel", requirements = "amssymb -mathabx", comments = "triple less-than"}
  , Record {point = "022D9", uchar = "\8921", latex = "\\ggg", unicodemath = "\\ggg", cls = "R", category = "mathrel", requirements = "amssymb -mathabx", comments = "triple greater-than"}
  , Record {point = "022DA", uchar = "\8922", latex = "\\lesseqgtr", unicodemath = "\\lesseqgtr", cls = "R", category = "mathrel", requirements = "amssymb", comments = "less, equals, greater"}
  , Record {point = "022DB", uchar = "\8923", latex = "\\gtreqless", unicodemath = "\\gtreqless", cls = "R", category = "mathrel", requirements = "amssymb", comments = "greater, equals, less"}
  , Record {point = "022DC", uchar = "\8924", latex = "", unicodemath = "\\eqless", cls = "R", category = "mathrel", requirements = "", comments = "equal-or-less"}
  , Record {point = "022DD", uchar = "\8925", latex = "", unicodemath = "\\eqgtr", cls = "R", category = "mathrel", requirements = "", comments = "equal-or-greater"}
  , Record {point = "022DE", uchar = "\8926", latex = "\\curlyeqprec", unicodemath = "\\curlyeqprec", cls = "R", category = "mathrel", requirements = "amssymb", comments = "curly equals, precedes"}
  , Record {point = "022DF", uchar = "\8927", latex = "\\curlyeqsucc", unicodemath = "\\curlyeqsucc", cls = "R", category = "mathrel", requirements = "amssymb", comments = "curly equals, succeeds"}
  , Record {point = "022E0", uchar = "\8928", latex = "\\npreceq", unicodemath = "\\npreccurlyeq", cls = "R", category = "mathrel", requirements = "amssymb wrisym", comments = "DOES NOT PRECEDE OR EQUAL"}
  , Record {point = "022E1", uchar = "\8929", latex = "\\nsucceq", unicodemath = "\\nsucccurlyeq", cls = "R", category = "mathrel", requirements = "amssymb wrisym", comments = "not succeeds, curly equals"}
  , Record {point = "022E2", uchar = "\8930", latex = "\\nsqsubseteq", unicodemath = "\\nsqsubseteq", cls = "R", category = "mathrel", requirements = "wrisym", comments = "not, square subset, equals"}
  , Record {point = "022E3", uchar = "\8931", latex = "\\nsqsupseteq", unicodemath = "\\nsqsupseteq", cls = "R", category = "mathrel", requirements = "wrisym", comments = "not, square superset, equals"}
  , Record {point = "022E4", uchar = "\8932", latex = "", unicodemath = "\\sqsubsetneq", cls = "R", category = "mathrel", requirements = "", comments = "square subset, not equals"}
  , Record {point = "022E5", uchar = "\8933", latex = "", unicodemath = "\\sqsupsetneq", cls = "R", category = "mathrel", requirements = "", comments = "square superset, not equals"}
  , Record {point = "022E6", uchar = "\8934", latex = "\\lnsim", unicodemath = "\\lnsim", cls = "R", category = "mathrel", requirements = "amssymb", comments = "less, not similar"}
  , Record {point = "022E7", uchar = "\8935", latex = "\\gnsim", unicodemath = "\\gnsim", cls = "R", category = "mathrel", requirements = "amssymb", comments = "greater, not similar"}
  , Record {point = "022E8", uchar = "\8936", latex = "\\precnsim", unicodemath = "\\precnsim", cls = "R", category = "mathrel", requirements = "amssymb", comments = "precedes, not similar"}
  , Record {point = "022E9", uchar = "\8937", latex = "\\succnsim", unicodemath = "\\succnsim", cls = "R", category = "mathrel", requirements = "amssymb", comments = "succeeds, not similar"}
  , Record {point = "022EA", uchar = "\8938", latex = "\\ntriangleleft", unicodemath = "\\ntriangleleft", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\NotLeftTriangle (wrisym), not left triangle"}
  , Record {point = "022EB", uchar = "\8939", latex = "\\ntriangleright", unicodemath = "\\ntriangleright", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\NotRightTriangle (wrisym), not right triangle"}
  , Record {point = "022EC", uchar = "\8940", latex = "\\ntrianglelefteq", unicodemath = "\\ntrianglelefteq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\nunlhd (wrisym), not left triangle, equals"}
  , Record {point = "022ED", uchar = "\8941", latex = "\\ntrianglerighteq", unicodemath = "\\ntrianglerighteq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "= \\nunrhd (wrisym), not right triangle, equals"}
  , Record {point = "022EE", uchar = "\8942", latex = "\\vdots", unicodemath = "\\vdots", cls = "R", category = "mathrel", requirements = "", comments = "VERTICAL ELLIPSIS"}
  , Record {point = "022EF", uchar = "\8943", latex = "\\cdots", unicodemath = "\\unicodecdots", cls = "R", category = "mathord", requirements = "", comments = "three dots, centered"}
  , Record {point = "022F0", uchar = "\8944", latex = "\\iddots", unicodemath = "\\adots", cls = "R", category = "mathrel", requirements = "mathdots", comments = "= \\adots (yhmath), three dots, ascending"}
  , Record {point = "022F1", uchar = "\8945", latex = "\\ddots", unicodemath = "\\ddots", cls = "R", category = "mathrel", requirements = "", comments = "three dots, descending"}
  , Record {point = "022F2", uchar = "\8946", latex = "", unicodemath = "\\disin", cls = "R", category = "mathrel", requirements = "", comments = "ELEMENT OF WITH LONG HORIZONTAL STROKE"}
  , Record {point = "022F3", uchar = "\8947", latex = "", unicodemath = "\\varisins", cls = "R", category = "mathrel", requirements = "", comments = "ELEMENT OF WITH VERTICAL BAR AT END OF HORIZONTAL STROKE"}
  , Record {point = "022F4", uchar = "\8948", latex = "", unicodemath = "\\isins", cls = "R", category = "mathrel", requirements = "", comments = "SMALL ELEMENT OF WITH VERTICAL BAR AT END OF HORIZONTAL STROKE"}
  , Record {point = "022F5", uchar = "\8949", latex = "", unicodemath = "\\isindot", cls = "R", category = "mathrel", requirements = "", comments = "ELEMENT OF WITH DOT ABOVE"}
  , Record {point = "022F6", uchar = "\8950", latex = "\\barin", unicodemath = "\\varisinobar", cls = "R", category = "mathrel", requirements = "mathabx", comments = "ELEMENT OF WITH OVERBAR"}
  , Record {point = "022F7", uchar = "\8951", latex = "", unicodemath = "\\isinobar", cls = "R", category = "mathrel", requirements = "", comments = "SMALL ELEMENT OF WITH OVERBAR"}
  , Record {point = "022F8", uchar = "\8952", latex = "", unicodemath = "\\isinvb", cls = "R", category = "mathrel", requirements = "", comments = "ELEMENT OF WITH UNDERBAR"}
  , Record {point = "022F9", uchar = "\8953", latex = "", unicodemath = "\\isinE", cls = "R", category = "mathrel", requirements = "", comments = "ELEMENT OF WITH TWO HORIZONTAL STROKES"}
  , Record {point = "022FA", uchar = "\8954", latex = "", unicodemath = "\\nisd", cls = "R", category = "mathrel", requirements = "", comments = "CONTAINS WITH LONG HORIZONTAL STROKE"}
  , Record {point = "022FB", uchar = "\8955", latex = "", unicodemath = "\\varnis", cls = "R", category = "mathrel", requirements = "", comments = "CONTAINS WITH VERTICAL BAR AT END OF HORIZONTAL STROKE"}
  , Record {point = "022FC", uchar = "\8956", latex = "", unicodemath = "\\nis", cls = "R", category = "mathrel", requirements = "", comments = "SMALL CONTAINS WITH VERTICAL BAR AT END OF HORIZONTAL STROKE"}
  , Record {point = "022FD", uchar = "\8957", latex = "", unicodemath = "\\varniobar", cls = "R", category = "mathrel", requirements = "", comments = "CONTAINS WITH OVERBAR"}
  , Record {point = "022FE", uchar = "\8958", latex = "", unicodemath = "\\niobar", cls = "R", category = "mathrel", requirements = "", comments = "SMALL CONTAINS WITH OVERBAR"}
  , Record {point = "022FF", uchar = "\8959", latex = "", unicodemath = "\\bagmember", cls = "R", category = "mathrel", requirements = "", comments = "# \\mathsf{E}, Z NOTATION BAG MEMBERSHIP"}
  , Record {point = "02300", uchar = "\8960", latex = "\\diameter", unicodemath = "\\diameter", cls = "N", category = "mathord", requirements = "mathabx", comments = "# \\varnothing (amssymb), DIAMETER SIGN"}
  , Record {point = "02302", uchar = "\8962", latex = "", unicodemath = "\\house", cls = "N", category = "mathord", requirements = "", comments = "HOUSE"}
  , Record {point = "02305", uchar = "\8965", latex = "", unicodemath = "\\varbarwedge", cls = "B", category = "mathbin", requirements = "", comments = "# \\barwedge (amssymb), PROJECTIVE (bar over small wedge) not nand"}
  , Record {point = "02306", uchar = "\8966", latex = "", unicodemath = "\\vardoublebarwedge", cls = "B", category = "mathbin", requirements = "", comments = "# \\doublebarwedge (amssymb), PERSPECTIVE (double bar over small wedge)"}
  , Record {point = "02308", uchar = "\8968", latex = "\\lceil", unicodemath = "\\lceil", cls = "O", category = "mathopen", requirements = "", comments = "LEFT CEILING"}
  , Record {point = "02309", uchar = "\8969", latex = "\\rceil", unicodemath = "\\rceil", cls = "C", category = "mathclose", requirements = "", comments = "RIGHT CEILING"}
  , Record {point = "0230A", uchar = "\8970", latex = "\\lfloor", unicodemath = "\\lfloor", cls = "O", category = "mathopen", requirements = "", comments = "LEFT FLOOR"}
  , Record {point = "0230B", uchar = "\8971", latex = "\\rfloor", unicodemath = "\\rfloor", cls = "C", category = "mathclose", requirements = "", comments = "RIGHT FLOOR"}
  , Record {point = "02310", uchar = "\8976", latex = "\\invneg", unicodemath = "\\invnot", cls = "N", category = "mathord", requirements = "wasysym", comments = "reverse not"}
  , Record {point = "02311", uchar = "\8977", latex = "\\wasylozenge", unicodemath = "\\sqlozenge", cls = "N", category = "mathord", requirements = "wasysym", comments = "SQUARE LOZENGE"}
  , Record {point = "02312", uchar = "\8978", latex = "", unicodemath = "\\profline", cls = "", category = "mathord", requirements = "", comments = "profile of a line"}
  , Record {point = "02313", uchar = "\8979", latex = "", unicodemath = "\\profsurf", cls = "", category = "mathord", requirements = "", comments = "profile of a surface"}
  , Record {point = "02317", uchar = "\8983", latex = "", unicodemath = "\\viewdata", cls = "", category = "mathord", requirements = "", comments = "VIEWDATA SQUARE"}
  , Record {point = "02319", uchar = "\8985", latex = "", unicodemath = "\\turnednot", cls = "N", category = "mathord", requirements = "", comments = "TURNED NOT SIGN"}
  , Record {point = "0231C", uchar = "\8988", latex = "\\ulcorner", unicodemath = "\\ulcorner", cls = "O", category = "mathopen", requirements = "amsfonts", comments = "upper left corner"}
  , Record {point = "0231D", uchar = "\8989", latex = "\\urcorner", unicodemath = "\\urcorner", cls = "C", category = "mathclose", requirements = "amsfonts", comments = "upper right corner"}
  , Record {point = "0231E", uchar = "\8990", latex = "\\llcorner", unicodemath = "\\llcorner", cls = "O", category = "mathopen", requirements = "amsfonts", comments = "lower left corner"}
  , Record {point = "0231F", uchar = "\8991", latex = "\\lrcorner", unicodemath = "\\lrcorner", cls = "C", category = "mathclose", requirements = "amsfonts", comments = "lower right corner"}
  , Record {point = "02320", uchar = "\8992", latex = "", unicodemath = "\\inttop", cls = "G", category = "mathord", requirements = "", comments = "TOP HALF INTEGRAL"}
  , Record {point = "02321", uchar = "\8993", latex = "", unicodemath = "\\intbottom", cls = "G", category = "mathord", requirements = "", comments = "BOTTOM HALF INTEGRAL"}
  , Record {point = "02322", uchar = "\8994", latex = "\\frown", unicodemath = "\\frown", cls = "R", category = "mathrel", requirements = "", comments = "# \\smallfrown, FROWN (down curve)"}
  , Record {point = "02323", uchar = "\8995", latex = "\\smile", unicodemath = "\\smile", cls = "R", category = "mathrel", requirements = "", comments = "# \\smallsmile, SMILE (up curve)"}
  , Record {point = "02329", uchar = "\9001", latex = "\\langle", unicodemath = "\\langle", cls = "O", category = "mathopen", requirements = "", comments = "LEFT-POINTING ANGLE BRACKET"}
  , Record {point = "0232A", uchar = "\9002", latex = "\\rangle", unicodemath = "\\rangle", cls = "O", category = "mathopen", requirements = "", comments = "RIGHT-POINTINGLEFT ANGLE BRACKET"}
  , Record {point = "0232C", uchar = "\9004", latex = "", unicodemath = "\\varhexagonlrbonds", cls = "", category = "mathord", requirements = "", comments = "six carbon ring, corner down, double bonds lower right etc"}
  , Record {point = "02332", uchar = "\9010", latex = "", unicodemath = "\\conictaper", cls = "", category = "mathord", requirements = "", comments = "CONICAL TAPER"}
  , Record {point = "02336", uchar = "\9014", latex = "", unicodemath = "\\topbot", cls = "N", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL I-BEAM, top and bottom"}
  , Record {point = "02337", uchar = "\9015", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL SQUISH QUAD"}
  , Record {point = "02338", uchar = "\9016", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD EQUAL"}
  , Record {point = "02339", uchar = "\9017", latex = "\\APLinv", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "APL FUNCTIONAL SYMBOL QUAD DIVIDE"}
  , Record {point = "0233A", uchar = "\9018", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD DIAMOND"}
  , Record {point = "0233B", uchar = "\9019", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD JOT"}
  , Record {point = "0233C", uchar = "\9020", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "# \\APLcirc{\\APLbox} (wasysym), APL FUNCTIONAL SYMBOL QUAD CIRCLE"}
  , Record {point = "0233D", uchar = "\9021", latex = "", unicodemath = "\\obar", cls = "B", category = "mathbin", requirements = "", comments = "# \\APLvert{\\Circle} (wasysym), x \\obar (stmaryrd), APL FUNCTIONAL SYMBOL CIRCLE STILE, circle with vertical bar"}
  , Record {point = "0233E", uchar = "\9022", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "# \\APLcirc{\\Circle} (wasysym), APL FUNCTIONAL SYMBOL CIRCLE JOT"}
  , Record {point = "0233F", uchar = "\9023", latex = "\\notslash", unicodemath = "\\APLnotslash", cls = "R", category = "mathrel", requirements = "wasysym", comments = "APL FUNCTIONAL SYMBOL SLASH BAR, solidus, bar through"}
  , Record {point = "02340", uchar = "\9024", latex = "\\notbackslash", unicodemath = "\\APLnotbackslash", cls = "", category = "mathord", requirements = "wasysym", comments = "APL FUNCTIONAL SYMBOL BACKSLASH BAR"}
  , Record {point = "02341", uchar = "\9025", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD SLASH"}
  , Record {point = "02342", uchar = "\9026", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD BACKSLASH"}
  , Record {point = "02343", uchar = "\9027", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD LESS-THAN"}
  , Record {point = "02344", uchar = "\9028", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD GREATER-THAN"}
  , Record {point = "02345", uchar = "\9029", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL LEFTWARDS VANE"}
  , Record {point = "02346", uchar = "\9030", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL RIGHTWARDS VANE"}
  , Record {point = "02347", uchar = "\9031", latex = "\\APLleftarrowbox", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "APL FUNCTIONAL SYMBOL QUAD LEFTWARDS ARROW"}
  , Record {point = "02348", uchar = "\9032", latex = "\\APLrightarrowbox", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "APL FUNCTIONAL SYMBOL QUAD RIGHTWARDS ARROW"}
  , Record {point = "02349", uchar = "\9033", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL CIRCLE BACKSLASH"}
  , Record {point = "0234A", uchar = "\9034", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL DOWN TACK UNDERBAR"}
  , Record {point = "0234B", uchar = "\9035", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "# \\APLvert{\\APLup} (wasysym), APL FUNCTIONAL SYMBOL DELTA STILE"}
  , Record {point = "0234C", uchar = "\9036", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD DOWN CARET"}
  , Record {point = "0234D", uchar = "\9037", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD DELTA"}
  , Record {point = "0234E", uchar = "\9038", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL DOWN TACK JOT"}
  , Record {point = "0234F", uchar = "\9039", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL UPWARDS VANE"}
  , Record {point = "02350", uchar = "\9040", latex = "\\APLuparrowbox", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "APL FUNCTIONAL SYMBOL QUAD UPWARDS ARROW"}
  , Record {point = "02351", uchar = "\9041", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL UP TACK OVERBAR"}
  , Record {point = "02352", uchar = "\9042", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "# \\APLvert{\\APLdown} (wasysym), APL FUNCTIONAL SYMBOL DEL STILE"}
  , Record {point = "02353", uchar = "\9043", latex = "", unicodemath = "\\APLboxupcaret", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD UP CARET"}
  , Record {point = "02354", uchar = "\9044", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD DEL"}
  , Record {point = "02355", uchar = "\9045", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL UP TACK JOT"}
  , Record {point = "02356", uchar = "\9046", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL DOWNWARDS VANE"}
  , Record {point = "02357", uchar = "\9047", latex = "\\APLdownarrowbox", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "APL FUNCTIONAL SYMBOL QUAD DOWNWARDS ARROW"}
  , Record {point = "02358", uchar = "\9048", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUOTE UNDERBAR"}
  , Record {point = "02359", uchar = "\9049", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL DELTA UNDERBAR"}
  , Record {point = "0235A", uchar = "\9050", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL DIAMOND UNDERBAR"}
  , Record {point = "0235B", uchar = "\9051", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL JOT UNDERBAR"}
  , Record {point = "0235C", uchar = "\9052", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL CIRCLE UNDERBAR"}
  , Record {point = "0235D", uchar = "\9053", latex = "\\APLcomment", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "APL FUNCTIONAL SYMBOL UP SHOE JOT"}
  , Record {point = "0235E", uchar = "\9054", latex = "\\APLinput", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "APL FUNCTIONAL SYMBOL QUOTE QUAD"}
  , Record {point = "0235F", uchar = "\9055", latex = "\\APLlog", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "APL FUNCTIONAL SYMBOL CIRCLE STAR"}
  , Record {point = "02360", uchar = "\9056", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD COLON"}
  , Record {point = "02361", uchar = "\9057", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL UP TACK DIAERESIS"}
  , Record {point = "02362", uchar = "\9058", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL DEL DIAERESIS"}
  , Record {point = "02363", uchar = "\9059", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL STAR DIAERESIS"}
  , Record {point = "02364", uchar = "\9060", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL JOT DIAERESIS"}
  , Record {point = "02365", uchar = "\9061", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL CIRCLE DIAERESIS"}
  , Record {point = "02366", uchar = "\9062", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL DOWN SHOE STILE"}
  , Record {point = "02367", uchar = "\9063", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL LEFT SHOE STILE"}
  , Record {point = "02368", uchar = "\9064", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL TILDE DIAERESIS"}
  , Record {point = "02369", uchar = "\9065", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL GREATER-THAN DIAERESIS"}
  , Record {point = "0236A", uchar = "\9066", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL COMMA BAR"}
  , Record {point = "0236B", uchar = "\9067", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "# \\APLnot{\\APLdown} (wasysym), APL FUNCTIONAL SYMBOL DEL TILDE"}
  , Record {point = "0236C", uchar = "\9068", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL ZILDE"}
  , Record {point = "0236D", uchar = "\9069", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL STILE TILDE"}
  , Record {point = "0236E", uchar = "\9070", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL SEMICOLON UNDERBAR"}
  , Record {point = "0236F", uchar = "\9071", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD NOT EQUAL"}
  , Record {point = "02370", uchar = "\9072", latex = "", unicodemath = "\\APLboxquestion", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL QUAD QUESTION"}
  , Record {point = "02371", uchar = "\9073", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL DOWN CARET TILDE"}
  , Record {point = "02372", uchar = "\9074", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL UP CARET TILDE"}
  , Record {point = "02373", uchar = "\9075", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL IOTA"}
  , Record {point = "02374", uchar = "\9076", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL RHO"}
  , Record {point = "02375", uchar = "\9077", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL OMEGA"}
  , Record {point = "02376", uchar = "\9078", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL ALPHA UNDERBAR"}
  , Record {point = "02377", uchar = "\9079", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL EPSILON UNDERBAR"}
  , Record {point = "02378", uchar = "\9080", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL IOTA UNDERBAR"}
  , Record {point = "02379", uchar = "\9081", latex = "", unicodemath = "", cls = "", category = "mathord", requirements = "", comments = "APL FUNCTIONAL SYMBOL OMEGA UNDERBAR"}
  , Record {point = "0237C", uchar = "\9084", latex = "", unicodemath = "\\rangledownzigzagarrow", cls = "", category = "mathord", requirements = "", comments = "RIGHT ANGLE WITH DOWNWARDS ZIGZAG ARROW"}
  , Record {point = "02394", uchar = "\9108", latex = "", unicodemath = "\\hexagon", cls = "N", category = "mathord", requirements = "", comments = "horizontal benzene ring [hexagon flat open]"}
  , Record {point = "0239B", uchar = "\9115", latex = "", unicodemath = "\\lparenuend", cls = "G", category = "mathord", requirements = "", comments = "LEFT PARENTHESIS UPPER HOOK"}
  , Record {point = "0239C", uchar = "\9116", latex = "", unicodemath = "\\lparenextender", cls = "G", category = "mathord", requirements = "", comments = "LEFT PARENTHESIS EXTENSION"}
  , Record {point = "0239D", uchar = "\9117", latex = "", unicodemath = "\\lparenlend", cls = "G", category = "mathord", requirements = "", comments = "LEFT PARENTHESIS LOWER HOOK"}
  , Record {point = "0239E", uchar = "\9118", latex = "", unicodemath = "\\rparenuend", cls = "G", category = "mathord", requirements = "", comments = "RIGHT PARENTHESIS UPPER HOOK"}
  , Record {point = "0239F", uchar = "\9119", latex = "", unicodemath = "\\rparenextender", cls = "G", category = "mathord", requirements = "", comments = "RIGHT PARENTHESIS EXTENSION"}
  , Record {point = "023A0", uchar = "\9120", latex = "", unicodemath = "\\rparenlend", cls = "G", category = "mathord", requirements = "", comments = "RIGHT PARENTHESIS LOWER HOOK"}
  , Record {point = "023A1", uchar = "\9121", latex = "", unicodemath = "\\lbrackuend", cls = "G", category = "mathord", requirements = "", comments = "LEFT SQUARE BRACKET UPPER CORNER"}
  , Record {point = "023A2", uchar = "\9122", latex = "", unicodemath = "\\lbrackextender", cls = "G", category = "mathord", requirements = "", comments = "LEFT SQUARE BRACKET EXTENSION"}
  , Record {point = "023A3", uchar = "\9123", latex = "", unicodemath = "\\lbracklend", cls = "G", category = "mathord", requirements = "", comments = "LEFT SQUARE BRACKET LOWER CORNER"}
  , Record {point = "023A4", uchar = "\9124", latex = "", unicodemath = "\\rbrackuend", cls = "G", category = "mathord", requirements = "", comments = "RIGHT SQUARE BRACKET UPPER CORNER"}
  , Record {point = "023A5", uchar = "\9125", latex = "", unicodemath = "\\rbrackextender", cls = "G", category = "mathord", requirements = "", comments = "RIGHT SQUARE BRACKET EXTENSION"}
  , Record {point = "023A6", uchar = "\9126", latex = "", unicodemath = "\\rbracklend", cls = "G", category = "mathord", requirements = "", comments = "RIGHT SQUARE BRACKET LOWER CORNER"}
  , Record {point = "023A7", uchar = "\9127", latex = "", unicodemath = "\\lbraceuend", cls = "G", category = "mathord", requirements = "", comments = "LEFT CURLY BRACKET UPPER HOOK"}
  , Record {point = "023A8", uchar = "\9128", latex = "", unicodemath = "\\lbracemid", cls = "G", category = "mathord", requirements = "", comments = "LEFT CURLY BRACKET MIDDLE PIECE"}
  , Record {point = "023A9", uchar = "\9129", latex = "", unicodemath = "\\lbracelend", cls = "G", category = "mathord", requirements = "", comments = "LEFT CURLY BRACKET LOWER HOOK"}
  , Record {point = "023AA", uchar = "\9130", latex = "", unicodemath = "\\vbraceextender", cls = "G", category = "mathord", requirements = "", comments = "CURLY BRACKET EXTENSION"}
  , Record {point = "023AB", uchar = "\9131", latex = "", unicodemath = "\\rbraceuend", cls = "G", category = "mathord", requirements = "", comments = "RIGHT CURLY BRACKET UPPER HOOK"}
  , Record {point = "023AC", uchar = "\9132", latex = "", unicodemath = "\\rbracemid", cls = "G", category = "mathord", requirements = "", comments = "RIGHT CURLY BRACKET MIDDLE PIECE"}
  , Record {point = "023AD", uchar = "\9133", latex = "", unicodemath = "\\rbracelend", cls = "G", category = "mathord", requirements = "", comments = "RIGHT CURLY BRACKET LOWER HOOK"}
  , Record {point = "023AE", uchar = "\9134", latex = "", unicodemath = "\\intextender", cls = "G", category = "mathord", requirements = "", comments = "INTEGRAL EXTENSION"}
  , Record {point = "023AF", uchar = "\9135", latex = "", unicodemath = "\\harrowextender", cls = "G", category = "mathord", requirements = "", comments = "HORIZONTAL LINE EXTENSION (used to extend arrows)"}
  , Record {point = "023B0", uchar = "\9136", latex = "", unicodemath = "\\lmoustache", cls = "R", category = "mathord", requirements = "", comments = "? \\lmoustache, UPPER LEFT OR LOWER RIGHT CURLY BRACKET SECTION"}
  , Record {point = "023B1", uchar = "\9137", latex = "", unicodemath = "\\rmoustache", cls = "R", category = "mathord", requirements = "", comments = "? \\rmoustache, UPPER RIGHT OR LOWER LEFT CURLY BRACKET SECTION"}
  , Record {point = "023B2", uchar = "\9138", latex = "", unicodemath = "\\sumtop", cls = "G", category = "mathord", requirements = "", comments = "SUMMATION TOP"}
  , Record {point = "023B3", uchar = "\9139", latex = "", unicodemath = "\\sumbottom", cls = "G", category = "mathord", requirements = "", comments = "SUMMATION BOTTOM"}
  , Record {point = "023B4", uchar = "\9140", latex = "", unicodemath = "\\overbracket", cls = "N", category = "mathover", requirements = "", comments = "TOP SQUARE BRACKET"}
  , Record {point = "023B5", uchar = "\9141", latex = "", unicodemath = "\\underbracket", cls = "N", category = "mathunder", requirements = "", comments = "BOTTOM SQUARE BRACKET"}
  , Record {point = "023B6", uchar = "\9142", latex = "", unicodemath = "\\bbrktbrk", cls = "N", category = "mathord", requirements = "", comments = "BOTTOM SQUARE BRACKET OVER TOP SQUARE BRACKET"}
  , Record {point = "023B7", uchar = "\9143", latex = "", unicodemath = "\\sqrtbottom", cls = "G", category = "mathord", requirements = "", comments = "RADICAL SYMBOL BOTTOM"}
  , Record {point = "023B8", uchar = "\9144", latex = "", unicodemath = "\\lvboxline", cls = "", category = "mathord", requirements = "", comments = "LEFT VERTICAL BOX LINE"}
  , Record {point = "023B9", uchar = "\9145", latex = "", unicodemath = "\\rvboxline", cls = "", category = "mathord", requirements = "", comments = "RIGHT VERTICAL BOX LINE"}
  , Record {point = "023CE", uchar = "\9166", latex = "", unicodemath = "\\varcarriagereturn", cls = "", category = "mathord", requirements = "", comments = "RETURN SYMBOL"}
  , Record {point = "023D0", uchar = "\9168", latex = "", unicodemath = "", cls = "G", category = "mathord", requirements = "", comments = "VERTICAL LINE EXTENSION (VERTICAL LINE EXTENSION)"}
  , Record {point = "023DC", uchar = "\9180", latex = "\\overparen", unicodemath = "\\overparen", cls = "N", category = "mathover", requirements = "wrisym", comments = "= \\wideparen (yhmath mathabx fourier), TOP PARENTHESIS (mathematical use)"}
  , Record {point = "023DD", uchar = "\9181", latex = "\\underparen", unicodemath = "\\underparen", cls = "N", category = "mathunder", requirements = "wrisym", comments = "BOTTOM PARENTHESIS (mathematical use)"}
  , Record {point = "023DE", uchar = "\9182", latex = "\\overbrace", unicodemath = "\\overbrace", cls = "N", category = "mathover", requirements = "", comments = "TOP CURLY BRACKET (mathematical use)"}
  , Record {point = "023DF", uchar = "\9183", latex = "\\underbrace", unicodemath = "\\underbrace", cls = "N", category = "mathunder", requirements = "", comments = "BOTTOM CURLY BRACKET (mathematical use)"}
  , Record {point = "023E0", uchar = "\9184", latex = "", unicodemath = "\\obrbrak", cls = "N", category = "mathord", requirements = "", comments = "TOP TORTOISE SHELL BRACKET (mathematical use)"}
  , Record {point = "023E1", uchar = "\9185", latex = "", unicodemath = "\\ubrbrak", cls = "N", category = "mathord", requirements = "", comments = "BOTTOM TORTOISE SHELL BRACKET (mathematical use)"}
  , Record {point = "023E2", uchar = "\9186", latex = "", unicodemath = "\\trapezium", cls = "N", category = "mathord", requirements = "", comments = "WHITE TRAPEZIUM"}
  , Record {point = "023E3", uchar = "\9187", latex = "", unicodemath = "\\benzenr", cls = "N", category = "mathord", requirements = "", comments = "BENZENE RING WITH CIRCLE"}
  , Record {point = "023E4", uchar = "\9188", latex = "", unicodemath = "\\strns", cls = "N", category = "mathord", requirements = "", comments = "STRAIGHTNESS"}
  , Record {point = "023E5", uchar = "\9189", latex = "", unicodemath = "\\fltns", cls = "N", category = "mathord", requirements = "", comments = "FLATNESS"}
  , Record {point = "023E6", uchar = "\9190", latex = "", unicodemath = "\\accurrent", cls = "N", category = "mathord", requirements = "", comments = "# \\AC (wasysym), AC CURRENT"}
  , Record {point = "023E7", uchar = "\9191", latex = "", unicodemath = "\\elinters", cls = "N", category = "mathord", requirements = "", comments = "ELECTRICAL INTERSECTION"}
  , Record {point = "024C8", uchar = "\9416", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "oS capital S in circle"}
  , Record {point = "02506", uchar = "\9478", latex = "", unicodemath = "\\bdtriplevdash", cls = "", category = "mathord", requirements = "", comments = "doubly broken vert"}
  , Record {point = "02580", uchar = "\9600", latex = "", unicodemath = "\\blockuphalf", cls = "", category = "mathord", requirements = "", comments = "UPPER HALF BLOCK"}
  , Record {point = "02584", uchar = "\9604", latex = "", unicodemath = "\\blocklowhalf", cls = "", category = "mathord", requirements = "", comments = "LOWER HALF BLOCK"}
  , Record {point = "02588", uchar = "\9608", latex = "", unicodemath = "\\blockfull", cls = "", category = "mathord", requirements = "", comments = "FULL BLOCK"}
  , Record {point = "0258C", uchar = "\9612", latex = "", unicodemath = "\\blocklefthalf", cls = "", category = "mathord", requirements = "", comments = "LEFT HALF BLOCK"}
  , Record {point = "02590", uchar = "\9616", latex = "", unicodemath = "\\blockrighthalf", cls = "", category = "mathord", requirements = "", comments = "RIGHT HALF BLOCK"}
  , Record {point = "02591", uchar = "\9617", latex = "", unicodemath = "\\blockqtrshaded", cls = "", category = "mathord", requirements = "", comments = "25\\% shaded block"}
  , Record {point = "02592", uchar = "\9618", latex = "", unicodemath = "\\blockhalfshaded", cls = "", category = "mathord", requirements = "", comments = "50\\% shaded block"}
  , Record {point = "02593", uchar = "\9619", latex = "", unicodemath = "\\blockthreeqtrshaded", cls = "", category = "mathord", requirements = "", comments = "75\\% shaded block"}
  , Record {point = "025A0", uchar = "\9632", latex = "\\blacksquare", unicodemath = "\\mdlgblksquare", cls = "N", category = "mathord", requirements = "amsmath", comments = "square, filled"}
  , Record {point = "025A1", uchar = "\9633", latex = "\\square", unicodemath = "\\mdlgwhtsquare", cls = "N", category = "mathord", requirements = "", comments = "square, open"}
  , Record {point = "025A2", uchar = "\9634", latex = "", unicodemath = "\\squoval", cls = "", category = "mathord", requirements = "", comments = "WHITE SQUARE WITH ROUNDED CORNERS"}
  , Record {point = "025A3", uchar = "\9635", latex = "", unicodemath = "\\blackinwhitesquare", cls = "", category = "mathord", requirements = "", comments = "WHITE SQUARE CONTAINING BLACK SMALL SQUARE"}
  , Record {point = "025A4", uchar = "\9636", latex = "", unicodemath = "\\squarehfill", cls = "", category = "mathord", requirements = "", comments = "square, horizontal rule filled"}
  , Record {point = "025A5", uchar = "\9637", latex = "", unicodemath = "\\squarevfill", cls = "", category = "mathord", requirements = "", comments = "square, vertical rule filled"}
  , Record {point = "025A6", uchar = "\9638", latex = "", unicodemath = "\\squarehvfill", cls = "", category = "mathord", requirements = "", comments = "SQUARE WITH ORTHOGONAL CROSSHATCH FILL"}
  , Record {point = "025A7", uchar = "\9639", latex = "", unicodemath = "\\squarenwsefill", cls = "", category = "mathord", requirements = "", comments = "square, nw-to-se rule filled"}
  , Record {point = "025A8", uchar = "\9640", latex = "", unicodemath = "\\squareneswfill", cls = "", category = "mathord", requirements = "", comments = "square, ne-to-sw rule filled"}
  , Record {point = "025A9", uchar = "\9641", latex = "", unicodemath = "\\squarecrossfill", cls = "", category = "mathord", requirements = "", comments = "SQUARE WITH DIAGONAL CROSSHATCH FILL"}
  , Record {point = "025AA", uchar = "\9642", latex = "", unicodemath = "\\smblksquare", cls = "N", category = "mathord", requirements = "", comments = "sq bullet, filled"}
  , Record {point = "025AB", uchar = "\9643", latex = "", unicodemath = "\\smwhtsquare", cls = "N", category = "mathord", requirements = "", comments = "WHITE SMALL SQUARE"}
  , Record {point = "025AC", uchar = "\9644", latex = "", unicodemath = "\\hrectangleblack", cls = "", category = "mathord", requirements = "", comments = "BLACK RECTANGLE"}
  , Record {point = "025AD", uchar = "\9645", latex = "", unicodemath = "\\hrectangle", cls = "N", category = "mathord", requirements = "", comments = "horizontal rectangle, open"}
  , Record {point = "025AE", uchar = "\9646", latex = "", unicodemath = "\\vrectangleblack", cls = "N", category = "mathord", requirements = "", comments = "BLACK VERTICAL RECTANGLE"}
  , Record {point = "025AF", uchar = "\9647", latex = "", unicodemath = "\\vrectangle", cls = "N", category = "mathord", requirements = "", comments = "rectangle, white (vertical)"}
  , Record {point = "025B0", uchar = "\9648", latex = "", unicodemath = "\\parallelogramblack", cls = "", category = "mathord", requirements = "", comments = "BLACK PARALLELOGRAM"}
  , Record {point = "025B1", uchar = "\9649", latex = "", unicodemath = "\\parallelogram", cls = "N", category = "mathord", requirements = "", comments = "parallelogram, open"}
  , Record {point = "025B2", uchar = "\9650", latex = "", unicodemath = "\\bigblacktriangleup", cls = "B", category = "mathord", requirements = "", comments = "BLACK UP-POINTING TRIANGLE"}
  , Record {point = "025B3", uchar = "\9651", latex = "\\bigtriangleup", unicodemath = "\\bigtriangleup", cls = "B", category = "mathbin", requirements = "-stmaryrd", comments = "= \\triangle (amsfonts), # \\vartriangle (amssymb), big up triangle, open"}
  , Record {point = "025B4", uchar = "\9652", latex = "\\blacktriangleup", unicodemath = "\\blacktriangle", cls = "B", category = "mathbin", requirements = "mathabx", comments = "up triangle, filled"}
  , Record {point = "025B5", uchar = "\9653", latex = "\\smalltriangleup", unicodemath = "\\vartriangle", cls = "B", category = "mathbin", requirements = "mathabx", comments = "# \\vartriangle (amssymb), small up triangle, open"}
  , Record {point = "025B6", uchar = "\9654", latex = "\\RHD", unicodemath = "\\blacktriangleright", cls = "B", category = "mathbin", requirements = "wasysym", comments = "= \\blacktriangleright (fourier -mathabx), (large) right triangle, filled"}
  , Record {point = "025B7", uchar = "\9655", latex = "\\rhd", unicodemath = "\\triangleright", cls = "B", category = "mathbin", requirements = "amssymb wasysym", comments = "= \\rres (oz), = \\RightTriangle (wrisym), (large) right triangle, open; z notation range restriction"}
  , Record {point = "025B8", uchar = "\9656", latex = "\\blacktriangleright", unicodemath = "\\smallblacktriangleright", cls = "B", category = "mathbin", requirements = "mathabx -fourier", comments = "right triangle, filled"}
  , Record {point = "025B9", uchar = "\9657", latex = "\\smalltriangleright", unicodemath = "\\smalltriangleright", cls = "B", category = "mathbin", requirements = "mathabx", comments = "# \\triangleright, x \\triangleright (mathabx), right triangle, open"}
  , Record {point = "025BA", uchar = "\9658", latex = "", unicodemath = "\\blackpointerright", cls = "", category = "mathord", requirements = "", comments = "BLACK RIGHT-POINTING POINTER"}
  , Record {point = "025BB", uchar = "\9659", latex = "", unicodemath = "\\whitepointerright", cls = "", category = "mathord", requirements = "", comments = "# \\triangleright (mathabx), WHITE RIGHT-POINTING POINTER"}
  , Record {point = "025BC", uchar = "\9660", latex = "", unicodemath = "\\bigblacktriangledown", cls = "B", category = "mathord", requirements = "", comments = "big down triangle, filled"}
  , Record {point = "025BD", uchar = "\9661", latex = "\\bigtriangledown", unicodemath = "\\bigtriangledown", cls = "B", category = "mathbin", requirements = "-stmaryrd", comments = "big down triangle, open"}
  , Record {point = "025BE", uchar = "\9662", latex = "\\blacktriangledown", unicodemath = "\\blacktriangledown", cls = "B", category = "mathbin", requirements = "mathabx", comments = "BLACK DOWN-POINTING SMALL TRIANGLE"}
  , Record {point = "025BF", uchar = "\9663", latex = "\\smalltriangledown", unicodemath = "\\triangledown", cls = "B", category = "mathbin", requirements = "mathabx", comments = "# \\triangledown (amssymb), WHITE DOWN-POINTING SMALL TRIANGLE"}
  , Record {point = "025C0", uchar = "\9664", latex = "\\LHD", unicodemath = "\\blacktriangleleft", cls = "B", category = "mathbin", requirements = "wasysym", comments = "= \\blacktriangleleft (fourier -mathabx), (large) left triangle, filled"}
  , Record {point = "025C1", uchar = "\9665", latex = "\\lhd", unicodemath = "\\triangleleft", cls = "B", category = "mathbin", requirements = "amssymb wasysym", comments = "= \\dres (oz), = \\LeftTriangle (wrisym), (large) left triangle, open; z notation domain restriction"}
  , Record {point = "025C2", uchar = "\9666", latex = "\\blacktriangleleft", unicodemath = "\\smallblacktriangleleft", cls = "B", category = "mathbin", requirements = "mathabx -fourier", comments = "left triangle, filled"}
  , Record {point = "025C3", uchar = "\9667", latex = "\\smalltriangleleft", unicodemath = "\\smalltriangleleft", cls = "B", category = "mathbin", requirements = "mathabx", comments = "# \\triangleleft, x \\triangleleft (mathabx), left triangle, open"}
  , Record {point = "025C4", uchar = "\9668", latex = "", unicodemath = "\\blackpointerleft", cls = "B", category = "mathord", requirements = "", comments = "BLACK LEFT-POINTING POINTER"}
  , Record {point = "025C5", uchar = "\9669", latex = "", unicodemath = "\\whitepointerleft", cls = "B", category = "mathord", requirements = "", comments = "# \\triangleleft (mathabx), WHITE LEFT-POINTING POINTER"}
  , Record {point = "025C6", uchar = "\9670", latex = "\\Diamondblack", unicodemath = "\\mdlgblkdiamond", cls = "N", category = "mathord", requirements = "txfonts", comments = "BLACK DIAMOND"}
  , Record {point = "025C7", uchar = "\9671", latex = "\\Diamond", unicodemath = "\\mdlgwhtdiamond", cls = "N", category = "mathord", requirements = "amssymb", comments = "WHITE DIAMOND; diamond, open"}
  , Record {point = "025C8", uchar = "\9672", latex = "", unicodemath = "\\blackinwhitediamond", cls = "N", category = "mathord", requirements = "", comments = "WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND"}
  , Record {point = "025C9", uchar = "\9673", latex = "", unicodemath = "\\fisheye", cls = "N", category = "mathord", requirements = "", comments = "FISHEYE"}
  , Record {point = "025CA", uchar = "\9674", latex = "\\lozenge", unicodemath = "\\mdlgwhtlozenge", cls = "B", category = "mathord", requirements = "amssymb", comments = "LOZENGE or total mark"}
  , Record {point = "025CB", uchar = "\9675", latex = "\\Circle", unicodemath = "\\mdlgwhtcircle", cls = "B", category = "mathbin", requirements = "wasysym", comments = "medium large circle"}
  , Record {point = "025CC", uchar = "\9676", latex = "", unicodemath = "\\dottedcircle", cls = "", category = "mathord", requirements = "", comments = "DOTTED CIRCLE"}
  , Record {point = "025CD", uchar = "\9677", latex = "", unicodemath = "\\circlevertfill", cls = "", category = "mathord", requirements = "", comments = "CIRCLE WITH VERTICAL FILL"}
  , Record {point = "025CE", uchar = "\9678", latex = "", unicodemath = "\\bullseye", cls = "N", category = "mathord", requirements = "", comments = "# \\circledcirc (amssymb), BULLSEYE"}
  , Record {point = "025CF", uchar = "\9679", latex = "\\CIRCLE", unicodemath = "\\mdlgblkcircle", cls = "N", category = "mathord", requirements = "wasysym", comments = "circle, filled"}
  , Record {point = "025D0", uchar = "\9680", latex = "\\LEFTcircle", unicodemath = "\\circlelefthalfblack", cls = "", category = "mathord", requirements = "wasysym", comments = "circle, filled left half [harvey ball]"}
  , Record {point = "025D1", uchar = "\9681", latex = "\\RIGHTcircle", unicodemath = "\\circlerighthalfblack", cls = "", category = "mathord", requirements = "wasysym", comments = "circle, filled right half"}
  , Record {point = "025D2", uchar = "\9682", latex = "", unicodemath = "\\circlebottomhalfblack", cls = "", category = "mathord", requirements = "", comments = "circle, filled bottom half"}
  , Record {point = "025D3", uchar = "\9683", latex = "", unicodemath = "\\circletophalfblack", cls = "", category = "mathord", requirements = "", comments = "circle, filled top half"}
  , Record {point = "025D4", uchar = "\9684", latex = "", unicodemath = "\\circleurquadblack", cls = "", category = "mathord", requirements = "", comments = "CIRCLE WITH UPPER RIGHT QUADRANT BLACK"}
  , Record {point = "025D5", uchar = "\9685", latex = "", unicodemath = "\\blackcircleulquadwhite", cls = "", category = "mathord", requirements = "", comments = "CIRCLE WITH ALL BUT UPPER LEFT QUADRANT BLACK"}
  , Record {point = "025D6", uchar = "\9686", latex = "\\LEFTCIRCLE", unicodemath = "\\blacklefthalfcircle", cls = "N", category = "mathord", requirements = "wasysym", comments = "LEFT HALF BLACK CIRCLE"}
  , Record {point = "025D7", uchar = "\9687", latex = "\\RIGHTCIRCLE", unicodemath = "\\blackrighthalfcircle", cls = "N", category = "mathord", requirements = "wasysym", comments = "RIGHT HALF BLACK CIRCLE"}
  , Record {point = "025D8", uchar = "\9688", latex = "", unicodemath = "\\inversebullet", cls = "", category = "mathord", requirements = "", comments = "INVERSE BULLET"}
  , Record {point = "025D9", uchar = "\9689", latex = "", unicodemath = "\\inversewhitecircle", cls = "", category = "mathord", requirements = "", comments = "INVERSE WHITE CIRCLE"}
  , Record {point = "025DA", uchar = "\9690", latex = "", unicodemath = "\\invwhiteupperhalfcircle", cls = "", category = "mathord", requirements = "", comments = "UPPER HALF INVERSE WHITE CIRCLE"}
  , Record {point = "025DB", uchar = "\9691", latex = "", unicodemath = "\\invwhitelowerhalfcircle", cls = "", category = "mathord", requirements = "", comments = "LOWER HALF INVERSE WHITE CIRCLE"}
  , Record {point = "025DC", uchar = "\9692", latex = "", unicodemath = "\\ularc", cls = "", category = "mathord", requirements = "", comments = "UPPER LEFT QUADRANT CIRCULAR ARC"}
  , Record {point = "025DD", uchar = "\9693", latex = "", unicodemath = "\\urarc", cls = "", category = "mathord", requirements = "", comments = "UPPER RIGHT QUADRANT CIRCULAR ARC"}
  , Record {point = "025DE", uchar = "\9694", latex = "", unicodemath = "\\lrarc", cls = "", category = "mathord", requirements = "", comments = "LOWER RIGHT QUADRANT CIRCULAR ARC"}
  , Record {point = "025DF", uchar = "\9695", latex = "", unicodemath = "\\llarc", cls = "", category = "mathord", requirements = "", comments = "LOWER LEFT QUADRANT CIRCULAR ARC"}
  , Record {point = "025E0", uchar = "\9696", latex = "", unicodemath = "\\topsemicircle", cls = "", category = "mathord", requirements = "", comments = "UPPER HALF CIRCLE"}
  , Record {point = "025E1", uchar = "\9697", latex = "", unicodemath = "\\botsemicircle", cls = "", category = "mathord", requirements = "", comments = "LOWER HALF CIRCLE"}
  , Record {point = "025E2", uchar = "\9698", latex = "", unicodemath = "\\lrblacktriangle", cls = "N", category = "mathord", requirements = "", comments = "lower right triangle, filled"}
  , Record {point = "025E3", uchar = "\9699", latex = "", unicodemath = "\\llblacktriangle", cls = "N", category = "mathord", requirements = "", comments = "lower left triangle, filled"}
  , Record {point = "025E4", uchar = "\9700", latex = "", unicodemath = "\\ulblacktriangle", cls = "N", category = "mathord", requirements = "", comments = "upper left triangle, filled"}
  , Record {point = "025E5", uchar = "\9701", latex = "", unicodemath = "\\urblacktriangle", cls = "N", category = "mathord", requirements = "", comments = "upper right triangle, filled"}
  , Record {point = "025E6", uchar = "\9702", latex = "", unicodemath = "\\smwhtcircle", cls = "B", category = "mathord", requirements = "", comments = "WHITE BULLET"}
  , Record {point = "025E7", uchar = "\9703", latex = "", unicodemath = "\\squareleftblack", cls = "", category = "mathord", requirements = "", comments = "square, filled left half"}
  , Record {point = "025E8", uchar = "\9704", latex = "", unicodemath = "\\squarerightblack", cls = "", category = "mathord", requirements = "", comments = "square, filled right half"}
  , Record {point = "025E9", uchar = "\9705", latex = "", unicodemath = "\\squareulblack", cls = "", category = "mathord", requirements = "", comments = "square, filled top left corner"}
  , Record {point = "025EA", uchar = "\9706", latex = "", unicodemath = "\\squarelrblack", cls = "", category = "mathord", requirements = "", comments = "square, filled bottom right corner"}
  , Record {point = "025EB", uchar = "\9707", latex = "\\boxbar", unicodemath = "\\boxbar", cls = "B", category = "mathbin", requirements = "stmaryrd txfonts", comments = "vertical bar in box"}
  , Record {point = "025EC", uchar = "\9708", latex = "", unicodemath = "\\trianglecdot", cls = "B", category = "mathord", requirements = "", comments = "triangle with centered dot"}
  , Record {point = "025ED", uchar = "\9709", latex = "", unicodemath = "\\triangleleftblack", cls = "", category = "mathord", requirements = "", comments = "UP-POINTING TRIANGLE WITH LEFT HALF BLACK"}
  , Record {point = "025EE", uchar = "\9710", latex = "", unicodemath = "\\trianglerightblack", cls = "", category = "mathord", requirements = "", comments = "UP-POINTING TRIANGLE WITH RIGHT HALF BLACK"}
  , Record {point = "025EF", uchar = "\9711", latex = "", unicodemath = "\\lgwhtcircle", cls = "N", category = "mathord", requirements = "", comments = "LARGE CIRCLE"}
  , Record {point = "025F0", uchar = "\9712", latex = "", unicodemath = "\\squareulquad", cls = "", category = "mathord", requirements = "", comments = "WHITE SQUARE WITH UPPER LEFT QUADRANT"}
  , Record {point = "025F1", uchar = "\9713", latex = "", unicodemath = "\\squarellquad", cls = "", category = "mathord", requirements = "", comments = "WHITE SQUARE WITH LOWER LEFT QUADRANT"}
  , Record {point = "025F2", uchar = "\9714", latex = "", unicodemath = "\\squarelrquad", cls = "", category = "mathord", requirements = "", comments = "WHITE SQUARE WITH LOWER RIGHT QUADRANT"}
  , Record {point = "025F3", uchar = "\9715", latex = "", unicodemath = "\\squareurquad", cls = "", category = "mathord", requirements = "", comments = "WHITE SQUARE WITH UPPER RIGHT QUADRANT"}
  , Record {point = "025F4", uchar = "\9716", latex = "", unicodemath = "\\circleulquad", cls = "", category = "mathord", requirements = "", comments = "WHITE CIRCLE WITH UPPER LEFT QUADRANT"}
  , Record {point = "025F5", uchar = "\9717", latex = "", unicodemath = "\\circlellquad", cls = "", category = "mathord", requirements = "", comments = "WHITE CIRCLE WITH LOWER LEFT QUADRANT"}
  , Record {point = "025F6", uchar = "\9718", latex = "", unicodemath = "\\circlelrquad", cls = "", category = "mathord", requirements = "", comments = "WHITE CIRCLE WITH LOWER RIGHT QUADRANT"}
  , Record {point = "025F7", uchar = "\9719", latex = "", unicodemath = "\\circleurquad", cls = "", category = "mathord", requirements = "", comments = "WHITE CIRCLE WITH UPPER RIGHT QUADRANT"}
  , Record {point = "025F8", uchar = "\9720", latex = "", unicodemath = "\\ultriangle", cls = "B", category = "mathord", requirements = "", comments = "UPPER LEFT TRIANGLE"}
  , Record {point = "025F9", uchar = "\9721", latex = "", unicodemath = "\\urtriangle", cls = "B", category = "mathord", requirements = "", comments = "UPPER RIGHT TRIANGLE"}
  , Record {point = "025FA", uchar = "\9722", latex = "", unicodemath = "\\lltriangle", cls = "B", category = "mathord", requirements = "", comments = "LOWER LEFT TRIANGLE"}
  , Record {point = "025FB", uchar = "\9723", latex = "\\square", unicodemath = "\\mdwhtsquare", cls = "B", category = "mathord", requirements = "amssymb -fourier", comments = "WHITE MEDIUM SQUARE"}
  , Record {point = "025FC", uchar = "\9724", latex = "\\blacksquare", unicodemath = "\\mdblksquare", cls = "B", category = "mathord", requirements = "amssymb -fourier", comments = "BLACK MEDIUM SQUARE"}
  , Record {point = "025FD", uchar = "\9725", latex = "", unicodemath = "\\mdsmwhtsquare", cls = "B", category = "mathord", requirements = "", comments = "WHITE MEDIUM SMALL SQUARE"}
  , Record {point = "025FE", uchar = "\9726", latex = "", unicodemath = "\\mdsmblksquare", cls = "B", category = "mathord", requirements = "", comments = "BLACK MEDIUM SMALL SQUARE"}
  , Record {point = "025FF", uchar = "\9727", latex = "", unicodemath = "\\lrtriangle", cls = "B", category = "mathord", requirements = "", comments = "LOWER RIGHT TRIANGLE"}
  , Record {point = "02605", uchar = "\9733", latex = "\\bigstar", unicodemath = "\\bigstar", cls = "B", category = "mathord", requirements = "amssymb", comments = "star, filled"}
  , Record {point = "02606", uchar = "\9734", latex = "", unicodemath = "\\bigwhitestar", cls = "B", category = "mathord", requirements = "", comments = "star, open"}
  , Record {point = "02609", uchar = "\9737", latex = "\\Sun", unicodemath = "\\astrosun", cls = "N", category = "mathord", requirements = "mathabx", comments = "SUN"}
  , Record {point = "0260C", uchar = "\9740", latex = "", unicodemath = "", cls = "N", category = "mathord", requirements = "wasysym", comments = "text \\CONJUNCTION (wasysym), CONJUNCTION"}
  , Record {point = "02610", uchar = "\9744", latex = "\\Square", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "BALLOT BOX"}
  , Record {point = "02611", uchar = "\9745", latex = "\\CheckedBox", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "t \\Checkedbox (marvosym), BALLOT BOX WITH CHECK"}
  , Record {point = "02612", uchar = "\9746", latex = "\\XBox", unicodemath = "", cls = "N", category = "mathord", requirements = "wasysym", comments = "t \\Crossedbox (marvosym), BALLOT BOX WITH X"}
  , Record {point = "02615", uchar = "\9749", latex = "\\steaming", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "HOT BEVERAGE"}
  , Record {point = "0261E", uchar = "\9758", latex = "\\pointright", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "WHITE RIGHT POINTING INDEX"}
  , Record {point = "02620", uchar = "\9760", latex = "\\skull", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "SKULL AND CROSSBONES"}
  , Record {point = "02621", uchar = "\9761", latex = "", unicodemath = "\\danger", cls = "", category = "mathord", requirements = "", comments = "CAUTION SIGN, dangerous bend"}
  , Record {point = "02622", uchar = "\9762", latex = "\\radiation", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "RADIOACTIVE SIGN"}
  , Record {point = "02623", uchar = "\9763", latex = "\\biohazard", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "BIOHAZARD SIGN"}
  , Record {point = "0262F", uchar = "\9775", latex = "\\yinyang", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "YIN YANG"}
  , Record {point = "02639", uchar = "\9785", latex = "\\frownie", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "= \\sadface (arevmath), WHITE FROWNING FACE"}
  , Record {point = "0263A", uchar = "\9786", latex = "\\smiley", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "= \\smileface (arevmath), WHITE SMILING FACE"}
  , Record {point = "0263B", uchar = "\9787", latex = "\\blacksmiley", unicodemath = "\\blacksmiley", cls = "", category = "mathord", requirements = "wasysym", comments = "= \\invsmileface (arevmath), BLACK SMILING FACE"}
  , Record {point = "0263C", uchar = "\9788", latex = "\\sun", unicodemath = "\\sun", cls = "", category = "mathord", requirements = "wasysym", comments = "WHITE SUN WITH RAYS"}
  , Record {point = "0263D", uchar = "\9789", latex = "\\rightmoon", unicodemath = "\\rightmoon", cls = "N", category = "mathord", requirements = "wasysym mathabx", comments = "FIRST QUARTER MOON"}
  , Record {point = "0263E", uchar = "\9790", latex = "\\leftmoon", unicodemath = "\\leftmoon", cls = "N", category = "mathord", requirements = "wasysym mathabx", comments = "LAST QUARTER MOON"}
  , Record {point = "0263F", uchar = "\9791", latex = "\\mercury", unicodemath = "", cls = "N", category = "mathord", requirements = "wasysym", comments = "= \\Mercury (mathabx), MERCURY"}
  , Record {point = "02640", uchar = "\9792", latex = "\\female", unicodemath = "\\female", cls = "N", category = "mathord", requirements = "wasysym", comments = "= \\Venus (mathabx), = \\girl (mathabx), venus, female"}
  , Record {point = "02641", uchar = "\9793", latex = "\\earth", unicodemath = "", cls = "N", category = "mathord", requirements = "wasysym", comments = "= \\varEarth (mathabx), EARTH"}
  , Record {point = "02642", uchar = "\9794", latex = "\\male", unicodemath = "\\male", cls = "N", category = "mathord", requirements = "wasysym", comments = "= \\Mars (mathabx), = \\boy (mathabx), mars, male"}
  , Record {point = "02643", uchar = "\9795", latex = "\\jupiter", unicodemath = "", cls = "N", category = "mathord", requirements = "wasysym", comments = "= \\Jupiter (mathabx), JUPITER"}
  , Record {point = "02644", uchar = "\9796", latex = "\\saturn", unicodemath = "", cls = "N", category = "mathord", requirements = "wasysym", comments = "= \\Saturn (mathabx), SATURN"}
  , Record {point = "02645", uchar = "\9797", latex = "\\uranus", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "= \\Uranus (mathabx), URANUS"}
  , Record {point = "02646", uchar = "\9798", latex = "\\neptune", unicodemath = "", cls = "N", category = "mathord", requirements = "wasysym", comments = "= \\Neptune (mathabx), NEPTUNE"}
  , Record {point = "02647", uchar = "\9799", latex = "\\pluto", unicodemath = "", cls = "N", category = "mathord", requirements = "wasysym", comments = "= \\Pluto (mathabx), PLUTO"}
  , Record {point = "02648", uchar = "\9800", latex = "\\aries", unicodemath = "", cls = "N", category = "mathord", requirements = "wasysym", comments = "= \\Aries (mathabx), ARIES"}
  , Record {point = "02649", uchar = "\9801", latex = "\\taurus", unicodemath = "", cls = "N", category = "mathord", requirements = "wasysym", comments = "= \\Taurus (mathabx), TAURUS"}
  , Record {point = "0264A", uchar = "\9802", latex = "\\gemini", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "= \\Gemini (mathabx), GEMINI"}
  , Record {point = "0264B", uchar = "\9803", latex = "\\cancer", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "CANCER"}
  , Record {point = "0264C", uchar = "\9804", latex = "\\leo", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "= \\Leo (mathabx), LEO"}
  , Record {point = "0264D", uchar = "\9805", latex = "\\virgo", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "VIRGO"}
  , Record {point = "0264E", uchar = "\9806", latex = "\\libra", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "= \\Libra (mathabx), LIBRA"}
  , Record {point = "0264F", uchar = "\9807", latex = "\\scorpio", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "= \\Scorpio (mathabx), SCORPIUS"}
  , Record {point = "02650", uchar = "\9808", latex = "\\sagittarius", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "SAGITTARIUS"}
  , Record {point = "02651", uchar = "\9809", latex = "\\capricornus", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "CAPRICORN"}
  , Record {point = "02652", uchar = "\9810", latex = "\\aquarius", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "AQUARIUS"}
  , Record {point = "02653", uchar = "\9811", latex = "\\pisces", unicodemath = "", cls = "", category = "mathord", requirements = "wasysym", comments = "PISCES"}
  , Record {point = "02660", uchar = "\9824", latex = "\\spadesuit", unicodemath = "\\spadesuit", cls = "N", category = "mathord", requirements = "", comments = "spades suit symbol"}
  , Record {point = "02661", uchar = "\9825", latex = "\\heartsuit", unicodemath = "\\heartsuit", cls = "N", category = "mathord", requirements = "", comments = "heart suit symbol"}
  , Record {point = "02662", uchar = "\9826", latex = "\\diamondsuit", unicodemath = "\\diamondsuit", cls = "N", category = "mathord", requirements = "", comments = "diamond suit symbol"}
  , Record {point = "02663", uchar = "\9827", latex = "\\clubsuit", unicodemath = "\\clubsuit", cls = "N", category = "mathord", requirements = "", comments = "club suit symbol"}
  , Record {point = "02664", uchar = "\9828", latex = "\\varspadesuit", unicodemath = "\\varspadesuit", cls = "N", category = "mathord", requirements = "txfonts", comments = "= \\varspade (arevmath), spade, white (card suit)"}
  , Record {point = "02665", uchar = "\9829", latex = "\\varheartsuit", unicodemath = "\\varheartsuit", cls = "N", category = "mathord", requirements = "txfonts", comments = "= \\varheart (arevmath), filled heart (card suit)"}
  , Record {point = "02666", uchar = "\9830", latex = "\\vardiamondsuit", unicodemath = "\\vardiamondsuit", cls = "N", category = "mathord", requirements = "txfonts", comments = "= \\vardiamond (arevmath), filled diamond (card suit)"}
  , Record {point = "02667", uchar = "\9831", latex = "\\varclubsuit", unicodemath = "\\varclubsuit", cls = "N", category = "mathord", requirements = "txfonts", comments = "= \\varclub (arevmath), club, white (card suit)"}
  , Record {point = "02669", uchar = "\9833", latex = "\\quarternote", unicodemath = "\\quarternote", cls = "N", category = "mathord", requirements = "arevmath wasysym", comments = "music note (sung text sign)"}
  , Record {point = "0266A", uchar = "\9834", latex = "\\eighthnote", unicodemath = "\\eighthnote", cls = "", category = "mathord", requirements = "arevmath", comments = "EIGHTH NOTE"}
  , Record {point = "0266B", uchar = "\9835", latex = "\\twonotes", unicodemath = "\\twonotes", cls = "", category = "mathord", requirements = "wasysym", comments = "BEAMED EIGHTH NOTES"}
  , Record {point = "0266C", uchar = "\9836", latex = "\\sixteenthnote", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "BEAMED SIXTEENTH NOTES"}
  , Record {point = "0266D", uchar = "\9837", latex = "\\flat", unicodemath = "\\flat", cls = "N", category = "mathord", requirements = "", comments = "musical flat"}
  , Record {point = "0266E", uchar = "\9838", latex = "\\natural", unicodemath = "\\natural", cls = "N", category = "mathord", requirements = "", comments = "music natural"}
  , Record {point = "0266F", uchar = "\9839", latex = "\\sharp", unicodemath = "\\sharp", cls = "N", category = "mathord", requirements = "", comments = "= \\# (oz), MUSIC SHARP SIGN, z notation infix bag count"}
  , Record {point = "0267B", uchar = "\9851", latex = "\\recycle", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "BLACK UNIVERSAL RECYCLING SYMBOL"}
  , Record {point = "0267E", uchar = "\9854", latex = "", unicodemath = "\\acidfree", cls = "", category = "mathord", requirements = "", comments = "PERMANENT PAPER SIGN"}
  , Record {point = "02680", uchar = "\9856", latex = "", unicodemath = "\\dicei", cls = "N", category = "mathord", requirements = "", comments = "DIE FACE-1"}
  , Record {point = "02681", uchar = "\9857", latex = "", unicodemath = "\\diceii", cls = "N", category = "mathord", requirements = "", comments = "DIE FACE-2"}
  , Record {point = "02682", uchar = "\9858", latex = "", unicodemath = "\\diceiii", cls = "N", category = "mathord", requirements = "", comments = "DIE FACE-3"}
  , Record {point = "02683", uchar = "\9859", latex = "", unicodemath = "\\diceiv", cls = "N", category = "mathord", requirements = "", comments = "DIE FACE-4"}
  , Record {point = "02684", uchar = "\9860", latex = "", unicodemath = "\\dicev", cls = "N", category = "mathord", requirements = "", comments = "DIE FACE-5"}
  , Record {point = "02685", uchar = "\9861", latex = "", unicodemath = "\\dicevi", cls = "N", category = "mathord", requirements = "", comments = "DIE FACE-6"}
  , Record {point = "02686", uchar = "\9862", latex = "", unicodemath = "\\circledrightdot", cls = "N", category = "mathord", requirements = "", comments = "WHITE CIRCLE WITH DOT RIGHT"}
  , Record {point = "02687", uchar = "\9863", latex = "", unicodemath = "\\circledtwodots", cls = "N", category = "mathord", requirements = "", comments = "WHITE CIRCLE WITH TWO DOTS"}
  , Record {point = "02688", uchar = "\9864", latex = "", unicodemath = "\\blackcircledrightdot", cls = "N", category = "mathord", requirements = "", comments = "BLACK CIRCLE WITH WHITE DOT RIGHT"}
  , Record {point = "02689", uchar = "\9865", latex = "", unicodemath = "\\blackcircledtwodots", cls = "N", category = "mathord", requirements = "", comments = "BLACK CIRCLE WITH TWO WHITE DOTS"}
  , Record {point = "02693", uchar = "\9875", latex = "\\anchor", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "ANCHOR"}
  , Record {point = "02694", uchar = "\9876", latex = "\\swords", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "CROSSED SWORDS"}
  , Record {point = "026A0", uchar = "\9888", latex = "\\warning", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "WARNING SIGN"}
  , Record {point = "026A5", uchar = "\9893", latex = "", unicodemath = "\\Hermaphrodite", cls = "", category = "mathord", requirements = "", comments = "MALE AND FEMALE SIGN"}
  , Record {point = "026AA", uchar = "\9898", latex = "\\medcirc", unicodemath = "\\mdwhtcircle", cls = "N", category = "mathord", requirements = "txfonts", comments = "MEDIUM WHITE CIRCLE"}
  , Record {point = "026AB", uchar = "\9899", latex = "\\medbullet", unicodemath = "\\mdblkcircle", cls = "N", category = "mathord", requirements = "txfonts", comments = "MEDIUM BLACK CIRCLE"}
  , Record {point = "026AC", uchar = "\9900", latex = "", unicodemath = "\\mdsmwhtcircle", cls = "N", category = "mathord", requirements = "", comments = "MEDIUM SMALL WHITE CIRCLE"}
  , Record {point = "026B2", uchar = "\9906", latex = "", unicodemath = "\\neuter", cls = "N", category = "mathord", requirements = "", comments = "NEUTER"}
  , Record {point = "0270E", uchar = "\9998", latex = "\\pencil", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "LOWER RIGHT PENCIL"}
  , Record {point = "02713", uchar = "\10003", latex = "\\checkmark", unicodemath = "\\checkmark", cls = "N", category = "mathord", requirements = "amsfonts", comments = "= \\ballotcheck (arevmath), tick, CHECK MARK"}
  , Record {point = "02717", uchar = "\10007", latex = "\\ballotx", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "BALLOT X"}
  , Record {point = "02720", uchar = "\10016", latex = "\\maltese", unicodemath = "\\maltese", cls = "N", category = "mathord", requirements = "amsfonts", comments = "MALTESE CROSS"}
  , Record {point = "0272A", uchar = "\10026", latex = "", unicodemath = "\\circledstar", cls = "N", category = "mathord", requirements = "", comments = "CIRCLED WHITE STAR"}
  , Record {point = "02736", uchar = "\10038", latex = "", unicodemath = "\\varstar", cls = "N", category = "mathord", requirements = "", comments = "SIX POINTED BLACK STAR"}
  , Record {point = "0273D", uchar = "\10045", latex = "", unicodemath = "\\dingasterisk", cls = "", category = "mathord", requirements = "", comments = "HEAVY TEARDROP-SPOKED ASTERISK"}
  , Record {point = "02772", uchar = "\10098", latex = "", unicodemath = "\\lbrbrak", cls = "O", category = "mathopen", requirements = "", comments = "LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT"}
  , Record {point = "02773", uchar = "\10099", latex = "", unicodemath = "\\rbrbrak", cls = "C", category = "mathclose", requirements = "", comments = "LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT"}
  , Record {point = "0279B", uchar = "\10139", latex = "", unicodemath = "\\draftingarrow", cls = "", category = "mathord", requirements = "", comments = "right arrow with bold head (drafting)"}
  , Record {point = "027A2", uchar = "\10146", latex = "\\arrowbullet", unicodemath = "", cls = "", category = "mathord", requirements = "arevmath", comments = "THREE-D TOP-LIGHTED RIGHTWARDS ARROWHEAD"}
  , Record {point = "027C0", uchar = "\10176", latex = "", unicodemath = "\\threedangle", cls = "N", category = "mathord", requirements = "", comments = "THREE DIMENSIONAL ANGLE"}
  , Record {point = "027C1", uchar = "\10177", latex = "", unicodemath = "\\whiteinwhitetriangle", cls = "N", category = "mathord", requirements = "", comments = "WHITE TRIANGLE CONTAINING SMALL WHITE TRIANGLE"}
  , Record {point = "027C2", uchar = "\10178", latex = "\\perp", unicodemath = "\\perp", cls = "R", category = "mathrel", requirements = "", comments = "PERPENDICULAR"}
  , Record {point = "027C3", uchar = "\10179", latex = "", unicodemath = "\\subsetcirc", cls = "R", category = "mathord", requirements = "", comments = "OPEN SUBSET"}
  , Record {point = "027C4", uchar = "\10180", latex = "", unicodemath = "\\supsetcirc", cls = "R", category = "mathord", requirements = "", comments = "OPEN SUPERSET"}
  , Record {point = "027C5", uchar = "\10181", latex = "\\Lbag", unicodemath = "\\lbag", cls = "R", category = "mathopen", requirements = "stmaryrd txfonts", comments = "= \\lbag (stmaryrd -oz), LEFT S-SHAPED BAG DELIMITER"}
  , Record {point = "027C6", uchar = "\10182", latex = "\\Rbag", unicodemath = "\\rbag", cls = "R", category = "mathclose", requirements = "stmaryrd txfonts", comments = "= \\rbag (stmaryrd -oz), RIGHT S-SHAPED BAG DELIMITER"}
  , Record {point = "027C7", uchar = "\10183", latex = "", unicodemath = "\\veedot", cls = "R", category = "mathbin", requirements = "", comments = "OR WITH DOT INSIDE"}
  , Record {point = "027C8", uchar = "\10184", latex = "", unicodemath = "\\bsolhsub", cls = "R", category = "mathrel", requirements = "", comments = "REVERSE SOLIDUS PRECEDING SUBSET"}
  , Record {point = "027C9", uchar = "\10185", latex = "", unicodemath = "\\suphsol", cls = "R", category = "mathrel", requirements = "", comments = "SUPERSET PRECEDING SOLIDUS"}
  , Record {point = "027CC", uchar = "\10188", latex = "", unicodemath = "\\longdivision", cls = "", category = "mathopen", requirements = "", comments = "LONG DIVISION"}
  , Record {point = "027D0", uchar = "\10192", latex = "\\Diamonddot", unicodemath = "\\diamondcdot", cls = "N", category = "mathord", requirements = "txfonts", comments = "WHITE DIAMOND WITH CENTRED DOT"}
  , Record {point = "027D1", uchar = "\10193", latex = "", unicodemath = "\\wedgedot", cls = "B", category = "mathbin", requirements = "", comments = "AND WITH DOT"}
  , Record {point = "027D2", uchar = "\10194", latex = "", unicodemath = "\\upin", cls = "R", category = "mathrel", requirements = "", comments = "ELEMENT OF OPENING UPWARDS"}
  , Record {point = "027D3", uchar = "\10195", latex = "", unicodemath = "\\pullback", cls = "R", category = "mathrel", requirements = "", comments = "LOWER RIGHT CORNER WITH DOT"}
  , Record {point = "027D4", uchar = "\10196", latex = "", unicodemath = "\\pushout", cls = "R", category = "mathrel", requirements = "", comments = "UPPER LEFT CORNER WITH DOT"}
  , Record {point = "027D5", uchar = "\10197", latex = "", unicodemath = "\\leftouterjoin", cls = "L", category = "mathop", requirements = "", comments = "LEFT OUTER JOIN"}
  , Record {point = "027D6", uchar = "\10198", latex = "", unicodemath = "\\rightouterjoin", cls = "L", category = "mathop", requirements = "", comments = "RIGHT OUTER JOIN"}
  , Record {point = "027D7", uchar = "\10199", latex = "", unicodemath = "\\fullouterjoin", cls = "L", category = "mathop", requirements = "", comments = "FULL OUTER JOIN"}
  , Record {point = "027D8", uchar = "\10200", latex = "", unicodemath = "\\bigbot", cls = "L", category = "mathop", requirements = "", comments = "LARGE UP TACK"}
  , Record {point = "027D9", uchar = "\10201", latex = "", unicodemath = "\\bigtop", cls = "L", category = "mathop", requirements = "", comments = "LARGE DOWN TACK"}
  , Record {point = "027DA", uchar = "\10202", latex = "", unicodemath = "\\DashVDash", cls = "R", category = "mathrel", requirements = "", comments = "LEFT AND RIGHT DOUBLE TURNSTILE"}
  , Record {point = "027DB", uchar = "\10203", latex = "", unicodemath = "\\dashVdash", cls = "R", category = "mathrel", requirements = "", comments = "LEFT AND RIGHT TACK"}
  , Record {point = "027DC", uchar = "\10204", latex = "\\multimapinv", unicodemath = "\\multimapinv", cls = "R", category = "mathrel", requirements = "txfonts", comments = "LEFT MULTIMAP"}
  , Record {point = "027DD", uchar = "\10205", latex = "", unicodemath = "\\vlongdash", cls = "R", category = "mathrel", requirements = "", comments = "long left tack"}
  , Record {point = "027DE", uchar = "\10206", latex = "", unicodemath = "\\longdashv", cls = "R", category = "mathrel", requirements = "", comments = "long right tack"}
  , Record {point = "027DF", uchar = "\10207", latex = "", unicodemath = "\\cirbot", cls = "R", category = "mathrel", requirements = "", comments = "UP TACK WITH CIRCLE ABOVE"}
  , Record {point = "027E0", uchar = "\10208", latex = "", unicodemath = "\\lozengeminus", cls = "B", category = "mathbin", requirements = "", comments = "LOZENGE DIVIDED BY HORIZONTAL RULE"}
  , Record {point = "027E1", uchar = "\10209", latex = "", unicodemath = "\\concavediamond", cls = "B", category = "mathbin", requirements = "", comments = "WHITE CONCAVE-SIDED DIAMOND"}
  , Record {point = "027E2", uchar = "\10210", latex = "", unicodemath = "\\concavediamondtickleft", cls = "B", category = "mathbin", requirements = "", comments = "WHITE CONCAVE-SIDED DIAMOND WITH LEFTWARDS TICK"}
  , Record {point = "027E3", uchar = "\10211", latex = "", unicodemath = "\\concavediamondtickright", cls = "B", category = "mathbin", requirements = "", comments = "WHITE CONCAVE-SIDED DIAMOND WITH RIGHTWARDS TICK"}
  , Record {point = "027E4", uchar = "\10212", latex = "", unicodemath = "\\whitesquaretickleft", cls = "B", category = "mathbin", requirements = "", comments = "WHITE SQUARE WITH LEFTWARDS TICK"}
  , Record {point = "027E5", uchar = "\10213", latex = "", unicodemath = "\\whitesquaretickright", cls = "B", category = "mathbin", requirements = "", comments = "WHITE SQUARE WITH RIGHTWARDS TICK"}
  , Record {point = "027E6", uchar = "\10214", latex = "\\llbracket", unicodemath = "\\lBrack", cls = "O", category = "mathopen", requirements = "stmaryrd wrisym kpfonts fourier", comments = "= \\Lbrack (mathbbol), = \\lbag (oz -stmaryrd), MATHEMATICAL LEFT WHITE SQUARE BRACKET"}
  , Record {point = "027E7", uchar = "\10215", latex = "\\rrbracket", unicodemath = "\\rBrack", cls = "C", category = "mathclose", requirements = "stmaryrd wrisym kpfonts fourier", comments = "= \\Rbrack (mathbbol), = \\rbag (oz -stmaryrd), MATHEMATICAL RIGHT WHITE SQUARE BRACKET"}
  , Record {point = "027E8", uchar = "\10216", latex = "\\langle", unicodemath = "\\langle", cls = "O", category = "mathopen", requirements = "", comments = "MATHEMATICAL LEFT ANGLE BRACKET"}
  , Record {point = "027E9", uchar = "\10217", latex = "\\rangle", unicodemath = "\\rangle", cls = "C", category = "mathclose", requirements = "", comments = "MATHEMATICAL RIGHT ANGLE BRACKET"}
  , Record {point = "027EA", uchar = "\10218", latex = "\\lang", unicodemath = "\\lAngle", cls = "O", category = "mathopen", requirements = "oz", comments = "MATHEMATICAL LEFT DOUBLE ANGLE BRACKET, z notation left chevron bracket"}
  , Record {point = "027EB", uchar = "\10219", latex = "\\rang", unicodemath = "\\rAngle", cls = "C", category = "mathclose", requirements = "oz", comments = "MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET, z notation right chevron bracket"}
  , Record {point = "027EC", uchar = "\10220", latex = "", unicodemath = "\\Lbrbrak", cls = "O", category = "mathopen", requirements = "", comments = "MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET"}
  , Record {point = "027ED", uchar = "\10221", latex = "", unicodemath = "\\Rbrbrak", cls = "C", category = "mathclose", requirements = "", comments = "MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET"}
  , Record {point = "027EE", uchar = "\10222", latex = "\\lgroup", unicodemath = "", cls = "O", category = "mathopen", requirements = "", comments = "MATHEMATICAL LEFT FLATTENED PARENTHESIS"}
  , Record {point = "027EF", uchar = "\10223", latex = "\\rgroup", unicodemath = "", cls = "C", category = "mathclose", requirements = "", comments = "MATHEMATICAL RIGHT FLATTENED PARENTHESIS"}
  , Record {point = "027F0", uchar = "\10224", latex = "", unicodemath = "\\UUparrow", cls = "R", category = "mathrel", requirements = "", comments = "UPWARDS QUADRUPLE ARROW"}
  , Record {point = "027F1", uchar = "\10225", latex = "", unicodemath = "\\DDownarrow", cls = "R", category = "mathrel", requirements = "", comments = "DOWNWARDS QUADRUPLE ARROW"}
  , Record {point = "027F2", uchar = "\10226", latex = "", unicodemath = "\\acwgapcirclearrow", cls = "R", category = "mathrel", requirements = "", comments = "ANTICLOCKWISE GAPPED CIRCLE ARROW"}
  , Record {point = "027F3", uchar = "\10227", latex = "", unicodemath = "\\cwgapcirclearrow", cls = "R", category = "mathrel", requirements = "", comments = "CLOCKWISE GAPPED CIRCLE ARROW"}
  , Record {point = "027F4", uchar = "\10228", latex = "", unicodemath = "\\rightarrowonoplus", cls = "R", category = "mathrel", requirements = "", comments = "RIGHT ARROW WITH CIRCLED PLUS"}
  , Record {point = "027F5", uchar = "\10229", latex = "\\longleftarrow", unicodemath = "\\longleftarrow", cls = "R", category = "mathrel", requirements = "", comments = "LONG LEFTWARDS ARROW"}
  , Record {point = "027F6", uchar = "\10230", latex = "\\longrightarrow", unicodemath = "\\longrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "LONG RIGHTWARDS ARROW"}
  , Record {point = "027F7", uchar = "\10231", latex = "\\longleftrightarrow", unicodemath = "\\longleftrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "LONG LEFT RIGHT ARROW"}
  , Record {point = "027F8", uchar = "\10232", latex = "\\Longleftarrow", unicodemath = "\\Longleftarrow", cls = "R", category = "mathrel", requirements = "", comments = "= \\impliedby (amsmath), LONG LEFTWARDS DOUBLE ARROW"}
  , Record {point = "027F9", uchar = "\10233", latex = "\\Longrightarrow", unicodemath = "\\Longrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "= \\implies (amsmath), LONG RIGHTWARDS DOUBLE ARROW"}
  , Record {point = "027FA", uchar = "\10234", latex = "\\Longleftrightarrow", unicodemath = "\\Longleftrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "= \\iff (oz), LONG LEFT RIGHT DOUBLE ARROW"}
  , Record {point = "027FB", uchar = "\10235", latex = "\\longmapsfrom", unicodemath = "\\longmapsfrom", cls = "R", category = "mathrel", requirements = "stmaryrd", comments = "= \\longmappedfrom (kpfonts), LONG LEFTWARDS ARROW FROM BAR"}
  , Record {point = "027FC", uchar = "\10236", latex = "\\longmapsto", unicodemath = "\\longmapsto", cls = "R", category = "mathrel", requirements = "", comments = "LONG RIGHTWARDS ARROW FROM BAR"}
  , Record {point = "027FD", uchar = "\10237", latex = "\\Longmapsfrom", unicodemath = "\\Longmapsfrom", cls = "R", category = "mathrel", requirements = "stmaryrd", comments = "= \\Longmappedfrom (kpfonts), LONG LEFTWARDS DOUBLE ARROW FROM BAR"}
  , Record {point = "027FE", uchar = "\10238", latex = "\\Longmapsto", unicodemath = "\\Longmapsto", cls = "R", category = "mathrel", requirements = "stmaryrd", comments = "LONG RIGHTWARDS DOUBLE ARROW FROM BAR"}
  , Record {point = "027FF", uchar = "\10239", latex = "", unicodemath = "\\longrightsquigarrow", cls = "R", category = "mathrel", requirements = "", comments = "LONG RIGHTWARDS SQUIGGLE ARROW"}
  , Record {point = "02900", uchar = "\10496", latex = "\\psur", unicodemath = "\\nvtwoheadrightarrow", cls = "R", category = "mathrel", requirements = "oz", comments = "= \\psurj (oz), RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE, z notation partial surjection"}
  , Record {point = "02901", uchar = "\10497", latex = "", unicodemath = "\\nVtwoheadrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS TWO-HEADED ARROW WITH DOUBLE VERTICAL STROKE, z notation finite surjection"}
  , Record {point = "02902", uchar = "\10498", latex = "", unicodemath = "\\nvLeftarrow", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS DOUBLE ARROW WITH VERTICAL STROKE"}
  , Record {point = "02903", uchar = "\10499", latex = "", unicodemath = "\\nvRightarrow", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS DOUBLE ARROW WITH VERTICAL STROKE"}
  , Record {point = "02904", uchar = "\10500", latex = "", unicodemath = "\\nvLeftrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE"}
  , Record {point = "02905", uchar = "\10501", latex = "", unicodemath = "\\twoheadmapsto", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS TWO-HEADED ARROW FROM BAR"}
  , Record {point = "02906", uchar = "\10502", latex = "\\Mapsfrom", unicodemath = "\\Mapsfrom", cls = "R", category = "mathrel", requirements = "stmaryrd", comments = "= \\Mappedfrom (kpfonts), LEFTWARDS DOUBLE ARROW FROM BAR"}
  , Record {point = "02907", uchar = "\10503", latex = "\\Mapsto", unicodemath = "\\Mapsto", cls = "R", category = "mathrel", requirements = "stmaryrd", comments = "RIGHTWARDS DOUBLE ARROW FROM BAR"}
  , Record {point = "02908", uchar = "\10504", latex = "", unicodemath = "\\downarrowbarred", cls = "R", category = "mathrel", requirements = "", comments = "DOWNWARDS ARROW WITH HORIZONTAL STROKE"}
  , Record {point = "02909", uchar = "\10505", latex = "", unicodemath = "\\uparrowbarred", cls = "R", category = "mathrel", requirements = "", comments = "UPWARDS ARROW WITH HORIZONTAL STROKE"}
  , Record {point = "0290A", uchar = "\10506", latex = "", unicodemath = "\\Uuparrow", cls = "R", category = "mathrel", requirements = "", comments = "UPWARDS TRIPLE ARROW"}
  , Record {point = "0290B", uchar = "\10507", latex = "", unicodemath = "\\Ddownarrow", cls = "R", category = "mathrel", requirements = "", comments = "DOWNWARDS TRIPLE ARROW"}
  , Record {point = "0290C", uchar = "\10508", latex = "", unicodemath = "\\leftbkarrow", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS DOUBLE DASH ARROW"}
  , Record {point = "0290D", uchar = "\10509", latex = "", unicodemath = "\\rightbkarrow", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS DOUBLE DASH ARROW"}
  , Record {point = "0290E", uchar = "\10510", latex = "", unicodemath = "\\leftdbkarrow", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS TRIPLE DASH ARROW"}
  , Record {point = "0290F", uchar = "\10511", latex = "", unicodemath = "\\dbkarow", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS TRIPLE DASH ARROW"}
  , Record {point = "02910", uchar = "\10512", latex = "", unicodemath = "\\drbkarow", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS TWO-HEADED TRIPLE DASH ARROW"}
  , Record {point = "02911", uchar = "\10513", latex = "", unicodemath = "\\rightdotarrow", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS ARROW WITH DOTTED STEM"}
  , Record {point = "02912", uchar = "\10514", latex = "\\UpArrowBar", unicodemath = "\\baruparrow", cls = "R", category = "mathrel", requirements = "wrisym", comments = "UPWARDS ARROW TO BAR"}
  , Record {point = "02913", uchar = "\10515", latex = "\\DownArrowBar", unicodemath = "\\downarrowbar", cls = "R", category = "mathrel", requirements = "wrisym", comments = "DOWNWARDS ARROW TO BAR"}
  , Record {point = "02914", uchar = "\10516", latex = "\\pinj", unicodemath = "\\nvrightarrowtail", cls = "R", category = "mathrel", requirements = "oz", comments = "RIGHTWARDS ARROW WITH TAIL WITH VERTICAL STROKE, z notation partial injection"}
  , Record {point = "02915", uchar = "\10517", latex = "\\finj", unicodemath = "\\nVrightarrowtail", cls = "R", category = "mathrel", requirements = "oz", comments = "RIGHTWARDS ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE, z notation finite injection"}
  , Record {point = "02916", uchar = "\10518", latex = "\\bij", unicodemath = "\\twoheadrightarrowtail", cls = "R", category = "mathrel", requirements = "oz", comments = "RIGHTWARDS TWO-HEADED ARROW WITH TAIL, z notation bijection"}
  , Record {point = "02917", uchar = "\10519", latex = "", unicodemath = "\\nvtwoheadrightarrowtail", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS TWO-HEADED ARROW WITH TAIL WITH VERTICAL STROKE, z notation surjective injection"}
  , Record {point = "02918", uchar = "\10520", latex = "", unicodemath = "\\nVtwoheadrightarrowtail", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS TWO-HEADED ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE, z notation finite surjective injection"}
  , Record {point = "02919", uchar = "\10521", latex = "", unicodemath = "\\lefttail", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW-TAIL"}
  , Record {point = "0291A", uchar = "\10522", latex = "", unicodemath = "\\righttail", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS ARROW-TAIL"}
  , Record {point = "0291B", uchar = "\10523", latex = "", unicodemath = "\\leftdbltail", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS DOUBLE ARROW-TAIL"}
  , Record {point = "0291C", uchar = "\10524", latex = "", unicodemath = "\\rightdbltail", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS DOUBLE ARROW-TAIL"}
  , Record {point = "0291D", uchar = "\10525", latex = "", unicodemath = "\\diamondleftarrow", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW TO BLACK DIAMOND"}
  , Record {point = "0291E", uchar = "\10526", latex = "", unicodemath = "\\rightarrowdiamond", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS ARROW TO BLACK DIAMOND"}
  , Record {point = "0291F", uchar = "\10527", latex = "", unicodemath = "\\diamondleftarrowbar", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW FROM BAR TO BLACK DIAMOND"}
  , Record {point = "02920", uchar = "\10528", latex = "", unicodemath = "\\barrightarrowdiamond", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS ARROW FROM BAR TO BLACK DIAMOND"}
  , Record {point = "02921", uchar = "\10529", latex = "", unicodemath = "\\nwsearrow", cls = "R", category = "mathrel", requirements = "", comments = "NORTH WEST AND SOUTH EAST ARROW"}
  , Record {point = "02922", uchar = "\10530", latex = "", unicodemath = "\\neswarrow", cls = "R", category = "mathrel", requirements = "", comments = "NORTH EAST AND SOUTH WEST ARROW"}
  , Record {point = "02923", uchar = "\10531", latex = "", unicodemath = "\\hknwarrow", cls = "R", category = "mathrel", requirements = "", comments = "NORTH WEST ARROW WITH HOOK"}
  , Record {point = "02924", uchar = "\10532", latex = "", unicodemath = "\\hknearrow", cls = "R", category = "mathrel", requirements = "", comments = "NORTH EAST ARROW WITH HOOK"}
  , Record {point = "02925", uchar = "\10533", latex = "", unicodemath = "\\hksearow", cls = "R", category = "mathrel", requirements = "", comments = "SOUTH EAST ARROW WITH HOOK"}
  , Record {point = "02926", uchar = "\10534", latex = "", unicodemath = "\\hkswarow", cls = "R", category = "mathrel", requirements = "", comments = "SOUTH WEST ARROW WITH HOOK"}
  , Record {point = "02927", uchar = "\10535", latex = "", unicodemath = "\\tona", cls = "R", category = "mathrel", requirements = "", comments = "NORTH WEST ARROW AND NORTH EAST ARROW"}
  , Record {point = "02928", uchar = "\10536", latex = "", unicodemath = "\\toea", cls = "R", category = "mathrel", requirements = "", comments = "NORTH EAST ARROW AND SOUTH EAST ARROW"}
  , Record {point = "02929", uchar = "\10537", latex = "", unicodemath = "\\tosa", cls = "R", category = "mathrel", requirements = "", comments = "SOUTH EAST ARROW AND SOUTH WEST ARROW"}
  , Record {point = "0292A", uchar = "\10538", latex = "", unicodemath = "\\towa", cls = "R", category = "mathrel", requirements = "", comments = "SOUTH WEST ARROW AND NORTH WEST ARROW"}
  , Record {point = "0292B", uchar = "\10539", latex = "", unicodemath = "\\rdiagovfdiag", cls = "R", category = "mathord", requirements = "", comments = "RISING DIAGONAL CROSSING FALLING DIAGONAL"}
  , Record {point = "0292C", uchar = "\10540", latex = "", unicodemath = "\\fdiagovrdiag", cls = "R", category = "mathord", requirements = "", comments = "FALLING DIAGONAL CROSSING RISING DIAGONAL"}
  , Record {point = "0292D", uchar = "\10541", latex = "", unicodemath = "\\seovnearrow", cls = "R", category = "mathord", requirements = "", comments = "SOUTH EAST ARROW CROSSING NORTH EAST ARROW"}
  , Record {point = "0292E", uchar = "\10542", latex = "", unicodemath = "\\neovsearrow", cls = "R", category = "mathord", requirements = "", comments = "NORTH EAST ARROW CROSSING SOUTH EAST ARROW"}
  , Record {point = "0292F", uchar = "\10543", latex = "", unicodemath = "\\fdiagovnearrow", cls = "R", category = "mathord", requirements = "", comments = "FALLING DIAGONAL CROSSING NORTH EAST ARROW"}
  , Record {point = "02930", uchar = "\10544", latex = "", unicodemath = "\\rdiagovsearrow", cls = "R", category = "mathord", requirements = "", comments = "RISING DIAGONAL CROSSING SOUTH EAST ARROW"}
  , Record {point = "02931", uchar = "\10545", latex = "", unicodemath = "\\neovnwarrow", cls = "R", category = "mathord", requirements = "", comments = "NORTH EAST ARROW CROSSING NORTH WEST ARROW"}
  , Record {point = "02932", uchar = "\10546", latex = "", unicodemath = "\\nwovnearrow", cls = "R", category = "mathord", requirements = "", comments = "NORTH WEST ARROW CROSSING NORTH EAST ARROW"}
  , Record {point = "02933", uchar = "\10547", latex = "\\leadsto", unicodemath = "\\rightcurvedarrow", cls = "R", category = "mathrel", requirements = "txfonts", comments = "WAVE ARROW POINTING DIRECTLY RIGHT"}
  , Record {point = "02934", uchar = "\10548", latex = "", unicodemath = "\\uprightcurvearrow", cls = "R", category = "mathord", requirements = "", comments = "ARROW POINTING RIGHTWARDS THEN CURVING UPWARDS"}
  , Record {point = "02935", uchar = "\10549", latex = "", unicodemath = "\\downrightcurvedarrow", cls = "R", category = "mathord", requirements = "", comments = "ARROW POINTING RIGHTWARDS THEN CURVING DOWNWARDS"}
  , Record {point = "02936", uchar = "\10550", latex = "", unicodemath = "\\leftdowncurvedarrow", cls = "R", category = "mathrel", requirements = "", comments = "ARROW POINTING DOWNWARDS THEN CURVING LEFTWARDS"}
  , Record {point = "02937", uchar = "\10551", latex = "", unicodemath = "\\rightdowncurvedarrow", cls = "R", category = "mathrel", requirements = "", comments = "ARROW POINTING DOWNWARDS THEN CURVING RIGHTWARDS"}
  , Record {point = "02938", uchar = "\10552", latex = "", unicodemath = "\\cwrightarcarrow", cls = "R", category = "mathrel", requirements = "", comments = "RIGHT-SIDE ARC CLOCKWISE ARROW"}
  , Record {point = "02939", uchar = "\10553", latex = "", unicodemath = "\\acwleftarcarrow", cls = "R", category = "mathrel", requirements = "", comments = "LEFT-SIDE ARC ANTICLOCKWISE ARROW"}
  , Record {point = "0293A", uchar = "\10554", latex = "", unicodemath = "\\acwoverarcarrow", cls = "R", category = "mathrel", requirements = "", comments = "TOP ARC ANTICLOCKWISE ARROW"}
  , Record {point = "0293B", uchar = "\10555", latex = "", unicodemath = "\\acwunderarcarrow", cls = "R", category = "mathrel", requirements = "", comments = "BOTTOM ARC ANTICLOCKWISE ARROW"}
  , Record {point = "0293C", uchar = "\10556", latex = "", unicodemath = "\\curvearrowrightminus", cls = "R", category = "mathrel", requirements = "", comments = "TOP ARC CLOCKWISE ARROW WITH MINUS"}
  , Record {point = "0293D", uchar = "\10557", latex = "", unicodemath = "\\curvearrowleftplus", cls = "R", category = "mathrel", requirements = "", comments = "TOP ARC ANTICLOCKWISE ARROW WITH PLUS"}
  , Record {point = "0293E", uchar = "\10558", latex = "", unicodemath = "\\cwundercurvearrow", cls = "R", category = "mathrel", requirements = "", comments = "LOWER RIGHT SEMICIRCULAR CLOCKWISE ARROW"}
  , Record {point = "0293F", uchar = "\10559", latex = "", unicodemath = "\\ccwundercurvearrow", cls = "R", category = "mathrel", requirements = "", comments = "LOWER LEFT SEMICIRCULAR ANTICLOCKWISE ARROW"}
  , Record {point = "02940", uchar = "\10560", latex = "", unicodemath = "\\acwcirclearrow", cls = "R", category = "mathrel", requirements = "", comments = "ANTICLOCKWISE CLOSED CIRCLE ARROW"}
  , Record {point = "02941", uchar = "\10561", latex = "", unicodemath = "\\cwcirclearrow", cls = "R", category = "mathrel", requirements = "", comments = "CLOCKWISE CLOSED CIRCLE ARROW"}
  , Record {point = "02942", uchar = "\10562", latex = "", unicodemath = "\\rightarrowshortleftarrow", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS ARROW ABOVE SHORT LEFTWARDS ARROW"}
  , Record {point = "02943", uchar = "\10563", latex = "", unicodemath = "\\leftarrowshortrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW ABOVE SHORT RIGHTWARDS ARROW"}
  , Record {point = "02944", uchar = "\10564", latex = "", unicodemath = "\\shortrightarrowleftarrow", cls = "R", category = "mathrel", requirements = "", comments = "SHORT RIGHTWARDS ARROW ABOVE LEFTWARDS ARROW"}
  , Record {point = "02945", uchar = "\10565", latex = "", unicodemath = "\\rightarrowplus", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS ARROW WITH PLUS BELOW"}
  , Record {point = "02946", uchar = "\10566", latex = "", unicodemath = "\\leftarrowplus", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW WITH PLUS BELOW"}
  , Record {point = "02947", uchar = "\10567", latex = "", unicodemath = "\\rightarrowx", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS ARROW THROUGH X"}
  , Record {point = "02948", uchar = "\10568", latex = "", unicodemath = "\\leftrightarrowcircle", cls = "R", category = "mathrel", requirements = "", comments = "LEFT RIGHT ARROW THROUGH SMALL CIRCLE"}
  , Record {point = "02949", uchar = "\10569", latex = "", unicodemath = "\\twoheaduparrowcircle", cls = "R", category = "mathrel", requirements = "", comments = "UPWARDS TWO-HEADED ARROW FROM SMALL CIRCLE"}
  , Record {point = "0294A", uchar = "\10570", latex = "\\leftrightharpoon", unicodemath = "\\leftrightharpoonupdown", cls = "R", category = "mathrel", requirements = "mathabx", comments = "LEFT BARB UP RIGHT BARB DOWN HARPOON"}
  , Record {point = "0294B", uchar = "\10571", latex = "\\rightleftharpoon", unicodemath = "\\leftrightharpoondownup", cls = "R", category = "mathrel", requirements = "mathabx", comments = "LEFT BARB DOWN RIGHT BARB UP HARPOON"}
  , Record {point = "0294C", uchar = "\10572", latex = "", unicodemath = "\\updownharpoonrightleft", cls = "R", category = "mathrel", requirements = "", comments = "UP BARB RIGHT DOWN BARB LEFT HARPOON"}
  , Record {point = "0294D", uchar = "\10573", latex = "", unicodemath = "\\updownharpoonleftright", cls = "R", category = "mathrel", requirements = "", comments = "UP BARB LEFT DOWN BARB RIGHT HARPOON"}
  , Record {point = "0294E", uchar = "\10574", latex = "\\leftrightharpoonup", unicodemath = "\\leftrightharpoonupup", cls = "R", category = "mathrel", requirements = "wrisym", comments = "LEFT BARB UP RIGHT BARB UP HARPOON"}
  , Record {point = "0294F", uchar = "\10575", latex = "\\rightupdownharpoon", unicodemath = "\\updownharpoonrightright", cls = "R", category = "mathrel", requirements = "wrisym", comments = "UP BARB RIGHT DOWN BARB RIGHT HARPOON"}
  , Record {point = "02950", uchar = "\10576", latex = "\\leftrightharpoondown", unicodemath = "\\leftrightharpoondowndown", cls = "R", category = "mathrel", requirements = "wrisym", comments = "LEFT BARB DOWN RIGHT BARB DOWN HARPOON"}
  , Record {point = "02951", uchar = "\10577", latex = "\\leftupdownharpoon", unicodemath = "\\updownharpoonleftleft", cls = "R", category = "mathrel", requirements = "wrisym", comments = "UP BARB LEFT DOWN BARB LEFT HARPOON"}
  , Record {point = "02952", uchar = "\10578", latex = "\\LeftVectorBar", unicodemath = "\\barleftharpoonup", cls = "R", category = "mathrel", requirements = "wrisym", comments = "LEFTWARDS HARPOON WITH BARB UP TO BAR"}
  , Record {point = "02953", uchar = "\10579", latex = "\\RightVectorBar", unicodemath = "\\rightharpoonupbar", cls = "R", category = "mathrel", requirements = "wrisym", comments = "RIGHTWARDS HARPOON WITH BARB UP TO BAR"}
  , Record {point = "02954", uchar = "\10580", latex = "\\RightUpVectorBar", unicodemath = "\\barupharpoonright", cls = "R", category = "mathrel", requirements = "wrisym", comments = "UPWARDS HARPOON WITH BARB RIGHT TO BAR"}
  , Record {point = "02955", uchar = "\10581", latex = "\\RightDownVectorBar", unicodemath = "\\downharpoonrightbar", cls = "R", category = "mathrel", requirements = "wrisym", comments = "DOWNWARDS HARPOON WITH BARB RIGHT TO BAR"}
  , Record {point = "02956", uchar = "\10582", latex = "\\DownLeftVectorBar", unicodemath = "\\barleftharpoondown", cls = "R", category = "mathrel", requirements = "wrisym", comments = "LEFTWARDS HARPOON WITH BARB DOWN TO BAR"}
  , Record {point = "02957", uchar = "\10583", latex = "\\DownRightVectorBar", unicodemath = "\\rightharpoondownbar", cls = "R", category = "mathrel", requirements = "wrisym", comments = "RIGHTWARDS HARPOON WITH BARB DOWN TO BAR"}
  , Record {point = "02958", uchar = "\10584", latex = "\\LeftUpVectorBar", unicodemath = "\\barupharpoonleft", cls = "R", category = "mathrel", requirements = "wrisym", comments = "UPWARDS HARPOON WITH BARB LEFT TO BAR"}
  , Record {point = "02959", uchar = "\10585", latex = "\\LeftDownVectorBar", unicodemath = "\\downharpoonleftbar", cls = "R", category = "mathrel", requirements = "wrisym", comments = "DOWNWARDS HARPOON WITH BARB LEFT TO BAR"}
  , Record {point = "0295A", uchar = "\10586", latex = "\\LeftTeeVector", unicodemath = "\\leftharpoonupbar", cls = "R", category = "mathrel", requirements = "wrisym", comments = "LEFTWARDS HARPOON WITH BARB UP FROM BAR"}
  , Record {point = "0295B", uchar = "\10587", latex = "\\RightTeeVector", unicodemath = "\\barrightharpoonup", cls = "R", category = "mathrel", requirements = "wrisym", comments = "RIGHTWARDS HARPOON WITH BARB UP FROM BAR"}
  , Record {point = "0295C", uchar = "\10588", latex = "\\RightUpTeeVector", unicodemath = "\\upharpoonrightbar", cls = "R", category = "mathrel", requirements = "wrisym", comments = "UPWARDS HARPOON WITH BARB RIGHT FROM BAR"}
  , Record {point = "0295D", uchar = "\10589", latex = "\\RightDownTeeVector", unicodemath = "\\bardownharpoonright", cls = "R", category = "mathrel", requirements = "wrisym", comments = "DOWNWARDS HARPOON WITH BARB RIGHT FROM BAR"}
  , Record {point = "0295E", uchar = "\10590", latex = "\\DownLeftTeeVector", unicodemath = "\\leftharpoondownbar", cls = "R", category = "mathrel", requirements = "wrisym", comments = "LEFTWARDS HARPOON WITH BARB DOWN FROM BAR"}
  , Record {point = "0295F", uchar = "\10591", latex = "\\DownRightTeeVector", unicodemath = "\\barrightharpoondown", cls = "R", category = "mathrel", requirements = "wrisym", comments = "RIGHTWARDS HARPOON WITH BARB DOWN FROM BAR"}
  , Record {point = "02960", uchar = "\10592", latex = "\\LeftUpTeeVector", unicodemath = "\\upharpoonleftbar", cls = "R", category = "mathrel", requirements = "wrisym", comments = "UPWARDS HARPOON WITH BARB LEFT FROM BAR"}
  , Record {point = "02961", uchar = "\10593", latex = "\\LeftDownTeeVector", unicodemath = "\\bardownharpoonleft", cls = "R", category = "mathrel", requirements = "wrisym", comments = "DOWNWARDS HARPOON WITH BARB LEFT FROM BAR"}
  , Record {point = "02962", uchar = "\10594", latex = "\\leftleftharpoons", unicodemath = "\\leftharpoonsupdown", cls = "R", category = "mathrel", requirements = "mathabx", comments = "LEFTWARDS HARPOON WITH BARB UP ABOVE LEFTWARDS HARPOON WITH BARB DOWN"}
  , Record {point = "02963", uchar = "\10595", latex = "\\upupharpoons", unicodemath = "\\upharpoonsleftright", cls = "R", category = "mathrel", requirements = "mathabx", comments = "UPWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT"}
  , Record {point = "02964", uchar = "\10596", latex = "\\rightrightharpoons", unicodemath = "\\rightharpoonsupdown", cls = "R", category = "mathrel", requirements = "mathabx", comments = "RIGHTWARDS HARPOON WITH BARB UP ABOVE RIGHTWARDS HARPOON WITH BARB DOWN"}
  , Record {point = "02965", uchar = "\10597", latex = "\\downdownharpoons", unicodemath = "\\downharpoonsleftright", cls = "R", category = "mathrel", requirements = "mathabx", comments = "DOWNWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT"}
  , Record {point = "02966", uchar = "\10598", latex = "", unicodemath = "\\leftrightharpoonsup", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS HARPOON WITH BARB UP ABOVE RIGHTWARDS HARPOON WITH BARB UP"}
  , Record {point = "02967", uchar = "\10599", latex = "", unicodemath = "\\leftrightharpoonsdown", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS HARPOON WITH BARB DOWN ABOVE RIGHTWARDS HARPOON WITH BARB DOWN"}
  , Record {point = "02968", uchar = "\10600", latex = "", unicodemath = "\\rightleftharpoonsup", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS HARPOON WITH BARB UP ABOVE LEFTWARDS HARPOON WITH BARB UP"}
  , Record {point = "02969", uchar = "\10601", latex = "", unicodemath = "\\rightleftharpoonsdown", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS HARPOON WITH BARB DOWN ABOVE LEFTWARDS HARPOON WITH BARB DOWN"}
  , Record {point = "0296A", uchar = "\10602", latex = "\\leftbarharpoon", unicodemath = "\\leftharpoonupdash", cls = "R", category = "mathrel", requirements = "mathabx", comments = "LEFTWARDS HARPOON WITH BARB UP ABOVE LONG DASH"}
  , Record {point = "0296B", uchar = "\10603", latex = "\\barleftharpoon", unicodemath = "\\dashleftharpoondown", cls = "R", category = "mathrel", requirements = "mathabx", comments = "LEFTWARDS HARPOON WITH BARB DOWN BELOW LONG DASH"}
  , Record {point = "0296C", uchar = "\10604", latex = "\\rightbarharpoon", unicodemath = "\\rightharpoonupdash", cls = "R", category = "mathrel", requirements = "mathabx", comments = "RIGHTWARDS HARPOON WITH BARB UP ABOVE LONG DASH"}
  , Record {point = "0296D", uchar = "\10605", latex = "\\barrightharpoon", unicodemath = "\\dashrightharpoondown", cls = "R", category = "mathrel", requirements = "mathabx", comments = "RIGHTWARDS HARPOON WITH BARB DOWN BELOW LONG DASH"}
  , Record {point = "0296E", uchar = "\10606", latex = "\\updownharpoons", unicodemath = "\\updownharpoonsleftright", cls = "R", category = "mathrel", requirements = "mathabx", comments = "= \\upequilibrium (wrisym), UPWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT"}
  , Record {point = "0296F", uchar = "\10607", latex = "\\downupharpoons", unicodemath = "\\downupharpoonsleftright", cls = "R", category = "mathrel", requirements = "mathabx", comments = "= \\uprevequilibrium (wrisym), DOWNWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT"}
  , Record {point = "02970", uchar = "\10608", latex = "", unicodemath = "\\rightimply", cls = "R", category = "mathrel", requirements = "", comments = "RIGHT DOUBLE ARROW WITH ROUNDED HEAD"}
  , Record {point = "02971", uchar = "\10609", latex = "", unicodemath = "\\equalrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "EQUALS SIGN ABOVE RIGHTWARDS ARROW"}
  , Record {point = "02972", uchar = "\10610", latex = "", unicodemath = "\\similarrightarrow", cls = "R", category = "mathrel", requirements = "", comments = "TILDE OPERATOR ABOVE RIGHTWARDS ARROW"}
  , Record {point = "02973", uchar = "\10611", latex = "", unicodemath = "\\leftarrowsimilar", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW ABOVE TILDE OPERATOR"}
  , Record {point = "02974", uchar = "\10612", latex = "", unicodemath = "\\rightarrowsimilar", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS ARROW ABOVE TILDE OPERATOR"}
  , Record {point = "02975", uchar = "\10613", latex = "", unicodemath = "\\rightarrowapprox", cls = "R", category = "mathrel", requirements = "", comments = "RIGHTWARDS ARROW ABOVE ALMOST EQUAL TO"}
  , Record {point = "02976", uchar = "\10614", latex = "", unicodemath = "\\ltlarr", cls = "R", category = "mathrel", requirements = "", comments = "LESS-THAN ABOVE LEFTWARDS ARROW"}
  , Record {point = "02977", uchar = "\10615", latex = "", unicodemath = "\\leftarrowless", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW THROUGH LESS-THAN"}
  , Record {point = "02978", uchar = "\10616", latex = "", unicodemath = "\\gtrarr", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN ABOVE RIGHTWARDS ARROW"}
  , Record {point = "02979", uchar = "\10617", latex = "", unicodemath = "\\subrarr", cls = "R", category = "mathrel", requirements = "", comments = "SUBSET ABOVE RIGHTWARDS ARROW"}
  , Record {point = "0297A", uchar = "\10618", latex = "", unicodemath = "\\leftarrowsubset", cls = "R", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW THROUGH SUBSET"}
  , Record {point = "0297B", uchar = "\10619", latex = "", unicodemath = "\\suplarr", cls = "R", category = "mathrel", requirements = "", comments = "SUPERSET ABOVE LEFTWARDS ARROW"}
  , Record {point = "0297C", uchar = "\10620", latex = "\\strictfi", unicodemath = "\\leftfishtail", cls = "R", category = "mathrel", requirements = "txfonts", comments = "LEFT FISH TAIL"}
  , Record {point = "0297D", uchar = "\10621", latex = "\\strictif", unicodemath = "\\rightfishtail", cls = "R", category = "mathrel", requirements = "txfonts", comments = "RIGHT FISH TAIL"}
  , Record {point = "0297E", uchar = "\10622", latex = "", unicodemath = "\\upfishtail", cls = "R", category = "mathrel", requirements = "", comments = "UP FISH TAIL"}
  , Record {point = "0297F", uchar = "\10623", latex = "", unicodemath = "\\downfishtail", cls = "R", category = "mathrel", requirements = "", comments = "DOWN FISH TAIL"}
  , Record {point = "02980", uchar = "\10624", latex = "\\VERT", unicodemath = "\\Vvert", cls = "F", category = "mathfence", requirements = "fourier", comments = "TRIPLE VERTICAL BAR DELIMITER"}
  , Record {point = "02981", uchar = "\10625", latex = "\\spot", unicodemath = "\\mdsmblkcircle", cls = "N", category = "mathord", requirements = "oz", comments = "= \\dot (oz), Z NOTATION SPOT"}
  , Record {point = "02982", uchar = "\10626", latex = "", unicodemath = "\\typecolon", cls = "F", category = "mathbin", requirements = "", comments = "Z NOTATION TYPE COLON, (present in bbold font but no command)"}
  , Record {point = "02983", uchar = "\10627", latex = "", unicodemath = "\\lBrace", cls = "O", category = "mathopen", requirements = "", comments = "LEFT WHITE CURLY BRACKET"}
  , Record {point = "02984", uchar = "\10628", latex = "", unicodemath = "\\rBrace", cls = "C", category = "mathclose", requirements = "", comments = "RIGHT WHITE CURLY BRACKET"}
  , Record {point = "02985", uchar = "\10629", latex = "\\Lparen", unicodemath = "\\lParen", cls = "O", category = "mathopen", requirements = "mathbbol", comments = "LEFT WHITE PARENTHESIS"}
  , Record {point = "02986", uchar = "\10630", latex = "\\Rparen", unicodemath = "\\rParen", cls = "C", category = "mathclose", requirements = "mathbbol", comments = "RIGHT WHITE PARENTHESIS"}
  , Record {point = "02987", uchar = "\10631", latex = "\\limg", unicodemath = "\\llparenthesis", cls = "O", category = "mathopen", requirements = "oz", comments = "= \\llparenthesis (stmaryrd), Z NOTATION LEFT IMAGE BRACKET"}
  , Record {point = "02988", uchar = "\10632", latex = "\\rimg", unicodemath = "\\rrparenthesis", cls = "C", category = "mathclose", requirements = "oz", comments = "= \\rrparenthesis (stmaryrd), Z NOTATION RIGHT IMAGE BRACKET"}
  , Record {point = "02989", uchar = "\10633", latex = "\\lblot", unicodemath = "\\llangle", cls = "O", category = "mathopen", requirements = "oz", comments = "Z NOTATION LEFT BINDING BRACKET"}
  , Record {point = "0298A", uchar = "\10634", latex = "\\rblot", unicodemath = "\\rrangle", cls = "C", category = "mathclose", requirements = "oz", comments = "Z NOTATION RIGHT BINDING BRACKET"}
  , Record {point = "0298B", uchar = "\10635", latex = "", unicodemath = "\\lbrackubar", cls = "O", category = "mathopen", requirements = "", comments = "LEFT SQUARE BRACKET WITH UNDERBAR"}
  , Record {point = "0298C", uchar = "\10636", latex = "", unicodemath = "\\rbrackubar", cls = "C", category = "mathclose", requirements = "", comments = "RIGHT SQUARE BRACKET WITH UNDERBAR"}
  , Record {point = "0298D", uchar = "\10637", latex = "", unicodemath = "\\lbrackultick", cls = "O", category = "mathopen", requirements = "", comments = "LEFT SQUARE BRACKET WITH TICK IN TOP CORNER"}
  , Record {point = "0298E", uchar = "\10638", latex = "", unicodemath = "\\rbracklrtick", cls = "C", category = "mathclose", requirements = "", comments = "RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER"}
  , Record {point = "0298F", uchar = "\10639", latex = "", unicodemath = "\\lbracklltick", cls = "O", category = "mathopen", requirements = "", comments = "LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER"}
  , Record {point = "02990", uchar = "\10640", latex = "", unicodemath = "\\rbrackurtick", cls = "C", category = "mathclose", requirements = "", comments = "RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER"}
  , Record {point = "02991", uchar = "\10641", latex = "", unicodemath = "\\langledot", cls = "O", category = "mathopen", requirements = "", comments = "LEFT ANGLE BRACKET WITH DOT"}
  , Record {point = "02992", uchar = "\10642", latex = "", unicodemath = "\\rangledot", cls = "C", category = "mathclose", requirements = "", comments = "RIGHT ANGLE BRACKET WITH DOT"}
  , Record {point = "02993", uchar = "\10643", latex = "", unicodemath = "\\lparenless", cls = "O", category = "mathopen", requirements = "", comments = "LEFT ARC LESS-THAN BRACKET"}
  , Record {point = "02994", uchar = "\10644", latex = "", unicodemath = "\\rparengtr", cls = "C", category = "mathclose", requirements = "", comments = "RIGHT ARC GREATER-THAN BRACKET"}
  , Record {point = "02995", uchar = "\10645", latex = "", unicodemath = "\\Lparengtr", cls = "O", category = "mathopen", requirements = "", comments = "DOUBLE LEFT ARC GREATER-THAN BRACKET"}
  , Record {point = "02996", uchar = "\10646", latex = "", unicodemath = "\\Rparenless", cls = "C", category = "mathclose", requirements = "", comments = "DOUBLE RIGHT ARC LESS-THAN BRACKET"}
  , Record {point = "02997", uchar = "\10647", latex = "", unicodemath = "\\lblkbrbrak", cls = "O", category = "mathopen", requirements = "", comments = "LEFT BLACK TORTOISE SHELL BRACKET"}
  , Record {point = "02998", uchar = "\10648", latex = "", unicodemath = "\\rblkbrbrak", cls = "C", category = "mathclose", requirements = "", comments = "RIGHT BLACK TORTOISE SHELL BRACKET"}
  , Record {point = "02999", uchar = "\10649", latex = "", unicodemath = "\\fourvdots", cls = "F", category = "mathord", requirements = "", comments = "DOTTED FENCE"}
  , Record {point = "0299A", uchar = "\10650", latex = "", unicodemath = "\\vzigzag", cls = "F", category = "mathord", requirements = "", comments = "VERTICAL ZIGZAG LINE"}
  , Record {point = "0299B", uchar = "\10651", latex = "", unicodemath = "\\measuredangleleft", cls = "N", category = "mathord", requirements = "", comments = "MEASURED ANGLE OPENING LEFT"}
  , Record {point = "0299C", uchar = "\10652", latex = "", unicodemath = "\\rightanglesqr", cls = "N", category = "mathord", requirements = "", comments = "RIGHT ANGLE VARIANT WITH SQUARE"}
  , Record {point = "0299D", uchar = "\10653", latex = "", unicodemath = "\\rightanglemdot", cls = "N", category = "mathord", requirements = "", comments = "MEASURED RIGHT ANGLE WITH DOT"}
  , Record {point = "0299E", uchar = "\10654", latex = "", unicodemath = "\\angles", cls = "N", category = "mathord", requirements = "", comments = "ANGLE WITH S INSIDE"}
  , Record {point = "0299F", uchar = "\10655", latex = "", unicodemath = "\\angdnr", cls = "N", category = "mathord", requirements = "", comments = "ACUTE ANGLE"}
  , Record {point = "029A0", uchar = "\10656", latex = "", unicodemath = "\\gtlpar", cls = "N", category = "mathord", requirements = "", comments = "SPHERICAL ANGLE OPENING LEFT"}
  , Record {point = "029A1", uchar = "\10657", latex = "", unicodemath = "\\sphericalangleup", cls = "N", category = "mathord", requirements = "", comments = "SPHERICAL ANGLE OPENING UP"}
  , Record {point = "029A2", uchar = "\10658", latex = "", unicodemath = "\\turnangle", cls = "N", category = "mathord", requirements = "", comments = "TURNED ANGLE"}
  , Record {point = "029A3", uchar = "\10659", latex = "", unicodemath = "\\revangle", cls = "N", category = "mathord", requirements = "", comments = "REVERSED ANGLE"}
  , Record {point = "029A4", uchar = "\10660", latex = "", unicodemath = "\\angleubar", cls = "N", category = "mathord", requirements = "", comments = "ANGLE WITH UNDERBAR"}
  , Record {point = "029A5", uchar = "\10661", latex = "", unicodemath = "\\revangleubar", cls = "N", category = "mathord", requirements = "", comments = "REVERSED ANGLE WITH UNDERBAR"}
  , Record {point = "029A6", uchar = "\10662", latex = "", unicodemath = "\\wideangledown", cls = "N", category = "mathord", requirements = "", comments = "OBLIQUE ANGLE OPENING UP"}
  , Record {point = "029A7", uchar = "\10663", latex = "", unicodemath = "\\wideangleup", cls = "N", category = "mathord", requirements = "", comments = "OBLIQUE ANGLE OPENING DOWN"}
  , Record {point = "029A8", uchar = "\10664", latex = "", unicodemath = "\\measanglerutone", cls = "N", category = "mathord", requirements = "", comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING UP AND RIGHT"}
  , Record {point = "029A9", uchar = "\10665", latex = "", unicodemath = "\\measanglelutonw", cls = "N", category = "mathord", requirements = "", comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING UP AND LEFT"}
  , Record {point = "029AA", uchar = "\10666", latex = "", unicodemath = "\\measanglerdtose", cls = "N", category = "mathord", requirements = "", comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING DOWN AND RIGHT"}
  , Record {point = "029AB", uchar = "\10667", latex = "", unicodemath = "\\measangleldtosw", cls = "N", category = "mathord", requirements = "", comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING DOWN AND LEFT"}
  , Record {point = "029AC", uchar = "\10668", latex = "", unicodemath = "\\measangleurtone", cls = "N", category = "mathord", requirements = "", comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING RIGHT AND UP"}
  , Record {point = "029AD", uchar = "\10669", latex = "", unicodemath = "\\measangleultonw", cls = "N", category = "mathord", requirements = "", comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING LEFT AND UP"}
  , Record {point = "029AE", uchar = "\10670", latex = "", unicodemath = "\\measangledrtose", cls = "N", category = "mathord", requirements = "", comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING RIGHT AND DOWN"}
  , Record {point = "029AF", uchar = "\10671", latex = "", unicodemath = "\\measangledltosw", cls = "N", category = "mathord", requirements = "", comments = "MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING LEFT AND DOWN"}
  , Record {point = "029B0", uchar = "\10672", latex = "", unicodemath = "\\revemptyset", cls = "N", category = "mathord", requirements = "", comments = "REVERSED EMPTY SET"}
  , Record {point = "029B1", uchar = "\10673", latex = "", unicodemath = "\\emptysetobar", cls = "N", category = "mathord", requirements = "", comments = "EMPTY SET WITH OVERBAR"}
  , Record {point = "029B2", uchar = "\10674", latex = "", unicodemath = "\\emptysetocirc", cls = "N", category = "mathord", requirements = "", comments = "EMPTY SET WITH SMALL CIRCLE ABOVE"}
  , Record {point = "029B3", uchar = "\10675", latex = "", unicodemath = "\\emptysetoarr", cls = "N", category = "mathord", requirements = "", comments = "EMPTY SET WITH RIGHT ARROW ABOVE"}
  , Record {point = "029B4", uchar = "\10676", latex = "", unicodemath = "\\emptysetoarrl", cls = "N", category = "mathord", requirements = "", comments = "EMPTY SET WITH LEFT ARROW ABOVE"}
  , Record {point = "029B5", uchar = "\10677", latex = "", unicodemath = "\\circlehbar", cls = "N", category = "mathbin", requirements = "", comments = "CIRCLE WITH HORIZONTAL BAR"}
  , Record {point = "029B6", uchar = "\10678", latex = "", unicodemath = "\\circledvert", cls = "B", category = "mathbin", requirements = "", comments = "CIRCLED VERTICAL BAR"}
  , Record {point = "029B7", uchar = "\10679", latex = "", unicodemath = "\\circledparallel", cls = "B", category = "mathbin", requirements = "", comments = "CIRCLED PARALLEL"}
  , Record {point = "029B8", uchar = "\10680", latex = "\\circledbslash", unicodemath = "\\obslash", cls = "B", category = "mathbin", requirements = "txfonts", comments = "CIRCLED REVERSE SOLIDUS"}
  , Record {point = "029B9", uchar = "\10681", latex = "", unicodemath = "\\operp", cls = "B", category = "mathbin", requirements = "", comments = "CIRCLED PERPENDICULAR"}
  , Record {point = "029BA", uchar = "\10682", latex = "", unicodemath = "\\obot", cls = "N", category = "mathord", requirements = "", comments = "CIRCLE DIVIDED BY HORIZONTAL BAR AND TOP HALF DIVIDED BY VERTICAL BAR"}
  , Record {point = "029BB", uchar = "\10683", latex = "", unicodemath = "\\olcross", cls = "N", category = "mathord", requirements = "", comments = "CIRCLE WITH SUPERIMPOSED X"}
  , Record {point = "029BC", uchar = "\10684", latex = "", unicodemath = "\\odotslashdot", cls = "N", category = "mathord", requirements = "", comments = "CIRCLED ANTICLOCKWISE-ROTATED DIVISION SIGN"}
  , Record {point = "029BD", uchar = "\10685", latex = "", unicodemath = "\\uparrowoncircle", cls = "N", category = "mathord", requirements = "", comments = "UP ARROW THROUGH CIRCLE"}
  , Record {point = "029BE", uchar = "\10686", latex = "", unicodemath = "\\circledwhitebullet", cls = "N", category = "mathord", requirements = "", comments = "CIRCLED WHITE BULLET"}
  , Record {point = "029BF", uchar = "\10687", latex = "", unicodemath = "\\circledbullet", cls = "N", category = "mathord", requirements = "", comments = "CIRCLED BULLET"}
  , Record {point = "029C0", uchar = "\10688", latex = "\\circledless", unicodemath = "\\olessthan", cls = "B", category = "mathbin", requirements = "txfonts", comments = "CIRCLED LESS-THAN"}
  , Record {point = "029C1", uchar = "\10689", latex = "\\circledgtr", unicodemath = "\\ogreaterthan", cls = "B", category = "mathbin", requirements = "txfonts", comments = "CIRCLED GREATER-THAN"}
  , Record {point = "029C2", uchar = "\10690", latex = "", unicodemath = "\\cirscir", cls = "N", category = "mathord", requirements = "", comments = "CIRCLE WITH SMALL CIRCLE TO THE RIGHT"}
  , Record {point = "029C3", uchar = "\10691", latex = "", unicodemath = "\\cirE", cls = "N", category = "mathord", requirements = "", comments = "CIRCLE WITH TWO HORIZONTAL STROKES TO THE RIGHT"}
  , Record {point = "029C4", uchar = "\10692", latex = "\\boxslash", unicodemath = "\\boxdiag", cls = "B", category = "mathbin", requirements = "stmaryrd txfonts", comments = "SQUARED RISING DIAGONAL SLASH"}
  , Record {point = "029C5", uchar = "\10693", latex = "\\boxbslash", unicodemath = "\\boxbslash", cls = "B", category = "mathbin", requirements = "stmaryrd txfonts", comments = "SQUARED FALLING DIAGONAL SLASH"}
  , Record {point = "029C6", uchar = "\10694", latex = "\\boxast", unicodemath = "\\boxast", cls = "B", category = "mathbin", requirements = "stmaryrd txfonts", comments = "SQUARED ASTERISK"}
  , Record {point = "029C7", uchar = "\10695", latex = "\\boxcircle", unicodemath = "\\boxcircle", cls = "B", category = "mathbin", requirements = "stmaryrd", comments = "SQUARED SMALL CIRCLE"}
  , Record {point = "029C8", uchar = "\10696", latex = "\\boxbox", unicodemath = "\\boxbox", cls = "B", category = "mathbin", requirements = "stmaryrd", comments = "SQUARED SQUARE"}
  , Record {point = "029C9", uchar = "\10697", latex = "", unicodemath = "\\boxonbox", cls = "N", category = "mathord", requirements = "", comments = "TWO JOINED SQUARES"}
  , Record {point = "029CA", uchar = "\10698", latex = "", unicodemath = "\\triangleodot", cls = "N", category = "mathord", requirements = "", comments = "TRIANGLE WITH DOT ABOVE"}
  , Record {point = "029CB", uchar = "\10699", latex = "", unicodemath = "\\triangleubar", cls = "N", category = "mathord", requirements = "", comments = "TRIANGLE WITH UNDERBAR"}
  , Record {point = "029CC", uchar = "\10700", latex = "", unicodemath = "\\triangles", cls = "N", category = "mathord", requirements = "", comments = "S IN TRIANGLE"}
  , Record {point = "029CD", uchar = "\10701", latex = "", unicodemath = "\\triangleserifs", cls = "N", category = "mathbin", requirements = "", comments = "TRIANGLE WITH SERIFS AT BOTTOM"}
  , Record {point = "029CE", uchar = "\10702", latex = "", unicodemath = "\\rtriltri", cls = "R", category = "mathrel", requirements = "", comments = "RIGHT TRIANGLE ABOVE LEFT TRIANGLE"}
  , Record {point = "029CF", uchar = "\10703", latex = "\\LeftTriangleBar", unicodemath = "\\ltrivb", cls = "R", category = "mathrel", requirements = "wrisym", comments = "LEFT TRIANGLE BESIDE VERTICAL BAR"}
  , Record {point = "029D0", uchar = "\10704", latex = "\\RightTriangleBar", unicodemath = "\\vbrtri", cls = "R", category = "mathrel", requirements = "wrisym", comments = "VERTICAL BAR BESIDE RIGHT TRIANGLE"}
  , Record {point = "029D1", uchar = "\10705", latex = "", unicodemath = "\\lfbowtie", cls = "R", category = "mathrel", requirements = "", comments = "left black bowtie"}
  , Record {point = "029D2", uchar = "\10706", latex = "", unicodemath = "\\rfbowtie", cls = "R", category = "mathrel", requirements = "", comments = "right black bowtie"}
  , Record {point = "029D3", uchar = "\10707", latex = "", unicodemath = "\\fbowtie", cls = "R", category = "mathrel", requirements = "", comments = "BLACK BOWTIE"}
  , Record {point = "029D4", uchar = "\10708", latex = "", unicodemath = "\\lftimes", cls = "R", category = "mathrel", requirements = "", comments = "left black times"}
  , Record {point = "029D5", uchar = "\10709", latex = "", unicodemath = "\\rftimes", cls = "R", category = "mathrel", requirements = "", comments = "right black times"}
  , Record {point = "029D6", uchar = "\10710", latex = "", unicodemath = "\\hourglass", cls = "B", category = "mathbin", requirements = "", comments = "WHITE HOURGLASS"}
  , Record {point = "029D7", uchar = "\10711", latex = "", unicodemath = "\\blackhourglass", cls = "B", category = "mathbin", requirements = "", comments = "BLACK HOURGLASS"}
  , Record {point = "029D8", uchar = "\10712", latex = "", unicodemath = "\\lvzigzag", cls = "O", category = "mathopen", requirements = "", comments = "LEFT WIGGLY FENCE"}
  , Record {point = "029D9", uchar = "\10713", latex = "", unicodemath = "\\rvzigzag", cls = "C", category = "mathclose", requirements = "", comments = "RIGHT WIGGLY FENCE"}
  , Record {point = "029DA", uchar = "\10714", latex = "", unicodemath = "\\Lvzigzag", cls = "O", category = "mathopen", requirements = "", comments = "LEFT DOUBLE WIGGLY FENCE"}
  , Record {point = "029DB", uchar = "\10715", latex = "", unicodemath = "\\Rvzigzag", cls = "C", category = "mathclose", requirements = "", comments = "RIGHT DOUBLE WIGGLY FENCE"}
  , Record {point = "029DC", uchar = "\10716", latex = "", unicodemath = "\\iinfin", cls = "N", category = "mathord", requirements = "", comments = "INCOMPLETE INFINITY"}
  , Record {point = "029DD", uchar = "\10717", latex = "", unicodemath = "\\tieinfty", cls = "N", category = "mathord", requirements = "", comments = "TIE OVER INFINITY"}
  , Record {point = "029DE", uchar = "\10718", latex = "", unicodemath = "\\nvinfty", cls = "N", category = "mathord", requirements = "", comments = "INFINITY NEGATED WITH VERTICAL BAR"}
  , Record {point = "029DF", uchar = "\10719", latex = "\\multimapboth", unicodemath = "\\dualmap", cls = "R", category = "mathrel", requirements = "txfonts", comments = "DOUBLE-ENDED MULTIMAP"}
  , Record {point = "029E0", uchar = "\10720", latex = "", unicodemath = "\\laplac", cls = "N", category = "mathord", requirements = "", comments = "SQUARE WITH CONTOURED OUTLINE"}
  , Record {point = "029E1", uchar = "\10721", latex = "", unicodemath = "\\lrtriangleeq", cls = "R", category = "mathrel", requirements = "", comments = "INCREASES AS"}
  , Record {point = "029E2", uchar = "\10722", latex = "", unicodemath = "\\shuffle", cls = "B", category = "mathbin", requirements = "", comments = "SHUFFLE PRODUCT"}
  , Record {point = "029E3", uchar = "\10723", latex = "", unicodemath = "\\eparsl", cls = "R", category = "mathrel", requirements = "", comments = "EQUALS SIGN AND SLANTED PARALLEL"}
  , Record {point = "029E4", uchar = "\10724", latex = "", unicodemath = "\\smeparsl", cls = "R", category = "mathrel", requirements = "", comments = "EQUALS SIGN AND SLANTED PARALLEL WITH TILDE ABOVE"}
  , Record {point = "029E5", uchar = "\10725", latex = "", unicodemath = "\\eqvparsl", cls = "R", category = "mathrel", requirements = "", comments = "IDENTICAL TO AND SLANTED PARALLEL"}
  , Record {point = "029E6", uchar = "\10726", latex = "", unicodemath = "\\gleichstark", cls = "R", category = "mathrel", requirements = "", comments = "GLEICH STARK"}
  , Record {point = "029E7", uchar = "\10727", latex = "", unicodemath = "\\thermod", cls = "N", category = "mathord", requirements = "", comments = "THERMODYNAMIC"}
  , Record {point = "029E8", uchar = "\10728", latex = "", unicodemath = "\\downtriangleleftblack", cls = "N", category = "mathord", requirements = "", comments = "DOWN-POINTING TRIANGLE WITH LEFT HALF BLACK"}
  , Record {point = "029E9", uchar = "\10729", latex = "", unicodemath = "\\downtrianglerightblack", cls = "N", category = "mathord", requirements = "", comments = "DOWN-POINTING TRIANGLE WITH RIGHT HALF BLACK"}
  , Record {point = "029EA", uchar = "\10730", latex = "", unicodemath = "\\blackdiamonddownarrow", cls = "N", category = "mathord", requirements = "", comments = "BLACK DIAMOND WITH DOWN ARROW"}
  , Record {point = "029EB", uchar = "\10731", latex = "\\blacklozenge", unicodemath = "\\mdlgblklozenge", cls = "B", category = "mathbin", requirements = "amssymb", comments = "BLACK LOZENGE"}
  , Record {point = "029EC", uchar = "\10732", latex = "", unicodemath = "\\circledownarrow", cls = "N", category = "mathord", requirements = "", comments = "WHITE CIRCLE WITH DOWN ARROW"}
  , Record {point = "029ED", uchar = "\10733", latex = "", unicodemath = "\\blackcircledownarrow", cls = "N", category = "mathord", requirements = "", comments = "BLACK CIRCLE WITH DOWN ARROW"}
  , Record {point = "029EE", uchar = "\10734", latex = "", unicodemath = "\\errbarsquare", cls = "N", category = "mathord", requirements = "", comments = "ERROR-BARRED WHITE SQUARE"}
  , Record {point = "029EF", uchar = "\10735", latex = "", unicodemath = "\\errbarblacksquare", cls = "N", category = "mathord", requirements = "", comments = "ERROR-BARRED BLACK SQUARE"}
  , Record {point = "029F0", uchar = "\10736", latex = "", unicodemath = "\\errbardiamond", cls = "N", category = "mathord", requirements = "", comments = "ERROR-BARRED WHITE DIAMOND"}
  , Record {point = "029F1", uchar = "\10737", latex = "", unicodemath = "\\errbarblackdiamond", cls = "N", category = "mathord", requirements = "", comments = "ERROR-BARRED BLACK DIAMOND"}
  , Record {point = "029F2", uchar = "\10738", latex = "", unicodemath = "\\errbarcircle", cls = "N", category = "mathord", requirements = "", comments = "ERROR-BARRED WHITE CIRCLE"}
  , Record {point = "029F3", uchar = "\10739", latex = "", unicodemath = "\\errbarblackcircle", cls = "N", category = "mathord", requirements = "", comments = "ERROR-BARRED BLACK CIRCLE"}
  , Record {point = "029F4", uchar = "\10740", latex = "", unicodemath = "\\ruledelayed", cls = "R", category = "mathrel", requirements = "", comments = "RULE-DELAYED"}
  , Record {point = "029F5", uchar = "\10741", latex = "\\setminus", unicodemath = "\\setminus", cls = "B", category = "mathbin", requirements = "", comments = "REVERSE SOLIDUS OPERATOR"}
  , Record {point = "029F6", uchar = "\10742", latex = "", unicodemath = "\\dsol", cls = "B", category = "mathbin", requirements = "", comments = "SOLIDUS WITH OVERBAR"}
  , Record {point = "029F7", uchar = "\10743", latex = "", unicodemath = "\\rsolbar", cls = "B", category = "mathbin", requirements = "", comments = "REVERSE SOLIDUS WITH HORIZONTAL STROKE"}
  , Record {point = "029F8", uchar = "\10744", latex = "", unicodemath = "\\xsol", cls = "L", category = "mathop", requirements = "", comments = "BIG SOLIDUS"}
  , Record {point = "029F9", uchar = "\10745", latex = "\\zhide", unicodemath = "\\xbsol", cls = "L", category = "mathop", requirements = "oz", comments = "= \\hide (oz), BIG REVERSE SOLIDUS, z notation schema hiding"}
  , Record {point = "029FA", uchar = "\10746", latex = "", unicodemath = "\\doubleplus", cls = "B", category = "mathbin", requirements = "", comments = "DOUBLE PLUS"}
  , Record {point = "029FB", uchar = "\10747", latex = "", unicodemath = "\\tripleplus", cls = "B", category = "mathbin", requirements = "", comments = "TRIPLE PLUS"}
  , Record {point = "029FC", uchar = "\10748", latex = "", unicodemath = "\\lcurvyangle", cls = "O", category = "mathopen", requirements = "", comments = "left pointing curved angle bracket"}
  , Record {point = "029FD", uchar = "\10749", latex = "", unicodemath = "\\rcurvyangle", cls = "C", category = "mathclose", requirements = "", comments = "right pointing curved angle bracket"}
  , Record {point = "029FE", uchar = "\10750", latex = "", unicodemath = "\\tplus", cls = "B", category = "mathbin", requirements = "", comments = "TINY"}
  , Record {point = "029FF", uchar = "\10751", latex = "", unicodemath = "\\tminus", cls = "B", category = "mathbin", requirements = "", comments = "MINY"}
  , Record {point = "02A00", uchar = "\10752", latex = "\\bigodot", unicodemath = "\\bigodot", cls = "L", category = "mathop", requirements = "", comments = "N-ARY CIRCLED DOT OPERATOR"}
  , Record {point = "02A01", uchar = "\10753", latex = "\\bigoplus", unicodemath = "\\bigoplus", cls = "L", category = "mathop", requirements = "", comments = "N-ARY CIRCLED PLUS OPERATOR"}
  , Record {point = "02A02", uchar = "\10754", latex = "\\bigotimes", unicodemath = "\\bigotimes", cls = "L", category = "mathop", requirements = "", comments = "N-ARY CIRCLED TIMES OPERATOR"}
  , Record {point = "02A03", uchar = "\10755", latex = "", unicodemath = "\\bigcupdot", cls = "L", category = "mathop", requirements = "", comments = "N-ARY UNION OPERATOR WITH DOT"}
  , Record {point = "02A04", uchar = "\10756", latex = "\\biguplus", unicodemath = "\\biguplus", cls = "L", category = "mathop", requirements = "", comments = "N-ARY UNION OPERATOR WITH PLUS"}
  , Record {point = "02A05", uchar = "\10757", latex = "\\bigsqcap", unicodemath = "\\bigsqcap", cls = "L", category = "mathop", requirements = "txfonts", comments = "N-ARY SQUARE INTERSECTION OPERATOR"}
  , Record {point = "02A06", uchar = "\10758", latex = "\\bigsqcup", unicodemath = "\\bigsqcup", cls = "L", category = "mathop", requirements = "", comments = "N-ARY SQUARE UNION OPERATOR"}
  , Record {point = "02A07", uchar = "\10759", latex = "", unicodemath = "\\conjquant", cls = "L", category = "mathop", requirements = "", comments = "TWO LOGICAL AND OPERATOR"}
  , Record {point = "02A08", uchar = "\10760", latex = "", unicodemath = "\\disjquant", cls = "L", category = "mathop", requirements = "", comments = "TWO LOGICAL OR OPERATOR"}
  , Record {point = "02A09", uchar = "\10761", latex = "\\varprod", unicodemath = "\\bigtimes", cls = "L", category = "mathop", requirements = "txfonts", comments = "N-ARY TIMES OPERATOR"}
  , Record {point = "02A0A", uchar = "\10762", latex = "", unicodemath = "\\modtwosum", cls = "L", category = "mathord", requirements = "", comments = "MODULO TWO SUM"}
  , Record {point = "02A0B", uchar = "\10763", latex = "", unicodemath = "\\sumint", cls = "L", category = "mathop", requirements = "", comments = "SUMMATION WITH INTEGRAL"}
  , Record {point = "02A0C", uchar = "\10764", latex = "\\iiiint", unicodemath = "\\iiiint", cls = "L", category = "mathop", requirements = "amsmath esint", comments = "QUADRUPLE INTEGRAL OPERATOR"}
  , Record {point = "02A0D", uchar = "\10765", latex = "", unicodemath = "\\intbar", cls = "L", category = "mathop", requirements = "", comments = "FINITE PART INTEGRAL"}
  , Record {point = "02A0E", uchar = "\10766", latex = "", unicodemath = "\\intBar", cls = "L", category = "mathop", requirements = "", comments = "INTEGRAL WITH DOUBLE STROKE"}
  , Record {point = "02A0F", uchar = "\10767", latex = "\\fint", unicodemath = "\\fint", cls = "L", category = "mathop", requirements = "esint wrisym", comments = "INTEGRAL AVERAGE WITH SLASH"}
  , Record {point = "02A10", uchar = "\10768", latex = "", unicodemath = "\\cirfnint", cls = "L", category = "mathop", requirements = "", comments = "CIRCULATION FUNCTION"}
  , Record {point = "02A11", uchar = "\10769", latex = "", unicodemath = "\\awint", cls = "L", category = "mathop", requirements = "", comments = "ANTICLOCKWISE INTEGRATION"}
  , Record {point = "02A12", uchar = "\10770", latex = "", unicodemath = "\\rppolint", cls = "L", category = "mathop", requirements = "", comments = "LINE INTEGRATION WITH RECTANGULAR PATH AROUND POLE"}
  , Record {point = "02A13", uchar = "\10771", latex = "", unicodemath = "\\scpolint", cls = "L", category = "mathop", requirements = "", comments = "LINE INTEGRATION WITH SEMICIRCULAR PATH AROUND POLE"}
  , Record {point = "02A14", uchar = "\10772", latex = "", unicodemath = "\\npolint", cls = "L", category = "mathop", requirements = "", comments = "LINE INTEGRATION NOT INCLUDING THE POLE"}
  , Record {point = "02A15", uchar = "\10773", latex = "", unicodemath = "\\pointint", cls = "L", category = "mathop", requirements = "", comments = "INTEGRAL AROUND A POINT OPERATOR"}
  , Record {point = "02A16", uchar = "\10774", latex = "\\sqint", unicodemath = "\\sqint", cls = "L", category = "mathop", requirements = "esint", comments = "= \\sqrint (wrisym), QUATERNION INTEGRAL OPERATOR"}
  , Record {point = "02A17", uchar = "\10775", latex = "", unicodemath = "\\intlarhk", cls = "L", category = "mathop", requirements = "", comments = "INTEGRAL WITH LEFTWARDS ARROW WITH HOOK"}
  , Record {point = "02A18", uchar = "\10776", latex = "", unicodemath = "\\intx", cls = "L", category = "mathop", requirements = "", comments = "INTEGRAL WITH TIMES SIGN"}
  , Record {point = "02A19", uchar = "\10777", latex = "", unicodemath = "\\intcap", cls = "L", category = "mathop", requirements = "", comments = "INTEGRAL WITH INTERSECTION"}
  , Record {point = "02A1A", uchar = "\10778", latex = "", unicodemath = "\\intcup", cls = "L", category = "mathop", requirements = "", comments = "INTEGRAL WITH UNION"}
  , Record {point = "02A1B", uchar = "\10779", latex = "", unicodemath = "\\upint", cls = "L", category = "mathop", requirements = "", comments = "INTEGRAL WITH OVERBAR"}
  , Record {point = "02A1C", uchar = "\10780", latex = "", unicodemath = "\\lowint", cls = "L", category = "mathop", requirements = "", comments = "INTEGRAL WITH UNDERBAR"}
  , Record {point = "02A1D", uchar = "\10781", latex = "\\Join", unicodemath = "\\Join", cls = "L", category = "mathop", requirements = "amssymb", comments = "JOIN"}
  , Record {point = "02A1E", uchar = "\10782", latex = "", unicodemath = "\\bigtriangleleft", cls = "L", category = "mathop", requirements = "", comments = "LARGE LEFT TRIANGLE OPERATOR"}
  , Record {point = "02A1F", uchar = "\10783", latex = "\\zcmp", unicodemath = "\\zcmp", cls = "L", category = "mathop", requirements = "oz", comments = "= \\semi (oz), = \\fatsemi (stmaryrd), Z NOTATION SCHEMA COMPOSITION"}
  , Record {point = "02A20", uchar = "\10784", latex = "\\zpipe", unicodemath = "\\zpipe", cls = "L", category = "mathop", requirements = "oz", comments = "Z NOTATION SCHEMA PIPING"}
  , Record {point = "02A21", uchar = "\10785", latex = "\\zproject", unicodemath = "\\zproject", cls = "L", category = "mathop", requirements = "oz", comments = "= \\project (oz), Z NOTATION SCHEMA PROJECTION"}
  , Record {point = "02A22", uchar = "\10786", latex = "", unicodemath = "\\ringplus", cls = "B", category = "mathbin", requirements = "", comments = "PLUS SIGN WITH SMALL CIRCLE ABOVE"}
  , Record {point = "02A23", uchar = "\10787", latex = "", unicodemath = "\\plushat", cls = "B", category = "mathbin", requirements = "", comments = "PLUS SIGN WITH CIRCUMFLEX ACCENT ABOVE"}
  , Record {point = "02A24", uchar = "\10788", latex = "", unicodemath = "\\simplus", cls = "B", category = "mathbin", requirements = "", comments = "PLUS SIGN WITH TILDE ABOVE"}
  , Record {point = "02A25", uchar = "\10789", latex = "", unicodemath = "\\plusdot", cls = "B", category = "mathbin", requirements = "", comments = "PLUS SIGN WITH DOT BELOW"}
  , Record {point = "02A26", uchar = "\10790", latex = "", unicodemath = "\\plussim", cls = "B", category = "mathbin", requirements = "", comments = "PLUS SIGN WITH TILDE BELOW"}
  , Record {point = "02A27", uchar = "\10791", latex = "", unicodemath = "\\plussubtwo", cls = "B", category = "mathbin", requirements = "", comments = "PLUS SIGN WITH SUBSCRIPT TWO"}
  , Record {point = "02A28", uchar = "\10792", latex = "", unicodemath = "\\plustrif", cls = "B", category = "mathbin", requirements = "", comments = "PLUS SIGN WITH BLACK TRIANGLE"}
  , Record {point = "02A29", uchar = "\10793", latex = "", unicodemath = "\\commaminus", cls = "B", category = "mathbin", requirements = "", comments = "MINUS SIGN WITH COMMA ABOVE"}
  , Record {point = "02A2A", uchar = "\10794", latex = "", unicodemath = "\\minusdot", cls = "B", category = "mathbin", requirements = "", comments = "MINUS SIGN WITH DOT BELOW"}
  , Record {point = "02A2B", uchar = "\10795", latex = "", unicodemath = "\\minusfdots", cls = "B", category = "mathbin", requirements = "", comments = "MINUS SIGN WITH FALLING DOTS"}
  , Record {point = "02A2C", uchar = "\10796", latex = "", unicodemath = "\\minusrdots", cls = "B", category = "mathbin", requirements = "", comments = "MINUS SIGN WITH RISING DOTS"}
  , Record {point = "02A2D", uchar = "\10797", latex = "", unicodemath = "\\opluslhrim", cls = "B", category = "mathbin", requirements = "", comments = "PLUS SIGN IN LEFT HALF CIRCLE"}
  , Record {point = "02A2E", uchar = "\10798", latex = "", unicodemath = "\\oplusrhrim", cls = "B", category = "mathbin", requirements = "", comments = "PLUS SIGN IN RIGHT HALF CIRCLE"}
  , Record {point = "02A2F", uchar = "\10799", latex = "", unicodemath = "\\vectimes", cls = "B", category = "mathbin", requirements = "", comments = "# \\times, VECTOR OR CROSS PRODUCT"}
  , Record {point = "02A30", uchar = "\10800", latex = "", unicodemath = "\\dottimes", cls = "B", category = "mathbin", requirements = "", comments = "MULTIPLICATION SIGN WITH DOT ABOVE"}
  , Record {point = "02A31", uchar = "\10801", latex = "", unicodemath = "\\timesbar", cls = "B", category = "mathbin", requirements = "", comments = "MULTIPLICATION SIGN WITH UNDERBAR"}
  , Record {point = "02A32", uchar = "\10802", latex = "", unicodemath = "\\btimes", cls = "B", category = "mathbin", requirements = "", comments = "SEMIDIRECT PRODUCT WITH BOTTOM CLOSED"}
  , Record {point = "02A33", uchar = "\10803", latex = "", unicodemath = "\\smashtimes", cls = "B", category = "mathbin", requirements = "", comments = "SMASH PRODUCT"}
  , Record {point = "02A34", uchar = "\10804", latex = "", unicodemath = "\\otimeslhrim", cls = "B", category = "mathbin", requirements = "", comments = "MULTIPLICATION SIGN IN LEFT HALF CIRCLE"}
  , Record {point = "02A35", uchar = "\10805", latex = "", unicodemath = "\\otimesrhrim", cls = "B", category = "mathbin", requirements = "", comments = "MULTIPLICATION SIGN IN RIGHT HALF CIRCLE"}
  , Record {point = "02A36", uchar = "\10806", latex = "", unicodemath = "\\otimeshat", cls = "B", category = "mathbin", requirements = "", comments = "CIRCLED MULTIPLICATION SIGN WITH CIRCUMFLEX ACCENT"}
  , Record {point = "02A37", uchar = "\10807", latex = "", unicodemath = "\\Otimes", cls = "B", category = "mathbin", requirements = "", comments = "MULTIPLICATION SIGN IN DOUBLE CIRCLE"}
  , Record {point = "02A38", uchar = "\10808", latex = "", unicodemath = "\\odiv", cls = "B", category = "mathbin", requirements = "", comments = "CIRCLED DIVISION SIGN"}
  , Record {point = "02A39", uchar = "\10809", latex = "", unicodemath = "\\triangleplus", cls = "B", category = "mathbin", requirements = "", comments = "PLUS SIGN IN TRIANGLE"}
  , Record {point = "02A3A", uchar = "\10810", latex = "", unicodemath = "\\triangleminus", cls = "B", category = "mathbin", requirements = "", comments = "MINUS SIGN IN TRIANGLE"}
  , Record {point = "02A3B", uchar = "\10811", latex = "", unicodemath = "\\triangletimes", cls = "B", category = "mathbin", requirements = "", comments = "MULTIPLICATION SIGN IN TRIANGLE"}
  , Record {point = "02A3C", uchar = "\10812", latex = "", unicodemath = "\\intprod", cls = "B", category = "mathbin", requirements = "", comments = "INTERIOR PRODUCT"}
  , Record {point = "02A3D", uchar = "\10813", latex = "", unicodemath = "\\intprodr", cls = "B", category = "mathbin", requirements = "", comments = "RIGHTHAND INTERIOR PRODUCT"}
  , Record {point = "02A3E", uchar = "\10814", latex = "\\fcmp", unicodemath = "\\fcmp", cls = "B", category = "mathbin", requirements = "oz", comments = "= \\comp (oz), Z NOTATION RELATIONAL COMPOSITION"}
  , Record {point = "02A3F", uchar = "\10815", latex = "\\amalg", unicodemath = "\\amalg", cls = "B", category = "mathbin", requirements = "", comments = "AMALGAMATION OR COPRODUCT"}
  , Record {point = "02A40", uchar = "\10816", latex = "", unicodemath = "\\capdot", cls = "B", category = "mathbin", requirements = "", comments = "INTERSECTION WITH DOT"}
  , Record {point = "02A41", uchar = "\10817", latex = "", unicodemath = "\\uminus", cls = "B", category = "mathbin", requirements = "", comments = "UNION WITH MINUS SIGN, z notation bag subtraction"}
  , Record {point = "02A42", uchar = "\10818", latex = "", unicodemath = "\\barcup", cls = "B", category = "mathbin", requirements = "", comments = "UNION WITH OVERBAR"}
  , Record {point = "02A43", uchar = "\10819", latex = "", unicodemath = "\\barcap", cls = "B", category = "mathbin", requirements = "", comments = "INTERSECTION WITH OVERBAR"}
  , Record {point = "02A44", uchar = "\10820", latex = "", unicodemath = "\\capwedge", cls = "B", category = "mathbin", requirements = "", comments = "INTERSECTION WITH LOGICAL AND"}
  , Record {point = "02A45", uchar = "\10821", latex = "", unicodemath = "\\cupvee", cls = "B", category = "mathbin", requirements = "", comments = "UNION WITH LOGICAL OR"}
  , Record {point = "02A46", uchar = "\10822", latex = "", unicodemath = "\\cupovercap", cls = "B", category = "mathbin", requirements = "", comments = "UNION ABOVE INTERSECTION"}
  , Record {point = "02A47", uchar = "\10823", latex = "", unicodemath = "\\capovercup", cls = "B", category = "mathbin", requirements = "", comments = "INTERSECTION ABOVE UNION"}
  , Record {point = "02A48", uchar = "\10824", latex = "", unicodemath = "\\cupbarcap", cls = "B", category = "mathbin", requirements = "", comments = "UNION ABOVE BAR ABOVE INTERSECTION"}
  , Record {point = "02A49", uchar = "\10825", latex = "", unicodemath = "\\capbarcup", cls = "B", category = "mathbin", requirements = "", comments = "INTERSECTION ABOVE BAR ABOVE UNION"}
  , Record {point = "02A4A", uchar = "\10826", latex = "", unicodemath = "\\twocups", cls = "B", category = "mathbin", requirements = "", comments = "UNION BESIDE AND JOINED WITH UNION"}
  , Record {point = "02A4B", uchar = "\10827", latex = "", unicodemath = "\\twocaps", cls = "B", category = "mathbin", requirements = "", comments = "INTERSECTION BESIDE AND JOINED WITH INTERSECTION"}
  , Record {point = "02A4C", uchar = "\10828", latex = "", unicodemath = "\\closedvarcup", cls = "B", category = "mathbin", requirements = "", comments = "CLOSED UNION WITH SERIFS"}
  , Record {point = "02A4D", uchar = "\10829", latex = "", unicodemath = "\\closedvarcap", cls = "B", category = "mathbin", requirements = "", comments = "CLOSED INTERSECTION WITH SERIFS"}
  , Record {point = "02A4E", uchar = "\10830", latex = "", unicodemath = "\\Sqcap", cls = "B", category = "mathbin", requirements = "", comments = "DOUBLE SQUARE INTERSECTION"}
  , Record {point = "02A4F", uchar = "\10831", latex = "", unicodemath = "\\Sqcup", cls = "B", category = "mathbin", requirements = "", comments = "DOUBLE SQUARE UNION"}
  , Record {point = "02A50", uchar = "\10832", latex = "", unicodemath = "\\closedvarcupsmashprod", cls = "B", category = "mathbin", requirements = "", comments = "CLOSED UNION WITH SERIFS AND SMASH PRODUCT"}
  , Record {point = "02A51", uchar = "\10833", latex = "", unicodemath = "\\wedgeodot", cls = "B", category = "mathbin", requirements = "", comments = "LOGICAL AND WITH DOT ABOVE"}
  , Record {point = "02A52", uchar = "\10834", latex = "", unicodemath = "\\veeodot", cls = "B", category = "mathbin", requirements = "", comments = "LOGICAL OR WITH DOT ABOVE"}
  , Record {point = "02A53", uchar = "\10835", latex = "", unicodemath = "\\Wedge", cls = "B", category = "mathbin", requirements = "", comments = "DOUBLE LOGICAL AND"}
  , Record {point = "02A54", uchar = "\10836", latex = "", unicodemath = "\\Vee", cls = "B", category = "mathbin", requirements = "", comments = "DOUBLE LOGICAL OR"}
  , Record {point = "02A55", uchar = "\10837", latex = "", unicodemath = "\\wedgeonwedge", cls = "B", category = "mathbin", requirements = "", comments = "TWO INTERSECTING LOGICAL AND"}
  , Record {point = "02A56", uchar = "\10838", latex = "", unicodemath = "\\veeonvee", cls = "B", category = "mathbin", requirements = "", comments = "TWO INTERSECTING LOGICAL OR"}
  , Record {point = "02A57", uchar = "\10839", latex = "", unicodemath = "\\bigslopedvee", cls = "B", category = "mathbin", requirements = "", comments = "SLOPING LARGE OR"}
  , Record {point = "02A58", uchar = "\10840", latex = "", unicodemath = "\\bigslopedwedge", cls = "B", category = "mathbin", requirements = "", comments = "SLOPING LARGE AND"}
  , Record {point = "02A59", uchar = "\10841", latex = "", unicodemath = "\\veeonwedge", cls = "R", category = "mathrel", requirements = "", comments = "LOGICAL OR OVERLAPPING LOGICAL AND"}
  , Record {point = "02A5A", uchar = "\10842", latex = "", unicodemath = "\\wedgemidvert", cls = "B", category = "mathbin", requirements = "", comments = "LOGICAL AND WITH MIDDLE STEM"}
  , Record {point = "02A5B", uchar = "\10843", latex = "", unicodemath = "\\veemidvert", cls = "B", category = "mathbin", requirements = "", comments = "LOGICAL OR WITH MIDDLE STEM"}
  , Record {point = "02A5C", uchar = "\10844", latex = "", unicodemath = "\\midbarwedge", cls = "B", category = "mathbin", requirements = "", comments = "ogical and with horizontal dash"}
  , Record {point = "02A5D", uchar = "\10845", latex = "", unicodemath = "\\midbarvee", cls = "B", category = "mathbin", requirements = "", comments = "LOGICAL OR WITH HORIZONTAL DASH"}
  , Record {point = "02A5E", uchar = "\10846", latex = "\\doublebarwedge", unicodemath = "\\doublebarwedge", cls = "B", category = "mathbin", requirements = "amssymb", comments = "LOGICAL AND WITH DOUBLE OVERBAR"}
  , Record {point = "02A5F", uchar = "\10847", latex = "", unicodemath = "\\wedgebar", cls = "B", category = "mathbin", requirements = "", comments = "LOGICAL AND WITH UNDERBAR"}
  , Record {point = "02A60", uchar = "\10848", latex = "", unicodemath = "\\wedgedoublebar", cls = "B", category = "mathbin", requirements = "", comments = "LOGICAL AND WITH DOUBLE UNDERBAR"}
  , Record {point = "02A61", uchar = "\10849", latex = "", unicodemath = "\\varveebar", cls = "B", category = "mathbin", requirements = "", comments = "SMALL VEE WITH UNDERBAR"}
  , Record {point = "02A62", uchar = "\10850", latex = "", unicodemath = "\\doublebarvee", cls = "B", category = "mathbin", requirements = "", comments = "LOGICAL OR WITH DOUBLE OVERBAR"}
  , Record {point = "02A63", uchar = "\10851", latex = "", unicodemath = "\\veedoublebar", cls = "B", category = "mathbin", requirements = "", comments = "LOGICAL OR WITH DOUBLE UNDERBAR"}
  , Record {point = "02A64", uchar = "\10852", latex = "\\dsub", unicodemath = "\\dsub", cls = "B", category = "mathbin", requirements = "oz", comments = "= \\ndres (oz), Z NOTATION DOMAIN ANTIRESTRICTION"}
  , Record {point = "02A65", uchar = "\10853", latex = "\\rsub", unicodemath = "\\rsub", cls = "B", category = "mathbin", requirements = "oz", comments = "= \\nrres (oz), Z NOTATION RANGE ANTIRESTRICTION"}
  , Record {point = "02A66", uchar = "\10854", latex = "", unicodemath = "\\eqdot", cls = "R", category = "mathrel", requirements = "", comments = "EQUALS SIGN WITH DOT BELOW"}
  , Record {point = "02A67", uchar = "\10855", latex = "", unicodemath = "\\dotequiv", cls = "R", category = "mathrel", requirements = "", comments = "IDENTICAL WITH DOT ABOVE"}
  , Record {point = "02A68", uchar = "\10856", latex = "", unicodemath = "\\equivVert", cls = "R", category = "mathrel", requirements = "", comments = "TRIPLE HORIZONTAL BAR WITH DOUBLE VERTICAL STROKE"}
  , Record {point = "02A69", uchar = "\10857", latex = "", unicodemath = "\\equivVvert", cls = "R", category = "mathrel", requirements = "", comments = "TRIPLE HORIZONTAL BAR WITH TRIPLE VERTICAL STROKE"}
  , Record {point = "02A6A", uchar = "\10858", latex = "", unicodemath = "\\dotsim", cls = "R", category = "mathrel", requirements = "", comments = "TILDE OPERATOR WITH DOT ABOVE"}
  , Record {point = "02A6B", uchar = "\10859", latex = "", unicodemath = "\\simrdots", cls = "R", category = "mathrel", requirements = "", comments = "TILDE OPERATOR WITH RISING DOTS"}
  , Record {point = "02A6C", uchar = "\10860", latex = "", unicodemath = "\\simminussim", cls = "R", category = "mathrel", requirements = "", comments = "SIMILAR MINUS SIMILAR"}
  , Record {point = "02A6D", uchar = "\10861", latex = "", unicodemath = "\\congdot", cls = "R", category = "mathrel", requirements = "", comments = "CONGRUENT WITH DOT ABOVE"}
  , Record {point = "02A6E", uchar = "\10862", latex = "", unicodemath = "\\asteq", cls = "R", category = "mathrel", requirements = "", comments = "EQUALS WITH ASTERISK"}
  , Record {point = "02A6F", uchar = "\10863", latex = "", unicodemath = "\\hatapprox", cls = "R", category = "mathrel", requirements = "", comments = "ALMOST EQUAL TO WITH CIRCUMFLEX ACCENT"}
  , Record {point = "02A70", uchar = "\10864", latex = "", unicodemath = "\\approxeqq", cls = "R", category = "mathrel", requirements = "", comments = "APPROXIMATELY EQUAL OR EQUAL TO"}
  , Record {point = "02A71", uchar = "\10865", latex = "", unicodemath = "\\eqqplus", cls = "B", category = "mathbin", requirements = "", comments = "EQUALS SIGN ABOVE PLUS SIGN"}
  , Record {point = "02A72", uchar = "\10866", latex = "", unicodemath = "\\pluseqq", cls = "B", category = "mathbin", requirements = "", comments = "PLUS SIGN ABOVE EQUALS SIGN"}
  , Record {point = "02A73", uchar = "\10867", latex = "", unicodemath = "\\eqqsim", cls = "R", category = "mathrel", requirements = "", comments = "EQUALS SIGN ABOVE TILDE OPERATOR"}
  , Record {point = "02A74", uchar = "\10868", latex = "\\Coloneqq", unicodemath = "\\Coloneq", cls = "R", category = "mathrel", requirements = "txfonts", comments = "# ::=, x \\Coloneq (txfonts), DOUBLE COLON EQUAL"}
  , Record {point = "02A75", uchar = "\10869", latex = "\\Equal", unicodemath = "\\eqeq", cls = "R", category = "mathrel", requirements = "wrisym", comments = "# ==, TWO CONSECUTIVE EQUALS SIGNS"}
  , Record {point = "02A76", uchar = "\10870", latex = "\\Same", unicodemath = "\\eqeqeq", cls = "R", category = "mathrel", requirements = "wrisym", comments = "# ===, THREE CONSECUTIVE EQUALS SIGNS"}
  , Record {point = "02A77", uchar = "\10871", latex = "", unicodemath = "\\ddotseq", cls = "R", category = "mathrel", requirements = "", comments = "EQUALS SIGN WITH TWO DOTS ABOVE AND TWO DOTS BELOW"}
  , Record {point = "02A78", uchar = "\10872", latex = "", unicodemath = "\\equivDD", cls = "R", category = "mathrel", requirements = "", comments = "EQUIVALENT WITH FOUR DOTS ABOVE"}
  , Record {point = "02A79", uchar = "\10873", latex = "", unicodemath = "\\ltcir", cls = "R", category = "mathrel", requirements = "", comments = "LESS-THAN WITH CIRCLE INSIDE"}
  , Record {point = "02A7A", uchar = "\10874", latex = "", unicodemath = "\\gtcir", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN WITH CIRCLE INSIDE"}
  , Record {point = "02A7B", uchar = "\10875", latex = "", unicodemath = "\\ltquest", cls = "R", category = "mathrel", requirements = "", comments = "LESS-THAN WITH QUESTION MARK ABOVE"}
  , Record {point = "02A7C", uchar = "\10876", latex = "", unicodemath = "\\gtquest", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN WITH QUESTION MARK ABOVE"}
  , Record {point = "02A7D", uchar = "\10877", latex = "\\leqslant", unicodemath = "\\leqslant", cls = "R", category = "mathrel", requirements = "amssymb fourier", comments = "LESS-THAN OR SLANTED EQUAL TO"}
  , Record {point = "02A7E", uchar = "\10878", latex = "\\geqslant", unicodemath = "\\geqslant", cls = "R", category = "mathrel", requirements = "amssymb fourier", comments = "GREATER-THAN OR SLANTED EQUAL TO"}
  , Record {point = "02A7F", uchar = "\10879", latex = "", unicodemath = "\\lesdot", cls = "R", category = "mathrel", requirements = "", comments = "LESS-THAN OR SLANTED EQUAL TO WITH DOT INSIDE"}
  , Record {point = "02A80", uchar = "\10880", latex = "", unicodemath = "\\gesdot", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN OR SLANTED EQUAL TO WITH DOT INSIDE"}
  , Record {point = "02A81", uchar = "\10881", latex = "", unicodemath = "\\lesdoto", cls = "R", category = "mathrel", requirements = "", comments = "LESS-THAN OR SLANTED EQUAL TO WITH DOT ABOVE"}
  , Record {point = "02A82", uchar = "\10882", latex = "", unicodemath = "\\gesdoto", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN OR SLANTED EQUAL TO WITH DOT ABOVE"}
  , Record {point = "02A83", uchar = "\10883", latex = "", unicodemath = "\\lesdotor", cls = "R", category = "mathrel", requirements = "", comments = "LESS-THAN OR SLANTED EQUAL TO WITH DOT ABOVE RIGHT"}
  , Record {point = "02A84", uchar = "\10884", latex = "", unicodemath = "\\gesdotol", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN OR SLANTED EQUAL TO WITH DOT ABOVE LEFT"}
  , Record {point = "02A85", uchar = "\10885", latex = "\\lessapprox", unicodemath = "\\lessapprox", cls = "R", category = "mathrel", requirements = "amssymb", comments = "LESS-THAN OR APPROXIMATE"}
  , Record {point = "02A86", uchar = "\10886", latex = "\\gtrapprox", unicodemath = "\\gtrapprox", cls = "R", category = "mathrel", requirements = "amssymb", comments = "GREATER-THAN OR APPROXIMATE"}
  , Record {point = "02A87", uchar = "\10887", latex = "\\lneq", unicodemath = "\\lneq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "LESS-THAN AND SINGLE-LINE NOT EQUAL TO"}
  , Record {point = "02A88", uchar = "\10888", latex = "\\gneq", unicodemath = "\\gneq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "GREATER-THAN AND SINGLE-LINE NOT EQUAL TO"}
  , Record {point = "02A89", uchar = "\10889", latex = "\\lnapprox", unicodemath = "\\lnapprox", cls = "R", category = "mathrel", requirements = "amssymb", comments = "LESS-THAN AND NOT APPROXIMATE"}
  , Record {point = "02A8A", uchar = "\10890", latex = "\\gnapprox", unicodemath = "\\gnapprox", cls = "R", category = "mathrel", requirements = "amssymb", comments = "GREATER-THAN AND NOT APPROXIMATE"}
  , Record {point = "02A8B", uchar = "\10891", latex = "\\lesseqqgtr", unicodemath = "\\lesseqqgtr", cls = "R", category = "mathrel", requirements = "amssymb", comments = "LESS-THAN ABOVE DOUBLE-LINE EQUAL ABOVE GREATER-THAN"}
  , Record {point = "02A8C", uchar = "\10892", latex = "\\gtreqqless", unicodemath = "\\gtreqqless", cls = "R", category = "mathrel", requirements = "amssymb", comments = "GREATER-THAN ABOVE DOUBLE-LINE EQUAL ABOVE LESS-THAN"}
  , Record {point = "02A8D", uchar = "\10893", latex = "", unicodemath = "\\lsime", cls = "R", category = "mathrel", requirements = "", comments = "LESS-THAN ABOVE SIMILAR OR EQUAL"}
  , Record {point = "02A8E", uchar = "\10894", latex = "", unicodemath = "\\gsime", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN ABOVE SIMILAR OR EQUAL"}
  , Record {point = "02A8F", uchar = "\10895", latex = "", unicodemath = "\\lsimg", cls = "R", category = "mathrel", requirements = "", comments = "LESS-THAN ABOVE SIMILAR ABOVE GREATER-THAN"}
  , Record {point = "02A90", uchar = "\10896", latex = "", unicodemath = "\\gsiml", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN ABOVE SIMILAR ABOVE LESS-THAN"}
  , Record {point = "02A91", uchar = "\10897", latex = "", unicodemath = "\\lgE", cls = "R", category = "mathrel", requirements = "", comments = "LESS-THAN ABOVE GREATER-THAN ABOVE DOUBLE-LINE EQUAL"}
  , Record {point = "02A92", uchar = "\10898", latex = "", unicodemath = "\\glE", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN ABOVE LESS-THAN ABOVE DOUBLE-LINE EQUAL"}
  , Record {point = "02A93", uchar = "\10899", latex = "", unicodemath = "\\lesges", cls = "R", category = "mathrel", requirements = "", comments = "LESS-THAN ABOVE SLANTED EQUAL ABOVE GREATER-THAN ABOVE SLANTED EQUAL"}
  , Record {point = "02A94", uchar = "\10900", latex = "", unicodemath = "\\gesles", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN ABOVE SLANTED EQUAL ABOVE LESS-THAN ABOVE SLANTED EQUAL"}
  , Record {point = "02A95", uchar = "\10901", latex = "\\eqslantless", unicodemath = "\\eqslantless", cls = "R", category = "mathrel", requirements = "amssymb", comments = "SLANTED EQUAL TO OR LESS-THAN"}
  , Record {point = "02A96", uchar = "\10902", latex = "\\eqslantgtr", unicodemath = "\\eqslantgtr", cls = "R", category = "mathrel", requirements = "amssymb", comments = "SLANTED EQUAL TO OR GREATER-THAN"}
  , Record {point = "02A97", uchar = "\10903", latex = "", unicodemath = "\\elsdot", cls = "R", category = "mathrel", requirements = "", comments = "SLANTED EQUAL TO OR LESS-THAN WITH DOT INSIDE"}
  , Record {point = "02A98", uchar = "\10904", latex = "", unicodemath = "\\egsdot", cls = "R", category = "mathrel", requirements = "", comments = "SLANTED EQUAL TO OR GREATER-THAN WITH DOT INSIDE"}
  , Record {point = "02A99", uchar = "\10905", latex = "", unicodemath = "\\eqqless", cls = "R", category = "mathrel", requirements = "", comments = "DOUBLE-LINE EQUAL TO OR LESS-THAN"}
  , Record {point = "02A9A", uchar = "\10906", latex = "", unicodemath = "\\eqqgtr", cls = "R", category = "mathrel", requirements = "", comments = "DOUBLE-LINE EQUAL TO OR GREATER-THAN"}
  , Record {point = "02A9B", uchar = "\10907", latex = "", unicodemath = "\\eqqslantless", cls = "R", category = "mathrel", requirements = "", comments = "DOUBLE-LINE SLANTED EQUAL TO OR LESS-THAN"}
  , Record {point = "02A9C", uchar = "\10908", latex = "", unicodemath = "\\eqqslantgtr", cls = "R", category = "mathrel", requirements = "", comments = "DOUBLE-LINE SLANTED EQUAL TO OR GREATER-THAN"}
  , Record {point = "02A9D", uchar = "\10909", latex = "", unicodemath = "\\simless", cls = "R", category = "mathrel", requirements = "", comments = "SIMILAR OR LESS-THAN"}
  , Record {point = "02A9E", uchar = "\10910", latex = "", unicodemath = "\\simgtr", cls = "R", category = "mathrel", requirements = "", comments = "SIMILAR OR GREATER-THAN"}
  , Record {point = "02A9F", uchar = "\10911", latex = "", unicodemath = "\\simlE", cls = "R", category = "mathrel", requirements = "", comments = "SIMILAR ABOVE LESS-THAN ABOVE EQUALS SIGN"}
  , Record {point = "02AA0", uchar = "\10912", latex = "", unicodemath = "\\simgE", cls = "R", category = "mathrel", requirements = "", comments = "SIMILAR ABOVE GREATER-THAN ABOVE EQUALS SIGN"}
  , Record {point = "02AA1", uchar = "\10913", latex = "\\NestedLessLess", unicodemath = "\\Lt", cls = "R", category = "mathrel", requirements = "wrisym", comments = "= \\lll (mathabx -amssymb), DOUBLE NESTED LESS-THAN"}
  , Record {point = "02AA2", uchar = "\10914", latex = "\\NestedGreaterGreater", unicodemath = "\\Gt", cls = "R", category = "mathrel", requirements = "wrisym", comments = "= \\ggg (mathabx -amssymb), DOUBLE NESTED GREATER-THAN"}
  , Record {point = "02AA3", uchar = "\10915", latex = "", unicodemath = "\\partialmeetcontraction", cls = "R", category = "mathrel", requirements = "", comments = "double less-than with underbar"}
  , Record {point = "02AA4", uchar = "\10916", latex = "", unicodemath = "\\glj", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN OVERLAPPING LESS-THAN"}
  , Record {point = "02AA5", uchar = "\10917", latex = "", unicodemath = "\\gla", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN BESIDE LESS-THAN"}
  , Record {point = "02AA6", uchar = "\10918", latex = "\\leftslice", unicodemath = "\\ltcc", cls = "R", category = "mathrel", requirements = "stmaryrd", comments = "LESS-THAN CLOSED BY CURVE"}
  , Record {point = "02AA7", uchar = "\10919", latex = "\\rightslice", unicodemath = "\\gtcc", cls = "R", category = "mathrel", requirements = "stmaryrd", comments = "GREATER-THAN CLOSED BY CURVE"}
  , Record {point = "02AA8", uchar = "\10920", latex = "", unicodemath = "\\lescc", cls = "R", category = "mathrel", requirements = "", comments = "LESS-THAN CLOSED BY CURVE ABOVE SLANTED EQUAL"}
  , Record {point = "02AA9", uchar = "\10921", latex = "", unicodemath = "\\gescc", cls = "R", category = "mathrel", requirements = "", comments = "GREATER-THAN CLOSED BY CURVE ABOVE SLANTED EQUAL"}
  , Record {point = "02AAA", uchar = "\10922", latex = "", unicodemath = "\\smt", cls = "R", category = "mathrel", requirements = "", comments = "SMALLER THAN"}
  , Record {point = "02AAB", uchar = "\10923", latex = "", unicodemath = "\\lat", cls = "R", category = "mathrel", requirements = "", comments = "LARGER THAN"}
  , Record {point = "02AAC", uchar = "\10924", latex = "", unicodemath = "\\smte", cls = "R", category = "mathrel", requirements = "", comments = "SMALLER THAN OR EQUAL TO"}
  , Record {point = "02AAD", uchar = "\10925", latex = "", unicodemath = "\\late", cls = "R", category = "mathrel", requirements = "", comments = "LARGER THAN OR EQUAL TO"}
  , Record {point = "02AAE", uchar = "\10926", latex = "", unicodemath = "\\bumpeqq", cls = "R", category = "mathrel", requirements = "", comments = "EQUALS SIGN WITH BUMPY ABOVE"}
  , Record {point = "02AAF", uchar = "\10927", latex = "\\preceq", unicodemath = "\\preceq", cls = "R", category = "mathrel", requirements = "", comments = "PRECEDES ABOVE SINGLE-LINE EQUALS SIGN"}
  , Record {point = "02AB0", uchar = "\10928", latex = "\\succeq", unicodemath = "\\succeq", cls = "R", category = "mathrel", requirements = "", comments = "SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN"}
  , Record {point = "02AB1", uchar = "\10929", latex = "", unicodemath = "\\precneq", cls = "R", category = "mathrel", requirements = "", comments = "PRECEDES ABOVE SINGLE-LINE NOT EQUAL TO"}
  , Record {point = "02AB2", uchar = "\10930", latex = "", unicodemath = "\\succneq", cls = "R", category = "mathrel", requirements = "", comments = "SUCCEEDS ABOVE SINGLE-LINE NOT EQUAL TO"}
  , Record {point = "02AB3", uchar = "\10931", latex = "\\preceqq", unicodemath = "\\preceqq", cls = "R", category = "mathrel", requirements = "txfonts", comments = "PRECEDES ABOVE EQUALS SIGN"}
  , Record {point = "02AB4", uchar = "\10932", latex = "\\succeqq", unicodemath = "\\succeqq", cls = "R", category = "mathrel", requirements = "txfonts", comments = "SUCCEEDS ABOVE EQUALS SIGN"}
  , Record {point = "02AB5", uchar = "\10933", latex = "", unicodemath = "\\precneqq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "PRECEDES ABOVE NOT EQUAL TO"}
  , Record {point = "02AB6", uchar = "\10934", latex = "", unicodemath = "\\succneqq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "SUCCEEDS ABOVE NOT EQUAL TO"}
  , Record {point = "02AB7", uchar = "\10935", latex = "\\precapprox", unicodemath = "\\precapprox", cls = "R", category = "mathrel", requirements = "amssymb", comments = "PRECEDES ABOVE ALMOST EQUAL TO"}
  , Record {point = "02AB8", uchar = "\10936", latex = "\\succapprox", unicodemath = "\\succapprox", cls = "R", category = "mathrel", requirements = "amssymb", comments = "SUCCEEDS ABOVE ALMOST EQUAL TO"}
  , Record {point = "02AB9", uchar = "\10937", latex = "\\precnapprox", unicodemath = "\\precnapprox", cls = "R", category = "mathrel", requirements = "amssymb", comments = "PRECEDES ABOVE NOT ALMOST EQUAL TO"}
  , Record {point = "02ABA", uchar = "\10938", latex = "\\succnapprox", unicodemath = "\\succnapprox", cls = "R", category = "mathrel", requirements = "amssymb", comments = "SUCCEEDS ABOVE NOT ALMOST EQUAL TO"}
  , Record {point = "02ABB", uchar = "\10939", latex = "\\llcurly", unicodemath = "\\Prec", cls = "R", category = "mathrel", requirements = "mathabx", comments = "DOUBLE PRECEDES"}
  , Record {point = "02ABC", uchar = "\10940", latex = "\\ggcurly", unicodemath = "\\Succ", cls = "R", category = "mathrel", requirements = "mathabx", comments = "DOUBLE SUCCEEDS"}
  , Record {point = "02ABD", uchar = "\10941", latex = "", unicodemath = "\\subsetdot", cls = "R", category = "mathrel", requirements = "", comments = "SUBSET WITH DOT"}
  , Record {point = "02ABE", uchar = "\10942", latex = "", unicodemath = "\\supsetdot", cls = "R", category = "mathrel", requirements = "", comments = "SUPERSET WITH DOT"}
  , Record {point = "02ABF", uchar = "\10943", latex = "", unicodemath = "\\subsetplus", cls = "R", category = "mathrel", requirements = "", comments = "SUBSET WITH PLUS SIGN BELOW"}
  , Record {point = "02AC0", uchar = "\10944", latex = "", unicodemath = "\\supsetplus", cls = "R", category = "mathrel", requirements = "", comments = "SUPERSET WITH PLUS SIGN BELOW"}
  , Record {point = "02AC1", uchar = "\10945", latex = "", unicodemath = "\\submult", cls = "R", category = "mathrel", requirements = "", comments = "SUBSET WITH MULTIPLICATION SIGN BELOW"}
  , Record {point = "02AC2", uchar = "\10946", latex = "", unicodemath = "\\supmult", cls = "R", category = "mathrel", requirements = "", comments = "SUPERSET WITH MULTIPLICATION SIGN BELOW"}
  , Record {point = "02AC3", uchar = "\10947", latex = "", unicodemath = "\\subedot", cls = "R", category = "mathrel", requirements = "", comments = "SUBSET OF OR EQUAL TO WITH DOT ABOVE"}
  , Record {point = "02AC4", uchar = "\10948", latex = "", unicodemath = "\\supedot", cls = "R", category = "mathrel", requirements = "", comments = "SUPERSET OF OR EQUAL TO WITH DOT ABOVE"}
  , Record {point = "02AC5", uchar = "\10949", latex = "\\subseteqq", unicodemath = "\\subseteqq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "SUBSET OF ABOVE EQUALS SIGN"}
  , Record {point = "02AC6", uchar = "\10950", latex = "\\supseteqq", unicodemath = "\\supseteqq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "SUPERSET OF ABOVE EQUALS SIGN"}
  , Record {point = "02AC7", uchar = "\10951", latex = "", unicodemath = "\\subsim", cls = "R", category = "mathrel", requirements = "", comments = "SUBSET OF ABOVE TILDE OPERATOR"}
  , Record {point = "02AC8", uchar = "\10952", latex = "", unicodemath = "\\supsim", cls = "R", category = "mathrel", requirements = "", comments = "SUPERSET OF ABOVE TILDE OPERATOR"}
  , Record {point = "02AC9", uchar = "\10953", latex = "", unicodemath = "\\subsetapprox", cls = "R", category = "mathrel", requirements = "", comments = "SUBSET OF ABOVE ALMOST EQUAL TO"}
  , Record {point = "02ACA", uchar = "\10954", latex = "", unicodemath = "\\supsetapprox", cls = "R", category = "mathrel", requirements = "", comments = "SUPERSET OF ABOVE ALMOST EQUAL TO"}
  , Record {point = "02ACB", uchar = "\10955", latex = "\\subsetneqq", unicodemath = "\\subsetneqq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "SUBSET OF ABOVE NOT EQUAL TO"}
  , Record {point = "02ACC", uchar = "\10956", latex = "\\supsetneqq", unicodemath = "\\supsetneqq", cls = "R", category = "mathrel", requirements = "amssymb", comments = "SUPERSET OF ABOVE NOT EQUAL TO"}
  , Record {point = "02ACD", uchar = "\10957", latex = "", unicodemath = "\\lsqhook", cls = "R", category = "mathrel", requirements = "", comments = "SQUARE LEFT OPEN BOX OPERATOR"}
  , Record {point = "02ACE", uchar = "\10958", latex = "", unicodemath = "\\rsqhook", cls = "R", category = "mathrel", requirements = "", comments = "SQUARE RIGHT OPEN BOX OPERATOR"}
  , Record {point = "02ACF", uchar = "\10959", latex = "", unicodemath = "\\csub", cls = "R", category = "mathrel", requirements = "", comments = "CLOSED SUBSET"}
  , Record {point = "02AD0", uchar = "\10960", latex = "", unicodemath = "\\csup", cls = "R", category = "mathrel", requirements = "", comments = "CLOSED SUPERSET"}
  , Record {point = "02AD1", uchar = "\10961", latex = "", unicodemath = "\\csube", cls = "R", category = "mathrel", requirements = "", comments = "CLOSED SUBSET OR EQUAL TO"}
  , Record {point = "02AD2", uchar = "\10962", latex = "", unicodemath = "\\csupe", cls = "R", category = "mathrel", requirements = "", comments = "CLOSED SUPERSET OR EQUAL TO"}
  , Record {point = "02AD3", uchar = "\10963", latex = "", unicodemath = "\\subsup", cls = "R", category = "mathrel", requirements = "", comments = "SUBSET ABOVE SUPERSET"}
  , Record {point = "02AD4", uchar = "\10964", latex = "", unicodemath = "\\supsub", cls = "R", category = "mathrel", requirements = "", comments = "SUPERSET ABOVE SUBSET"}
  , Record {point = "02AD5", uchar = "\10965", latex = "", unicodemath = "\\subsub", cls = "R", category = "mathrel", requirements = "", comments = "SUBSET ABOVE SUBSET"}
  , Record {point = "02AD6", uchar = "\10966", latex = "", unicodemath = "\\supsup", cls = "R", category = "mathrel", requirements = "", comments = "SUPERSET ABOVE SUPERSET"}
  , Record {point = "02AD7", uchar = "\10967", latex = "", unicodemath = "\\suphsub", cls = "R", category = "mathrel", requirements = "", comments = "SUPERSET BESIDE SUBSET"}
  , Record {point = "02AD8", uchar = "\10968", latex = "", unicodemath = "\\supdsub", cls = "R", category = "mathrel", requirements = "", comments = "SUPERSET BESIDE AND JOINED BY DASH WITH SUBSET"}
  , Record {point = "02AD9", uchar = "\10969", latex = "", unicodemath = "\\forkv", cls = "R", category = "mathrel", requirements = "", comments = "ELEMENT OF OPENING DOWNWARDS"}
  , Record {point = "02ADA", uchar = "\10970", latex = "", unicodemath = "\\topfork", cls = "R", category = "mathrel", requirements = "", comments = "PITCHFORK WITH TEE TOP"}
  , Record {point = "02ADB", uchar = "\10971", latex = "", unicodemath = "\\mlcp", cls = "R", category = "mathrel", requirements = "", comments = "TRANSVERSAL INTERSECTION"}
  , Record {point = "02ADC", uchar = "\10972", latex = "", unicodemath = "\\forks", cls = "R", category = "mathrel", requirements = "", comments = "FORKING"}
  , Record {point = "02ADD", uchar = "\10973", latex = "", unicodemath = "\\forksnot", cls = "R", category = "mathrel", requirements = "", comments = "NONFORKING"}
  , Record {point = "02ADE", uchar = "\10974", latex = "", unicodemath = "\\shortlefttack", cls = "R", category = "mathrel", requirements = "", comments = "SHORT LEFT TACK"}
  , Record {point = "02ADF", uchar = "\10975", latex = "", unicodemath = "\\shortdowntack", cls = "R", category = "mathrel", requirements = "", comments = "SHORT DOWN TACK"}
  , Record {point = "02AE0", uchar = "\10976", latex = "", unicodemath = "\\shortuptack", cls = "R", category = "mathrel", requirements = "", comments = "SHORT UP TACK"}
  , Record {point = "02AE1", uchar = "\10977", latex = "", unicodemath = "\\perps", cls = "N", category = "mathord", requirements = "", comments = "PERPENDICULAR WITH S"}
  , Record {point = "02AE2", uchar = "\10978", latex = "", unicodemath = "\\vDdash", cls = "R", category = "mathrel", requirements = "", comments = "VERTICAL BAR TRIPLE RIGHT TURNSTILE"}
  , Record {point = "02AE3", uchar = "\10979", latex = "", unicodemath = "\\dashV", cls = "R", category = "mathrel", requirements = "", comments = "DOUBLE VERTICAL BAR LEFT TURNSTILE"}
  , Record {point = "02AE4", uchar = "\10980", latex = "", unicodemath = "\\Dashv", cls = "R", category = "mathrel", requirements = "", comments = "VERTICAL BAR DOUBLE LEFT TURNSTILE"}
  , Record {point = "02AE5", uchar = "\10981", latex = "", unicodemath = "\\DashV", cls = "R", category = "mathrel", requirements = "", comments = "DOUBLE VERTICAL BAR DOUBLE LEFT TURNSTILE"}
  , Record {point = "02AE6", uchar = "\10982", latex = "", unicodemath = "\\varVdash", cls = "R", category = "mathrel", requirements = "", comments = "LONG DASH FROM LEFT MEMBER OF DOUBLE VERTICAL"}
  , Record {point = "02AE7", uchar = "\10983", latex = "", unicodemath = "\\Barv", cls = "R", category = "mathrel", requirements = "", comments = "SHORT DOWN TACK WITH OVERBAR"}
  , Record {point = "02AE8", uchar = "\10984", latex = "", unicodemath = "\\vBar", cls = "R", category = "mathrel", requirements = "", comments = "SHORT UP TACK WITH UNDERBAR"}
  , Record {point = "02AE9", uchar = "\10985", latex = "", unicodemath = "\\vBarv", cls = "R", category = "mathrel", requirements = "", comments = "SHORT UP TACK ABOVE SHORT DOWN TACK"}
  , Record {point = "02AEA", uchar = "\10986", latex = "\\Top", unicodemath = "\\barV", cls = "R", category = "mathrel", requirements = "txfonts", comments = "DOUBLE DOWN TACK"}
  , Record {point = "02AEB", uchar = "\10987", latex = "\\Bot", unicodemath = "\\Vbar", cls = "R", category = "mathrel", requirements = "txfonts", comments = "= \\Perp (txfonts), DOUBLE UP TACK"}
  , Record {point = "02AEC", uchar = "\10988", latex = "", unicodemath = "\\Not", cls = "R", category = "mathrel", requirements = "", comments = "DOUBLE STROKE NOT SIGN"}
  , Record {point = "02AED", uchar = "\10989", latex = "", unicodemath = "\\bNot", cls = "R", category = "mathrel", requirements = "", comments = "REVERSED DOUBLE STROKE NOT SIGN"}
  , Record {point = "02AEE", uchar = "\10990", latex = "", unicodemath = "\\revnmid", cls = "R", category = "mathrel", requirements = "", comments = "DOES NOT DIVIDE WITH REVERSED NEGATION SLASH"}
  , Record {point = "02AEF", uchar = "\10991", latex = "", unicodemath = "\\cirmid", cls = "R", category = "mathrel", requirements = "", comments = "VERTICAL LINE WITH CIRCLE ABOVE"}
  , Record {point = "02AF0", uchar = "\10992", latex = "", unicodemath = "\\midcir", cls = "R", category = "mathrel", requirements = "", comments = "VERTICAL LINE WITH CIRCLE BELOW"}
  , Record {point = "02AF1", uchar = "\10993", latex = "", unicodemath = "\\topcir", cls = "N", category = "mathord", requirements = "", comments = "DOWN TACK WITH CIRCLE BELOW"}
  , Record {point = "02AF2", uchar = "\10994", latex = "", unicodemath = "\\nhpar", cls = "R", category = "mathrel", requirements = "", comments = "PARALLEL WITH HORIZONTAL STROKE"}
  , Record {point = "02AF3", uchar = "\10995", latex = "", unicodemath = "\\parsim", cls = "R", category = "mathrel", requirements = "", comments = "PARALLEL WITH TILDE OPERATOR"}
  , Record {point = "02AF4", uchar = "\10996", latex = "\\interleave", unicodemath = "\\interleave", cls = "B", category = "mathbin", requirements = "stmaryrd", comments = "TRIPLE VERTICAL BAR BINARY RELATION"}
  , Record {point = "02AF5", uchar = "\10997", latex = "", unicodemath = "\\nhVvert", cls = "B", category = "mathbin", requirements = "", comments = "TRIPLE VERTICAL BAR WITH HORIZONTAL STROKE"}
  , Record {point = "02AF6", uchar = "\10998", latex = "", unicodemath = "\\threedotcolon", cls = "B", category = "mathbin", requirements = "", comments = "TRIPLE COLON OPERATOR"}
  , Record {point = "02AF7", uchar = "\10999", latex = "", unicodemath = "\\lllnest", cls = "R", category = "mathrel", requirements = "", comments = "TRIPLE NESTED LESS-THAN"}
  , Record {point = "02AF8", uchar = "\11000", latex = "", unicodemath = "\\gggnest", cls = "R", category = "mathrel", requirements = "", comments = "TRIPLE NESTED GREATER-THAN"}
  , Record {point = "02AF9", uchar = "\11001", latex = "", unicodemath = "\\leqqslant", cls = "R", category = "mathrel", requirements = "", comments = "DOUBLE-LINE SLANTED LESS-THAN OR EQUAL TO"}
  , Record {point = "02AFA", uchar = "\11002", latex = "", unicodemath = "\\geqqslant", cls = "R", category = "mathrel", requirements = "", comments = "DOUBLE-LINE SLANTED GREATER-THAN OR EQUAL TO"}
  , Record {point = "02AFB", uchar = "\11003", latex = "", unicodemath = "\\trslash", cls = "B", category = "mathbin", requirements = "", comments = "TRIPLE SOLIDUS BINARY RELATION"}
  , Record {point = "02AFC", uchar = "\11004", latex = "\\biginterleave", unicodemath = "\\biginterleave", cls = "L", category = "mathop", requirements = "stmaryrd", comments = "LARGE TRIPLE VERTICAL BAR OPERATOR"}
  , Record {point = "02AFD", uchar = "\11005", latex = "\\sslash", unicodemath = "\\sslash", cls = "B", category = "mathbin", requirements = "stmaryrd", comments = "# \\varparallel (txfonts), DOUBLE SOLIDUS OPERATOR"}
  , Record {point = "02AFE", uchar = "\11006", latex = "\\talloblong", unicodemath = "\\talloblong", cls = "B", category = "mathbin", requirements = "stmaryrd", comments = "WHITE VERTICAL BAR"}
  , Record {point = "02AFF", uchar = "\11007", latex = "", unicodemath = "\\bigtalloblong", cls = "L", category = "mathop", requirements = "", comments = "N-ARY WHITE VERTICAL BAR"}
  , Record {point = "02B00", uchar = "\11008", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "NORTH EAST WHITE ARROW"}
  , Record {point = "02B01", uchar = "\11009", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "NORTH WEST WHITE ARROW"}
  , Record {point = "02B02", uchar = "\11010", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "SOUTH EAST WHITE ARROW"}
  , Record {point = "02B03", uchar = "\11011", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "SOUTH WEST WHITE ARROW"}
  , Record {point = "02B04", uchar = "\11012", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "LEFT RIGHT WHITE ARROW"}
  , Record {point = "02B05", uchar = "\11013", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "LEFTWARDS BLACK ARROW"}
  , Record {point = "02B06", uchar = "\11014", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "UPWARDS BLACK ARROW"}
  , Record {point = "02B07", uchar = "\11015", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "DOWNWARDS BLACK ARROW"}
  , Record {point = "02B08", uchar = "\11016", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "NORTH EAST BLACK ARROW"}
  , Record {point = "02B09", uchar = "\11017", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "NORTH WEST BLACK ARROW"}
  , Record {point = "02B0A", uchar = "\11018", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "SOUTH EAST BLACK ARROW"}
  , Record {point = "02B0B", uchar = "\11019", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "SOUTH WEST BLACK ARROW"}
  , Record {point = "02B0C", uchar = "\11020", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "LEFT RIGHT BLACK ARROW"}
  , Record {point = "02B0D", uchar = "\11021", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "UP DOWN BLACK ARROW"}
  , Record {point = "02B0E", uchar = "\11022", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "RIGHTWARDS ARROW WITH TIP DOWNWARDS"}
  , Record {point = "02B0F", uchar = "\11023", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "RIGHTWARDS ARROW WITH TIP UPWARDS"}
  , Record {point = "02B10", uchar = "\11024", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "LEFTWARDS ARROW WITH TIP DOWNWARDS"}
  , Record {point = "02B11", uchar = "\11025", latex = "", unicodemath = "", cls = "R?", category = "mathord", requirements = "", comments = "LEFTWARDS ARROW WITH TIP UPWARDS"}
  , Record {point = "02B12", uchar = "\11026", latex = "", unicodemath = "\\squaretopblack", cls = "N", category = "mathord", requirements = "", comments = "SQUARE WITH TOP HALF BLACK"}
  , Record {point = "02B13", uchar = "\11027", latex = "", unicodemath = "\\squarebotblack", cls = "N", category = "mathord", requirements = "", comments = "SQUARE WITH BOTTOM HALF BLACK"}
  , Record {point = "02B14", uchar = "\11028", latex = "", unicodemath = "\\squareurblack", cls = "N", category = "mathord", requirements = "", comments = "SQUARE WITH UPPER RIGHT DIAGONAL HALF BLACK"}
  , Record {point = "02B15", uchar = "\11029", latex = "", unicodemath = "\\squarellblack", cls = "N", category = "mathord", requirements = "", comments = "SQUARE WITH LOWER LEFT DIAGONAL HALF BLACK"}
  , Record {point = "02B16", uchar = "\11030", latex = "", unicodemath = "\\diamondleftblack", cls = "N", category = "mathord", requirements = "", comments = "DIAMOND WITH LEFT HALF BLACK"}
  , Record {point = "02B17", uchar = "\11031", latex = "", unicodemath = "\\diamondrightblack", cls = "N", category = "mathord", requirements = "", comments = "DIAMOND WITH RIGHT HALF BLACK"}
  , Record {point = "02B18", uchar = "\11032", latex = "", unicodemath = "\\diamondtopblack", cls = "N", category = "mathord", requirements = "", comments = "DIAMOND WITH TOP HALF BLACK"}
  , Record {point = "02B19", uchar = "\11033", latex = "", unicodemath = "\\diamondbotblack", cls = "N", category = "mathord", requirements = "", comments = "DIAMOND WITH BOTTOM HALF BLACK"}
  , Record {point = "02B1A", uchar = "\11034", latex = "", unicodemath = "\\dottedsquare", cls = "", category = "mathord", requirements = "", comments = "DOTTED SQUARE"}
  , Record {point = "02B1B", uchar = "\11035", latex = "\\blacksquare", unicodemath = "\\lgblksquare", cls = "", category = "mathord", requirements = "fourier -amssymb", comments = "BLACK LARGE SQUARE"}
  , Record {point = "02B1C", uchar = "\11036", latex = "\\square", unicodemath = "\\lgwhtsquare", cls = "", category = "mathord", requirements = "fourier -amssymb", comments = "WHITE LARGE SQUARE"}
  , Record {point = "02B1D", uchar = "\11037", latex = "", unicodemath = "\\vysmblksquare", cls = "", category = "mathord", requirements = "", comments = "# \\centerdot (amssymb), t \\Squaredot (marvosym), BLACK VERY SMALL SQUARE"}
  , Record {point = "02B1E", uchar = "\11038", latex = "", unicodemath = "\\vysmwhtsquare", cls = "", category = "mathord", requirements = "", comments = "WHITE VERY SMALL SQUARE"}
  , Record {point = "02B1F", uchar = "\11039", latex = "", unicodemath = "\\pentagonblack", cls = "", category = "mathord", requirements = "", comments = "BLACK PENTAGON"}
  , Record {point = "02B20", uchar = "\11040", latex = "", unicodemath = "\\pentagon", cls = "N", category = "mathord", requirements = "", comments = "WHITE PENTAGON"}
  , Record {point = "02B21", uchar = "\11041", latex = "", unicodemath = "\\varhexagon", cls = "N", category = "mathord", requirements = "", comments = "WHITE HEXAGON"}
  , Record {point = "02B22", uchar = "\11042", latex = "", unicodemath = "\\varhexagonblack", cls = "N", category = "mathord", requirements = "", comments = "BLACK HEXAGON"}
  , Record {point = "02B23", uchar = "\11043", latex = "", unicodemath = "\\hexagonblack", cls = "N", category = "mathord", requirements = "", comments = "HORIZONTAL BLACK HEXAGON"}
  , Record {point = "02B24", uchar = "\11044", latex = "", unicodemath = "\\lgblkcircle", cls = "", category = "mathord", requirements = "", comments = "BLACK LARGE CIRCLE"}
  , Record {point = "02B25", uchar = "\11045", latex = "", unicodemath = "\\mdblkdiamond", cls = "", category = "mathord", requirements = "", comments = "BLACK MEDIUM DIAMOND"}
  , Record {point = "02B26", uchar = "\11046", latex = "", unicodemath = "\\mdwhtdiamond", cls = "", category = "mathord", requirements = "", comments = "WHITE MEDIUM DIAMOND"}
  , Record {point = "02B27", uchar = "\11047", latex = "", unicodemath = "\\mdblklozenge", cls = "", category = "mathord", requirements = "", comments = "# \\blacklozenge (amssymb), BLACK MEDIUM LOZENGE"}
  , Record {point = "02B28", uchar = "\11048", latex = "", unicodemath = "\\mdwhtlozenge", cls = "", category = "mathord", requirements = "", comments = "# \\lozenge (amssymb), WHITE MEDIUM LOZENGE"}
  , Record {point = "02B29", uchar = "\11049", latex = "", unicodemath = "\\smblkdiamond", cls = "", category = "mathord", requirements = "", comments = "BLACK SMALL DIAMOND"}
  , Record {point = "02B2A", uchar = "\11050", latex = "", unicodemath = "\\smblklozenge", cls = "", category = "mathord", requirements = "", comments = "BLACK SMALL LOZENGE"}
  , Record {point = "02B2B", uchar = "\11051", latex = "", unicodemath = "\\smwhtlozenge", cls = "", category = "mathord", requirements = "", comments = "WHITE SMALL LOZENGE"}
  , Record {point = "02B2C", uchar = "\11052", latex = "", unicodemath = "\\blkhorzoval", cls = "", category = "mathord", requirements = "", comments = "BLACK HORIZONTAL ELLIPSE"}
  , Record {point = "02B2D", uchar = "\11053", latex = "", unicodemath = "\\whthorzoval", cls = "", category = "mathord", requirements = "", comments = "WHITE HORIZONTAL ELLIPSE"}
  , Record {point = "02B2E", uchar = "\11054", latex = "", unicodemath = "\\blkvertoval", cls = "", category = "mathord", requirements = "", comments = "BLACK VERTICAL ELLIPSE"}
  , Record {point = "02B2F", uchar = "\11055", latex = "", unicodemath = "\\whtvertoval", cls = "", category = "mathord", requirements = "", comments = "WHITE VERTICAL ELLIPSE"}
  , Record {point = "02B30", uchar = "\11056", latex = "", unicodemath = "\\circleonleftarrow", cls = "", category = "mathrel", requirements = "", comments = "LEFT ARROW WITH SMALL CIRCLE"}
  , Record {point = "02B31", uchar = "\11057", latex = "", unicodemath = "\\leftthreearrows", cls = "", category = "mathrel", requirements = "", comments = "THREE LEFTWARDS ARROWS"}
  , Record {point = "02B32", uchar = "\11058", latex = "", unicodemath = "\\leftarrowonoplus", cls = "", category = "mathrel", requirements = "", comments = "LEFT ARROW WITH CIRCLED PLUS"}
  , Record {point = "02B33", uchar = "\11059", latex = "", unicodemath = "\\longleftsquigarrow", cls = "", category = "mathrel", requirements = "", comments = "LONG LEFTWARDS SQUIGGLE ARROW"}
  , Record {point = "02B34", uchar = "\11060", latex = "", unicodemath = "\\nvtwoheadleftarrow", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE"}
  , Record {point = "02B35", uchar = "\11061", latex = "", unicodemath = "\\nVtwoheadleftarrow", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS TWO-HEADED ARROW WITH DOUBLE VERTICAL STROKE"}
  , Record {point = "02B36", uchar = "\11062", latex = "", unicodemath = "\\twoheadmapsfrom", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS TWO-HEADED ARROW FROM BAR"}
  , Record {point = "02B37", uchar = "\11063", latex = "", unicodemath = "\\twoheadleftdbkarrow", cls = "", category = "mathrel", requirements = "", comments = "leftwards two-headed triple-dash arrow"}
  , Record {point = "02B38", uchar = "\11064", latex = "", unicodemath = "\\leftdotarrow", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW WITH DOTTED STEM"}
  , Record {point = "02B39", uchar = "\11065", latex = "", unicodemath = "\\nvleftarrowtail", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW WITH TAIL WITH VERTICAL STROKE"}
  , Record {point = "02B3A", uchar = "\11066", latex = "", unicodemath = "\\nVleftarrowtail", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE"}
  , Record {point = "02B3B", uchar = "\11067", latex = "", unicodemath = "\\twoheadleftarrowtail", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS TWO-HEADED ARROW WITH TAIL"}
  , Record {point = "02B3C", uchar = "\11068", latex = "", unicodemath = "\\nvtwoheadleftarrowtail", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS TWO-HEADED ARROW WITH TAIL WITH VERTICAL STROKE"}
  , Record {point = "02B3D", uchar = "\11069", latex = "", unicodemath = "\\nVtwoheadleftarrowtail", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS TWO-HEADED ARROW WITH TAIL WITH DOUBLE VERTICAL STROKE"}
  , Record {point = "02B3E", uchar = "\11070", latex = "", unicodemath = "\\leftarrowx", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW THROUGH X"}
  , Record {point = "02B3F", uchar = "\11071", latex = "", unicodemath = "\\leftcurvedarrow", cls = "", category = "mathrel", requirements = "", comments = "WAVE ARROW POINTING DIRECTLY LEFT"}
  , Record {point = "02B40", uchar = "\11072", latex = "", unicodemath = "\\equalleftarrow", cls = "", category = "mathrel", requirements = "", comments = "EQUALS SIGN ABOVE LEFTWARDS ARROW"}
  , Record {point = "02B41", uchar = "\11073", latex = "", unicodemath = "\\bsimilarleftarrow", cls = "", category = "mathrel", requirements = "", comments = "REVERSE TILDE OPERATOR ABOVE LEFTWARDS ARROW"}
  , Record {point = "02B42", uchar = "\11074", latex = "", unicodemath = "\\leftarrowbackapprox", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW ABOVE REVERSE ALMOST EQUAL TO"}
  , Record {point = "02B43", uchar = "\11075", latex = "", unicodemath = "\\rightarrowgtr", cls = "", category = "mathrel", requirements = "", comments = "rightwards arrow through less-than"}
  , Record {point = "02B44", uchar = "\11076", latex = "", unicodemath = "\\rightarrowsupset", cls = "", category = "mathrel", requirements = "", comments = "rightwards arrow through subset"}
  , Record {point = "02B45", uchar = "\11077", latex = "", unicodemath = "\\LLeftarrow", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS QUADRUPLE ARROW"}
  , Record {point = "02B46", uchar = "\11078", latex = "", unicodemath = "\\RRightarrow", cls = "", category = "mathrel", requirements = "", comments = "RIGHTWARDS QUADRUPLE ARROW"}
  , Record {point = "02B47", uchar = "\11079", latex = "", unicodemath = "\\bsimilarrightarrow", cls = "", category = "mathrel", requirements = "", comments = "REVERSE TILDE OPERATOR ABOVE RIGHTWARDS ARROW"}
  , Record {point = "02B48", uchar = "\11080", latex = "", unicodemath = "\\rightarrowbackapprox", cls = "", category = "mathrel", requirements = "", comments = "RIGHTWARDS ARROW ABOVE REVERSE ALMOST EQUAL TO"}
  , Record {point = "02B49", uchar = "\11081", latex = "", unicodemath = "\\similarleftarrow", cls = "", category = "mathrel", requirements = "", comments = "TILDE OPERATOR ABOVE LEFTWARDS ARROW"}
  , Record {point = "02B4A", uchar = "\11082", latex = "", unicodemath = "\\leftarrowapprox", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW ABOVE ALMOST EQUAL TO"}
  , Record {point = "02B4B", uchar = "\11083", latex = "", unicodemath = "\\leftarrowbsimilar", cls = "", category = "mathrel", requirements = "", comments = "LEFTWARDS ARROW ABOVE REVERSE TILDE OPERATOR"}
  , Record {point = "02B4C", uchar = "\11084", latex = "", unicodemath = "\\rightarrowbsimilar", cls = "", category = "mathrel", requirements = "", comments = "righttwards arrow above reverse tilde operator"}
  , Record {point = "02B50", uchar = "\11088", latex = "", unicodemath = "\\medwhitestar", cls = "", category = "mathord", requirements = "", comments = "WHITE MEDIUM STAR"}
  , Record {point = "02B51", uchar = "\11089", latex = "", unicodemath = "\\medblackstar", cls = "", category = "mathord", requirements = "", comments = "black medium star"}
  , Record {point = "02B52", uchar = "\11090", latex = "", unicodemath = "\\smwhitestar", cls = "", category = "mathord", requirements = "", comments = "WHITE SMALL STAR"}
  , Record {point = "02B53", uchar = "\11091", latex = "", unicodemath = "\\rightpentagonblack", cls = "", category = "mathord", requirements = "", comments = "BLACK RIGHT-POINTING PENTAGON"}
  , Record {point = "02B54", uchar = "\11092", latex = "", unicodemath = "\\rightpentagon", cls = "", category = "mathord", requirements = "", comments = "WHITE RIGHT-POINTING PENTAGON"}
  , Record {point = "03008", uchar = "\12296", latex = "", unicodemath = "", cls = "X", category = "mathopen", requirements = "", comments = "# \\langle, LEFT ANGLE BRACKET (deprecated for math use)"}
  , Record {point = "03009", uchar = "\12297", latex = "", unicodemath = "", cls = "X", category = "mathclose", requirements = "", comments = "# \\rangle, RIGHT ANGLE BRACKET (deprecated for math use)"}
  , Record {point = "03012", uchar = "\12306", latex = "", unicodemath = "\\postalmark", cls = "", category = "mathord", requirements = "", comments = "POSTAL MARK"}
  , Record {point = "03014", uchar = "\12308", latex = "", unicodemath = "\\lbrbrak", cls = "", category = "mathopen", requirements = "", comments = "left broken bracket"}
  , Record {point = "03015", uchar = "\12309", latex = "", unicodemath = "\\rbrbrak", cls = "", category = "mathclose", requirements = "", comments = "right broken bracket"}
  , Record {point = "03018", uchar = "\12312", latex = "", unicodemath = "\\Lbrbrak", cls = "", category = "mathopen", requirements = "", comments = "LEFT WHITE TORTOISE SHELL BRACKET"}
  , Record {point = "03019", uchar = "\12313", latex = "", unicodemath = "\\Rbrbrak", cls = "", category = "mathclose", requirements = "", comments = "RIGHT WHITE TORTOISE SHELL BRACKET"}
  , Record {point = "0301A", uchar = "\12314", latex = "", unicodemath = "", cls = "X", category = "mathopen", requirements = "", comments = "# \\llbracket (stmaryrd), LEFT WHITE SQUARE BRACKET (deprecated for math use)"}
  , Record {point = "0301B", uchar = "\12315", latex = "", unicodemath = "", cls = "X", category = "mathclose", requirements = "", comments = "# \\rrbracket (stmaryrd), RIGHT WHITE SQUARE BRACKET (deprecated for math use)"}
  , Record {point = "03030", uchar = "\12336", latex = "", unicodemath = "\\hzigzag", cls = "", category = "mathord", requirements = "", comments = "zigzag"}
  , Record {point = "0306E", uchar = "\12398", latex = "", unicodemath = "", cls = "N", category = "mathalpha", requirements = "", comments = "HIRAGANA LETTER NO"}
  , Record {point = "0FB29", uchar = "\64297", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "HEBREW LETTER ALTERNATIVE PLUS SIGN (doesn't have cross shape)"}
  , Record {point = "0FE00", uchar = "\65024", latex = "", unicodemath = "", cls = "D", category = "mathaccent", requirements = "", comments = "VARIATION SELECTOR-1"}
  , Record {point = "0FE61", uchar = "\65121", latex = "", unicodemath = "", cls = "X", category = "", requirements = "", comments = "SMALL ASTERISK"}
  , Record {point = "0FE62", uchar = "\65122", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "SMALL PLUS SIGN"}
  , Record {point = "0FE63", uchar = "\65123", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "SMALL HYPHEN-MINUS"}
  , Record {point = "0FE64", uchar = "\65124", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "SMALL LESS-THAN SIGN"}
  , Record {point = "0FE65", uchar = "\65125", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "SMALL GREATER-THAN SIGN"}
  , Record {point = "0FE66", uchar = "\65126", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "SMALL EQUALS SIGN"}
  , Record {point = "0FE68", uchar = "\65128", latex = "", unicodemath = "", cls = "X", category = "", requirements = "", comments = "SMALL REVERSE SOLIDUS"}
  , Record {point = "0FF0B", uchar = "\65291", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "FULLWIDTH PLUS SIGN"}
  , Record {point = "0FF1C", uchar = "\65308", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "FULLWIDTH LESS-THAN SIGN"}
  , Record {point = "0FF1D", uchar = "\65309", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "FULLWIDTH EQUALS SIGN"}
  , Record {point = "0FF1E", uchar = "\65310", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "FULLWIDTH GREATER-THAN SIGN"}
  , Record {point = "0FF3C", uchar = "\65340", latex = "", unicodemath = "", cls = "X", category = "", requirements = "", comments = "FULLWIDTH REVERSE SOLIDUS"}
  , Record {point = "0FF3E", uchar = "\65342", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "FULLWIDTH CIRCUMFLEX ACCENT"}
  , Record {point = "0FF5C", uchar = "\65372", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "FULLWIDTH VERTICAL LINE"}
  , Record {point = "0FF5E", uchar = "\65374", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "FULLWIDTH TILDE"}
  , Record {point = "0FFE2", uchar = "\65506", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "FULLWIDTH NOT SIGN"}
  , Record {point = "0FFE9", uchar = "\65513", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "HALFWIDTH LEFTWARDS ARROW"}
  , Record {point = "0FFEA", uchar = "\65514", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "HALFWIDTH UPWARDS ARROW"}
  , Record {point = "0FFEB", uchar = "\65515", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "HALFWIDTH RIGHTWARDS ARROW"}
  , Record {point = "0FFEC", uchar = "\65516", latex = "", unicodemath = "", cls = "X", category = "mathord", requirements = "", comments = "HALFWIDTH DOWNWARDS ARROW"}
  , Record {point = "1D400", uchar = "\119808", latex = "\\mathbf{A}", unicodemath = "\\mbfA", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL A"}
  , Record {point = "1D401", uchar = "\119809", latex = "\\mathbf{B}", unicodemath = "\\mbfB", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL B"}
  , Record {point = "1D402", uchar = "\119810", latex = "\\mathbf{C}", unicodemath = "\\mbfC", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL C"}
  , Record {point = "1D403", uchar = "\119811", latex = "\\mathbf{D}", unicodemath = "\\mbfD", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL D"}
  , Record {point = "1D404", uchar = "\119812", latex = "\\mathbf{E}", unicodemath = "\\mbfE", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL E"}
  , Record {point = "1D405", uchar = "\119813", latex = "\\mathbf{F}", unicodemath = "\\mbfF", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL F"}
  , Record {point = "1D406", uchar = "\119814", latex = "\\mathbf{G}", unicodemath = "\\mbfG", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL G"}
  , Record {point = "1D407", uchar = "\119815", latex = "\\mathbf{H}", unicodemath = "\\mbfH", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL H"}
  , Record {point = "1D408", uchar = "\119816", latex = "\\mathbf{I}", unicodemath = "\\mbfI", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL I"}
  , Record {point = "1D409", uchar = "\119817", latex = "\\mathbf{J}", unicodemath = "\\mbfJ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL J"}
  , Record {point = "1D40A", uchar = "\119818", latex = "\\mathbf{K}", unicodemath = "\\mbfK", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL K"}
  , Record {point = "1D40B", uchar = "\119819", latex = "\\mathbf{L}", unicodemath = "\\mbfL", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL L"}
  , Record {point = "1D40C", uchar = "\119820", latex = "\\mathbf{M}", unicodemath = "\\mbfM", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL M"}
  , Record {point = "1D40D", uchar = "\119821", latex = "\\mathbf{N}", unicodemath = "\\mbfN", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL N"}
  , Record {point = "1D40E", uchar = "\119822", latex = "\\mathbf{O}", unicodemath = "\\mbfO", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL O"}
  , Record {point = "1D40F", uchar = "\119823", latex = "\\mathbf{P}", unicodemath = "\\mbfP", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL P"}
  , Record {point = "1D410", uchar = "\119824", latex = "\\mathbf{Q}", unicodemath = "\\mbfQ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL Q"}
  , Record {point = "1D411", uchar = "\119825", latex = "\\mathbf{R}", unicodemath = "\\mbfR", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL R"}
  , Record {point = "1D412", uchar = "\119826", latex = "\\mathbf{S}", unicodemath = "\\mbfS", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL S"}
  , Record {point = "1D413", uchar = "\119827", latex = "\\mathbf{T}", unicodemath = "\\mbfT", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL T"}
  , Record {point = "1D414", uchar = "\119828", latex = "\\mathbf{U}", unicodemath = "\\mbfU", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL U"}
  , Record {point = "1D415", uchar = "\119829", latex = "\\mathbf{V}", unicodemath = "\\mbfV", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL V"}
  , Record {point = "1D416", uchar = "\119830", latex = "\\mathbf{W}", unicodemath = "\\mbfW", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL W"}
  , Record {point = "1D417", uchar = "\119831", latex = "\\mathbf{X}", unicodemath = "\\mbfX", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL X"}
  , Record {point = "1D418", uchar = "\119832", latex = "\\mathbf{Y}", unicodemath = "\\mbfY", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL Y"}
  , Record {point = "1D419", uchar = "\119833", latex = "\\mathbf{Z}", unicodemath = "\\mbfZ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL Z"}
  , Record {point = "1D41A", uchar = "\119834", latex = "\\mathbf{a}", unicodemath = "\\mbfa", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL A"}
  , Record {point = "1D41B", uchar = "\119835", latex = "\\mathbf{b}", unicodemath = "\\mbfb", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL B"}
  , Record {point = "1D41C", uchar = "\119836", latex = "\\mathbf{c}", unicodemath = "\\mbfc", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL C"}
  , Record {point = "1D41D", uchar = "\119837", latex = "\\mathbf{d}", unicodemath = "\\mbfd", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL D"}
  , Record {point = "1D41E", uchar = "\119838", latex = "\\mathbf{e}", unicodemath = "\\mbfe", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL E"}
  , Record {point = "1D41F", uchar = "\119839", latex = "\\mathbf{f}", unicodemath = "\\mbff", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL F"}
  , Record {point = "1D420", uchar = "\119840", latex = "\\mathbf{g}", unicodemath = "\\mbfg", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL G"}
  , Record {point = "1D421", uchar = "\119841", latex = "\\mathbf{h}", unicodemath = "\\mbfh", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL H"}
  , Record {point = "1D422", uchar = "\119842", latex = "\\mathbf{i}", unicodemath = "\\mbfi", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL I"}
  , Record {point = "1D423", uchar = "\119843", latex = "\\mathbf{j}", unicodemath = "\\mbfj", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL J"}
  , Record {point = "1D424", uchar = "\119844", latex = "\\mathbf{k}", unicodemath = "\\mbfk", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL K"}
  , Record {point = "1D425", uchar = "\119845", latex = "\\mathbf{l}", unicodemath = "\\mbfl", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL L"}
  , Record {point = "1D426", uchar = "\119846", latex = "\\mathbf{m}", unicodemath = "\\mbfm", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL M"}
  , Record {point = "1D427", uchar = "\119847", latex = "\\mathbf{n}", unicodemath = "\\mbfn", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL N"}
  , Record {point = "1D428", uchar = "\119848", latex = "\\mathbf{o}", unicodemath = "\\mbfo", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL O"}
  , Record {point = "1D429", uchar = "\119849", latex = "\\mathbf{p}", unicodemath = "\\mbfp", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL P"}
  , Record {point = "1D42A", uchar = "\119850", latex = "\\mathbf{q}", unicodemath = "\\mbfq", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL Q"}
  , Record {point = "1D42B", uchar = "\119851", latex = "\\mathbf{r}", unicodemath = "\\mbfr", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL R"}
  , Record {point = "1D42C", uchar = "\119852", latex = "\\mathbf{s}", unicodemath = "\\mbfs", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL S"}
  , Record {point = "1D42D", uchar = "\119853", latex = "\\mathbf{t}", unicodemath = "\\mbft", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL T"}
  , Record {point = "1D42E", uchar = "\119854", latex = "\\mathbf{u}", unicodemath = "\\mbfu", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL U"}
  , Record {point = "1D42F", uchar = "\119855", latex = "\\mathbf{v}", unicodemath = "\\mbfv", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL V"}
  , Record {point = "1D430", uchar = "\119856", latex = "\\mathbf{w}", unicodemath = "\\mbfw", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL W"}
  , Record {point = "1D431", uchar = "\119857", latex = "\\mathbf{x}", unicodemath = "\\mbfx", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL X"}
  , Record {point = "1D432", uchar = "\119858", latex = "\\mathbf{y}", unicodemath = "\\mbfy", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL Y"}
  , Record {point = "1D433", uchar = "\119859", latex = "\\mathbf{z}", unicodemath = "\\mbfz", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL Z"}
  , Record {point = "1D434", uchar = "\119860", latex = "A", unicodemath = "\\mitA", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{A}, MATHEMATICAL ITALIC CAPITAL A"}
  , Record {point = "1D435", uchar = "\119861", latex = "B", unicodemath = "\\mitB", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{B}, MATHEMATICAL ITALIC CAPITAL B"}
  , Record {point = "1D436", uchar = "\119862", latex = "C", unicodemath = "\\mitC", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{C}, MATHEMATICAL ITALIC CAPITAL C"}
  , Record {point = "1D437", uchar = "\119863", latex = "D", unicodemath = "\\mitD", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{D}, MATHEMATICAL ITALIC CAPITAL D"}
  , Record {point = "1D438", uchar = "\119864", latex = "E", unicodemath = "\\mitE", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{E}, MATHEMATICAL ITALIC CAPITAL E"}
  , Record {point = "1D439", uchar = "\119865", latex = "F", unicodemath = "\\mitF", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{F}, MATHEMATICAL ITALIC CAPITAL F"}
  , Record {point = "1D43A", uchar = "\119866", latex = "G", unicodemath = "\\mitG", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{G}, MATHEMATICAL ITALIC CAPITAL G"}
  , Record {point = "1D43B", uchar = "\119867", latex = "H", unicodemath = "\\mitH", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{H}, MATHEMATICAL ITALIC CAPITAL H"}
  , Record {point = "1D43C", uchar = "\119868", latex = "I", unicodemath = "\\mitI", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{I}, MATHEMATICAL ITALIC CAPITAL I"}
  , Record {point = "1D43D", uchar = "\119869", latex = "J", unicodemath = "\\mitJ", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{J}, MATHEMATICAL ITALIC CAPITAL J"}
  , Record {point = "1D43E", uchar = "\119870", latex = "K", unicodemath = "\\mitK", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{K}, MATHEMATICAL ITALIC CAPITAL K"}
  , Record {point = "1D43F", uchar = "\119871", latex = "L", unicodemath = "\\mitL", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{L}, MATHEMATICAL ITALIC CAPITAL L"}
  , Record {point = "1D440", uchar = "\119872", latex = "M", unicodemath = "\\mitM", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{M}, MATHEMATICAL ITALIC CAPITAL M"}
  , Record {point = "1D441", uchar = "\119873", latex = "N", unicodemath = "\\mitN", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{N}, MATHEMATICAL ITALIC CAPITAL N"}
  , Record {point = "1D442", uchar = "\119874", latex = "O", unicodemath = "\\mitO", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{O}, MATHEMATICAL ITALIC CAPITAL O"}
  , Record {point = "1D443", uchar = "\119875", latex = "P", unicodemath = "\\mitP", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{P}, MATHEMATICAL ITALIC CAPITAL P"}
  , Record {point = "1D444", uchar = "\119876", latex = "Q", unicodemath = "\\mitQ", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{Q}, MATHEMATICAL ITALIC CAPITAL Q"}
  , Record {point = "1D445", uchar = "\119877", latex = "R", unicodemath = "\\mitR", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{R}, MATHEMATICAL ITALIC CAPITAL R"}
  , Record {point = "1D446", uchar = "\119878", latex = "S", unicodemath = "\\mitS", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{S}, MATHEMATICAL ITALIC CAPITAL S"}
  , Record {point = "1D447", uchar = "\119879", latex = "T", unicodemath = "\\mitT", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{T}, MATHEMATICAL ITALIC CAPITAL T"}
  , Record {point = "1D448", uchar = "\119880", latex = "U", unicodemath = "\\mitU", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{U}, MATHEMATICAL ITALIC CAPITAL U"}
  , Record {point = "1D449", uchar = "\119881", latex = "V", unicodemath = "\\mitV", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{V}, MATHEMATICAL ITALIC CAPITAL V"}
  , Record {point = "1D44A", uchar = "\119882", latex = "W", unicodemath = "\\mitW", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{W}, MATHEMATICAL ITALIC CAPITAL W"}
  , Record {point = "1D44B", uchar = "\119883", latex = "X", unicodemath = "\\mitX", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{X}, MATHEMATICAL ITALIC CAPITAL X"}
  , Record {point = "1D44C", uchar = "\119884", latex = "Y", unicodemath = "\\mitY", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{Y}, MATHEMATICAL ITALIC CAPITAL Y"}
  , Record {point = "1D44D", uchar = "\119885", latex = "Z", unicodemath = "\\mitZ", cls = "A", category = "mathalpha", requirements = "-frenchstyle", comments = "= \\mathit{Z}, MATHEMATICAL ITALIC CAPITAL Z"}
  , Record {point = "1D44E", uchar = "\119886", latex = "a", unicodemath = "\\mita", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{a}, MATHEMATICAL ITALIC SMALL A"}
  , Record {point = "1D44F", uchar = "\119887", latex = "b", unicodemath = "\\mitb", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{b}, MATHEMATICAL ITALIC SMALL B"}
  , Record {point = "1D450", uchar = "\119888", latex = "c", unicodemath = "\\mitc", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{c}, MATHEMATICAL ITALIC SMALL C"}
  , Record {point = "1D451", uchar = "\119889", latex = "d", unicodemath = "\\mitd", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{d}, MATHEMATICAL ITALIC SMALL D"}
  , Record {point = "1D452", uchar = "\119890", latex = "e", unicodemath = "\\mite", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{e}, MATHEMATICAL ITALIC SMALL E"}
  , Record {point = "1D453", uchar = "\119891", latex = "f", unicodemath = "\\mitf", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{f}, MATHEMATICAL ITALIC SMALL F"}
  , Record {point = "1D454", uchar = "\119892", latex = "g", unicodemath = "\\mitg", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{g}, MATHEMATICAL ITALIC SMALL G"}
  , Record {point = "1D456", uchar = "\119894", latex = "i", unicodemath = "\\miti", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{i}, MATHEMATICAL ITALIC SMALL I"}
  , Record {point = "1D457", uchar = "\119895", latex = "j", unicodemath = "\\mitj", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{j}, MATHEMATICAL ITALIC SMALL J"}
  , Record {point = "1D458", uchar = "\119896", latex = "k", unicodemath = "\\mitk", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{k}, MATHEMATICAL ITALIC SMALL K"}
  , Record {point = "1D459", uchar = "\119897", latex = "l", unicodemath = "\\mitl", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{l}, MATHEMATICAL ITALIC SMALL L"}
  , Record {point = "1D45A", uchar = "\119898", latex = "m", unicodemath = "\\mitm", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{m}, MATHEMATICAL ITALIC SMALL M"}
  , Record {point = "1D45B", uchar = "\119899", latex = "n", unicodemath = "\\mitn", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{n}, MATHEMATICAL ITALIC SMALL N"}
  , Record {point = "1D45C", uchar = "\119900", latex = "o", unicodemath = "\\mito", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{o}, MATHEMATICAL ITALIC SMALL O"}
  , Record {point = "1D45D", uchar = "\119901", latex = "p", unicodemath = "\\mitp", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{p}, MATHEMATICAL ITALIC SMALL P"}
  , Record {point = "1D45E", uchar = "\119902", latex = "q", unicodemath = "\\mitq", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{q}, MATHEMATICAL ITALIC SMALL Q"}
  , Record {point = "1D45F", uchar = "\119903", latex = "r", unicodemath = "\\mitr", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{r}, MATHEMATICAL ITALIC SMALL R"}
  , Record {point = "1D460", uchar = "\119904", latex = "s", unicodemath = "\\mits", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{s}, MATHEMATICAL ITALIC SMALL S"}
  , Record {point = "1D461", uchar = "\119905", latex = "t", unicodemath = "\\mitt", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{t}, MATHEMATICAL ITALIC SMALL T"}
  , Record {point = "1D462", uchar = "\119906", latex = "u", unicodemath = "\\mitu", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{u}, MATHEMATICAL ITALIC SMALL U"}
  , Record {point = "1D463", uchar = "\119907", latex = "v", unicodemath = "\\mitv", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{v}, MATHEMATICAL ITALIC SMALL V"}
  , Record {point = "1D464", uchar = "\119908", latex = "w", unicodemath = "\\mitw", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{w}, MATHEMATICAL ITALIC SMALL W"}
  , Record {point = "1D465", uchar = "\119909", latex = "x", unicodemath = "\\mitx", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{x}, MATHEMATICAL ITALIC SMALL X"}
  , Record {point = "1D466", uchar = "\119910", latex = "y", unicodemath = "\\mity", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{y}, MATHEMATICAL ITALIC SMALL Y"}
  , Record {point = "1D467", uchar = "\119911", latex = "z", unicodemath = "\\mitz", cls = "A", category = "mathalpha", requirements = "-uprightstyle", comments = "= \\mathit{z}, MATHEMATICAL ITALIC SMALL Z"}
  , Record {point = "1D468", uchar = "\119912", latex = "\\mathbfit{A}", unicodemath = "\\mbfitA", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{A} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL A"}
  , Record {point = "1D469", uchar = "\119913", latex = "\\mathbfit{B}", unicodemath = "\\mbfitB", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{B} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL B"}
  , Record {point = "1D46A", uchar = "\119914", latex = "\\mathbfit{C}", unicodemath = "\\mbfitC", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{C} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL C"}
  , Record {point = "1D46B", uchar = "\119915", latex = "\\mathbfit{D}", unicodemath = "\\mbfitD", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{D} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL D"}
  , Record {point = "1D46C", uchar = "\119916", latex = "\\mathbfit{E}", unicodemath = "\\mbfitE", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{E} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL E"}
  , Record {point = "1D46D", uchar = "\119917", latex = "\\mathbfit{F}", unicodemath = "\\mbfitF", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{F} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL F"}
  , Record {point = "1D46E", uchar = "\119918", latex = "\\mathbfit{G}", unicodemath = "\\mbfitG", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{G} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL G"}
  , Record {point = "1D46F", uchar = "\119919", latex = "\\mathbfit{H}", unicodemath = "\\mbfitH", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{H} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL H"}
  , Record {point = "1D470", uchar = "\119920", latex = "\\mathbfit{I}", unicodemath = "\\mbfitI", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{I} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL I"}
  , Record {point = "1D471", uchar = "\119921", latex = "\\mathbfit{J}", unicodemath = "\\mbfitJ", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{J} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL J"}
  , Record {point = "1D472", uchar = "\119922", latex = "\\mathbfit{K}", unicodemath = "\\mbfitK", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{K} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL K"}
  , Record {point = "1D473", uchar = "\119923", latex = "\\mathbfit{L}", unicodemath = "\\mbfitL", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{L} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL L"}
  , Record {point = "1D474", uchar = "\119924", latex = "\\mathbfit{M}", unicodemath = "\\mbfitM", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{M} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL M"}
  , Record {point = "1D475", uchar = "\119925", latex = "\\mathbfit{N}", unicodemath = "\\mbfitN", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{N} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL N"}
  , Record {point = "1D476", uchar = "\119926", latex = "\\mathbfit{O}", unicodemath = "\\mbfitO", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{O} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL O"}
  , Record {point = "1D477", uchar = "\119927", latex = "\\mathbfit{P}", unicodemath = "\\mbfitP", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{P} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL P"}
  , Record {point = "1D478", uchar = "\119928", latex = "\\mathbfit{Q}", unicodemath = "\\mbfitQ", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{Q} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL Q"}
  , Record {point = "1D479", uchar = "\119929", latex = "\\mathbfit{R}", unicodemath = "\\mbfitR", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{R} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL R"}
  , Record {point = "1D47A", uchar = "\119930", latex = "\\mathbfit{S}", unicodemath = "\\mbfitS", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{S} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL S"}
  , Record {point = "1D47B", uchar = "\119931", latex = "\\mathbfit{T}", unicodemath = "\\mbfitT", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{T} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL T"}
  , Record {point = "1D47C", uchar = "\119932", latex = "\\mathbfit{U}", unicodemath = "\\mbfitU", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{U} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL U"}
  , Record {point = "1D47D", uchar = "\119933", latex = "\\mathbfit{V}", unicodemath = "\\mbfitV", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{V} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL V"}
  , Record {point = "1D47E", uchar = "\119934", latex = "\\mathbfit{W}", unicodemath = "\\mbfitW", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{W} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL W"}
  , Record {point = "1D47F", uchar = "\119935", latex = "\\mathbfit{X}", unicodemath = "\\mbfitX", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{X} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL X"}
  , Record {point = "1D480", uchar = "\119936", latex = "\\mathbfit{Y}", unicodemath = "\\mbfitY", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{Y} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL Y"}
  , Record {point = "1D481", uchar = "\119937", latex = "\\mathbfit{Z}", unicodemath = "\\mbfitZ", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{Z} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL Z"}
  , Record {point = "1D482", uchar = "\119938", latex = "\\mathbfit{a}", unicodemath = "\\mbfita", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{a} (fixmath), MATHEMATICAL BOLD ITALIC SMALL A"}
  , Record {point = "1D483", uchar = "\119939", latex = "\\mathbfit{b}", unicodemath = "\\mbfitb", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{b} (fixmath), MATHEMATICAL BOLD ITALIC SMALL B"}
  , Record {point = "1D484", uchar = "\119940", latex = "\\mathbfit{c}", unicodemath = "\\mbfitc", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{c} (fixmath), MATHEMATICAL BOLD ITALIC SMALL C"}
  , Record {point = "1D485", uchar = "\119941", latex = "\\mathbfit{d}", unicodemath = "\\mbfitd", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{d} (fixmath), MATHEMATICAL BOLD ITALIC SMALL D"}
  , Record {point = "1D486", uchar = "\119942", latex = "\\mathbfit{e}", unicodemath = "\\mbfite", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{e} (fixmath), MATHEMATICAL BOLD ITALIC SMALL E"}
  , Record {point = "1D487", uchar = "\119943", latex = "\\mathbfit{f}", unicodemath = "\\mbfitf", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{f} (fixmath), MATHEMATICAL BOLD ITALIC SMALL F"}
  , Record {point = "1D488", uchar = "\119944", latex = "\\mathbfit{g}", unicodemath = "\\mbfitg", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{g} (fixmath), MATHEMATICAL BOLD ITALIC SMALL G"}
  , Record {point = "1D489", uchar = "\119945", latex = "\\mathbfit{h}", unicodemath = "\\mbfith", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{h} (fixmath), MATHEMATICAL BOLD ITALIC SMALL H"}
  , Record {point = "1D48A", uchar = "\119946", latex = "\\mathbfit{i}", unicodemath = "\\mbfiti", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{i} (fixmath), MATHEMATICAL BOLD ITALIC SMALL I"}
  , Record {point = "1D48B", uchar = "\119947", latex = "\\mathbfit{j}", unicodemath = "\\mbfitj", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{j} (fixmath), MATHEMATICAL BOLD ITALIC SMALL J"}
  , Record {point = "1D48C", uchar = "\119948", latex = "\\mathbfit{k}", unicodemath = "\\mbfitk", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{k} (fixmath), MATHEMATICAL BOLD ITALIC SMALL K"}
  , Record {point = "1D48D", uchar = "\119949", latex = "\\mathbfit{l}", unicodemath = "\\mbfitl", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{l} (fixmath), MATHEMATICAL BOLD ITALIC SMALL L"}
  , Record {point = "1D48E", uchar = "\119950", latex = "\\mathbfit{m}", unicodemath = "\\mbfitm", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{m} (fixmath), MATHEMATICAL BOLD ITALIC SMALL M"}
  , Record {point = "1D48F", uchar = "\119951", latex = "\\mathbfit{n}", unicodemath = "\\mbfitn", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{n} (fixmath), MATHEMATICAL BOLD ITALIC SMALL N"}
  , Record {point = "1D490", uchar = "\119952", latex = "\\mathbfit{o}", unicodemath = "\\mbfito", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{o} (fixmath), MATHEMATICAL BOLD ITALIC SMALL O"}
  , Record {point = "1D491", uchar = "\119953", latex = "\\mathbfit{p}", unicodemath = "\\mbfitp", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{p} (fixmath), MATHEMATICAL BOLD ITALIC SMALL P"}
  , Record {point = "1D492", uchar = "\119954", latex = "\\mathbfit{q}", unicodemath = "\\mbfitq", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{q} (fixmath), MATHEMATICAL BOLD ITALIC SMALL Q"}
  , Record {point = "1D493", uchar = "\119955", latex = "\\mathbfit{r}", unicodemath = "\\mbfitr", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{r} (fixmath), MATHEMATICAL BOLD ITALIC SMALL R"}
  , Record {point = "1D494", uchar = "\119956", latex = "\\mathbfit{s}", unicodemath = "\\mbfits", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{s} (fixmath), MATHEMATICAL BOLD ITALIC SMALL S"}
  , Record {point = "1D495", uchar = "\119957", latex = "\\mathbfit{t}", unicodemath = "\\mbfitt", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{t} (fixmath), MATHEMATICAL BOLD ITALIC SMALL T"}
  , Record {point = "1D496", uchar = "\119958", latex = "\\mathbfit{u}", unicodemath = "\\mbfitu", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{u} (fixmath), MATHEMATICAL BOLD ITALIC SMALL U"}
  , Record {point = "1D497", uchar = "\119959", latex = "\\mathbfit{v}", unicodemath = "\\mbfitv", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{v} (fixmath), MATHEMATICAL BOLD ITALIC SMALL V"}
  , Record {point = "1D498", uchar = "\119960", latex = "\\mathbfit{w}", unicodemath = "\\mbfitw", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{w} (fixmath), MATHEMATICAL BOLD ITALIC SMALL W"}
  , Record {point = "1D499", uchar = "\119961", latex = "\\mathbfit{x}", unicodemath = "\\mbfitx", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{x} (fixmath), MATHEMATICAL BOLD ITALIC SMALL X"}
  , Record {point = "1D49A", uchar = "\119962", latex = "\\mathbfit{y}", unicodemath = "\\mbfity", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{y} (fixmath), MATHEMATICAL BOLD ITALIC SMALL Y"}
  , Record {point = "1D49B", uchar = "\119963", latex = "\\mathbfit{z}", unicodemath = "\\mbfitz", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{z} (fixmath), MATHEMATICAL BOLD ITALIC SMALL Z"}
  , Record {point = "1D49C", uchar = "\119964", latex = "\\mathcal{A}", unicodemath = "\\mscrA", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL A"}
  , Record {point = "1D49E", uchar = "\119966", latex = "\\mathcal{C}", unicodemath = "\\mscrC", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL C"}
  , Record {point = "1D49F", uchar = "\119967", latex = "\\mathcal{D}", unicodemath = "\\mscrD", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL D"}
  , Record {point = "1D4A2", uchar = "\119970", latex = "\\mathcal{G}", unicodemath = "\\mscrG", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL G"}
  , Record {point = "1D4A5", uchar = "\119973", latex = "\\mathcal{J}", unicodemath = "\\mscrJ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL J"}
  , Record {point = "1D4A6", uchar = "\119974", latex = "\\mathcal{K}", unicodemath = "\\mscrK", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL K"}
  , Record {point = "1D4A9", uchar = "\119977", latex = "\\mathcal{N}", unicodemath = "\\mscrN", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL N"}
  , Record {point = "1D4AA", uchar = "\119978", latex = "\\mathcal{O}", unicodemath = "\\mscrO", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL O"}
  , Record {point = "1D4AB", uchar = "\119979", latex = "\\mathcal{P}", unicodemath = "\\mscrP", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL P"}
  , Record {point = "1D4AC", uchar = "\119980", latex = "\\mathcal{Q}", unicodemath = "\\mscrQ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL Q"}
  , Record {point = "1D4AE", uchar = "\119982", latex = "\\mathcal{S}", unicodemath = "\\mscrS", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL S"}
  , Record {point = "1D4AF", uchar = "\119983", latex = "\\mathcal{T}", unicodemath = "\\mscrT", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL T"}
  , Record {point = "1D4B0", uchar = "\119984", latex = "\\mathcal{U}", unicodemath = "\\mscrU", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL U"}
  , Record {point = "1D4B1", uchar = "\119985", latex = "\\mathcal{V}", unicodemath = "\\mscrV", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL V"}
  , Record {point = "1D4B2", uchar = "\119986", latex = "\\mathcal{W}", unicodemath = "\\mscrW", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL W"}
  , Record {point = "1D4B3", uchar = "\119987", latex = "\\mathcal{X}", unicodemath = "\\mscrX", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL X"}
  , Record {point = "1D4B4", uchar = "\119988", latex = "\\mathcal{Y}", unicodemath = "\\mscrY", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL Y"}
  , Record {point = "1D4B5", uchar = "\119989", latex = "\\mathcal{Z}", unicodemath = "\\mscrZ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SCRIPT CAPITAL Z"}
  , Record {point = "1D4B6", uchar = "\119990", latex = "\\mathcal{a}", unicodemath = "\\mscra", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL A"}
  , Record {point = "1D4B7", uchar = "\119991", latex = "\\mathcal{b}", unicodemath = "\\mscrb", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL B"}
  , Record {point = "1D4B8", uchar = "\119992", latex = "\\mathcal{c}", unicodemath = "\\mscrc", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL C"}
  , Record {point = "1D4B9", uchar = "\119993", latex = "\\mathcal{d}", unicodemath = "\\mscrd", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL D"}
  , Record {point = "1D4BB", uchar = "\119995", latex = "\\mathcal{f}", unicodemath = "\\mscrf", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL F"}
  , Record {point = "1D4BD", uchar = "\119997", latex = "\\mathcal{h}", unicodemath = "\\mscrh", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL H"}
  , Record {point = "1D4BE", uchar = "\119998", latex = "\\mathcal{i}", unicodemath = "\\mscri", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL I"}
  , Record {point = "1D4BF", uchar = "\119999", latex = "\\mathcal{j}", unicodemath = "\\mscrj", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL J"}
  , Record {point = "1D4C0", uchar = "\120000", latex = "\\mathcal{k}", unicodemath = "\\mscrk", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL K"}
  , Record {point = "1D4C1", uchar = "\120001", latex = "\\mathcal{l}", unicodemath = "\\mscrl", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL L"}
  , Record {point = "1D4C2", uchar = "\120002", latex = "\\mathcal{m}", unicodemath = "\\mscrm", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL M"}
  , Record {point = "1D4C3", uchar = "\120003", latex = "\\mathcal{n}", unicodemath = "\\mscrn", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL N"}
  , Record {point = "1D4C5", uchar = "\120005", latex = "\\mathcal{p}", unicodemath = "\\mscrp", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL P"}
  , Record {point = "1D4C6", uchar = "\120006", latex = "\\mathcal{q}", unicodemath = "\\mscrq", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL Q"}
  , Record {point = "1D4C7", uchar = "\120007", latex = "\\mathcal{r}", unicodemath = "\\mscrr", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL R"}
  , Record {point = "1D4C8", uchar = "\120008", latex = "\\mathcal{s}", unicodemath = "\\mscrs", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL S"}
  , Record {point = "1D4C9", uchar = "\120009", latex = "\\mathcal{t}", unicodemath = "\\mscrt", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL T"}
  , Record {point = "1D4CA", uchar = "\120010", latex = "\\mathcal{u}", unicodemath = "\\mscru", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL U"}
  , Record {point = "1D4CB", uchar = "\120011", latex = "\\mathcal{v}", unicodemath = "\\mscrv", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL V"}
  , Record {point = "1D4CC", uchar = "\120012", latex = "\\mathcal{w}", unicodemath = "\\mscrw", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL W"}
  , Record {point = "1D4CD", uchar = "\120013", latex = "\\mathcal{x}", unicodemath = "\\mscrx", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL X"}
  , Record {point = "1D4CE", uchar = "\120014", latex = "\\mathcal{y}", unicodemath = "\\mscry", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL Y"}
  , Record {point = "1D4CF", uchar = "\120015", latex = "\\mathcal{z}", unicodemath = "\\mscrz", cls = "A", category = "mathalpha", requirements = "urwchancal", comments = "MATHEMATICAL SCRIPT SMALL Z"}
  , Record {point = "1D4D0", uchar = "\120016", latex = "", unicodemath = "\\mbfscrA", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL A"}
  , Record {point = "1D4D1", uchar = "\120017", latex = "", unicodemath = "\\mbfscrB", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL B"}
  , Record {point = "1D4D2", uchar = "\120018", latex = "", unicodemath = "\\mbfscrC", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL C"}
  , Record {point = "1D4D3", uchar = "\120019", latex = "", unicodemath = "\\mbfscrD", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL D"}
  , Record {point = "1D4D4", uchar = "\120020", latex = "", unicodemath = "\\mbfscrE", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL E"}
  , Record {point = "1D4D5", uchar = "\120021", latex = "", unicodemath = "\\mbfscrF", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL F"}
  , Record {point = "1D4D6", uchar = "\120022", latex = "", unicodemath = "\\mbfscrG", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL G"}
  , Record {point = "1D4D7", uchar = "\120023", latex = "", unicodemath = "\\mbfscrH", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL H"}
  , Record {point = "1D4D8", uchar = "\120024", latex = "", unicodemath = "\\mbfscrI", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL I"}
  , Record {point = "1D4D9", uchar = "\120025", latex = "", unicodemath = "\\mbfscrJ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL J"}
  , Record {point = "1D4DA", uchar = "\120026", latex = "", unicodemath = "\\mbfscrK", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL K"}
  , Record {point = "1D4DB", uchar = "\120027", latex = "", unicodemath = "\\mbfscrL", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL L"}
  , Record {point = "1D4DC", uchar = "\120028", latex = "", unicodemath = "\\mbfscrM", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL M"}
  , Record {point = "1D4DD", uchar = "\120029", latex = "", unicodemath = "\\mbfscrN", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL N"}
  , Record {point = "1D4DE", uchar = "\120030", latex = "", unicodemath = "\\mbfscrO", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL O"}
  , Record {point = "1D4DF", uchar = "\120031", latex = "", unicodemath = "\\mbfscrP", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL P"}
  , Record {point = "1D4E0", uchar = "\120032", latex = "", unicodemath = "\\mbfscrQ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL Q"}
  , Record {point = "1D4E1", uchar = "\120033", latex = "", unicodemath = "\\mbfscrR", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL R"}
  , Record {point = "1D4E2", uchar = "\120034", latex = "", unicodemath = "\\mbfscrS", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL S"}
  , Record {point = "1D4E3", uchar = "\120035", latex = "", unicodemath = "\\mbfscrT", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL T"}
  , Record {point = "1D4E4", uchar = "\120036", latex = "", unicodemath = "\\mbfscrU", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL U"}
  , Record {point = "1D4E5", uchar = "\120037", latex = "", unicodemath = "\\mbfscrV", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL V"}
  , Record {point = "1D4E6", uchar = "\120038", latex = "", unicodemath = "\\mbfscrW", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL W"}
  , Record {point = "1D4E7", uchar = "\120039", latex = "", unicodemath = "\\mbfscrX", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL X"}
  , Record {point = "1D4E8", uchar = "\120040", latex = "", unicodemath = "\\mbfscrY", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL Y"}
  , Record {point = "1D4E9", uchar = "\120041", latex = "", unicodemath = "\\mbfscrZ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT CAPITAL Z"}
  , Record {point = "1D4EA", uchar = "\120042", latex = "", unicodemath = "\\mbfscra", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL A"}
  , Record {point = "1D4EB", uchar = "\120043", latex = "", unicodemath = "\\mbfscrb", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL B"}
  , Record {point = "1D4EC", uchar = "\120044", latex = "", unicodemath = "\\mbfscrc", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL C"}
  , Record {point = "1D4ED", uchar = "\120045", latex = "", unicodemath = "\\mbfscrd", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL D"}
  , Record {point = "1D4EE", uchar = "\120046", latex = "", unicodemath = "\\mbfscre", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL E"}
  , Record {point = "1D4EF", uchar = "\120047", latex = "", unicodemath = "\\mbfscrf", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL F"}
  , Record {point = "1D4F0", uchar = "\120048", latex = "", unicodemath = "\\mbfscrg", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL G"}
  , Record {point = "1D4F1", uchar = "\120049", latex = "", unicodemath = "\\mbfscrh", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL H"}
  , Record {point = "1D4F2", uchar = "\120050", latex = "", unicodemath = "\\mbfscri", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL I"}
  , Record {point = "1D4F3", uchar = "\120051", latex = "", unicodemath = "\\mbfscrj", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL J"}
  , Record {point = "1D4F4", uchar = "\120052", latex = "", unicodemath = "\\mbfscrk", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL K"}
  , Record {point = "1D4F5", uchar = "\120053", latex = "", unicodemath = "\\mbfscrl", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL L"}
  , Record {point = "1D4F6", uchar = "\120054", latex = "", unicodemath = "\\mbfscrm", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL M"}
  , Record {point = "1D4F7", uchar = "\120055", latex = "", unicodemath = "\\mbfscrn", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL N"}
  , Record {point = "1D4F8", uchar = "\120056", latex = "", unicodemath = "\\mbfscro", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL O"}
  , Record {point = "1D4F9", uchar = "\120057", latex = "", unicodemath = "\\mbfscrp", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL P"}
  , Record {point = "1D4FA", uchar = "\120058", latex = "", unicodemath = "\\mbfscrq", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL Q"}
  , Record {point = "1D4FB", uchar = "\120059", latex = "", unicodemath = "\\mbfscrr", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL R"}
  , Record {point = "1D4FC", uchar = "\120060", latex = "", unicodemath = "\\mbfscrs", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL S"}
  , Record {point = "1D4FD", uchar = "\120061", latex = "", unicodemath = "\\mbfscrt", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL T"}
  , Record {point = "1D4FE", uchar = "\120062", latex = "", unicodemath = "\\mbfscru", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL U"}
  , Record {point = "1D4FF", uchar = "\120063", latex = "", unicodemath = "\\mbfscrv", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL V"}
  , Record {point = "1D500", uchar = "\120064", latex = "", unicodemath = "\\mbfscrw", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL W"}
  , Record {point = "1D501", uchar = "\120065", latex = "", unicodemath = "\\mbfscrx", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL X"}
  , Record {point = "1D502", uchar = "\120066", latex = "", unicodemath = "\\mbfscry", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL Y"}
  , Record {point = "1D503", uchar = "\120067", latex = "", unicodemath = "\\mbfscrz", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SCRIPT SMALL Z"}
  , Record {point = "1D504", uchar = "\120068", latex = "\\mathfrak{A}", unicodemath = "\\mfrakA", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL A"}
  , Record {point = "1D505", uchar = "\120069", latex = "\\mathfrak{B}", unicodemath = "\\mfrakB", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL B"}
  , Record {point = "1D507", uchar = "\120071", latex = "\\mathfrak{D}", unicodemath = "\\mfrakD", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL D"}
  , Record {point = "1D508", uchar = "\120072", latex = "\\mathfrak{E}", unicodemath = "\\mfrakE", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL E"}
  , Record {point = "1D509", uchar = "\120073", latex = "\\mathfrak{F}", unicodemath = "\\mfrakF", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL F"}
  , Record {point = "1D50A", uchar = "\120074", latex = "\\mathfrak{G}", unicodemath = "\\mfrakG", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL G"}
  , Record {point = "1D50D", uchar = "\120077", latex = "\\mathfrak{J}", unicodemath = "\\mfrakJ", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL J"}
  , Record {point = "1D50E", uchar = "\120078", latex = "\\mathfrak{K}", unicodemath = "\\mfrakK", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL K"}
  , Record {point = "1D50F", uchar = "\120079", latex = "\\mathfrak{L}", unicodemath = "\\mfrakL", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL L"}
  , Record {point = "1D510", uchar = "\120080", latex = "\\mathfrak{M}", unicodemath = "\\mfrakM", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL M"}
  , Record {point = "1D511", uchar = "\120081", latex = "\\mathfrak{N}", unicodemath = "\\mfrakN", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL N"}
  , Record {point = "1D512", uchar = "\120082", latex = "\\mathfrak{O}", unicodemath = "\\mfrakO", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL O"}
  , Record {point = "1D513", uchar = "\120083", latex = "\\mathfrak{P}", unicodemath = "\\mfrakP", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL P"}
  , Record {point = "1D514", uchar = "\120084", latex = "\\mathfrak{Q}", unicodemath = "\\mfrakQ", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL Q"}
  , Record {point = "1D516", uchar = "\120086", latex = "\\mathfrak{S}", unicodemath = "\\mfrakS", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL S"}
  , Record {point = "1D517", uchar = "\120087", latex = "\\mathfrak{T}", unicodemath = "\\mfrakT", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL T"}
  , Record {point = "1D518", uchar = "\120088", latex = "\\mathfrak{U}", unicodemath = "\\mfrakU", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL U"}
  , Record {point = "1D519", uchar = "\120089", latex = "\\mathfrak{V}", unicodemath = "\\mfrakV", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL V"}
  , Record {point = "1D51A", uchar = "\120090", latex = "\\mathfrak{W}", unicodemath = "\\mfrakW", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL W"}
  , Record {point = "1D51B", uchar = "\120091", latex = "\\mathfrak{X}", unicodemath = "\\mfrakX", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL X"}
  , Record {point = "1D51C", uchar = "\120092", latex = "\\mathfrak{Y}", unicodemath = "\\mfrakY", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR CAPITAL Y"}
  , Record {point = "1D51E", uchar = "\120094", latex = "\\mathfrak{a}", unicodemath = "\\mfraka", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL A"}
  , Record {point = "1D51F", uchar = "\120095", latex = "\\mathfrak{b}", unicodemath = "\\mfrakb", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL B"}
  , Record {point = "1D520", uchar = "\120096", latex = "\\mathfrak{c}", unicodemath = "\\mfrakc", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL C"}
  , Record {point = "1D521", uchar = "\120097", latex = "\\mathfrak{d}", unicodemath = "\\mfrakd", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL D"}
  , Record {point = "1D522", uchar = "\120098", latex = "\\mathfrak{e}", unicodemath = "\\mfrake", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL E"}
  , Record {point = "1D523", uchar = "\120099", latex = "\\mathfrak{f}", unicodemath = "\\mfrakf", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL F"}
  , Record {point = "1D524", uchar = "\120100", latex = "\\mathfrak{g}", unicodemath = "\\mfrakg", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL G"}
  , Record {point = "1D525", uchar = "\120101", latex = "\\mathfrak{h}", unicodemath = "\\mfrakh", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL H"}
  , Record {point = "1D526", uchar = "\120102", latex = "\\mathfrak{i}", unicodemath = "\\mfraki", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL I"}
  , Record {point = "1D527", uchar = "\120103", latex = "\\mathfrak{j}", unicodemath = "\\mfrakj", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL J"}
  , Record {point = "1D528", uchar = "\120104", latex = "\\mathfrak{k}", unicodemath = "\\mfrakk", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL K"}
  , Record {point = "1D529", uchar = "\120105", latex = "\\mathfrak{l}", unicodemath = "\\mfrakl", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL L"}
  , Record {point = "1D52A", uchar = "\120106", latex = "\\mathfrak{m}", unicodemath = "\\mfrakm", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL M"}
  , Record {point = "1D52B", uchar = "\120107", latex = "\\mathfrak{n}", unicodemath = "\\mfrakn", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL N"}
  , Record {point = "1D52C", uchar = "\120108", latex = "\\mathfrak{o}", unicodemath = "\\mfrako", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL O"}
  , Record {point = "1D52D", uchar = "\120109", latex = "\\mathfrak{p}", unicodemath = "\\mfrakp", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL P"}
  , Record {point = "1D52E", uchar = "\120110", latex = "\\mathfrak{q}", unicodemath = "\\mfrakq", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL Q"}
  , Record {point = "1D52F", uchar = "\120111", latex = "\\mathfrak{r}", unicodemath = "\\mfrakr", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL R"}
  , Record {point = "1D530", uchar = "\120112", latex = "\\mathfrak{s}", unicodemath = "\\mfraks", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL S"}
  , Record {point = "1D531", uchar = "\120113", latex = "\\mathfrak{t}", unicodemath = "\\mfrakt", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL T"}
  , Record {point = "1D532", uchar = "\120114", latex = "\\mathfrak{u}", unicodemath = "\\mfraku", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL U"}
  , Record {point = "1D533", uchar = "\120115", latex = "\\mathfrak{v}", unicodemath = "\\mfrakv", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL V"}
  , Record {point = "1D534", uchar = "\120116", latex = "\\mathfrak{w}", unicodemath = "\\mfrakw", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL W"}
  , Record {point = "1D535", uchar = "\120117", latex = "\\mathfrak{x}", unicodemath = "\\mfrakx", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL X"}
  , Record {point = "1D536", uchar = "\120118", latex = "\\mathfrak{y}", unicodemath = "\\mfraky", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL Y"}
  , Record {point = "1D537", uchar = "\120119", latex = "\\mathfrak{z}", unicodemath = "\\mfrakz", cls = "A", category = "mathalpha", requirements = "eufrak", comments = "MATHEMATICAL FRAKTUR SMALL Z"}
  , Record {point = "1D538", uchar = "\120120", latex = "\\mathbb{A}", unicodemath = "\\BbbA", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{A} (dsfont), MATHEMATICAL DOUBLE-STRUCK CAPITAL A"}
  , Record {point = "1D539", uchar = "\120121", latex = "\\mathbb{B}", unicodemath = "\\BbbB", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{B} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL B"}
  , Record {point = "1D53B", uchar = "\120123", latex = "\\mathbb{D}", unicodemath = "\\BbbD", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{D} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL D"}
  , Record {point = "1D53C", uchar = "\120124", latex = "\\mathbb{E}", unicodemath = "\\BbbE", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{E} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL E"}
  , Record {point = "1D53D", uchar = "\120125", latex = "\\mathbb{F}", unicodemath = "\\BbbF", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{F} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL F"}
  , Record {point = "1D53E", uchar = "\120126", latex = "\\mathbb{G}", unicodemath = "\\BbbG", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{G} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL G"}
  , Record {point = "1D540", uchar = "\120128", latex = "\\mathbb{I}", unicodemath = "\\BbbI", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{I} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL I"}
  , Record {point = "1D541", uchar = "\120129", latex = "\\mathbb{J}", unicodemath = "\\BbbJ", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{J} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL J"}
  , Record {point = "1D542", uchar = "\120130", latex = "\\mathbb{K}", unicodemath = "\\BbbK", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{K} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL K"}
  , Record {point = "1D543", uchar = "\120131", latex = "\\mathbb{L}", unicodemath = "\\BbbL", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{L} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL L"}
  , Record {point = "1D544", uchar = "\120132", latex = "\\mathbb{M}", unicodemath = "\\BbbM", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{M} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL M"}
  , Record {point = "1D546", uchar = "\120134", latex = "\\mathbb{O}", unicodemath = "\\BbbO", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{O} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL O"}
  , Record {point = "1D54A", uchar = "\120138", latex = "\\mathbb{S}", unicodemath = "\\BbbS", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{S} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL S"}
  , Record {point = "1D54B", uchar = "\120139", latex = "\\mathbb{T}", unicodemath = "\\BbbT", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{T} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL T"}
  , Record {point = "1D54C", uchar = "\120140", latex = "\\mathbb{U}", unicodemath = "\\BbbU", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{U} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL U"}
  , Record {point = "1D54D", uchar = "\120141", latex = "\\mathbb{V}", unicodemath = "\\BbbV", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{V} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL V"}
  , Record {point = "1D54E", uchar = "\120142", latex = "\\mathbb{W}", unicodemath = "\\BbbW", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{W} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL W"}
  , Record {point = "1D54F", uchar = "\120143", latex = "\\mathbb{X}", unicodemath = "\\BbbX", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{X} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL X"}
  , Record {point = "1D550", uchar = "\120144", latex = "\\mathbb{Y}", unicodemath = "\\BbbY", cls = "A", category = "mathalpha", requirements = "mathbb", comments = "= \\mathds{Y} (dsfont), matMATHEMATICAL DOUBLE-STRUCK CAPITAL Y"}
  , Record {point = "1D552", uchar = "\120146", latex = "\\mathbb{a}", unicodemath = "\\Bbba", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL A"}
  , Record {point = "1D553", uchar = "\120147", latex = "\\mathbb{b}", unicodemath = "\\Bbbb", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL B"}
  , Record {point = "1D554", uchar = "\120148", latex = "\\mathbb{c}", unicodemath = "\\Bbbc", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL C"}
  , Record {point = "1D555", uchar = "\120149", latex = "\\mathbb{d}", unicodemath = "\\Bbbd", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL D"}
  , Record {point = "1D556", uchar = "\120150", latex = "\\mathbb{e}", unicodemath = "\\Bbbe", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL E"}
  , Record {point = "1D557", uchar = "\120151", latex = "\\mathbb{f}", unicodemath = "\\Bbbf", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL F"}
  , Record {point = "1D558", uchar = "\120152", latex = "\\mathbb{g}", unicodemath = "\\Bbbg", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL G"}
  , Record {point = "1D559", uchar = "\120153", latex = "\\mathbb{h}", unicodemath = "\\Bbbh", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL H"}
  , Record {point = "1D55A", uchar = "\120154", latex = "\\mathbb{i}", unicodemath = "\\Bbbi", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL I"}
  , Record {point = "1D55B", uchar = "\120155", latex = "\\mathbb{j}", unicodemath = "\\Bbbj", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL J"}
  , Record {point = "1D55C", uchar = "\120156", latex = "\\mathbb{k}", unicodemath = "\\Bbbk", cls = "A", category = "mathalpha", requirements = "bbold fourier", comments = "= \\Bbbk (amssymb), MATHEMATICAL DOUBLE-STRUCK SMALL K"}
  , Record {point = "1D55D", uchar = "\120157", latex = "\\mathbb{l}", unicodemath = "\\Bbbl", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL L"}
  , Record {point = "1D55E", uchar = "\120158", latex = "\\mathbb{m}", unicodemath = "\\Bbbm", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL M"}
  , Record {point = "1D55F", uchar = "\120159", latex = "\\mathbb{n}", unicodemath = "\\Bbbn", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL N"}
  , Record {point = "1D560", uchar = "\120160", latex = "\\mathbb{o}", unicodemath = "\\Bbbo", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL O"}
  , Record {point = "1D561", uchar = "\120161", latex = "\\mathbb{p}", unicodemath = "\\Bbbp", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL P"}
  , Record {point = "1D562", uchar = "\120162", latex = "\\mathbb{q}", unicodemath = "\\Bbbq", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL Q"}
  , Record {point = "1D563", uchar = "\120163", latex = "\\mathbb{r}", unicodemath = "\\Bbbr", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL R"}
  , Record {point = "1D564", uchar = "\120164", latex = "\\mathbb{s}", unicodemath = "\\Bbbs", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL S"}
  , Record {point = "1D565", uchar = "\120165", latex = "\\mathbb{t}", unicodemath = "\\Bbbt", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL T"}
  , Record {point = "1D566", uchar = "\120166", latex = "\\mathbb{u}", unicodemath = "\\Bbbu", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL U"}
  , Record {point = "1D567", uchar = "\120167", latex = "\\mathbb{v}", unicodemath = "\\Bbbv", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL V"}
  , Record {point = "1D568", uchar = "\120168", latex = "\\mathbb{w}", unicodemath = "\\Bbbw", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL W"}
  , Record {point = "1D569", uchar = "\120169", latex = "\\mathbb{x}", unicodemath = "\\Bbbx", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL X"}
  , Record {point = "1D56A", uchar = "\120170", latex = "\\mathbb{y}", unicodemath = "\\Bbby", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL Y"}
  , Record {point = "1D56B", uchar = "\120171", latex = "\\mathbb{z}", unicodemath = "\\Bbbz", cls = "A", category = "mathalpha", requirements = "bbold", comments = "MATHEMATICAL DOUBLE-STRUCK SMALL Z"}
  , Record {point = "1D56C", uchar = "\120172", latex = "", unicodemath = "\\mbffrakA", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL A"}
  , Record {point = "1D56D", uchar = "\120173", latex = "", unicodemath = "\\mbffrakB", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL B"}
  , Record {point = "1D56E", uchar = "\120174", latex = "", unicodemath = "\\mbffrakC", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL C"}
  , Record {point = "1D56F", uchar = "\120175", latex = "", unicodemath = "\\mbffrakD", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL D"}
  , Record {point = "1D570", uchar = "\120176", latex = "", unicodemath = "\\mbffrakE", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL E"}
  , Record {point = "1D571", uchar = "\120177", latex = "", unicodemath = "\\mbffrakF", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL F"}
  , Record {point = "1D572", uchar = "\120178", latex = "", unicodemath = "\\mbffrakG", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL G"}
  , Record {point = "1D573", uchar = "\120179", latex = "", unicodemath = "\\mbffrakH", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL H"}
  , Record {point = "1D574", uchar = "\120180", latex = "", unicodemath = "\\mbffrakI", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL I"}
  , Record {point = "1D575", uchar = "\120181", latex = "", unicodemath = "\\mbffrakJ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL J"}
  , Record {point = "1D576", uchar = "\120182", latex = "", unicodemath = "\\mbffrakK", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL K"}
  , Record {point = "1D577", uchar = "\120183", latex = "", unicodemath = "\\mbffrakL", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL L"}
  , Record {point = "1D578", uchar = "\120184", latex = "", unicodemath = "\\mbffrakM", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL M"}
  , Record {point = "1D579", uchar = "\120185", latex = "", unicodemath = "\\mbffrakN", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL N"}
  , Record {point = "1D57A", uchar = "\120186", latex = "", unicodemath = "\\mbffrakO", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL O"}
  , Record {point = "1D57B", uchar = "\120187", latex = "", unicodemath = "\\mbffrakP", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL P"}
  , Record {point = "1D57C", uchar = "\120188", latex = "", unicodemath = "\\mbffrakQ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL Q"}
  , Record {point = "1D57D", uchar = "\120189", latex = "", unicodemath = "\\mbffrakR", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL R"}
  , Record {point = "1D57E", uchar = "\120190", latex = "", unicodemath = "\\mbffrakS", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL S"}
  , Record {point = "1D57F", uchar = "\120191", latex = "", unicodemath = "\\mbffrakT", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL T"}
  , Record {point = "1D580", uchar = "\120192", latex = "", unicodemath = "\\mbffrakU", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL U"}
  , Record {point = "1D581", uchar = "\120193", latex = "", unicodemath = "\\mbffrakV", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL V"}
  , Record {point = "1D582", uchar = "\120194", latex = "", unicodemath = "\\mbffrakW", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL W"}
  , Record {point = "1D583", uchar = "\120195", latex = "", unicodemath = "\\mbffrakX", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL X"}
  , Record {point = "1D584", uchar = "\120196", latex = "", unicodemath = "\\mbffrakY", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL Y"}
  , Record {point = "1D585", uchar = "\120197", latex = "", unicodemath = "\\mbffrakZ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR CAPITAL Z"}
  , Record {point = "1D586", uchar = "\120198", latex = "", unicodemath = "\\mbffraka", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL A"}
  , Record {point = "1D587", uchar = "\120199", latex = "", unicodemath = "\\mbffrakb", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL B"}
  , Record {point = "1D588", uchar = "\120200", latex = "", unicodemath = "\\mbffrakc", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL C"}
  , Record {point = "1D589", uchar = "\120201", latex = "", unicodemath = "\\mbffrakd", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL D"}
  , Record {point = "1D58A", uchar = "\120202", latex = "", unicodemath = "\\mbffrake", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL E"}
  , Record {point = "1D58B", uchar = "\120203", latex = "", unicodemath = "\\mbffrakf", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL F"}
  , Record {point = "1D58C", uchar = "\120204", latex = "", unicodemath = "\\mbffrakg", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL G"}
  , Record {point = "1D58D", uchar = "\120205", latex = "", unicodemath = "\\mbffrakh", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL H"}
  , Record {point = "1D58E", uchar = "\120206", latex = "", unicodemath = "\\mbffraki", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL I"}
  , Record {point = "1D58F", uchar = "\120207", latex = "", unicodemath = "\\mbffrakj", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL J"}
  , Record {point = "1D590", uchar = "\120208", latex = "", unicodemath = "\\mbffrakk", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL K"}
  , Record {point = "1D591", uchar = "\120209", latex = "", unicodemath = "\\mbffrakl", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL L"}
  , Record {point = "1D592", uchar = "\120210", latex = "", unicodemath = "\\mbffrakm", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL M"}
  , Record {point = "1D593", uchar = "\120211", latex = "", unicodemath = "\\mbffrakn", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL N"}
  , Record {point = "1D594", uchar = "\120212", latex = "", unicodemath = "\\mbffrako", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL O"}
  , Record {point = "1D595", uchar = "\120213", latex = "", unicodemath = "\\mbffrakp", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL P"}
  , Record {point = "1D596", uchar = "\120214", latex = "", unicodemath = "\\mbffrakq", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL Q"}
  , Record {point = "1D597", uchar = "\120215", latex = "", unicodemath = "\\mbffrakr", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL R"}
  , Record {point = "1D598", uchar = "\120216", latex = "", unicodemath = "\\mbffraks", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL S"}
  , Record {point = "1D599", uchar = "\120217", latex = "", unicodemath = "\\mbffrakt", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL T"}
  , Record {point = "1D59A", uchar = "\120218", latex = "", unicodemath = "\\mbffraku", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL U"}
  , Record {point = "1D59B", uchar = "\120219", latex = "", unicodemath = "\\mbffrakv", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL V"}
  , Record {point = "1D59C", uchar = "\120220", latex = "", unicodemath = "\\mbffrakw", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL W"}
  , Record {point = "1D59D", uchar = "\120221", latex = "", unicodemath = "\\mbffrakx", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL X"}
  , Record {point = "1D59E", uchar = "\120222", latex = "", unicodemath = "\\mbffraky", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL Y"}
  , Record {point = "1D59F", uchar = "\120223", latex = "", unicodemath = "\\mbffrakz", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD FRAKTUR SMALL Z"}
  , Record {point = "1D5A0", uchar = "\120224", latex = "\\mathsf{A}", unicodemath = "\\msansA", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL A"}
  , Record {point = "1D5A1", uchar = "\120225", latex = "\\mathsf{B}", unicodemath = "\\msansB", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL B"}
  , Record {point = "1D5A2", uchar = "\120226", latex = "\\mathsf{C}", unicodemath = "\\msansC", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL C"}
  , Record {point = "1D5A3", uchar = "\120227", latex = "\\mathsf{D}", unicodemath = "\\msansD", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL D"}
  , Record {point = "1D5A4", uchar = "\120228", latex = "\\mathsf{E}", unicodemath = "\\msansE", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL E"}
  , Record {point = "1D5A5", uchar = "\120229", latex = "\\mathsf{F}", unicodemath = "\\msansF", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL F"}
  , Record {point = "1D5A6", uchar = "\120230", latex = "\\mathsf{G}", unicodemath = "\\msansG", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL G"}
  , Record {point = "1D5A7", uchar = "\120231", latex = "\\mathsf{H}", unicodemath = "\\msansH", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL H"}
  , Record {point = "1D5A8", uchar = "\120232", latex = "\\mathsf{I}", unicodemath = "\\msansI", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL I"}
  , Record {point = "1D5A9", uchar = "\120233", latex = "\\mathsf{J}", unicodemath = "\\msansJ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL J"}
  , Record {point = "1D5AA", uchar = "\120234", latex = "\\mathsf{K}", unicodemath = "\\msansK", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL K"}
  , Record {point = "1D5AB", uchar = "\120235", latex = "\\mathsf{L}", unicodemath = "\\msansL", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL L"}
  , Record {point = "1D5AC", uchar = "\120236", latex = "\\mathsf{M}", unicodemath = "\\msansM", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL M"}
  , Record {point = "1D5AD", uchar = "\120237", latex = "\\mathsf{N}", unicodemath = "\\msansN", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL N"}
  , Record {point = "1D5AE", uchar = "\120238", latex = "\\mathsf{O}", unicodemath = "\\msansO", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL O"}
  , Record {point = "1D5AF", uchar = "\120239", latex = "\\mathsf{P}", unicodemath = "\\msansP", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL P"}
  , Record {point = "1D5B0", uchar = "\120240", latex = "\\mathsf{Q}", unicodemath = "\\msansQ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL Q"}
  , Record {point = "1D5B1", uchar = "\120241", latex = "\\mathsf{R}", unicodemath = "\\msansR", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL R"}
  , Record {point = "1D5B2", uchar = "\120242", latex = "\\mathsf{S}", unicodemath = "\\msansS", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL S"}
  , Record {point = "1D5B3", uchar = "\120243", latex = "\\mathsf{T}", unicodemath = "\\msansT", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL T"}
  , Record {point = "1D5B4", uchar = "\120244", latex = "\\mathsf{U}", unicodemath = "\\msansU", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL U"}
  , Record {point = "1D5B5", uchar = "\120245", latex = "\\mathsf{V}", unicodemath = "\\msansV", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL V"}
  , Record {point = "1D5B6", uchar = "\120246", latex = "\\mathsf{W}", unicodemath = "\\msansW", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL W"}
  , Record {point = "1D5B7", uchar = "\120247", latex = "\\mathsf{X}", unicodemath = "\\msansX", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL X"}
  , Record {point = "1D5B8", uchar = "\120248", latex = "\\mathsf{Y}", unicodemath = "\\msansY", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL Y"}
  , Record {point = "1D5B9", uchar = "\120249", latex = "\\mathsf{Z}", unicodemath = "\\msansZ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF CAPITAL Z"}
  , Record {point = "1D5BA", uchar = "\120250", latex = "\\mathsf{a}", unicodemath = "\\msansa", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL A"}
  , Record {point = "1D5BB", uchar = "\120251", latex = "\\mathsf{b}", unicodemath = "\\msansb", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL B"}
  , Record {point = "1D5BC", uchar = "\120252", latex = "\\mathsf{c}", unicodemath = "\\msansc", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL C"}
  , Record {point = "1D5BD", uchar = "\120253", latex = "\\mathsf{d}", unicodemath = "\\msansd", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL D"}
  , Record {point = "1D5BE", uchar = "\120254", latex = "\\mathsf{e}", unicodemath = "\\msanse", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL E"}
  , Record {point = "1D5BF", uchar = "\120255", latex = "\\mathsf{f}", unicodemath = "\\msansf", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL F"}
  , Record {point = "1D5C0", uchar = "\120256", latex = "\\mathsf{g}", unicodemath = "\\msansg", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL G"}
  , Record {point = "1D5C1", uchar = "\120257", latex = "\\mathsf{h}", unicodemath = "\\msansh", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL H"}
  , Record {point = "1D5C2", uchar = "\120258", latex = "\\mathsf{i}", unicodemath = "\\msansi", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL I"}
  , Record {point = "1D5C3", uchar = "\120259", latex = "\\mathsf{j}", unicodemath = "\\msansj", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL J"}
  , Record {point = "1D5C4", uchar = "\120260", latex = "\\mathsf{k}", unicodemath = "\\msansk", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL K"}
  , Record {point = "1D5C5", uchar = "\120261", latex = "\\mathsf{l}", unicodemath = "\\msansl", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL L"}
  , Record {point = "1D5C6", uchar = "\120262", latex = "\\mathsf{m}", unicodemath = "\\msansm", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL M"}
  , Record {point = "1D5C7", uchar = "\120263", latex = "\\mathsf{n}", unicodemath = "\\msansn", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL N"}
  , Record {point = "1D5C8", uchar = "\120264", latex = "\\mathsf{o}", unicodemath = "\\msanso", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL O"}
  , Record {point = "1D5C9", uchar = "\120265", latex = "\\mathsf{p}", unicodemath = "\\msansp", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL P"}
  , Record {point = "1D5CA", uchar = "\120266", latex = "\\mathsf{q}", unicodemath = "\\msansq", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL Q"}
  , Record {point = "1D5CB", uchar = "\120267", latex = "\\mathsf{r}", unicodemath = "\\msansr", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL R"}
  , Record {point = "1D5CC", uchar = "\120268", latex = "\\mathsf{s}", unicodemath = "\\msanss", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL S"}
  , Record {point = "1D5CD", uchar = "\120269", latex = "\\mathsf{t}", unicodemath = "\\msanst", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL T"}
  , Record {point = "1D5CE", uchar = "\120270", latex = "\\mathsf{u}", unicodemath = "\\msansu", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL U"}
  , Record {point = "1D5CF", uchar = "\120271", latex = "\\mathsf{v}", unicodemath = "\\msansv", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL V"}
  , Record {point = "1D5D0", uchar = "\120272", latex = "\\mathsf{w}", unicodemath = "\\msansw", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL W"}
  , Record {point = "1D5D1", uchar = "\120273", latex = "\\mathsf{x}", unicodemath = "\\msansx", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL X"}
  , Record {point = "1D5D2", uchar = "\120274", latex = "\\mathsf{y}", unicodemath = "\\msansy", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL Y"}
  , Record {point = "1D5D3", uchar = "\120275", latex = "\\mathsf{z}", unicodemath = "\\msansz", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF SMALL Z"}
  , Record {point = "1D5D4", uchar = "\120276", latex = "\\mathsfbf{A}", unicodemath = "\\mbfsansA", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL A"}
  , Record {point = "1D5D5", uchar = "\120277", latex = "\\mathsfbf{B}", unicodemath = "\\mbfsansB", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL B"}
  , Record {point = "1D5D6", uchar = "\120278", latex = "\\mathsfbf{C}", unicodemath = "\\mbfsansC", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL C"}
  , Record {point = "1D5D7", uchar = "\120279", latex = "\\mathsfbf{D}", unicodemath = "\\mbfsansD", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL D"}
  , Record {point = "1D5D8", uchar = "\120280", latex = "\\mathsfbf{E}", unicodemath = "\\mbfsansE", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL E"}
  , Record {point = "1D5D9", uchar = "\120281", latex = "\\mathsfbf{F}", unicodemath = "\\mbfsansF", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL F"}
  , Record {point = "1D5DA", uchar = "\120282", latex = "\\mathsfbf{G}", unicodemath = "\\mbfsansG", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL G"}
  , Record {point = "1D5DB", uchar = "\120283", latex = "\\mathsfbf{H}", unicodemath = "\\mbfsansH", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL H"}
  , Record {point = "1D5DC", uchar = "\120284", latex = "\\mathsfbf{I}", unicodemath = "\\mbfsansI", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL I"}
  , Record {point = "1D5DD", uchar = "\120285", latex = "\\mathsfbf{J}", unicodemath = "\\mbfsansJ", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL J"}
  , Record {point = "1D5DE", uchar = "\120286", latex = "\\mathsfbf{K}", unicodemath = "\\mbfsansK", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL K"}
  , Record {point = "1D5DF", uchar = "\120287", latex = "\\mathsfbf{L}", unicodemath = "\\mbfsansL", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL L"}
  , Record {point = "1D5E0", uchar = "\120288", latex = "\\mathsfbf{M}", unicodemath = "\\mbfsansM", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL M"}
  , Record {point = "1D5E1", uchar = "\120289", latex = "\\mathsfbf{N}", unicodemath = "\\mbfsansN", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL N"}
  , Record {point = "1D5E2", uchar = "\120290", latex = "\\mathsfbf{O}", unicodemath = "\\mbfsansO", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL O"}
  , Record {point = "1D5E3", uchar = "\120291", latex = "\\mathsfbf{P}", unicodemath = "\\mbfsansP", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL P"}
  , Record {point = "1D5E4", uchar = "\120292", latex = "\\mathsfbf{Q}", unicodemath = "\\mbfsansQ", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL Q"}
  , Record {point = "1D5E5", uchar = "\120293", latex = "\\mathsfbf{R}", unicodemath = "\\mbfsansR", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL R"}
  , Record {point = "1D5E6", uchar = "\120294", latex = "\\mathsfbf{S}", unicodemath = "\\mbfsansS", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL S"}
  , Record {point = "1D5E7", uchar = "\120295", latex = "\\mathsfbf{T}", unicodemath = "\\mbfsansT", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL T"}
  , Record {point = "1D5E8", uchar = "\120296", latex = "\\mathsfbf{U}", unicodemath = "\\mbfsansU", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL U"}
  , Record {point = "1D5E9", uchar = "\120297", latex = "\\mathsfbf{V}", unicodemath = "\\mbfsansV", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL V"}
  , Record {point = "1D5EA", uchar = "\120298", latex = "\\mathsfbf{W}", unicodemath = "\\mbfsansW", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL W"}
  , Record {point = "1D5EB", uchar = "\120299", latex = "\\mathsfbf{X}", unicodemath = "\\mbfsansX", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL X"}
  , Record {point = "1D5EC", uchar = "\120300", latex = "\\mathsfbf{Y}", unicodemath = "\\mbfsansY", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL Y"}
  , Record {point = "1D5ED", uchar = "\120301", latex = "\\mathsfbf{Z}", unicodemath = "\\mbfsansZ", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL Z"}
  , Record {point = "1D5EE", uchar = "\120302", latex = "\\mathsfbf{a}", unicodemath = "\\mbfsansa", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL A"}
  , Record {point = "1D5EF", uchar = "\120303", latex = "\\mathsfbf{b}", unicodemath = "\\mbfsansb", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL B"}
  , Record {point = "1D5F0", uchar = "\120304", latex = "\\mathsfbf{c}", unicodemath = "\\mbfsansc", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL C"}
  , Record {point = "1D5F1", uchar = "\120305", latex = "\\mathsfbf{d}", unicodemath = "\\mbfsansd", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL D"}
  , Record {point = "1D5F2", uchar = "\120306", latex = "\\mathsfbf{e}", unicodemath = "\\mbfsanse", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL E"}
  , Record {point = "1D5F3", uchar = "\120307", latex = "\\mathsfbf{f}", unicodemath = "\\mbfsansf", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL F"}
  , Record {point = "1D5F4", uchar = "\120308", latex = "\\mathsfbf{g}", unicodemath = "\\mbfsansg", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL G"}
  , Record {point = "1D5F5", uchar = "\120309", latex = "\\mathsfbf{h}", unicodemath = "\\mbfsansh", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL H"}
  , Record {point = "1D5F6", uchar = "\120310", latex = "\\mathsfbf{i}", unicodemath = "\\mbfsansi", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL I"}
  , Record {point = "1D5F7", uchar = "\120311", latex = "\\mathsfbf{j}", unicodemath = "\\mbfsansj", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL J"}
  , Record {point = "1D5F8", uchar = "\120312", latex = "\\mathsfbf{k}", unicodemath = "\\mbfsansk", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL K"}
  , Record {point = "1D5F9", uchar = "\120313", latex = "\\mathsfbf{l}", unicodemath = "\\mbfsansl", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL L"}
  , Record {point = "1D5FA", uchar = "\120314", latex = "\\mathsfbf{m}", unicodemath = "\\mbfsansm", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL M"}
  , Record {point = "1D5FB", uchar = "\120315", latex = "\\mathsfbf{n}", unicodemath = "\\mbfsansn", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL N"}
  , Record {point = "1D5FC", uchar = "\120316", latex = "\\mathsfbf{o}", unicodemath = "\\mbfsanso", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL O"}
  , Record {point = "1D5FD", uchar = "\120317", latex = "\\mathsfbf{p}", unicodemath = "\\mbfsansp", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL P"}
  , Record {point = "1D5FE", uchar = "\120318", latex = "\\mathsfbf{q}", unicodemath = "\\mbfsansq", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL Q"}
  , Record {point = "1D5FF", uchar = "\120319", latex = "\\mathsfbf{r}", unicodemath = "\\mbfsansr", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL R"}
  , Record {point = "1D600", uchar = "\120320", latex = "\\mathsfbf{s}", unicodemath = "\\mbfsanss", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL S"}
  , Record {point = "1D601", uchar = "\120321", latex = "\\mathsfbf{t}", unicodemath = "\\mbfsanst", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL T"}
  , Record {point = "1D602", uchar = "\120322", latex = "\\mathsfbf{u}", unicodemath = "\\mbfsansu", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL U"}
  , Record {point = "1D603", uchar = "\120323", latex = "\\mathsfbf{v}", unicodemath = "\\mbfsansv", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL V"}
  , Record {point = "1D604", uchar = "\120324", latex = "\\mathsfbf{w}", unicodemath = "\\mbfsansw", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL W"}
  , Record {point = "1D605", uchar = "\120325", latex = "\\mathsfbf{x}", unicodemath = "\\mbfsansx", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL X"}
  , Record {point = "1D606", uchar = "\120326", latex = "\\mathsfbf{y}", unicodemath = "\\mbfsansy", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL Y"}
  , Record {point = "1D607", uchar = "\120327", latex = "\\mathsfbf{z}", unicodemath = "\\mbfsansz", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL Z"}
  , Record {point = "1D608", uchar = "\120328", latex = "\\mathsfit{A}", unicodemath = "\\mitsansA", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL A"}
  , Record {point = "1D609", uchar = "\120329", latex = "\\mathsfit{B}", unicodemath = "\\mitsansB", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL B"}
  , Record {point = "1D60A", uchar = "\120330", latex = "\\mathsfit{C}", unicodemath = "\\mitsansC", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL C"}
  , Record {point = "1D60B", uchar = "\120331", latex = "\\mathsfit{D}", unicodemath = "\\mitsansD", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL D"}
  , Record {point = "1D60C", uchar = "\120332", latex = "\\mathsfit{E}", unicodemath = "\\mitsansE", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL E"}
  , Record {point = "1D60D", uchar = "\120333", latex = "\\mathsfit{F}", unicodemath = "\\mitsansF", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL F"}
  , Record {point = "1D60E", uchar = "\120334", latex = "\\mathsfit{G}", unicodemath = "\\mitsansG", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL G"}
  , Record {point = "1D60F", uchar = "\120335", latex = "\\mathsfit{H}", unicodemath = "\\mitsansH", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL H"}
  , Record {point = "1D610", uchar = "\120336", latex = "\\mathsfit{I}", unicodemath = "\\mitsansI", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL I"}
  , Record {point = "1D611", uchar = "\120337", latex = "\\mathsfit{J}", unicodemath = "\\mitsansJ", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL J"}
  , Record {point = "1D612", uchar = "\120338", latex = "\\mathsfit{K}", unicodemath = "\\mitsansK", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL K"}
  , Record {point = "1D613", uchar = "\120339", latex = "\\mathsfit{L}", unicodemath = "\\mitsansL", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL L"}
  , Record {point = "1D614", uchar = "\120340", latex = "\\mathsfit{M}", unicodemath = "\\mitsansM", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL M"}
  , Record {point = "1D615", uchar = "\120341", latex = "\\mathsfit{N}", unicodemath = "\\mitsansN", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL N"}
  , Record {point = "1D616", uchar = "\120342", latex = "\\mathsfit{O}", unicodemath = "\\mitsansO", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL O"}
  , Record {point = "1D617", uchar = "\120343", latex = "\\mathsfit{P}", unicodemath = "\\mitsansP", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL P"}
  , Record {point = "1D618", uchar = "\120344", latex = "\\mathsfit{Q}", unicodemath = "\\mitsansQ", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL Q"}
  , Record {point = "1D619", uchar = "\120345", latex = "\\mathsfit{R}", unicodemath = "\\mitsansR", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL R"}
  , Record {point = "1D61A", uchar = "\120346", latex = "\\mathsfit{S}", unicodemath = "\\mitsansS", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL S"}
  , Record {point = "1D61B", uchar = "\120347", latex = "\\mathsfit{T}", unicodemath = "\\mitsansT", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL T"}
  , Record {point = "1D61C", uchar = "\120348", latex = "\\mathsfit{U}", unicodemath = "\\mitsansU", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL U"}
  , Record {point = "1D61D", uchar = "\120349", latex = "\\mathsfit{V}", unicodemath = "\\mitsansV", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL V"}
  , Record {point = "1D61E", uchar = "\120350", latex = "\\mathsfit{W}", unicodemath = "\\mitsansW", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL W"}
  , Record {point = "1D61F", uchar = "\120351", latex = "\\mathsfit{X}", unicodemath = "\\mitsansX", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL X"}
  , Record {point = "1D620", uchar = "\120352", latex = "\\mathsfit{Y}", unicodemath = "\\mitsansY", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL Y"}
  , Record {point = "1D621", uchar = "\120353", latex = "\\mathsfit{Z}", unicodemath = "\\mitsansZ", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL Z"}
  , Record {point = "1D622", uchar = "\120354", latex = "\\mathsfit{a}", unicodemath = "\\mitsansa", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL A"}
  , Record {point = "1D623", uchar = "\120355", latex = "\\mathsfit{b}", unicodemath = "\\mitsansb", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL B"}
  , Record {point = "1D624", uchar = "\120356", latex = "\\mathsfit{c}", unicodemath = "\\mitsansc", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL C"}
  , Record {point = "1D625", uchar = "\120357", latex = "\\mathsfit{d}", unicodemath = "\\mitsansd", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL D"}
  , Record {point = "1D626", uchar = "\120358", latex = "\\mathsfit{e}", unicodemath = "\\mitsanse", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL E"}
  , Record {point = "1D627", uchar = "\120359", latex = "\\mathsfit{f}", unicodemath = "\\mitsansf", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL F"}
  , Record {point = "1D628", uchar = "\120360", latex = "\\mathsfit{g}", unicodemath = "\\mitsansg", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL G"}
  , Record {point = "1D629", uchar = "\120361", latex = "\\mathsfit{h}", unicodemath = "\\mitsansh", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL H"}
  , Record {point = "1D62A", uchar = "\120362", latex = "\\mathsfit{i}", unicodemath = "\\mitsansi", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL I"}
  , Record {point = "1D62B", uchar = "\120363", latex = "\\mathsfit{j}", unicodemath = "\\mitsansj", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL J"}
  , Record {point = "1D62C", uchar = "\120364", latex = "\\mathsfit{k}", unicodemath = "\\mitsansk", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL K"}
  , Record {point = "1D62D", uchar = "\120365", latex = "\\mathsfit{l}", unicodemath = "\\mitsansl", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL L"}
  , Record {point = "1D62E", uchar = "\120366", latex = "\\mathsfit{m}", unicodemath = "\\mitsansm", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL M"}
  , Record {point = "1D62F", uchar = "\120367", latex = "\\mathsfit{n}", unicodemath = "\\mitsansn", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL N"}
  , Record {point = "1D630", uchar = "\120368", latex = "\\mathsfit{o}", unicodemath = "\\mitsanso", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL O"}
  , Record {point = "1D631", uchar = "\120369", latex = "\\mathsfit{p}", unicodemath = "\\mitsansp", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL P"}
  , Record {point = "1D632", uchar = "\120370", latex = "\\mathsfit{q}", unicodemath = "\\mitsansq", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL Q"}
  , Record {point = "1D633", uchar = "\120371", latex = "\\mathsfit{r}", unicodemath = "\\mitsansr", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL R"}
  , Record {point = "1D634", uchar = "\120372", latex = "\\mathsfit{s}", unicodemath = "\\mitsanss", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL S"}
  , Record {point = "1D635", uchar = "\120373", latex = "\\mathsfit{t}", unicodemath = "\\mitsanst", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL T"}
  , Record {point = "1D636", uchar = "\120374", latex = "\\mathsfit{u}", unicodemath = "\\mitsansu", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL U"}
  , Record {point = "1D637", uchar = "\120375", latex = "\\mathsfit{v}", unicodemath = "\\mitsansv", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL V"}
  , Record {point = "1D638", uchar = "\120376", latex = "\\mathsfit{w}", unicodemath = "\\mitsansw", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL W"}
  , Record {point = "1D639", uchar = "\120377", latex = "\\mathsfit{x}", unicodemath = "\\mitsansx", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL X"}
  , Record {point = "1D63A", uchar = "\120378", latex = "\\mathsfit{y}", unicodemath = "\\mitsansy", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL Y"}
  , Record {point = "1D63B", uchar = "\120379", latex = "\\mathsfit{z}", unicodemath = "\\mitsansz", cls = "A", category = "mathalpha", requirements = "omlmathsfit", comments = "MATHEMATICAL SANS-SERIF ITALIC SMALL Z"}
  , Record {point = "1D63C", uchar = "\120380", latex = "\\mathsfbfit{A}", unicodemath = "\\mbfitsansA", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL A"}
  , Record {point = "1D63D", uchar = "\120381", latex = "\\mathsfbfit{B}", unicodemath = "\\mbfitsansB", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL B"}
  , Record {point = "1D63E", uchar = "\120382", latex = "\\mathsfbfit{C}", unicodemath = "\\mbfitsansC", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL C"}
  , Record {point = "1D63F", uchar = "\120383", latex = "\\mathsfbfit{D}", unicodemath = "\\mbfitsansD", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL D"}
  , Record {point = "1D640", uchar = "\120384", latex = "\\mathsfbfit{E}", unicodemath = "\\mbfitsansE", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL E"}
  , Record {point = "1D641", uchar = "\120385", latex = "\\mathsfbfit{F}", unicodemath = "\\mbfitsansF", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL F"}
  , Record {point = "1D642", uchar = "\120386", latex = "\\mathsfbfit{G}", unicodemath = "\\mbfitsansG", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL G"}
  , Record {point = "1D643", uchar = "\120387", latex = "\\mathsfbfit{H}", unicodemath = "\\mbfitsansH", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL H"}
  , Record {point = "1D644", uchar = "\120388", latex = "\\mathsfbfit{I}", unicodemath = "\\mbfitsansI", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL I"}
  , Record {point = "1D645", uchar = "\120389", latex = "\\mathsfbfit{J}", unicodemath = "\\mbfitsansJ", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL J"}
  , Record {point = "1D646", uchar = "\120390", latex = "\\mathsfbfit{K}", unicodemath = "\\mbfitsansK", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL K"}
  , Record {point = "1D647", uchar = "\120391", latex = "\\mathsfbfit{L}", unicodemath = "\\mbfitsansL", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL L"}
  , Record {point = "1D648", uchar = "\120392", latex = "\\mathsfbfit{M}", unicodemath = "\\mbfitsansM", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL M"}
  , Record {point = "1D649", uchar = "\120393", latex = "\\mathsfbfit{N}", unicodemath = "\\mbfitsansN", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL N"}
  , Record {point = "1D64A", uchar = "\120394", latex = "\\mathsfbfit{O}", unicodemath = "\\mbfitsansO", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL O"}
  , Record {point = "1D64B", uchar = "\120395", latex = "\\mathsfbfit{P}", unicodemath = "\\mbfitsansP", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL P"}
  , Record {point = "1D64C", uchar = "\120396", latex = "\\mathsfbfit{Q}", unicodemath = "\\mbfitsansQ", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Q"}
  , Record {point = "1D64D", uchar = "\120397", latex = "\\mathsfbfit{R}", unicodemath = "\\mbfitsansR", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL R"}
  , Record {point = "1D64E", uchar = "\120398", latex = "\\mathsfbfit{S}", unicodemath = "\\mbfitsansS", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL S"}
  , Record {point = "1D64F", uchar = "\120399", latex = "\\mathsfbfit{T}", unicodemath = "\\mbfitsansT", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL T"}
  , Record {point = "1D650", uchar = "\120400", latex = "\\mathsfbfit{U}", unicodemath = "\\mbfitsansU", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL U"}
  , Record {point = "1D651", uchar = "\120401", latex = "\\mathsfbfit{V}", unicodemath = "\\mbfitsansV", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL V"}
  , Record {point = "1D652", uchar = "\120402", latex = "\\mathsfbfit{W}", unicodemath = "\\mbfitsansW", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL W"}
  , Record {point = "1D653", uchar = "\120403", latex = "\\mathsfbfit{X}", unicodemath = "\\mbfitsansX", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL X"}
  , Record {point = "1D654", uchar = "\120404", latex = "\\mathsfbfit{Y}", unicodemath = "\\mbfitsansY", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Y"}
  , Record {point = "1D655", uchar = "\120405", latex = "\\mathsfbfit{Z}", unicodemath = "\\mbfitsansZ", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Z"}
  , Record {point = "1D656", uchar = "\120406", latex = "\\mathsfbfit{a}", unicodemath = "\\mbfitsansa", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL A"}
  , Record {point = "1D657", uchar = "\120407", latex = "\\mathsfbfit{b}", unicodemath = "\\mbfitsansb", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL B"}
  , Record {point = "1D658", uchar = "\120408", latex = "\\mathsfbfit{c}", unicodemath = "\\mbfitsansc", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL C"}
  , Record {point = "1D659", uchar = "\120409", latex = "\\mathsfbfit{d}", unicodemath = "\\mbfitsansd", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL D"}
  , Record {point = "1D65A", uchar = "\120410", latex = "\\mathsfbfit{e}", unicodemath = "\\mbfitsanse", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL E"}
  , Record {point = "1D65B", uchar = "\120411", latex = "\\mathsfbfit{f}", unicodemath = "\\mbfitsansf", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL F"}
  , Record {point = "1D65C", uchar = "\120412", latex = "\\mathsfbfit{g}", unicodemath = "\\mbfitsansg", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL G"}
  , Record {point = "1D65D", uchar = "\120413", latex = "\\mathsfbfit{h}", unicodemath = "\\mbfitsansh", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL H"}
  , Record {point = "1D65E", uchar = "\120414", latex = "\\mathsfbfit{i}", unicodemath = "\\mbfitsansi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL I"}
  , Record {point = "1D65F", uchar = "\120415", latex = "\\mathsfbfit{j}", unicodemath = "\\mbfitsansj", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL J"}
  , Record {point = "1D660", uchar = "\120416", latex = "\\mathsfbfit{k}", unicodemath = "\\mbfitsansk", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL K"}
  , Record {point = "1D661", uchar = "\120417", latex = "\\mathsfbfit{l}", unicodemath = "\\mbfitsansl", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL L"}
  , Record {point = "1D662", uchar = "\120418", latex = "\\mathsfbfit{m}", unicodemath = "\\mbfitsansm", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL M"}
  , Record {point = "1D663", uchar = "\120419", latex = "\\mathsfbfit{n}", unicodemath = "\\mbfitsansn", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL N"}
  , Record {point = "1D664", uchar = "\120420", latex = "\\mathsfbfit{o}", unicodemath = "\\mbfitsanso", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL O"}
  , Record {point = "1D665", uchar = "\120421", latex = "\\mathsfbfit{p}", unicodemath = "\\mbfitsansp", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL P"}
  , Record {point = "1D666", uchar = "\120422", latex = "\\mathsfbfit{q}", unicodemath = "\\mbfitsansq", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Q"}
  , Record {point = "1D667", uchar = "\120423", latex = "\\mathsfbfit{r}", unicodemath = "\\mbfitsansr", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL R"}
  , Record {point = "1D668", uchar = "\120424", latex = "\\mathsfbfit{s}", unicodemath = "\\mbfitsanss", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL S"}
  , Record {point = "1D669", uchar = "\120425", latex = "\\mathsfbfit{t}", unicodemath = "\\mbfitsanst", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL T"}
  , Record {point = "1D66A", uchar = "\120426", latex = "\\mathsfbfit{u}", unicodemath = "\\mbfitsansu", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL U"}
  , Record {point = "1D66B", uchar = "\120427", latex = "\\mathsfbfit{v}", unicodemath = "\\mbfitsansv", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL V"}
  , Record {point = "1D66C", uchar = "\120428", latex = "\\mathsfbfit{w}", unicodemath = "\\mbfitsansw", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL W"}
  , Record {point = "1D66D", uchar = "\120429", latex = "\\mathsfbfit{x}", unicodemath = "\\mbfitsansx", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL X"}
  , Record {point = "1D66E", uchar = "\120430", latex = "\\mathsfbfit{y}", unicodemath = "\\mbfitsansy", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Y"}
  , Record {point = "1D66F", uchar = "\120431", latex = "\\mathsfbfit{z}", unicodemath = "\\mbfitsansz", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Z"}
  , Record {point = "1D670", uchar = "\120432", latex = "\\mathtt{A}", unicodemath = "\\mttA", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL A"}
  , Record {point = "1D671", uchar = "\120433", latex = "\\mathtt{B}", unicodemath = "\\mttB", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL B"}
  , Record {point = "1D672", uchar = "\120434", latex = "\\mathtt{C}", unicodemath = "\\mttC", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL C"}
  , Record {point = "1D673", uchar = "\120435", latex = "\\mathtt{D}", unicodemath = "\\mttD", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL D"}
  , Record {point = "1D674", uchar = "\120436", latex = "\\mathtt{E}", unicodemath = "\\mttE", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL E"}
  , Record {point = "1D675", uchar = "\120437", latex = "\\mathtt{F}", unicodemath = "\\mttF", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL F"}
  , Record {point = "1D676", uchar = "\120438", latex = "\\mathtt{G}", unicodemath = "\\mttG", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL G"}
  , Record {point = "1D677", uchar = "\120439", latex = "\\mathtt{H}", unicodemath = "\\mttH", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL H"}
  , Record {point = "1D678", uchar = "\120440", latex = "\\mathtt{I}", unicodemath = "\\mttI", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL I"}
  , Record {point = "1D679", uchar = "\120441", latex = "\\mathtt{J}", unicodemath = "\\mttJ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL J"}
  , Record {point = "1D67A", uchar = "\120442", latex = "\\mathtt{K}", unicodemath = "\\mttK", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL K"}
  , Record {point = "1D67B", uchar = "\120443", latex = "\\mathtt{L}", unicodemath = "\\mttL", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL L"}
  , Record {point = "1D67C", uchar = "\120444", latex = "\\mathtt{M}", unicodemath = "\\mttM", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL M"}
  , Record {point = "1D67D", uchar = "\120445", latex = "\\mathtt{N}", unicodemath = "\\mttN", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL N"}
  , Record {point = "1D67E", uchar = "\120446", latex = "\\mathtt{O}", unicodemath = "\\mttO", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL O"}
  , Record {point = "1D67F", uchar = "\120447", latex = "\\mathtt{P}", unicodemath = "\\mttP", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL P"}
  , Record {point = "1D680", uchar = "\120448", latex = "\\mathtt{Q}", unicodemath = "\\mttQ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL Q"}
  , Record {point = "1D681", uchar = "\120449", latex = "\\mathtt{R}", unicodemath = "\\mttR", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL R"}
  , Record {point = "1D682", uchar = "\120450", latex = "\\mathtt{S}", unicodemath = "\\mttS", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL S"}
  , Record {point = "1D683", uchar = "\120451", latex = "\\mathtt{T}", unicodemath = "\\mttT", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL T"}
  , Record {point = "1D684", uchar = "\120452", latex = "\\mathtt{U}", unicodemath = "\\mttU", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL U"}
  , Record {point = "1D685", uchar = "\120453", latex = "\\mathtt{V}", unicodemath = "\\mttV", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL V"}
  , Record {point = "1D686", uchar = "\120454", latex = "\\mathtt{W}", unicodemath = "\\mttW", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL W"}
  , Record {point = "1D687", uchar = "\120455", latex = "\\mathtt{X}", unicodemath = "\\mttX", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL X"}
  , Record {point = "1D688", uchar = "\120456", latex = "\\mathtt{Y}", unicodemath = "\\mttY", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL Y"}
  , Record {point = "1D689", uchar = "\120457", latex = "\\mathtt{Z}", unicodemath = "\\mttZ", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE CAPITAL Z"}
  , Record {point = "1D68A", uchar = "\120458", latex = "\\mathtt{a}", unicodemath = "\\mtta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL A"}
  , Record {point = "1D68B", uchar = "\120459", latex = "\\mathtt{b}", unicodemath = "\\mttb", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL B"}
  , Record {point = "1D68C", uchar = "\120460", latex = "\\mathtt{c}", unicodemath = "\\mttc", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL C"}
  , Record {point = "1D68D", uchar = "\120461", latex = "\\mathtt{d}", unicodemath = "\\mttd", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL D"}
  , Record {point = "1D68E", uchar = "\120462", latex = "\\mathtt{e}", unicodemath = "\\mtte", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL E"}
  , Record {point = "1D68F", uchar = "\120463", latex = "\\mathtt{f}", unicodemath = "\\mttf", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL F"}
  , Record {point = "1D690", uchar = "\120464", latex = "\\mathtt{g}", unicodemath = "\\mttg", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL G"}
  , Record {point = "1D691", uchar = "\120465", latex = "\\mathtt{h}", unicodemath = "\\mtth", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL H"}
  , Record {point = "1D692", uchar = "\120466", latex = "\\mathtt{i}", unicodemath = "\\mtti", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL I"}
  , Record {point = "1D693", uchar = "\120467", latex = "\\mathtt{j}", unicodemath = "\\mttj", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL J"}
  , Record {point = "1D694", uchar = "\120468", latex = "\\mathtt{k}", unicodemath = "\\mttk", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL K"}
  , Record {point = "1D695", uchar = "\120469", latex = "\\mathtt{l}", unicodemath = "\\mttl", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL L"}
  , Record {point = "1D696", uchar = "\120470", latex = "\\mathtt{m}", unicodemath = "\\mttm", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL M"}
  , Record {point = "1D697", uchar = "\120471", latex = "\\mathtt{n}", unicodemath = "\\mttn", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL N"}
  , Record {point = "1D698", uchar = "\120472", latex = "\\mathtt{o}", unicodemath = "\\mtto", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL O"}
  , Record {point = "1D699", uchar = "\120473", latex = "\\mathtt{p}", unicodemath = "\\mttp", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL P"}
  , Record {point = "1D69A", uchar = "\120474", latex = "\\mathtt{q}", unicodemath = "\\mttq", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL Q"}
  , Record {point = "1D69B", uchar = "\120475", latex = "\\mathtt{r}", unicodemath = "\\mttr", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL R"}
  , Record {point = "1D69C", uchar = "\120476", latex = "\\mathtt{s}", unicodemath = "\\mtts", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL S"}
  , Record {point = "1D69D", uchar = "\120477", latex = "\\mathtt{t}", unicodemath = "\\mttt", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL T"}
  , Record {point = "1D69E", uchar = "\120478", latex = "\\mathtt{u}", unicodemath = "\\mttu", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL U"}
  , Record {point = "1D69F", uchar = "\120479", latex = "\\mathtt{v}", unicodemath = "\\mttv", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL V"}
  , Record {point = "1D6A0", uchar = "\120480", latex = "\\mathtt{w}", unicodemath = "\\mttw", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL W"}
  , Record {point = "1D6A1", uchar = "\120481", latex = "\\mathtt{x}", unicodemath = "\\mttx", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL X"}
  , Record {point = "1D6A2", uchar = "\120482", latex = "\\mathtt{y}", unicodemath = "\\mtty", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL Y"}
  , Record {point = "1D6A3", uchar = "\120483", latex = "\\mathtt{z}", unicodemath = "\\mttz", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL MONOSPACE SMALL Z"}
  , Record {point = "1D6A4", uchar = "\120484", latex = "\\imath", unicodemath = "\\imath", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC SMALL DOTLESS I"}
  , Record {point = "1D6A5", uchar = "\120485", latex = "\\jmath", unicodemath = "\\jmath", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC SMALL DOTLESS J"}
  , Record {point = "1D6A8", uchar = "\120488", latex = "", unicodemath = "\\mbfAlpha", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL ALPHA"}
  , Record {point = "1D6A9", uchar = "\120489", latex = "", unicodemath = "\\mbfBeta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL BETA"}
  , Record {point = "1D6AA", uchar = "\120490", latex = "\\mathbf{\\Gamma}", unicodemath = "\\mbfGamma", cls = "A", category = "mathalpha", requirements = "-fourier", comments = "MATHEMATICAL BOLD CAPITAL GAMMA"}
  , Record {point = "1D6AB", uchar = "\120491", latex = "\\mathbf{\\Delta}", unicodemath = "\\mbfDelta", cls = "A", category = "mathalpha", requirements = "-fourier", comments = "MATHEMATICAL BOLD CAPITAL DELTA"}
  , Record {point = "1D6AC", uchar = "\120492", latex = "", unicodemath = "\\mbfEpsilon", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL EPSILON"}
  , Record {point = "1D6AD", uchar = "\120493", latex = "", unicodemath = "\\mbfZeta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL ZETA"}
  , Record {point = "1D6AE", uchar = "\120494", latex = "", unicodemath = "\\mbfEta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL ETA"}
  , Record {point = "1D6AF", uchar = "\120495", latex = "\\mathbf{\\Theta}", unicodemath = "\\mbfTheta", cls = "A", category = "mathalpha", requirements = "-fourier", comments = "MATHEMATICAL BOLD CAPITAL THETA"}
  , Record {point = "1D6B0", uchar = "\120496", latex = "", unicodemath = "\\mbfIota", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL IOTA"}
  , Record {point = "1D6B1", uchar = "\120497", latex = "", unicodemath = "\\mbfKappa", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL KAPPA"}
  , Record {point = "1D6B2", uchar = "\120498", latex = "\\mathbf{\\Lambda}", unicodemath = "\\mbfLambda", cls = "A", category = "mathalpha", requirements = "-fourier", comments = "mathematical bold capital lambda"}
  , Record {point = "1D6B3", uchar = "\120499", latex = "", unicodemath = "\\mbfMu", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL MU"}
  , Record {point = "1D6B4", uchar = "\120500", latex = "", unicodemath = "\\mbfNu", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL NU"}
  , Record {point = "1D6B5", uchar = "\120501", latex = "\\mathbf{\\Xi}", unicodemath = "\\mbfXi", cls = "A", category = "mathalpha", requirements = "-fourier", comments = "MATHEMATICAL BOLD CAPITAL XI"}
  , Record {point = "1D6B6", uchar = "\120502", latex = "", unicodemath = "\\mbfOmicron", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL OMICRON"}
  , Record {point = "1D6B7", uchar = "\120503", latex = "\\mathbf{\\Pi}", unicodemath = "\\mbfPi", cls = "A", category = "mathalpha", requirements = "-fourier", comments = "MATHEMATICAL BOLD CAPITAL PI"}
  , Record {point = "1D6B8", uchar = "\120504", latex = "", unicodemath = "\\mbfRho", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL RHO"}
  , Record {point = "1D6B9", uchar = "\120505", latex = "", unicodemath = "\\mbfvarTheta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL THETA SYMBOL"}
  , Record {point = "1D6BA", uchar = "\120506", latex = "\\mathbf{\\Sigma}", unicodemath = "\\mbfSigma", cls = "A", category = "mathalpha", requirements = "-fourier", comments = "MATHEMATICAL BOLD CAPITAL SIGMA"}
  , Record {point = "1D6BB", uchar = "\120507", latex = "", unicodemath = "\\mbfTau", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL TAU"}
  , Record {point = "1D6BC", uchar = "\120508", latex = "\\mathbf{\\Upsilon}", unicodemath = "\\mbfUpsilon", cls = "A", category = "mathalpha", requirements = "-fourier", comments = "MATHEMATICAL BOLD CAPITAL UPSILON"}
  , Record {point = "1D6BD", uchar = "\120509", latex = "\\mathbf{\\Phi}", unicodemath = "\\mbfPhi", cls = "A", category = "mathalpha", requirements = "-fourier", comments = "MATHEMATICAL BOLD CAPITAL PHI"}
  , Record {point = "1D6BE", uchar = "\120510", latex = "", unicodemath = "\\mbfChi", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL CHI"}
  , Record {point = "1D6BF", uchar = "\120511", latex = "\\mathbf{\\Psi}", unicodemath = "\\mbfPsi", cls = "A", category = "mathalpha", requirements = "-fourier", comments = "MATHEMATICAL BOLD CAPITAL PSI"}
  , Record {point = "1D6C0", uchar = "\120512", latex = "\\mathbf{\\Omega}", unicodemath = "\\mbfOmega", cls = "A", category = "mathalpha", requirements = "-fourier", comments = "MATHEMATICAL BOLD CAPITAL OMEGA"}
  , Record {point = "1D6C1", uchar = "\120513", latex = "", unicodemath = "\\mbfnabla", cls = "A", category = "mathord", requirements = "", comments = "MATHEMATICAL BOLD NABLA"}
  , Record {point = "1D6C2", uchar = "\120514", latex = "\\mathbf{\\alpha}", unicodemath = "\\mbfalpha", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL ALPHA"}
  , Record {point = "1D6C3", uchar = "\120515", latex = "\\mathbf{\\beta}", unicodemath = "\\mbfbeta", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL BETA"}
  , Record {point = "1D6C4", uchar = "\120516", latex = "\\mathbf{\\gamma}", unicodemath = "\\mbfgamma", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL GAMMA"}
  , Record {point = "1D6C5", uchar = "\120517", latex = "\\mathbf{\\delta}", unicodemath = "\\mbfdelta", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL DELTA"}
  , Record {point = "1D6C6", uchar = "\120518", latex = "\\mathbf{\\varepsilon}", unicodemath = "\\mbfepsilon", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL EPSILON"}
  , Record {point = "1D6C7", uchar = "\120519", latex = "\\mathbf{\\zeta}", unicodemath = "\\mbfzeta", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL ZETA"}
  , Record {point = "1D6C8", uchar = "\120520", latex = "\\mathbf{\\eta}", unicodemath = "\\mbfeta", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL ETA"}
  , Record {point = "1D6C9", uchar = "\120521", latex = "\\mathbf{\\theta}", unicodemath = "\\mbftheta", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL THETA"}
  , Record {point = "1D6CA", uchar = "\120522", latex = "\\mathbf{\\iota}", unicodemath = "\\mbfiota", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL IOTA"}
  , Record {point = "1D6CB", uchar = "\120523", latex = "\\mathbf{\\kappa}", unicodemath = "\\mbfkappa", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL KAPPA"}
  , Record {point = "1D6CC", uchar = "\120524", latex = "\\mathbf{\\lambda}", unicodemath = "\\mbflambda", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "mathematical bold small lambda"}
  , Record {point = "1D6CD", uchar = "\120525", latex = "\\mathbf{\\mu}", unicodemath = "\\mbfmu", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL MU"}
  , Record {point = "1D6CE", uchar = "\120526", latex = "\\mathbf{\\nu}", unicodemath = "\\mbfnu", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL NU"}
  , Record {point = "1D6CF", uchar = "\120527", latex = "\\mathbf{\\xi}", unicodemath = "\\mbfxi", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL XI"}
  , Record {point = "1D6D0", uchar = "\120528", latex = "", unicodemath = "\\mbfomicron", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL OMICRON"}
  , Record {point = "1D6D1", uchar = "\120529", latex = "\\mathbf{\\pi}", unicodemath = "\\mbfpi", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL PI"}
  , Record {point = "1D6D2", uchar = "\120530", latex = "\\mathbf{\\rho}", unicodemath = "\\mbfrho", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL RHO"}
  , Record {point = "1D6D3", uchar = "\120531", latex = "\\mathbf{\\varsigma}", unicodemath = "\\mbfvarsigma", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL FINAL SIGMA"}
  , Record {point = "1D6D4", uchar = "\120532", latex = "\\mathbf{\\sigma}", unicodemath = "\\mbfsigma", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL SIGMA"}
  , Record {point = "1D6D5", uchar = "\120533", latex = "\\mathbf{\\tau}", unicodemath = "\\mbftau", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL TAU"}
  , Record {point = "1D6D6", uchar = "\120534", latex = "\\mathbf{\\upsilon}", unicodemath = "\\mbfupsilon", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL UPSILON"}
  , Record {point = "1D6D7", uchar = "\120535", latex = "\\mathbf{\\varphi}", unicodemath = "\\mbfvarphi", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL PHI"}
  , Record {point = "1D6D8", uchar = "\120536", latex = "\\mathbf{\\chi}", unicodemath = "\\mbfchi", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL CHI"}
  , Record {point = "1D6D9", uchar = "\120537", latex = "\\mathbf{\\psi}", unicodemath = "\\mbfpsi", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL PSI"}
  , Record {point = "1D6DA", uchar = "\120538", latex = "\\mathbf{\\omega}", unicodemath = "\\mbfomega", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD SMALL OMEGA"}
  , Record {point = "1D6DB", uchar = "\120539", latex = "", unicodemath = "\\mbfpartial", cls = "A", category = "mathord", requirements = "", comments = "MATHEMATICAL BOLD PARTIAL DIFFERENTIAL"}
  , Record {point = "1D6DC", uchar = "\120540", latex = "\\mathbf{\\epsilon}", unicodemath = "\\mbfvarepsilon", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD EPSILON SYMBOL"}
  , Record {point = "1D6DD", uchar = "\120541", latex = "\\mathbf{\\vartheta}", unicodemath = "\\mbfvartheta", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD THETA SYMBOL"}
  , Record {point = "1D6DE", uchar = "\120542", latex = "", unicodemath = "\\mbfvarkappa", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD KAPPA SYMBOL"}
  , Record {point = "1D6DF", uchar = "\120543", latex = "\\mathbf{\\phi}", unicodemath = "\\mbfphi", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD PHI SYMBOL"}
  , Record {point = "1D6E0", uchar = "\120544", latex = "\\mathbf{\\varrho}", unicodemath = "\\mbfvarrho", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD RHO SYMBOL"}
  , Record {point = "1D6E1", uchar = "\120545", latex = "\\mathbf{\\varpi}", unicodemath = "\\mbfvarpi", cls = "A", category = "mathalpha", requirements = "omlmathbf", comments = "MATHEMATICAL BOLD PI SYMBOL"}
  , Record {point = "1D6E2", uchar = "\120546", latex = "", unicodemath = "\\mitAlpha", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL ALPHA"}
  , Record {point = "1D6E3", uchar = "\120547", latex = "", unicodemath = "\\mitBeta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL BETA"}
  , Record {point = "1D6E4", uchar = "\120548", latex = "\\Gamma", unicodemath = "\\mitGamma", cls = "A", category = "mathalpha", requirements = "slantedGreek", comments = "= \\mathit{\\Gamma} (-fourier), = \\varGamma (amsmath fourier), MATHEMATICAL ITALIC CAPITAL GAMMA"}
  , Record {point = "1D6E5", uchar = "\120549", latex = "\\Delta", unicodemath = "\\mitDelta", cls = "A", category = "mathalpha", requirements = "slantedGreek", comments = "= \\mathit{\\Delta} (-fourier), = \\varDelta (amsmath fourier), MATHEMATICAL ITALIC CAPITAL DELTA"}
  , Record {point = "1D6E6", uchar = "\120550", latex = "", unicodemath = "\\mitEpsilon", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL EPSILON"}
  , Record {point = "1D6E7", uchar = "\120551", latex = "", unicodemath = "\\mitZeta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL ZETA"}
  , Record {point = "1D6E8", uchar = "\120552", latex = "", unicodemath = "\\mitEta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL ETA"}
  , Record {point = "1D6E9", uchar = "\120553", latex = "\\Theta", unicodemath = "\\mitTheta", cls = "A", category = "mathalpha", requirements = "slantedGreek", comments = "= \\mathit{\\Theta} (-fourier), = \\varTheta (amsmath fourier), MATHEMATICAL ITALIC CAPITAL THETA"}
  , Record {point = "1D6EA", uchar = "\120554", latex = "", unicodemath = "\\mitIota", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL IOTA"}
  , Record {point = "1D6EB", uchar = "\120555", latex = "", unicodemath = "\\mitKappa", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL KAPPA"}
  , Record {point = "1D6EC", uchar = "\120556", latex = "\\Lambda", unicodemath = "\\mitLambda", cls = "A", category = "mathalpha", requirements = "slantedGreek", comments = "= \\mathit{\\Lambda} (-fourier), = \\varLambda (amsmath fourier), mathematical italic capital lambda"}
  , Record {point = "1D6ED", uchar = "\120557", latex = "", unicodemath = "\\mitMu", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL MU"}
  , Record {point = "1D6EE", uchar = "\120558", latex = "", unicodemath = "\\mitNu", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL NU"}
  , Record {point = "1D6EF", uchar = "\120559", latex = "\\Xi", unicodemath = "\\mitXi", cls = "A", category = "mathalpha", requirements = "slantedGreek", comments = "= \\mathit{\\Xi} (-fourier), = \\varXi (amsmath fourier), MATHEMATICAL ITALIC CAPITAL XI"}
  , Record {point = "1D6F0", uchar = "\120560", latex = "", unicodemath = "\\mitOmicron", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL OMICRON"}
  , Record {point = "1D6F1", uchar = "\120561", latex = "\\Pi", unicodemath = "\\mitPi", cls = "A", category = "mathalpha", requirements = "slantedGreek", comments = "= \\mathit{\\Pi} (-fourier), = \\varPi (amsmath fourier), MATHEMATICAL ITALIC CAPITAL PI"}
  , Record {point = "1D6F2", uchar = "\120562", latex = "", unicodemath = "\\mitRho", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL RHO"}
  , Record {point = "1D6F3", uchar = "\120563", latex = "", unicodemath = "\\mitvarTheta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL THETA SYMBOL"}
  , Record {point = "1D6F4", uchar = "\120564", latex = "\\Sigma", unicodemath = "\\mitSigma", cls = "A", category = "mathalpha", requirements = "slantedGreek", comments = "= \\mathit{\\Sigma} (-fourier), = \\varSigma (amsmath fourier), MATHEMATICAL ITALIC CAPITAL SIGMA"}
  , Record {point = "1D6F5", uchar = "\120565", latex = "", unicodemath = "\\mitTau", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL TAU"}
  , Record {point = "1D6F6", uchar = "\120566", latex = "\\Upsilon", unicodemath = "\\mitUpsilon", cls = "A", category = "mathalpha", requirements = "slantedGreek", comments = "= \\mathit{\\Upsilon} (-fourier), = \\varUpsilon (amsmath fourier), MATHEMATICAL ITALIC CAPITAL UPSILON"}
  , Record {point = "1D6F7", uchar = "\120567", latex = "\\Phi", unicodemath = "\\mitPhi", cls = "A", category = "mathalpha", requirements = "slantedGreek", comments = "= \\mathit{\\Phi} (-fourier), = \\varPhi (amsmath fourier), MATHEMATICAL ITALIC CAPITAL PHI"}
  , Record {point = "1D6F8", uchar = "\120568", latex = "", unicodemath = "\\mitChi", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC CAPITAL CHI"}
  , Record {point = "1D6F9", uchar = "\120569", latex = "\\Psi", unicodemath = "\\mitPsi", cls = "A", category = "mathalpha", requirements = "slantedGreek", comments = "= \\mathit{\\Psi} (-fourier), = \\varPsi (amsmath fourier), MATHEMATICAL ITALIC CAPITAL PSI"}
  , Record {point = "1D6FA", uchar = "\120570", latex = "\\Omega", unicodemath = "\\mitOmega", cls = "A", category = "mathalpha", requirements = "slantedGreek", comments = "= \\mathit{\\Omega} (-fourier), = \\varOmega (amsmath fourier), MATHEMATICAL ITALIC CAPITAL OMEGA"}
  , Record {point = "1D6FB", uchar = "\120571", latex = "", unicodemath = "\\mitnabla", cls = "A", category = "mathord", requirements = "", comments = "MATHEMATICAL ITALIC NABLA"}
  , Record {point = "1D6FC", uchar = "\120572", latex = "\\alpha", unicodemath = "\\mitalpha", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\alpha} (omlmathit), MATHEMATICAL ITALIC SMALL ALPHA"}
  , Record {point = "1D6FD", uchar = "\120573", latex = "\\beta", unicodemath = "\\mitbeta", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\beta} (omlmathit), MATHEMATICAL ITALIC SMALL BETA"}
  , Record {point = "1D6FE", uchar = "\120574", latex = "\\gamma", unicodemath = "\\mitgamma", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\gamma} (omlmathit), MATHEMATICAL ITALIC SMALL GAMMA"}
  , Record {point = "1D6FF", uchar = "\120575", latex = "\\delta", unicodemath = "\\mitdelta", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\delta} (omlmathit), MATHEMATICAL ITALIC SMALL DELTA"}
  , Record {point = "1D700", uchar = "\120576", latex = "\\varepsilon", unicodemath = "\\mitepsilon", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\varepsilon} (omlmathit), MATHEMATICAL ITALIC SMALL EPSILON"}
  , Record {point = "1D701", uchar = "\120577", latex = "\\zeta", unicodemath = "\\mitzeta", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\zeta} (omlmathit), MATHEMATICAL ITALIC SMALL ZETA"}
  , Record {point = "1D702", uchar = "\120578", latex = "\\eta", unicodemath = "\\miteta", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\eta} (omlmathit), MATHEMATICAL ITALIC SMALL ETA"}
  , Record {point = "1D703", uchar = "\120579", latex = "\\theta", unicodemath = "\\mittheta", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\theta} (omlmathit), MATHEMATICAL ITALIC SMALL THETA"}
  , Record {point = "1D704", uchar = "\120580", latex = "\\iota", unicodemath = "\\mitiota", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\iota} (omlmathit), MATHEMATICAL ITALIC SMALL IOTA"}
  , Record {point = "1D705", uchar = "\120581", latex = "\\kappa", unicodemath = "\\mitkappa", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\kappa} (omlmathit), MATHEMATICAL ITALIC SMALL KAPPA"}
  , Record {point = "1D706", uchar = "\120582", latex = "\\lambda", unicodemath = "\\mitlambda", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\lambda} (omlmathit), mathematical italic small lambda"}
  , Record {point = "1D707", uchar = "\120583", latex = "\\mu", unicodemath = "\\mitmu", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\mu} (omlmathit), MATHEMATICAL ITALIC SMALL MU"}
  , Record {point = "1D708", uchar = "\120584", latex = "\\nu", unicodemath = "\\mitnu", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\nu} (omlmathit), MATHEMATICAL ITALIC SMALL NU"}
  , Record {point = "1D709", uchar = "\120585", latex = "\\xi", unicodemath = "\\mitxi", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\xi} (omlmathit), MATHEMATICAL ITALIC SMALL XI"}
  , Record {point = "1D70A", uchar = "\120586", latex = "", unicodemath = "\\mitomicron", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL ITALIC SMALL OMICRON"}
  , Record {point = "1D70B", uchar = "\120587", latex = "\\pi", unicodemath = "\\mitpi", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\pi} (omlmathit), MATHEMATICAL ITALIC SMALL PI"}
  , Record {point = "1D70C", uchar = "\120588", latex = "\\rho", unicodemath = "\\mitrho", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\rho} (omlmathit), MATHEMATICAL ITALIC SMALL RHO"}
  , Record {point = "1D70D", uchar = "\120589", latex = "\\varsigma", unicodemath = "\\mitvarsigma", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\varsigma} (omlmathit), MATHEMATICAL ITALIC SMALL FINAL SIGMA"}
  , Record {point = "1D70E", uchar = "\120590", latex = "\\sigma", unicodemath = "\\mitsigma", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\sigma} (omlmathit), MATHEMATICAL ITALIC SMALL SIGMA"}
  , Record {point = "1D70F", uchar = "\120591", latex = "\\tau", unicodemath = "\\mittau", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\tau} (omlmathit), MATHEMATICAL ITALIC SMALL TAU"}
  , Record {point = "1D710", uchar = "\120592", latex = "\\upsilon", unicodemath = "\\mitupsilon", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\upsilon} (omlmathit), MATHEMATICAL ITALIC SMALL UPSILON"}
  , Record {point = "1D711", uchar = "\120593", latex = "\\varphi", unicodemath = "\\mitphi", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\varphi} (omlmathit), MATHEMATICAL ITALIC SMALL PHI"}
  , Record {point = "1D712", uchar = "\120594", latex = "\\chi", unicodemath = "\\mitchi", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\chi} (omlmathit), MATHEMATICAL ITALIC SMALL CHI"}
  , Record {point = "1D713", uchar = "\120595", latex = "\\psi", unicodemath = "\\mitpsi", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\psi} (omlmathit), MATHEMATICAL ITALIC SMALL PSI"}
  , Record {point = "1D714", uchar = "\120596", latex = "\\omega", unicodemath = "\\mitomega", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\omega} (omlmathit), MATHEMATICAL ITALIC SMALL OMEGA"}
  , Record {point = "1D715", uchar = "\120597", latex = "\\partial", unicodemath = "\\mitpartial", cls = "A", category = "mathord", requirements = "", comments = "= \\mathit{\\partial} (omlmathit), MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL"}
  , Record {point = "1D716", uchar = "\120598", latex = "\\epsilon", unicodemath = "\\mitvarepsilon", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\epsilon} (omlmathit), MATHEMATICAL ITALIC EPSILON SYMBOL"}
  , Record {point = "1D717", uchar = "\120599", latex = "\\vartheta", unicodemath = "\\mitvartheta", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\vartheta} (omlmathit), MATHEMATICAL ITALIC THETA SYMBOL"}
  , Record {point = "1D718", uchar = "\120600", latex = "\\varkappa", unicodemath = "\\mitvarkappa", cls = "A", category = "mathalpha", requirements = "amssymb", comments = "MATHEMATICAL ITALIC KAPPA SYMBOL"}
  , Record {point = "1D719", uchar = "\120601", latex = "\\phi", unicodemath = "\\mitvarphi", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\phi} (omlmathit), MATHEMATICAL ITALIC PHI SYMBOL"}
  , Record {point = "1D71A", uchar = "\120602", latex = "\\varrho", unicodemath = "\\mitvarrho", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\varrho} (omlmathit), MATHEMATICAL ITALIC RHO SYMBOL"}
  , Record {point = "1D71B", uchar = "\120603", latex = "\\varpi", unicodemath = "\\mitvarpi", cls = "A", category = "mathalpha", requirements = "", comments = "= \\mathit{\\varpi} (omlmathit), MATHEMATICAL ITALIC PI SYMBOL"}
  , Record {point = "1D71C", uchar = "\120604", latex = "", unicodemath = "\\mbfitAlpha", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL ALPHA"}
  , Record {point = "1D71D", uchar = "\120605", latex = "", unicodemath = "\\mbfitBeta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL BETA"}
  , Record {point = "1D71E", uchar = "\120606", latex = "\\mathbfit{\\Gamma}", unicodemath = "\\mbfitGamma", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\Gamma} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL GAMMA"}
  , Record {point = "1D71F", uchar = "\120607", latex = "\\mathbfit{\\Delta}", unicodemath = "\\mbfitDelta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\Delta} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL DELTA"}
  , Record {point = "1D720", uchar = "\120608", latex = "", unicodemath = "\\mbfitEpsilon", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL EPSILON"}
  , Record {point = "1D721", uchar = "\120609", latex = "", unicodemath = "\\mbfitZeta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL ZETA"}
  , Record {point = "1D722", uchar = "\120610", latex = "", unicodemath = "\\mbfitEta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL ETA"}
  , Record {point = "1D723", uchar = "\120611", latex = "\\mathbfit{\\Theta}", unicodemath = "\\mbfitTheta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\Theta} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL THETA"}
  , Record {point = "1D724", uchar = "\120612", latex = "", unicodemath = "\\mbfitIota", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL IOTA"}
  , Record {point = "1D725", uchar = "\120613", latex = "", unicodemath = "\\mbfitKappa", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL KAPPA"}
  , Record {point = "1D726", uchar = "\120614", latex = "\\mathbfit{\\Lambda}", unicodemath = "\\mbfitLambda", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\Lambda} (fixmath), mathematical bold italic capital lambda"}
  , Record {point = "1D727", uchar = "\120615", latex = "", unicodemath = "\\mbfitMu", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL MU"}
  , Record {point = "1D728", uchar = "\120616", latex = "", unicodemath = "\\mbfitNu", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL NU"}
  , Record {point = "1D729", uchar = "\120617", latex = "\\mathbfit{\\Xi}", unicodemath = "\\mbfitXi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\Xi} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL XI"}
  , Record {point = "1D72A", uchar = "\120618", latex = "", unicodemath = "\\mbfitOmicron", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL OMICRON"}
  , Record {point = "1D72B", uchar = "\120619", latex = "\\mathbfit{\\Pi}", unicodemath = "\\mbfitPi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\Pi} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL PI"}
  , Record {point = "1D72C", uchar = "\120620", latex = "", unicodemath = "\\mbfitRho", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL RHO"}
  , Record {point = "1D72D", uchar = "\120621", latex = "", unicodemath = "\\mbfitvarTheta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL THETA SYMBOL"}
  , Record {point = "1D72E", uchar = "\120622", latex = "\\mathbfit{\\Sigma}", unicodemath = "\\mbfitSigma", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\Sigma} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL SIGMA"}
  , Record {point = "1D72F", uchar = "\120623", latex = "", unicodemath = "\\mbfitTau", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL TAU"}
  , Record {point = "1D730", uchar = "\120624", latex = "\\mathbfit{\\Upsilon}", unicodemath = "\\mbfitUpsilon", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\Upsilon} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL UPSILON"}
  , Record {point = "1D731", uchar = "\120625", latex = "\\mathbfit{\\Phi}", unicodemath = "\\mbfitPhi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\Phi} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL PHI"}
  , Record {point = "1D732", uchar = "\120626", latex = "", unicodemath = "\\mbfitChi", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC CAPITAL CHI"}
  , Record {point = "1D733", uchar = "\120627", latex = "\\mathbfit{\\Psi}", unicodemath = "\\mbfitPsi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\Psi} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL PSI"}
  , Record {point = "1D734", uchar = "\120628", latex = "\\mathbfit{\\Omega}", unicodemath = "\\mbfitOmega", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\Omega} (fixmath), MATHEMATICAL BOLD ITALIC CAPITAL OMEGA"}
  , Record {point = "1D735", uchar = "\120629", latex = "", unicodemath = "\\mbfitnabla", cls = "A", category = "mathord", requirements = "", comments = "MATHEMATICAL BOLD ITALIC NABLA"}
  , Record {point = "1D736", uchar = "\120630", latex = "\\mathbfit{\\alpha}", unicodemath = "\\mbfitalpha", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\alpha} (fixmath), MATHEMATICAL BOLD ITALIC SMALL ALPHA"}
  , Record {point = "1D737", uchar = "\120631", latex = "\\mathbfit{\\beta}", unicodemath = "\\mbfitbeta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\beta} (fixmath), MATHEMATICAL BOLD ITALIC SMALL BETA"}
  , Record {point = "1D738", uchar = "\120632", latex = "\\mathbfit{\\gamma}", unicodemath = "\\mbfitgamma", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\gamma} (fixmath), MATHEMATICAL BOLD ITALIC SMALL GAMMA"}
  , Record {point = "1D739", uchar = "\120633", latex = "\\mathbfit{\\delta}", unicodemath = "\\mbfitdelta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\delta} (fixmath), MATHEMATICAL BOLD ITALIC SMALL DELTA"}
  , Record {point = "1D73A", uchar = "\120634", latex = "\\mathbfit{\\varepsilon}", unicodemath = "\\mbfitepsilon", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\varepsilon} (fixmath), MATHEMATICAL BOLD ITALIC SMALL EPSILON"}
  , Record {point = "1D73B", uchar = "\120635", latex = "\\mathbfit{\\zeta}", unicodemath = "\\mbfitzeta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\zeta} (fixmath), MATHEMATICAL BOLD ITALIC SMALL ZETA"}
  , Record {point = "1D73C", uchar = "\120636", latex = "\\mathbfit{\\eta}", unicodemath = "\\mbfiteta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\eta} (fixmath), MATHEMATICAL BOLD ITALIC SMALL ETA"}
  , Record {point = "1D73D", uchar = "\120637", latex = "\\mathbfit{\\theta}", unicodemath = "\\mbfittheta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\theta} (fixmath), MATHEMATICAL BOLD ITALIC SMALL THETA"}
  , Record {point = "1D73E", uchar = "\120638", latex = "\\mathbfit{\\iota}", unicodemath = "\\mbfitiota", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\iota} (fixmath), MATHEMATICAL BOLD ITALIC SMALL IOTA"}
  , Record {point = "1D73F", uchar = "\120639", latex = "\\mathbfit{\\kappa}", unicodemath = "\\mbfitkappa", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\kappa} (fixmath), MATHEMATICAL BOLD ITALIC SMALL KAPPA"}
  , Record {point = "1D740", uchar = "\120640", latex = "\\mathbfit{\\lambda}", unicodemath = "\\mbfitlambda", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\lambda} (fixmath), mathematical bold italic small lambda"}
  , Record {point = "1D741", uchar = "\120641", latex = "\\mathbfit{\\mu}", unicodemath = "\\mbfitmu", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\mu} (fixmath), MATHEMATICAL BOLD ITALIC SMALL MU"}
  , Record {point = "1D742", uchar = "\120642", latex = "\\mathbfit{\\nu}", unicodemath = "\\mbfitnu", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\nu} (fixmath), MATHEMATICAL BOLD ITALIC SMALL NU"}
  , Record {point = "1D743", uchar = "\120643", latex = "\\mathbfit{\\xi}", unicodemath = "\\mbfitxi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\xi} (fixmath), MATHEMATICAL BOLD ITALIC SMALL XI"}
  , Record {point = "1D744", uchar = "\120644", latex = "", unicodemath = "\\mbfitomicron", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC SMALL OMICRON"}
  , Record {point = "1D745", uchar = "\120645", latex = "\\mathbfit{\\pi}", unicodemath = "\\mbfitpi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\pi} (fixmath), MATHEMATICAL BOLD ITALIC SMALL PI"}
  , Record {point = "1D746", uchar = "\120646", latex = "\\mathbfit{\\rho}", unicodemath = "\\mbfitrho", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\rho} (fixmath), MATHEMATICAL BOLD ITALIC SMALL RHO"}
  , Record {point = "1D747", uchar = "\120647", latex = "\\mathbfit{\\varsigma}", unicodemath = "\\mbfitvarsigma", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\varsigma} (fixmath), MATHEMATICAL BOLD ITALIC SMALL FINAL SIGMA"}
  , Record {point = "1D748", uchar = "\120648", latex = "\\mathbfit{\\sigma}", unicodemath = "\\mbfitsigma", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\sigma} (fixmath), MATHEMATICAL BOLD ITALIC SMALL SIGMA"}
  , Record {point = "1D749", uchar = "\120649", latex = "\\mathbfit{\\tau}", unicodemath = "\\mbfittau", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\tau} (fixmath), MATHEMATICAL BOLD ITALIC SMALL TAU"}
  , Record {point = "1D74A", uchar = "\120650", latex = "\\mathbfit{\\upsilon}", unicodemath = "\\mbfitupsilon", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\upsilon} (fixmath), MATHEMATICAL BOLD ITALIC SMALL UPSILON"}
  , Record {point = "1D74B", uchar = "\120651", latex = "\\mathbfit{\\varphi}", unicodemath = "\\mbfitphi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\varphi} (fixmath), MATHEMATICAL BOLD ITALIC SMALL PHI"}
  , Record {point = "1D74C", uchar = "\120652", latex = "\\mathbfit{\\chi}", unicodemath = "\\mbfitchi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\chi} (fixmath), MATHEMATICAL BOLD ITALIC SMALL CHI"}
  , Record {point = "1D74D", uchar = "\120653", latex = "\\mathbfit{\\psi}", unicodemath = "\\mbfitpsi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\psi} (fixmath), MATHEMATICAL BOLD ITALIC SMALL PSI"}
  , Record {point = "1D74E", uchar = "\120654", latex = "\\mathbfit{\\omega}", unicodemath = "\\mbfitomega", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\omega} (fixmath), MATHEMATICAL BOLD ITALIC SMALL OMEGA"}
  , Record {point = "1D74F", uchar = "\120655", latex = "", unicodemath = "\\mbfitpartial", cls = "A", category = "mathord", requirements = "", comments = "MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL"}
  , Record {point = "1D750", uchar = "\120656", latex = "\\mathbfit{\\epsilon}", unicodemath = "\\mbfitvarepsilon", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\epsilon} (fixmath), MATHEMATICAL BOLD ITALIC EPSILON SYMBOL"}
  , Record {point = "1D751", uchar = "\120657", latex = "\\mathbfit{\\vartheta}", unicodemath = "\\mbfitvartheta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\vartheta} (fixmath), MATHEMATICAL BOLD ITALIC THETA SYMBOL"}
  , Record {point = "1D752", uchar = "\120658", latex = "", unicodemath = "\\mbfitvarkappa", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD ITALIC KAPPA SYMBOL"}
  , Record {point = "1D753", uchar = "\120659", latex = "\\mathbfit{\\phi}", unicodemath = "\\mbfitvarphi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\phi} (fixmath), MATHEMATICAL BOLD ITALIC PHI SYMBOL"}
  , Record {point = "1D754", uchar = "\120660", latex = "\\mathbfit{\\varrho}", unicodemath = "\\mbfitvarrho", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\varrho} (fixmath), MATHEMATICAL BOLD ITALIC RHO SYMBOL"}
  , Record {point = "1D755", uchar = "\120661", latex = "\\mathbfit{\\varpi}", unicodemath = "\\mbfitvarpi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "= \\mathbold{\\varpi} (fixmath), MATHEMATICAL BOLD ITALIC PI SYMBOL"}
  , Record {point = "1D756", uchar = "\120662", latex = "", unicodemath = "\\mbfsansAlpha", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL ALPHA"}
  , Record {point = "1D757", uchar = "\120663", latex = "", unicodemath = "\\mbfsansBeta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL BETA"}
  , Record {point = "1D758", uchar = "\120664", latex = "\\mathsfbf{\\Gamma}", unicodemath = "\\mbfsansGamma", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL GAMMA"}
  , Record {point = "1D759", uchar = "\120665", latex = "\\mathsfbf{\\Delta}", unicodemath = "\\mbfsansDelta", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL DELTA"}
  , Record {point = "1D75A", uchar = "\120666", latex = "", unicodemath = "\\mbfsansEpsilon", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL EPSILON"}
  , Record {point = "1D75B", uchar = "\120667", latex = "", unicodemath = "\\mbfsansZeta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL ZETA"}
  , Record {point = "1D75C", uchar = "\120668", latex = "", unicodemath = "\\mbfsansEta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL ETA"}
  , Record {point = "1D75D", uchar = "\120669", latex = "\\mathsfbf{\\Theta}", unicodemath = "\\mbfsansTheta", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA"}
  , Record {point = "1D75E", uchar = "\120670", latex = "", unicodemath = "\\mbfsansIota", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL IOTA"}
  , Record {point = "1D75F", uchar = "\120671", latex = "", unicodemath = "\\mbfsansKappa", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL KAPPA"}
  , Record {point = "1D760", uchar = "\120672", latex = "\\mathsfbf{\\Lambda}", unicodemath = "\\mbfsansLambda", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "mathematical sans-serif bold capital lambda"}
  , Record {point = "1D761", uchar = "\120673", latex = "", unicodemath = "\\mbfsansMu", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL MU"}
  , Record {point = "1D762", uchar = "\120674", latex = "", unicodemath = "\\mbfsansNu", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL NU"}
  , Record {point = "1D763", uchar = "\120675", latex = "\\mathsfbf{\\Xi}", unicodemath = "\\mbfsansXi", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL XI"}
  , Record {point = "1D764", uchar = "\120676", latex = "", unicodemath = "\\mbfsansOmicron", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL OMICRON"}
  , Record {point = "1D765", uchar = "\120677", latex = "\\mathsfbf{\\Pi}", unicodemath = "\\mbfsansPi", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL PI"}
  , Record {point = "1D766", uchar = "\120678", latex = "", unicodemath = "\\mbfsansRho", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL RHO"}
  , Record {point = "1D767", uchar = "\120679", latex = "", unicodemath = "\\mbfsansvarTheta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA SYMBOL"}
  , Record {point = "1D768", uchar = "\120680", latex = "\\mathsfbf{\\Sigma}", unicodemath = "\\mbfsansSigma", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL SIGMA"}
  , Record {point = "1D769", uchar = "\120681", latex = "", unicodemath = "\\mbfsansTau", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL TAU"}
  , Record {point = "1D76A", uchar = "\120682", latex = "\\mathsfbf{\\Upsilon}", unicodemath = "\\mbfsansUpsilon", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL UPSILON"}
  , Record {point = "1D76B", uchar = "\120683", latex = "\\mathsfbf{\\Phi}", unicodemath = "\\mbfsansPhi", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL PHI"}
  , Record {point = "1D76C", uchar = "\120684", latex = "", unicodemath = "\\mbfsansChi", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL CHI"}
  , Record {point = "1D76D", uchar = "\120685", latex = "\\mathsfbf{\\Psi}", unicodemath = "\\mbfsansPsi", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL PSI"}
  , Record {point = "1D76E", uchar = "\120686", latex = "\\mathsfbf{\\Omega}", unicodemath = "\\mbfsansOmega", cls = "A", category = "mathalpha", requirements = "mathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA"}
  , Record {point = "1D76F", uchar = "\120687", latex = "", unicodemath = "\\mbfsansnabla", cls = "A", category = "mathord", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD NABLA"}
  , Record {point = "1D770", uchar = "\120688", latex = "\\mathsfbf{\\alpha}", unicodemath = "\\mbfsansalpha", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA"}
  , Record {point = "1D771", uchar = "\120689", latex = "\\mathsfbf{\\beta}", unicodemath = "\\mbfsansbeta", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL BETA"}
  , Record {point = "1D772", uchar = "\120690", latex = "\\mathsfbf{\\gamma}", unicodemath = "\\mbfsansgamma", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL GAMMA"}
  , Record {point = "1D773", uchar = "\120691", latex = "\\mathsfbf{\\delta}", unicodemath = "\\mbfsansdelta", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL DELTA"}
  , Record {point = "1D774", uchar = "\120692", latex = "\\mathsfbf{\\varepsilon}", unicodemath = "\\mbfsansepsilon", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL EPSILON"}
  , Record {point = "1D775", uchar = "\120693", latex = "\\mathsfbf{\\zeta}", unicodemath = "\\mbfsanszeta", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL ZETA"}
  , Record {point = "1D776", uchar = "\120694", latex = "\\mathsfbf{\\eta}", unicodemath = "\\mbfsanseta", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL ETA"}
  , Record {point = "1D777", uchar = "\120695", latex = "\\mathsfbf{\\theta}", unicodemath = "\\mbfsanstheta", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL THETA"}
  , Record {point = "1D778", uchar = "\120696", latex = "\\mathsfbf{\\iota}", unicodemath = "\\mbfsansiota", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL IOTA"}
  , Record {point = "1D779", uchar = "\120697", latex = "\\mathsfbf{\\kappa}", unicodemath = "\\mbfsanskappa", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL KAPPA"}
  , Record {point = "1D77A", uchar = "\120698", latex = "\\mathsfbf{\\lambda}", unicodemath = "\\mbfsanslambda", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "mathematical sans-serif bold small lambda"}
  , Record {point = "1D77B", uchar = "\120699", latex = "\\mathsfbf{\\mu}", unicodemath = "\\mbfsansmu", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL MU"}
  , Record {point = "1D77C", uchar = "\120700", latex = "\\mathsfbf{\\nu}", unicodemath = "\\mbfsansnu", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL NU"}
  , Record {point = "1D77D", uchar = "\120701", latex = "\\mathsfbf{\\xi}", unicodemath = "\\mbfsansxi", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL XI"}
  , Record {point = "1D77E", uchar = "\120702", latex = "", unicodemath = "\\mbfsansomicron", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL OMICRON"}
  , Record {point = "1D77F", uchar = "\120703", latex = "\\mathsfbf{\\pi}", unicodemath = "\\mbfsanspi", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL PI"}
  , Record {point = "1D780", uchar = "\120704", latex = "\\mathsfbf{\\rho}", unicodemath = "\\mbfsansrho", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL RHO"}
  , Record {point = "1D781", uchar = "\120705", latex = "\\mathsfbf{\\varsigma}", unicodemath = "\\mbfsansvarsigma", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL FINAL SIGMA"}
  , Record {point = "1D782", uchar = "\120706", latex = "\\mathsfbf{\\sigma}", unicodemath = "\\mbfsanssigma", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL SIGMA"}
  , Record {point = "1D783", uchar = "\120707", latex = "\\mathsfbf{\\tau}", unicodemath = "\\mbfsanstau", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL TAU"}
  , Record {point = "1D784", uchar = "\120708", latex = "\\mathsfbf{\\upsilon}", unicodemath = "\\mbfsansupsilon", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL UPSILON"}
  , Record {point = "1D785", uchar = "\120709", latex = "\\mathsfbf{\\varphi}", unicodemath = "\\mbfsansphi", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL PHI"}
  , Record {point = "1D786", uchar = "\120710", latex = "\\mathsfbf{\\chi}", unicodemath = "\\mbfsanschi", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL CHI"}
  , Record {point = "1D787", uchar = "\120711", latex = "\\mathsfbf{\\psi}", unicodemath = "\\mbfsanspsi", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL PSI"}
  , Record {point = "1D788", uchar = "\120712", latex = "\\mathsfbf{\\omega}", unicodemath = "\\mbfsansomega", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA"}
  , Record {point = "1D789", uchar = "\120713", latex = "", unicodemath = "\\mbfsanspartial", cls = "A", category = "mathord", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL"}
  , Record {point = "1D78A", uchar = "\120714", latex = "\\mathsfbf{\\epsilon}", unicodemath = "\\mbfsansvarepsilon", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL"}
  , Record {point = "1D78B", uchar = "\120715", latex = "\\mathsfbf{\\vartheta}", unicodemath = "\\mbfsansvartheta", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD THETA SYMBOL"}
  , Record {point = "1D78C", uchar = "\120716", latex = "", unicodemath = "\\mbfsansvarkappa", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD KAPPA SYMBOL"}
  , Record {point = "1D78D", uchar = "\120717", latex = "\\mathsfbf{\\phi}", unicodemath = "\\mbfsansvarphi", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD PHI SYMBOL"}
  , Record {point = "1D78E", uchar = "\120718", latex = "\\mathsfbf{\\varrho}", unicodemath = "\\mbfsansvarrho", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD RHO SYMBOL"}
  , Record {point = "1D78F", uchar = "\120719", latex = "\\mathsfbf{\\varpi}", unicodemath = "\\mbfsansvarpi", cls = "A", category = "mathalpha", requirements = "omlmathsfbf", comments = "MATHEMATICAL SANS-SERIF BOLD PI SYMBOL"}
  , Record {point = "1D790", uchar = "\120720", latex = "", unicodemath = "\\mbfitsansAlpha", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ALPHA"}
  , Record {point = "1D791", uchar = "\120721", latex = "", unicodemath = "\\mbfitsansBeta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL BETA"}
  , Record {point = "1D792", uchar = "\120722", latex = "\\mathsfbfit{\\Gamma}", unicodemath = "\\mbfitsansGamma", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL GAMMA"}
  , Record {point = "1D793", uchar = "\120723", latex = "\\mathsfbfit{\\Delta}", unicodemath = "\\mbfitsansDelta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL DELTA"}
  , Record {point = "1D794", uchar = "\120724", latex = "", unicodemath = "\\mbfitsansEpsilon", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL EPSILON"}
  , Record {point = "1D795", uchar = "\120725", latex = "", unicodemath = "\\mbfitsansZeta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ZETA"}
  , Record {point = "1D796", uchar = "\120726", latex = "", unicodemath = "\\mbfitsansEta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ETA"}
  , Record {point = "1D797", uchar = "\120727", latex = "\\mathsfbfit{\\Theta}", unicodemath = "\\mbfitsansTheta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA"}
  , Record {point = "1D798", uchar = "\120728", latex = "", unicodemath = "\\mbfitsansIota", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL IOTA"}
  , Record {point = "1D799", uchar = "\120729", latex = "", unicodemath = "\\mbfitsansKappa", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL KAPPA"}
  , Record {point = "1D79A", uchar = "\120730", latex = "\\mathsfbfit{\\Lambda}", unicodemath = "\\mbfitsansLambda", cls = "A", category = "mathalpha", requirements = "isomath", comments = "mathematical sans-serif bold italic capital lambda"}
  , Record {point = "1D79B", uchar = "\120731", latex = "", unicodemath = "\\mbfitsansMu", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL MU"}
  , Record {point = "1D79C", uchar = "\120732", latex = "", unicodemath = "\\mbfitsansNu", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL NU"}
  , Record {point = "1D79D", uchar = "\120733", latex = "\\mathsfbfit{\\Xi}", unicodemath = "\\mbfitsansXi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL XI"}
  , Record {point = "1D79E", uchar = "\120734", latex = "", unicodemath = "\\mbfitsansOmicron", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMICRON"}
  , Record {point = "1D79F", uchar = "\120735", latex = "\\mathsfbfit{\\Pi}", unicodemath = "\\mbfitsansPi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PI"}
  , Record {point = "1D7A0", uchar = "\120736", latex = "", unicodemath = "\\mbfitsansRho", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL RHO"}
  , Record {point = "1D7A1", uchar = "\120737", latex = "", unicodemath = "\\mbfitsansvarTheta", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA SYMBOL"}
  , Record {point = "1D7A2", uchar = "\120738", latex = "\\mathsfbfit{\\Sigma}", unicodemath = "\\mbfitsansSigma", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL SIGMA"}
  , Record {point = "1D7A3", uchar = "\120739", latex = "", unicodemath = "\\mbfitsansTau", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL TAU"}
  , Record {point = "1D7A4", uchar = "\120740", latex = "\\mathsfbfit{\\Upsilon}", unicodemath = "\\mbfitsansUpsilon", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL UPSILON"}
  , Record {point = "1D7A5", uchar = "\120741", latex = "\\mathsfbfit{\\Phi}", unicodemath = "\\mbfitsansPhi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PHI"}
  , Record {point = "1D7A6", uchar = "\120742", latex = "", unicodemath = "\\mbfitsansChi", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL CHI"}
  , Record {point = "1D7A7", uchar = "\120743", latex = "\\mathsfbfit{\\Psi}", unicodemath = "\\mbfitsansPsi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PSI"}
  , Record {point = "1D7A8", uchar = "\120744", latex = "\\mathsfbfit{\\Omega}", unicodemath = "\\mbfitsansOmega", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA"}
  , Record {point = "1D7A9", uchar = "\120745", latex = "", unicodemath = "\\mbfitsansnabla", cls = "A", category = "mathord", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA"}
  , Record {point = "1D7AA", uchar = "\120746", latex = "\\mathsfbfit{\\alpha}", unicodemath = "\\mbfitsansalpha", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA"}
  , Record {point = "1D7AB", uchar = "\120747", latex = "\\mathsfbfit{\\beta}", unicodemath = "\\mbfitsansbeta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL BETA"}
  , Record {point = "1D7AC", uchar = "\120748", latex = "\\mathsfbfit{\\gamma}", unicodemath = "\\mbfitsansgamma", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL GAMMA"}
  , Record {point = "1D7AD", uchar = "\120749", latex = "\\mathsfbfit{\\delta}", unicodemath = "\\mbfitsansdelta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL DELTA"}
  , Record {point = "1D7AE", uchar = "\120750", latex = "\\mathsfbfit{\\varepsilon}", unicodemath = "\\mbfitsansepsilon", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL EPSILON"}
  , Record {point = "1D7AF", uchar = "\120751", latex = "\\mathsfbfit{\\zeta}", unicodemath = "\\mbfitsanszeta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ZETA"}
  , Record {point = "1D7B0", uchar = "\120752", latex = "\\mathsfbfit{\\eta}", unicodemath = "\\mbfitsanseta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ETA"}
  , Record {point = "1D7B1", uchar = "\120753", latex = "\\mathsfbfit{\\theta}", unicodemath = "\\mbfitsanstheta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL THETA"}
  , Record {point = "1D7B2", uchar = "\120754", latex = "\\mathsfbfit{\\iota}", unicodemath = "\\mbfitsansiota", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL IOTA"}
  , Record {point = "1D7B3", uchar = "\120755", latex = "\\mathsfbfit{\\kappa}", unicodemath = "\\mbfitsanskappa", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL KAPPA"}
  , Record {point = "1D7B4", uchar = "\120756", latex = "\\mathsfbfit{\\lambda}", unicodemath = "\\mbfitsanslambda", cls = "A", category = "mathalpha", requirements = "isomath", comments = "mathematical sans-serif bold italic small lambda"}
  , Record {point = "1D7B5", uchar = "\120757", latex = "\\mathsfbfit{\\mu}", unicodemath = "\\mbfitsansmu", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL MU"}
  , Record {point = "1D7B6", uchar = "\120758", latex = "\\mathsfbfit{\\nu}", unicodemath = "\\mbfitsansnu", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL NU"}
  , Record {point = "1D7B7", uchar = "\120759", latex = "\\mathsfbfit{\\xi}", unicodemath = "\\mbfitsansxi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL XI"}
  , Record {point = "1D7B8", uchar = "\120760", latex = "", unicodemath = "\\mbfitsansomicron", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMICRON"}
  , Record {point = "1D7B9", uchar = "\120761", latex = "\\mathsfbfit{\\pi}", unicodemath = "\\mbfitsanspi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PI"}
  , Record {point = "1D7BA", uchar = "\120762", latex = "\\mathsfbfit{\\rho}", unicodemath = "\\mbfitsansrho", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL RHO"}
  , Record {point = "1D7BB", uchar = "\120763", latex = "\\mathsfbfit{\\varsigma}", unicodemath = "\\mbfitsansvarsigma", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL FINAL SIGMA"}
  , Record {point = "1D7BC", uchar = "\120764", latex = "\\mathsfbfit{\\sigma}", unicodemath = "\\mbfitsanssigma", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL SIGMA"}
  , Record {point = "1D7BD", uchar = "\120765", latex = "\\mathsfbfit{\\tau}", unicodemath = "\\mbfitsanstau", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL TAU"}
  , Record {point = "1D7BE", uchar = "\120766", latex = "\\mathsfbfit{\\upsilon}", unicodemath = "\\mbfitsansupsilon", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL UPSILON"}
  , Record {point = "1D7BF", uchar = "\120767", latex = "\\mathsfbfit{\\varphi}", unicodemath = "\\mbfitsansphi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PHI"}
  , Record {point = "1D7C0", uchar = "\120768", latex = "\\mathsfbfit{\\chi}", unicodemath = "\\mbfitsanschi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL CHI"}
  , Record {point = "1D7C1", uchar = "\120769", latex = "\\mathsfbfit{\\psi}", unicodemath = "\\mbfitsanspsi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PSI"}
  , Record {point = "1D7C2", uchar = "\120770", latex = "\\mathsfbfit{\\omega}", unicodemath = "\\mbfitsansomega", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA"}
  , Record {point = "1D7C3", uchar = "\120771", latex = "", unicodemath = "\\mbfitsanspartial", cls = "A", category = "mathord", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL"}
  , Record {point = "1D7C4", uchar = "\120772", latex = "\\mathsfbfit{\\epsilon}", unicodemath = "\\mbfitsansvarepsilon", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL"}
  , Record {point = "1D7C5", uchar = "\120773", latex = "\\mathsfbfit{\\vartheta}", unicodemath = "\\mbfitsansvartheta", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC THETA SYMBOL"}
  , Record {point = "1D7C6", uchar = "\120774", latex = "", unicodemath = "\\mbfitsansvarkappa", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC KAPPA SYMBOL"}
  , Record {point = "1D7C7", uchar = "\120775", latex = "\\mathsfbfit{\\phi}", unicodemath = "\\mbfitsansvarphi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC PHI SYMBOL"}
  , Record {point = "1D7C8", uchar = "\120776", latex = "\\mathsfbfit{\\varrho}", unicodemath = "\\mbfitsansvarrho", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC RHO SYMBOL"}
  , Record {point = "1D7C9", uchar = "\120777", latex = "\\mathsfbfit{\\varpi}", unicodemath = "\\mbfitsansvarpi", cls = "A", category = "mathalpha", requirements = "isomath", comments = "MATHEMATICAL SANS-SERIF BOLD ITALIC PI SYMBOL"}
  , Record {point = "1D7CA", uchar = "\120778", latex = "", unicodemath = "\\mbfDigamma", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD CAPITAL DIGAMMA"}
  , Record {point = "1D7CB", uchar = "\120779", latex = "", unicodemath = "\\mbfdigamma", cls = "A", category = "mathalpha", requirements = "", comments = "MATHEMATICAL BOLD SMALL DIGAMMA"}
  , Record {point = "1D7CE", uchar = "\120782", latex = "\\mathbf{0}", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "mathematical bold digit 0"}
  , Record {point = "1D7CF", uchar = "\120783", latex = "\\mathbf{1}", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "mathematical bold digit 1"}
  , Record {point = "1D7D0", uchar = "\120784", latex = "\\mathbf{2}", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "mathematical bold digit 2"}
  , Record {point = "1D7D1", uchar = "\120785", latex = "\\mathbf{3}", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "mathematical bold digit 3"}
  , Record {point = "1D7D2", uchar = "\120786", latex = "\\mathbf{4}", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "mathematical bold digit 4"}
  , Record {point = "1D7D3", uchar = "\120787", latex = "\\mathbf{5}", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "mathematical bold digit 5"}
  , Record {point = "1D7D4", uchar = "\120788", latex = "\\mathbf{6}", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "mathematical bold digit 6"}
  , Record {point = "1D7D5", uchar = "\120789", latex = "\\mathbf{7}", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "mathematical bold digit 7"}
  , Record {point = "1D7D6", uchar = "\120790", latex = "\\mathbf{8}", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "mathematical bold digit 8"}
  , Record {point = "1D7D7", uchar = "\120791", latex = "\\mathbf{9}", unicodemath = "", cls = "N", category = "mathord", requirements = "", comments = "mathematical bold digit 9"}
  , Record {point = "1D7D8", uchar = "\120792", latex = "\\mathbb{0}", unicodemath = "\\Bbbzero", cls = "N", category = "mathord", requirements = "bbold", comments = "mathematical double-struck digit 0"}
  , Record {point = "1D7D9", uchar = "\120793", latex = "\\mathbb{1}", unicodemath = "\\Bbbone", cls = "N", category = "mathord", requirements = "bbold fourier", comments = "= \\mathds{1} (dsfont), mathematical double-struck digit 1"}
  , Record {point = "1D7DA", uchar = "\120794", latex = "\\mathbb{2}", unicodemath = "\\Bbbtwo", cls = "N", category = "mathord", requirements = "bbold", comments = "mathematical double-struck digit 2"}
  , Record {point = "1D7DB", uchar = "\120795", latex = "\\mathbb{3}", unicodemath = "\\Bbbthree", cls = "N", category = "mathord", requirements = "bbold", comments = "mathematical double-struck digit 3"}
  , Record {point = "1D7DC", uchar = "\120796", latex = "\\mathbb{4}", unicodemath = "\\Bbbfour", cls = "N", category = "mathord", requirements = "bbold", comments = "mathematical double-struck digit 4"}
  , Record {point = "1D7DD", uchar = "\120797", latex = "\\mathbb{5}", unicodemath = "\\Bbbfive", cls = "N", category = "mathord", requirements = "bbold", comments = "mathematical double-struck digit 5"}
  , Record {point = "1D7DE", uchar = "\120798", latex = "\\mathbb{6}", unicodemath = "\\Bbbsix", cls = "N", category = "mathord", requirements = "bbold", comments = "mathematical double-struck digit 6"}
  , Record {point = "1D7DF", uchar = "\120799", latex = "\\mathbb{7}", unicodemath = "\\Bbbseven", cls = "N", category = "mathord", requirements = "bbold", comments = "mathematical double-struck digit 7"}
  , Record {point = "1D7E0", uchar = "\120800", latex = "\\mathbb{8}", unicodemath = "\\Bbbeight", cls = "N", category = "mathord", requirements = "bbold", comments = "mathematical double-struck digit 8"}
  , Record {point = "1D7E1", uchar = "\120801", latex = "\\mathbb{9}", unicodemath = "\\Bbbnine", cls = "N", category = "mathord", requirements = "bbold", comments = "mathematical double-struck digit 9"}
  , Record {point = "1D7E2", uchar = "\120802", latex = "\\mathsf{0}", unicodemath = "\\msanszero", cls = "N", category = "mathord", requirements = "", comments = "mathematical sans-serif digit 0"}
  , Record {point = "1D7E3", uchar = "\120803", latex = "\\mathsf{1}", unicodemath = "\\msansone", cls = "N", category = "mathord", requirements = "", comments = "mathematical sans-serif digit 1"}
  , Record {point = "1D7E4", uchar = "\120804", latex = "\\mathsf{2}", unicodemath = "\\msanstwo", cls = "N", category = "mathord", requirements = "", comments = "mathematical sans-serif digit 2"}
  , Record {point = "1D7E5", uchar = "\120805", latex = "\\mathsf{3}", unicodemath = "\\msansthree", cls = "N", category = "mathord", requirements = "", comments = "mathematical sans-serif digit 3"}
  , Record {point = "1D7E6", uchar = "\120806", latex = "\\mathsf{4}", unicodemath = "\\msansfour", cls = "N", category = "mathord", requirements = "", comments = "mathematical sans-serif digit 4"}
  , Record {point = "1D7E7", uchar = "\120807", latex = "\\mathsf{5}", unicodemath = "\\msansfive", cls = "N", category = "mathord", requirements = "", comments = "mathematical sans-serif digit 5"}
  , Record {point = "1D7E8", uchar = "\120808", latex = "\\mathsf{6}", unicodemath = "\\msanssix", cls = "N", category = "mathord", requirements = "", comments = "mathematical sans-serif digit 6"}
  , Record {point = "1D7E9", uchar = "\120809", latex = "\\mathsf{7}", unicodemath = "\\msansseven", cls = "N", category = "mathord", requirements = "", comments = "mathematical sans-serif digit 7"}
  , Record {point = "1D7EA", uchar = "\120810", latex = "\\mathsf{8}", unicodemath = "\\msanseight", cls = "N", category = "mathord", requirements = "", comments = "mathematical sans-serif digit 8"}
  , Record {point = "1D7EB", uchar = "\120811", latex = "\\mathsf{9}", unicodemath = "\\msansnine", cls = "N", category = "mathord", requirements = "", comments = "mathematical sans-serif digit 9"}
  , Record {point = "1D7EC", uchar = "\120812", latex = "\\mathsfbf{0}", unicodemath = "\\mbfsanszero", cls = "N", category = "mathord", requirements = "mathsfbf", comments = "mathematical sans-serif bold digit 0"}
  , Record {point = "1D7ED", uchar = "\120813", latex = "\\mathsfbf{1}", unicodemath = "\\mbfsansone", cls = "N", category = "mathord", requirements = "mathsfbf", comments = "mathematical sans-serif bold digit 1"}
  , Record {point = "1D7EE", uchar = "\120814", latex = "\\mathsfbf{2}", unicodemath = "\\mbfsanstwo", cls = "N", category = "mathord", requirements = "mathsfbf", comments = "mathematical sans-serif bold digit 2"}
  , Record {point = "1D7EF", uchar = "\120815", latex = "\\mathsfbf{3}", unicodemath = "\\mbfsansthree", cls = "N", category = "mathord", requirements = "mathsfbf", comments = "mathematical sans-serif bold digit 3"}
  , Record {point = "1D7F0", uchar = "\120816", latex = "\\mathsfbf{4}", unicodemath = "\\mbfsansfour", cls = "N", category = "mathord", requirements = "mathsfbf", comments = "mathematical sans-serif bold digit 4"}
  , Record {point = "1D7F1", uchar = "\120817", latex = "\\mathsfbf{5}", unicodemath = "\\mbfsansfive", cls = "N", category = "mathord", requirements = "mathsfbf", comments = "mathematical sans-serif bold digit 5"}
  , Record {point = "1D7F2", uchar = "\120818", latex = "\\mathsfbf{6}", unicodemath = "\\mbfsanssix", cls = "N", category = "mathord", requirements = "mathsfbf", comments = "mathematical sans-serif bold digit 6"}
  , Record {point = "1D7F3", uchar = "\120819", latex = "\\mathsfbf{7}", unicodemath = "\\mbfsansseven", cls = "N", category = "mathord", requirements = "mathsfbf", comments = "mathematical sans-serif bold digit 7"}
  , Record {point = "1D7F4", uchar = "\120820", latex = "\\mathsfbf{8}", unicodemath = "\\mbfsanseight", cls = "N", category = "mathord", requirements = "mathsfbf", comments = "mathematical sans-serif bold digit 8"}
  , Record {point = "1D7F5", uchar = "\120821", latex = "\\mathsfbf{9}", unicodemath = "\\mbfsansnine", cls = "N", category = "mathord", requirements = "mathsfbf", comments = "mathematical sans-serif bold digit 9"}
  , Record {point = "1D7F6", uchar = "\120822", latex = "\\mathtt{0}", unicodemath = "\\mttzero", cls = "N", category = "mathord", requirements = "", comments = "mathematical monospace digit 0"}
  , Record {point = "1D7F7", uchar = "\120823", latex = "\\mathtt{1}", unicodemath = "\\mttone", cls = "N", category = "mathord", requirements = "", comments = "mathematical monospace digit 1"}
  , Record {point = "1D7F8", uchar = "\120824", latex = "\\mathtt{2}", unicodemath = "\\mtttwo", cls = "N", category = "mathord", requirements = "", comments = "mathematical monospace digit 2"}
  , Record {point = "1D7F9", uchar = "\120825", latex = "\\mathtt{3}", unicodemath = "\\mttthree", cls = "N", category = "mathord", requirements = "", comments = "mathematical monospace digit 3"}
  , Record {point = "1D7FA", uchar = "\120826", latex = "\\mathtt{4}", unicodemath = "\\mttfour", cls = "N", category = "mathord", requirements = "", comments = "mathematical monospace digit 4"}
  , Record {point = "1D7FB", uchar = "\120827", latex = "\\mathtt{5}", unicodemath = "\\mttfive", cls = "N", category = "mathord", requirements = "", comments = "mathematical monospace digit 5"}
  , Record {point = "1D7FC", uchar = "\120828", latex = "\\mathtt{6}", unicodemath = "\\mttsix", cls = "N", category = "mathord", requirements = "", comments = "mathematical monospace digit 6"}
  , Record {point = "1D7FD", uchar = "\120829", latex = "\\mathtt{7}", unicodemath = "\\mttseven", cls = "N", category = "mathord", requirements = "", comments = "mathematical monospace digit 7"}
  , Record {point = "1D7FE", uchar = "\120830", latex = "\\mathtt{8}", unicodemath = "\\mtteight", cls = "N", category = "mathord", requirements = "", comments = "mathematical monospace digit 8"}
  , Record {point = "1D7FF", uchar = "\120831", latex = "\\mathtt{9}", unicodemath = "\\mttnine", cls = "N", category = "mathord", requirements = "", comments = "mathematical monospace digit 9"}]
