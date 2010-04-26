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

{- | Functions for parsing a LaTeX formula to a Haskell representation.
-}

module Text.TeXMath.Parser (expr, formula, Exp(..), TeXSymbolType(..), ArrayLine, Alignment(..))
where

import Control.Monad
import Data.Char (isAlphaNum, isDigit)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

data TeXSymbolType = Ord | Op | Bin | Rel | Open | Close | Pun | Accent
                     deriving (Show, Read, Eq)

data Alignment = AlignLeft | AlignCenter | AlignRight | AlignDefault
                 deriving (Show, Read, Eq)

type ArrayLine = [[Exp]]

data Exp =
    ENumber String
  | EGrouped [Exp]
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
  | EUnary String Exp
  | EScaled String Exp
  | EStretchy Exp
  | EArray [Alignment] [ArrayLine]
  | EText String String
  deriving (Show, Read, Eq)

texMathDef :: LanguageDef st
texMathDef = LanguageDef 
   { commentStart   = ""
   , commentEnd     = ""
   , commentLine    = "%"
   , nestedComments = False
   , identStart     = letter
   , identLetter    = letter
   , opStart        = opLetter texMathDef
   , opLetter       = oneOf ":_+*/=^-(),;.?'~[]<>!"
   , reservedOpNames= []
   , reservedNames  = []
   , caseSensitive  = True
   }

-- The parser

expr1 :: GenParser Char st Exp
expr1 =  choice [
    inbraces
  , variable
  , number
  , texSymbol
  , text
  , root 
  , unary
  , binary
  , enclosure
  , array
  , diacritical
  , escaped
  ]

formula :: GenParser Char st [Exp]
formula = do
  whiteSpace
  f <- many expr
  eof
  return f

expr :: GenParser Char st Exp
expr = do
  a <- expr1
  limits <- option False (try $ symbol "\\limits" >> return True)
  subSup limits a <|> superOrSubscripted limits a <|> return a

inbraces :: GenParser Char st Exp
inbraces = liftM EGrouped (braces $ many $ notFollowedBy (char '}') >> expr)

texToken :: GenParser Char st Exp
texToken = inbraces <|> inbrackets <|>
             do c <- anyChar
                spaces
                return $ if isDigit c
                            then (ENumber [c])
                            else (EIdentifier [c])

inbrackets :: GenParser Char st Exp
inbrackets = liftM EGrouped (brackets $ many $ notFollowedBy (char ']') >> expr)

number :: GenParser Char st Exp
number = lexeme $ liftM ENumber $ many1 digit

enclosure :: GenParser Char st Exp
enclosure = basicEnclosure <|> left <|> right <|> scaledEnclosure

basicEnclosure :: GenParser Char st Exp
basicEnclosure = choice $ map (\(s, v) -> try (symbol s) >> return v) enclosures

left :: GenParser Char st Exp
left = try $ do
  try (symbol "\\left")
  enc <- basicEnclosure <|> (symbol "." >> return (ESymbol Open "\xFEFF"))
  case enc of
    (ESymbol Open _) -> tilRight enc <|> return (EStretchy enc)
    _ -> pzero

right :: GenParser Char st Exp
right = try $ do
  try (symbol "\\right")
  enc <- basicEnclosure <|> (symbol "." >> return (ESymbol Open "\xFEFF"))
  case enc of
    (ESymbol Close x) -> return (EStretchy $ ESymbol Open x)
    _ -> pzero

-- We want stuff between \left( and \right) to be in an mrow,
-- so that the scaling is based just on this unit, and not the
-- whole containing formula.
tilRight :: Exp -> GenParser Char st Exp
tilRight start = try $ do
  contents <- manyTill expr
               (try $ symbol "\\right" >> lookAhead basicEnclosure)
  end <- basicEnclosure
  return $ EGrouped $ EStretchy start : (contents ++ [EStretchy end])

scaledEnclosure :: GenParser Char st Exp
scaledEnclosure = try $ do
  cmd <- command
  case M.lookup cmd scalers of
       Just  r -> liftM (EScaled r . EStretchy) basicEnclosure
       Nothing -> pzero 

arrayLine :: GenParser Char st ArrayLine
arrayLine = notFollowedBy (try $ char '\\' >> symbol "end" >> return '\n') >>
  sepBy1 (many (notFollowedBy (try $ char '\\' >> char '\\') >> expr)) (symbol "&")

array :: GenParser Char st Exp
array = stdarray <|> eqnarray <|> align <|> cases <|> matrix

matrix :: GenParser Char st Exp
matrix =  matrixWith "pmatrix" "(" ")"
      <|> matrixWith "bmatrix" "[" "]"
      <|> matrixWith "Bmatrix" "{" "}"
      <|> matrixWith "vmatrix" "\x2223" "\x2223"
      <|> matrixWith "Vmatrix" "\x2225" "\x2225"

matrixWith :: String -> String -> String -> GenParser Char st Exp
matrixWith keywd opendelim closedelim =
  inEnvironment keywd $ do
    aligns <- option [] arrayAlignments
    lines' <- sepEndBy1 arrayLine (try $ symbol "\\\\")
    return $ EGrouped [ EStretchy (ESymbol Open opendelim)
                      , EArray aligns lines'
                      , EStretchy (ESymbol Close closedelim)]

stdarray :: GenParser Char st Exp
stdarray = inEnvironment "array" $ do
  aligns <- option [] arrayAlignments
  liftM (EArray aligns) $ sepEndBy1 arrayLine (try $ symbol "\\\\")

eqnarray :: GenParser Char st Exp
eqnarray = inEnvironment "eqnarray" $
  liftM (EArray [AlignRight, AlignCenter, AlignLeft]) $
    sepEndBy1 arrayLine (try $ symbol "\\\\")

align :: GenParser Char st Exp
align = inEnvironment "align" $
  liftM (EArray [AlignRight, AlignLeft]) $
    sepEndBy1 arrayLine (try $ symbol "\\\\")

cases :: GenParser Char st Exp
cases = inEnvironment "cases" $ do
  rs <- sepEndBy1 arrayLine (try $ symbol "\\\\")
  return $ EGrouped [EStretchy (ESymbol Open "{"), EArray [] rs]

arrayAlignments :: GenParser Char st [Alignment]
arrayAlignments = try $ do
  as <- braces (many letter)
  let letterToAlignment 'l' = AlignLeft
      letterToAlignment 'c' = AlignCenter
      letterToAlignment 'r' = AlignRight
      letterToAlignment _   = AlignDefault
  return $ map letterToAlignment as

inEnvironment :: String
              -> GenParser Char st Exp
              -> GenParser Char st Exp
inEnvironment envType p = do
  try $ do char '\\'
           symbol "begin"
           braces $ symbol envType >> optional (symbol "*")
  result <- p
  char '\\'
  symbol "end"
  braces $ symbol envType >> optional (symbol "*")
  return result 

variable :: GenParser Char st Exp
variable = do
  v <- letter
  spaces
  return $ EIdentifier [v]

subSup :: Bool -> Exp -> GenParser Char st Exp
subSup limits a = try $ do
  char '_'
  b <- expr1
  char '^'
  c <- expr
  return $ if limits
              then EUnderover a b c
              else ESubsup a b c 

superOrSubscripted :: Bool -> Exp -> GenParser Char st Exp
superOrSubscripted limits a = try $ do
  c <- oneOf "^_"
  b <- expr
  case c of
       '^' -> return $ if limits
                          then EOver a b
                          else ESuper a b
       '_' -> return $ if limits
                          then EUnder a b
                          else ESub a b
       _   -> pzero 

escaped :: GenParser Char st Exp
escaped = lexeme $ try $ 
          char '\\' >>
          liftM (ESymbol Ord . (:[])) (satisfy $ not . isAlphaNum)

command :: GenParser Char st String
command = try $ char '\\' >> liftM ('\\':) (identifier <|> lexeme (count 1 anyChar))

unaryOps :: [String]
unaryOps = ["\\sqrt", "\\surd"]

textOps :: M.Map String (String -> Exp)
textOps = M.fromList
          [ ("\\textrm", EText "normal")
          , ("\\mathrm", EText "normal")
          , ("\\text",   EText "normal")
          , ("\\mbox",   EText "normal")
          , ("\\mathbf", EText "bold")
          , ("\\textbf", EText "bold")
          , ("\\mathit", EText "italic")
          , ("\\textit", EText "italic")
          , ("\\mathtt", EText "monospace")
          , ("\\texttt", EText "monospace")
          , ("\\mathsf", EText "sans-serif")
          , ("\\mathbb", \e -> maybe (EText "double-struck" e) (ESymbol Pun) (M.lookup e mathbb))
          , ("\\mathcal", \e -> maybe (EText "script" e) (ESymbol Pun) (M.lookup e mathcal))
          , ("\\mathfrak", EText "fraktur")
          ]

diacritical :: GenParser Char st Exp
diacritical = try $ do
  c <- command
  case M.lookup c diacriticals of
       Just r  -> liftM r texToken
       Nothing -> pzero 

diacriticals :: M.Map String (Exp -> Exp)
diacriticals = M.fromList
               [ ("\\acute", \e -> EOver e (ESymbol Accent "\x00B4"))
               , ("\\grave", \e -> EOver e (ESymbol Accent "\x0060"))
               , ("\\breve", \e -> EOver e (ESymbol Accent "\x02D8"))
               , ("\\check", \e -> EOver e (ESymbol Accent "\x02C7"))
               , ("\\dot", \e -> EOver e (ESymbol Accent "."))
               , ("\\ddot", \e -> EOver e (ESymbol Accent ".."))
               , ("\\mathring", \e -> EOver e (ESymbol Accent "\x00B0"))
               , ("\\vec", \e -> EOver e (ESymbol Accent "\x20D7"))
               , ("\\overrightarrow", \e -> EOver e (ESymbol Accent "\x20D7"))
               , ("\\overleftarrow", \e -> EOver e (ESymbol Accent "\x20D6"))
               , ("\\hat", \e -> EOver e (ESymbol Accent "\x005E"))
               , ("\\widehat", \e -> EOver e (ESymbol Accent "\x0302"))
               , ("\\tilde", \e -> EOver e (ESymbol Accent "~"))
               , ("\\widetilde", \e -> EOver e (ESymbol Accent "\x02DC"))
               , ("\\bar", \e -> EOver e (ESymbol Accent "\x203E"))
               , ("\\overbrace", \e -> EOver e (ESymbol Accent "\xFE37"))
               , ("\\overbracket", \e -> EOver e (ESymbol Accent "\x23B4"))
               , ("\\overline", \e -> EOver e (ESymbol Accent "\x00AF"))
               , ("\\underbrace", \e -> EUnder e (ESymbol Accent "\xFE38"))
               , ("\\underbracket", \e -> EUnder e (ESymbol Accent "\x23B5"))
               , ("\\underline", \e -> EUnder e (ESymbol Accent "\x00AF"))
               ]

unary :: GenParser Char st Exp
unary = try $ do
  c <- command
  unless (c `elem` unaryOps) pzero 
  a <- texToken
  return $ EUnary c a

text :: GenParser Char st Exp
text = try $ do
  c <- command
  case M.lookup c textOps of
       Just f   -> liftM f $ braces (many (noneOf "}" <|> (char '\\' >> char '}')))
       Nothing  -> pzero 

-- note: sqrt can be unary, \sqrt{2}, or binary, \sqrt[3]{2}
root :: GenParser Char st Exp
root = try $ do
  try (symbol "\\sqrt") <|> symbol "\\surd"
  a <- inbrackets
  b <- texToken
  return $ EBinary "\\sqrt" b a

binary :: GenParser Char st Exp
binary = try $ do
  c <- command
  unless (c `elem` binaryOps) pzero 
  a <- texToken
  b <- texToken
  return $ EBinary c a b

texSymbol :: GenParser Char st Exp
texSymbol = try $ do
  sym <- operator <|> command
  case M.lookup sym symbols of
       Just s   -> return s
       Nothing  -> pzero 

-- The lexer
lexer :: P.TokenParser st
lexer = P.makeTokenParser texMathDef
    
lexeme :: CharParser st a -> CharParser st a
lexeme = P.lexeme lexer

whiteSpace :: CharParser st () 
whiteSpace = P.whiteSpace lexer

identifier :: CharParser st String
identifier = lexeme (P.identifier lexer)

operator :: CharParser st String
operator = lexeme $ liftM (:[]) (opLetter texMathDef)
                 <|> many1 (char '\'')

symbol :: String -> CharParser st String
symbol = lexeme . P.symbol lexer

braces :: CharParser st a -> CharParser st a 
braces = lexeme . P.braces lexer

brackets :: CharParser st a -> CharParser st a
brackets = lexeme . P.brackets lexer

binaryOps :: [String]
binaryOps = ["\\frac", "\\tfrac", "\\dfrac", "\\stackrel", "\\overset", "\\underset", "\\binom"]

scalers :: M.Map String String
scalers = M.fromList
          [ ("\\bigg", "2.2")
          , ("\\Bigg", "2.9")
          , ("\\big", "1.2")
          , ("\\Big", "1.6")
          , ("\\biggr", "2.2")
          , ("\\Biggr", "2.9")
          , ("\\bigr", "1.2")
          , ("\\Bigr", "1.6")
          , ("\\biggl", "2.2")
          , ("\\Biggl", "2.9")
          , ("\\bigl", "1.2")
          , ("\\Bigl", "1.6")
          ]

enclosures :: [(String, Exp)]
enclosures = [ ("(", ESymbol Open "(")
             , (")", ESymbol Close ")")
             , ("[", ESymbol Open "[")
             , ("]", ESymbol Close "]")
             , ("\\{", ESymbol Open "{")
             , ("\\}", ESymbol Close "}")
             , ("\\lbrack", ESymbol Open "[")
             , ("\\lbrace", ESymbol Open "{")
             , ("\\rbrack", ESymbol Close "]")
             , ("\\rbrace", ESymbol Close "}")
             , ("\\llbracket", ESymbol Open "\x27E6")
             , ("\\rrbracket", ESymbol Close "\x230B")
             , ("\\langle", ESymbol Open "\x27E8")
             , ("\\rangle", ESymbol Close "\x27E9")
             , ("\\lfloor", ESymbol Open "\x230A")
             , ("\\rfloor", ESymbol Close "\x230B")
             , ("\\lceil", ESymbol Open "\x2308")
             , ("\\rceil", ESymbol Close "\x2309")
             , ("|", ESymbol Open "\x2223")
             , ("|", ESymbol Close "\x2223")
             , ("\\|", ESymbol Open "\x2225")
             , ("\\|", ESymbol Close "\x2225")
             , ("\\vert", ESymbol Open "\x2223")
             , ("\\vert", ESymbol Close "\x2223")
             , ("\\Vert", ESymbol Open "\x2225")
             , ("\\Vert", ESymbol Close "\x2225")
             ]

symbols :: M.Map String Exp
symbols = M.fromList [
             ("+", ESymbol Bin "+")
           , ("-", ESymbol Bin "-")
           , ("*", ESymbol Bin "*")
           , (",", ESymbol Pun ",")
           , (".", ESymbol Pun ".")
           , (";", ESymbol Pun ";")
           , (":", ESymbol Pun ":")
           , ("?", ESymbol Pun "?")
           , (">", ESymbol Rel ">")
           , ("<", ESymbol Rel "<")
           , ("!", ESymbol Ord "!")
           , ("'", ESymbol Ord "\x02B9")
           , ("''", ESymbol Ord "\x02BA")
           , ("'''", ESymbol Ord "\x2034")
           , ("''''", ESymbol Ord "\x2057")
           , ("=", ESymbol Rel "=")
           , (":=", ESymbol Rel ":=")
           , ("\\mid", ESymbol Bin "\x2223")
           , ("\\parallel", ESymbol Rel "\x2225")
           , ("\\backslash", ESymbol Bin "\x2216")
           , ("/", ESymbol Bin "/")
           , ("\\setminus",	ESymbol Bin "\\")
           , ("\\times", ESymbol Bin "\x00D7")
           , ("\\alpha", ESymbol Ord "\x03B1")
           , ("\\beta", ESymbol Ord "\x03B2")
           , ("\\chi", ESymbol Ord "\x03C7")
           , ("\\delta", ESymbol Ord "\x03B4")
           , ("\\Delta", ESymbol Op "\x0394")
           , ("\\epsi", ESymbol Ord "\x03B5")
           , ("\\varepsilon", ESymbol Ord "\x025B")
           , ("\\eta", ESymbol Ord "\x03B7")
           , ("\\gamma", ESymbol Ord "\x03B3")
           , ("\\Gamma", ESymbol Op "\x0393") 
           , ("\\iota", ESymbol Ord "\x03B9")
           , ("\\kappa", ESymbol Ord "\x03BA")
           , ("\\lambda", ESymbol Ord "\x03BB")
           , ("\\Lambda", ESymbol Op "\x039B") 
           , ("\\mu", ESymbol Ord "\x03BC")
           , ("\\nu", ESymbol Ord "\x03BD")
           , ("\\omega", ESymbol Ord "\x03C9")
           , ("\\Omega", ESymbol Op "\x03A9")
           , ("\\phi", ESymbol Ord "\x03C6")
           , ("\\varphi", ESymbol Ord "\x03D5")
           , ("\\Phi", ESymbol Op "\x03A6") 
           , ("\\pi", ESymbol Ord "\x03C0")
           , ("\\Pi", ESymbol Op "\x03A0") 
           , ("\\psi", ESymbol Ord "\x03C8")
           , ("\\Psi", ESymbol Ord "\x03A8")
           , ("\\rho", ESymbol Ord "\x03C1")
           , ("\\sigma", ESymbol Ord "\x03C3")
           , ("\\Sigma", ESymbol Op "\x03A3") 
           , ("\\tau", ESymbol Ord "\x03C4")
           , ("\\theta", ESymbol Ord "\x03B8")
           , ("\\vartheta", ESymbol Ord "\x03D1")
           , ("\\Theta", ESymbol Op "\x0398") 
           , ("\\upsilon", ESymbol Ord "\x03C5")
           , ("\\xi", ESymbol Ord "\x03BE")
           , ("\\Xi", ESymbol Op "\x039E") 
           , ("\\zeta", ESymbol Ord "\x03B6")
           , ("\\frac12", ESymbol Ord "\x00BD")
           , ("\\frac14", ESymbol Ord "\x00BC")
           , ("\\frac34", ESymbol Ord "\x00BE")
           , ("\\frac13", ESymbol Ord "\x2153")
           , ("\\frac23", ESymbol Ord "\x2154")
           , ("\\frac15", ESymbol Ord "\x2155")
           , ("\\frac25", ESymbol Ord "\x2156")
           , ("\\frac35", ESymbol Ord "\x2157")
           , ("\\frac45", ESymbol Ord "\x2158")
           , ("\\frac16", ESymbol Ord "\x2159")
           , ("\\frac56", ESymbol Ord "\x215A")
           , ("\\frac18", ESymbol Ord "\x215B")
           , ("\\frac38", ESymbol Ord "\x215C")
           , ("\\frac58", ESymbol Ord "\x215D")
           , ("\\frac78", ESymbol Ord "\x215E")
           , ("\\pm", ESymbol Bin "\x00B1")
           , ("\\mp", ESymbol Bin "\x2213")
           , ("\\triangleleft", ESymbol Bin "\x22B2")
           , ("\\triangleright", ESymbol Bin "\x22B3")
           , ("\\cdot", ESymbol Bin "\x22C5")
           , ("\\star", ESymbol Bin "\x22C6")
           , ("\\ast", ESymbol Bin "\x002A")
           , ("\\times", ESymbol Bin "\x00D7")
           , ("\\div", ESymbol Bin "\x00F7")
           , ("\\circ", ESymbol Bin "\x2218")
           , ("\\bullet", ESymbol Bin "\x2022")
           , ("\\oplus", ESymbol Bin "\x2295")
           , ("\\ominus", ESymbol Bin "\x2296")
           , ("\\otimes", ESymbol Bin "\x2297")
           , ("\\bigcirc", ESymbol Bin "\x25CB")
           , ("\\oslash", ESymbol Bin "\x2298")
           , ("\\odot", ESymbol Bin "\x2299")
           , ("\\land", ESymbol Bin "\x2227")
           , ("\\wedge", ESymbol Bin "\x2227")
           , ("\\lor", ESymbol Bin "\x2228")
           , ("\\vee", ESymbol Bin "\x2228")
           , ("\\cap", ESymbol Bin "\x2229")
           , ("\\cup", ESymbol Bin "\x222A")
           , ("\\sqcap", ESymbol Bin "\x2293")
           , ("\\sqcup", ESymbol Bin "\x2294")
           , ("\\uplus", ESymbol Bin "\x228E")
           , ("\\amalg", ESymbol Bin "\x2210")
           , ("\\bigtriangleup", ESymbol Bin "\x25B3")
           , ("\\bigtriangledown", ESymbol Bin "\x25BD")
           , ("\\dag", ESymbol Bin "\x2020")
           , ("\\dagger", ESymbol Bin "\x2020")
           , ("\\ddag", ESymbol Bin "\x2021")
           , ("\\ddagger", ESymbol Bin "\x2021")
           , ("\\lhd", ESymbol Bin "\x22B2")
           , ("\\rhd", ESymbol Bin "\x22B3")
           , ("\\unlhd", ESymbol Bin "\x22B4")
           , ("\\unrhd", ESymbol Bin "\x22B5")
           , ("\\lt", ESymbol Rel "<")
           , ("\\gt", ESymbol Rel ">")
           , ("\\ne", ESymbol Rel "\x2260")
           , ("\\neq", ESymbol Rel "\x2260")
           , ("\\le", ESymbol Rel "\x2264")
           , ("\\leq", ESymbol Rel "\x2264")
           , ("\\leqslant", ESymbol Rel "\x2264")
           , ("\\ge", ESymbol Rel "\x2265")
           , ("\\geq", ESymbol Rel "\x2265")
           , ("\\geqslant", ESymbol Rel "\x2265")
           , ("\\equiv", ESymbol Rel "\x2261")
           , ("\\ll", ESymbol Rel "\x226A")
           , ("\\gg", ESymbol Rel "\x226B")
           , ("\\doteq", ESymbol Rel "\x2250")
           , ("\\prec", ESymbol Rel "\x227A")
           , ("\\succ", ESymbol Rel "\x227B")
           , ("\\preceq", ESymbol Rel "\x227C")
           , ("\\succeq", ESymbol Rel "\x227D")
           , ("\\subset", ESymbol Rel "\x2282")
           , ("\\supset", ESymbol Rel "\x2283")
           , ("\\subseteq", ESymbol Rel "\x2286")
           , ("\\supseteq", ESymbol Rel "\x2287")
           , ("\\sqsubset", ESymbol Rel "\x228F")
           , ("\\sqsupset", ESymbol Rel "\x2290")
           , ("\\sqsubseteq", ESymbol Rel "\x2291")
           , ("\\sqsupseteq", ESymbol Rel "\x2292")
           , ("\\sim", ESymbol Rel "\x223C")
           , ("\\simeq", ESymbol Rel "\x2243")
           , ("\\approx", ESymbol Rel "\x2248")
           , ("\\cong", ESymbol Rel "\x2245")
           , ("\\Join", ESymbol Rel "\x22C8")
           , ("\\bowtie", ESymbol Rel "\x22C8")
           , ("\\in", ESymbol Rel "\x2208")
           , ("\\ni", ESymbol Rel "\x220B")
           , ("\\owns", ESymbol Rel "\x220B")
           , ("\\propto", ESymbol Rel "\x221D")
           , ("\\vdash", ESymbol Rel "\x22A2")
           , ("\\dashv", ESymbol Rel "\x22A3")
           , ("\\models", ESymbol Rel "\x22A8")
           , ("\\perp", ESymbol Rel "\x22A5")
           , ("\\smile", ESymbol Rel "\x2323")
           , ("\\frown", ESymbol Rel "\x2322")
           , ("\\asymp", ESymbol Rel "\x224D")
           , ("\\notin", ESymbol Rel "\x2209")
           , ("\\gets", ESymbol Rel "\x2190")
           , ("\\leftarrow", ESymbol Rel "\x2190")
           , ("\\to", ESymbol Rel "\x2192")
           , ("\\rightarrow", ESymbol Rel "\x2192")
           , ("\\leftrightarrow", ESymbol Rel "\x2194")
           , ("\\uparrow", ESymbol Rel "\x2191")
           , ("\\downarrow", ESymbol Rel "\x2193")
           , ("\\updownarrow", ESymbol Rel "\x2195")
           , ("\\Leftarrow", ESymbol Rel "\x21D0")
           , ("\\Rightarrow", ESymbol Rel "\x21D2")
           , ("\\Leftrightarrow", ESymbol Rel "\x21D4")
           , ("\\iff", ESymbol Rel "\x21D4")
           , ("\\Uparrow", ESymbol Rel "\x21D1")
           , ("\\Downarrow", ESymbol Rel "\x21D3")
           , ("\\Updownarrow", ESymbol Rel "\x21D5")
           , ("\\mapsto", ESymbol Rel "\x21A6")
           , ("\\longleftarrow", ESymbol Rel "\x2190")
           , ("\\longrightarrow", ESymbol Rel "\x2192")
           , ("\\longleftrightarrow", ESymbol Rel "\x2194")
           , ("\\Longleftarrow", ESymbol Rel "\x21D0")
           , ("\\Longrightarrow", ESymbol Rel "\x21D2")
           , ("\\Longleftrightarrow", ESymbol Rel "\x21D4")
           , ("\\longmapsto", ESymbol Rel "\x21A6")
           , ("\\sum", ESymbol Op "\x2211")
           , ("\\prod", ESymbol Op "\x220F")
           , ("\\bigcap", ESymbol Op "\x22C2")
           , ("\\bigcup", ESymbol Op "\x22C3")
           , ("\\bigwedge", ESymbol Op "\x22C0")
           , ("\\bigvee", ESymbol Op "\x22C1")
           , ("\\bigsqcap", ESymbol Op "\x2A05")
           , ("\\bigsqcup", ESymbol Op "\x2A06")
           , ("\\coprod", ESymbol Op "\x2210")
           , ("\\bigoplus", ESymbol Op "\x2A01")
           , ("\\bigotimes", ESymbol Op "\x2A02")
           , ("\\bigodot", ESymbol Op "\x2A00")
           , ("\\biguplus", ESymbol Op "\x2A04")
           , ("\\int", ESymbol Op "\x222B")
           , ("\\iint", ESymbol Op "\x222C")
           , ("\\iiint", ESymbol Op "\x222D")
           , ("\\oint", ESymbol Op "\x222E")
           , ("\\prime", ESymbol Ord "\x2032")
           , ("\\dots", ESymbol Ord "\x2026")
           , ("\\ldots", ESymbol Ord "\x2026")
           , ("\\cdots", ESymbol Ord "\x22EF")
           , ("\\vdots", ESymbol Ord "\x22EE")
           , ("\\ddots", ESymbol Ord "\x22F1")
           , ("\\forall", ESymbol Op "\x2200")
           , ("\\exists", ESymbol Op "\x2203")
           , ("\\Re", ESymbol Ord "\x211C")
           , ("\\Im", ESymbol Ord "\x2111")
           , ("\\aleph", ESymbol Ord "\x2135")
           , ("\\hbar", ESymbol Ord "\x210F")
           , ("\\ell", ESymbol Ord "\x2113")
           , ("\\wp", ESymbol Ord "\x2118")
           , ("\\emptyset", ESymbol Ord "\x2205")
           , ("\\infty", ESymbol Ord "\x221E")
           , ("\\partial", ESymbol Ord "\x2202")
           , ("\\nabla", ESymbol Ord "\x2207")
           , ("\\triangle", ESymbol Ord "\x25B3")
           , ("\\therefore", ESymbol Pun "\x2234")
           , ("\\angle", ESymbol Ord "\x2220")
           , ("\\diamond", ESymbol Op "\x22C4")
           , ("\\Diamond", ESymbol Op "\x25C7")
           , ("\\neg", ESymbol Op "\x00AC")
           , ("\\lnot", ESymbol Ord "\x00AC")
           , ("\\bot", ESymbol Ord "\x22A5")
           , ("\\top", ESymbol Ord "\x22A4")
           , ("\\square", ESymbol Ord "\x25AB")
           , ("\\Box", ESymbol Op "\x25A1")
           , ("\\wr", ESymbol Ord "\x2240")
           , ("\\!", ESpace "-0.167em")
           , ("\\,", ESpace "0.167em")
           , ("\\>", ESpace "0.222em")
           , ("\\:", ESpace "0.222em")
           , ("\\;", ESpace "0.278em")
           , ("~", ESpace "0.333em")
           , ("\\quad", ESpace "1em")
           , ("\\qquad", ESpace "2em")
           , ("\\arccos", EMathOperator "arccos")
           , ("\\arcsin", EMathOperator "arcsin")
           , ("\\arctan", EMathOperator "arctan")
           , ("\\arg", EMathOperator "arg")
           , ("\\cos", EMathOperator "cos")
           , ("\\cosh", EMathOperator "cosh")
           , ("\\cot", EMathOperator "cot")
           , ("\\coth", EMathOperator "coth")
           , ("\\csc", EMathOperator "csc")
           , ("\\deg", EMathOperator "deg")
           , ("\\det", EMathOperator "det")
           , ("\\dim", EMathOperator "dim")
           , ("\\exp", EMathOperator "exp")
           , ("\\gcd", EMathOperator "gcd")
           , ("\\hom", EMathOperator "hom")
           , ("\\inf", EMathOperator "inf")
           , ("\\ker", EMathOperator "ker")
           , ("\\lg", EMathOperator "lg")
           , ("\\lim", EMathOperator "lim")
           , ("\\liminf", EMathOperator "liminf")
           , ("\\limsup", EMathOperator "limsup")
           , ("\\ln", EMathOperator "ln")
           , ("\\log", EMathOperator "log")
           , ("\\max", EMathOperator "max")
           , ("\\min", EMathOperator "min")
           , ("\\Pr", EMathOperator "Pr")
           , ("\\sec", EMathOperator "sec")
           , ("\\sin", EMathOperator "sin")
           , ("\\sinh", EMathOperator "sinh")
           , ("\\sup", EMathOperator "sup")
           , ("\\tan", EMathOperator "tan")
           , ("\\tanh", EMathOperator "tanh")
           ] 

-- MathML has a mathvariant attribute which is unimplemented in Firefox
--    (see https://bugzilla.mozilla.org/show_bug.cgi?id=114365)
-- Therefore, we translate mathcal to unicode symbols directly.
-- This list is from http://www.w3.org/TR/MathML2/script.html
mathcal :: M.Map String String
mathcal = M.fromList [
             ("A", "\x1D49C")
           , ("B", "\x0212C")
           , ("C", "\x1D49E")
           , ("D", "\x1D49F")
           , ("E", "\x02130")
           , ("F", "\x02131")
           , ("G", "\x1D4A2")
           , ("H", "\x0210B")
           , ("I", "\x02110")
           , ("J", "\x1D4A5")
           , ("K", "\x1D4A6")
           , ("L", "\x02112")
           , ("M", "\x02133")
           , ("N", "\x1D4A9")
           , ("O", "\x1D4AA")
           , ("P", "\x1D4AB")
           , ("Q", "\x1D4AC")
           , ("R", "\x0211B")
           , ("S", "\x1D4AE")
           , ("T", "\x1D4AF")
           , ("U", "\x1D4B0")
           , ("V", "\x1D4B1")
           , ("W", "\x1D4B2")
           , ("X", "\x1D4B3")
           , ("Y", "\x1D4B4")
           , ("Z", "\x1D4B5")
           , ("a", "\x1D4B6")
           , ("b", "\x1D4B7")
           , ("c", "\x1D4B8")
           , ("d", "\x1D4B9")
           , ("e", "\x0212F")
           , ("f", "\x1D4BB")
           , ("g", "\x0210A")
           , ("h", "\x1D4BD")
           , ("i", "\x1D4BE")
           , ("j", "\x1D4BF")
           , ("k", "\x1D4C0")
           , ("l", "\x1D4C1")
           , ("m", "\x1D4C2")
           , ("n", "\x1D4C3")
           , ("o", "\x02134")
           , ("p", "\x1D4C5")
           , ("q", "\x1D4C6")
           , ("r", "\x1D4C7")
           , ("s", "\x1D4C8")
           , ("t", "\x1D4C9")
           , ("u", "\x1D4CA")
           , ("v", "\x1D4CB")
           , ("w", "\x1D4CC")
           , ("x", "\x1D4CD")
           , ("y", "\x1D4CE")
           , ("z", "\x1D4CF")
           ]

-- Similar to mathcal above, we translate manually.
-- This list is from http://www.w3.org/TR/MathML2/double-struck.html
mathbb :: M.Map String String
mathbb = M.fromList [
             ("A", "\x1D538")
           , ("B", "\x1D539")
           , ("C", "\x02102")
           , ("D", "\x1D53B")
           , ("E", "\x1D53C")
           , ("F", "\x1D53D")
           , ("G", "\x1D53E")
           , ("H", "\x0210D")
           , ("I", "\x1D540")
           , ("J", "\x1D541")
           , ("K", "\x1D542")
           , ("L", "\x1D543")
           , ("M", "\x1D544")
           , ("N", "\x02115")
           , ("O", "\x1D546")
           , ("P", "\x02119")
           , ("Q", "\x0211A")
           , ("R", "\x0211D")
           , ("S", "\x1D54A")
           , ("T", "\x1D54B")
           , ("U", "\x1D54C")
           , ("V", "\x1D54D")
           , ("W", "\x1D54E")
           , ("X", "\x1D54F")
           , ("Y", "\x1D550")
           , ("Z", "\x02124")
           , ("a", "\x1D552")
           , ("b", "\x1D553")
           , ("c", "\x1D554")
           , ("d", "\x1D555")
           , ("e", "\x1D556")
           , ("f", "\x1D557")
           , ("g", "\x1D558")
           , ("h", "\x1D559")
           , ("i", "\x1D55A")
           , ("j", "\x1D55B")
           , ("k", "\x1D55C")
           , ("l", "\x1D55D")
           , ("m", "\x1D55E")
           , ("n", "\x1D55F")
           , ("o", "\x1D560")
           , ("p", "\x1D561")
           , ("q", "\x1D562")
           , ("r", "\x1D563")
           , ("s", "\x1D564")
           , ("t", "\x1D565")
           , ("u", "\x1D566")
           , ("v", "\x1D567")
           , ("w", "\x1D568")
           , ("x", "\x1D569")
           , ("y", "\x1D56A")
           , ("z", "\x1D56B")
           , ("0", "\x1D7D8")
           , ("1", "\x1D7D9")
           , ("2", "\x1D7DA")
           , ("3", "\x1D7DB")
           , ("4", "\x1D7DC")
           , ("5", "\x1D7DD")
           , ("6", "\x1D7DE")
           , ("7", "\x1D7DF")
           , ("8", "\x1D7E0")
           , ("9", "\x1D7E1")
           ]
