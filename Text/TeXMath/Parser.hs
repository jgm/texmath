module Text.TeXMath.Parser (expr, formula, Exp(..), TeXSymbolType(..))
where

import Control.Monad
import Data.Char (isAlphaNum)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

data TeXSymbolType = Ord | Op | Bin | Rel | Open | Close | Pun
                     deriving (Show, Read, Eq)

data Exp =
    EInteger Integer
  | EFloat   Double 
  | EGrouped [Exp]
  | EIdentifier String
  | ESymbol TeXSymbolType String
  | ESpace String
  | EBinary String Exp Exp
  | ESub Exp Exp
  | ESuper Exp Exp
  | ESubsup Exp Exp Exp
  | EUnary String Exp
  deriving (Show, Read, Eq)

texMathDef :: LanguageDef st
texMathDef = LanguageDef 
   { commentStart   = ""
   , commentEnd     = ""
   , commentLine    = "%"
   , nestedComments = False
   , identStart     = letter
   , identLetter    = alphaNum
   , opStart        = opLetter texMathDef
   , opLetter       = oneOf ":_+/=^-(),;.?'"
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
  , unary
  , root 
  , binary
  , texSymbol
  , escaped
  , function       -- will catch things like \sin, and also unknown commands 
  ]

formula :: GenParser Char st [Exp]
formula = do
  f <- many $ do e <- expr
                 whiteSpace
                 return e
  eof
  return f

expr :: GenParser Char st Exp
expr = subSup <|> superOrSubscripted <|> expr1

inbraces :: GenParser Char st Exp
inbraces = liftM EGrouped (braces $ many expr)

inbrackets :: GenParser Char st Exp
inbrackets = liftM EGrouped (brackets $ many expr)

number :: GenParser Char st Exp
number = try (liftM EFloat float)
      <|> liftM EInteger decimal 

variable :: GenParser Char st Exp
variable = liftM (EIdentifier . (:[])) letter

subSup :: GenParser Char st Exp
subSup = try $ do
  a <- expr1
  char '_'
  b <- expr1
  char '^'
  c <- expr
  return $ ESubsup a b c 

superOrSubscripted :: GenParser Char st Exp
superOrSubscripted = try $ do
  a <- expr1
  c <- oneOf "^_"
  b <- expr
  case c of
       '^' -> return $ ESuper a b
       '_' -> return $ ESub a b
       _   -> fail "expecting ^ or _"

escaped :: GenParser Char st Exp
escaped = try $ char '\\' >> liftM (ESymbol Ord . (:[])) (satisfy $ not . isAlphaNum)

function :: GenParser Char st Exp
function = liftM EIdentifier command

command :: GenParser Char st String
command = try $ char '\\' >> liftM ('\\':) (identifier <|> count 1 anyChar)

unaryOps :: [String]
unaryOps = ["\\sqrt"] 

unary :: GenParser Char st Exp
unary = try $ do
  c <- command
  unless (c `elem` unaryOps) $
    fail $ "Unknown unary op: " ++ c
  a <- inbraces
  return $ EUnary c a

-- note: sqrt can be unary, \sqrt{2}, or binary, \sqrt[3]{2}
root :: GenParser Char st Exp
root = try $ do
  symbol "\\sqrt"
  a <- inbrackets
  b <- inbraces
  return $ EBinary "\\sqrt" b a

binaryOps :: [String]
binaryOps = ["\\frac", "\\stackrel"] 

binary :: GenParser Char st Exp
binary = try $ do
  c <- command
  unless (c `elem` binaryOps) $
    fail $ "Unknown binary op: " ++ c
  a <- inbraces
  b <- inbraces
  return $ EBinary c a b

symbols :: M.Map [Char] Exp
symbols = M.fromList [
             ("+", ESymbol Bin "+")
           , ("-", ESymbol Bin "-")
           , ("(", ESymbol Open "(")
           , (")", ESymbol Close ")")
           , ("\\[", ESymbol Open "[")
           , ("\\]", ESymbol Close "]")
           , ("\\{", ESymbol Open "{")
           , ("\\}", ESymbol Close "}")
           , (", ESymbol", ESymbol Pun ", ESymbol")
           , (".", ESymbol Pun ".")
           , (";", ESymbol Pun ";")
           , (":", ESymbol Pun ":")
           , ("?", ESymbol Pun "?")
           , ("'", ESymbol Ord "\x02B9")
           , ("''", ESymbol Ord "\x02BA")
           , ("'''", ESymbol Ord "\x2034")
           , ("''''", ESymbol Ord "\x2057")
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
           , ("=", ESymbol Rel "=")
           , (":=", ESymbol Rel ":=")
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
           , ("\\oint", ESymbol Op "\x222E")
           , ("\\prime", ESymbol Ord "\x2032")
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
           ] 

texSymbol :: GenParser Char st Exp
texSymbol = try $ do
  sym <- operator <|> command
  case M.lookup sym symbols of
       Just s   -> return s
       Nothing  -> fail $ "Unknown symbol: " ++ sym

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
operator = lexeme (P.operator lexer)

decimal :: CharParser st Integer
decimal = lexeme (P.decimal lexer)

float :: CharParser st Double
float = lexeme (P.float lexer)

symbol :: String -> CharParser st String
symbol p = lexeme (P.symbol lexer p)

braces :: CharParser st a -> CharParser st a 
braces p = lexeme (P.braces lexer p)

brackets :: CharParser st a -> CharParser st a
brackets p = lexeme (P.brackets lexer p)

