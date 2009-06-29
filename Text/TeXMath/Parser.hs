module Text.TeXMath.Parser (expr, formula, Exp(..), TeXSymbol(..))
where

import Control.Monad
import Data.Char (isAlphaNum)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

data TeXSymbol =
    Ord   String
  | Op    String
  | Bin   String
  | Rel   String
  | Open  String
  | Close String
  | Pun String
  deriving (Show, Read, Eq)

data Exp =
    EInteger Integer
  | EFloat   Double 
  | EGrouped [Exp]
  | EIdentifier String
  | ESymbol TeXSymbol
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
escaped = try $ char '\\' >> liftM (ESymbol . Ord . (:[])) (satisfy $ not . isAlphaNum)

function :: GenParser Char st Exp
function = liftM EIdentifier command

command :: GenParser Char st String
command = try $ char '\\' >> liftM ('\\':) identifier

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
  return $ EBinary "root" b a

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

symbols :: M.Map [Char] TeXSymbol
symbols = M.fromList [
             ("+", Bin "+")
           , ("-", Bin "-")
           , ("(", Open "(")
           , (")", Close ")")
           , ("\\[", Open "[")
           , ("\\]", Close "]")
           , ("\\{", Open "{")
           , ("\\}", Close "}")
           , (",", Pun ",")
           , (".", Pun ".")
           , (";", Pun ";")
           , (":", Pun ":")
           , ("?", Pun "?")
           , ("'", Ord "\x02B9")
           , ("''", Ord "\x02BA")
           , ("'''", Ord "\x2034")
           , ("''''", Ord "\x2057")
           , ("\\times", Bin "\x00D7")
           , ("\\alpha", Ord "\x03B1")
           , ("\\beta", Ord "\x03B2")
           , ("\\chi", Ord "\x03C7")
           , ("\\delta", Ord "\x03B4")
           , ("\\Delta", Op "\x0394")
           , ("\\epsi", Ord "\x03B5")
           , ("\\varepsilon", Ord "\x025B")
           , ("\\eta", Ord "\x03B7")
           , ("\\gamma", Ord "\x03B3")
           , ("\\Gamma", Op "\x0393") 
           , ("\\iota", Ord "\x03B9")
           , ("\\kappa", Ord "\x03BA")
           , ("\\lambda", Ord "\x03BB")
           , ("\\Lambda", Op "\x039B") 
           , ("\\mu", Ord "\x03BC")
           , ("\\nu", Ord "\x03BD")
           , ("\\omega", Ord "\x03C9")
           , ("\\Omega", Op "\x03A9")
           , ("\\phi", Ord "\x03C6")
           , ("\\varphi", Ord "\x03D5")
           , ("\\Phi", Op "\x03A6") 
           , ("\\pi", Ord "\x03C0")
           , ("\\Pi", Op "\x03A0") 
           , ("\\psi", Ord "\x03C8")
           , ("\\Psi", Ord "\x03A8")
           , ("\\rho", Ord "\x03C1")
           , ("\\sigma", Ord "\x03C3")
           , ("\\Sigma", Op "\x03A3") 
           , ("\\tau", Ord "\x03C4")
           , ("\\theta", Ord "\x03B8")
           , ("\\vartheta", Ord "\x03D1")
           , ("\\Theta", Op "\x0398") 
           , ("\\upsilon", Ord "\x03C5")
           , ("\\xi", Ord "\x03BE")
           , ("\\Xi", Op "\x039E") 
           , ("\\zeta", Ord "\x03B6")
           , ("\\frac12", Ord "\x00BD")
           , ("\\frac14", Ord "\x00BC")
           , ("\\frac34", Ord "\x00BE")
           , ("\\frac13", Ord "\x2153")
           , ("\\frac23", Ord "\x2154")
           , ("\\frac15", Ord "\x2155")
           , ("\\frac25", Ord "\x2156")
           , ("\\frac35", Ord "\x2157")
           , ("\\frac45", Ord "\x2158")
           , ("\\frac16", Ord "\x2159")
           , ("\\frac56", Ord "\x215A")
           , ("\\frac18", Ord "\x215B")
           , ("\\frac38", Ord "\x215C")
           , ("\\frac58", Ord "\x215D")
           , ("\\frac78", Ord "\x215E")
           , ("\\pm", Bin "\x00B1")
           , ("\\mp", Bin "\x2213")
           , ("\\triangleleft", Bin "\x22B2")
           , ("\\triangleright", Bin "\x22B3")
           , ("\\cdot", Bin "\x22C5")
           , ("\\star", Bin "\x22C6")
           , ("\\ast", Bin "\x002A")
           , ("\\times", Bin "\x00D7")
           , ("\\div", Bin "\x00F7")
           , ("\\circ", Bin "\x2218")
           , ("\\bullet", Bin "\x2022")
           , ("\\oplus", Bin "\x2295")
           , ("\\ominus", Bin "\x2296")
           , ("\\otimes", Bin "\x2297")
           , ("\\bigcirc", Bin "\x25CB")
           , ("\\oslash", Bin "\x2298")
           , ("\\odot", Bin "\x2299")
           , ("\\land", Bin "\x2227")
           , ("\\wedge", Bin "\x2227")
           , ("\\lor", Bin "\x2228")
           , ("\\vee", Bin "\x2228")
           , ("\\cap", Bin "\x2229")
           , ("\\cup", Bin "\x222A")
           , ("\\sqcap", Bin "\x2293")
           , ("\\sqcup", Bin "\x2294")
           , ("\\uplus", Bin "\x228E")
           , ("\\amalg", Bin "\x2210")
           , ("\\bigtriangleup", Bin "\x25B3")
           , ("\\bigtriangledown",Bin "\x25BD")
           , ("\\dag", Bin "\x2020")
           , ("\\dagger", Bin "\x2020")
           , ("\\ddag", Bin "\x2021")
           , ("\\ddagger", Bin "\x2021")
           , ("\\lhd", Bin "\x22B2")
           , ("\\rhd", Bin "\x22B3")
           , ("\\unlhd", Bin "\x22B4")
           , ("\\unrhd", Bin "\x22B5")
           , ("=", Rel "=")
           , (":=", Rel ":=")
           , ("\\lt", Rel "<")
           , ("\\gt", Rel ">")
           , ("\\ne", Rel "\x2260")
           , ("\\neq", Rel "\x2260")
           , ("\\le", Rel "\x2264")
           , ("\\leq", Rel "\x2264")
           , ("\\leqslant", Rel "\x2264")
           , ("\\ge", Rel "\x2265")
           , ("\\geq", Rel "\x2265")
           , ("\\geqslant", Rel "\x2265")
           , ("\\equiv", Rel "\x2261")
           , ("\\ll", Rel "\x226A")
           , ("\\gg", Rel "\x226B")
           , ("\\doteq", Rel "\x2250")
           , ("\\prec", Rel "\x227A")
           , ("\\succ", Rel "\x227B")
           , ("\\preceq", Rel "\x227C")
           , ("\\succeq", Rel "\x227D")
           , ("\\subset", Rel "\x2282")
           , ("\\supset", Rel "\x2283")
           , ("\\subseteq", Rel "\x2286")
           , ("\\supseteq", Rel "\x2287")
           , ("\\sqsubset", Rel "\x228F")
           , ("\\sqsupset", Rel "\x2290")
           , ("\\sqsubseteq", Rel "\x2291")
           , ("\\sqsupseteq", Rel "\x2292")
           , ("\\sim", Rel "\x223C")
           , ("\\simeq", Rel "\x2243")
           , ("\\approx", Rel "\x2248")
           , ("\\cong", Rel "\x2245")
           , ("\\Join", Rel "\x22C8")
           , ("\\bowtie", Rel "\x22C8")
           , ("\\in", Rel "\x2208")
           , ("\\ni", Rel "\x220B")
           , ("\\owns", Rel "\x220B")
           , ("\\propto", Rel "\x221D")
           , ("\\vdash", Rel "\x22A2")
           , ("\\dashv", Rel "\x22A3")
           , ("\\models", Rel "\x22A8")
           , ("\\perp", Rel "\x22A5")
           , ("\\smile", Rel "\x2323")
           , ("\\frown", Rel "\x2322")
           , ("\\asymp", Rel "\x224D")
           , ("\\notin", Rel "\x2209")
           , ("\\gets", Rel "\x2190")
           , ("\\leftarrow", Rel "\x2190")
           , ("\\to", Rel "\x2192")
           , ("\\rightarrow", Rel "\x2192")
           , ("\\leftrightarrow", Rel "\x2194")
           , ("\\uparrow", Rel "\x2191")
           , ("\\downarrow", Rel "\x2193")
           , ("\\updownarrow", Rel "\x2195")
           , ("\\Leftarrow", Rel "\x21D0")
           , ("\\Rightarrow", Rel "\x21D2")
           , ("\\Leftrightarrow", Rel "\x21D4")
           , ("\\iff", Rel "\x21D4")
           , ("\\Uparrow", Rel "\x21D1")
           , ("\\Downarrow", Rel "\x21D3")
           , ("\\Updownarrow", Rel "\x21D5")
           , ("\\mapsto", Rel "\x21A6")
           , ("\\longleftarrow", Rel "\x2190")
           , ("\\longrightarrow", Rel "\x2192")
           , ("\\longleftrightarrow", Rel "\x2194")
           , ("\\Longleftarrow", Rel "\x21D0")
           , ("\\Longrightarrow", Rel "\x21D2")
           , ("\\Longleftrightarrow", Rel "\x21D4")
           , ("\\longmapsto", Rel "\x21A6")
           , ("\\sum", Op "\x2211")
           , ("\\prod", Op "\x220F")
           , ("\\bigcap", Op "\x22C2")
           , ("\\bigcup", Op "\x22C3")
           , ("\\bigwedge", Op "\x22C0")
           , ("\\bigvee", Op "\x22C1")
           , ("\\bigsqcap", Op "\x2A05")
           , ("\\bigsqcup", Op "\x2A06")
           , ("\\coprod", Op "\x2210")
           , ("\\bigoplus", Op "\x2A01")
           , ("\\bigotimes", Op "\x2A02")
           , ("\\bigodot", Op "\x2A00")
           , ("\\biguplus", Op "\x2A04")
           , ("\\int", Op "\x222B")
           , ("\\oint", Op "\x222E")
           , ("\\prime", Ord "\x2032")
           , ("\\ldots", Ord "\x2026")
           , ("\\cdots", Ord "\x22EF")
           , ("\\vdots", Ord "\x22EE")
           , ("\\ddots", Ord "\x22F1")
           , ("\\forall", Op "\x2200")
           , ("\\exists", Op "\x2203")
           , ("\\Re", Ord "\x211C")
           , ("\\Im", Ord "\x2111")
           , ("\\aleph", Ord "\x2135")
           , ("\\hbar", Ord "\x210F")
           , ("\\ell", Ord "\x2113")
           , ("\\wp", Ord "\x2118")
           , ("\\emptyset", Ord "\x2205")
           , ("\\infty", Ord "\x221E")
           , ("\\partial", Ord "\x2202")
           , ("\\nabla", Ord "\x2207")
           , ("\\triangle", Ord "\x25B3")
           , ("\\therefore", Pun "\x2234")
           , ("\\angle", Ord "\x2220")
           , ("\\diamond", Op "\x22C4")
           , ("\\Diamond", Op "\x25C7")
           , ("\\neg", Op "\x00AC")
           , ("\\lnot", Ord "\x00AC")
           , ("\\bot", Ord "\x22A5")
           , ("\\top", Ord "\x22A4")
           , ("\\square", Ord "\x25AB")
           , ("\\Box", Op "\x25A1")
           , ("\\wr", Ord "\x2240")
           ] 

texSymbol :: GenParser Char st Exp
texSymbol = try $ do
  sym <- operator <|> command
  c <- case M.lookup sym symbols of
            Just s   -> return s
            Nothing  -> fail $ "Unknown symbol: " ++ sym
  return $ ESymbol c

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

