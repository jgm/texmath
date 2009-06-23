module Text.TeXMathML.Parser (expr, formula, Exp(..), TeXSymbol(..))
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
  | EBinary String Exp Exp
  | EUnary String Exp
  deriving (Show, Read, Eq)

texMathDef = LanguageDef 
   { commentStart   = ""
   , commentEnd     = ""
   , commentLine    = "%"
   , nestedComments = False
   , identStart     = letter
   , identLetter    = alphaNum <|> oneOf "'"
   , opStart        = opLetter texMathDef
   , opLetter       = oneOf ":_+/=^-"
   , reservedOpNames= []
   , reservedNames  = []
   , caseSensitive  = True
   }

-- The parser

expr1 =  choice [
    inbraces
  , variable
  , number
  , unary
  , binary
  , texSymbol
  , escaped
  ]

formula = do
  f <- many $ do e <- expr
                 whiteSpace
                 return e
  eof
  return f

expr = supersubscripted <|> expr1

inbraces = liftM EGrouped (braces $ many expr)

number = try (liftM EFloat float)
      <|> liftM EInteger decimal 

variable = liftM EIdentifier identifier

supersubscripted = try $ do
  a <- expr1
  c <- oneOf "^_"
  b <- expr
  return $ EBinary [c] a b

escaped = try $ char '\\' >> liftM (ESymbol . Ord . (:[])) (satisfy $ not . isAlphaNum)

command = try $ char '\\' >> identifier

unaryOps = ["sqrt"] 

unary = try $ do
  c <- command
  unless (c `elem` unaryOps) $
    fail $ "Unknown unary op: " ++ c
  a <- inbraces
  return $ EUnary c a

binaryOps = ["frac", "root", "stackrel"] 

binary = try $ do
  c <- command
  unless (c `elem` binaryOps) $
    fail $ "Unknown binary op: " ++ c
  a <- inbraces
  b <- inbraces
  return $ EBinary c a b

symbols = M.fromList [
             ("+", Bin "+")
           , ("-", Bin "-")
           , ("times", Bin "\x00D7")
           , ("alpha", Ord "\x03B1")
           , ("beta", Ord "\x03B2")
           , ("chi", Ord "\x03C7")
           , ("delta", Ord "\x03B4")
           , ("Delta", Op "\x0394")
           , ("epsi", Ord "\x03B5")
           , ("varepsilon", Ord "\x025B")
           , ("eta", Ord "\x03B7")
           , ("gamma", Ord "\x03B3")
           , ("Gamma", Op "\x0393") 
           , ("iota", Ord "\x03B9")
           , ("kappa", Ord "\x03BA")
           , ("lambda", Ord "\x03BB")
           , ("Lambda", Op "\x039B") 
           , ("mu", Ord "\x03BC")
           , ("nu", Ord "\x03BD")
           , ("omega", Ord "\x03C9")
           , ("Omega", Op "\x03A9")
           , ("phi", Ord "\x03C6")
           , ("varphi", Ord "\x03D5")
           , ("Phi", Op "\x03A6") 
           , ("pi", Ord "\x03C0")
           , ("Pi", Op "\x03A0") 
           , ("psi", Ord "\x03C8")
           , ("Psi", Ord "\x03A8")
           , ("rho", Ord "\x03C1")
           , ("sigma", Ord "\x03C3")
           , ("Sigma", Op "\x03A3") 
           , ("tau", Ord "\x03C4")
           , ("theta", Ord "\x03B8")
           , ("vartheta", Ord "\x03D1")
           , ("Theta", Op "\x0398") 
           , ("upsilon", Ord "\x03C5")
           , ("xi", Ord "\x03BE")
           , ("Xi", Op "\x039E") 
           , ("zeta", Ord "\x03B6")
           , ("frac12", Ord "\x00BD")
           , ("frac14", Ord "\x00BC")
           , ("frac34", Ord "\x00BE")
           , ("frac13", Ord "\x2153")
           , ("frac23", Ord "\x2154")
           , ("frac15", Ord "\x2155")
           , ("frac25", Ord "\x2156")
           , ("frac35", Ord "\x2157")
           , ("frac45", Ord "\x2158")
           , ("frac16", Ord "\x2159")
           , ("frac56", Ord "\x215A")
           , ("frac18", Ord "\x215B")
           , ("frac38", Ord "\x215C")
           , ("frac58", Ord "\x215D")
           , ("frac78", Ord "\x215E")
           , ("pm", Bin "\x00B1")
           , ("mp", Bin "\x2213")
           , ("triangleleft", Bin "\x22B2")
           , ("triangleright", Bin "\x22B3")
           , ("cdot", Bin "\x22C5")
           , ("star", Bin "\x22C6")
           , ("ast", Bin "\x002A")
           , ("times", Bin "\x00D7")
           , ("div", Bin "\x00F7")
           , ("circ", Bin "\x2218")
           , ("bullet", Bin "\x2022")
           , ("oplus", Bin "\x2295")
           , ("ominus", Bin "\x2296")
           , ("otimes", Bin "\x2297")
           , ("bigcirc", Bin "\x25CB")
           , ("oslash", Bin "\x2298")
           , ("odot", Bin "\x2299")
           , ("land", Bin "\x2227")
           , ("wedge", Bin "\x2227")
           , ("lor", Bin "\x2228")
           , ("vee", Bin "\x2228")
           , ("cap", Bin "\x2229")
           , ("cup", Bin "\x222A")
           , ("sqcap", Bin "\x2293")
           , ("sqcup", Bin "\x2294")
           , ("uplus", Bin "\x228E")
           , ("amalg", Bin "\x2210")
           , ("bigtriangleup", Bin "\x25B3")
           , ("bigtriangledown",Bin "\x25BD")
           , ("dag", Bin "\x2020")
           , ("dagger", Bin "\x2020")
           , ("ddag", Bin "\x2021")
           , ("ddagger", Bin "\x2021")
           , ("lhd", Bin "\x22B2")
           , ("rhd", Bin "\x22B3")
           , ("unlhd", Bin "\x22B4")
           , ("unrhd", Bin "\x22B5")
           , ("=", Rel "=")
           , (":=", Rel ":=")
           , ("lt", Rel "<")
           , ("gt", Rel ">")
           , ("ne", Rel "\x2260")
           , ("neq", Rel "\x2260")
           , ("le", Rel "\x2264")
           , ("leq", Rel "\x2264")
           , ("leqslant", Rel "\x2264")
           , ("ge", Rel "\x2265")
           , ("geq", Rel "\x2265")
           , ("geqslant", Rel "\x2265")
           , ("equiv", Rel "\x2261")
           , ("ll", Rel "\x226A")
           , ("gg", Rel "\x226B")
           , ("doteq", Rel "\x2250")
           , ("prec", Rel "\x227A")
           , ("succ", Rel "\x227B")
           , ("preceq", Rel "\x227C")
           , ("succeq", Rel "\x227D")
           , ("subset", Rel "\x2282")
           , ("supset", Rel "\x2283")
           , ("subseteq", Rel "\x2286")
           , ("supseteq", Rel "\x2287")
           , ("sqsubset", Rel "\x228F")
           , ("sqsupset", Rel "\x2290")
           , ("sqsubseteq", Rel "\x2291")
           , ("sqsupseteq", Rel "\x2292")
           , ("sim", Rel "\x223C")
           , ("simeq", Rel "\x2243")
           , ("approx", Rel "\x2248")
           , ("cong", Rel "\x2245")
           , ("Join", Rel "\x22C8")
           , ("bowtie", Rel "\x22C8")
           , ("in", Rel "\x2208")
           , ("ni", Rel "\x220B")
           , ("owns", Rel "\x220B")
           , ("propto", Rel "\x221D")
           , ("vdash", Rel "\x22A2")
           , ("dashv", Rel "\x22A3")
           , ("models", Rel "\x22A8")
           , ("perp", Rel "\x22A5")
           , ("smile", Rel "\x2323")
           , ("frown", Rel "\x2322")
           , ("asymp", Rel "\x224D")
           , ("notin", Rel "\x2209")
           , ("gets", Rel "\x2190")
           , ("leftarrow", Rel "\x2190")
           , ("to", Rel "\x2192")
           , ("rightarrow", Rel "\x2192")
           , ("leftrightarrow", Rel "\x2194")
           , ("uparrow", Rel "\x2191")
           , ("downarrow", Rel "\x2193")
           , ("updownarrow", Rel "\x2195")
           , ("Leftarrow", Rel "\x21D0")
           , ("Rightarrow", Rel "\x21D2")
           , ("Leftrightarrow", Rel "\x21D4")
           , ("iff", Rel "\x21D4")
           , ("Uparrow", Rel "\x21D1")
           , ("Downarrow", Rel "\x21D3")
           , ("Updownarrow", Rel "\x21D5")
           , ("mapsto", Rel "\x21A6")
           , ("longleftarrow", Rel "\x2190")
           , ("longrightarrow", Rel "\x2192")
           , ("longleftrightarrow", Rel "\x2194")
           , ("Longleftarrow", Rel "\x21D0")
           , ("Longrightarrow", Rel "\x21D2")
           , ("Longleftrightarrow", Rel "\x21D4")
           , ("longmapsto", Rel "\x21A6")
           ] 

texSymbol = try $ do
  sym <- operator <|> command
  c <- case M.lookup sym symbols of
            Just s   -> return s
            Nothing  -> fail $ "Unknown symbol: " ++ sym
  return $ ESymbol c

-- The lexer
lexer       = P.makeTokenParser texMathDef
    
lexeme         = P.lexeme lexer
whiteSpace     = P.whiteSpace lexer
identifier     = lexeme (P.identifier lexer)
operator       = lexeme (P.operator lexer)
decimal        = lexeme (P.decimal lexer)
float          = lexeme (P.float lexer)
symbol p       = lexeme (P.symbol lexer p)
braces p       = lexeme (P.braces lexer p)
angles p       = lexeme (P.angles lexer p)
brackets p     = lexeme (P.brackets lexer p)
squares p      = lexeme (P.squares lexer p)

