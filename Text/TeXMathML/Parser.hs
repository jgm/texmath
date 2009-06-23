module Text.TeXMathML.Parser (expr, formula, Exp(..), TeXSymbol(..))
where

import Control.Monad
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
  | EVariable String
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
   , opLetter       = oneOf "_+/=^-"
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

variable = liftM EVariable identifier

supersubscripted = try $ do
  a <- expr1
  c <- oneOf "^_"
  b <- expr
  return $ EBinary [c] a b

escaped = try $ char '\\' >> liftM (ESymbol . Ord . (:[])) anyChar

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
           , ("alpha",      Ord "\x03B1")
           , ("beta",       Ord "\x03B2")
           , ("chi",        Ord "\x03C7")
           , ("delta",      Ord "\x03B4")
           , ("Delta",      Op "\x0394")
           , ("epsi",       Ord "\x03B5")
           , ("varepsilon", Ord "\x025B")
           , ("eta",        Ord "\x03B7")
           , ("gamma",      Ord "\x03B3")
           , ("Gamma",      Op "\x0393") 
           , ("iota",       Ord "\x03B9")
           , ("kappa",      Ord "\x03BA")
           , ("lambda",     Ord "\x03BB")
           , ("Lambda",     Op "\x039B") 
           , ("mu",         Ord "\x03BC")
           , ("nu",         Ord "\x03BD")
           , ("omega",      Ord "\x03C9")
           , ("Omega",      Op "\x03A9")
           , ("phi",        Ord "\x03C6")
           , ("varphi",     Ord "\x03D5")
           , ("Phi",        Op "\x03A6") 
           , ("pi",         Ord "\x03C0")
           , ("Pi",         Op "\x03A0") 
           , ("psi",        Ord "\x03C8")
           , ("Psi",        Ord "\x03A8")
           , ("rho",        Ord "\x03C1")
           , ("sigma",      Ord "\x03C3")
           , ("Sigma",      Op "\x03A3") 
           , ("tau",        Ord "\x03C4")
           , ("theta",      Ord "\x03B8")
           , ("vartheta",   Ord "\x03D1")
           , ("Theta",      Op "\x0398") 
           , ("upsilon",    Ord "\x03C5")
           , ("xi",         Ord "\x03BE")
           , ("Xi",         Op "\x039E") 
           , ("zeta",       Ord "\x03B6")
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

