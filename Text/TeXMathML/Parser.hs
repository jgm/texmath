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
  ]

formula = do
  f <- many expr
  eof
  return f

expr = infixOp <|> expr1

inbraces = liftM EGrouped (braces $ many expr)

number = try (liftM EFloat float)
      <|> liftM EInteger decimal 

variable = liftM EVariable identifier

infixOp = superscripted <|> subscripted

subscripted = try $ do
  a <- expr1
  char '_'
  b <- expr
  return $ EBinary "^" a b

superscripted = try $ do
  a <- expr1
  char '^'
  b <- expr
  return $ EBinary "_" a b

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
           , ("pi", Ord "π")
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

