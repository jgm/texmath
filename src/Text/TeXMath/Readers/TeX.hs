{-# LANGUAGE TupleSections #-}
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

module Text.TeXMath.Readers.TeX (readTeX)
where

import Data.List (intercalate)
import Control.Monad
import Data.Char (isDigit, isAscii, isLetter)
import qualified Data.Map as M
import Text.Parsec hiding (label)
import Text.Parsec.Error
import Text.Parsec.String
import Text.TeXMath.Types
import Control.Applicative ((<*), (*>), (<*>), (<$>), (<$), pure)
import qualified Text.TeXMath.Shared as S
import Text.TeXMath.Readers.TeX.Macros (applyMacros, parseMacroDefinitions)
import Text.TeXMath.Unicode.ToTeX (getSymbolType)
import Data.Maybe (fromMaybe, fromJust)

type TP = Parser

-- The parser

expr1 :: TP Exp
expr1 = choice
          [ inbraces
          , variable
          , number
          , text
          , styled
          , root
          , mspace
          , phantom
          , boxed
          , binary
          , genfrac
          , bareSubSup
          , environment
          , diacritical
          , unicode
          , ensuremath
          , enclosure
          , texSymbol
          ] <* ignorable

-- | Parse a formula, returning a list of 'Exp'.
readTeX :: String -> Either String [Exp]
readTeX inp =
  let (ms, rest) = parseMacroDefinitions inp in
  either (Left . showParseError inp) (Right . id)
    $ parse formula "formula" (applyMacros ms rest)

showParseError :: String -> ParseError -> String
showParseError inp pe =
  snippet ++ "\n" ++ caretline ++
    showErrorMessages "or" "unknown" "expecting" "unexpected" "eof"
       (errorMessages pe)
  where errln = sourceLine (errorPos pe)
        errcol = sourceColumn (errorPos pe)
        snipoffset = max 0 (errcol - 20)
        inplns = lines inp
        ln = if length inplns >= errln
                then inplns !! (errln - 1)
                else ""  -- should not happen
        snippet = take 40 $ drop snipoffset ln
        caretline = replicate (errcol - snipoffset - 1) ' ' ++ "^"


ctrlseq :: String -> TP String
ctrlseq s = lexeme $ try $ do
  result <- string ('\\':s)
  case s of
       [c] | not (isLetter c) -> return ()
       _ -> (do pos <- getPosition
                letter
                setPosition pos
                mzero <?> ("non-letter after \\" ++ s))
            <|> return ()
  return result

ignorable :: TP ()
ignorable = skipMany (comment <|> label <|> (skipMany1 space <?> "whitespace"))

comment :: TP ()
comment = char '%' *> skipMany (noneOf "\n") *> optional newline

label :: TP ()
label = ctrlseq "label" *> braces (skipMany (noneOf "}"))

unGrouped :: Exp -> [Exp]
unGrouped (EGrouped xs) = xs
unGrouped x = [x]

formula :: TP [Exp]
formula = unGrouped <$> (ignorable *> manyExp expr <* eof)

expr :: TP Exp
expr = do
  optional (ctrlseq "displaystyle")
  (a, convertible) <- try (braces operatorname) -- needed because macros add {}
                 <|> ((,False) <$> expr1)
                 <|> operatorname
  limits <- limitsIndicator
  subSup limits convertible a <|> superOrSubscripted limits convertible a <|> return a

-- | Parser for \operatorname command.
-- Returns a tuple of EMathOperator name and Bool depending on the flavor
-- of the command:
--
--     - True for convertible operator (\operator*)
--
--     - False otherwise
operatorname :: TP (Exp, Bool)
operatorname = do
    ctrlseq "operatorname"
    convertible <- (char '*' >> spaces >> return True) <|> return False
    op <- expToOperatorName <$> texToken
    maybe mzero (\s -> return (EMathOperator s, convertible)) op

-- | Converts identifiers, symbols and numbers to a flat string.
-- Returns Nothing if the expression contains anything else.
expToOperatorName :: Exp -> Maybe String
expToOperatorName e = case e of
            EGrouped xs ->  concat <$> mapM fl xs
            _ -> fl e
    where fl f = case f of
                    EIdentifier s -> Just s
                    -- handle special characters
                    ESymbol _ "\x2212" -> Just "-"
                    ESymbol _ "\x2032" -> Just "'"
                    ESymbol _ "\x2033" -> Just "''"
                    ESymbol _ "\x2034" -> Just "'''"
                    ESymbol _ "\x2057" -> Just "''''"
                    ESymbol _ "\x02B9" -> Just "'"
                    ESymbol _ s -> Just s
                    ENumber s -> Just s
                    _ -> Nothing

bareSubSup :: TP Exp
bareSubSup = subSup Nothing False (EIdentifier "")
  <|> superOrSubscripted Nothing False (EIdentifier "")

limitsIndicator :: TP (Maybe Bool)
limitsIndicator =
   (ctrlseq "limits" >> return (Just True))
  <|> (ctrlseq "nolimits" >> return (Just False))
  <|> return Nothing

binomCmd :: TP String
binomCmd = oneOfCommands (map fst binomCmds)

binomCmds :: [(String, Exp -> Exp -> Exp)]
binomCmds = [ ("\\choose", \x y ->
                EDelimited "(" ")" [Right (EFraction NoLineFrac x y)])
            , ("\\brack", \x y ->
                EDelimited "[" "]" [Right (EFraction NoLineFrac x y)])
            , ("\\brace", \x y ->
                EDelimited "{" "}" [Right (EFraction NoLineFrac x y)])
            , ("\\bangle", \x y ->
                EDelimited "\x27E8" "\x27E9" [Right (EFraction NoLineFrac x y)])
            ]

genfrac :: TP Exp
genfrac = do
  ctrlseq "genfrac"
  openDelim <- braces $ option '(' ((char '\\' >> oneOf "{}") <|> anyChar)
  closeDelim <- braces $ option ')' ((char '\\' >> oneOf "{}") <|> anyChar)
  bar <- False <$ try (braces (string "0pt")) <|> True <$ texToken
  displayStyle <- True <$ try (braces (char '0')) <|> False <$ texToken
  x <- texToken
  y <- texToken
  let fracType = case (bar, displayStyle) of
                      (False, _)   -> NoLineFrac
                      (True, True) -> DisplayFrac
                      _            -> NormalFrac
  return $ EDelimited [openDelim] [closeDelim] [Right (EFraction fracType x y)]

asGroup :: [Exp] -> Exp
asGroup [x] = x
asGroup xs = EGrouped xs

-- variant of many that is sensitive to \choose and other such commands
manyExp' :: Bool -> TP Exp -> TP Exp
manyExp' requireNonempty p = do
  initial <- if requireNonempty
                then many1 (notFollowedBy binomCmd >> p)
                else many (notFollowedBy binomCmd >> p)
  let withCmd :: String -> TP Exp
      withCmd cmd =
         case lookup cmd binomCmds of
              Just f  -> f <$> (asGroup <$> pure initial)
                           <*> (asGroup <$> many p)
              Nothing -> fail $ "Unknown command " ++ cmd
  (binomCmd >>= withCmd) <|> return (asGroup initial)

manyExp :: TP Exp -> TP Exp
manyExp = manyExp' False

many1Exp :: TP Exp -> TP Exp
many1Exp = manyExp' True

inbraces :: TP Exp
inbraces = braces (manyExp expr)

texToken :: TP Exp
texToken = texSymbol <|> inbraces <|> inbrackets <|> texChar

texChar :: TP Exp
texChar =
  do
    c <- noneOf "\n\t\r \\{}" <* spaces
    return $ if isDigit c
              then ENumber [c]
              else EIdentifier [c]

inbrackets :: TP Exp
inbrackets = (brackets $ manyExp $ notFollowedBy (char ']') >> expr)

number :: TP Exp
number = lexeme $ ENumber <$> many1 digit

enclosure :: TP Exp
enclosure = basicEnclosure <|> scaledEnclosure <|> delimited

-- Expensive
basicEnclosure :: TP Exp
basicEnclosure = choice (map (\(s, v) -> symbol s >> return v) enclosures)

fence :: String -> TP String
fence cmd = do
  symbol cmd
  enc <- basicEnclosure <|> (try (symbol ".") >> return (ESymbol Open ""))
  case enc of
       ESymbol Open x  -> return x
       ESymbol Close x -> return x
       _ -> mzero

middle :: TP String
middle = fence "\\middle"

right :: TP String
right = fence "\\right"

delimited :: TP Exp
delimited = do
  openc <- try $ fence "\\left"
  contents <- concat <$>
              many (try $ ((:[]) . Left <$> middle)
                      <|> (map Right . unGrouped <$>
                             many1Exp (notFollowedBy right *> expr)))
  closec <- right <|> return ""
  return $ EDelimited openc closec contents

scaledEnclosure :: TP Exp
scaledEnclosure = do
  cmd <- oneOfCommands (map fst S.scalers)
  case S.getScalerValue cmd of
       Just  r -> EScaled r <$> basicEnclosure
       Nothing -> mzero

endLine :: TP Char
endLine = try $ do
  symbol "\\\\"
  optional inbrackets  -- can contain e.g. [1.0in] for a line height, not yet supported
  optional $ ctrlseq "hline"
  -- we don't represent the line, but it shouldn't crash parsing
  return '\n'

arrayLine :: TP ArrayLine
arrayLine = notFollowedBy (ctrlseq "end" >> return '\n') >>
  sepBy1 (unGrouped <$> manyExp (notFollowedBy endLine >> expr)) (symbol "&")

arrayAlignments :: TP [Alignment]
arrayAlignments = try $ do
  as <- braces (many letter)
  let letterToAlignment 'l' = AlignLeft
      letterToAlignment 'c' = AlignCenter
      letterToAlignment 'r' = AlignRight
      letterToAlignment _   = AlignDefault
  return $ map letterToAlignment as

environment :: TP Exp
environment = do
  ctrlseq "begin"
  name <- braces (oneOfStrings (M.keys environments) <* optional (char '*'))
  spaces
  case M.lookup name environments of
        Just env -> do
          result <- env
          spaces
          ctrlseq "end"
          braces (string name <* optional (char '*'))
          spaces
          return result
        Nothing  -> mzero  -- should not happen

environments :: M.Map String (TP Exp)
environments = M.fromList
  [ ("array", stdarray)
  , ("eqnarray", eqnarray)
  , ("align", align)
  , ("aligned", align)
  , ("alignat", inbraces *> spaces *> align)
  , ("alignedat", inbraces *> spaces *> align)
  , ("flalign", flalign)
  , ("flaligned", flalign)
  , ("cases", cases)
  , ("matrix", matrixWith "" "")
  , ("smallmatrix", matrixWith "" "")
  , ("pmatrix", matrixWith "(" ")")
  , ("bmatrix", matrixWith "[" "]")
  , ("Bmatrix", matrixWith "{" "}")
  , ("vmatrix", matrixWith "\x2223" "\x2223")
  , ("Vmatrix", matrixWith "\x2225" "\x2225")
  , ("split", align)
  , ("multline", gather)
  , ("gather", gather)
  , ("gathered", gather)
  ]

mbArrayAlignments :: TP (Maybe [Alignment])
mbArrayAlignments = option Nothing $ Just <$> arrayAlignments

alignsFromRows :: Alignment -> [ArrayLine] -> [Alignment]
alignsFromRows _ [] = []
alignsFromRows defaultAlignment (r:_) = replicate (length r) defaultAlignment

matrixWith :: String -> String -> TP Exp
matrixWith opendelim closedelim = do
  mbaligns <- mbArrayAlignments
  lines' <- sepEndBy1 arrayLine endLine
  let aligns = fromMaybe (alignsFromRows AlignCenter lines') mbaligns
  return $ if null opendelim && null closedelim
              then EArray aligns lines'
              else EDelimited opendelim closedelim [Right $ EArray aligns lines']

stdarray :: TP Exp
stdarray = do
  mbaligns <- mbArrayAlignments
  lines' <- sepEndBy1 arrayLine endLine
  let aligns = fromMaybe (alignsFromRows AlignDefault lines') mbaligns
  return $ EArray aligns lines'

gather :: TP Exp
gather = do
  rows <- sepEndBy arrayLine endLine
  return $ EArray (alignsFromRows AlignCenter rows) rows

eqnarray :: TP Exp
eqnarray = (EArray [AlignRight, AlignCenter, AlignLeft]) <$>
  sepEndBy1 arrayLine endLine

align :: TP Exp
align = (EArray [AlignRight, AlignLeft]) <$> sepEndBy1 arrayLine endLine

flalign :: TP Exp
flalign = (EArray [AlignLeft, AlignRight]) <$> sepEndBy1 arrayLine endLine

cases :: TP Exp
cases = do
  rs <- sepEndBy1 arrayLine endLine
  return $ EDelimited "{" "" [Right $ EArray (alignsFromRows AlignDefault rs) rs]

variable :: TP Exp
variable = do
  v <- letter
  spaces
  return $ EIdentifier [v]

isConvertible :: Exp -> Bool
isConvertible (EMathOperator x) = x `elem` convertibleOps
  where convertibleOps = [ "lim","liminf","limsup","inf","sup"
                         , "min","max","Pr","det","gcd"
                         ]
isConvertible (ESymbol Rel _) = True
isConvertible (ESymbol Bin _) = True
isConvertible (ESymbol Op x) = x `elem` convertibleSyms
  where convertibleSyms = ["\x2211","\x220F","\x22C2",
           "\x22C3","\x22C0","\x22C1","\x2A05","\x2A06",
           "\x2210","\x2A01","\x2A02","\x2A00","\x2A04"]
isConvertible _ = False

-- check if sub/superscripts should always be under and over the expression
isUnderover :: Exp -> Bool
isUnderover (EOver _ _ (ESymbol Accent "\xFE37")) = True   -- \overbrace
isUnderover (EOver _ _ (ESymbol Accent "\x23B4")) = True   -- \overbracket
isUnderover (EUnder _ _ (ESymbol Accent "\xFE38")) = True  -- \underbrace
isUnderover (EUnder _ _ (ESymbol Accent "\x23B5")) = True  -- \underbracket
isUnderover (EOver _  _ (ESymbol Accent "\x23DE")) = True  -- \overbrace
isUnderover (EUnder _  _ (ESymbol Accent "\x23DF")) = True  -- \underbrace
isUnderover _ = False

subSup :: Maybe Bool -> Bool -> Exp -> TP Exp
subSup limits convertible a = try $ do
  let sub1 = symbol "_" >> expr1
  let sup1 = symbol "^" >> expr1
  (b,c) <- try (do {m <- sub1; n <- sup1; return (m,n)})
       <|> (do {n <- sup1; m <- sub1; return (m,n)})
  return $ case limits of
            Just True  -> EUnderover False a b c
            Nothing | convertible || isConvertible a -> EUnderover True a b c
                    | isUnderover a -> EUnderover False a b c
            _          -> ESubsup a b c

superOrSubscripted :: Maybe Bool -> Bool -> Exp -> TP Exp
superOrSubscripted limits convertible a = try $ do
  c <- oneOf "^_"
  spaces
  b <- expr
  case c of
       '^' -> return $ case limits of
                        Just True  -> EOver False a b
                        Nothing
                          | convertible || isConvertible a -> EOver True a b
                          | isUnderover a -> EOver False a b
                        _          -> ESuper a b
       '_' -> return $ case limits of
                        Just True  -> EUnder False a b
                        Nothing
                          | convertible || isConvertible a -> EUnder True a b
                          | isUnderover a -> EUnder False a b
                        _          -> ESub a b
       _   -> mzero

unicode :: TP Exp
unicode = lexeme $
  do
    c <- satisfy (not . isAscii)
    return (ESymbol (getSymbolType c) [c])

ensuremath :: TP Exp
ensuremath = ctrlseq "ensuremath" *> inbraces

-- Note: cal and scr are treated the same way, as unicode is lacking such two different sets for those.
styleOps :: M.Map String ([Exp] -> Exp)
styleOps = M.fromList
          [ ("\\mathrm",     EStyled TextNormal)
          , ("\\mathup",     EStyled TextNormal)
          , ("\\mbox",       EStyled TextNormal)
          , ("\\mathbf",     EStyled TextBold)
          , ("\\mathbfup",   EStyled TextBold)
          , ("\\mathit",     EStyled TextItalic)
          , ("\\mathtt",     EStyled TextMonospace)
          , ("\\texttt",     EStyled TextMonospace)
          , ("\\mathsf",     EStyled TextSansSerif)
          , ("\\mathsfup",   EStyled TextSansSerif)
          , ("\\mathbb",     EStyled TextDoubleStruck)
          , ("\\mathcal",    EStyled TextScript)
          , ("\\mathscr",    EStyled TextScript)
          , ("\\mathfrak",   EStyled TextFraktur)
          , ("\\mathbfit",   EStyled TextBoldItalic)
          , ("\\mathbfsfup", EStyled TextSansSerifBold)
          , ("\\mathbfsfit", EStyled TextSansSerifBoldItalic)
          , ("\\mathbfscr",  EStyled TextBoldScript)
          , ("\\mathbffrak", EStyled TextBoldFraktur)
          , ("\\mathbfcal",  EStyled TextBoldScript)
          , ("\\mathsfit",   EStyled TextSansSerifItalic)
          ]

diacritical :: TP Exp
diacritical = do
  c <- oneOfCommands (map snd S.diacriticals)
  case S.getDiacriticalCons c of
       Just r  -> r <$> texToken
       Nothing -> mzero


phantom :: TP Exp
phantom = EPhantom <$> (ctrlseq "phantom" *> texToken)

boxed :: TP Exp
boxed = EBoxed <$> (ctrlseq "boxed" *> texToken)

text :: TP Exp
text = do
  c <- oneOfCommands (M.keys textOps)
  maybe mzero (<$> (bracedText <* spaces)) $ M.lookup c textOps

textOps :: M.Map String (String -> Exp)
textOps = M.fromList
          [ ("\\textrm", (EText TextNormal))
          , ("\\text",   (EText TextNormal))
          , ("\\textbf", (EText TextBold))
          , ("\\textit", (EText TextItalic))
          , ("\\texttt", (EText TextMonospace))
          , ("\\textsf", (EText TextSansSerif))
          ]

styled :: TP Exp
styled = do
  c <- oneOfCommands (M.keys styleOps)
  case M.lookup c styleOps of
       Just f   -> do
         x <- inbraces
         return $ case x of
                       EGrouped xs -> f xs
                       _           -> f [x]
       Nothing  -> mzero

-- note: sqrt can be unary, \sqrt{2}, or binary, \sqrt[3]{2}
root :: TP Exp
root = do
  ctrlseq "sqrt" <|> ctrlseq "surd"
  (ERoot <$> inbrackets <*> texToken) <|> (ESqrt <$> texToken)

mspace :: TP Exp
mspace = do
  ctrlseq "mspace"
  lexeme $ char '{'
  len <- many1 digit
  lexeme $ string "mu"
  lexeme $ char '}'
  case reads len of
       ((n,[]):_) -> return $ ESpace (n/18)
       _          -> mzero


binary :: TP Exp
binary = do
  c <- oneOfCommands binops
  a <- texToken
  b <- texToken
  case c of
     "\\overset"  -> return $ EOver False b a
     "\\stackrel" -> return $ EOver False b a
     "\\underset" -> return $ EUnder False b a
     "\\frac"     -> return $ EFraction NormalFrac a b
     "\\tfrac"    -> return $ EFraction InlineFrac a b
     "\\dfrac"    -> return $ EFraction DisplayFrac a b
     "\\binom"    -> return $ EDelimited "(" ")"
                              [Right (EFraction NoLineFrac a b)]
     _            -> fail "Unrecognised binary operator"
  where
    binops = ["\\overset", "\\stackrel", "\\underset", "\\frac", "\\tfrac", "\\dfrac", "\\binom"]

texSymbol :: TP Exp
texSymbol = do
  negated <- (try (ctrlseq "not") >> return True) <|> return False
  sym <- operator <|> tSymbol
  if negated then neg sym else return sym

oneOfCommands :: [String] -> TP String
oneOfCommands cmds = try $ do
  cmd <- oneOfStrings cmds
  case cmd of
    ['\\',c] | not (isLetter c) -> return ()
    _ -> (do pos <- getPosition
             letter
             setPosition pos
             mzero <?> ("non-letter after " ++ cmd))
         <|> return ()
  spaces
  return cmd

oneOfStrings' :: (Char -> Char -> Bool) -> [String] -> TP String
oneOfStrings' _ []         = mzero
oneOfStrings' matches strs = try $ do
    c <- anyChar
    let strs' = [xs | (x:xs) <- strs, x `matches` c]
    case strs' of
      []  -> mzero
      _   -> (c:) <$> oneOfStrings' matches strs'
                     <|> if "" `elem` strs'
                          then return [c]
                          else mzero

-- | Parses one of a list of strings.  If the list contains
-- two strings one of which is a prefix of the other, the longer
-- string will be matched if possible.
oneOfStrings :: [String] -> TP String
oneOfStrings strs = oneOfStrings' (==) strs <??> (intercalate ", " $ map show strs)

-- | Like '(<?>)', but moves position back to the beginning of the parse
-- before reporting the error.
(<??>) :: Monad m => ParsecT s u m a -> String -> ParsecT s u m a
(<??>) p expected = do
  pos <- getPosition
  p <|> (setPosition pos >> mzero <?> expected)

infix 0 <??>

tSymbol :: TP Exp
tSymbol = do
  sym <- oneOfCommands (M.keys symbols)
  return $ fromJust (M.lookup sym symbols)

operator :: TP Exp
operator = do
  sym <- lexeme (oneOfStrings $ M.keys operators)
  return $ fromJust (M.lookup sym operators)

neg :: Exp -> TP Exp
neg (ESymbol Rel x) = ESymbol Rel `fmap`
  case x of
       "\x2282" -> return "\x2284"
       "\x2283" -> return "\x2285"
       "\x2286" -> return "\x2288"
       "\x2287" -> return "\x2289"
       "\x2208" -> return "\x2209"
       _        -> mzero
neg _ = mzero

lexeme :: TP a -> TP a
lexeme p = p <* spaces

braces :: TP a -> TP a
braces p = lexeme $ char '{' *> spaces *> p <* spaces <* char '}'

brackets :: TP a -> TP a
brackets p = lexeme $ char '[' *> spaces *> p <* spaces <* char ']'


symbol :: String -> TP String
symbol s = lexeme $ try $ string s

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
             , ("\\rrbracket", ESymbol Close "\x27E7")
             , ("\\langle", ESymbol Open "\x27E8")
             , ("\\rangle", ESymbol Close "\x27E9")
             , ("\\lfloor", ESymbol Open "\x230A")
             , ("\\rfloor", ESymbol Close "\x230B")
             , ("\\lceil", ESymbol Open "\x2308")
             , ("\\rceil", ESymbol Close "\x2309")
             , ("|", ESymbol Open "|")
             , ("|", ESymbol Close "|")
             , ("\\|", ESymbol Open "\x2225")
             , ("\\|", ESymbol Close "\x2225")
             , ("\\lvert", ESymbol Open "\x7C")
             , ("\\rvert", ESymbol Close "\x7C")
             , ("\\vert", ESymbol Close "\x7C")
             , ("\\lVert", ESymbol Open "\x2225")
             , ("\\rVert", ESymbol Close "\x2225")
             , ("\\Vert", ESymbol Close "\x2016")
             , ("\\ulcorner", ESymbol Open "\x231C")
             , ("\\urcorner", ESymbol Close "\x231D")
             ]

operators :: M.Map String Exp
operators = M.fromList [
             ("+", ESymbol Bin "+")
           , ("-", ESymbol Bin "\x2212")
           , ("*", ESymbol Bin "*")
           , ("@", ESymbol Ord "@")
           , (",", ESymbol Pun ",")
           , (".", ESymbol Ord ".")
           , (";", ESymbol Pun ";")
           , (":", ESymbol Rel ":")
           , ("?", ESymbol Ord "?")
           , (">", ESymbol Rel ">")
           , ("<", ESymbol Rel "<")
           , ("!", ESymbol Ord "!")
           , ("'", ESymbol Ord "\x2032")
           , ("''", ESymbol Ord "\x2033")
           , ("'''", ESymbol Ord "\x2034")
           , ("''''", ESymbol Ord "\x2057")
           , ("=", ESymbol Rel "=")
           , (":=", ESymbol Rel ":=")
           , ("/", ESymbol Ord "/")
           , ("~", ESpace (4/18)) ]

symbols :: M.Map String Exp
symbols = M.fromList
           [ ("\\$", ESymbol Ord "$")
           , ("\\%", ESymbol Ord "%")
           , ("\\&", ESymbol Ord "&")
           , ("\\_", ESymbol Ord "_")
           , ("\\#", ESymbol Ord "#")
           , ("\\^", ESymbol Ord "^")
           , ("\\mid", ESymbol Bin "\x2223")
           , ("\\colon", ESymbol Pun ":")
           , ("\\parallel", ESymbol Rel "\x2225")
           , ("\\backslash", ESymbol Bin "\x2216")
           , ("\\setminus", ESymbol Bin "\\")
           , ("\\times", ESymbol Bin "\x00D7")
           , ("\\alpha", EIdentifier "\x03B1")
           , ("\\beta", EIdentifier "\x03B2")
           , ("\\chi", EIdentifier "\x03C7")
           , ("\\delta", EIdentifier "\x03B4")
           , ("\\Delta", ESymbol Op "\x0394")
           , ("\\epsilon", EIdentifier "\x03F5")
           , ("\\varepsilon", EIdentifier "\x03B5")
           , ("\\eta", EIdentifier "\x03B7")
           , ("\\gamma", EIdentifier "\x03B3")
           , ("\\Gamma", ESymbol Op "\x0393")
           , ("\\iota", EIdentifier "\x03B9")
           , ("\\kappa", EIdentifier "\x03BA")
           , ("\\lambda", EIdentifier "\x03BB")
           , ("\\Lambda", ESymbol Op "\x039B")
           , ("\\mu", EIdentifier "\x03BC")
           , ("\\nu", EIdentifier "\x03BD")
           , ("\\omega", EIdentifier "\x03C9")
           , ("\\Omega", ESymbol Op "\x03A9")
           , ("\\phi", EIdentifier "\x03D5")
           , ("\\varphi", EIdentifier "\x03C6")
           , ("\\Phi", ESymbol Op "\x03A6")
           , ("\\pi", EIdentifier "\x03C0")
           , ("\\Pi", ESymbol Op "\x03A0")
           , ("\\psi", EIdentifier "\x03C8")
           , ("\\Psi", ESymbol Ord "\x03A8")
           , ("\\rho", EIdentifier "\x03C1")
           , ("\\sigma", EIdentifier "\x03C3")
           , ("\\Sigma", ESymbol Op "\x03A3")
           , ("\\tau", EIdentifier "\x03C4")
           , ("\\theta", EIdentifier "\x03B8")
           , ("\\vartheta", EIdentifier "\x03D1")
           , ("\\Theta", ESymbol Op "\x0398")
           , ("\\upsilon", EIdentifier "\x03C5")
           , ("\\Upsilon", EIdentifier "\x03A5")
           , ("\\xi", EIdentifier "\x03BE")
           , ("\\Xi", ESymbol Op "\x039E")
           , ("\\zeta", EIdentifier "\x03B6")
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
           , ("\\nsubset", ESymbol Rel "\x2284")
           , ("\\nsupset", ESymbol Rel "\x2285")
           , ("\\nsubseteq", ESymbol Rel "\x2288")
           , ("\\nsupseteq", ESymbol Rel "\x2289")
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
           , ("\\lozenge", ESymbol Op "\x25CA")
           , ("\\neg", ESymbol Op "\x00AC")
           , ("\\lnot", ESymbol Ord "\x00AC")
           , ("\\bot", ESymbol Ord "\x22A5")
           , ("\\top", ESymbol Ord "\x22A4")
           , ("\\square", ESymbol Ord "\x25AB")
           , ("\\Box", ESymbol Op "\x25A1")
           , ("\\wr", ESymbol Ord "\x2240")
           , ("\\!", ESpace (-3/18))
           , ("\\,", ESpace (3/18))
           , ("\\>", ESpace (4/18))
           , ("\\:", ESpace (4/18))
           , ("\\;", ESpace (5/18))
           , ("\\ ", ESpace (4/18))
           , ("\\quad", ESpace 1)
           , ("\\qquad", ESpace 2)
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

-- text mode parsing

textual :: TP String
textual = regular <|> sps <|> ligature <|> textCommand <|> bracedText
            <?> "text"

sps :: TP String
sps = " " <$ skipMany1 (oneOf " \t\n")

regular :: TP String
regular = many1 (noneOf "`'-~${}\\ \t")

ligature :: TP String
ligature = try ("\x2014" <$ string "---")
       <|> try ("\x2013" <$ string "--")
       <|> try ("\x201C" <$ string "``")
       <|> try ("\x201D" <$ string "''")
       <|> try ("\x2019" <$ string "'")
       <|> try ("\x2018" <$ string "`")
       <|> try ("\xA0"   <$ string "~")

textCommand :: TP String
textCommand = do
  cmd <- oneOfCommands (M.keys textCommands)
  case M.lookup cmd textCommands of
       Nothing -> fail ("Unknown control sequence " ++ cmd)
       Just c  -> c

bracedText :: TP String
bracedText = do
  char '{'
  inner <- concat <$> many textual
  char '}'
  return inner

tok :: TP Char
tok = (try $ char '{' *> spaces *> anyChar <* spaces <* char '}')
   <|> anyChar

textCommands :: M.Map String (TP String)
textCommands = M.fromList
  [ ("\\#", return "#")
  , ("\\$", return "$")
  , ("\\%", return "%")
  , ("\\&", return "&")
  , ("\\_", return "_")
  , ("\\{", return "{")
  , ("\\}", return "}")
  , ("\\ldots", return "\x2026")
  , ("\\textasciitilde", return "~")
  , ("\\textasciicircum", return "^")
  , ("\\textbackslash", return "\\")
  , ("\\char", parseC)
  , ("\\aa", return "å")
  , ("\\AA", return "Å")
  , ("\\ss", return "ß")
  , ("\\o", return "ø")
  , ("\\O", return "Ø")
  , ("\\L", return "Ł")
  , ("\\l", return "ł")
  , ("\\ae", return "æ")
  , ("\\AE", return "Æ")
  , ("\\oe", return "œ")
  , ("\\OE", return "Œ")
  , ("\\`", option "`" $ grave <$> tok)
  , ("\\'", option "'" $ acute <$> tok)
  , ("\\^", option "^" $ circ  <$> tok)
  , ("\\~", option "~" $ tilde <$> tok)
  , ("\\\"", option "\"" $ try $ umlaut <$> tok)
  , ("\\.", option "." $ try $ dot <$> tok)
  , ("\\=", option "=" $ try $ macron <$> tok)
  , ("\\c", option "c" $ try $ cedilla <$> tok)
  , ("\\v", option "v" $ try $ hacek <$> tok)
  , ("\\u", option "u" $ try $ breve <$> tok)
  , ("\\ ", return " ")
  ]

parseC :: TP String
parseC = try $ char '`' >> count 1 anyChar

-- the functions below taken from pandoc:

grave :: Char -> String
grave 'A' = "À"
grave 'E' = "È"
grave 'I' = "Ì"
grave 'O' = "Ò"
grave 'U' = "Ù"
grave 'a' = "à"
grave 'e' = "è"
grave 'i' = "ì"
grave 'o' = "ò"
grave 'u' = "ù"
grave c   = [c]

acute :: Char -> String
acute 'A' = "Á"
acute 'E' = "É"
acute 'I' = "Í"
acute 'O' = "Ó"
acute 'U' = "Ú"
acute 'Y' = "Ý"
acute 'a' = "á"
acute 'e' = "é"
acute 'i' = "í"
acute 'o' = "ó"
acute 'u' = "ú"
acute 'y' = "ý"
acute 'C' = "Ć"
acute 'c' = "ć"
acute 'L' = "Ĺ"
acute 'l' = "ĺ"
acute 'N' = "Ń"
acute 'n' = "ń"
acute 'R' = "Ŕ"
acute 'r' = "ŕ"
acute 'S' = "Ś"
acute 's' = "ś"
acute 'Z' = "Ź"
acute 'z' = "ź"
acute c   = [c]

circ :: Char -> String
circ 'A' = "Â"
circ 'E' = "Ê"
circ 'I' = "Î"
circ 'O' = "Ô"
circ 'U' = "Û"
circ 'a' = "â"
circ 'e' = "ê"
circ 'i' = "î"
circ 'o' = "ô"
circ 'u' = "û"
circ 'C' = "Ĉ"
circ 'c' = "ĉ"
circ 'G' = "Ĝ"
circ 'g' = "ĝ"
circ 'H' = "Ĥ"
circ 'h' = "ĥ"
circ 'J' = "Ĵ"
circ 'j' = "ĵ"
circ 'S' = "Ŝ"
circ 's' = "ŝ"
circ 'W' = "Ŵ"
circ 'w' = "ŵ"
circ 'Y' = "Ŷ"
circ 'y' = "ŷ"
circ c   = [c]

tilde :: Char -> String
tilde 'A' = "Ã"
tilde 'a' = "ã"
tilde 'O' = "Õ"
tilde 'o' = "õ"
tilde 'I' = "Ĩ"
tilde 'i' = "ĩ"
tilde 'U' = "Ũ"
tilde 'u' = "ũ"
tilde 'N' = "Ñ"
tilde 'n' = "ñ"
tilde c   = [c]

umlaut :: Char -> String
umlaut 'A' = "Ä"
umlaut 'E' = "Ë"
umlaut 'I' = "Ï"
umlaut 'O' = "Ö"
umlaut 'U' = "Ü"
umlaut 'a' = "ä"
umlaut 'e' = "ë"
umlaut 'i' = "ï"
umlaut 'o' = "ö"
umlaut 'u' = "ü"
umlaut c   = [c]

dot :: Char -> String
dot 'C' = "Ċ"
dot 'c' = "ċ"
dot 'E' = "Ė"
dot 'e' = "ė"
dot 'G' = "Ġ"
dot 'g' = "ġ"
dot 'I' = "İ"
dot 'Z' = "Ż"
dot 'z' = "ż"
dot c   = [c]

macron :: Char -> String
macron 'A' = "Ā"
macron 'E' = "Ē"
macron 'I' = "Ī"
macron 'O' = "Ō"
macron 'U' = "Ū"
macron 'a' = "ā"
macron 'e' = "ē"
macron 'i' = "ī"
macron 'o' = "ō"
macron 'u' = "ū"
macron c   = [c]

cedilla :: Char -> String
cedilla 'c' = "ç"
cedilla 'C' = "Ç"
cedilla 's' = "ş"
cedilla 'S' = "Ş"
cedilla 't' = "ţ"
cedilla 'T' = "Ţ"
cedilla 'e' = "ȩ"
cedilla 'E' = "Ȩ"
cedilla 'h' = "ḩ"
cedilla 'H' = "Ḩ"
cedilla 'o' = "o̧"
cedilla 'O' = "O̧"
cedilla c   = [c]

hacek :: Char -> String
hacek 'A' = "Ǎ"
hacek 'a' = "ǎ"
hacek 'C' = "Č"
hacek 'c' = "č"
hacek 'D' = "Ď"
hacek 'd' = "ď"
hacek 'E' = "Ě"
hacek 'e' = "ě"
hacek 'G' = "Ǧ"
hacek 'g' = "ǧ"
hacek 'H' = "Ȟ"
hacek 'h' = "ȟ"
hacek 'I' = "Ǐ"
hacek 'i' = "ǐ"
hacek 'j' = "ǰ"
hacek 'K' = "Ǩ"
hacek 'k' = "ǩ"
hacek 'L' = "Ľ"
hacek 'l' = "ľ"
hacek 'N' = "Ň"
hacek 'n' = "ň"
hacek 'O' = "Ǒ"
hacek 'o' = "ǒ"
hacek 'R' = "Ř"
hacek 'r' = "ř"
hacek 'S' = "Š"
hacek 's' = "š"
hacek 'T' = "Ť"
hacek 't' = "ť"
hacek 'U' = "Ǔ"
hacek 'u' = "ǔ"
hacek 'Z' = "Ž"
hacek 'z' = "ž"
hacek c   = [c]

breve :: Char -> String
breve 'A' = "Ă"
breve 'a' = "ă"
breve 'E' = "Ĕ"
breve 'e' = "ĕ"
breve 'G' = "Ğ"
breve 'g' = "ğ"
breve 'I' = "Ĭ"
breve 'i' = "ĭ"
breve 'O' = "Ŏ"
breve 'o' = "ŏ"
breve 'U' = "Ŭ"
breve 'u' = "ŭ"
breve c   = [c]
