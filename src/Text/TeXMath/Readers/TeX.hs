{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.List (intercalate, find)
import Data.Ratio ((%))
import Control.Monad
import Data.Char (isDigit, isAscii, isLetter)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import Text.Parsec hiding (label)
import Text.Parsec.Error
import Text.Parsec.Text
import Text.TeXMath.Types
import Data.Functor (($>))
import Control.Applicative ((<*), (*>), (<*>), (<$>), (<$), pure)
import qualified Text.TeXMath.Shared as S
import Text.TeXMath.Readers.TeX.Macros (applyMacros, parseMacroDefinitions)
import Text.TeXMath.Unicode.ToTeX (getSymbolType)
import Data.Maybe (fromJust)
import Text.TeXMath.Unicode.ToUnicode (toUnicode)
import Text.TeXMath.Shared (getSpaceChars)

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
          , hspace
          , mathop
          , phantom
          , boxed
          , binary
          , genfrac
          , substack
          , bareSubSup
          , environment
          , unicode
          , ensuremath
          , scaled
          , enclosure
          , texSymbol
          ] <* ignorable

-- | Parse a formula, returning a list of 'Exp'.
readTeX :: T.Text -> Either T.Text [Exp]
readTeX inp =
  let (ms, rest) = parseMacroDefinitions inp in
  either (Left . showParseError inp) (Right . id)
    $ parse formula "formula" $ applyMacros ms rest

showParseError :: T.Text -> ParseError -> T.Text
showParseError inp pe =
  snippet <> "\n" <> caretline <>
    T.pack (showErrorMessages "or" "unknown" "expecting" "unexpected" "eof"
            (errorMessages pe))
  where errln = sourceLine (errorPos pe)
        errcol = sourceColumn (errorPos pe)
        snipoffset = max 0 (errcol - 20)
        inplns = T.lines inp
        ln = if length inplns >= errln
                then inplns !! (errln - 1)
                else ""  -- should not happen
        snippet = T.take 40 $ T.drop snipoffset ln
        caretline = T.replicate (errcol - snipoffset - 1) " " <> "^"

anyCtrlSeq :: TP T.Text
anyCtrlSeq = lexeme $ try $ do
  char '\\'
  res <- count 1 (satisfy (not . isLetter)) <|> many1 (satisfy isLetter)
  return $ T.pack $ '\\' : res

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
ignorable = skipMany $
        comment
    <|> label
    <|> () <$ ctrlseq "nonumber"
    <|> (skipMany1 space <?> "whitespace")

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
    -- these are slightly different but we won't worry about that here...
    convertible <- (char '*' >> spaces >> return True) <|> return False
    op <- expToOperatorName <$> texToken
    maybe mzero (\s -> return (EMathOperator s, convertible)) op

-- | Converts identifiers, symbols and numbers to a flat string.
-- Returns Nothing if the expression contains anything else.
expToOperatorName :: Exp -> Maybe T.Text
expToOperatorName e = case e of
            EGrouped xs -> T.concat <$> mapM fl xs
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
                    EStyled sty xs -> T.concat <$> sequence (map (toStr sty) xs)
                    _ -> Nothing
          toStr sty (EIdentifier s)     = Just $ toUnicode sty s
          toStr _   (EText sty' s)      = Just $ toUnicode sty' s
          toStr sty (ENumber s)         = Just $ toUnicode sty s
          toStr sty (EMathOperator s)   = Just $ toUnicode sty s
          toStr sty (ESymbol _ s)       = Just $ toUnicode sty s
          toStr _   (ESpace n)          = Just $ getSpaceChars n
          toStr _   (EStyled sty' exps) = T.concat <$>
                                            sequence (map (toStr sty') exps)
          toStr _   _                   = Nothing

bareSubSup :: TP Exp
bareSubSup = subSup Nothing False (EIdentifier "")
  <|> superOrSubscripted Nothing False (EIdentifier "")

limitsIndicator :: TP (Maybe Bool)
limitsIndicator =
   (ctrlseq "limits" >> return (Just True))
  <|> (ctrlseq "nolimits" >> return (Just False))
  <|> return Nothing

binomCmd :: TP T.Text
binomCmd = oneOfCommands (M.keys binomCmds)

binomCmds :: M.Map T.Text (Exp -> Exp -> Exp)
binomCmds = M.fromList
            [ ("\\choose", \x y ->
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
  return $ EDelimited (T.singleton openDelim)
                      (T.singleton closeDelim)
                      [Right (EFraction fracType x y)]

substack :: TP Exp
substack = do
  ctrlseq "substack"
  formulas <- braces $ ignorable >> (manyExp expr) `sepEndBy` endLine
  return $ EArray [AlignCenter] $ map (\x -> [[x]]) formulas

asGroup :: [Exp] -> Exp
asGroup [x] = x
asGroup xs = EGrouped xs

-- variant of many that is sensitive to \choose and other such commands
manyExp' :: Bool -> TP Exp -> TP Exp
manyExp' requireNonempty p = do
  initial <- if requireNonempty
                then many1 (notFollowedBy binomCmd >> p)
                else many (notFollowedBy binomCmd >> p)
  let withCmd :: T.Text -> TP Exp
      withCmd cmd =
         case M.lookup cmd binomCmds of
              Just f  -> f <$> (asGroup <$> pure initial)
                           <*> (asGroup <$> many p)
              Nothing -> fail $ "Unknown command " <> T.unpack cmd
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
    return $ (if isDigit c then ENumber else EIdentifier) $ T.singleton c

inbrackets :: TP Exp
inbrackets = (brackets $ manyExp $ notFollowedBy (char ']') >> expr)

number :: TP Exp
number = lexeme $ ENumber <$> try decimalNumber
  where decimalNumber = do
          xs <- many digit
          ys <- option [] $ try (char '.' >> (('.':) <$> many1 digit))
          case xs ++ ys of
               []  -> mzero
               zs  -> return $ T.pack zs

enclosure :: TP Exp
enclosure = basicEnclosure <|> delimited

basicEnclosure :: TP Exp
basicEnclosure = try $ do
  possibleEncl <- lexeme (anyCtrlSeq <|> countChar 1 (oneOf "()[]|"))
  case M.lookup possibleEncl enclosures of
       Just x  -> return x
       Nothing -> mzero

fence :: String -> TP T.Text
fence cmd = do
  symbol cmd
  enc <- basicEnclosure <|> (try (symbol ".") >> return (ESymbol Open ""))
  case enc of
       ESymbol Open x  -> return x
       ESymbol Close x -> return x
       _ -> mzero

middle :: TP T.Text
middle = fence "\\middle"

right :: TP T.Text
right = fence "\\right"

delimited :: TP Exp
delimited = do
  openc <- try $ fence "\\left"
  contents <- concat <$>
              many (try $ ((:[]) . Left  <$> middle)
                      <|> (map Right . unGrouped <$>
                             many1Exp (notFollowedBy right *> expr)))
  closec <- right <|> return ""
  return $ EDelimited openc closec contents

scaled :: TP Exp
scaled = do
  cmd <- oneOfCommands (map fst S.scalers)
  case S.getScalerValue cmd of
       Just r  -> EScaled r <$> (basicEnclosure <|> operator)
       Nothing -> mzero

endLine :: TP Char
endLine = try $ do
  symbol "\\\\"
  optional inbrackets  -- can contain e.g. [1.0in] for a line height, not yet supported
  return '\n'

-- Within environments provided by AMSmath, spaces are not allowed between
-- the double-backslash command and its optional argument.
endLineAMS :: TP Char
endLineAMS = lexeme $ try $ do
  string "\\\\"
  skipMany comment
  optional inbrackets  -- can contain e.g. [1.0in] for a line height, not yet supported
  return '\n'

arrayLine :: TP ArrayLine
arrayLine = notFollowedBy (ctrlseq "end" >> return '\n') >>
  sepBy1 (unGrouped <$>
    manyExp (try $ ignorable' *>
               notFollowedBy endLine *>
               expr <*
               ignorable')) (symbol "&")
  where ignorable' = ignorable >>
                     optional (try (ctrlseq "hline" >> ignorable'))
  -- we don't represent the line, but it shouldn't crash parsing

arrayAlignments :: TP [Alignment]
arrayAlignments = try $ do
  as <- braces (many (letter <|> char '|'))
  let letterToAlignment 'l' = AlignLeft
      letterToAlignment 'c' = AlignCenter
      letterToAlignment 'r' = AlignRight
      letterToAlignment _   = AlignCenter
  return $ map letterToAlignment $ filter (/= '|') as

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
          braces (textStr name <* optional (char '*'))
          spaces
          return result
        Nothing  -> mzero  -- should not happen

environments :: M.Map T.Text (TP Exp)
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
  , ("equation", equation)
  ]

alignsFromRows :: Alignment -> [ArrayLine] -> [Alignment]
alignsFromRows _ [] = []
alignsFromRows defaultAlignment (r:_) = replicate (length r) defaultAlignment

matrixWith :: T.Text -> T.Text -> TP Exp
matrixWith opendelim closedelim = do
  lines' <- sepEndBy1 arrayLine endLineAMS
  let aligns = alignsFromRows AlignCenter lines'
  return $ if T.null opendelim && T.null closedelim
              then EArray aligns lines'
              else EDelimited opendelim closedelim
                       [Right $ EArray aligns lines']

stdarray :: TP Exp
stdarray = do
  aligns <- arrayAlignments
  lines' <- sepEndBy1 arrayLine endLine
  return $ EArray aligns lines'

gather :: TP Exp
gather = do
  rows <- sepEndBy arrayLine endLineAMS
  return $ EArray (alignsFromRows AlignCenter rows) rows

equation :: TP Exp
equation = do
  notFollowedBy (ctrlseq "end" >> return '\n')
  manyExp (notFollowedBy endLine >> expr)

eqnarray :: TP Exp
eqnarray = do
  rows <- sepEndBy1 arrayLine endLine
  let n = maximum $ map length rows
  return $ EArray (take n $ cycle [AlignRight, AlignCenter, AlignLeft]) rows

align :: TP Exp
align = do
  rows <- sepEndBy1 arrayLine endLineAMS
  let n = maximum $ map length rows
  return $ EArray (take n $ cycle [AlignRight, AlignLeft]) rows

flalign :: TP Exp
flalign = do
  rows <- sepEndBy1 arrayLine endLineAMS
  let n = maximum $ map length rows
  return $ EArray (take n $ cycle [AlignLeft, AlignRight]) rows

cases :: TP Exp
cases = do
  rs <- sepEndBy1 arrayLine endLineAMS
  return $ EDelimited "{" "" [Right $ EArray (alignsFromRows AlignLeft rs) rs]

variable :: TP Exp
variable = do
  v <- letter
  spaces
  return $ EIdentifier $ T.singleton v

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
isUnderover (EOver _ _ (ESymbol TOver "\xFE37")) = True   -- \overbrace
isUnderover (EOver _ _ (ESymbol TOver "\x23B4")) = True   -- \overbracket
isUnderover (EOver _  _ (ESymbol TOver "\x23DE")) = True  -- \overbrace
isUnderover (EUnder _ _ (ESymbol TUnder "\xFE38")) = True  -- \underbrace
isUnderover (EUnder _ _ (ESymbol TUnder "\x23B5")) = True  -- \underbracket
isUnderover (EUnder _  _ (ESymbol TUnder "\x23DF")) = True  -- \underbrace
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
    return (ESymbol (getSymbolType c) $ T.singleton c)

ensuremath :: TP Exp
ensuremath = ctrlseq "ensuremath" *> inbraces

-- Note: cal and scr are treated the same way, as unicode is lacking such two different sets for those.
styleOps :: M.Map T.Text ([Exp] -> Exp)
styleOps = M.fromList
          [ ("\\mathrm",     EStyled TextNormal)
          , ("\\mathup",     EStyled TextNormal)
          , ("\\mathbf",     EStyled TextBold)
          , ("\\boldsymbol", EStyled TextBold)
          , ("\\bm",         EStyled TextBold)
          , ("\\symbf",      EStyled TextBold)
          , ("\\mathbold",   EStyled TextBold)
          , ("\\pmb",        EStyled TextBold)
          , ("\\mathbfup",   EStyled TextBold)
          , ("\\mathit",     EStyled TextItalic)
          , ("\\mathtt",     EStyled TextMonospace)
          , ("\\texttt",     EStyled TextMonospace)
          , ("\\mathsf",     EStyled TextSansSerif)
          , ("\\mathsfup",   EStyled TextSansSerif)
          , ("\\mathbb",     EStyled TextDoubleStruck)
          , ("\\mathds",     EStyled TextDoubleStruck) -- mathds package
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

phantom :: TP Exp
phantom = EPhantom <$> (ctrlseq "phantom" *> texToken)

boxed :: TP Exp
boxed = EBoxed <$> (ctrlseq "boxed" *> texToken)

text :: TP Exp
text = do
  c <- oneOfCommands (M.keys textOps)
  op <- maybe mzero return $ M.lookup c textOps
  char '{'
  let chunk = ((op . T.concat) <$> many1 textual)
            <|> (char '{' *> (asGroup <$> manyTill chunk (char '}')))
            <|> innermath
  contents <- manyTill chunk (char '}')
  spaces
  case contents of
       []   -> return (op "")
       [x]  -> return x
       xs   -> return (EGrouped xs)

innermath :: TP Exp
innermath = choice $ map innerMathWith
              [("$","$"),("$$","$$"),("\\(","\\)"),("\\[","\\]")]

innerMathWith :: (String, String) -> TP Exp
innerMathWith (opener, closer) = do
  try (string opener)
  e <- manyExp expr
  string closer
  return e

textOps :: M.Map T.Text (T.Text -> Exp)
textOps = M.fromList
          [ ("\\textrm", (EText TextNormal))
          , ("\\text",   (EText TextNormal))
          , ("\\textbf", (EText TextBold))
          , ("\\textit", (EText TextItalic))
          , ("\\texttt", (EText TextMonospace))
          , ("\\textsf", (EText TextSansSerif))
          , ("\\mbox",   (EText TextNormal))
          ]

styled :: TP Exp
styled = do
  c <- oneOfCommands (M.keys styleOps)
  case M.lookup c styleOps of
       Just f   -> do
         x <- texSymbol <|> inbraces <|> texChar
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
  braces $ do
    len <- many1 digit
    lexeme $ string "mu"
    case reads len of
       ((n :: Integer,[]):_) -> return $ ESpace (fromIntegral n/18)
       _                     -> mzero


hspace :: TP Exp
hspace = do
  ctrlseq "hspace"
  braces $ do
    len <- many1 digit
    scaleFactor <-
           1      <$ (string "em")
      <|> (1/12)  <$ (string "pt")
      <|> 6       <$ (string "in")
      <|> (50/21) <$ (string "cm")
    case reads len of
       ((n :: Integer,[]):_) -> return $ ESpace (fromIntegral n * scaleFactor)
       _                     -> mzero


mathop :: TP Exp
mathop = mathopWith "mathop" Op
     <|> mathopWith "mathrel" Rel
     <|> mathopWith "mathbin" Bin
     <|> mathopWith "mathord" Ord
     <|> mathopWith "mathopen" Open
     <|> mathopWith "mathclose" Close
     <|> mathopWith "mathpunct" Pun

mathopWith :: String -> TeXSymbolType -> TP Exp
mathopWith name ty = try $ do
  ctrlseq name
  e <- inbraces <|> expr1
  let es' = case e of
                 EGrouped xs -> xs
                 x           -> [x]
  case es' of
     [ESymbol _ x]   -> return $ ESymbol ty x
     [EIdentifier x] -> return $ ESymbol ty x
     [EText TextNormal x] -> return $ ESymbol ty x
     [EText sty x] -> return $ EStyled sty [ESymbol ty x]
     xs | ty == Op  -> return $ EMathOperator $
                         T.concat $ mapMaybe expToOperatorName xs
        | otherwise -> return $ EGrouped xs

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

oneOfCommands :: [T.Text] -> TP T.Text
oneOfCommands cmds = try $ do
  cmd <- oneOfStrings cmds
  case T.unpack cmd of
    ['\\',c] | not (isLetter c) -> return ()
    cmd' -> (do pos <- getPosition
                letter
                setPosition pos
                mzero <?> ("non-letter after " <> cmd'))
         <|> return ()
  spaces
  return cmd

oneOfStrings' :: (Char -> Char -> Bool) -> [(String, T.Text)] -> TP T.Text
oneOfStrings' _ [] = mzero
oneOfStrings' matches strs = try $ do
    c <- anyChar
    let strs' = [(xs, t) | ((x:xs), t) <- strs, x `matches` c]
    case strs' of
      []  -> mzero
      _   -> oneOfStrings' matches strs'
             <|> case find (null . fst) strs' of
                   Just (_, t) -> return t
                   Nothing     -> mzero

-- | Parses one of a list of strings.  If the list contains
-- two strings one of which is a prefix of the other, the longer
-- string will be matched if possible.
oneOfStrings :: [T.Text] -> TP T.Text
oneOfStrings strs = oneOfStrings' (==) strs' <??> (intercalate ", " $ map show strs)
  where
    strs' = map (\x -> (T.unpack x, x)) strs

-- | Like '(<?>)', but moves position back to the beginning of the parse
-- before reporting the error.
(<??>) :: Monad m => ParsecT s u m a -> String -> ParsecT s u m a
(<??>) p expected = do
  pos <- getPosition
  p <|> (setPosition pos >> mzero <?> expected)

infix 0 <??>

tSymbol :: TP Exp
tSymbol = try $ do
  sym <- anyCtrlSeq
  case M.lookup sym symbols of
       Just acc@(ESymbol Accent _) ->
         (\t -> EOver False t acc) <$> texToken
       Just acc@(ESymbol TUnder _) ->
         (\t -> EUnder False t acc) <$> texToken
       Just acc@(ESymbol TOver _) ->
         (\t -> EOver False t acc) <$> texToken
       Just x  -> return x
       Nothing -> mzero

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
lexeme p = p <* ignorable

braces :: TP a -> TP a
braces p = lexeme $ char '{' *> spaces *> p <* spaces <* char '}'

brackets :: TP a -> TP a
brackets p = lexeme $ char '[' *> spaces *> p <* spaces <* char ']'

textStr :: T.Text -> TP T.Text
textStr t = string (T.unpack t) $> t

countChar :: Int -> TP Char -> TP T.Text
countChar n = fmap T.pack . count n

symbol :: String -> TP String
symbol s = lexeme $ try $ string s

enclosures :: M.Map T.Text Exp
enclosures = M.fromList
  [ ("(", ESymbol Open "(")
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
  , ("|", ESymbol Close "|")
  , ("|", ESymbol Open "|")
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

operators :: M.Map T.Text Exp
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

symbols :: M.Map T.Text Exp
symbols = M.fromList
  [ ("\\$",ESymbol Ord "$")
  , ("\\%",ESymbol Ord "%")
  , ("\\&",ESymbol Ord "&")
  , ("\\_",ESymbol Ord "_")
  , ("\\#",ESymbol Ord "#")
  , ("\\^",ESymbol Ord "^")
  , ("\\mid",ESymbol Bin "\8739")
  , ("\\colon",ESymbol Pun ":")
  , ("\\parallel",ESymbol Rel "\8741")
  , ("\\backslash",ESymbol Bin "\8726")
  , ("\\setminus",ESymbol Bin "\\")
  , ("\\times",ESymbol Bin "\215")
  , ("\\ltimes",ESymbol Bin "\8905")
  , ("\\rtimes",ESymbol Bin "\8906")
  , ("\\alpha",EIdentifier "\945")
  , ("\\beta",EIdentifier "\946")
  , ("\\chi",EIdentifier "\967")
  , ("\\delta",EIdentifier "\948")
  , ("\\Delta",EIdentifier "\916")
  , ("\\epsilon",EIdentifier "\1013")
  , ("\\varepsilon",EIdentifier "\949")
  , ("\\eta",EIdentifier "\951")
  , ("\\gamma",EIdentifier "\947")
  , ("\\Gamma",EIdentifier "\915")
  , ("\\iota",EIdentifier "\953")
  , ("\\kappa",EIdentifier "\954")
  , ("\\lambda",EIdentifier "\955")
  , ("\\Lambda",EIdentifier "\923")
  , ("\\mu",EIdentifier "\956")
  , ("\\nu",EIdentifier "\957")
  , ("\\omega",EIdentifier "\969")
  , ("\\Omega",EIdentifier "\937")
  , ("\\phi",EIdentifier "\981")
  , ("\\varphi",EIdentifier "\966")
  , ("\\Phi",EIdentifier "\934")
  , ("\\pi",EIdentifier "\960")
  , ("\\Pi",EIdentifier "\928")
  , ("\\psi",EIdentifier "\968")
  , ("\\Psi",EIdentifier "\936")
  , ("\\rho",EIdentifier "\961")
  , ("\\sigma",EIdentifier "\963")
  , ("\\Sigma",EIdentifier "\931")
  , ("\\tau",EIdentifier "\964")
  , ("\\theta",EIdentifier "\952")
  , ("\\vartheta",EIdentifier "\977")
  , ("\\Theta",EIdentifier "\920")
  , ("\\upsilon",EIdentifier "\965")
  , ("\\Upsilon",EIdentifier "\933")
  , ("\\xi",EIdentifier "\958")
  , ("\\Xi",EIdentifier "\926")
  , ("\\zeta",EIdentifier "\950")
  , ("\\pm",ESymbol Bin "\177")
  , ("\\mp",ESymbol Bin "\8723")
  , ("\\triangleleft",ESymbol Bin "\8882")
  , ("\\triangleright",ESymbol Bin "\8883")
  , ("\\cdot",ESymbol Bin "\8901")
  , ("\\star",ESymbol Bin "\8902")
  , ("\\ast",ESymbol Bin "*")
  , ("\\times",ESymbol Bin "\215")
  , ("\\div",ESymbol Bin "\247")
  , ("\\circ",ESymbol Bin "\8728")
  , ("\\bullet",ESymbol Bin "\8226")
  , ("\\oplus",ESymbol Bin "\8853")
  , ("\\ominus",ESymbol Bin "\8854")
  , ("\\otimes",ESymbol Bin "\8855")
  , ("\\bigcirc",ESymbol Bin "\9675")
  , ("\\oslash",ESymbol Bin "\8856")
  , ("\\odot",ESymbol Bin "\8857")
  , ("\\land",ESymbol Bin "\8743")
  , ("\\wedge",ESymbol Bin "\8743")
  , ("\\lor",ESymbol Bin "\8744")
  , ("\\vee",ESymbol Bin "\8744")
  , ("\\cap",ESymbol Bin "\8745")
  , ("\\cup",ESymbol Bin "\8746")
  , ("\\sqcap",ESymbol Bin "\8851")
  , ("\\sqcup",ESymbol Bin "\8852")
  , ("\\uplus",ESymbol Bin "\8846")
  , ("\\amalg",ESymbol Bin "\8720")
  , ("\\bigtriangleup",ESymbol Bin "\9651")
  , ("\\bigtriangledown",ESymbol Bin "\9661")
  , ("\\dag",ESymbol Bin "\8224")
  , ("\\dagger",ESymbol Bin "\8224")
  , ("\\ddag",ESymbol Bin "\8225")
  , ("\\ddagger",ESymbol Bin "\8225")
  , ("\\lhd",ESymbol Bin "\8882")
  , ("\\rhd",ESymbol Bin "\8883")
  , ("\\unlhd",ESymbol Bin "\8884")
  , ("\\unrhd",ESymbol Bin "\8885")
  , ("\\lt",ESymbol Rel "<")
  , ("\\gt",ESymbol Rel ">")
  , ("\\ne",ESymbol Rel "\8800")
  , ("\\neq",ESymbol Rel "\8800")
  , ("\\le",ESymbol Rel "\8804")
  , ("\\leq",ESymbol Rel "\8804")
  , ("\\leqslant",ESymbol Rel "\8804")
  , ("\\ge",ESymbol Rel "\8805")
  , ("\\geq",ESymbol Rel "\8805")
  , ("\\geqslant",ESymbol Rel "\8805")
  , ("\\equiv",ESymbol Rel "\8801")
  , ("\\ll",ESymbol Rel "\8810")
  , ("\\gg",ESymbol Rel "\8811")
  , ("\\doteq",ESymbol Rel "\8784")
  , ("\\prec",ESymbol Rel "\8826")
  , ("\\succ",ESymbol Rel "\8827")
  , ("\\preceq",ESymbol Rel "\8828")
  , ("\\succeq",ESymbol Rel "\8829")
  , ("\\subset",ESymbol Rel "\8834")
  , ("\\supset",ESymbol Rel "\8835")
  , ("\\subseteq",ESymbol Rel "\8838")
  , ("\\supseteq",ESymbol Rel "\8839")
  , ("\\nsubset",ESymbol Rel "\8836")
  , ("\\nsupset",ESymbol Rel "\8837")
  , ("\\nsubseteq",ESymbol Rel "\8840")
  , ("\\nsupseteq",ESymbol Rel "\8841")
  , ("\\sqsubset",ESymbol Rel "\8847")
  , ("\\sqsupset",ESymbol Rel "\8848")
  , ("\\sqsubseteq",ESymbol Rel "\8849")
  , ("\\sqsupseteq",ESymbol Rel "\8850")
  , ("\\sim",ESymbol Rel "\8764")
  , ("\\simeq",ESymbol Rel "\8771")
  , ("\\approx",ESymbol Rel "\8776")
  , ("\\cong",ESymbol Rel "\8773")
  , ("\\Join",ESymbol Rel "\8904")
  , ("\\bowtie",ESymbol Rel "\8904")
  , ("\\in",ESymbol Rel "\8712")
  , ("\\ni",ESymbol Rel "\8715")
  , ("\\owns",ESymbol Rel "\8715")
  , ("\\propto",ESymbol Rel "\8733")
  , ("\\vdash",ESymbol Rel "\8866")
  , ("\\dashv",ESymbol Rel "\8867")
  , ("\\models",ESymbol Rel "\8872")
  , ("\\perp",ESymbol Rel "\8869")
  , ("\\smile",ESymbol Rel "\8995")
  , ("\\frown",ESymbol Rel "\8994")
  , ("\\asymp",ESymbol Rel "\8781")
  , ("\\notin",ESymbol Rel "\8713")
  , ("\\gets",ESymbol Rel "\8592")
  , ("\\leftarrow",ESymbol Rel "\8592")
  , ("\\nwarrow",ESymbol Rel "\8598")
  , ("\\nearrow",ESymbol Rel "\8599")
  , ("\\searrow",ESymbol Rel "\8600")
  , ("\\swarrow",ESymbol Rel "\8601")
  , ("\\to",ESymbol Rel "\8594")
  , ("\\rightarrow",ESymbol Rel "\8594")
  , ("\\leftrightarrow",ESymbol Rel "\8596")
  , ("\\uparrow",ESymbol Rel "\8593")
  , ("\\downarrow",ESymbol Rel "\8595")
  , ("\\updownarrow",ESymbol Rel "\8597")
  , ("\\Leftarrow",ESymbol Rel "\8656")
  , ("\\Rightarrow",ESymbol Rel "\8658")
  , ("\\Leftrightarrow",ESymbol Rel "\8660")
  , ("\\iff",ESymbol Rel "\8660")
  , ("\\Uparrow",ESymbol Rel "\8657")
  , ("\\Downarrow",ESymbol Rel "\8659")
  , ("\\Updownarrow",ESymbol Rel "\8661")
  , ("\\mapsto",ESymbol Rel "\8614")
  , ("\\longleftarrow",ESymbol Rel "\8592")
  , ("\\longrightarrow",ESymbol Rel "\8594")
  , ("\\longleftrightarrow",ESymbol Rel "\8596")
  , ("\\Longleftarrow",ESymbol Rel "\8656")
  , ("\\Longrightarrow",ESymbol Rel "\8658")
  , ("\\Longleftrightarrow",ESymbol Rel "\8660")
  , ("\\longmapsto",ESymbol Rel "\8614")
  , ("\\sum",ESymbol Op "\8721")
  , ("\\prod",ESymbol Op "\8719")
  , ("\\bigcap",ESymbol Op "\8898")
  , ("\\bigcup",ESymbol Op "\8899")
  , ("\\bigwedge",ESymbol Op "\8896")
  , ("\\bigvee",ESymbol Op "\8897")
  , ("\\bigsqcap",ESymbol Op "\10757")
  , ("\\bigsqcup",ESymbol Op "\10758")
  , ("\\coprod",ESymbol Op "\8720")
  , ("\\bigoplus",ESymbol Op "\10753")
  , ("\\bigotimes",ESymbol Op "\10754")
  , ("\\bigodot",ESymbol Op "\10752")
  , ("\\biguplus",ESymbol Op "\10756")
  , ("\\int",ESymbol Op "\8747")
  , ("\\iint",ESymbol Op "\8748")
  , ("\\iiint",ESymbol Op "\8749")
  , ("\\oint",ESymbol Op "\8750")
  , ("\\prime",ESymbol Ord "\8242")
  , ("\\dots",ESymbol Ord "\8230")
  , ("\\ldots",ESymbol Ord "\8230")
  , ("\\hdots",ESymbol Ord "\8230")
  , ("\\cdots",ESymbol Ord "\8943")
  , ("\\vdots",ESymbol Ord "\8942")
  , ("\\ddots",ESymbol Ord "\8945")
  , ("\\forall",ESymbol Op "\8704")
  , ("\\exists",ESymbol Op "\8707")
  , ("\\Re",ESymbol Ord "\8476")
  , ("\\Im",ESymbol Ord "\8465")
  , ("\\aleph",ESymbol Ord "\8501")
  , ("\\hbar",ESymbol Ord "\8463")
  , ("\\ell",ESymbol Ord "\8467")
  , ("\\wp",ESymbol Ord "\8472")
  , ("\\emptyset",ESymbol Ord "\8709")
  , ("\\infty",ESymbol Ord "\8734")
  , ("\\partial",ESymbol Ord "\8706")
  , ("\\nabla",ESymbol Ord "\8711")
  , ("\\triangle",ESymbol Ord "\9651")
  , ("\\therefore",ESymbol Pun "\8756")
  , ("\\angle",ESymbol Ord "\8736")
  , ("\\diamond",ESymbol Op "\8900")
  , ("\\Diamond",ESymbol Op "\9671")
  , ("\\lozenge",ESymbol Op "\9674")
  , ("\\neg",ESymbol Op "\172")
  , ("\\lnot",ESymbol Ord "\172")
  , ("\\bot",ESymbol Ord "\8869")
  , ("\\top",ESymbol Ord "\8868")
  , ("\\square",ESymbol Ord "\9643")
  , ("\\Box",ESymbol Op "\9633")
  , ("\\wr",ESymbol Ord "\8768")
  , ("\\!",ESpace ((-1) % 6))
  , ("\\,",ESpace (1 % 6))
  , ("\\>",ESpace (2 % 9))
  , ("\\:",ESpace (2 % 9))
  , ("\\;",ESpace (5 % 18))
  , ("\\ ",ESpace (2 % 9))
  , ("\\\n",ESpace (2 % 9))
  , ("\\quad",ESpace (1 % 1))
  , ("\\qquad",ESpace (2 % 1))
  , ("\\arccos",EMathOperator "arccos")
  , ("\\arcsin",EMathOperator "arcsin")
  , ("\\arctan",EMathOperator "arctan")
  , ("\\arg",EMathOperator "arg")
  , ("\\cos",EMathOperator "cos")
  , ("\\cosh",EMathOperator "cosh")
  , ("\\cot",EMathOperator "cot")
  , ("\\coth",EMathOperator "coth")
  , ("\\csc",EMathOperator "csc")
  , ("\\deg",EMathOperator "deg")
  , ("\\det",EMathOperator "det")
  , ("\\dim",EMathOperator "dim")
  , ("\\exp",EMathOperator "exp")
  , ("\\gcd",EMathOperator "gcd")
  , ("\\hom",EMathOperator "hom")
  , ("\\inf",EMathOperator "inf")
  , ("\\ker",EMathOperator "ker")
  , ("\\lg",EMathOperator "lg")
  , ("\\lim",EMathOperator "lim")
  , ("\\liminf",EMathOperator "liminf")
  , ("\\limsup",EMathOperator "limsup")
  , ("\\ln",EMathOperator "ln")
  , ("\\log",EMathOperator "log")
  , ("\\max",EMathOperator "max")
  , ("\\min",EMathOperator "min")
  , ("\\mod",EMathOperator "mod")
  , ("\\bmod",ESymbol Rel "mod")
  , ("\\Pr",EMathOperator "Pr")
  , ("\\sec",EMathOperator "sec")
  , ("\\sin",EMathOperator "sin")
  , ("\\sinh",EMathOperator "sinh")
  , ("\\sup",EMathOperator "sup")
  , ("\\tan",EMathOperator "tan")
  , ("\\tanh",EMathOperator "tanh")
  , ("\\AC",ESymbol Ord "\8767")
  , ("\\AC",ESymbol Ord "\9190")
  , ("\\APLboxquestion",ESymbol Ord "\9072")
  , ("\\APLboxupcaret",ESymbol Ord "\9043")
  , ("\\APLcomment",ESymbol Ord "\9053")
  , ("\\APLdownarrowbox",ESymbol Ord "\9047")
  , ("\\APLinput",ESymbol Ord "\9054")
  , ("\\APLinv",ESymbol Ord "\9017")
  , ("\\APLleftarrowbox",ESymbol Ord "\9031")
  , ("\\APLlog",ESymbol Ord "\9055")
  , ("\\APLnotbackslash",ESymbol Ord "\9024")
  , ("\\APLnotslash",ESymbol Rel "\9023")
  , ("\\APLrightarrowbox",ESymbol Ord "\9032")
  , ("\\APLuparrowbox",ESymbol Ord "\9040")
  , ("\\Angstroem",ESymbol Alpha "\8491")
  , ("\\Angstrom",ESymbol Alpha "\8491")
  , ("\\Aries",ESymbol Ord "\9800")
  , ("\\Barv",ESymbol Rel "\10983")
  , ("\\BbbA",ESymbol Alpha "\120120")
  , ("\\BbbB",ESymbol Alpha "\120121")
  , ("\\BbbC",ESymbol Alpha "\8450")
  , ("\\BbbD",ESymbol Alpha "\120123")
  , ("\\BbbE",ESymbol Alpha "\120124")
  , ("\\BbbF",ESymbol Alpha "\120125")
  , ("\\BbbG",ESymbol Alpha "\120126")
  , ("\\BbbGamma",ESymbol Alpha "\8510")
  , ("\\BbbH",ESymbol Alpha "\8461")
  , ("\\BbbI",ESymbol Alpha "\120128")
  , ("\\BbbJ",ESymbol Alpha "\120129")
  , ("\\BbbK",ESymbol Alpha "\120130")
  , ("\\BbbL",ESymbol Alpha "\120131")
  , ("\\BbbM",ESymbol Alpha "\120132")
  , ("\\BbbN",ESymbol Alpha "\8469")
  , ("\\BbbO",ESymbol Alpha "\120134")
  , ("\\BbbP",ESymbol Alpha "\8473")
  , ("\\BbbPi",ESymbol Alpha "\8511")
  , ("\\BbbQ",ESymbol Alpha "\8474")
  , ("\\BbbR",ESymbol Alpha "\8477")
  , ("\\BbbS",ESymbol Alpha "\120138")
  , ("\\BbbT",ESymbol Alpha "\120139")
  , ("\\BbbU",ESymbol Alpha "\120140")
  , ("\\BbbV",ESymbol Alpha "\120141")
  , ("\\BbbW",ESymbol Alpha "\120142")
  , ("\\BbbX",ESymbol Alpha "\120143")
  , ("\\BbbY",ESymbol Alpha "\120144")
  , ("\\BbbZ",ESymbol Alpha "\8484")
  , ("\\Bbba",ESymbol Alpha "\120146")
  , ("\\Bbbb",ESymbol Alpha "\120147")
  , ("\\Bbbc",ESymbol Alpha "\120148")
  , ("\\Bbbd",ESymbol Alpha "\120149")
  , ("\\Bbbe",ESymbol Alpha "\120150")
  , ("\\Bbbeight",ESymbol Ord "\120800")
  , ("\\Bbbf",ESymbol Alpha "\120151")
  , ("\\Bbbfive",ESymbol Ord "\120797")
  , ("\\Bbbfour",ESymbol Ord "\120796")
  , ("\\Bbbg",ESymbol Alpha "\120152")
  , ("\\Bbbgamma",ESymbol Alpha "\8509")
  , ("\\Bbbh",ESymbol Alpha "\120153")
  , ("\\Bbbi",ESymbol Alpha "\120154")
  , ("\\Bbbj",ESymbol Alpha "\120155")
  , ("\\Bbbk",ESymbol Alpha "\120156")
  , ("\\Bbbl",ESymbol Alpha "\120157")
  , ("\\Bbbm",ESymbol Alpha "\120158")
  , ("\\Bbbn",ESymbol Alpha "\120159")
  , ("\\Bbbnine",ESymbol Ord "\120801")
  , ("\\Bbbo",ESymbol Alpha "\120160")
  , ("\\Bbbone",ESymbol Ord "\120793")
  , ("\\Bbbp",ESymbol Alpha "\120161")
  , ("\\Bbbpi",ESymbol Ord "\8508")
  , ("\\Bbbq",ESymbol Alpha "\120162")
  , ("\\Bbbr",ESymbol Alpha "\120163")
  , ("\\Bbbs",ESymbol Alpha "\120164")
  , ("\\Bbbseven",ESymbol Ord "\120799")
  , ("\\Bbbsix",ESymbol Ord "\120798")
  , ("\\Bbbsum",ESymbol Op "\8512")
  , ("\\Bbbt",ESymbol Alpha "\120165")
  , ("\\Bbbthree",ESymbol Ord "\120795")
  , ("\\Bbbtwo",ESymbol Ord "\120794")
  , ("\\Bbbu",ESymbol Alpha "\120166")
  , ("\\Bbbv",ESymbol Alpha "\120167")
  , ("\\Bbbw",ESymbol Alpha "\120168")
  , ("\\Bbbx",ESymbol Alpha "\120169")
  , ("\\Bbby",ESymbol Alpha "\120170")
  , ("\\Bbbz",ESymbol Alpha "\120171")
  , ("\\Bbbzero",ESymbol Ord "\120792")
  , ("\\Bot",ESymbol Rel "\10987")
  , ("\\Bumpeq",ESymbol Rel "\8782")
  , ("\\CIRCLE",ESymbol Ord "\9679")
  , ("\\Cap",ESymbol Bin "\8914")
  , ("\\CapitalDifferentialD",ESymbol Ord "\8517")
  , ("\\CheckedBox",ESymbol Ord "\9745")
  , ("\\Circle",ESymbol Bin "\9675")
  , ("\\Colon",ESymbol Rel "\8759")
  , ("\\Coloneq",ESymbol Rel "\10868")
  , ("\\Coloneqq",ESymbol Rel "\10868")
  , ("\\ComplexI",ESymbol Ord "\8520")
  , ("\\ComplexJ",ESymbol Ord "\8521")
  , ("\\Cup",ESymbol Bin "\8915")
  , ("\\DD",ESymbol Ord "\8517")
  , ("\\DDDot",ESymbol Accent "\8411")
  , ("\\DDot",ESymbol Accent "\776")
  , ("\\DDownarrow",ESymbol Rel "\10225")
  , ("\\DashV",ESymbol Rel "\10981")
  , ("\\DashVDash",ESymbol Rel "\10202")
  , ("\\Dashv",ESymbol Rel "\10980")
  , ("\\Ddownarrow",ESymbol Rel "\10507")
  , ("\\Diamondblack",ESymbol Ord "\9670")
  , ("\\Diamonddot",ESymbol Ord "\10192")
  , ("\\DifferentialD",ESymbol Ord "\8518")
  , ("\\Digamma",ESymbol Alpha "\988")
  , ("\\Dot",ESymbol Accent "\775")
  , ("\\Doteq",ESymbol Rel "\8785")
  , ("\\DownArrowBar",ESymbol Rel "\10515")
  , ("\\DownLeftTeeVector",ESymbol Rel "\10590")
  , ("\\DownLeftVectorBar",ESymbol Rel "\10582")
  , ("\\DownRightTeeVector",ESymbol Rel "\10591")
  , ("\\DownRightVectorBar",ESymbol Rel "\10583")
  , ("\\Equal",ESymbol Rel "\10869")
  , ("\\Equiv",ESymbol Rel "\8803")
  , ("\\Euler",ESymbol Ord "\8455")
  , ("\\Eulerconst",ESymbol Ord "\8455")
  , ("\\Exclam",ESymbol Ord "\8252")
  , ("\\ExponetialE",ESymbol Ord "\8519")
  , ("\\Finv",ESymbol Ord "\8498")
  , ("\\Game",ESymbol Ord "\8513")
  , ("\\Gemini",ESymbol Ord "\9802")
  , ("\\GreaterLess",ESymbol Rel "\8823")
  , ("\\Gt",ESymbol Rel "\10914")
  , ("\\HBar",ESymbol Alpha "\8463")
  , ("\\Hermaphrodite",ESymbol Ord "\9893")
  , ("\\Jupiter",ESymbol Ord "\9795")
  , ("\\Koppa",ESymbol Alpha "\990")
  , ("\\Koppa",ESymbol Ord "\984")
  , ("\\LEFTCIRCLE",ESymbol Ord "\9686")
  , ("\\LEFTcircle",ESymbol Ord "\9680")
  , ("\\LHD",ESymbol Bin "\9664")
  , ("\\LLeftarrow",ESymbol Rel "\11077")
  , ("\\LVec",ESymbol Accent "\8406")
  , ("\\Lbag",ESymbol Open "\10181")
  , ("\\Lbrack",ESymbol Open "\10214")
  , ("\\Lbrbrak",ESymbol Open "\10220")
  , ("\\Lbrbrak",ESymbol Open "\12312")
  , ("\\Ldsh",ESymbol Rel "\8626")
  , ("\\LeftArrowBar",ESymbol Rel "\8676")
  , ("\\LeftDownTeeVector",ESymbol Rel "\10593")
  , ("\\LeftDownVectorBar",ESymbol Rel "\10585")
  , ("\\LeftTeeVector",ESymbol Rel "\10586")
  , ("\\LeftTriangleBar",ESymbol Rel "\10703")
  , ("\\LeftUpTeeVector",ESymbol Rel "\10592")
  , ("\\LeftUpVectorBar",ESymbol Rel "\10584")
  , ("\\LeftVectorBar",ESymbol Rel "\10578")
  , ("\\Leo",ESymbol Ord "\9804")
  , ("\\Libra",ESymbol Ord "\9806")
  , ("\\Lleftarrow",ESymbol Rel "\8666")
  , ("\\Longmappedfrom",ESymbol Rel "\10237")
  , ("\\Longmapsfrom",ESymbol Rel "\10237")
  , ("\\Longmapsto",ESymbol Rel "\10238")
  , ("\\Lparen",ESymbol Open "\10629")
  , ("\\Lparengtr",ESymbol Open "\10645")
  , ("\\Lsh",ESymbol Rel "\8624")
  , ("\\Lt",ESymbol Rel "\10913")
  , ("\\Lvzigzag",ESymbol Open "\10714")
  , ("\\Mappedfrom",ESymbol Rel "\10502")
  , ("\\MapsDown",ESymbol Rel "\8615")
  , ("\\MapsUp",ESymbol Rel "\8613")
  , ("\\Mapsfrom",ESymbol Rel "\10502")
  , ("\\Mapsto",ESymbol Rel "\10503")
  , ("\\Mars",ESymbol Ord "\9794")
  , ("\\Mercury",ESymbol Ord "\9791")
  , ("\\Mho",ESymbol Ord "\8487")
  , ("\\Micro",ESymbol Alpha "\181")
  , ("\\Nearrow",ESymbol Rel "\8663")
  , ("\\Neptune",ESymbol Ord "\9798")
  , ("\\NestedGreaterGreater",ESymbol Rel "\10914")
  , ("\\NestedLessLess",ESymbol Rel "\10913")
  , ("\\Not",ESymbol Rel "\10988")
  , ("\\NotGreaterLess",ESymbol Rel "\8825")
  , ("\\NotGreaterTilde",ESymbol Rel "\8821")
  , ("\\NotLeftTriangle",ESymbol Rel "\8938")
  , ("\\NotLessTilde",ESymbol Rel "\8820")
  , ("\\NotRightTriangle",ESymbol Rel "\8939")
  , ("\\Nwarrow",ESymbol Rel "\8662")
  , ("\\Otimes",ESymbol Bin "\10807")
  , ("\\Perp",ESymbol Rel "\10987")
  , ("\\Planckconst",ESymbol Ord "\8462")
  , ("\\Pluto",ESymbol Ord "\9799")
  , ("\\Prec",ESymbol Rel "\10939")
  , ("\\PrecedesSlantEqual",ESymbol Rel "\8828")
  , ("\\PrecedesTilde",ESymbol Rel "\8830")
  , ("\\PropertyLine",ESymbol Ord "\8522")
  , ("\\Proportion",ESymbol Rel "\8759")
  , ("\\QED",ESymbol Ord "\8718")
  , ("\\Qoppa",ESymbol Ord "\984")
  , ("\\Question",ESymbol Ord "\8263")
  , ("\\RHD",ESymbol Bin "\9654")
  , ("\\RIGHTCIRCLE",ESymbol Ord "\9687")
  , ("\\RIGHTcircle",ESymbol Ord "\9681")
  , ("\\RRightarrow",ESymbol Rel "\11078")
  , ("\\Rbag",ESymbol Close "\10182")
  , ("\\Rbrack",ESymbol Close "\10215")
  , ("\\Rbrbrak",ESymbol Close "\10221")
  , ("\\Rbrbrak",ESymbol Close "\12313")
  , ("\\Rdsh",ESymbol Rel "\8627")
  , ("\\RightArrowBar",ESymbol Rel "\8677")
  , ("\\RightDownTeeVector",ESymbol Rel "\10589")
  , ("\\RightDownVectorBar",ESymbol Rel "\10581")
  , ("\\RightTeeVector",ESymbol Rel "\10587")
  , ("\\RightTriangleBar",ESymbol Rel "\10704")
  , ("\\RightUpTeeVector",ESymbol Rel "\10588")
  , ("\\RightUpVectorBar",ESymbol Rel "\10580")
  , ("\\RightVectorBar",ESymbol Rel "\10579")
  , ("\\Rparen",ESymbol Close "\10630")
  , ("\\Rparenless",ESymbol Close "\10646")
  , ("\\Rrightarrow",ESymbol Rel "\8667")
  , ("\\Rsh",ESymbol Rel "\8625")
  , ("\\Rvzigzag",ESymbol Close "\10715")
  , ("\\Same",ESymbol Rel "\10870")
  , ("\\Sampi",ESymbol Alpha "\992")
  , ("\\Saturn",ESymbol Ord "\9796")
  , ("\\Scorpio",ESymbol Ord "\9807")
  , ("\\Searrow",ESymbol Rel "\8664")
  , ("\\Sqcap",ESymbol Bin "\10830")
  , ("\\Sqcup",ESymbol Bin "\10831")
  , ("\\Square",ESymbol Ord "\9744")
  , ("\\Stigma",ESymbol Alpha "\986")
  , ("\\Subset",ESymbol Rel "\8912")
  , ("\\Succ",ESymbol Rel "\10940")
  , ("\\SucceedsSlantEqual",ESymbol Rel "\8829")
  , ("\\SucceedsTilde",ESymbol Rel "\8831")
  , ("\\Sun",ESymbol Ord "\9737")
  , ("\\Supset",ESymbol Rel "\8913")
  , ("\\Swarrow",ESymbol Rel "\8665")
  , ("\\Taurus",ESymbol Ord "\9801")
  , ("\\Top",ESymbol Rel "\10986")
  , ("\\UUparrow",ESymbol Rel "\10224")
  , ("\\UpArrowBar",ESymbol Rel "\10514")
  , ("\\Uranus",ESymbol Ord "\9797")
  , ("\\Uuparrow",ESymbol Rel "\10506")
  , ("\\VDash",ESymbol Rel "\8875")
  , ("\\VERT",ESymbol Fence "\10624")
  , ("\\Vbar",ESymbol Rel "\10987")
  , ("\\Vdash",ESymbol Rel "\8873")
  , ("\\Vec",ESymbol Accent "\8407")
  , ("\\Vee",ESymbol Bin "\10836")
  , ("\\Venus",ESymbol Ord "\9792")
  , ("\\Vert",ESymbol Fence "\8214")
  , ("\\Vvdash",ESymbol Rel "\8874")
  , ("\\Vvert",ESymbol Fence "\10624")
  , ("\\Wedge",ESymbol Bin "\10835")
  , ("\\XBox",ESymbol Ord "\9746")
  , ("\\Yup",ESymbol Ord "\8516")
  , ("\\Zbar",ESymbol Ord "\437")
  , ("\\accurrent",ESymbol Ord "\9190")
  , ("\\acidfree",ESymbol Ord "\9854")
  , ("\\acute",ESymbol Accent "\769")
  , ("\\acwcirclearrow",ESymbol Rel "\10560")
  , ("\\acwgapcirclearrow",ESymbol Rel "\10226")
  , ("\\acwleftarcarrow",ESymbol Rel "\10553")
  , ("\\acwopencirclearrow",ESymbol Ord "\8634")
  , ("\\acwoverarcarrow",ESymbol Rel "\10554")
  , ("\\acwunderarcarrow",ESymbol Rel "\10555")
  , ("\\adots",ESymbol Rel "\8944")
  , ("\\ampersand",ESymbol Ord "&")
  , ("\\anchor",ESymbol Ord "\9875")
  , ("\\angdnr",ESymbol Ord "\10655")
  , ("\\angles",ESymbol Ord "\10654")
  , ("\\angleubar",ESymbol Ord "\10660")
  , ("\\annuity",ESymbol Accent "\8423")
  , ("\\apprge",ESymbol Rel "\8819")
  , ("\\apprle",ESymbol Rel "\8818")
  , ("\\approxeq",ESymbol Rel "\8778")
  , ("\\approxeqq",ESymbol Rel "\10864")
  , ("\\approxident",ESymbol Rel "\8779")
  , ("\\aquarius",ESymbol Ord "\9810")
  , ("\\arceq",ESymbol Rel "\8792")
  , ("\\aries",ESymbol Ord "\9800")
  , ("\\arrowbullet",ESymbol Ord "\10146")
  , ("\\assert",ESymbol Rel "\8870")
  , ("\\asteq",ESymbol Rel "\10862")
  , ("\\asteraccent",ESymbol Accent "\8432")
  , ("\\astrosun",ESymbol Ord "\9737")
  , ("\\atsign",ESymbol Ord "@")
  , ("\\awint",ESymbol Op "\10769")
  , ("\\bNot",ESymbol Rel "\10989")
  , ("\\backcong",ESymbol Rel "\8780")
  , ("\\backdprime",ESymbol Ord "\8246")
  , ("\\backepsilon",ESymbol Ord "\1014")
  , ("\\backprime",ESymbol Ord "\8245")
  , ("\\backsim",ESymbol Rel "\8765")
  , ("\\backsimeq",ESymbol Rel "\8909")
  , ("\\backtrprime",ESymbol Ord "\8247")
  , ("\\bagmember",ESymbol Rel "\8959")
  , ("\\ballotcheck",ESymbol Ord "\10003")
  , ("\\ballotx",ESymbol Ord "\10007")
  , ("\\bar",ESymbol Accent "\8254")
  , ("\\barV",ESymbol Rel "\10986")
  , ("\\barcap",ESymbol Bin "\10819")
  , ("\\barcup",ESymbol Bin "\10818")
  , ("\\bardownharpoonleft",ESymbol Rel "\10593")
  , ("\\bardownharpoonright",ESymbol Rel "\10589")
  , ("\\barin",ESymbol Rel "\8950")
  , ("\\barleftarrow",ESymbol Rel "\8676")
  , ("\\barleftarrowrightarrowba",ESymbol Ord "\8633")
  , ("\\barleftharpoon",ESymbol Rel "\10603")
  , ("\\barleftharpoondown",ESymbol Rel "\10582")
  , ("\\barleftharpoonup",ESymbol Rel "\10578")
  , ("\\barovernorthwestarrow",ESymbol Ord "\8632")
  , ("\\barrightarrowdiamond",ESymbol Rel "\10528")
  , ("\\barrightharpoon",ESymbol Rel "\10605")
  , ("\\barrightharpoondown",ESymbol Rel "\10591")
  , ("\\barrightharpoonup",ESymbol Rel "\10587")
  , ("\\baruparrow",ESymbol Rel "\10514")
  , ("\\barupharpoonleft",ESymbol Rel "\10584")
  , ("\\barupharpoonright",ESymbol Rel "\10580")
  , ("\\barvee",ESymbol Bin "\8893")
  , ("\\barwedge",ESymbol Bin "\8892")
  , ("\\barwedge",ESymbol Bin "\8965")
  , ("\\bbrktbrk",ESymbol Ord "\9142")
  , ("\\bdtriplevdash",ESymbol Ord "\9478")
  , ("\\because",ESymbol Ord "\8757")
  , ("\\benzenr",ESymbol Ord "\9187")
  , ("\\beth",ESymbol Alpha "\8502")
  , ("\\between",ESymbol Rel "\8812")
  , ("\\bigblacktriangledown",ESymbol Ord "\9660")
  , ("\\bigblacktriangleup",ESymbol Ord "\9650")
  , ("\\bigbot",ESymbol Op "\10200")
  , ("\\bigcupdot",ESymbol Op "\10755")
  , ("\\biginterleave",ESymbol Op "\11004")
  , ("\\bigslopedvee",ESymbol Bin "\10839")
  , ("\\bigslopedwedge",ESymbol Bin "\10840")
  , ("\\bigstar",ESymbol Ord "\9733")
  , ("\\bigtalloblong",ESymbol Op "\11007")
  , ("\\bigtimes",ESymbol Op "\10761")
  , ("\\bigtop",ESymbol Op "\10201")
  , ("\\bigtriangleleft",ESymbol Op "\10782")
  , ("\\bigwhitestar",ESymbol Ord "\9734")
  , ("\\bij",ESymbol Rel "\10518")
  , ("\\binampersand",ESymbol Ord "&")
  , ("\\bindnasrepma",ESymbol Bin "\8523")
  , ("\\biohazard",ESymbol Ord "\9763")
  , ("\\blackcircledownarrow",ESymbol Ord "\10733")
  , ("\\blackcircledrightdot",ESymbol Ord "\9864")
  , ("\\blackcircledtwodots",ESymbol Ord "\9865")
  , ("\\blackcircleulquadwhite",ESymbol Ord "\9685")
  , ("\\blackdiamonddownarrow",ESymbol Ord "\10730")
  , ("\\blackhourglass",ESymbol Bin "\10711")
  , ("\\blackinwhitediamond",ESymbol Ord "\9672")
  , ("\\blackinwhitesquare",ESymbol Ord "\9635")
  , ("\\blacklefthalfcircle",ESymbol Ord "\9686")
  , ("\\blacklozenge",ESymbol Bin "\10731")
  , ("\\blacklozenge",ESymbol Ord "\11047")
  , ("\\blackpointerleft",ESymbol Ord "\9668")
  , ("\\blackpointerright",ESymbol Ord "\9658")
  , ("\\blackrighthalfcircle",ESymbol Ord "\9687")
  , ("\\blacksmiley",ESymbol Ord "\9787")
  , ("\\blacksquare",ESymbol Ord "\11035")
  , ("\\blacksquare",ESymbol Ord "\8718")
  , ("\\blacksquare",ESymbol Ord "\9632")
  , ("\\blacksquare",ESymbol Ord "\9724")
  , ("\\blacktriangle",ESymbol Bin "\9652")
  , ("\\blacktriangledown",ESymbol Bin "\9662")
  , ("\\blacktriangleleft",ESymbol Bin "\9664")
  , ("\\blacktriangleleft",ESymbol Bin "\9666")
  , ("\\blacktriangleright",ESymbol Bin "\9654")
  , ("\\blacktriangleright",ESymbol Bin "\9656")
  , ("\\blacktriangleup",ESymbol Bin "\9652")
  , ("\\blkhorzoval",ESymbol Ord "\11052")
  , ("\\blkvertoval",ESymbol Ord "\11054")
  , ("\\blockfull",ESymbol Ord "\9608")
  , ("\\blockhalfshaded",ESymbol Ord "\9618")
  , ("\\blocklefthalf",ESymbol Ord "\9612")
  , ("\\blocklowhalf",ESymbol Ord "\9604")
  , ("\\blockqtrshaded",ESymbol Ord "\9617")
  , ("\\blockrighthalf",ESymbol Ord "\9616")
  , ("\\blockthreeqtrshaded",ESymbol Ord "\9619")
  , ("\\blockuphalf",ESymbol Ord "\9600")
  , ("\\botsemicircle",ESymbol Ord "\9697")
  , ("\\boxast",ESymbol Bin "\10694")
  , ("\\boxbar",ESymbol Bin "\9707")
  , ("\\boxbox",ESymbol Bin "\10696")
  , ("\\boxbslash",ESymbol Bin "\10693")
  , ("\\boxcircle",ESymbol Bin "\10695")
  , ("\\boxdiag",ESymbol Bin "\10692")
  , ("\\boxdot",ESymbol Bin "\8865")
  , ("\\boxminus",ESymbol Bin "\8863")
  , ("\\boxonbox",ESymbol Ord "\10697")
  , ("\\boxplus",ESymbol Bin "\8862")
  , ("\\boxslash",ESymbol Bin "\10692")
  , ("\\boxtimes",ESymbol Bin "\8864")
  , ("\\breve",ESymbol Accent "\774")
  , ("\\bsimilarleftarrow",ESymbol Rel "\11073")
  , ("\\bsimilarrightarrow",ESymbol Rel "\11079")
  , ("\\bsolhsub",ESymbol Rel "\10184")
  , ("\\btimes",ESymbol Bin "\10802")
  , ("\\bullseye",ESymbol Ord "\9678")
  , ("\\bumpeq",ESymbol Rel "\8783")
  , ("\\bumpeqq",ESymbol Rel "\10926")
  , ("\\buni",ESymbol Bin "\8846")
  , ("\\cancer",ESymbol Ord "\9803")
  , ("\\candra",ESymbol Accent "\784")
  , ("\\capbarcup",ESymbol Bin "\10825")
  , ("\\capdot",ESymbol Bin "\10816")
  , ("\\capovercup",ESymbol Bin "\10823")
  , ("\\capricornus",ESymbol Ord "\9809")
  , ("\\capwedge",ESymbol Bin "\10820")
  , ("\\caretinsert",ESymbol Ord "\8248")
  , ("\\carriagereturn",ESymbol Ord "\8629")
  , ("\\cat",ESymbol Bin "\8256")
  , ("\\ccwundercurvearrow",ESymbol Rel "\10559")
  , ("\\cdotp",ESymbol Bin "\183")
  , ("\\cent",ESymbol Ord "\162")
  , ("\\centerdot",ESymbol Ord "\11037")
  , ("\\check",ESymbol Accent "\780")
  , ("\\checkmark",ESymbol Ord "\10003")
  , ("\\cirE",ESymbol Ord "\10691")
  , ("\\cirbot",ESymbol Rel "\10207")
  , ("\\circeq",ESymbol Rel "\8791")
  , ("\\circlearrowleft",ESymbol Ord "\8634")
  , ("\\circlearrowright",ESymbol Ord "\8635")
  , ("\\circlebottomhalfblack",ESymbol Ord "\9682")
  , ("\\circledR",ESymbol Ord "\174")
  , ("\\circledast",ESymbol Bin "\8859")
  , ("\\circledbslash",ESymbol Bin "\10680")
  , ("\\circledbullet",ESymbol Ord "\10687")
  , ("\\circledcirc",ESymbol Bin "\8858")
  , ("\\circledcirc",ESymbol Ord "\9678")
  , ("\\circleddash",ESymbol Bin "\8861")
  , ("\\circledequal",ESymbol Bin "\8860")
  , ("\\circledgtr",ESymbol Bin "\10689")
  , ("\\circledless",ESymbol Bin "\10688")
  , ("\\circledownarrow",ESymbol Ord "\10732")
  , ("\\circledparallel",ESymbol Bin "\10679")
  , ("\\circledrightdot",ESymbol Ord "\9862")
  , ("\\circledstar",ESymbol Ord "\10026")
  , ("\\circledtwodots",ESymbol Ord "\9863")
  , ("\\circledvert",ESymbol Bin "\10678")
  , ("\\circledwhitebullet",ESymbol Ord "\10686")
  , ("\\circlehbar",ESymbol Bin "\10677")
  , ("\\circlelefthalfblack",ESymbol Ord "\9680")
  , ("\\circlellquad",ESymbol Ord "\9717")
  , ("\\circlelrquad",ESymbol Ord "\9718")
  , ("\\circleonleftarrow",ESymbol Rel "\11056")
  , ("\\circleonrightarrow",ESymbol Rel "\8692")
  , ("\\circlerighthalfblack",ESymbol Ord "\9681")
  , ("\\circletophalfblack",ESymbol Ord "\9683")
  , ("\\circleulquad",ESymbol Ord "\9716")
  , ("\\circleurquad",ESymbol Ord "\9719")
  , ("\\circleurquadblack",ESymbol Ord "\9684")
  , ("\\circlevertfill",ESymbol Ord "\9677")
  , ("\\cirfnint",ESymbol Op "\10768")
  , ("\\cirmid",ESymbol Rel "\10991")
  , ("\\cirscir",ESymbol Ord "\10690")
  , ("\\clockoint",ESymbol Op "\8754")
  , ("\\closedvarcap",ESymbol Bin "\10829")
  , ("\\closedvarcup",ESymbol Bin "\10828")
  , ("\\closedvarcupsmashprod",ESymbol Bin "\10832")
  , ("\\closure",ESymbol Rel "\8272")
  , ("\\clubsuit",ESymbol Ord "\9827")
  , ("\\cntclockoint",ESymbol Op "\8755")
  , ("\\coloneq",ESymbol Rel "\8788")
  , ("\\coloneqq",ESymbol Rel "\8788")
  , ("\\comma",ESymbol Pun ",")
  , ("\\commaminus",ESymbol Bin "\10793")
  , ("\\comp",ESymbol Bin "\10814")
  , ("\\complement",ESymbol Ord "\8705")
  , ("\\concavediamond",ESymbol Bin "\10209")
  , ("\\concavediamondtickleft",ESymbol Bin "\10210")
  , ("\\concavediamondtickright",ESymbol Bin "\10211")
  , ("\\congdot",ESymbol Rel "\10861")
  , ("\\conictaper",ESymbol Ord "\9010")
  , ("\\conjquant",ESymbol Op "\10759")
  , ("\\corresponds",ESymbol Rel "\8793")
  , ("\\csub",ESymbol Rel "\10959")
  , ("\\csube",ESymbol Rel "\10961")
  , ("\\csup",ESymbol Rel "\10960")
  , ("\\csupe",ESymbol Rel "\10962")
  , ("\\cuberoot",ESymbol Rad "\8731")
  , ("\\cupbarcap",ESymbol Bin "\10824")
  , ("\\cupdot",ESymbol Bin "\8845")
  , ("\\cupleftarrow",ESymbol Bin "\8844")
  , ("\\cupovercap",ESymbol Bin "\10822")
  , ("\\cupvee",ESymbol Bin "\10821")
  , ("\\curlyeqprec",ESymbol Rel "\8926")
  , ("\\curlyeqsucc",ESymbol Rel "\8927")
  , ("\\curlyvee",ESymbol Bin "\8910")
  , ("\\curlywedge",ESymbol Bin "\8911")
  , ("\\curvearrowleft",ESymbol Rel "\8630")
  , ("\\curvearrowleftplus",ESymbol Rel "\10557")
  , ("\\curvearrowright",ESymbol Rel "\8631")
  , ("\\curvearrowrightminus",ESymbol Rel "\10556")
  , ("\\cwcirclearrow",ESymbol Rel "\10561")
  , ("\\cwgapcirclearrow",ESymbol Rel "\10227")
  , ("\\cwopencirclearrow",ESymbol Ord "\8635")
  , ("\\cwrightarcarrow",ESymbol Rel "\10552")
  , ("\\cwundercurvearrow",ESymbol Rel "\10558")
  , ("\\daleth",ESymbol Alpha "\8504")
  , ("\\danger",ESymbol Ord "\9761")
  , ("\\dashV",ESymbol Rel "\10979")
  , ("\\dashVdash",ESymbol Rel "\10203")
  , ("\\dasharrow",ESymbol Ord "\8674")
  , ("\\dashcolon",ESymbol Rel "\8761")
  , ("\\dashleftarrow",ESymbol Ord "\8672")
  , ("\\dashleftharpoondown",ESymbol Rel "\10603")
  , ("\\dashrightarrow",ESymbol Ord "\8674")
  , ("\\dashrightharpoondown",ESymbol Rel "\10605")
  , ("\\dbkarow",ESymbol Rel "\10511")
  , ("\\dbloint",ESymbol Op "\8751")
  , ("\\dd",ESymbol Ord "\8518")
  , ("\\ddddot",ESymbol Accent "\8412")
  , ("\\dddot",ESymbol Accent "\8411")
  , ("\\ddot",ESymbol Accent "\776")
  , ("\\ddotseq",ESymbol Rel "\10871")
  , ("\\diameter",ESymbol Ord "\8960")
  , ("\\diamondbotblack",ESymbol Ord "\11033")
  , ("\\diamondcdot",ESymbol Ord "\10192")
  , ("\\diamondleftarrow",ESymbol Rel "\10525")
  , ("\\diamondleftarrowbar",ESymbol Rel "\10527")
  , ("\\diamondleftblack",ESymbol Ord "\11030")
  , ("\\diamondrightblack",ESymbol Ord "\11031")
  , ("\\diamondsuit",ESymbol Ord "\9826")
  , ("\\diamondtopblack",ESymbol Ord "\11032")
  , ("\\dicei",ESymbol Ord "\9856")
  , ("\\diceii",ESymbol Ord "\9857")
  , ("\\diceiii",ESymbol Ord "\9858")
  , ("\\diceiv",ESymbol Ord "\9859")
  , ("\\dicev",ESymbol Ord "\9860")
  , ("\\dicevi",ESymbol Ord "\9861")
  , ("\\digamma",ESymbol Alpha "\988")
  , ("\\digamma",ESymbol Alpha "\989")
  , ("\\dingasterisk",ESymbol Ord "\10045")
  , ("\\dint",ESymbol Op "\8898")
  , ("\\disin",ESymbol Rel "\8946")
  , ("\\disjquant",ESymbol Op "\10760")
  , ("\\divideontimes",ESymbol Bin "\8903")
  , ("\\divslash",ESymbol Bin "\8725")
  , ("\\dlsh",ESymbol Rel "\8626")
  , ("\\dot",ESymbol Accent "\775")
  , ("\\doteqdot",ESymbol Rel "\8785")
  , ("\\dotequal",ESymbol Rel "\8784")
  , ("\\dotequiv",ESymbol Rel "\10855")
  , ("\\dotminus",ESymbol Bin "\8760")
  , ("\\dotplus",ESymbol Bin "\8724")
  , ("\\dotsim",ESymbol Rel "\10858")
  , ("\\dotsminusdots",ESymbol Rel "\8762")
  , ("\\dottedcircle",ESymbol Ord "\9676")
  , ("\\dottedsquare",ESymbol Ord "\11034")
  , ("\\dottimes",ESymbol Bin "\10800")
  , ("\\doublebarvee",ESymbol Bin "\10850")
  , ("\\doublebarwedge",ESymbol Bin "\10846")
  , ("\\doublebarwedge",ESymbol Bin "\8966")
  , ("\\doubleplus",ESymbol Bin "\10746")
  , ("\\downarrowbar",ESymbol Rel "\10515")
  , ("\\downarrowbarred",ESymbol Rel "\10504")
  , ("\\downarrowuparrow",ESymbol Rel "\8693")
  , ("\\downdasharrow",ESymbol Ord "\8675")
  , ("\\downdownarrows",ESymbol Rel "\8650")
  , ("\\downdownharpoons",ESymbol Rel "\10597")
  , ("\\downfishtail",ESymbol Rel "\10623")
  , ("\\downharpoonleft",ESymbol Rel "\8643")
  , ("\\downharpoonleftbar",ESymbol Rel "\10585")
  , ("\\downharpoonright",ESymbol Rel "\8642")
  , ("\\downharpoonrightbar",ESymbol Rel "\10581")
  , ("\\downharpoonsleftright",ESymbol Rel "\10597")
  , ("\\downrightcurvedarrow",ESymbol Ord "\10549")
  , ("\\downtriangleleftblack",ESymbol Ord "\10728")
  , ("\\downtrianglerightblack",ESymbol Ord "\10729")
  , ("\\downuparrows",ESymbol Rel "\8693")
  , ("\\downupharpoons",ESymbol Rel "\10607")
  , ("\\downupharpoonsleftright",ESymbol Rel "\10607")
  , ("\\downwhitearrow",ESymbol Ord "\8681")
  , ("\\downzigzagarrow",ESymbol Rel "\8623")
  , ("\\dprime",ESymbol Ord "\8243")
  , ("\\draftingarrow",ESymbol Ord "\10139")
  , ("\\drbkarow",ESymbol Rel "\10512")
  , ("\\dres",ESymbol Bin "\9665")
  , ("\\droang",ESymbol Accent "\794")
  , ("\\drsh",ESymbol Rel "\8627")
  , ("\\dsol",ESymbol Bin "\10742")
  , ("\\dsub",ESymbol Bin "\10852")
  , ("\\dualmap",ESymbol Rel "\10719")
  , ("\\duni",ESymbol Op "\8899")
  , ("\\earth",ESymbol Ord "\9793")
  , ("\\ee",ESymbol Ord "\8519")
  , ("\\egsdot",ESymbol Rel "\10904")
  , ("\\eighthnote",ESymbol Ord "\9834")
  , ("\\elinters",ESymbol Ord "\9191")
  , ("\\elsdot",ESymbol Rel "\10903")
  , ("\\emptysetoarr",ESymbol Ord "\10675")
  , ("\\emptysetoarrl",ESymbol Ord "\10676")
  , ("\\emptysetobar",ESymbol Ord "\10673")
  , ("\\emptysetocirc",ESymbol Ord "\10674")
  , ("\\enclosecircle",ESymbol Accent "\8413")
  , ("\\enclosediamond",ESymbol Accent "\8415")
  , ("\\enclosesquare",ESymbol Accent "\8414")
  , ("\\enclosetriangle",ESymbol Accent "\8420")
  , ("\\enleadertwodots",ESymbol Ord "\8229")
  , ("\\eparsl",ESymbol Rel "\10723")
  , ("\\eqcirc",ESymbol Rel "\8790")
  , ("\\eqcolon",ESymbol Rel "\8761")
  , ("\\eqcolon",ESymbol Rel "\8789")
  , ("\\eqdef",ESymbol Rel "\8797")
  , ("\\eqdot",ESymbol Rel "\10854")
  , ("\\eqeq",ESymbol Rel "\10869")
  , ("\\eqeqeq",ESymbol Rel "\10870")
  , ("\\eqgtr",ESymbol Rel "\8925")
  , ("\\eqless",ESymbol Rel "\8924")
  , ("\\eqqcolon",ESymbol Rel "\8789")
  , ("\\eqqgtr",ESymbol Rel "\10906")
  , ("\\eqqless",ESymbol Rel "\10905")
  , ("\\eqqplus",ESymbol Bin "\10865")
  , ("\\eqqsim",ESymbol Rel "\10867")
  , ("\\eqqslantgtr",ESymbol Rel "\10908")
  , ("\\eqqslantless",ESymbol Rel "\10907")
  , ("\\eqsim",ESymbol Rel "\8770")
  , ("\\eqslantgtr",ESymbol Rel "\10902")
  , ("\\eqslantless",ESymbol Rel "\10901")
  , ("\\equal",ESymbol Rel "=")
  , ("\\equalleftarrow",ESymbol Rel "\11072")
  , ("\\equalparallel",ESymbol Rel "\8917")
  , ("\\equalrightarrow",ESymbol Rel "\10609")
  , ("\\equilibrium",ESymbol Rel "\8652")
  , ("\\equivDD",ESymbol Rel "\10872")
  , ("\\equivVert",ESymbol Rel "\10856")
  , ("\\equivVvert",ESymbol Rel "\10857")
  , ("\\eqvparsl",ESymbol Rel "\10725")
  , ("\\errbarblackcircle",ESymbol Ord "\10739")
  , ("\\errbarblackdiamond",ESymbol Ord "\10737")
  , ("\\errbarblacksquare",ESymbol Ord "\10735")
  , ("\\errbarcircle",ESymbol Ord "\10738")
  , ("\\errbardiamond",ESymbol Ord "\10736")
  , ("\\errbarsquare",ESymbol Ord "\10734")
  , ("\\eth",ESymbol Alpha "\240")
  , ("\\euro",ESymbol Ord "\8364")
  , ("\\exclam",ESymbol Pun "!")
  , ("\\exi",ESymbol Ord "\8707")
  , ("\\fallingdotseq",ESymbol Rel "\8786")
  , ("\\fbowtie",ESymbol Rel "\10707")
  , ("\\fcmp",ESymbol Bin "\10814")
  , ("\\fdiagovnearrow",ESymbol Ord "\10543")
  , ("\\fdiagovrdiag",ESymbol Ord "\10540")
  , ("\\female",ESymbol Ord "\9792")
  , ("\\ffun",ESymbol Rel "\8699")
  , ("\\finj",ESymbol Rel "\10517")
  , ("\\fint",ESymbol Op "\10767")
  , ("\\fisheye",ESymbol Ord "\9673")
  , ("\\flat",ESymbol Ord "\9837")
  , ("\\fltns",ESymbol Ord "\9189")
  , ("\\forks",ESymbol Rel "\10972")
  , ("\\forksnot",ESymbol Rel "\10973")
  , ("\\forkv",ESymbol Rel "\10969")
  , ("\\fourth",ESymbol Ord "\8279")
  , ("\\fourthroot",ESymbol Rad "\8732")
  , ("\\fourvdots",ESymbol Ord "\10649")
  , ("\\fracslash",ESymbol Bin "\8260")
  , ("\\frownie",ESymbol Ord "\9785")
  , ("\\fullouterjoin",ESymbol Op "\10199")
  , ("\\gemini",ESymbol Ord "\9802")
  , ("\\geqq",ESymbol Rel "\8807")
  , ("\\geqqslant",ESymbol Rel "\11002")
  , ("\\gescc",ESymbol Rel "\10921")
  , ("\\gesdot",ESymbol Rel "\10880")
  , ("\\gesdoto",ESymbol Rel "\10882")
  , ("\\gesdotol",ESymbol Rel "\10884")
  , ("\\gesles",ESymbol Rel "\10900")
  , ("\\ggcurly",ESymbol Rel "\10940")
  , ("\\ggg",ESymbol Rel "\10914")
  , ("\\ggg",ESymbol Rel "\8921")
  , ("\\gggnest",ESymbol Rel "\11000")
  , ("\\gimel",ESymbol Alpha "\8503")
  , ("\\glE",ESymbol Rel "\10898")
  , ("\\gla",ESymbol Rel "\10917")
  , ("\\gleichstark",ESymbol Rel "\10726")
  , ("\\glj",ESymbol Rel "\10916")
  , ("\\gnapprox",ESymbol Rel "\10890")
  , ("\\gneq",ESymbol Rel "\10888")
  , ("\\gneqq",ESymbol Rel "\8809")
  , ("\\gnsim",ESymbol Rel "\8935")
  , ("\\grave",ESymbol Accent "\768")
  , ("\\greater",ESymbol Rel ">")
  , ("\\gsime",ESymbol Rel "\10894")
  , ("\\gsiml",ESymbol Rel "\10896")
  , ("\\gtcc",ESymbol Rel "\10919")
  , ("\\gtcir",ESymbol Rel "\10874")
  , ("\\gtlpar",ESymbol Ord "\10656")
  , ("\\gtquest",ESymbol Rel "\10876")
  , ("\\gtrapprox",ESymbol Rel "\10886")
  , ("\\gtrarr",ESymbol Rel "\10616")
  , ("\\gtrdot",ESymbol Rel "\8919")
  , ("\\gtreqless",ESymbol Rel "\8923")
  , ("\\gtreqqless",ESymbol Rel "\10892")
  , ("\\gtrless",ESymbol Rel "\8823")
  , ("\\gtrsim",ESymbol Rel "\8819")
  , ("\\harrowextender",ESymbol Ord "\9135")
  , ("\\hash",ESymbol Rel "\8917")
  , ("\\hat",ESymbol Accent "\770")
  , ("\\hatapprox",ESymbol Rel "\10863")
  , ("\\heartsuit",ESymbol Ord "\9825")
  , ("\\hermitmatrix",ESymbol Ord "\8889")
  , ("\\hexagon",ESymbol Ord "\9108")
  , ("\\hexagonblack",ESymbol Ord "\11043")
  , ("\\hide",ESymbol Op "\10745")
  , ("\\hknearrow",ESymbol Rel "\10532")
  , ("\\hknwarrow",ESymbol Rel "\10531")
  , ("\\hksearow",ESymbol Rel "\10533")
  , ("\\hkswarow",ESymbol Rel "\10534")
  , ("\\hookleftarrow",ESymbol Rel "\8617")
  , ("\\hookrightarrow",ESymbol Rel "\8618")
  , ("\\horizbar",ESymbol Ord "\8213")
  , ("\\hourglass",ESymbol Bin "\10710")
  , ("\\house",ESymbol Ord "\8962")
  , ("\\hrectangle",ESymbol Ord "\9645")
  , ("\\hrectangleblack",ESymbol Ord "\9644")
  , ("\\hslash",ESymbol Alpha "\8463")
  , ("\\hyphenbullet",ESymbol Ord "\8259")
  , ("\\hzigzag",ESymbol Ord "\12336")
  , ("\\iddots",ESymbol Rel "\8944")
  , ("\\ii",ESymbol Ord "\8520")
  , ("\\iiiint",ESymbol Op "\10764")
  , ("\\iinfin",ESymbol Ord "\10716")
  , ("\\imageof",ESymbol Rel "\8887")
  , ("\\imath",ESymbol Alpha "\120484")
  , ("\\imath",ESymbol Alpha "\305")
  , ("\\impliedby",ESymbol Rel "\10232")
  , ("\\implies",ESymbol Rel "\10233")
  , ("\\increment",ESymbol Ord "\8710")
  , ("\\intBar",ESymbol Op "\10766")
  , ("\\intbar",ESymbol Op "\10765")
  , ("\\intbottom",ESymbol Ord "\8993")
  , ("\\intcap",ESymbol Op "\10777")
  , ("\\intclockwise",ESymbol Op "\8753")
  , ("\\intcup",ESymbol Op "\10778")
  , ("\\intercal",ESymbol Bin "\8890")
  , ("\\interleave",ESymbol Bin "\10996")
  , ("\\intextender",ESymbol Ord "\9134")
  , ("\\intlarhk",ESymbol Op "\10775")
  , ("\\intprod",ESymbol Bin "\10812")
  , ("\\intprodr",ESymbol Bin "\10813")
  , ("\\inttop",ESymbol Ord "\8992")
  , ("\\intx",ESymbol Op "\10776")
  , ("\\invamp",ESymbol Bin "\8523")
  , ("\\inversebullet",ESymbol Ord "\9688")
  , ("\\inversewhitecircle",ESymbol Ord "\9689")
  , ("\\invlazys",ESymbol Bin "\8766")
  , ("\\invneg",ESymbol Ord "\8976")
  , ("\\invnot",ESymbol Ord "\8976")
  , ("\\invsmileface",ESymbol Ord "\9787")
  , ("\\invwhitelowerhalfcircle",ESymbol Ord "\9691")
  , ("\\invwhiteupperhalfcircle",ESymbol Ord "\9690")
  , ("\\isinE",ESymbol Rel "\8953")
  , ("\\isindot",ESymbol Rel "\8949")
  , ("\\isinobar",ESymbol Rel "\8951")
  , ("\\isins",ESymbol Rel "\8948")
  , ("\\isinvb",ESymbol Rel "\8952")
  , ("\\jj",ESymbol Ord "\8521")
  , ("\\jmath",ESymbol Alpha "\120485")
  , ("\\jmath",ESymbol Alpha "\567")
  , ("\\jupiter",ESymbol Ord "\9795")
  , ("\\kernelcontraction",ESymbol Rel "\8763")
  , ("\\koppa",ESymbol Alpha "\991")
  , ("\\koppa",ESymbol Ord "\985")
  , ("\\lAngle",ESymbol Open "\10218")
  , ("\\lBrace",ESymbol Open "\10627")
  , ("\\lBrack",ESymbol Open "\10214")
  , ("\\lParen",ESymbol Open "\10629")
  , ("\\lang",ESymbol Open "\10218")
  , ("\\langle",ESymbol Open "\10216")
  , ("\\langle",ESymbol Open "\12296")
  , ("\\langle",ESymbol Open "\9001")
  , ("\\langledot",ESymbol Open "\10641")
  , ("\\laplac",ESymbol Ord "\10720")
  , ("\\lat",ESymbol Rel "\10923")
  , ("\\late",ESymbol Rel "\10925")
  , ("\\lbag",ESymbol Open "\10181")
  , ("\\lblkbrbrak",ESymbol Open "\10647")
  , ("\\lblot",ESymbol Open "\10633")
  , ("\\lbrace",ESymbol Open "{")
  , ("\\lbracelend",ESymbol Ord "\9129")
  , ("\\lbracemid",ESymbol Ord "\9128")
  , ("\\lbraceuend",ESymbol Ord "\9127")
  , ("\\lbrack",ESymbol Open "[")
  , ("\\lbrackextender",ESymbol Ord "\9122")
  , ("\\lbracklend",ESymbol Ord "\9123")
  , ("\\lbracklltick",ESymbol Open "\10639")
  , ("\\lbrackubar",ESymbol Open "\10635")
  , ("\\lbrackuend",ESymbol Ord "\9121")
  , ("\\lbrackultick",ESymbol Open "\10637")
  , ("\\lbrbrak",ESymbol Open "\10098")
  , ("\\lbrbrak",ESymbol Open "\12308")
  , ("\\lceil",ESymbol Open "\8968")
  , ("\\lcurvyangle",ESymbol Open "\10748")
  , ("\\leadsto",ESymbol Rel "\10547")
  , ("\\leftarrowapprox",ESymbol Rel "\11082")
  , ("\\leftarrowbackapprox",ESymbol Rel "\11074")
  , ("\\leftarrowbsimilar",ESymbol Rel "\11083")
  , ("\\leftarrowless",ESymbol Rel "\10615")
  , ("\\leftarrowonoplus",ESymbol Rel "\11058")
  , ("\\leftarrowplus",ESymbol Rel "\10566")
  , ("\\leftarrowshortrightarrow",ESymbol Rel "\10563")
  , ("\\leftarrowsimilar",ESymbol Rel "\10611")
  , ("\\leftarrowsubset",ESymbol Rel "\10618")
  , ("\\leftarrowtail",ESymbol Rel "\8610")
  , ("\\leftarrowtriangle",ESymbol Rel "\8701")
  , ("\\leftarrowx",ESymbol Rel "\11070")
  , ("\\leftbarharpoon",ESymbol Rel "\10602")
  , ("\\leftbkarrow",ESymbol Rel "\10508")
  , ("\\leftcurvedarrow",ESymbol Rel "\11071")
  , ("\\leftdasharrow",ESymbol Ord "\8672")
  , ("\\leftdbkarrow",ESymbol Rel "\10510")
  , ("\\leftdbltail",ESymbol Rel "\10523")
  , ("\\leftdotarrow",ESymbol Rel "\11064")
  , ("\\leftdowncurvedarrow",ESymbol Rel "\10550")
  , ("\\leftfishtail",ESymbol Rel "\10620")
  , ("\\leftharpoonaccent",ESymbol Accent "\8400")
  , ("\\leftharpoondown",ESymbol Rel "\8637")
  , ("\\leftharpoondownbar",ESymbol Rel "\10590")
  , ("\\leftharpoonsupdown",ESymbol Rel "\10594")
  , ("\\leftharpoonup",ESymbol Rel "\8636")
  , ("\\leftharpoonupbar",ESymbol Rel "\10586")
  , ("\\leftharpoonupdash",ESymbol Rel "\10602")
  , ("\\leftleftarrows",ESymbol Rel "\8647")
  , ("\\leftleftharpoons",ESymbol Rel "\10594")
  , ("\\leftmoon",ESymbol Ord "\9790")
  , ("\\leftouterjoin",ESymbol Op "\10197")
  , ("\\leftrightarrowcircle",ESymbol Rel "\10568")
  , ("\\leftrightarrows",ESymbol Rel "\8646")
  , ("\\leftrightarrowtriangle",ESymbol Rel "\8703")
  , ("\\leftrightharpoon",ESymbol Rel "\10570")
  , ("\\leftrightharpoondown",ESymbol Rel "\10576")
  , ("\\leftrightharpoondowndown",ESymbol Rel "\10576")
  , ("\\leftrightharpoondownup",ESymbol Rel "\10571")
  , ("\\leftrightharpoons",ESymbol Rel "\8651")
  , ("\\leftrightharpoonsdown",ESymbol Rel "\10599")
  , ("\\leftrightharpoonsup",ESymbol Rel "\10598")
  , ("\\leftrightharpoonup",ESymbol Rel "\10574")
  , ("\\leftrightharpoonupdown",ESymbol Rel "\10570")
  , ("\\leftrightharpoonupup",ESymbol Rel "\10574")
  , ("\\leftrightsquigarrow",ESymbol Rel "\8621")
  , ("\\leftslice",ESymbol Rel "\10918")
  , ("\\leftsquigarrow",ESymbol Rel "\8668")
  , ("\\lefttail",ESymbol Rel "\10521")
  , ("\\leftthreearrows",ESymbol Rel "\11057")
  , ("\\leftthreetimes",ESymbol Bin "\8907")
  , ("\\leftturn",ESymbol Ord "\8634")
  , ("\\leftupdownharpoon",ESymbol Rel "\10577")
  , ("\\leftwavearrow",ESymbol Rel "\8604")
  , ("\\leftwhitearrow",ESymbol Ord "\8678")
  , ("\\leo",ESymbol Ord "\9804")
  , ("\\leqq",ESymbol Rel "\8806")
  , ("\\leqqslant",ESymbol Rel "\11001")
  , ("\\lescc",ESymbol Rel "\10920")
  , ("\\lesdot",ESymbol Rel "\10879")
  , ("\\lesdoto",ESymbol Rel "\10881")
  , ("\\lesdotor",ESymbol Rel "\10883")
  , ("\\lesges",ESymbol Rel "\10899")
  , ("\\less",ESymbol Rel "<")
  , ("\\lessapprox",ESymbol Rel "\10885")
  , ("\\lessdot",ESymbol Rel "\8918")
  , ("\\lesseqgtr",ESymbol Rel "\8922")
  , ("\\lesseqqgtr",ESymbol Rel "\10891")
  , ("\\lessgtr",ESymbol Rel "\8822")
  , ("\\lesssim",ESymbol Rel "\8818")
  , ("\\lfbowtie",ESymbol Rel "\10705")
  , ("\\lfloor",ESymbol Open "\8970")
  , ("\\lftimes",ESymbol Rel "\10708")
  , ("\\lgE",ESymbol Rel "\10897")
  , ("\\lgblkcircle",ESymbol Ord "\11044")
  , ("\\lgblksquare",ESymbol Ord "\11035")
  , ("\\lgroup",ESymbol Open "\10222")
  , ("\\lgwhtcircle",ESymbol Ord "\9711")
  , ("\\lgwhtsquare",ESymbol Ord "\11036")
  , ("\\libra",ESymbol Ord "\9806")
  , ("\\lightning",ESymbol Rel "\8623")
  , ("\\limg",ESymbol Open "\10631")
  , ("\\linefeed",ESymbol Ord "\8628")
  , ("\\llangle",ESymbol Open "\10633")
  , ("\\llarc",ESymbol Ord "\9695")
  , ("\\llblacktriangle",ESymbol Ord "\9699")
  , ("\\llbracket",ESymbol Open "\10214")
  , ("\\llbracket",ESymbol Open "\12314")
  , ("\\llcorner",ESymbol Open "\8990")
  , ("\\llcurly",ESymbol Rel "\10939")
  , ("\\lll",ESymbol Rel "\10913")
  , ("\\lll",ESymbol Rel "\8920")
  , ("\\lllnest",ESymbol Rel "\10999")
  , ("\\llparenthesis",ESymbol Open "\10631")
  , ("\\lltriangle",ESymbol Ord "\9722")
  , ("\\lmoustache",ESymbol Ord "\9136")
  , ("\\lnapprox",ESymbol Rel "\10889")
  , ("\\lneq",ESymbol Rel "\10887")
  , ("\\lneqq",ESymbol Rel "\8808")
  , ("\\lnsim",ESymbol Rel "\8934")
  , ("\\longdashv",ESymbol Rel "\10206")
  , ("\\longdivision",ESymbol Open "\10188")
  , ("\\longleftsquigarrow",ESymbol Rel "\11059")
  , ("\\longmappedfrom",ESymbol Rel "\10235")
  , ("\\longmapsfrom",ESymbol Rel "\10235")
  , ("\\longrightsquigarrow",ESymbol Rel "\10239")
  , ("\\looparrowleft",ESymbol Rel "\8619")
  , ("\\looparrowright",ESymbol Rel "\8620")
  , ("\\lowint",ESymbol Op "\10780")
  , ("\\lozengeminus",ESymbol Bin "\10208")
  , ("\\lparen",ESymbol Open "(")
  , ("\\lparenextender",ESymbol Ord "\9116")
  , ("\\lparenlend",ESymbol Ord "\9117")
  , ("\\lparenless",ESymbol Open "\10643")
  , ("\\lparenuend",ESymbol Ord "\9115")
  , ("\\lrarc",ESymbol Ord "\9694")
  , ("\\lrblacktriangle",ESymbol Ord "\9698")
  , ("\\lrcorner",ESymbol Close "\8991")
  , ("\\lrtimes",ESymbol Rel "\8904")
  , ("\\lrtriangle",ESymbol Ord "\9727")
  , ("\\lrtriangleeq",ESymbol Rel "\10721")
  , ("\\lsime",ESymbol Rel "\10893")
  , ("\\lsimg",ESymbol Rel "\10895")
  , ("\\lsqhook",ESymbol Rel "\10957")
  , ("\\ltcc",ESymbol Rel "\10918")
  , ("\\ltcir",ESymbol Rel "\10873")
  , ("\\ltlarr",ESymbol Rel "\10614")
  , ("\\ltquest",ESymbol Rel "\10875")
  , ("\\ltrivb",ESymbol Rel "\10703")
  , ("\\lvboxline",ESymbol Ord "\9144")
  , ("\\lvec",ESymbol Accent "\8400")
  , ("\\lvzigzag",ESymbol Open "\10712")
  , ("\\male",ESymbol Ord "\9794")
  , ("\\maltese",ESymbol Ord "\10016")
  , ("\\mappedfrom",ESymbol Rel "\8612")
  , ("\\mapsdown",ESymbol Rel "\8615")
  , ("\\mapsfrom",ESymbol Rel "\8612")
  , ("\\mapsup",ESymbol Rel "\8613")
  , ("\\mathcent",ESymbol Ord "\162")
  , ("\\mathcolon",ESymbol Pun ":")
  , ("\\mathdollar",ESymbol Ord "$")
  , ("\\matheth",ESymbol Alpha "\240")
  , ("\\mathratio",ESymbol Rel "\8758")
  , ("\\mathring",ESymbol Accent "\778")
  , ("\\mathslash",ESymbol Ord "/")
  , ("\\mathsterling",ESymbol Ord "\163")
  , ("\\mbfA",ESymbol Alpha "\119808")
  , ("\\mbfAlpha",ESymbol Alpha "\120488")
  , ("\\mbfB",ESymbol Alpha "\119809")
  , ("\\mbfBeta",ESymbol Alpha "\120489")
  , ("\\mbfC",ESymbol Alpha "\119810")
  , ("\\mbfChi",ESymbol Alpha "\120510")
  , ("\\mbfD",ESymbol Alpha "\119811")
  , ("\\mbfDelta",ESymbol Alpha "\120491")
  , ("\\mbfDigamma",ESymbol Alpha "\120778")
  , ("\\mbfE",ESymbol Alpha "\119812")
  , ("\\mbfEpsilon",ESymbol Alpha "\120492")
  , ("\\mbfEta",ESymbol Alpha "\120494")
  , ("\\mbfF",ESymbol Alpha "\119813")
  , ("\\mbfG",ESymbol Alpha "\119814")
  , ("\\mbfGamma",ESymbol Alpha "\120490")
  , ("\\mbfH",ESymbol Alpha "\119815")
  , ("\\mbfI",ESymbol Alpha "\119816")
  , ("\\mbfIota",ESymbol Alpha "\120496")
  , ("\\mbfJ",ESymbol Alpha "\119817")
  , ("\\mbfK",ESymbol Alpha "\119818")
  , ("\\mbfKappa",ESymbol Alpha "\120497")
  , ("\\mbfL",ESymbol Alpha "\119819")
  , ("\\mbfLambda",ESymbol Alpha "\120498")
  , ("\\mbfM",ESymbol Alpha "\119820")
  , ("\\mbfMu",ESymbol Alpha "\120499")
  , ("\\mbfN",ESymbol Alpha "\119821")
  , ("\\mbfNu",ESymbol Alpha "\120500")
  , ("\\mbfO",ESymbol Alpha "\119822")
  , ("\\mbfOmega",ESymbol Alpha "\120512")
  , ("\\mbfOmicron",ESymbol Alpha "\120502")
  , ("\\mbfP",ESymbol Alpha "\119823")
  , ("\\mbfPhi",ESymbol Alpha "\120509")
  , ("\\mbfPi",ESymbol Alpha "\120503")
  , ("\\mbfPsi",ESymbol Alpha "\120511")
  , ("\\mbfQ",ESymbol Alpha "\119824")
  , ("\\mbfR",ESymbol Alpha "\119825")
  , ("\\mbfRho",ESymbol Alpha "\120504")
  , ("\\mbfS",ESymbol Alpha "\119826")
  , ("\\mbfSigma",ESymbol Alpha "\120506")
  , ("\\mbfT",ESymbol Alpha "\119827")
  , ("\\mbfTau",ESymbol Alpha "\120507")
  , ("\\mbfTheta",ESymbol Alpha "\120495")
  , ("\\mbfU",ESymbol Alpha "\119828")
  , ("\\mbfUpsilon",ESymbol Alpha "\120508")
  , ("\\mbfV",ESymbol Alpha "\119829")
  , ("\\mbfW",ESymbol Alpha "\119830")
  , ("\\mbfX",ESymbol Alpha "\119831")
  , ("\\mbfXi",ESymbol Alpha "\120501")
  , ("\\mbfY",ESymbol Alpha "\119832")
  , ("\\mbfZ",ESymbol Alpha "\119833")
  , ("\\mbfZeta",ESymbol Alpha "\120493")
  , ("\\mbfa",ESymbol Alpha "\119834")
  , ("\\mbfalpha",ESymbol Alpha "\120514")
  , ("\\mbfb",ESymbol Alpha "\119835")
  , ("\\mbfbeta",ESymbol Alpha "\120515")
  , ("\\mbfc",ESymbol Alpha "\119836")
  , ("\\mbfchi",ESymbol Alpha "\120536")
  , ("\\mbfd",ESymbol Alpha "\119837")
  , ("\\mbfdelta",ESymbol Alpha "\120517")
  , ("\\mbfdigamma",ESymbol Alpha "\120779")
  , ("\\mbfe",ESymbol Alpha "\119838")
  , ("\\mbfepsilon",ESymbol Alpha "\120518")
  , ("\\mbfeta",ESymbol Alpha "\120520")
  , ("\\mbff",ESymbol Alpha "\119839")
  , ("\\mbffrakA",ESymbol Alpha "\120172")
  , ("\\mbffrakB",ESymbol Alpha "\120173")
  , ("\\mbffrakC",ESymbol Alpha "\120174")
  , ("\\mbffrakD",ESymbol Alpha "\120175")
  , ("\\mbffrakE",ESymbol Alpha "\120176")
  , ("\\mbffrakF",ESymbol Alpha "\120177")
  , ("\\mbffrakG",ESymbol Alpha "\120178")
  , ("\\mbffrakH",ESymbol Alpha "\120179")
  , ("\\mbffrakI",ESymbol Alpha "\120180")
  , ("\\mbffrakJ",ESymbol Alpha "\120181")
  , ("\\mbffrakK",ESymbol Alpha "\120182")
  , ("\\mbffrakL",ESymbol Alpha "\120183")
  , ("\\mbffrakM",ESymbol Alpha "\120184")
  , ("\\mbffrakN",ESymbol Alpha "\120185")
  , ("\\mbffrakO",ESymbol Alpha "\120186")
  , ("\\mbffrakP",ESymbol Alpha "\120187")
  , ("\\mbffrakQ",ESymbol Alpha "\120188")
  , ("\\mbffrakR",ESymbol Alpha "\120189")
  , ("\\mbffrakS",ESymbol Alpha "\120190")
  , ("\\mbffrakT",ESymbol Alpha "\120191")
  , ("\\mbffrakU",ESymbol Alpha "\120192")
  , ("\\mbffrakV",ESymbol Alpha "\120193")
  , ("\\mbffrakW",ESymbol Alpha "\120194")
  , ("\\mbffrakX",ESymbol Alpha "\120195")
  , ("\\mbffrakY",ESymbol Alpha "\120196")
  , ("\\mbffrakZ",ESymbol Alpha "\120197")
  , ("\\mbffraka",ESymbol Alpha "\120198")
  , ("\\mbffrakb",ESymbol Alpha "\120199")
  , ("\\mbffrakc",ESymbol Alpha "\120200")
  , ("\\mbffrakd",ESymbol Alpha "\120201")
  , ("\\mbffrake",ESymbol Alpha "\120202")
  , ("\\mbffrakf",ESymbol Alpha "\120203")
  , ("\\mbffrakg",ESymbol Alpha "\120204")
  , ("\\mbffrakh",ESymbol Alpha "\120205")
  , ("\\mbffraki",ESymbol Alpha "\120206")
  , ("\\mbffrakj",ESymbol Alpha "\120207")
  , ("\\mbffrakk",ESymbol Alpha "\120208")
  , ("\\mbffrakl",ESymbol Alpha "\120209")
  , ("\\mbffrakm",ESymbol Alpha "\120210")
  , ("\\mbffrakn",ESymbol Alpha "\120211")
  , ("\\mbffrako",ESymbol Alpha "\120212")
  , ("\\mbffrakp",ESymbol Alpha "\120213")
  , ("\\mbffrakq",ESymbol Alpha "\120214")
  , ("\\mbffrakr",ESymbol Alpha "\120215")
  , ("\\mbffraks",ESymbol Alpha "\120216")
  , ("\\mbffrakt",ESymbol Alpha "\120217")
  , ("\\mbffraku",ESymbol Alpha "\120218")
  , ("\\mbffrakv",ESymbol Alpha "\120219")
  , ("\\mbffrakw",ESymbol Alpha "\120220")
  , ("\\mbffrakx",ESymbol Alpha "\120221")
  , ("\\mbffraky",ESymbol Alpha "\120222")
  , ("\\mbffrakz",ESymbol Alpha "\120223")
  , ("\\mbfg",ESymbol Alpha "\119840")
  , ("\\mbfgamma",ESymbol Alpha "\120516")
  , ("\\mbfh",ESymbol Alpha "\119841")
  , ("\\mbfi",ESymbol Alpha "\119842")
  , ("\\mbfiota",ESymbol Alpha "\120522")
  , ("\\mbfitA",ESymbol Alpha "\119912")
  , ("\\mbfitAlpha",ESymbol Alpha "\120604")
  , ("\\mbfitB",ESymbol Alpha "\119913")
  , ("\\mbfitBeta",ESymbol Alpha "\120605")
  , ("\\mbfitC",ESymbol Alpha "\119914")
  , ("\\mbfitChi",ESymbol Alpha "\120626")
  , ("\\mbfitD",ESymbol Alpha "\119915")
  , ("\\mbfitDelta",ESymbol Alpha "\120607")
  , ("\\mbfitE",ESymbol Alpha "\119916")
  , ("\\mbfitEpsilon",ESymbol Alpha "\120608")
  , ("\\mbfitEta",ESymbol Alpha "\120610")
  , ("\\mbfitF",ESymbol Alpha "\119917")
  , ("\\mbfitG",ESymbol Alpha "\119918")
  , ("\\mbfitGamma",ESymbol Alpha "\120606")
  , ("\\mbfitH",ESymbol Alpha "\119919")
  , ("\\mbfitI",ESymbol Alpha "\119920")
  , ("\\mbfitIota",ESymbol Alpha "\120612")
  , ("\\mbfitJ",ESymbol Alpha "\119921")
  , ("\\mbfitK",ESymbol Alpha "\119922")
  , ("\\mbfitKappa",ESymbol Alpha "\120613")
  , ("\\mbfitL",ESymbol Alpha "\119923")
  , ("\\mbfitLambda",ESymbol Alpha "\120614")
  , ("\\mbfitM",ESymbol Alpha "\119924")
  , ("\\mbfitMu",ESymbol Alpha "\120615")
  , ("\\mbfitN",ESymbol Alpha "\119925")
  , ("\\mbfitNu",ESymbol Alpha "\120616")
  , ("\\mbfitO",ESymbol Alpha "\119926")
  , ("\\mbfitOmega",ESymbol Alpha "\120628")
  , ("\\mbfitOmicron",ESymbol Alpha "\120618")
  , ("\\mbfitP",ESymbol Alpha "\119927")
  , ("\\mbfitPhi",ESymbol Alpha "\120625")
  , ("\\mbfitPi",ESymbol Alpha "\120619")
  , ("\\mbfitPsi",ESymbol Alpha "\120627")
  , ("\\mbfitQ",ESymbol Alpha "\119928")
  , ("\\mbfitR",ESymbol Alpha "\119929")
  , ("\\mbfitRho",ESymbol Alpha "\120620")
  , ("\\mbfitS",ESymbol Alpha "\119930")
  , ("\\mbfitSigma",ESymbol Alpha "\120622")
  , ("\\mbfitT",ESymbol Alpha "\119931")
  , ("\\mbfitTau",ESymbol Alpha "\120623")
  , ("\\mbfitTheta",ESymbol Alpha "\120611")
  , ("\\mbfitU",ESymbol Alpha "\119932")
  , ("\\mbfitUpsilon",ESymbol Alpha "\120624")
  , ("\\mbfitV",ESymbol Alpha "\119933")
  , ("\\mbfitW",ESymbol Alpha "\119934")
  , ("\\mbfitX",ESymbol Alpha "\119935")
  , ("\\mbfitXi",ESymbol Alpha "\120617")
  , ("\\mbfitY",ESymbol Alpha "\119936")
  , ("\\mbfitZ",ESymbol Alpha "\119937")
  , ("\\mbfitZeta",ESymbol Alpha "\120609")
  , ("\\mbfita",ESymbol Alpha "\119938")
  , ("\\mbfitalpha",ESymbol Alpha "\120630")
  , ("\\mbfitb",ESymbol Alpha "\119939")
  , ("\\mbfitbeta",ESymbol Alpha "\120631")
  , ("\\mbfitc",ESymbol Alpha "\119940")
  , ("\\mbfitchi",ESymbol Alpha "\120652")
  , ("\\mbfitd",ESymbol Alpha "\119941")
  , ("\\mbfitdelta",ESymbol Alpha "\120633")
  , ("\\mbfite",ESymbol Alpha "\119942")
  , ("\\mbfitepsilon",ESymbol Alpha "\120634")
  , ("\\mbfiteta",ESymbol Alpha "\120636")
  , ("\\mbfitf",ESymbol Alpha "\119943")
  , ("\\mbfitg",ESymbol Alpha "\119944")
  , ("\\mbfitgamma",ESymbol Alpha "\120632")
  , ("\\mbfith",ESymbol Alpha "\119945")
  , ("\\mbfiti",ESymbol Alpha "\119946")
  , ("\\mbfitiota",ESymbol Alpha "\120638")
  , ("\\mbfitj",ESymbol Alpha "\119947")
  , ("\\mbfitk",ESymbol Alpha "\119948")
  , ("\\mbfitkappa",ESymbol Alpha "\120639")
  , ("\\mbfitl",ESymbol Alpha "\119949")
  , ("\\mbfitlambda",ESymbol Alpha "\120640")
  , ("\\mbfitm",ESymbol Alpha "\119950")
  , ("\\mbfitmu",ESymbol Alpha "\120641")
  , ("\\mbfitn",ESymbol Alpha "\119951")
  , ("\\mbfitnabla",ESymbol Ord "\120629")
  , ("\\mbfitnu",ESymbol Alpha "\120642")
  , ("\\mbfito",ESymbol Alpha "\119952")
  , ("\\mbfitomega",ESymbol Alpha "\120654")
  , ("\\mbfitomicron",ESymbol Alpha "\120644")
  , ("\\mbfitp",ESymbol Alpha "\119953")
  , ("\\mbfitpartial",ESymbol Ord "\120655")
  , ("\\mbfitphi",ESymbol Alpha "\120651")
  , ("\\mbfitpi",ESymbol Alpha "\120645")
  , ("\\mbfitpsi",ESymbol Alpha "\120653")
  , ("\\mbfitq",ESymbol Alpha "\119954")
  , ("\\mbfitr",ESymbol Alpha "\119955")
  , ("\\mbfitrho",ESymbol Alpha "\120646")
  , ("\\mbfits",ESymbol Alpha "\119956")
  , ("\\mbfitsansA",ESymbol Alpha "\120380")
  , ("\\mbfitsansAlpha",ESymbol Alpha "\120720")
  , ("\\mbfitsansB",ESymbol Alpha "\120381")
  , ("\\mbfitsansBeta",ESymbol Alpha "\120721")
  , ("\\mbfitsansC",ESymbol Alpha "\120382")
  , ("\\mbfitsansChi",ESymbol Alpha "\120742")
  , ("\\mbfitsansD",ESymbol Alpha "\120383")
  , ("\\mbfitsansDelta",ESymbol Alpha "\120723")
  , ("\\mbfitsansE",ESymbol Alpha "\120384")
  , ("\\mbfitsansEpsilon",ESymbol Alpha "\120724")
  , ("\\mbfitsansEta",ESymbol Alpha "\120726")
  , ("\\mbfitsansF",ESymbol Alpha "\120385")
  , ("\\mbfitsansG",ESymbol Alpha "\120386")
  , ("\\mbfitsansGamma",ESymbol Alpha "\120722")
  , ("\\mbfitsansH",ESymbol Alpha "\120387")
  , ("\\mbfitsansI",ESymbol Alpha "\120388")
  , ("\\mbfitsansIota",ESymbol Alpha "\120728")
  , ("\\mbfitsansJ",ESymbol Alpha "\120389")
  , ("\\mbfitsansK",ESymbol Alpha "\120390")
  , ("\\mbfitsansKappa",ESymbol Alpha "\120729")
  , ("\\mbfitsansL",ESymbol Alpha "\120391")
  , ("\\mbfitsansLambda",ESymbol Alpha "\120730")
  , ("\\mbfitsansM",ESymbol Alpha "\120392")
  , ("\\mbfitsansMu",ESymbol Alpha "\120731")
  , ("\\mbfitsansN",ESymbol Alpha "\120393")
  , ("\\mbfitsansNu",ESymbol Alpha "\120732")
  , ("\\mbfitsansO",ESymbol Alpha "\120394")
  , ("\\mbfitsansOmega",ESymbol Alpha "\120744")
  , ("\\mbfitsansOmicron",ESymbol Alpha "\120734")
  , ("\\mbfitsansP",ESymbol Alpha "\120395")
  , ("\\mbfitsansPhi",ESymbol Alpha "\120741")
  , ("\\mbfitsansPi",ESymbol Alpha "\120735")
  , ("\\mbfitsansPsi",ESymbol Alpha "\120743")
  , ("\\mbfitsansQ",ESymbol Alpha "\120396")
  , ("\\mbfitsansR",ESymbol Alpha "\120397")
  , ("\\mbfitsansRho",ESymbol Alpha "\120736")
  , ("\\mbfitsansS",ESymbol Alpha "\120398")
  , ("\\mbfitsansSigma",ESymbol Alpha "\120738")
  , ("\\mbfitsansT",ESymbol Alpha "\120399")
  , ("\\mbfitsansTau",ESymbol Alpha "\120739")
  , ("\\mbfitsansTheta",ESymbol Alpha "\120727")
  , ("\\mbfitsansU",ESymbol Alpha "\120400")
  , ("\\mbfitsansUpsilon",ESymbol Alpha "\120740")
  , ("\\mbfitsansV",ESymbol Alpha "\120401")
  , ("\\mbfitsansW",ESymbol Alpha "\120402")
  , ("\\mbfitsansX",ESymbol Alpha "\120403")
  , ("\\mbfitsansXi",ESymbol Alpha "\120733")
  , ("\\mbfitsansY",ESymbol Alpha "\120404")
  , ("\\mbfitsansZ",ESymbol Alpha "\120405")
  , ("\\mbfitsansZeta",ESymbol Alpha "\120725")
  , ("\\mbfitsansa",ESymbol Alpha "\120406")
  , ("\\mbfitsansalpha",ESymbol Alpha "\120746")
  , ("\\mbfitsansb",ESymbol Alpha "\120407")
  , ("\\mbfitsansbeta",ESymbol Alpha "\120747")
  , ("\\mbfitsansc",ESymbol Alpha "\120408")
  , ("\\mbfitsanschi",ESymbol Alpha "\120768")
  , ("\\mbfitsansd",ESymbol Alpha "\120409")
  , ("\\mbfitsansdelta",ESymbol Alpha "\120749")
  , ("\\mbfitsanse",ESymbol Alpha "\120410")
  , ("\\mbfitsansepsilon",ESymbol Alpha "\120750")
  , ("\\mbfitsanseta",ESymbol Alpha "\120752")
  , ("\\mbfitsansf",ESymbol Alpha "\120411")
  , ("\\mbfitsansg",ESymbol Alpha "\120412")
  , ("\\mbfitsansgamma",ESymbol Alpha "\120748")
  , ("\\mbfitsansh",ESymbol Alpha "\120413")
  , ("\\mbfitsansi",ESymbol Alpha "\120414")
  , ("\\mbfitsansiota",ESymbol Alpha "\120754")
  , ("\\mbfitsansj",ESymbol Alpha "\120415")
  , ("\\mbfitsansk",ESymbol Alpha "\120416")
  , ("\\mbfitsanskappa",ESymbol Alpha "\120755")
  , ("\\mbfitsansl",ESymbol Alpha "\120417")
  , ("\\mbfitsanslambda",ESymbol Alpha "\120756")
  , ("\\mbfitsansm",ESymbol Alpha "\120418")
  , ("\\mbfitsansmu",ESymbol Alpha "\120757")
  , ("\\mbfitsansn",ESymbol Alpha "\120419")
  , ("\\mbfitsansnabla",ESymbol Ord "\120745")
  , ("\\mbfitsansnu",ESymbol Alpha "\120758")
  , ("\\mbfitsanso",ESymbol Alpha "\120420")
  , ("\\mbfitsansomega",ESymbol Alpha "\120770")
  , ("\\mbfitsansomicron",ESymbol Alpha "\120760")
  , ("\\mbfitsansp",ESymbol Alpha "\120421")
  , ("\\mbfitsanspartial",ESymbol Ord "\120771")
  , ("\\mbfitsansphi",ESymbol Alpha "\120767")
  , ("\\mbfitsanspi",ESymbol Alpha "\120761")
  , ("\\mbfitsanspsi",ESymbol Alpha "\120769")
  , ("\\mbfitsansq",ESymbol Alpha "\120422")
  , ("\\mbfitsansr",ESymbol Alpha "\120423")
  , ("\\mbfitsansrho",ESymbol Alpha "\120762")
  , ("\\mbfitsanss",ESymbol Alpha "\120424")
  , ("\\mbfitsanssigma",ESymbol Alpha "\120764")
  , ("\\mbfitsanst",ESymbol Alpha "\120425")
  , ("\\mbfitsanstau",ESymbol Alpha "\120765")
  , ("\\mbfitsanstheta",ESymbol Alpha "\120753")
  , ("\\mbfitsansu",ESymbol Alpha "\120426")
  , ("\\mbfitsansupsilon",ESymbol Alpha "\120766")
  , ("\\mbfitsansv",ESymbol Alpha "\120427")
  , ("\\mbfitsansvarTheta",ESymbol Alpha "\120737")
  , ("\\mbfitsansvarepsilon",ESymbol Alpha "\120772")
  , ("\\mbfitsansvarkappa",ESymbol Alpha "\120774")
  , ("\\mbfitsansvarphi",ESymbol Alpha "\120775")
  , ("\\mbfitsansvarpi",ESymbol Alpha "\120777")
  , ("\\mbfitsansvarrho",ESymbol Alpha "\120776")
  , ("\\mbfitsansvarsigma",ESymbol Alpha "\120763")
  , ("\\mbfitsansvartheta",ESymbol Alpha "\120773")
  , ("\\mbfitsansw",ESymbol Alpha "\120428")
  , ("\\mbfitsansx",ESymbol Alpha "\120429")
  , ("\\mbfitsansxi",ESymbol Alpha "\120759")
  , ("\\mbfitsansy",ESymbol Alpha "\120430")
  , ("\\mbfitsansz",ESymbol Alpha "\120431")
  , ("\\mbfitsanszeta",ESymbol Alpha "\120751")
  , ("\\mbfitsigma",ESymbol Alpha "\120648")
  , ("\\mbfitt",ESymbol Alpha "\119957")
  , ("\\mbfittau",ESymbol Alpha "\120649")
  , ("\\mbfittheta",ESymbol Alpha "\120637")
  , ("\\mbfitu",ESymbol Alpha "\119958")
  , ("\\mbfitupsilon",ESymbol Alpha "\120650")
  , ("\\mbfitv",ESymbol Alpha "\119959")
  , ("\\mbfitvarTheta",ESymbol Alpha "\120621")
  , ("\\mbfitvarepsilon",ESymbol Alpha "\120656")
  , ("\\mbfitvarkappa",ESymbol Alpha "\120658")
  , ("\\mbfitvarphi",ESymbol Alpha "\120659")
  , ("\\mbfitvarpi",ESymbol Alpha "\120661")
  , ("\\mbfitvarrho",ESymbol Alpha "\120660")
  , ("\\mbfitvarsigma",ESymbol Alpha "\120647")
  , ("\\mbfitvartheta",ESymbol Alpha "\120657")
  , ("\\mbfitw",ESymbol Alpha "\119960")
  , ("\\mbfitx",ESymbol Alpha "\119961")
  , ("\\mbfitxi",ESymbol Alpha "\120643")
  , ("\\mbfity",ESymbol Alpha "\119962")
  , ("\\mbfitz",ESymbol Alpha "\119963")
  , ("\\mbfitzeta",ESymbol Alpha "\120635")
  , ("\\mbfj",ESymbol Alpha "\119843")
  , ("\\mbfk",ESymbol Alpha "\119844")
  , ("\\mbfkappa",ESymbol Alpha "\120523")
  , ("\\mbfl",ESymbol Alpha "\119845")
  , ("\\mbflambda",ESymbol Alpha "\120524")
  , ("\\mbfm",ESymbol Alpha "\119846")
  , ("\\mbfmu",ESymbol Alpha "\120525")
  , ("\\mbfn",ESymbol Alpha "\119847")
  , ("\\mbfnabla",ESymbol Ord "\120513")
  , ("\\mbfnu",ESymbol Alpha "\120526")
  , ("\\mbfo",ESymbol Alpha "\119848")
  , ("\\mbfomega",ESymbol Alpha "\120538")
  , ("\\mbfomicron",ESymbol Alpha "\120528")
  , ("\\mbfp",ESymbol Alpha "\119849")
  , ("\\mbfpartial",ESymbol Ord "\120539")
  , ("\\mbfphi",ESymbol Alpha "\120543")
  , ("\\mbfpi",ESymbol Alpha "\120529")
  , ("\\mbfpsi",ESymbol Alpha "\120537")
  , ("\\mbfq",ESymbol Alpha "\119850")
  , ("\\mbfr",ESymbol Alpha "\119851")
  , ("\\mbfrho",ESymbol Alpha "\120530")
  , ("\\mbfs",ESymbol Alpha "\119852")
  , ("\\mbfsansA",ESymbol Alpha "\120276")
  , ("\\mbfsansAlpha",ESymbol Alpha "\120662")
  , ("\\mbfsansB",ESymbol Alpha "\120277")
  , ("\\mbfsansBeta",ESymbol Alpha "\120663")
  , ("\\mbfsansC",ESymbol Alpha "\120278")
  , ("\\mbfsansChi",ESymbol Alpha "\120684")
  , ("\\mbfsansD",ESymbol Alpha "\120279")
  , ("\\mbfsansDelta",ESymbol Alpha "\120665")
  , ("\\mbfsansE",ESymbol Alpha "\120280")
  , ("\\mbfsansEpsilon",ESymbol Alpha "\120666")
  , ("\\mbfsansEta",ESymbol Alpha "\120668")
  , ("\\mbfsansF",ESymbol Alpha "\120281")
  , ("\\mbfsansG",ESymbol Alpha "\120282")
  , ("\\mbfsansGamma",ESymbol Alpha "\120664")
  , ("\\mbfsansH",ESymbol Alpha "\120283")
  , ("\\mbfsansI",ESymbol Alpha "\120284")
  , ("\\mbfsansIota",ESymbol Alpha "\120670")
  , ("\\mbfsansJ",ESymbol Alpha "\120285")
  , ("\\mbfsansK",ESymbol Alpha "\120286")
  , ("\\mbfsansKappa",ESymbol Alpha "\120671")
  , ("\\mbfsansL",ESymbol Alpha "\120287")
  , ("\\mbfsansLambda",ESymbol Alpha "\120672")
  , ("\\mbfsansM",ESymbol Alpha "\120288")
  , ("\\mbfsansMu",ESymbol Alpha "\120673")
  , ("\\mbfsansN",ESymbol Alpha "\120289")
  , ("\\mbfsansNu",ESymbol Alpha "\120674")
  , ("\\mbfsansO",ESymbol Alpha "\120290")
  , ("\\mbfsansOmega",ESymbol Alpha "\120686")
  , ("\\mbfsansOmicron",ESymbol Alpha "\120676")
  , ("\\mbfsansP",ESymbol Alpha "\120291")
  , ("\\mbfsansPhi",ESymbol Alpha "\120683")
  , ("\\mbfsansPi",ESymbol Alpha "\120677")
  , ("\\mbfsansPsi",ESymbol Alpha "\120685")
  , ("\\mbfsansQ",ESymbol Alpha "\120292")
  , ("\\mbfsansR",ESymbol Alpha "\120293")
  , ("\\mbfsansRho",ESymbol Alpha "\120678")
  , ("\\mbfsansS",ESymbol Alpha "\120294")
  , ("\\mbfsansSigma",ESymbol Alpha "\120680")
  , ("\\mbfsansT",ESymbol Alpha "\120295")
  , ("\\mbfsansTau",ESymbol Alpha "\120681")
  , ("\\mbfsansTheta",ESymbol Alpha "\120669")
  , ("\\mbfsansU",ESymbol Alpha "\120296")
  , ("\\mbfsansUpsilon",ESymbol Alpha "\120682")
  , ("\\mbfsansV",ESymbol Alpha "\120297")
  , ("\\mbfsansW",ESymbol Alpha "\120298")
  , ("\\mbfsansX",ESymbol Alpha "\120299")
  , ("\\mbfsansXi",ESymbol Alpha "\120675")
  , ("\\mbfsansY",ESymbol Alpha "\120300")
  , ("\\mbfsansZ",ESymbol Alpha "\120301")
  , ("\\mbfsansZeta",ESymbol Alpha "\120667")
  , ("\\mbfsansa",ESymbol Alpha "\120302")
  , ("\\mbfsansalpha",ESymbol Alpha "\120688")
  , ("\\mbfsansb",ESymbol Alpha "\120303")
  , ("\\mbfsansbeta",ESymbol Alpha "\120689")
  , ("\\mbfsansc",ESymbol Alpha "\120304")
  , ("\\mbfsanschi",ESymbol Alpha "\120710")
  , ("\\mbfsansd",ESymbol Alpha "\120305")
  , ("\\mbfsansdelta",ESymbol Alpha "\120691")
  , ("\\mbfsanse",ESymbol Alpha "\120306")
  , ("\\mbfsanseight",ESymbol Ord "\120820")
  , ("\\mbfsansepsilon",ESymbol Alpha "\120692")
  , ("\\mbfsanseta",ESymbol Alpha "\120694")
  , ("\\mbfsansf",ESymbol Alpha "\120307")
  , ("\\mbfsansfive",ESymbol Ord "\120817")
  , ("\\mbfsansfour",ESymbol Ord "\120816")
  , ("\\mbfsansg",ESymbol Alpha "\120308")
  , ("\\mbfsansgamma",ESymbol Alpha "\120690")
  , ("\\mbfsansh",ESymbol Alpha "\120309")
  , ("\\mbfsansi",ESymbol Alpha "\120310")
  , ("\\mbfsansiota",ESymbol Alpha "\120696")
  , ("\\mbfsansj",ESymbol Alpha "\120311")
  , ("\\mbfsansk",ESymbol Alpha "\120312")
  , ("\\mbfsanskappa",ESymbol Alpha "\120697")
  , ("\\mbfsansl",ESymbol Alpha "\120313")
  , ("\\mbfsanslambda",ESymbol Alpha "\120698")
  , ("\\mbfsansm",ESymbol Alpha "\120314")
  , ("\\mbfsansmu",ESymbol Alpha "\120699")
  , ("\\mbfsansn",ESymbol Alpha "\120315")
  , ("\\mbfsansnabla",ESymbol Ord "\120687")
  , ("\\mbfsansnine",ESymbol Ord "\120821")
  , ("\\mbfsansnu",ESymbol Alpha "\120700")
  , ("\\mbfsanso",ESymbol Alpha "\120316")
  , ("\\mbfsansomega",ESymbol Alpha "\120712")
  , ("\\mbfsansomicron",ESymbol Alpha "\120702")
  , ("\\mbfsansone",ESymbol Ord "\120813")
  , ("\\mbfsansp",ESymbol Alpha "\120317")
  , ("\\mbfsanspartial",ESymbol Ord "\120713")
  , ("\\mbfsansphi",ESymbol Alpha "\120709")
  , ("\\mbfsanspi",ESymbol Alpha "\120703")
  , ("\\mbfsanspsi",ESymbol Alpha "\120711")
  , ("\\mbfsansq",ESymbol Alpha "\120318")
  , ("\\mbfsansr",ESymbol Alpha "\120319")
  , ("\\mbfsansrho",ESymbol Alpha "\120704")
  , ("\\mbfsanss",ESymbol Alpha "\120320")
  , ("\\mbfsansseven",ESymbol Ord "\120819")
  , ("\\mbfsanssigma",ESymbol Alpha "\120706")
  , ("\\mbfsanssix",ESymbol Ord "\120818")
  , ("\\mbfsanst",ESymbol Alpha "\120321")
  , ("\\mbfsanstau",ESymbol Alpha "\120707")
  , ("\\mbfsanstheta",ESymbol Alpha "\120695")
  , ("\\mbfsansthree",ESymbol Ord "\120815")
  , ("\\mbfsanstwo",ESymbol Ord "\120814")
  , ("\\mbfsansu",ESymbol Alpha "\120322")
  , ("\\mbfsansupsilon",ESymbol Alpha "\120708")
  , ("\\mbfsansv",ESymbol Alpha "\120323")
  , ("\\mbfsansvarTheta",ESymbol Alpha "\120679")
  , ("\\mbfsansvarepsilon",ESymbol Alpha "\120714")
  , ("\\mbfsansvarkappa",ESymbol Alpha "\120716")
  , ("\\mbfsansvarphi",ESymbol Alpha "\120717")
  , ("\\mbfsansvarpi",ESymbol Alpha "\120719")
  , ("\\mbfsansvarrho",ESymbol Alpha "\120718")
  , ("\\mbfsansvarsigma",ESymbol Alpha "\120705")
  , ("\\mbfsansvartheta",ESymbol Alpha "\120715")
  , ("\\mbfsansw",ESymbol Alpha "\120324")
  , ("\\mbfsansx",ESymbol Alpha "\120325")
  , ("\\mbfsansxi",ESymbol Alpha "\120701")
  , ("\\mbfsansy",ESymbol Alpha "\120326")
  , ("\\mbfsansz",ESymbol Alpha "\120327")
  , ("\\mbfsanszero",ESymbol Ord "\120812")
  , ("\\mbfsanszeta",ESymbol Alpha "\120693")
  , ("\\mbfscrA",ESymbol Alpha "\120016")
  , ("\\mbfscrB",ESymbol Alpha "\120017")
  , ("\\mbfscrC",ESymbol Alpha "\120018")
  , ("\\mbfscrD",ESymbol Alpha "\120019")
  , ("\\mbfscrE",ESymbol Alpha "\120020")
  , ("\\mbfscrF",ESymbol Alpha "\120021")
  , ("\\mbfscrG",ESymbol Alpha "\120022")
  , ("\\mbfscrH",ESymbol Alpha "\120023")
  , ("\\mbfscrI",ESymbol Alpha "\120024")
  , ("\\mbfscrJ",ESymbol Alpha "\120025")
  , ("\\mbfscrK",ESymbol Alpha "\120026")
  , ("\\mbfscrL",ESymbol Alpha "\120027")
  , ("\\mbfscrM",ESymbol Alpha "\120028")
  , ("\\mbfscrN",ESymbol Alpha "\120029")
  , ("\\mbfscrO",ESymbol Alpha "\120030")
  , ("\\mbfscrP",ESymbol Alpha "\120031")
  , ("\\mbfscrQ",ESymbol Alpha "\120032")
  , ("\\mbfscrR",ESymbol Alpha "\120033")
  , ("\\mbfscrS",ESymbol Alpha "\120034")
  , ("\\mbfscrT",ESymbol Alpha "\120035")
  , ("\\mbfscrU",ESymbol Alpha "\120036")
  , ("\\mbfscrV",ESymbol Alpha "\120037")
  , ("\\mbfscrW",ESymbol Alpha "\120038")
  , ("\\mbfscrX",ESymbol Alpha "\120039")
  , ("\\mbfscrY",ESymbol Alpha "\120040")
  , ("\\mbfscrZ",ESymbol Alpha "\120041")
  , ("\\mbfscra",ESymbol Alpha "\120042")
  , ("\\mbfscrb",ESymbol Alpha "\120043")
  , ("\\mbfscrc",ESymbol Alpha "\120044")
  , ("\\mbfscrd",ESymbol Alpha "\120045")
  , ("\\mbfscre",ESymbol Alpha "\120046")
  , ("\\mbfscrf",ESymbol Alpha "\120047")
  , ("\\mbfscrg",ESymbol Alpha "\120048")
  , ("\\mbfscrh",ESymbol Alpha "\120049")
  , ("\\mbfscri",ESymbol Alpha "\120050")
  , ("\\mbfscrj",ESymbol Alpha "\120051")
  , ("\\mbfscrk",ESymbol Alpha "\120052")
  , ("\\mbfscrl",ESymbol Alpha "\120053")
  , ("\\mbfscrm",ESymbol Alpha "\120054")
  , ("\\mbfscrn",ESymbol Alpha "\120055")
  , ("\\mbfscro",ESymbol Alpha "\120056")
  , ("\\mbfscrp",ESymbol Alpha "\120057")
  , ("\\mbfscrq",ESymbol Alpha "\120058")
  , ("\\mbfscrr",ESymbol Alpha "\120059")
  , ("\\mbfscrs",ESymbol Alpha "\120060")
  , ("\\mbfscrt",ESymbol Alpha "\120061")
  , ("\\mbfscru",ESymbol Alpha "\120062")
  , ("\\mbfscrv",ESymbol Alpha "\120063")
  , ("\\mbfscrw",ESymbol Alpha "\120064")
  , ("\\mbfscrx",ESymbol Alpha "\120065")
  , ("\\mbfscry",ESymbol Alpha "\120066")
  , ("\\mbfscrz",ESymbol Alpha "\120067")
  , ("\\mbfsigma",ESymbol Alpha "\120532")
  , ("\\mbft",ESymbol Alpha "\119853")
  , ("\\mbftau",ESymbol Alpha "\120533")
  , ("\\mbftheta",ESymbol Alpha "\120521")
  , ("\\mbfu",ESymbol Alpha "\119854")
  , ("\\mbfupsilon",ESymbol Alpha "\120534")
  , ("\\mbfv",ESymbol Alpha "\119855")
  , ("\\mbfvarTheta",ESymbol Alpha "\120505")
  , ("\\mbfvarepsilon",ESymbol Alpha "\120540")
  , ("\\mbfvarkappa",ESymbol Alpha "\120542")
  , ("\\mbfvarphi",ESymbol Alpha "\120535")
  , ("\\mbfvarpi",ESymbol Alpha "\120545")
  , ("\\mbfvarrho",ESymbol Alpha "\120544")
  , ("\\mbfvarsigma",ESymbol Alpha "\120531")
  , ("\\mbfvartheta",ESymbol Alpha "\120541")
  , ("\\mbfw",ESymbol Alpha "\119856")
  , ("\\mbfx",ESymbol Alpha "\119857")
  , ("\\mbfxi",ESymbol Alpha "\120527")
  , ("\\mbfy",ESymbol Alpha "\119858")
  , ("\\mbfz",ESymbol Alpha "\119859")
  , ("\\mbfzeta",ESymbol Alpha "\120519")
  , ("\\mdblkcircle",ESymbol Ord "\9899")
  , ("\\mdblkdiamond",ESymbol Ord "\11045")
  , ("\\mdblklozenge",ESymbol Ord "\11047")
  , ("\\mdblksquare",ESymbol Ord "\9724")
  , ("\\mdlgblkcircle",ESymbol Ord "\9679")
  , ("\\mdlgblkdiamond",ESymbol Ord "\9670")
  , ("\\mdlgblklozenge",ESymbol Bin "\10731")
  , ("\\mdlgblksquare",ESymbol Ord "\9632")
  , ("\\mdlgwhtcircle",ESymbol Bin "\9675")
  , ("\\mdlgwhtdiamond",ESymbol Ord "\9671")
  , ("\\mdlgwhtlozenge",ESymbol Ord "\9674")
  , ("\\mdlgwhtsquare",ESymbol Ord "\9633")
  , ("\\mdsmblkcircle",ESymbol Ord "\10625")
  , ("\\mdsmblksquare",ESymbol Ord "\9726")
  , ("\\mdsmwhtcircle",ESymbol Ord "\9900")
  , ("\\mdsmwhtsquare",ESymbol Ord "\9725")
  , ("\\mdwhtcircle",ESymbol Ord "\9898")
  , ("\\mdwhtdiamond",ESymbol Ord "\11046")
  , ("\\mdwhtlozenge",ESymbol Ord "\11048")
  , ("\\mdwhtsquare",ESymbol Ord "\9723")
  , ("\\measangledltosw",ESymbol Ord "\10671")
  , ("\\measangledrtose",ESymbol Ord "\10670")
  , ("\\measangleldtosw",ESymbol Ord "\10667")
  , ("\\measanglelutonw",ESymbol Ord "\10665")
  , ("\\measanglerdtose",ESymbol Ord "\10666")
  , ("\\measanglerutone",ESymbol Ord "\10664")
  , ("\\measangleultonw",ESymbol Ord "\10669")
  , ("\\measangleurtone",ESymbol Ord "\10668")
  , ("\\measeq",ESymbol Rel "\8798")
  , ("\\measuredangle",ESymbol Ord "\8737")
  , ("\\measuredangleleft",ESymbol Ord "\10651")
  , ("\\measuredrightangle",ESymbol Ord "\8894")
  , ("\\medblackstar",ESymbol Ord "\11089")
  , ("\\medbullet",ESymbol Ord "\9899")
  , ("\\medcirc",ESymbol Ord "\9898")
  , ("\\medspace",ESymbol Ord "\8287")
  , ("\\medwhitestar",ESymbol Ord "\11088")
  , ("\\mercury",ESymbol Ord "\9791")
  , ("\\mfrakA",ESymbol Alpha "\120068")
  , ("\\mfrakB",ESymbol Alpha "\120069")
  , ("\\mfrakC",ESymbol Alpha "\8493")
  , ("\\mfrakD",ESymbol Alpha "\120071")
  , ("\\mfrakE",ESymbol Alpha "\120072")
  , ("\\mfrakF",ESymbol Alpha "\120073")
  , ("\\mfrakG",ESymbol Alpha "\120074")
  , ("\\mfrakH",ESymbol Alpha "\8460")
  , ("\\mfrakJ",ESymbol Alpha "\120077")
  , ("\\mfrakK",ESymbol Alpha "\120078")
  , ("\\mfrakL",ESymbol Alpha "\120079")
  , ("\\mfrakM",ESymbol Alpha "\120080")
  , ("\\mfrakN",ESymbol Alpha "\120081")
  , ("\\mfrakO",ESymbol Alpha "\120082")
  , ("\\mfrakP",ESymbol Alpha "\120083")
  , ("\\mfrakQ",ESymbol Alpha "\120084")
  , ("\\mfrakS",ESymbol Alpha "\120086")
  , ("\\mfrakT",ESymbol Alpha "\120087")
  , ("\\mfrakU",ESymbol Alpha "\120088")
  , ("\\mfrakV",ESymbol Alpha "\120089")
  , ("\\mfrakW",ESymbol Alpha "\120090")
  , ("\\mfrakX",ESymbol Alpha "\120091")
  , ("\\mfrakY",ESymbol Alpha "\120092")
  , ("\\mfrakZ",ESymbol Alpha "\8488")
  , ("\\mfraka",ESymbol Alpha "\120094")
  , ("\\mfrakb",ESymbol Alpha "\120095")
  , ("\\mfrakc",ESymbol Alpha "\120096")
  , ("\\mfrakd",ESymbol Alpha "\120097")
  , ("\\mfrake",ESymbol Alpha "\120098")
  , ("\\mfrakf",ESymbol Alpha "\120099")
  , ("\\mfrakg",ESymbol Alpha "\120100")
  , ("\\mfrakh",ESymbol Alpha "\120101")
  , ("\\mfraki",ESymbol Alpha "\120102")
  , ("\\mfrakj",ESymbol Alpha "\120103")
  , ("\\mfrakk",ESymbol Alpha "\120104")
  , ("\\mfrakl",ESymbol Alpha "\120105")
  , ("\\mfrakm",ESymbol Alpha "\120106")
  , ("\\mfrakn",ESymbol Alpha "\120107")
  , ("\\mfrako",ESymbol Alpha "\120108")
  , ("\\mfrakp",ESymbol Alpha "\120109")
  , ("\\mfrakq",ESymbol Alpha "\120110")
  , ("\\mfrakr",ESymbol Alpha "\120111")
  , ("\\mfraks",ESymbol Alpha "\120112")
  , ("\\mfrakt",ESymbol Alpha "\120113")
  , ("\\mfraku",ESymbol Alpha "\120114")
  , ("\\mfrakv",ESymbol Alpha "\120115")
  , ("\\mfrakw",ESymbol Alpha "\120116")
  , ("\\mfrakx",ESymbol Alpha "\120117")
  , ("\\mfraky",ESymbol Alpha "\120118")
  , ("\\mfrakz",ESymbol Alpha "\120119")
  , ("\\mho",ESymbol Ord "\8487")
  , ("\\midbarvee",ESymbol Bin "\10845")
  , ("\\midbarwedge",ESymbol Bin "\10844")
  , ("\\midcir",ESymbol Rel "\10992")
  , ("\\minus",ESymbol Bin "\8722")
  , ("\\minusdot",ESymbol Bin "\10794")
  , ("\\minusfdots",ESymbol Bin "\10795")
  , ("\\minusrdots",ESymbol Bin "\10796")
  , ("\\mitA",ESymbol Alpha "\119860")
  , ("\\mitAlpha",ESymbol Alpha "\120546")
  , ("\\mitB",ESymbol Alpha "\119861")
  , ("\\mitBbbD",ESymbol Ord "\8517")
  , ("\\mitBbbd",ESymbol Ord "\8518")
  , ("\\mitBbbe",ESymbol Ord "\8519")
  , ("\\mitBbbi",ESymbol Ord "\8520")
  , ("\\mitBbbj",ESymbol Ord "\8521")
  , ("\\mitBeta",ESymbol Alpha "\120547")
  , ("\\mitC",ESymbol Alpha "\119862")
  , ("\\mitChi",ESymbol Alpha "\120568")
  , ("\\mitD",ESymbol Alpha "\119863")
  , ("\\mitDelta",ESymbol Alpha "\120549")
  , ("\\mitE",ESymbol Alpha "\119864")
  , ("\\mitEpsilon",ESymbol Alpha "\120550")
  , ("\\mitEta",ESymbol Alpha "\120552")
  , ("\\mitF",ESymbol Alpha "\119865")
  , ("\\mitG",ESymbol Alpha "\119866")
  , ("\\mitGamma",ESymbol Alpha "\120548")
  , ("\\mitH",ESymbol Alpha "\119867")
  , ("\\mitI",ESymbol Alpha "\119868")
  , ("\\mitIota",ESymbol Alpha "\120554")
  , ("\\mitJ",ESymbol Alpha "\119869")
  , ("\\mitK",ESymbol Alpha "\119870")
  , ("\\mitKappa",ESymbol Alpha "\120555")
  , ("\\mitL",ESymbol Alpha "\119871")
  , ("\\mitLambda",ESymbol Alpha "\120556")
  , ("\\mitM",ESymbol Alpha "\119872")
  , ("\\mitMu",ESymbol Alpha "\120557")
  , ("\\mitN",ESymbol Alpha "\119873")
  , ("\\mitNu",ESymbol Alpha "\120558")
  , ("\\mitO",ESymbol Alpha "\119874")
  , ("\\mitOmega",ESymbol Alpha "\120570")
  , ("\\mitOmicron",ESymbol Alpha "\120560")
  , ("\\mitP",ESymbol Alpha "\119875")
  , ("\\mitPhi",ESymbol Alpha "\120567")
  , ("\\mitPi",ESymbol Alpha "\120561")
  , ("\\mitPsi",ESymbol Alpha "\120569")
  , ("\\mitQ",ESymbol Alpha "\119876")
  , ("\\mitR",ESymbol Alpha "\119877")
  , ("\\mitRho",ESymbol Alpha "\120562")
  , ("\\mitS",ESymbol Alpha "\119878")
  , ("\\mitSigma",ESymbol Alpha "\120564")
  , ("\\mitT",ESymbol Alpha "\119879")
  , ("\\mitTau",ESymbol Alpha "\120565")
  , ("\\mitTheta",ESymbol Alpha "\120553")
  , ("\\mitU",ESymbol Alpha "\119880")
  , ("\\mitUpsilon",ESymbol Alpha "\120566")
  , ("\\mitV",ESymbol Alpha "\119881")
  , ("\\mitW",ESymbol Alpha "\119882")
  , ("\\mitX",ESymbol Alpha "\119883")
  , ("\\mitXi",ESymbol Alpha "\120559")
  , ("\\mitY",ESymbol Alpha "\119884")
  , ("\\mitZ",ESymbol Alpha "\119885")
  , ("\\mitZeta",ESymbol Alpha "\120551")
  , ("\\mita",ESymbol Alpha "\119886")
  , ("\\mitalpha",ESymbol Alpha "\120572")
  , ("\\mitb",ESymbol Alpha "\119887")
  , ("\\mitbeta",ESymbol Alpha "\120573")
  , ("\\mitc",ESymbol Alpha "\119888")
  , ("\\mitchi",ESymbol Alpha "\120594")
  , ("\\mitd",ESymbol Alpha "\119889")
  , ("\\mitdelta",ESymbol Alpha "\120575")
  , ("\\mite",ESymbol Alpha "\119890")
  , ("\\mitepsilon",ESymbol Alpha "\120576")
  , ("\\miteta",ESymbol Alpha "\120578")
  , ("\\mitf",ESymbol Alpha "\119891")
  , ("\\mitg",ESymbol Alpha "\119892")
  , ("\\mitgamma",ESymbol Alpha "\120574")
  , ("\\miti",ESymbol Alpha "\119894")
  , ("\\mitiota",ESymbol Alpha "\120580")
  , ("\\mitj",ESymbol Alpha "\119895")
  , ("\\mitk",ESymbol Alpha "\119896")
  , ("\\mitkappa",ESymbol Alpha "\120581")
  , ("\\mitl",ESymbol Alpha "\119897")
  , ("\\mitlambda",ESymbol Alpha "\120582")
  , ("\\mitm",ESymbol Alpha "\119898")
  , ("\\mitmu",ESymbol Alpha "\120583")
  , ("\\mitn",ESymbol Alpha "\119899")
  , ("\\mitnabla",ESymbol Ord "\120571")
  , ("\\mitnu",ESymbol Alpha "\120584")
  , ("\\mito",ESymbol Alpha "\119900")
  , ("\\mitomega",ESymbol Alpha "\120596")
  , ("\\mitomicron",ESymbol Alpha "\120586")
  , ("\\mitp",ESymbol Alpha "\119901")
  , ("\\mitpartial",ESymbol Ord "\120597")
  , ("\\mitphi",ESymbol Alpha "\120593")
  , ("\\mitpi",ESymbol Alpha "\120587")
  , ("\\mitpsi",ESymbol Alpha "\120595")
  , ("\\mitq",ESymbol Alpha "\119902")
  , ("\\mitr",ESymbol Alpha "\119903")
  , ("\\mitrho",ESymbol Alpha "\120588")
  , ("\\mits",ESymbol Alpha "\119904")
  , ("\\mitsansA",ESymbol Alpha "\120328")
  , ("\\mitsansB",ESymbol Alpha "\120329")
  , ("\\mitsansC",ESymbol Alpha "\120330")
  , ("\\mitsansD",ESymbol Alpha "\120331")
  , ("\\mitsansE",ESymbol Alpha "\120332")
  , ("\\mitsansF",ESymbol Alpha "\120333")
  , ("\\mitsansG",ESymbol Alpha "\120334")
  , ("\\mitsansH",ESymbol Alpha "\120335")
  , ("\\mitsansI",ESymbol Alpha "\120336")
  , ("\\mitsansJ",ESymbol Alpha "\120337")
  , ("\\mitsansK",ESymbol Alpha "\120338")
  , ("\\mitsansL",ESymbol Alpha "\120339")
  , ("\\mitsansM",ESymbol Alpha "\120340")
  , ("\\mitsansN",ESymbol Alpha "\120341")
  , ("\\mitsansO",ESymbol Alpha "\120342")
  , ("\\mitsansP",ESymbol Alpha "\120343")
  , ("\\mitsansQ",ESymbol Alpha "\120344")
  , ("\\mitsansR",ESymbol Alpha "\120345")
  , ("\\mitsansS",ESymbol Alpha "\120346")
  , ("\\mitsansT",ESymbol Alpha "\120347")
  , ("\\mitsansU",ESymbol Alpha "\120348")
  , ("\\mitsansV",ESymbol Alpha "\120349")
  , ("\\mitsansW",ESymbol Alpha "\120350")
  , ("\\mitsansX",ESymbol Alpha "\120351")
  , ("\\mitsansY",ESymbol Alpha "\120352")
  , ("\\mitsansZ",ESymbol Alpha "\120353")
  , ("\\mitsansa",ESymbol Alpha "\120354")
  , ("\\mitsansb",ESymbol Alpha "\120355")
  , ("\\mitsansc",ESymbol Alpha "\120356")
  , ("\\mitsansd",ESymbol Alpha "\120357")
  , ("\\mitsanse",ESymbol Alpha "\120358")
  , ("\\mitsansf",ESymbol Alpha "\120359")
  , ("\\mitsansg",ESymbol Alpha "\120360")
  , ("\\mitsansh",ESymbol Alpha "\120361")
  , ("\\mitsansi",ESymbol Alpha "\120362")
  , ("\\mitsansj",ESymbol Alpha "\120363")
  , ("\\mitsansk",ESymbol Alpha "\120364")
  , ("\\mitsansl",ESymbol Alpha "\120365")
  , ("\\mitsansm",ESymbol Alpha "\120366")
  , ("\\mitsansn",ESymbol Alpha "\120367")
  , ("\\mitsanso",ESymbol Alpha "\120368")
  , ("\\mitsansp",ESymbol Alpha "\120369")
  , ("\\mitsansq",ESymbol Alpha "\120370")
  , ("\\mitsansr",ESymbol Alpha "\120371")
  , ("\\mitsanss",ESymbol Alpha "\120372")
  , ("\\mitsanst",ESymbol Alpha "\120373")
  , ("\\mitsansu",ESymbol Alpha "\120374")
  , ("\\mitsansv",ESymbol Alpha "\120375")
  , ("\\mitsansw",ESymbol Alpha "\120376")
  , ("\\mitsansx",ESymbol Alpha "\120377")
  , ("\\mitsansy",ESymbol Alpha "\120378")
  , ("\\mitsansz",ESymbol Alpha "\120379")
  , ("\\mitsigma",ESymbol Alpha "\120590")
  , ("\\mitt",ESymbol Alpha "\119905")
  , ("\\mittau",ESymbol Alpha "\120591")
  , ("\\mittheta",ESymbol Alpha "\120579")
  , ("\\mitu",ESymbol Alpha "\119906")
  , ("\\mitupsilon",ESymbol Alpha "\120592")
  , ("\\mitv",ESymbol Alpha "\119907")
  , ("\\mitvarTheta",ESymbol Alpha "\120563")
  , ("\\mitvarepsilon",ESymbol Alpha "\120598")
  , ("\\mitvarkappa",ESymbol Alpha "\120600")
  , ("\\mitvarphi",ESymbol Alpha "\120601")
  , ("\\mitvarpi",ESymbol Alpha "\120603")
  , ("\\mitvarrho",ESymbol Alpha "\120602")
  , ("\\mitvarsigma",ESymbol Alpha "\120589")
  , ("\\mitvartheta",ESymbol Alpha "\120599")
  , ("\\mitw",ESymbol Alpha "\119908")
  , ("\\mitx",ESymbol Alpha "\119909")
  , ("\\mitxi",ESymbol Alpha "\120585")
  , ("\\mity",ESymbol Alpha "\119910")
  , ("\\mitz",ESymbol Alpha "\119911")
  , ("\\mitzeta",ESymbol Alpha "\120577")
  , ("\\mlcp",ESymbol Rel "\10971")
  , ("\\modtwosum",ESymbol Ord "\10762")
  , ("\\msansA",ESymbol Alpha "\120224")
  , ("\\msansB",ESymbol Alpha "\120225")
  , ("\\msansC",ESymbol Alpha "\120226")
  , ("\\msansD",ESymbol Alpha "\120227")
  , ("\\msansE",ESymbol Alpha "\120228")
  , ("\\msansF",ESymbol Alpha "\120229")
  , ("\\msansG",ESymbol Alpha "\120230")
  , ("\\msansH",ESymbol Alpha "\120231")
  , ("\\msansI",ESymbol Alpha "\120232")
  , ("\\msansJ",ESymbol Alpha "\120233")
  , ("\\msansK",ESymbol Alpha "\120234")
  , ("\\msansL",ESymbol Alpha "\120235")
  , ("\\msansM",ESymbol Alpha "\120236")
  , ("\\msansN",ESymbol Alpha "\120237")
  , ("\\msansO",ESymbol Alpha "\120238")
  , ("\\msansP",ESymbol Alpha "\120239")
  , ("\\msansQ",ESymbol Alpha "\120240")
  , ("\\msansR",ESymbol Alpha "\120241")
  , ("\\msansS",ESymbol Alpha "\120242")
  , ("\\msansT",ESymbol Alpha "\120243")
  , ("\\msansU",ESymbol Alpha "\120244")
  , ("\\msansV",ESymbol Alpha "\120245")
  , ("\\msansW",ESymbol Alpha "\120246")
  , ("\\msansX",ESymbol Alpha "\120247")
  , ("\\msansY",ESymbol Alpha "\120248")
  , ("\\msansZ",ESymbol Alpha "\120249")
  , ("\\msansa",ESymbol Alpha "\120250")
  , ("\\msansb",ESymbol Alpha "\120251")
  , ("\\msansc",ESymbol Alpha "\120252")
  , ("\\msansd",ESymbol Alpha "\120253")
  , ("\\msanse",ESymbol Alpha "\120254")
  , ("\\msanseight",ESymbol Ord "\120810")
  , ("\\msansf",ESymbol Alpha "\120255")
  , ("\\msansfive",ESymbol Ord "\120807")
  , ("\\msansfour",ESymbol Ord "\120806")
  , ("\\msansg",ESymbol Alpha "\120256")
  , ("\\msansh",ESymbol Alpha "\120257")
  , ("\\msansi",ESymbol Alpha "\120258")
  , ("\\msansj",ESymbol Alpha "\120259")
  , ("\\msansk",ESymbol Alpha "\120260")
  , ("\\msansl",ESymbol Alpha "\120261")
  , ("\\msansm",ESymbol Alpha "\120262")
  , ("\\msansn",ESymbol Alpha "\120263")
  , ("\\msansnine",ESymbol Ord "\120811")
  , ("\\msanso",ESymbol Alpha "\120264")
  , ("\\msansone",ESymbol Ord "\120803")
  , ("\\msansp",ESymbol Alpha "\120265")
  , ("\\msansq",ESymbol Alpha "\120266")
  , ("\\msansr",ESymbol Alpha "\120267")
  , ("\\msanss",ESymbol Alpha "\120268")
  , ("\\msansseven",ESymbol Ord "\120809")
  , ("\\msanssix",ESymbol Ord "\120808")
  , ("\\msanst",ESymbol Alpha "\120269")
  , ("\\msansthree",ESymbol Ord "\120805")
  , ("\\msanstwo",ESymbol Ord "\120804")
  , ("\\msansu",ESymbol Alpha "\120270")
  , ("\\msansv",ESymbol Alpha "\120271")
  , ("\\msansw",ESymbol Alpha "\120272")
  , ("\\msansx",ESymbol Alpha "\120273")
  , ("\\msansy",ESymbol Alpha "\120274")
  , ("\\msansz",ESymbol Alpha "\120275")
  , ("\\msanszero",ESymbol Ord "\120802")
  , ("\\mscrA",ESymbol Alpha "\119964")
  , ("\\mscrB",ESymbol Alpha "\8492")
  , ("\\mscrC",ESymbol Alpha "\119966")
  , ("\\mscrD",ESymbol Alpha "\119967")
  , ("\\mscrE",ESymbol Alpha "\8496")
  , ("\\mscrF",ESymbol Alpha "\8497")
  , ("\\mscrG",ESymbol Alpha "\119970")
  , ("\\mscrH",ESymbol Alpha "\8459")
  , ("\\mscrI",ESymbol Alpha "\8464")
  , ("\\mscrJ",ESymbol Alpha "\119973")
  , ("\\mscrK",ESymbol Alpha "\119974")
  , ("\\mscrL",ESymbol Alpha "\8466")
  , ("\\mscrM",ESymbol Alpha "\8499")
  , ("\\mscrN",ESymbol Alpha "\119977")
  , ("\\mscrO",ESymbol Alpha "\119978")
  , ("\\mscrP",ESymbol Alpha "\119979")
  , ("\\mscrQ",ESymbol Alpha "\119980")
  , ("\\mscrR",ESymbol Alpha "\8475")
  , ("\\mscrS",ESymbol Alpha "\119982")
  , ("\\mscrT",ESymbol Alpha "\119983")
  , ("\\mscrU",ESymbol Alpha "\119984")
  , ("\\mscrV",ESymbol Alpha "\119985")
  , ("\\mscrW",ESymbol Alpha "\119986")
  , ("\\mscrX",ESymbol Alpha "\119987")
  , ("\\mscrY",ESymbol Alpha "\119988")
  , ("\\mscrZ",ESymbol Alpha "\119989")
  , ("\\mscra",ESymbol Alpha "\119990")
  , ("\\mscrb",ESymbol Alpha "\119991")
  , ("\\mscrc",ESymbol Alpha "\119992")
  , ("\\mscrd",ESymbol Alpha "\119993")
  , ("\\mscre",ESymbol Alpha "\8495")
  , ("\\mscrf",ESymbol Alpha "\119995")
  , ("\\mscrg",ESymbol Alpha "\8458")
  , ("\\mscrh",ESymbol Alpha "\119997")
  , ("\\mscri",ESymbol Alpha "\119998")
  , ("\\mscrj",ESymbol Alpha "\119999")
  , ("\\mscrk",ESymbol Alpha "\120000")
  , ("\\mscrl",ESymbol Alpha "\120001")
  , ("\\mscrm",ESymbol Alpha "\120002")
  , ("\\mscrn",ESymbol Alpha "\120003")
  , ("\\mscro",ESymbol Alpha "\8500")
  , ("\\mscrp",ESymbol Alpha "\120005")
  , ("\\mscrq",ESymbol Alpha "\120006")
  , ("\\mscrr",ESymbol Alpha "\120007")
  , ("\\mscrs",ESymbol Alpha "\120008")
  , ("\\mscrt",ESymbol Alpha "\120009")
  , ("\\mscru",ESymbol Alpha "\120010")
  , ("\\mscrv",ESymbol Alpha "\120011")
  , ("\\mscrw",ESymbol Alpha "\120012")
  , ("\\mscrx",ESymbol Alpha "\120013")
  , ("\\mscry",ESymbol Alpha "\120014")
  , ("\\mscrz",ESymbol Alpha "\120015")
  , ("\\mttA",ESymbol Alpha "\120432")
  , ("\\mttB",ESymbol Alpha "\120433")
  , ("\\mttC",ESymbol Alpha "\120434")
  , ("\\mttD",ESymbol Alpha "\120435")
  , ("\\mttE",ESymbol Alpha "\120436")
  , ("\\mttF",ESymbol Alpha "\120437")
  , ("\\mttG",ESymbol Alpha "\120438")
  , ("\\mttH",ESymbol Alpha "\120439")
  , ("\\mttI",ESymbol Alpha "\120440")
  , ("\\mttJ",ESymbol Alpha "\120441")
  , ("\\mttK",ESymbol Alpha "\120442")
  , ("\\mttL",ESymbol Alpha "\120443")
  , ("\\mttM",ESymbol Alpha "\120444")
  , ("\\mttN",ESymbol Alpha "\120445")
  , ("\\mttO",ESymbol Alpha "\120446")
  , ("\\mttP",ESymbol Alpha "\120447")
  , ("\\mttQ",ESymbol Alpha "\120448")
  , ("\\mttR",ESymbol Alpha "\120449")
  , ("\\mttS",ESymbol Alpha "\120450")
  , ("\\mttT",ESymbol Alpha "\120451")
  , ("\\mttU",ESymbol Alpha "\120452")
  , ("\\mttV",ESymbol Alpha "\120453")
  , ("\\mttW",ESymbol Alpha "\120454")
  , ("\\mttX",ESymbol Alpha "\120455")
  , ("\\mttY",ESymbol Alpha "\120456")
  , ("\\mttZ",ESymbol Alpha "\120457")
  , ("\\mtta",ESymbol Alpha "\120458")
  , ("\\mttb",ESymbol Alpha "\120459")
  , ("\\mttc",ESymbol Alpha "\120460")
  , ("\\mttd",ESymbol Alpha "\120461")
  , ("\\mtte",ESymbol Alpha "\120462")
  , ("\\mtteight",ESymbol Ord "\120830")
  , ("\\mttf",ESymbol Alpha "\120463")
  , ("\\mttfive",ESymbol Ord "\120827")
  , ("\\mttfour",ESymbol Ord "\120826")
  , ("\\mttg",ESymbol Alpha "\120464")
  , ("\\mtth",ESymbol Alpha "\120465")
  , ("\\mtti",ESymbol Alpha "\120466")
  , ("\\mttj",ESymbol Alpha "\120467")
  , ("\\mttk",ESymbol Alpha "\120468")
  , ("\\mttl",ESymbol Alpha "\120469")
  , ("\\mttm",ESymbol Alpha "\120470")
  , ("\\mttn",ESymbol Alpha "\120471")
  , ("\\mttnine",ESymbol Ord "\120831")
  , ("\\mtto",ESymbol Alpha "\120472")
  , ("\\mttone",ESymbol Ord "\120823")
  , ("\\mttp",ESymbol Alpha "\120473")
  , ("\\mttq",ESymbol Alpha "\120474")
  , ("\\mttr",ESymbol Alpha "\120475")
  , ("\\mtts",ESymbol Alpha "\120476")
  , ("\\mttseven",ESymbol Ord "\120829")
  , ("\\mttsix",ESymbol Ord "\120828")
  , ("\\mttt",ESymbol Alpha "\120477")
  , ("\\mttthree",ESymbol Ord "\120825")
  , ("\\mtttwo",ESymbol Ord "\120824")
  , ("\\mttu",ESymbol Alpha "\120478")
  , ("\\mttv",ESymbol Alpha "\120479")
  , ("\\mttw",ESymbol Alpha "\120480")
  , ("\\mttx",ESymbol Alpha "\120481")
  , ("\\mtty",ESymbol Alpha "\120482")
  , ("\\mttz",ESymbol Alpha "\120483")
  , ("\\mttzero",ESymbol Ord "\120822")
  , ("\\multimap",ESymbol Rel "\8888")
  , ("\\multimapboth",ESymbol Rel "\10719")
  , ("\\multimapdotbothA",ESymbol Rel "\8886")
  , ("\\multimapdotbothB",ESymbol Rel "\8887")
  , ("\\multimapinv",ESymbol Rel "\10204")
  , ("\\nHdownarrow",ESymbol Ord "\8671")
  , ("\\nHuparrow",ESymbol Ord "\8670")
  , ("\\nLeftarrow",ESymbol Rel "\8653")
  , ("\\nLeftrightarrow",ESymbol Rel "\8654")
  , ("\\nRightarrow",ESymbol Rel "\8655")
  , ("\\nVDash",ESymbol Rel "\8879")
  , ("\\nVdash",ESymbol Rel "\8878")
  , ("\\nVleftarrow",ESymbol Rel "\8698")
  , ("\\nVleftarrowtail",ESymbol Rel "\11066")
  , ("\\nVleftrightarrow",ESymbol Rel "\8700")
  , ("\\nVrightarrow",ESymbol Rel "\8699")
  , ("\\nVrightarrowtail",ESymbol Rel "\10517")
  , ("\\nVtwoheadleftarrow",ESymbol Rel "\11061")
  , ("\\nVtwoheadleftarrowtail",ESymbol Rel "\11069")
  , ("\\nVtwoheadrightarrow",ESymbol Rel "\10497")
  , ("\\nVtwoheadrightarrowtail",ESymbol Rel "\10520")
  , ("\\napprox",ESymbol Rel "\8777")
  , ("\\nasymp",ESymbol Rel "\8813")
  , ("\\natural",ESymbol Ord "\9838")
  , ("\\ncong",ESymbol Rel "\8775")
  , ("\\ndres",ESymbol Bin "\10852")
  , ("\\neovnwarrow",ESymbol Ord "\10545")
  , ("\\neovsearrow",ESymbol Ord "\10542")
  , ("\\neptune",ESymbol Ord "\9798")
  , ("\\nequiv",ESymbol Rel "\8802")
  , ("\\neswarrow",ESymbol Rel "\10530")
  , ("\\neuter",ESymbol Ord "\9906")
  , ("\\nexi",ESymbol Ord "\8708")
  , ("\\nexists",ESymbol Ord "\8708")
  , ("\\ngeq",ESymbol Rel "\8817")
  , ("\\ngeqslant",ESymbol Rel "\8817")
  , ("\\ngtr",ESymbol Rel "\8815")
  , ("\\ngtrless",ESymbol Rel "\8825")
  , ("\\ngtrsim",ESymbol Rel "\8821")
  , ("\\nhVvert",ESymbol Bin "\10997")
  , ("\\nhpar",ESymbol Rel "\10994")
  , ("\\nin",ESymbol Rel "\8713")
  , ("\\niobar",ESymbol Rel "\8958")
  , ("\\nis",ESymbol Rel "\8956")
  , ("\\nisd",ESymbol Rel "\8954")
  , ("\\nleftarrow",ESymbol Rel "\8602")
  , ("\\nleftrightarrow",ESymbol Rel "\8622")
  , ("\\nleq",ESymbol Rel "\8816")
  , ("\\nleqslant",ESymbol Rel "\8816")
  , ("\\nless",ESymbol Rel "\8814")
  , ("\\nlessgtr",ESymbol Rel "\8824")
  , ("\\nlesssim",ESymbol Rel "\8820")
  , ("\\nmid",ESymbol Rel "\8740")
  , ("\\nni",ESymbol Rel "\8716")
  , ("\\not",ESymbol Accent "\824")
  , ("\\notasymp",ESymbol Rel "\8813")
  , ("\\notbackslash",ESymbol Ord "\9024")
  , ("\\notni",ESymbol Rel "\8716")
  , ("\\notslash",ESymbol Rel "\9023")
  , ("\\nparallel",ESymbol Rel "\8742")
  , ("\\npolint",ESymbol Op "\10772")
  , ("\\nprec",ESymbol Rel "\8832")
  , ("\\npreccurlyeq",ESymbol Rel "\8928")
  , ("\\npreceq",ESymbol Rel "\8928")
  , ("\\nrightarrow",ESymbol Rel "\8603")
  , ("\\nrres",ESymbol Bin "\10853")
  , ("\\nsim",ESymbol Rel "\8769")
  , ("\\nsime",ESymbol Rel "\8772")
  , ("\\nsimeq",ESymbol Rel "\8772")
  , ("\\nsqsubseteq",ESymbol Rel "\8930")
  , ("\\nsqsupseteq",ESymbol Rel "\8931")
  , ("\\nsucc",ESymbol Rel "\8833")
  , ("\\nsucccurlyeq",ESymbol Rel "\8929")
  , ("\\nsucceq",ESymbol Rel "\8929")
  , ("\\ntriangleleft",ESymbol Rel "\8938")
  , ("\\ntrianglelefteq",ESymbol Rel "\8940")
  , ("\\ntriangleright",ESymbol Rel "\8939")
  , ("\\ntrianglerighteq",ESymbol Rel "\8941")
  , ("\\nunlhd",ESymbol Rel "\8940")
  , ("\\nunrhd",ESymbol Rel "\8941")
  , ("\\nvDash",ESymbol Rel "\8877")
  , ("\\nvLeftarrow",ESymbol Rel "\10498")
  , ("\\nvLeftrightarrow",ESymbol Rel "\10500")
  , ("\\nvRightarrow",ESymbol Rel "\10499")
  , ("\\nvdash",ESymbol Rel "\8876")
  , ("\\nvinfty",ESymbol Ord "\10718")
  , ("\\nvleftarrow",ESymbol Rel "\8695")
  , ("\\nvleftarrowtail",ESymbol Rel "\11065")
  , ("\\nvleftrightarrow",ESymbol Rel "\8697")
  , ("\\nvrightarrow",ESymbol Rel "\8696")
  , ("\\nvrightarrowtail",ESymbol Rel "\10516")
  , ("\\nvtwoheadleftarrow",ESymbol Rel "\11060")
  , ("\\nvtwoheadleftarrowtail",ESymbol Rel "\11068")
  , ("\\nvtwoheadrightarrow",ESymbol Rel "\10496")
  , ("\\nvtwoheadrightarrowtail",ESymbol Rel "\10519")
  , ("\\nwovnearrow",ESymbol Ord "\10546")
  , ("\\nwsearrow",ESymbol Rel "\10529")
  , ("\\obar",ESymbol Bin "\9021")
  , ("\\obot",ESymbol Ord "\10682")
  , ("\\obrbrak",ESymbol Ord "\9184")
  , ("\\obslash",ESymbol Bin "\10680")
  , ("\\ocirc",ESymbol Accent "\778")
  , ("\\ocommatopright",ESymbol Accent "\789")
  , ("\\octothorpe",ESymbol Ord "#")
  , ("\\odiv",ESymbol Bin "\10808")
  , ("\\odotslashdot",ESymbol Ord "\10684")
  , ("\\ogreaterthan",ESymbol Bin "\10689")
  , ("\\oiiint",ESymbol Op "\8752")
  , ("\\oiint",ESymbol Op "\8751")
  , ("\\ointctrclockwise",ESymbol Op "\8755")
  , ("\\olcross",ESymbol Ord "\10683")
  , ("\\olessthan",ESymbol Bin "\10688")
  , ("\\operp",ESymbol Bin "\10681")
  , ("\\opluslhrim",ESymbol Bin "\10797")
  , ("\\oplusrhrim",ESymbol Bin "\10798")
  , ("\\origof",ESymbol Rel "\8886")
  , ("\\otimeshat",ESymbol Bin "\10806")
  , ("\\otimeslhrim",ESymbol Bin "\10804")
  , ("\\otimesrhrim",ESymbol Bin "\10805")
  , ("\\oturnedcomma",ESymbol Accent "\786")
  , ("\\overbar",ESymbol Accent "\175")
  , ("\\overbrace",ESymbol TOver "\9182")
  , ("\\overbracket",ESymbol TOver "\9140")
  , ("\\overleftarrow",ESymbol Accent "\8406")
  , ("\\overrightarrow",ESymbol Accent "\8407")
  , ("\\overleftrightarrow",ESymbol Accent "\8417")
  , ("\\overline",ESymbol TOver "\175")
  , ("\\overparen",ESymbol TOver "\9180")
  , ("\\ovhook",ESymbol Accent "\777")
  , ("\\parallelogram",ESymbol Ord "\9649")
  , ("\\parallelogramblack",ESymbol Ord "\9648")
  , ("\\parsim",ESymbol Rel "\10995")
  , ("\\partialmeetcontraction",ESymbol Rel "\10915")
  , ("\\partialup",ESymbol Ord "\8706")
  , ("\\pencil",ESymbol Ord "\9998")
  , ("\\pentagon",ESymbol Ord "\11040")
  , ("\\pentagonblack",ESymbol Ord "\11039")
  , ("\\percent",ESymbol Ord "%")
  , ("\\period",ESymbol Alpha ".")
  , ("\\perps",ESymbol Ord "\10977")
  , ("\\pfun",ESymbol Rel "\8696")
  , ("\\pinj",ESymbol Rel "\10516")
  , ("\\pisces",ESymbol Ord "\9811")
  , ("\\pitchfork",ESymbol Rel "\8916")
  , ("\\plus",ESymbol Bin "+")
  , ("\\plusdot",ESymbol Bin "\10789")
  , ("\\pluseqq",ESymbol Bin "\10866")
  , ("\\plushat",ESymbol Bin "\10787")
  , ("\\plussim",ESymbol Bin "\10790")
  , ("\\plussubtwo",ESymbol Bin "\10791")
  , ("\\plustrif",ESymbol Bin "\10792")
  , ("\\pluto",ESymbol Ord "\9799")
  , ("\\pointint",ESymbol Op "\10773")
  , ("\\pointright",ESymbol Ord "\9758")
  , ("\\postalmark",ESymbol Ord "\12306")
  , ("\\pounds",ESymbol Ord "\163")
  , ("\\precapprox",ESymbol Rel "\10935")
  , ("\\preccurlyeq",ESymbol Rel "\8828")
  , ("\\preceqq",ESymbol Rel "\10931")
  , ("\\precnapprox",ESymbol Rel "\10937")
  , ("\\precneq",ESymbol Rel "\10929")
  , ("\\precneqq",ESymbol Rel "\10933")
  , ("\\precnsim",ESymbol Rel "\8936")
  , ("\\precsim",ESymbol Rel "\8830")
  , ("\\profline",ESymbol Ord "\8978")
  , ("\\profsurf",ESymbol Ord "\8979")
  , ("\\project",ESymbol Op "\10785")
  , ("\\prurel",ESymbol Rel "\8880")
  , ("\\psur",ESymbol Rel "\10496")
  , ("\\psurj",ESymbol Rel "\10496")
  , ("\\pullback",ESymbol Rel "\10195")
  , ("\\pushout",ESymbol Rel "\10196")
  , ("\\qoppa",ESymbol Ord "\985")
  , ("\\qprime",ESymbol Ord "\8279")
  , ("\\quarternote",ESymbol Ord "\9833")
  , ("\\questeq",ESymbol Rel "\8799")
  , ("\\question",ESymbol Ord "?")
  , ("\\rAngle",ESymbol Close "\10219")
  , ("\\rBrace",ESymbol Close "\10628")
  , ("\\rBrack",ESymbol Close "\10215")
  , ("\\rParen",ESymbol Close "\10630")
  , ("\\radiation",ESymbol Ord "\9762")
  , ("\\rang",ESymbol Close "\10219")
  , ("\\rangle",ESymbol Close "\10217")
  , ("\\rangle",ESymbol Close "\12297")
  , ("\\rangle",ESymbol Close "\9002")
  , ("\\rangledot",ESymbol Close "\10642")
  , ("\\rangledownzigzagarrow",ESymbol Ord "\9084")
  , ("\\rbag",ESymbol Close "\10182")
  , ("\\rblkbrbrak",ESymbol Close "\10648")
  , ("\\rblot",ESymbol Close "\10634")
  , ("\\rbrace",ESymbol Close "}")
  , ("\\rbracelend",ESymbol Ord "\9133")
  , ("\\rbracemid",ESymbol Ord "\9132")
  , ("\\rbraceuend",ESymbol Ord "\9131")
  , ("\\rbrack",ESymbol Close "]")
  , ("\\rbrackextender",ESymbol Ord "\9125")
  , ("\\rbracklend",ESymbol Ord "\9126")
  , ("\\rbracklrtick",ESymbol Close "\10638")
  , ("\\rbrackubar",ESymbol Close "\10636")
  , ("\\rbrackuend",ESymbol Ord "\9124")
  , ("\\rbrackurtick",ESymbol Close "\10640")
  , ("\\rbrbrak",ESymbol Close "\10099")
  , ("\\rbrbrak",ESymbol Close "\12309")
  , ("\\rceil",ESymbol Close "\8969")
  , ("\\rcurvyangle",ESymbol Close "\10749")
  , ("\\rdiagovfdiag",ESymbol Ord "\10539")
  , ("\\rdiagovsearrow",ESymbol Ord "\10544")
  , ("\\recycle",ESymbol Ord "\9851")
  , ("\\rel",ESymbol Rel "\8596")
  , ("\\restriction",ESymbol Rel "\8638")
  , ("\\revangle",ESymbol Ord "\10659")
  , ("\\revangleubar",ESymbol Ord "\10661")
  , ("\\revemptyset",ESymbol Ord "\10672")
  , ("\\revequilibrium",ESymbol Rel "\8651")
  , ("\\revnmid",ESymbol Rel "\10990")
  , ("\\rfbowtie",ESymbol Rel "\10706")
  , ("\\rfloor",ESymbol Close "\8971")
  , ("\\rftimes",ESymbol Rel "\10709")
  , ("\\rgroup",ESymbol Close "\10223")
  , ("\\rightangle",ESymbol Ord "\8735")
  , ("\\rightanglemdot",ESymbol Ord "\10653")
  , ("\\rightanglesqr",ESymbol Ord "\10652")
  , ("\\rightarrowapprox",ESymbol Rel "\10613")
  , ("\\rightarrowbackapprox",ESymbol Rel "\11080")
  , ("\\rightarrowbar",ESymbol Rel "\8677")
  , ("\\rightarrowbsimilar",ESymbol Rel "\11084")
  , ("\\rightarrowdiamond",ESymbol Rel "\10526")
  , ("\\rightarrowgtr",ESymbol Rel "\11075")
  , ("\\rightarrowonoplus",ESymbol Rel "\10228")
  , ("\\rightarrowplus",ESymbol Rel "\10565")
  , ("\\rightarrowshortleftarrow",ESymbol Rel "\10562")
  , ("\\rightarrowsimilar",ESymbol Rel "\10612")
  , ("\\rightarrowsupset",ESymbol Rel "\11076")
  , ("\\rightarrowtail",ESymbol Rel "\8611")
  , ("\\rightarrowtriangle",ESymbol Rel "\8702")
  , ("\\rightarrowx",ESymbol Rel "\10567")
  , ("\\rightbarharpoon",ESymbol Rel "\10604")
  , ("\\rightbkarrow",ESymbol Rel "\10509")
  , ("\\rightcurvedarrow",ESymbol Rel "\10547")
  , ("\\rightdasharrow",ESymbol Ord "\8674")
  , ("\\rightdbltail",ESymbol Rel "\10524")
  , ("\\rightdotarrow",ESymbol Rel "\10513")
  , ("\\rightdowncurvedarrow",ESymbol Rel "\10551")
  , ("\\rightfishtail",ESymbol Rel "\10621")
  , ("\\rightharpoonaccent",ESymbol Accent "\8401")
  , ("\\rightharpoondown",ESymbol Rel "\8641")
  , ("\\rightharpoondownbar",ESymbol Rel "\10583")
  , ("\\rightharpoonsupdown",ESymbol Rel "\10596")
  , ("\\rightharpoonup",ESymbol Rel "\8640")
  , ("\\rightharpoonupbar",ESymbol Rel "\10579")
  , ("\\rightharpoonupdash",ESymbol Rel "\10604")
  , ("\\rightimply",ESymbol Rel "\10608")
  , ("\\rightleftarrow",ESymbol Rel "\8644")
  , ("\\rightleftarrows",ESymbol Rel "\8644")
  , ("\\rightleftharpoon",ESymbol Rel "\10571")
  , ("\\rightleftharpoons",ESymbol Rel "\8652")
  , ("\\rightleftharpoonsdown",ESymbol Rel "\10601")
  , ("\\rightleftharpoonsup",ESymbol Rel "\10600")
  , ("\\rightmoon",ESymbol Ord "\9789")
  , ("\\rightouterjoin",ESymbol Op "\10198")
  , ("\\rightpentagon",ESymbol Ord "\11092")
  , ("\\rightpentagonblack",ESymbol Ord "\11091")
  , ("\\rightrightarrows",ESymbol Rel "\8649")
  , ("\\rightrightharpoons",ESymbol Rel "\10596")
  , ("\\rightslice",ESymbol Rel "\10919")
  , ("\\rightsquigarrow",ESymbol Rel "\8669")
  , ("\\righttail",ESymbol Rel "\10522")
  , ("\\rightthreearrows",ESymbol Rel "\8694")
  , ("\\rightthreetimes",ESymbol Bin "\8908")
  , ("\\rightturn",ESymbol Ord "\8635")
  , ("\\rightupdownharpoon",ESymbol Rel "\10575")
  , ("\\rightwavearrow",ESymbol Rel "\8605")
  , ("\\rightwhitearrow",ESymbol Ord "\8680")
  , ("\\rimg",ESymbol Close "\10632")
  , ("\\ring",ESymbol Accent "\778")
  , ("\\ringplus",ESymbol Bin "\10786")
  , ("\\risingdotseq",ESymbol Rel "\8787")
  , ("\\rmoustache",ESymbol Ord "\9137")
  , ("\\rparen",ESymbol Close ")")
  , ("\\rparenextender",ESymbol Ord "\9119")
  , ("\\rparengtr",ESymbol Close "\10644")
  , ("\\rparenlend",ESymbol Ord "\9120")
  , ("\\rparenuend",ESymbol Ord "\9118")
  , ("\\rppolint",ESymbol Op "\10770")
  , ("\\rrangle",ESymbol Close "\10634")
  , ("\\rrbracket",ESymbol Close "\10215")
  , ("\\rrbracket",ESymbol Close "\12315")
  , ("\\rres",ESymbol Bin "\9655")
  , ("\\rrparenthesis",ESymbol Close "\10632")
  , ("\\rsolbar",ESymbol Bin "\10743")
  , ("\\rsqhook",ESymbol Rel "\10958")
  , ("\\rsub",ESymbol Bin "\10853")
  , ("\\rtriltri",ESymbol Rel "\10702")
  , ("\\ruledelayed",ESymbol Rel "\10740")
  , ("\\rvboxline",ESymbol Ord "\9145")
  , ("\\rvzigzag",ESymbol Close "\10713")
  , ("\\sadface",ESymbol Ord "\9785")
  , ("\\sagittarius",ESymbol Ord "\9808")
  , ("\\sampi",ESymbol Alpha "\993")
  , ("\\sansLmirrored",ESymbol Ord "\8515")
  , ("\\sansLturned",ESymbol Ord "\8514")
  , ("\\saturn",ESymbol Ord "\9796")
  , ("\\scorpio",ESymbol Ord "\9807")
  , ("\\scpolint",ESymbol Op "\10771")
  , ("\\scurel",ESymbol Rel "\8881")
  , ("\\sdef",ESymbol Rel "\8793")
  , ("\\second",ESymbol Ord "\8243")
  , ("\\semi",ESymbol Op "\10783")
  , ("\\semicolon",ESymbol Pun ";")
  , ("\\seovnearrow",ESymbol Ord "\10541")
  , ("\\sharp",ESymbol Ord "\9839")
  , ("\\shortdowntack",ESymbol Rel "\10975")
  , ("\\shortlefttack",ESymbol Rel "\10974")
  , ("\\shortrightarrowleftarrow",ESymbol Rel "\10564")
  , ("\\shortuptack",ESymbol Rel "\10976")
  , ("\\shuffle",ESymbol Bin "\10722")
  , ("\\simgE",ESymbol Rel "\10912")
  , ("\\simgtr",ESymbol Rel "\10910")
  , ("\\similarleftarrow",ESymbol Rel "\11081")
  , ("\\similarrightarrow",ESymbol Rel "\10610")
  , ("\\simlE",ESymbol Rel "\10911")
  , ("\\simless",ESymbol Rel "\10909")
  , ("\\simminussim",ESymbol Rel "\10860")
  , ("\\simneqq",ESymbol Rel "\8774")
  , ("\\simplus",ESymbol Bin "\10788")
  , ("\\simrdots",ESymbol Rel "\10859")
  , ("\\sinewave",ESymbol Ord "\8767")
  , ("\\sixteenthnote",ESymbol Ord "\9836")
  , ("\\skull",ESymbol Ord "\9760")
  , ("\\slash",ESymbol Bin "\8725")
  , ("\\slash",ESymbol Ord "/")
  , ("\\smallblacktriangleleft",ESymbol Bin "\9666")
  , ("\\smallblacktriangleright",ESymbol Bin "\9656")
  , ("\\smallfrown",ESymbol Rel "\8994")
  , ("\\smallin",ESymbol Rel "\8714")
  , ("\\smallni",ESymbol Rel "\8717")
  , ("\\smallsetminus",ESymbol Bin "\8726")
  , ("\\smallsmile",ESymbol Rel "\8995")
  , ("\\smalltriangledown",ESymbol Bin "\9663")
  , ("\\smalltriangleleft",ESymbol Bin "\9667")
  , ("\\smalltriangleright",ESymbol Bin "\9657")
  , ("\\smalltriangleup",ESymbol Bin "\9653")
  , ("\\smashtimes",ESymbol Bin "\10803")
  , ("\\smblkcircle",ESymbol Bin "\8226")
  , ("\\smblkdiamond",ESymbol Ord "\11049")
  , ("\\smblklozenge",ESymbol Ord "\11050")
  , ("\\smblksquare",ESymbol Ord "\9642")
  , ("\\smeparsl",ESymbol Rel "\10724")
  , ("\\smileface",ESymbol Ord "\9786")
  , ("\\smiley",ESymbol Ord "\9786")
  , ("\\smt",ESymbol Rel "\10922")
  , ("\\smte",ESymbol Rel "\10924")
  , ("\\smwhitestar",ESymbol Ord "\11090")
  , ("\\smwhtcircle",ESymbol Ord "\9702")
  , ("\\smwhtdiamond",ESymbol Bin "\8900")
  , ("\\smwhtlozenge",ESymbol Ord "\11051")
  , ("\\smwhtsquare",ESymbol Ord "\9643")
  , ("\\spadesuit",ESymbol Ord "\9824")
  , ("\\spddot",ESymbol Ord "\168")
  , ("\\sphat",ESymbol Ord "^")
  , ("\\sphericalangle",ESymbol Ord "\8738")
  , ("\\sphericalangleup",ESymbol Ord "\10657")
  , ("\\spot",ESymbol Ord "\10625")
  , ("\\sptilde",ESymbol Ord "~")
  , ("\\sqint",ESymbol Op "\10774")
  , ("\\sqlozenge",ESymbol Ord "\8977")
  , ("\\sqrint",ESymbol Op "\10774")
  , ("\\sqrt",ESymbol Rad "\8730")
  , ("\\sqrt[3]",ESymbol Rad "\8731")
  , ("\\sqrt[4]",ESymbol Rad "\8732")
  , ("\\sqrtbottom",ESymbol Ord "\9143")
  , ("\\sqsubsetneq",ESymbol Rel "\8932")
  , ("\\sqsupsetneq",ESymbol Rel "\8933")
  , ("\\squarebotblack",ESymbol Ord "\11027")
  , ("\\squarecrossfill",ESymbol Ord "\9641")
  , ("\\squarehfill",ESymbol Ord "\9636")
  , ("\\squarehvfill",ESymbol Ord "\9638")
  , ("\\squareleftblack",ESymbol Ord "\9703")
  , ("\\squarellblack",ESymbol Ord "\11029")
  , ("\\squarellquad",ESymbol Ord "\9713")
  , ("\\squarelrblack",ESymbol Ord "\9706")
  , ("\\squarelrquad",ESymbol Ord "\9714")
  , ("\\squareneswfill",ESymbol Ord "\9640")
  , ("\\squarenwsefill",ESymbol Ord "\9639")
  , ("\\squarerightblack",ESymbol Ord "\9704")
  , ("\\squaretopblack",ESymbol Ord "\11026")
  , ("\\squareulblack",ESymbol Ord "\9705")
  , ("\\squareulquad",ESymbol Ord "\9712")
  , ("\\squareurblack",ESymbol Ord "\11028")
  , ("\\squareurquad",ESymbol Ord "\9715")
  , ("\\squarevfill",ESymbol Ord "\9637")
  , ("\\squoval",ESymbol Ord "\9634")
  , ("\\sslash",ESymbol Bin "\11005")
  , ("\\stareq",ESymbol Rel "\8795")
  , ("\\steaming",ESymbol Ord "\9749")
  , ("\\sterling",ESymbol Ord "\163")
  , ("\\stigma",ESymbol Alpha "\987")
  , ("\\strictfi",ESymbol Rel "\10620")
  , ("\\strictif",ESymbol Rel "\10621")
  , ("\\strns",ESymbol Ord "\9188")
  , ("\\subedot",ESymbol Rel "\10947")
  , ("\\submult",ESymbol Rel "\10945")
  , ("\\subrarr",ESymbol Rel "\10617")
  , ("\\subsetapprox",ESymbol Rel "\10953")
  , ("\\subsetcirc",ESymbol Ord "\10179")
  , ("\\subsetdot",ESymbol Rel "\10941")
  , ("\\subseteqq",ESymbol Rel "\10949")
  , ("\\subsetneq",ESymbol Rel "\8842")
  , ("\\subsetneqq",ESymbol Rel "\10955")
  , ("\\subsetplus",ESymbol Rel "\10943")
  , ("\\subsim",ESymbol Rel "\10951")
  , ("\\subsub",ESymbol Rel "\10965")
  , ("\\subsup",ESymbol Rel "\10963")
  , ("\\succapprox",ESymbol Rel "\10936")
  , ("\\succcurlyeq",ESymbol Rel "\8829")
  , ("\\succeqq",ESymbol Rel "\10932")
  , ("\\succnapprox",ESymbol Rel "\10938")
  , ("\\succneq",ESymbol Rel "\10930")
  , ("\\succneqq",ESymbol Rel "\10934")
  , ("\\succnsim",ESymbol Rel "\8937")
  , ("\\succsim",ESymbol Rel "\8831")
  , ("\\sumbottom",ESymbol Ord "\9139")
  , ("\\sumint",ESymbol Op "\10763")
  , ("\\sumtop",ESymbol Ord "\9138")
  , ("\\sun",ESymbol Ord "\9788")
  , ("\\supdsub",ESymbol Rel "\10968")
  , ("\\supedot",ESymbol Rel "\10948")
  , ("\\suphsol",ESymbol Rel "\10185")
  , ("\\suphsub",ESymbol Rel "\10967")
  , ("\\suplarr",ESymbol Rel "\10619")
  , ("\\supmult",ESymbol Rel "\10946")
  , ("\\supsetapprox",ESymbol Rel "\10954")
  , ("\\supsetcirc",ESymbol Ord "\10180")
  , ("\\supsetdot",ESymbol Rel "\10942")
  , ("\\supseteqq",ESymbol Rel "\10950")
  , ("\\supsetneq",ESymbol Rel "\8843")
  , ("\\supsetneqq",ESymbol Rel "\10956")
  , ("\\supsetplus",ESymbol Rel "\10944")
  , ("\\supsim",ESymbol Rel "\10952")
  , ("\\supsub",ESymbol Rel "\10964")
  , ("\\supsup",ESymbol Rel "\10966")
  , ("\\swords",ESymbol Ord "\9876")
  , ("\\talloblong",ESymbol Bin "\11006")
  , ("\\taurus",ESymbol Ord "\9801")
  , ("\\tcmu",ESymbol Alpha "\181")
  , ("\\tcohm",ESymbol Alpha "\8486")
  , ("\\thermod",ESymbol Ord "\10727")
  , ("\\third",ESymbol Ord "\8244")
  , ("\\threedangle",ESymbol Ord "\10176")
  , ("\\threedotcolon",ESymbol Bin "\10998")
  , ("\\threeunderdot",ESymbol Accent "\8424")
  , ("\\tieconcat",ESymbol Bin "\8256")
  , ("\\tieinfty",ESymbol Ord "\10717")
  , ("\\tilde",ESymbol Accent "\771")
  , ("\\timesbar",ESymbol Bin "\10801")
  , ("\\tinj",ESymbol Rel "\8611")
  , ("\\tminus",ESymbol Bin "\10751")
  , ("\\toea",ESymbol Rel "\10536")
  , ("\\tona",ESymbol Rel "\10535")
  , ("\\topbot",ESymbol Ord "\9014")
  , ("\\topcir",ESymbol Ord "\10993")
  , ("\\topfork",ESymbol Rel "\10970")
  , ("\\topsemicircle",ESymbol Ord "\9696")
  , ("\\tosa",ESymbol Rel "\10537")
  , ("\\towa",ESymbol Rel "\10538")
  , ("\\tplus",ESymbol Bin "\10750")
  , ("\\trapezium",ESymbol Ord "\9186")
  , ("\\trianglecdot",ESymbol Ord "\9708")
  , ("\\triangledown",ESymbol Bin "\9663")
  , ("\\triangleleftblack",ESymbol Ord "\9709")
  , ("\\trianglelefteq",ESymbol Rel "\8884")
  , ("\\triangleminus",ESymbol Bin "\10810")
  , ("\\triangleodot",ESymbol Ord "\10698")
  , ("\\triangleplus",ESymbol Bin "\10809")
  , ("\\triangleq",ESymbol Rel "\8796")
  , ("\\trianglerightblack",ESymbol Ord "\9710")
  , ("\\trianglerighteq",ESymbol Rel "\8885")
  , ("\\triangles",ESymbol Ord "\10700")
  , ("\\triangleserifs",ESymbol Bin "\10701")
  , ("\\triangletimes",ESymbol Bin "\10811")
  , ("\\triangleubar",ESymbol Ord "\10699")
  , ("\\tripleplus",ESymbol Bin "\10747")
  , ("\\trprime",ESymbol Ord "\8244")
  , ("\\trslash",ESymbol Bin "\11003")
  , ("\\tsur",ESymbol Rel "\8608")
  , ("\\turnangle",ESymbol Ord "\10658")
  , ("\\turnediota",ESymbol Alpha "\8489")
  , ("\\turnednot",ESymbol Ord "\8985")
  , ("\\twocaps",ESymbol Bin "\10827")
  , ("\\twocups",ESymbol Bin "\10826")
  , ("\\twoheaddownarrow",ESymbol Rel "\8609")
  , ("\\twoheadleftarrow",ESymbol Rel "\8606")
  , ("\\twoheadleftarrowtail",ESymbol Rel "\11067")
  , ("\\twoheadleftdbkarrow",ESymbol Rel "\11063")
  , ("\\twoheadmapsfrom",ESymbol Rel "\11062")
  , ("\\twoheadmapsto",ESymbol Rel "\10501")
  , ("\\twoheadrightarrow",ESymbol Rel "\8608")
  , ("\\twoheadrightarrowtail",ESymbol Rel "\10518")
  , ("\\twoheaduparrow",ESymbol Rel "\8607")
  , ("\\twoheaduparrowcircle",ESymbol Rel "\10569")
  , ("\\twolowline",ESymbol Ord "\8215")
  , ("\\twonotes",ESymbol Ord "\9835")
  , ("\\typecolon",ESymbol Bin "\10626")
  , ("\\ubrbrak",ESymbol Ord "\9185")
  , ("\\ularc",ESymbol Ord "\9692")
  , ("\\ulblacktriangle",ESymbol Ord "\9700")
  , ("\\ulcorner",ESymbol Open "\8988")
  , ("\\ultriangle",ESymbol Ord "\9720")
  , ("\\uminus",ESymbol Bin "\10817")
  , ("\\underbar",ESymbol TUnder "\817")
  , ("\\underbrace",ESymbol TUnder "\9183")
  , ("\\underbracket",ESymbol TUnder "\9141")
  , ("\\underleftarrow",ESymbol Accent "\8430")
  , ("\\underleftharpoondown",ESymbol Accent "\8429")
  , ("\\underline",ESymbol TUnder "_")
  , ("\\underparen",ESymbol TUnder "\9181")
  , ("\\underrightarrow",ESymbol Accent "\8431")
  , ("\\underrightharpoondown",ESymbol Accent "\8428")
  , ("\\unicodecdots",ESymbol Ord "\8943")
  , ("\\unicodeellipsis",ESymbol Ord "\8230")
  , ("\\upAlpha",ESymbol Alpha "\913")
  , ("\\upBeta",ESymbol Alpha "\914")
  , ("\\upChi",ESymbol Alpha "\935")
  , ("\\upDelta",ESymbol Alpha "\916")
  , ("\\upDigamma",ESymbol Alpha "\988")
  , ("\\upEpsilon",ESymbol Alpha "\917")
  , ("\\upEta",ESymbol Alpha "\919")
  , ("\\upGamma",ESymbol Alpha "\915")
  , ("\\upIota",ESymbol Alpha "\921")
  , ("\\upKappa",ESymbol Alpha "\922")
  , ("\\upKoppa",ESymbol Alpha "\990")
  , ("\\upLambda",ESymbol Alpha "\923")
  , ("\\upMu",ESymbol Alpha "\924")
  , ("\\upNu",ESymbol Alpha "\925")
  , ("\\upOmega",ESymbol Alpha "\937")
  , ("\\upOmicron",ESymbol Alpha "\927")
  , ("\\upPhi",ESymbol Alpha "\934")
  , ("\\upPi",ESymbol Alpha "\928")
  , ("\\upPsi",ESymbol Alpha "\936")
  , ("\\upRho",ESymbol Alpha "\929")
  , ("\\upSampi",ESymbol Alpha "\992")
  , ("\\upSigma",ESymbol Alpha "\931")
  , ("\\upStigma",ESymbol Alpha "\986")
  , ("\\upTau",ESymbol Alpha "\932")
  , ("\\upTheta",ESymbol Alpha "\920")
  , ("\\upUpsilon",ESymbol Alpha "\933")
  , ("\\upUpsilon",ESymbol Alpha "\978")
  , ("\\upXi",ESymbol Alpha "\926")
  , ("\\upZeta",ESymbol Alpha "\918")
  , ("\\upalpha",ESymbol Alpha "\945")
  , ("\\upand",ESymbol Bin "\8523")
  , ("\\uparrowbarred",ESymbol Rel "\10505")
  , ("\\uparrowdownarrow",ESymbol Rel "\8645")
  , ("\\uparrowoncircle",ESymbol Ord "\10685")
  , ("\\upbackepsilon",ESymbol Ord "\1014")
  , ("\\upbeta",ESymbol Alpha "\946")
  , ("\\upchi",ESymbol Alpha "\967")
  , ("\\updasharrow",ESymbol Ord "\8673")
  , ("\\updelta",ESymbol Alpha "\948")
  , ("\\updigamma",ESymbol Alpha "\989")
  , ("\\updownarrowbar",ESymbol Ord "\8616")
  , ("\\updownarrows",ESymbol Rel "\8645")
  , ("\\updownharpoonleftleft",ESymbol Rel "\10577")
  , ("\\updownharpoonleftright",ESymbol Rel "\10573")
  , ("\\updownharpoonrightleft",ESymbol Rel "\10572")
  , ("\\updownharpoonrightright",ESymbol Rel "\10575")
  , ("\\updownharpoons",ESymbol Rel "\10606")
  , ("\\updownharpoonsleftright",ESymbol Rel "\10606")
  , ("\\upepsilon",ESymbol Alpha "\949")
  , ("\\upequilibrium",ESymbol Rel "\10606")
  , ("\\upeta",ESymbol Alpha "\951")
  , ("\\upfishtail",ESymbol Rel "\10622")
  , ("\\upgamma",ESymbol Alpha "\947")
  , ("\\upharpoonleft",ESymbol Rel "\8639")
  , ("\\upharpoonleftbar",ESymbol Rel "\10592")
  , ("\\upharpoonleftdown",ESymbol Rel "\8643")
  , ("\\upharpoonleftup",ESymbol Rel "\8639")
  , ("\\upharpoonright",ESymbol Rel "\8638")
  , ("\\upharpoonrightbar",ESymbol Rel "\10588")
  , ("\\upharpoonrightdown",ESymbol Rel "\8642")
  , ("\\upharpoonsleftright",ESymbol Rel "\10595")
  , ("\\upin",ESymbol Rel "\10194")
  , ("\\upint",ESymbol Op "\10779")
  , ("\\upiota",ESymbol Alpha "\953")
  , ("\\upkappa",ESymbol Alpha "\954")
  , ("\\upkoppa",ESymbol Alpha "\991")
  , ("\\uplambda",ESymbol Alpha "\955")
  , ("\\upmu",ESymbol Alpha "\956")
  , ("\\upnu",ESymbol Alpha "\957")
  , ("\\upoldKoppa",ESymbol Ord "\984")
  , ("\\upoldkoppa",ESymbol Ord "\985")
  , ("\\upomega",ESymbol Alpha "\969")
  , ("\\upomicron",ESymbol Alpha "\959")
  , ("\\upphi",ESymbol Alpha "\981")
  , ("\\uppi",ESymbol Alpha "\960")
  , ("\\uppsi",ESymbol Alpha "\968")
  , ("\\uprevequilibrium",ESymbol Rel "\10607")
  , ("\\uprho",ESymbol Alpha "\961")
  , ("\\uprightcurvearrow",ESymbol Ord "\10548")
  , ("\\upsampi",ESymbol Alpha "\993")
  , ("\\upsigma",ESymbol Alpha "\963")
  , ("\\upstigma",ESymbol Alpha "\987")
  , ("\\uptau",ESymbol Alpha "\964")
  , ("\\uptheta",ESymbol Alpha "\952")
  , ("\\upuparrows",ESymbol Rel "\8648")
  , ("\\upupharpoons",ESymbol Rel "\10595")
  , ("\\upupsilon",ESymbol Alpha "\965")
  , ("\\upvarTheta",ESymbol Alpha "\1012")
  , ("\\upvarbeta",ESymbol Alpha "\976")
  , ("\\upvarepsilon",ESymbol Alpha "\1013")
  , ("\\upvarkappa",ESymbol Alpha "\1008")
  , ("\\upvarphi",ESymbol Alpha "\966")
  , ("\\upvarpi",ESymbol Alpha "\982")
  , ("\\upvarrho",ESymbol Alpha "\1009")
  , ("\\upvarsigma",ESymbol Alpha "\962")
  , ("\\upvartheta",ESymbol Alpha "\977")
  , ("\\upwhitearrow",ESymbol Ord "\8679")
  , ("\\upxi",ESymbol Alpha "\958")
  , ("\\upzeta",ESymbol Alpha "\950")
  , ("\\uranus",ESymbol Ord "\9797")
  , ("\\urarc",ESymbol Ord "\9693")
  , ("\\urblacktriangle",ESymbol Ord "\9701")
  , ("\\urcorner",ESymbol Close "\8989")
  , ("\\urtriangle",ESymbol Ord "\9721")
  , ("\\utilde",ESymbol Accent "\816")
  , ("\\vBar",ESymbol Rel "\10984")
  , ("\\vBarv",ESymbol Rel "\10985")
  , ("\\vDash",ESymbol Rel "\8872")
  , ("\\vDdash",ESymbol Rel "\10978")
  , ("\\varEarth",ESymbol Ord "\9793")
  , ("\\varVdash",ESymbol Rel "\10982")
  , ("\\varbarwedge",ESymbol Bin "\8965")
  , ("\\varbeta",ESymbol Alpha "\976")
  , ("\\varcarriagereturn",ESymbol Ord "\9166")
  , ("\\varclub",ESymbol Ord "\9831")
  , ("\\varclubsuit",ESymbol Ord "\9831")
  , ("\\vardiamond",ESymbol Ord "\9830")
  , ("\\vardiamondsuit",ESymbol Ord "\9830")
  , ("\\vardoublebarwedge",ESymbol Bin "\8966")
  , ("\\varheart",ESymbol Ord "\9829")
  , ("\\varheartsuit",ESymbol Ord "\9829")
  , ("\\varhexagon",ESymbol Ord "\11041")
  , ("\\varhexagonblack",ESymbol Ord "\11042")
  , ("\\varhexagonlrbonds",ESymbol Ord "\9004")
  , ("\\varisinobar",ESymbol Rel "\8950")
  , ("\\varisins",ESymbol Rel "\8947")
  , ("\\varkappa",ESymbol Alpha "\120600")
  , ("\\varlrtriangle",ESymbol Ord "\8895")
  , ("\\varniobar",ESymbol Rel "\8957")
  , ("\\varnis",ESymbol Rel "\8955")
  , ("\\varnothing",ESymbol Ord "\8709")
  , ("\\varnothing",ESymbol Ord "\8960")
  , ("\\varointclockwise",ESymbol Op "\8754")
  , ("\\varparallel",ESymbol Bin "\11005")
  , ("\\varpi",ESymbol Alpha "\120603")
  , ("\\varpi",ESymbol Alpha "\982")
  , ("\\varprod",ESymbol Op "\10761")
  , ("\\varpropto",ESymbol Rel "\8733")
  , ("\\varrho",ESymbol Alpha "\1009")
  , ("\\varrho",ESymbol Alpha "\120602")
  , ("\\varsdef",ESymbol Rel "\8796")
  , ("\\varsigma",ESymbol Alpha "\120589")
  , ("\\varsigma",ESymbol Alpha "\962")
  , ("\\varspade",ESymbol Ord "\9828")
  , ("\\varspadesuit",ESymbol Ord "\9828")
  , ("\\varstar",ESymbol Ord "\10038")
  , ("\\varsubsetneq",ESymbol Rel "\8842")
  , ("\\vartriangle",ESymbol Bin "\9653")
  , ("\\vartriangleleft",ESymbol Rel "\8882")
  , ("\\vartriangleright",ESymbol Rel "\8883")
  , ("\\varveebar",ESymbol Bin "\10849")
  , ("\\vbraceextender",ESymbol Ord "\9130")
  , ("\\vbrtri",ESymbol Rel "\10704")
  , ("\\vec",ESymbol Accent "\8401")
  , ("\\vec",ESymbol Accent "\8407")
  , ("\\vectimes",ESymbol Bin "\10799")
  , ("\\veebar",ESymbol Bin "\8891")
  , ("\\veedot",ESymbol Bin "\10183")
  , ("\\veedoublebar",ESymbol Bin "\10851")
  , ("\\veeeq",ESymbol Rel "\8794")
  , ("\\veemidvert",ESymbol Bin "\10843")
  , ("\\veeodot",ESymbol Bin "\10834")
  , ("\\veeonvee",ESymbol Bin "\10838")
  , ("\\veeonwedge",ESymbol Rel "\10841")
  , ("\\vert",ESymbol Fence "|")
  , ("\\vertoverlay",ESymbol Accent "\8402")
  , ("\\viewdata",ESymbol Ord "\8983")
  , ("\\virgo",ESymbol Ord "\9805")
  , ("\\vlongdash",ESymbol Rel "\10205")
  , ("\\vrectangle",ESymbol Ord "\9647")
  , ("\\vrectangleblack",ESymbol Ord "\9646")
  , ("\\vysmblkcircle",ESymbol Bin "\8729")
  , ("\\vysmblksquare",ESymbol Ord "\11037")
  , ("\\vysmwhtcircle",ESymbol Bin "\8728")
  , ("\\vysmwhtsquare",ESymbol Ord "\11038")
  , ("\\vzigzag",ESymbol Ord "\10650")
  , ("\\warning",ESymbol Ord "\9888")
  , ("\\wasylozenge",ESymbol Ord "\8977")
  , ("\\wasytherefore",ESymbol Ord "\8756")
  , ("\\wedgebar",ESymbol Bin "\10847")
  , ("\\wedgedot",ESymbol Bin "\10193")
  , ("\\wedgedoublebar",ESymbol Bin "\10848")
  , ("\\wedgemidvert",ESymbol Bin "\10842")
  , ("\\wedgeodot",ESymbol Bin "\10833")
  , ("\\wedgeonwedge",ESymbol Bin "\10837")
  , ("\\wedgeq",ESymbol Rel "\8793")
  , ("\\whitearrowupfrombar",ESymbol Ord "\8682")
  , ("\\whiteinwhitetriangle",ESymbol Ord "\10177")
  , ("\\whitepointerleft",ESymbol Ord "\9669")
  , ("\\whitepointerright",ESymbol Ord "\9659")
  , ("\\whitesquaretickleft",ESymbol Bin "\10212")
  , ("\\whitesquaretickright",ESymbol Bin "\10213")
  , ("\\whthorzoval",ESymbol Ord "\11053")
  , ("\\whtvertoval",ESymbol Ord "\11055")
  , ("\\wideangledown",ESymbol Ord "\10662")
  , ("\\wideangleup",ESymbol Ord "\10663")
  , ("\\widebridgeabove",ESymbol Accent "\8425")
  , ("\\widehat",ESymbol Accent "\770")
  , ("\\wideparen",ESymbol TOver "\9180")
  , ("\\widetilde",ESymbol Accent "\771")
  , ("\\wideutilde",ESymbol Accent "\816")
  , ("\\xbsol",ESymbol Op "\10745")
  , ("\\xsol",ESymbol Op "\10744")
  , ("\\yen",ESymbol Ord "\165")
  , ("\\yinyang",ESymbol Ord "\9775")
  , ("\\zcmp",ESymbol Op "\10783")
  , ("\\zhide",ESymbol Op "\10745")
  , ("\\zpipe",ESymbol Op "\10784")
  , ("\\zproject",ESymbol Op "\10785")
  , ("\\{",ESymbol Open "{")
  , ("\\|",ESymbol Fence "\8214")
  , ("\\}",ESymbol Close "}")
  ]

-- text mode parsing

textual :: TP T.Text
textual = regular <|> sps <|> ligature <|> textCommand
            <?> "text"

sps :: TP T.Text
sps = " " <$ skipMany1 (oneOf " \t\n")

regular :: TP T.Text
regular = T.pack <$> many1 (noneOf "`'-~${}\\ \t")

ligature :: TP T.Text
ligature = try ("\x2014" <$ string "---")
       <|> try ("\x2013" <$ string "--")
       <|> try (textStr "-")
       <|> try ("\x201C" <$ string "``")
       <|> try ("\x201D" <$ string "''")
       <|> try ("\x2019" <$ string "'")
       <|> try ("\x2018" <$ string "`")
       <|> try ("\xA0"   <$ string "~")

textCommand :: TP T.Text
textCommand = do
  cmd <- oneOfCommands (M.keys textCommands)
  optional $ try (char '{' >> spaces >> char '}')
  case M.lookup cmd textCommands of
       Nothing -> fail $ T.unpack $ "Unknown control sequence " <> cmd
       Just c  -> c

tok :: TP Char
tok = (try $ char '{' *> spaces *> anyChar <* spaces <* char '}')
   <|> anyChar

textCommands :: M.Map T.Text (TP T.Text)
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

parseC :: TP T.Text
parseC = try $ char '`' >> countChar 1 anyChar

-- the functions below taken from pandoc:

grave :: Char -> T.Text
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
grave c   = T.singleton c

acute :: Char -> T.Text
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
acute c   = T.singleton c

circ :: Char -> T.Text
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
circ c   = T.singleton c

tilde :: Char -> T.Text
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
tilde c   = T.singleton c

umlaut :: Char -> T.Text
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
umlaut c   = T.singleton c

dot :: Char -> T.Text
dot 'C' = "Ċ"
dot 'c' = "ċ"
dot 'E' = "Ė"
dot 'e' = "ė"
dot 'G' = "Ġ"
dot 'g' = "ġ"
dot 'I' = "İ"
dot 'Z' = "Ż"
dot 'z' = "ż"
dot c   = T.singleton c

macron :: Char -> T.Text
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
macron c   = T.singleton c

cedilla :: Char -> T.Text
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
cedilla c   = T.singleton c

hacek :: Char -> T.Text
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
hacek c   = T.singleton c

breve :: Char -> T.Text
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
breve c   = T.singleton c
