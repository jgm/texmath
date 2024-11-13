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

import Data.List (intercalate, intersperse, find, foldl')
import Control.Monad
import Data.Char (isDigit, isAscii, isLetter)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Text.Parsec hiding (label)
import Text.Parsec.Error
import Text.Parsec.Text
import Text.TeXMath.Types
import Data.Functor (($>))
import qualified Text.TeXMath.Shared as S
import Text.TeXMath.Readers.TeX.Macros (applyMacros, parseMacroDefinitions)
import Text.TeXMath.Unicode.ToTeX (getSymbolType)
import Text.TeXMath.Unicode.ToUnicode (toUnicode)
import Text.TeXMath.Shared (getSpaceChars)
import Data.Generics (everywhere, mkT)
import Text.TeXMath.Readers.TeX.Commands ( styleOps, textOps, enclosures,
                                           operators, symbols, siUnitMap )
import Data.Text.Read (decimal)

type TP = Parser

-- The parser

expr1 :: TP Exp
expr1 = choice
          [ inbraces
          , variable
          , number
          , unicode
          , operator
          , bareSubSup
          , enclosure
          , hyperref
          , command
          ] <* ignorable

-- | Parse a formula, returning a list of 'Exp'.
readTeX :: Text -> Either Text [Exp]
readTeX inp =
  let (ms, rest) = parseMacroDefinitions inp in
  either (Left . showParseError inp) (Right . fixBinList)
    $ parse formula "formula" $ applyMacros ms rest
 where
  -- | Convert Bin symbol type in certain contexts (#176, #234).
  fixBinList :: [Exp] -> [Exp]
  fixBinList =
    reverse . foldl' goExp [] . everywhere (mkT fixBins)

  -- TeXBook:
  -- 5. If the current item is a Bin atom, and if this was the first
  -- atom in the list, or if the most recent previous atom was Bin, Op,
  -- Rel, Open, or Punct, change the current Bin to Ord and continue with
  -- Rule 14. Otherwise continue with Rule 17.
  -- 6. If the current item is a Rel or Close or Punct atom, and if
  -- the most recent previous atom was Bin, change that previous Bin
  -- to Ord. Continue with Rule 17.

  fixBins :: Exp -> Exp
  fixBins e =
    case e of
      EGrouped es
        -> EGrouped (reverse $ foldl' goExp [] es)
      EDelimited op cl des
        -> EDelimited op cl (reverse $ foldl' goInDel [] des)
      EArray als alines
        -> EArray als (map (map (reverse . foldl' goExp [])) alines)
      EStyled tt es
        -> EStyled tt (reverse $ foldl' goExp [] es)
      _ -> e

  goExp :: [Exp] -> Exp -> [Exp]
  goExp [] (ESymbol Bin t) = [ESymbol Ord t]
  goExp  accum@(ESymbol ty _ : _) (ESymbol Bin t)
    | ty `elem` [Bin, Op, Rel, Open, Pun]
      = ESymbol Ord t : accum
  goExp (ESymbol Bin t' : rest) (ESymbol ty t)
    | ty `elem` [Rel, Close, Pun]
      = ESymbol ty t : ESymbol Ord t' : rest
  goExp xs x = x : xs

  goInDel :: [InEDelimited] -> InEDelimited -> [InEDelimited]
  goInDel [] (Right (ESymbol Bin t)) = [Right (ESymbol Ord t)]
  goInDel accum@(Left _ : _) (Right (ESymbol Bin t))
    = Right (ESymbol Ord t) : accum
  goInDel accum@(Right (ESymbol ty _) : _) (Right (ESymbol Bin t))
    | ty `elem` [Bin, Op, Rel, Open, Pun]
      = Right (ESymbol Ord t) : accum
  goInDel (Right (ESymbol Bin t') : rest) (Right (ESymbol ty t))
    | ty `elem` [Rel, Close, Pun]
      = Right (ESymbol ty t) : Right (ESymbol Ord t') : rest
  goInDel xs x = x : xs

showParseError :: Text -> ParseError -> Text
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

anyCtrlSeq :: TP Text
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
    <|> tag
    <|> () <$ ctrlseq "nonumber"
    <|> (skipMany1 space <?> "whitespace")
    <|> (() <$ ctrlseq "allowbreak")

comment :: TP ()
comment = char '%' *> skipMany (noneOf "\n") *> optional newline

label :: TP ()
label = ctrlseq "label" *> braces (skipMany (noneOf "}"))

tag :: TP ()
tag = ctrlseq "tag" *> optional (char '*') *> braces (skipMany (noneOf "}"))

unGrouped :: Exp -> [Exp]
unGrouped (EGrouped xs) = xs
unGrouped x = [x]

formula :: TP [Exp]
formula = unGrouped <$> (ignorable *> manyExp expr <* eof)

expr :: TP Exp
expr = do
  optional (ctrlseq "displaystyle" <|> ctrlseq "textstyle" <|>
            ctrlseq "scriptstyle" <|> ctrlseq "scriptscriptstyle")
  (a, convertible) <- try (braces operatorname) -- needed because macros add {}
                 <|> operatorname
                 <|> ((,False) <$> expr1)
  limits <- limitsIndicator
  subSup limits convertible a <|> superOrSubscripted limits convertible a <|> return a

hyperref :: TP Exp
hyperref = do
  ctrlseq "hyperref"  -- we just ignore hyperref, see #186
  optional inbrackets
  inbraces

command :: TP Exp
command = try $ do
  c <- anyCtrlSeq
  guard $ c /= "\\end" -- handled in environment
       && c /= "\\operatorname" -- handled in expr
  choice
    [ text c
    , styled c
    , colored c
    , root c
    , xspace c
    , mathop c
    , phantom c
    , boxed c
    , binary c
    , genfrac c
    , substack c
    , environment c
    , ensuremath c
    , scaled c
    , negated c
    , siunitx c
    , arrow c
    , tSymbol c
    ] <|> unexpected ("control sequence " <> T.unpack c)


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
    tok' <- texToken
    tok'' <- (EMathOperator <$> (expToOperatorName tok'))
           <|> return (EStyled TextNormal [tok'])
    return (tok'', convertible)

-- | Converts identifiers, symbols and numbers to a flat string.
-- Fails if the expression contains anything else.
expToOperatorName :: MonadPlus m => Exp -> m Text
expToOperatorName e = case e of
            EGrouped xs -> T.concat <$> mapM (toStr TextNormal) xs
            _ -> toStr TextNormal e
    where
          toStr sty (EIdentifier s)     = return $ toUnicode sty s
          toStr _   (EText sty' s)      = return $ toUnicode sty' s
          toStr sty (ENumber s)         = return $ toUnicode sty s
          toStr sty (EMathOperator s)   = return $ toUnicode sty s
          -- handle special characters
          toStr _ (ESymbol _ "\x2212")  = return "-"
          toStr _ (ESymbol _ "\x2032")  = return "'"
          toStr _ (ESymbol _ "\x2033")  = return "''"
          toStr _ (ESymbol _ "\x2034")  = return "'''"
          toStr _ (ESymbol _ "\x2057")  = return "''''"
          toStr _ (ESymbol _ "\x02B9")  = return "'"
          toStr sty (ESymbol _ s)       = return $ toUnicode sty s
          toStr _ (ESpace n)            = return $ getSpaceChars n
          toStr _ (EStyled sty' exps)   = T.concat <$>
                                            sequence (map (toStr sty') exps)
          toStr _   _                   = mzero

bareSubSup :: TP Exp
bareSubSup = subSup Nothing False (EIdentifier "")
  <|> superOrSubscripted Nothing False (EIdentifier "")

limitsIndicator :: TP (Maybe Bool)
limitsIndicator =
   (ctrlseq "limits" >> return (Just True))
  <|> (ctrlseq "nolimits" >> return (Just False))
  <|> return Nothing

binomCmd :: TP Text
binomCmd = oneOfCommands (M.keys binomCmds)

binomCmds :: M.Map Text (Exp -> Exp -> Exp)
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

genfrac :: Text -> TP Exp
genfrac "\\genfrac" = do
  let opener = option "" $
                T.singleton <$> ((char '\\' >> anyChar) <|> anyChar)
  let closer = option "" $
                T.singleton <$> ((char '\\' >> anyChar) <|> anyChar)
  openDelim <- braces opener <|> opener
  closeDelim <- braces closer <|> closer
  bar <- False <$ try (braces (string "0pt")) <|> True <$ texToken
  displayStyle <- True <$ try (braces (char '0')) <|> False <$ texToken
  x <- texToken
  y <- texToken
  let fracType = case (bar, displayStyle) of
                      (False, _)   -> NoLineFrac
                      (True, True) -> DisplayFrac
                      _            -> NormalFrac
  return $ EDelimited openDelim closeDelim
                      [Right (EFraction fracType x y)]
genfrac _ = mzero

substack :: Text -> TP Exp
substack "\\substack" = do
  formulas <- braces $ ignorable >> (manyExp expr) `sepEndBy` endLine
  return $ EArray [AlignCenter] $ map (\x -> [[x]]) formulas
substack _ = mzero

asGroup :: [Exp] -> Exp
asGroup [x] = x
asGroup xs = EGrouped xs

-- variant of many that is sensitive to \choose and other such commands
manyExp' :: Bool -> TP Exp -> TP Exp
manyExp' requireNonempty p = do
  initial <- if requireNonempty
                then many1 (notFollowedBy binomCmd >> p)
                else many (notFollowedBy binomCmd >> p)
  let withCmd :: Text -> TP Exp
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
texToken = texSymbol <|> inbraces <|> texChar

-- Remove superfluous EGrouped if present.
deGroup :: Exp -> Exp
deGroup (EGrouped [x]) = x
deGroup x = x

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
enclosure = delimited <|> delimitedImplicit <|> basicEnclosure

basicEnclosure :: TP Exp
basicEnclosure = try $ do
  possibleEncl <- lexeme (anyCtrlSeq <|> countChar 1 (oneOf "()[]|"))
  case M.lookup possibleEncl enclosures of
       Just x  -> return x
       Nothing -> mzero

fence :: String -> TP Text
fence cmd = do
  symbol cmd
  let nullDelim = try (ESymbol Open "" <$ symbol ".")
      angleDelim = try $ choice
        [ ESymbol Open "\x27E8" <$ symbol "<"
        , ESymbol Close "\x27E9" <$ symbol ">"
        ]
  enc <- basicEnclosure <|> nullDelim <|> angleDelim
  case enc of
       ESymbol Open x  -> return x
       ESymbol Close x -> return x
       _ -> mzero

middle :: TP Text
middle = fence "\\middle"

right :: TP Text
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

delimitedImplicit :: TP Exp
delimitedImplicit = try $ do
  (opent, closer) <-
           (("(", symbol ")") <$ symbol "(")
       <|> (("[", symbol "]") <$ symbol "[")
       <|> (("|", symbol "|") <$ symbol "|")
       <|> (("\x2016", "\x2016" <$ ctrlseq "rVert") <$ ctrlseq "lVert")
  contents <- concat <$>
              many (try $ ((:[]) . Left  <$> middle)
                      <|> (map Right . unGrouped <$>
                             many1Exp (notFollowedBy closer *> expr)))
  closet <- T.pack <$> closer
  return $ EDelimited opent closet contents

scaled :: Text -> TP Exp
scaled cmd = do
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
arrayLine =
  sepBy1
    (ignorable' *>
      (unGrouped <$>
        manyExp (try (notFollowedBy (('\n' <$ ctrlseq "end") <|> endLine)) *>
                   expr <* ignorable'))) (symbol "&")
  where ignorable' = ignorable >> optional (try (ctrlseq "hline" >> ignorable'))
  -- we don't represent the line, but it shouldn't crash parsing

arrayAlignments :: TP [Alignment]
arrayAlignments = mconcat <$>
  braces (many (
                ((:[]) . letterToAlignment <$> letter)
            <|> ([] <$ char '|')
            <|> ([] <$ oneOf " \t")
            <|> ([] <$ ((char '@' <|> char '!') <* inbraces))
            <|> (do char '*'
                    num <- T.pack <$> braces (many1 digit)
                    cols <- arrayAlignments
                    case decimal num of
                      Left msg -> fail msg
                      Right (n :: Int, _)
                               -> return $ mconcat $ replicate n cols)
               ))
 where
   letterToAlignment 'l' = AlignLeft
   letterToAlignment 'c' = AlignCenter
   letterToAlignment 'r' = AlignRight
   letterToAlignment _   = AlignCenter

environment :: Text -> TP Exp
environment "\\begin" = do
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
environment _ = mzero

environments :: M.Map Text (TP Exp)
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
  , ("multlined", gather)
  , ("gather", gather)
  , ("gathered", gather)
  , ("equation", equation)
  ]

alignsFromRows :: Alignment -> [ArrayLine] -> [Alignment]
alignsFromRows _ [] = []
alignsFromRows defaultAlignment (r:_) = replicate (length r) defaultAlignment

matrixWith :: Text -> Text -> TP Exp
matrixWith opendelim closedelim = do
  lines' <- sepEndBy arrayLine endLineAMS
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
                         , "min","max","Pr","det","gcd" ]
isConvertible (ESymbol Op x) = x `elem` convertibleSyms
  where convertibleSyms =
         ["\x2211", "\x220F", "\x2210", -- \sum \prod \amalg
          "\x22C0", "\x22C1", -- bigwedge \bigvee
          "\x22C2", "\x22C3", -- \bigcap \bigcup
          "\x2A05", "\x2A06"] -- \bigsqcap \bisqcup
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

ensuremath :: Text -> TP Exp
ensuremath "\\ensuremath" = inbraces
ensuremath _ = mzero

phantom :: Text -> TP Exp
phantom "\\phantom" = EPhantom <$> texToken
phantom _ = mzero

boxed :: Text -> TP Exp
boxed "\\boxed" = EBoxed <$> texToken
boxed _ = mzero

text :: Text -> TP Exp
text c = do
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

styled :: Text -> TP Exp
styled c = do
  case M.lookup c styleOps of
       Just f   -> do
         x <- texSymbol <|> inbraces <|> texChar
         return $ case x of
                       EGrouped xs -> f xs
                       _           -> f [x]
       Nothing  -> mzero

colored :: Text -> TP Exp
colored "\\color" = do
  _ <- inbraces -- skip the color
  -- in the future we might add color to the types or to the styles
  texSymbol <|> inbraces <|> texChar
colored _ = mzero

-- note: sqrt can be unary, \sqrt{2}, or binary, \sqrt[3]{2}
root :: Text -> TP Exp
root c = do
  guard $ c == "\\sqrt" || c == "\\surd"
  (ERoot <$> inbrackets <*> texToken) <|> (ESqrt <$> texToken)

xspace :: Text -> TP Exp
xspace "\\enspace" = return $ ESpace (1/2)
xspace "\\mspace" =
  braces $ do
    len <- many1 digit
    lexeme $ string "mu"
    case reads len of
       ((n :: Integer,[]):_) -> return $ ESpace (fromIntegral n/18)
       _                     -> mzero
xspace "\\hspace" = do
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
xspace _ = mzero

mathop :: Text -> TP Exp
mathop c =
  case c of
    "\\mathop"    -> mathopWith Op
    "\\mathrel"   -> mathopWith Rel
    "\\mathbin"   -> mathopWith Bin
    "\\mathord"   -> mathopWith Ord
    "\\mathopen"  -> mathopWith Open
    "\\mathclose" -> mathopWith Close
    "\\mathpunct" -> mathopWith Pun
    _              -> mzero

mathopWith :: TeXSymbolType -> TP Exp
mathopWith ty = do
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

binary :: Text -> TP Exp
binary c = do
  case c of
     "\\overset"  -> do
       a <- texToken
       b <- texToken
       return $ EOver False b a
     "\\stackrel" -> do
       a <- texToken
       b <- texToken
       return $ EOver False b a
     "\\underset" -> do
       a <- texToken
       b <- texToken
       return $ EUnder False b a
     "\\frac"     -> EFraction NormalFrac <$> texToken <*> texToken
     "\\tfrac"    -> EFraction InlineFrac <$> texToken <*> texToken
     "\\dfrac"    -> EFraction DisplayFrac <$> texToken <*> texToken
     "\\binom"    -> do
       a <- texToken
       b <- texToken
       return $ EDelimited "(" ")" [Right (EFraction NoLineFrac a b)]
     _            -> mzero

texSymbol :: TP Exp
texSymbol = operator <|>
            try (do c <- anyCtrlSeq
                    tSymbol c <|> negated c)

negated :: Text -> TP Exp
negated "\\not" = do
  sym <- texSymbol <|> texChar
  case sym of
    ESymbol Rel x -> return $ ESymbol Rel $ toNeg x
    EText tt x    -> return $ EText tt $ toNeg x
    ENumber x     -> return $ ENumber $ toNeg x
    EIdentifier x -> return $ EIdentifier $ toNeg x
    _             -> mzero
negated _ = mzero

toNeg :: Text -> Text
toNeg x = case x of
            "\x2203" -> "\x2204"
            "\x2208" -> "\x2209"
            "\x220B" -> "\x220C"
            "\x2223" -> "\x2224"
            "\x2225" -> "\x2226"
            "\x2243" -> "\x2244"
            "\x2245" -> "\x2246"
            "\x2248" -> "\x2249"
            "="      -> "\x2260"
            "\x2261" -> "\x2262"
            "<"      -> "\x226E"
            ">"      -> "\x226F"
            "\x2264" -> "\x2270"
            "\x2265" -> "\x2271"
            "\x2272" -> "\x2274"
            "\x2273" -> "\x2275"
            "\x227A" -> "\x2280"
            "\x227B" -> "\x2281"
            "\x2282" -> "\x2284"
            "\x2283" -> "\x2285"
            "\x2286" -> "\x2288"
            "\x2287" -> "\x2289"
            "\x227C" -> "\x22E0"
            "\x227D" -> "\x22E1"
            "\x2291" -> "\x22E2"
            "\x2292" -> "\x22E3"
            _        -> x <> "\x0338"


oneOfCommands :: [Text] -> TP Text
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

oneOfStrings' :: (Char -> Char -> Bool) -> [(String, Text)] -> TP Text
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
oneOfStrings :: [Text] -> TP Text
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

tSymbol :: Text -> TP Exp
tSymbol sym =
  case M.lookup sym symbols of
       Just acc@(ESymbol Accent _) ->
         (\t -> EOver False t acc) <$> texToken
       Just acc@(ESymbol TUnder _) ->
         (\t -> EUnder False t acc) <$> texToken
       Just acc@(ESymbol TOver _) ->
         (\t -> EOver False t acc) <$> texToken
       Just x  -> return x
       Nothing
         | sym == "\\mod" -> do
             x <- deGroup <$> expr
             return $ EGrouped
               [ESpace (8/18), EMathOperator "mod", ESpace (4/18), x]
         | sym == "\\bmod" -> do
             x <- deGroup <$> expr
             return $ EGrouped
               [ESpace (4/18), EMathOperator "mod", ESpace (4/18), x]
         | sym == "\\pmod" -> do
             x <- deGroup <$> expr
             return $ EGrouped
               [ESpace (4/18), ESymbol Open "(", EMathOperator "mod",
                ESpace (4/18), x, ESymbol Close ")"]
         | sym == "\\pod"  -> do
             x <- deGroup <$> expr
             return $ EGrouped
               [ESpace (4/18), ESymbol Open "(", x, ESymbol Close ")"]
         | otherwise -> mzero

operator :: TP Exp
operator = do
  sym <- lexeme (oneOfStrings $ M.keys operators)
  return $ fromJust (M.lookup sym operators)

lexeme :: TP a -> TP a
lexeme p = p <* ignorable

braces :: TP a -> TP a
braces p = lexeme $ char '{' *> spaces *> p <* spaces <* char '}'

brackets :: TP a -> TP a
brackets p = lexeme $ char '[' *> spaces *> p <* spaces <* char ']'

textStr :: Text -> TP Text
textStr t = string (T.unpack t) $> t

countChar :: Int -> TP Char -> TP Text
countChar n = fmap T.pack . count n

symbol :: String -> TP String
symbol s = lexeme $ try $ string s

-- text mode parsing

textual :: TP Text
textual = regular <|> sps <|> ligature <|> textCommand
            <?> "text"

sps :: TP Text
sps = " " <$ skipMany1 (oneOf " \t\n")

regular :: TP Text
regular = T.pack <$> many1 (noneOf "`'-~${}\\ \t")

ligature :: TP Text
ligature = try ("\x2014" <$ string "---")
       <|> try ("\x2013" <$ string "--")
       <|> try (textStr "-")
       <|> try ("\x201C" <$ string "``")
       <|> try ("\x201D" <$ string "''")
       <|> try ("\x2019" <$ string "'")
       <|> try ("\x2018" <$ string "`")
       <|> try ("\xA0"   <$ string "~")

textCommand :: TP Text
textCommand = do
  cmd <- oneOfCommands (M.keys textCommands)
  optional $ try (char '{' >> spaces >> char '}')
  case M.lookup cmd textCommands of
       Nothing -> fail $ T.unpack $ "Unknown control sequence " <> cmd
       Just c  -> c

tok :: TP Char
tok = (try $ char '{' *> spaces *> anyChar <* spaces <* char '}')
   <|> anyChar

textCommands :: M.Map Text (TP Text)
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

parseC :: TP Text
parseC = try $ char '`' >> countChar 1 anyChar

-- the functions below taken from pandoc:

grave :: Char -> Text
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

acute :: Char -> Text
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

circ :: Char -> Text
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

tilde :: Char -> Text
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

umlaut :: Char -> Text
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

dot :: Char -> Text
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

macron :: Char -> Text
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

cedilla :: Char -> Text
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

hacek :: Char -> Text
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

breve :: Char -> Text
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

-- siunitx

siunitx :: Text -> TP Exp
siunitx c = do
  case c of
    "\\si"       -> dosi
    "\\unit"     -> dosi  -- v3 version of \si
    "\\SI"       -> doSI
    "\\qty"      -> doSI  -- v3 version of \SI
    "\\SIrange"  -> doSIrange True
    "\\qtyrange" -> doSIrange True -- v3 version of SIrange
    "\\numrange" -> doSIrange False
    "\\numlist"  -> doSInumlist
  -- "\\SIlist"   -> doSIlist -- v3 version of SIlist
  -- "\\qtylist"  -> doSIlist -- v3 version of SIlist
    "\\num"      -> doSInum
    "\\ang"      -> doSIang
    _          -> mzero

-- converts e.g. \SIrange{100}{200}{\ms} to "100 ms--200 ms"
doSIrange :: Bool -> TP Exp
doSIrange includeUnits = do
  optional $ skipMany inbrackets
  startvalue <- Just <$> doSInum
  startvalueprefix <- option Nothing $ Just <$> brackets expr
  stopvalue <- Just <$> doSInum
  stopvalueprefix <- option Nothing $ Just <$> brackets expr
  unit <- if includeUnits
             then option Nothing $ Just <$> dosi
             else return Nothing
  return $ EGrouped $ catMaybes
           [startvalueprefix,
            emptyOr160 startvalueprefix,
            startvalue,
            emptyOr160 unit,
            unit,
            Just (EText TextNormal "\8211"), -- An en-dash
            stopvalueprefix,
            emptyOr160 stopvalueprefix,
            stopvalue,
            emptyOr160 unit,
            unit]


doSInumlist :: TP Exp
doSInumlist = do
  optional $ skipMany inbrackets
  xs <- braces (sepBy siNum (spaces *> char ';' <* spaces))
  return $
    case xs of
      []  -> EGrouped []
      [x] -> x
      _   -> EGrouped $
               intersperse (EText TextNormal ", ") (init xs) ++
               [EText TextNormal ", & ", last xs]

dosi :: TP Exp
dosi = siUnit <|> braces (manyExp (siUnit <|> expr))

doSIang :: TP Exp
doSIang = do
  optional $ skipMany inbrackets
  ps <- braces $ sepBy siNum (spaces *> char ';' <* spaces)
  return $ EGrouped $
    case ps ++ repeat (EGrouped []) of
      (d:m:s:_) ->
        (if d == EGrouped [] then [] else [d, EText TextNormal "\xb0"]) <>
        (if m == EGrouped [] then [] else [m, EText TextNormal "\x2032"]) <>
        (if s == EGrouped [] then [] else [s, EText TextNormal "\x2033"])
      _ -> []

-- converts e.g. \SI{1}[\$]{} to "$ 1" or \SI{1}{\euro} to "1 €"
doSI :: TP Exp
doSI = do
  optional $ skipMany inbrackets
  value <- Just <$> doSInum
  valueprefix <- option Nothing $ do
                    x <- inbrackets
                    if x == EGrouped []
                       then return Nothing
                       else return $ Just x
  unit <- option Nothing $ Just <$> dosi
  return $ EGrouped $ catMaybes
         [ valueprefix,
           emptyOr160 valueprefix,
           value,
           emptyOr160 unit,
           unit
         ]

emptyOr160 :: Maybe Exp -> Maybe Exp
emptyOr160 (Just _) = Just (ESpace (4/18))
emptyOr160 Nothing  = Nothing


siUnit :: TP Exp
siUnit = try $ do
  name <- T.dropWhile (=='\\') <$> anyCtrlSeq
  case name of
    "square" -> do
       unit <- siUnit
       return $ ESuper unit (ENumber "2")
    "cubic" -> do
       unit <- siUnit
       return $ ESuper unit (ENumber "3")
    "raisetothe" -> do
       n <- expr
       unit <- siUnit
       return $ ESuper unit n
    _ ->
       case M.lookup name siUnitMap of
            Just il ->
              option il $
                choice
                 [ (ESuper il (ENumber "2")) <$ ctrlseq "squared"
                 , (ESuper il (ENumber "3")) <$ ctrlseq "cubed"
                 , (\n -> ESuper il n) <$> (ctrlseq "tothe" *> expr)
                 ]
            Nothing -> fail "not an siunit unit command"

doSInum :: TP Exp
doSInum = do
  optional $ skipMany inbrackets
  braces siNum

siNum :: TP Exp
siNum = asGroup . mconcat <$> many parseNumPart

parseNumPart :: TP [Exp]
parseNumPart =
  parseDecimalNum <|>
  parseComma <|>
  parsePlusMinus <|>
  parseI <|>
  parseExp <|>
  parseX <|>
  parseSpace
 where
  parseDecimalNum = do
    pref <- option mempty $ (mempty <$ char '+') <|> ("\x2212" <$ char '-')
    basenum <- (pref <>) . T.pack
                <$> many1 (satisfy (\c -> isDigit c || c == '.'))
    uncertainty <- option mempty $ T.pack <$> parseParens
    if T.null uncertainty
       then return [ENumber basenum]
       else return [ENumber $ basenum <> "\xa0\xb1\xa0" <>
             let (_,ys) = T.break (=='.') basenum
              in case (T.length ys - 1, T.length uncertainty) of
                   (0,_) -> uncertainty
                   (x,y)
                     | x > y  -> "0." <> T.replicate (x - y) "0" <>
                                      T.dropWhileEnd (=='0') uncertainty
                     | otherwise -> T.take (y - x) uncertainty <>
                                      case T.dropWhileEnd (=='0')
                                             (T.drop (y - x) uncertainty) of
                                             t | T.null t -> mempty
                                               | otherwise -> "." <> t]
  parseComma = [ENumber "."] <$ char ','
  parsePlusMinus = [EText TextNormal "\xa0\xb1\xa0"] <$ try (string "+-")
  parseParens =
    char '(' *> many1 (satisfy (\c -> isDigit c || c == '.')) <* char ')'
  parseI = [EIdentifier "i"] <$ char 'i'
  parseX = [ESymbol Rel "\xa0\xd7\xa0"] <$ char 'x'
  parseExp = do
    n <- asGroup <$> (char 'e' *> parseDecimalNum)
    return $ [ESymbol Rel "\xa0\xd7\xa0", ESuper (ENumber "10") n ]
  parseSpace = mempty <$ skipMany1 (char ' ')

arrow :: Text -> TP Exp
arrow c = case c of
  "\\xleftarrow"   -> underoverarrow "\x2190"
  "\\xrightarrow"  -> underoverarrow "\x2192"
  _                -> mzero
 where
  underoverarrow s = do
    munder <- optionMaybe inbrackets
    over <- texToken
    return $ case munder of
      Nothing    -> EOver False (ESymbol Op s) over
      Just under -> EUnderover False (ESymbol Op s) under over

