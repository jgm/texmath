{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-
Copyright (C) 2010 John MacFarlane <jgm@berkeley.edu>

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

{- | Functions for parsing LaTeX macro definitions and applying macros
 to LateX expressions.
-}

module Text.TeXMath.Readers.TeX.Macros
                           ( Macro
                           , parseMacroDefinitions
                           , pMacroDefinition
                           , applyMacros
                           )
where

import Data.Char (isDigit, isLetter)
import qualified Data.Text as T
import Control.Monad
import Text.Parsec

data Macro = Macro { macroDefinition :: T.Text
                   , macroParser     :: forall st m s . Stream s m Char =>
                          ParsecT s st m T.Text }

instance Show Macro where
  show m = "Macro " ++ show (macroDefinition m)

-- | Parses a string for a list of macro definitions, optionally
-- separated and ended by spaces and TeX comments.  Returns
-- the list of macros (which may be empty) and the unparsed
-- portion of the input string.
parseMacroDefinitions :: T.Text -> ([Macro], T.Text)
parseMacroDefinitions s =
  case parse pMacroDefinitions "input" s of
       Left _       -> ([], s)
       Right res    -> res

-- | Parses one or more macro definitions separated by comments & space.
-- Return list of macros parsed + remainder of string.
pMacroDefinitions :: (Monad m, Stream s m Char)
                  => ParsecT s st m ([Macro], s)
pMacroDefinitions = do
  pSkipSpaceComments
  defs <- sepEndBy pMacroDefinition pSkipSpaceComments
  rest <- getInput
  return (reverse defs, rest)  -- reversed so later macros shadow earlier

-- | Parses a @\\newcommand@ or @\\renewcommand@ macro definition and
-- returns a 'Macro'.
pMacroDefinition :: (Monad m, Stream s m Char)
                 => ParsecT s st m Macro
pMacroDefinition = newcommand <|> declareMathOperator <|> newenvironment

-- | Skip whitespace and comments.
pSkipSpaceComments :: (Monad m, Stream s m Char)
                   => ParsecT s st m ()
pSkipSpaceComments = spaces >> skipMany (comment >> spaces)

-- | Applies a list of macros to a string recursively until a fixed
-- point is reached.  If there are several macros in the list with the
-- same name, earlier ones will shadow later ones.
applyMacros :: [Macro] -> T.Text -> T.Text
applyMacros [] s = s
applyMacros ms s =
  maybe s id $ iterateToFixedPoint ((2 * length ms) + 1)
    (applyMacrosOnce ms) s

------------------------------------------------------------------------------

iterateToFixedPoint :: Eq a => Int -> (a -> Maybe a) -> a -> Maybe a
iterateToFixedPoint 0     _ _ = Nothing
  -- Macro application did not terminate in a reasonable time, possibly
  -- because of a loop in the macro.
iterateToFixedPoint limit f x =
  case f x of
       Nothing       -> Nothing
       Just y
         | y == x    -> Just y
         | otherwise -> iterateToFixedPoint (limit - 1) f y

applyMacrosOnce :: [Macro] -> T.Text -> Maybe T.Text
applyMacrosOnce ms s =
  case parse (many tok) "input" s of
       Right r -> Just $ T.concat r
       Left _  -> Nothing
    where tok = try $ do
                  skipComment
                  choice [ choice (map (\m -> macroParser m) ms)
                         , T.pack <$> ctrlseq
                         , T.pack <$> count 1 anyChar ]

ctrlseq :: (Monad m, Stream s m Char)
        => ParsecT s st m String
ctrlseq = do
          char '\\'
          res <- many1 letter <|> count 1 anyChar
          return $ '\\' : res

newcommand :: (Monad m, Stream s m Char)
           => ParsecT s st m Macro
newcommand = try $ do
  char '\\'
  -- we ignore differences between these so far:
  try (string "newcommand")
    <|> try (string "renewcommand")
    <|> string "providecommand"
  optional (char '*')
  pSkipSpaceComments
  name <- inbraces <|> ctrlseq
  guard (take 1 name == "\\")
  let name' = drop 1 name
  numargs <- numArgs
  pSkipSpaceComments
  optarg <- if numargs > 0
               then optArg
               else return Nothing
  let numargs' = case optarg of
                   Just _  -> numargs - 1
                   Nothing -> numargs
  pSkipSpaceComments
  body <- inbraces <|> ctrlseq
  let defn = "\\newcommand{" ++ name ++ "}" ++
             (if numargs > 0 then ("[" ++ show numargs ++ "]") else "") ++
             case optarg of { Nothing -> ""; Just x -> "[" ++ x ++ "]"} ++
             "{" ++ body ++ "}"
  return $ Macro (T.pack defn) $ fmap T.pack $ try $ do
    char '\\'
    string name'
    when (all isLetter name') $
      notFollowedBy letter
    pSkipSpaceComments
    opt <- case optarg of
                Nothing  -> return Nothing
                Just _   -> liftM (`mplus` optarg) optArg
    args <- count numargs' (pSkipSpaceComments >>
                  (inbraces <|> ctrlseq <|> count 1 anyChar))
    let args' = case opt of
                     Just x  -> x : args
                     Nothing -> args
    return $ apply args' $ "{" ++ body ++ "}"

newenvironment :: (Monad m, Stream s m Char)
               => ParsecT s st m Macro
newenvironment = try $ do
  char '\\'
  -- we ignore differences between these so far:
  optional (string "re")
  string "newenvironment"
  optional (char '*')
  pSkipSpaceComments
  name <- inbraces <|> ctrlseq
  numargs <- numArgs
  pSkipSpaceComments
  optarg <- if numargs > 0
               then optArg <* pSkipSpaceComments
               else return Nothing
  let numargs' = case optarg of
                   Just _  -> numargs - 1
                   Nothing -> numargs
  opener <- inbraces <|> ctrlseq
  pSkipSpaceComments
  closer <- inbraces <|> ctrlseq
  let defn = "\\newenvironment{" ++ name ++ "}" ++
             (if numargs > 0 then ("[" ++ show numargs ++ "]") else "") ++
             case optarg of { Nothing -> ""; Just x -> "[" ++ x ++ "]"} ++
             "%\n{" ++ opener ++ "}%\n" ++ "{" ++ closer ++ "}"
  return $ Macro (T.pack defn) $ fmap T.pack $ try $ do
    string "\\begin"
    pSkipSpaceComments
    char '{'
    string name
    pSkipSpaceComments
    char '}'
    opt <- case optarg of
                Nothing  -> return Nothing
                Just _   -> liftM (`mplus` optarg) optArg
    args <- count numargs' (pSkipSpaceComments >>
                  (inbraces <|> ctrlseq <|> count 1 anyChar))
    let args' = case opt of
                     Just x  -> x : args
                     Nothing -> args
    let ender = try $ do
                      string "\\end"
                      pSkipSpaceComments
                      char '{'
                      string name
                      char '}'
    body <- manyTill anyChar ender
    return $ apply args'
           $ opener ++ body ++ closer

-- | Parser for \DeclareMathOperator(*) command.
declareMathOperator :: (Monad m, Stream s m Char)
                    => ParsecT s st m Macro
declareMathOperator = try $ do
  string "\\DeclareMathOperator"
  pSkipSpaceComments
  star <- option "" (string "*")
  pSkipSpaceComments
  name <- inbraces <|> ctrlseq
  guard (take 1 name == "\\")
  let name' = drop 1 name
  pSkipSpaceComments
  body <- inbraces <|> ctrlseq
  let defn = "\\DeclareMathOperator" ++ star ++ "{" ++ name ++ "}" ++
             "{" ++ body ++ "}"
  return $ Macro (T.pack defn) $ fmap T.pack $ try $ do
    char '\\'
    string name'
    when (all isLetter name') $
      notFollowedBy letter
    pSkipSpaceComments
    return $ "\\operatorname" ++ star ++ "{" ++ body ++ "}"


apply :: [String] -> String -> String
apply args ('#':d:xs) | isDigit d, d /= '0' =
  let argnum = read [d]
  in  if length args >= argnum
         then args !! (argnum - 1) ++ apply args xs
         else '#' : d : apply args xs
apply args ('\\':'#':xs) = '\\':'#' : apply args xs
apply args (x:xs) = x : apply args xs
apply _ "" = ""

skipComment :: (Monad m, Stream s m Char)
            => ParsecT s st m ()
skipComment = skipMany comment

comment :: (Monad m, Stream s m Char)
        => ParsecT s st m ()
comment = do
  char '%'
  skipMany (notFollowedBy newline >> anyChar)
  newline
  return ()

numArgs :: (Monad m, Stream s m Char)
        => ParsecT s st m Int
numArgs = option 0 $ try $ do
  pSkipSpaceComments
  char '['
  pSkipSpaceComments
  n <- digit
  pSkipSpaceComments
  char ']'
  return $ read [n]

optArg :: (Monad m, Stream s m Char)
       => ParsecT s st m (Maybe String)
optArg = option Nothing $ (liftM Just $ inBrackets)

escaped :: (Monad m, Stream s m Char)
         => String -> ParsecT s st m String
escaped xs = try $ char '\\' >> oneOf xs >>= \x -> return ['\\',x]

inBrackets :: (Monad m, Stream s m Char)
           => ParsecT s st m String
inBrackets = try $ do
  char '['
  pSkipSpaceComments
  res <- manyTill (skipComment >> (escaped "[]" <|> count 1 anyChar))
          (try $ pSkipSpaceComments >> char ']')
  return $ concat res

inbraces :: (Monad m, Stream s m Char)
         => ParsecT s st m String
inbraces = try $ do
  char '{'
  res <- manyTill (skipComment >>
            (inbraces' <|> count 1 anyChar <|> escaped "{}"))
    (try $ skipComment >> char '}')
  return $ concat res

inbraces' :: (Monad m, Stream s m Char)
          => ParsecT s st m String
inbraces' = do
  res <- inbraces
  return $ '{' : (res ++ "}")

