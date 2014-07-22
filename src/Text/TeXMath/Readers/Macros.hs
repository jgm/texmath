{-# LANGUAGE RankNTypes #-}
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

module Text.TeXMath.Readers.Macros
                           ( Macro
                           , parseMacroDefinitions
                           , applyMacros
                           )
where

import Data.Char (isDigit, isLetter)
import Control.Monad
import Text.ParserCombinators.Parsec

data Macro = Macro { macroDefinition :: String
                   , macroParser     :: forall st . GenParser Char st String }

instance Show Macro where
  show m = "Macro " ++ show (macroDefinition m)

-- | Parses a string for a list of macro definitions, optionally
-- separated and ended by spaces and TeX comments.  Returns
-- the list of macros (which may be empty) and the unparsed
-- portion of the input string.
parseMacroDefinitions :: String -> ([Macro], String)
parseMacroDefinitions s =
  case parse pMacroDefinitions "input" s of
       Left _       -> ([], s)
       Right res    -> res

-- | Parses one or more macro definitions separated by comments & space.
-- Return list of macros parsed + remainder of string.
pMacroDefinitions :: GenParser Char st ([Macro], String)
pMacroDefinitions = do
  defs <- sepEndBy pMacroDefinition pSkipSpaceComments
  rest <- getInput
  return (reverse defs, rest)  -- reversed so later macros shadow earlier

-- | Parses a @\\newcommand@ or @\\renewcommand@ macro definition and
-- returns a 'Macro'.
pMacroDefinition :: GenParser Char st Macro
pMacroDefinition = newcommand <|> declareMathOperator

-- | Skip whitespace and comments.
pSkipSpaceComments :: GenParser Char st ()
pSkipSpaceComments = spaces >> skipMany (comment >> spaces)

-- | Applies a list of macros to a string recursively until a fixed
-- point is reached.  If there are several macros in the list with the
-- same name, earlier ones will shadow later ones.
applyMacros :: [Macro] -> String -> String
applyMacros [] = id
applyMacros ms = iterateToFixedPoint ((2 * length ms) + 1) $ applyMacrosOnce ms

------------------------------------------------------------------------------

iterateToFixedPoint :: Eq a => Int -> (a -> a) -> a -> a
iterateToFixedPoint 0     _ _ = error $
  "Macro application did not terminate in a reasonable time.\n" ++
  "Check your macros for loops."
iterateToFixedPoint limit f x =
  if x' == x
     then x'
     else iterateToFixedPoint (limit - 1) f x'
    where x' = f x

applyMacrosOnce :: [Macro] -> String -> String
applyMacrosOnce ms s =
  case parse (many tok) "input" s of
       Right r -> concat r
       Left _  -> s  -- just return original on error
    where tok = try $ do
                  skipComment
                  choice [ choice (map macroParser ms)
                         , ctrlseq
                         , count 1 anyChar ]

ctrlseq :: GenParser Char st String
ctrlseq = do
          char '\\'
          res <- many1 letter <|> count 1 anyChar
          return $ '\\' : res

newcommand :: GenParser Char st Macro
newcommand = try $ do
  char '\\'
  -- we ignore differences between these so far:
  try (string "newcommand")
    <|> try (string "renewcommand")
    <|> string "providecommand"
  pSkipSpaceComments
  name <- inbraces <|> ctrlseq
  guard (take 1 name == "\\")
  let name' = drop 1 name
  pSkipSpaceComments
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
  return $ Macro defn $ try $ do
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

-- | Parser for \DeclareMathOperator(*) command.
declareMathOperator :: GenParser Char st Macro
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
  return $ Macro defn $ try $ do
    char '\\'
    string name'
    when (all isLetter name') $
      notFollowedBy letter
    pSkipSpaceComments
    return $ "\\operatorname" ++ star ++ "{" ++ body ++ "}"


apply :: [String] -> String -> String
apply args ('#':d:xs) | isDigit d =
  let argnum = read [d]
  in  if length args >= argnum
         then args !! (argnum - 1) ++ apply args xs
         else '#' : d : apply args xs
apply args ('\\':'#':xs) = '\\':'#' : apply args xs
apply args (x:xs) = x : apply args xs
apply _ "" = ""

skipComment :: GenParser Char st ()
skipComment = skipMany comment

comment :: GenParser Char st ()
comment = do
  char '%'
  skipMany (notFollowedBy newline >> anyChar)
  newline
  return ()

numArgs :: GenParser Char st Int
numArgs = option 0 $ do
  pSkipSpaceComments
  char '['
  pSkipSpaceComments
  n <- digit
  pSkipSpaceComments
  char ']'
  return $ read [n]

optArg :: GenParser Char st (Maybe String)
optArg = option Nothing $ (liftM Just $ inBrackets)

escaped :: String -> GenParser Char st String
escaped xs = try $ char '\\' >> oneOf xs >>= \x -> return ['\\',x]

inBrackets :: GenParser Char st String
inBrackets = try $ do
  char '['
  pSkipSpaceComments
  res <- manyTill (skipComment >> (escaped "[]" <|> count 1 anyChar))
          (try $ pSkipSpaceComments >> char ']')
  return $ concat res

inbraces :: GenParser Char st String
inbraces = try $ do
  char '{'
  res <- manyTill (skipComment >> (inbraces' <|> count 1 anyChar <|> escaped "{}"))
    (try $ skipComment >> char '}')
  return $ concat res

inbraces' :: GenParser Char st String
inbraces' = do
  res <- inbraces
  return $ '{' : (res ++ "}")

