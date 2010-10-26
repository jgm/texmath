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

module Text.TeXMath.Macros ( Macro(..)
                           , pMacroDefinition
                           , pSkipSpaceComments
                           , applyMacros
                           )
where

import Data.Char (isDigit)
import Control.Monad
import Text.ParserCombinators.Parsec

newtype Macro = Macro { macroParser :: Parser String }

-- | Parses a @\\newcommand@ or @\\renewcommand@ macro definition and
-- returns a 'Macro'.
pMacroDefinition :: Parser Macro
pMacroDefinition = newcommand

-- | Skip whitespace and comments.
pSkipSpaceComments :: Parser ()
pSkipSpaceComments = spaces >> skipMany (comment >> spaces)

-- | Applies a list of macros to a string recursively until a fixed
-- point is reached.  If there are several macros in the list with the
-- same name, later ones will shadow earlier ones.
applyMacros :: [Macro] -> String -> String
applyMacros [] = id
applyMacros ms = iterateToFixedPoint ((2 * length ms) + 1) $
                  applyMacrosOnce $ reverse ms -- we reverse so that the most recently
                                               -- defined macros will be applied first
                                               -- and shadow others with the same name

------------------------------------------------------------------------------------

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
                  choice [ escaped "\\"
                         , choice (map macroParser ms)
                         , ctrlseq
                         , count 1 anyChar ]
          ctrlseq = do
                    char '\\'
                    res <- many1 letter <|> count 1 anyChar
                    return $ '\\' : res

newcommand :: Parser Macro
newcommand = try $ do
  char '\\'
  optional $ try $ string "re"
  string "newcommand"
  pSkipSpaceComments
  name <- inbraces
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
  body <- inbraces
  return $ Macro $ try $ do
    char '\\'
    string name'
    opt <- case optarg of
                Nothing  -> return Nothing
                Just _   -> liftM (`mplus` optarg) optArg
    args <- count numargs' inbraces
    let args' = case opt of
                     Just x  -> x : args
                     Nothing -> args
    return $ apply args' body

apply :: [String] -> String -> String
apply args ('#':d:xs) | isDigit d =
  let argnum = read [d]
  in  if length args >= argnum
         then args !! (argnum - 1) ++ apply args xs
         else '#' : d : apply args xs
apply args ('\\':'#':xs) = '\\':'#' : apply args xs
apply args (x:xs) = x : apply args xs
apply _ "" = ""

skipComment :: Parser ()
skipComment = skipMany comment

comment :: Parser ()
comment = do
  char '%'
  skipMany (notFollowedBy newline >> anyChar)
  newline
  return ()

numArgs :: Parser Int
numArgs = option 0 $ do
  pSkipSpaceComments
  char '['
  pSkipSpaceComments
  n <- digit
  pSkipSpaceComments
  char ']'
  return $ read [n]

optArg :: Parser (Maybe String)
optArg = option Nothing $ (liftM Just $ inBrackets)

escaped :: String -> Parser String
escaped xs = try $ char '\\' >> oneOf xs >>= \x -> return ['\\',x]

inBrackets :: Parser String
inBrackets = try $ do
  char '['
  pSkipSpaceComments
  res <- manyTill (skipComment >> (escaped "[]" <|> count 1 anyChar))
          (try $ pSkipSpaceComments >> char ']')
  return $ concat res

inbraces :: Parser String
inbraces = try $ do
  char '{'
  res <- manyTill (skipComment >> (inbraces' <|> count 1 anyChar <|> escaped "{}"))
    (try $ skipComment >> char '}')
  return $ concat res

inbraces' :: Parser String
inbraces' = do
  res <- inbraces
  return $ '{' : (res ++ "}")

