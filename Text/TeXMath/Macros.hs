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
 - to LateX expressions.
-}

module Text.TeXMath.Macros (Macro(..), pMacroDefinition, applyMacros)
where

import Data.Char (isDigit)
import Control.Monad
import Text.ParserCombinators.Parsec

-- API here:

newtype Macro = Macro { macroParser :: Parser String }

pMacroDefinition :: Parser Macro
pMacroDefinition = newcommand {- <|> newenvironment -}

applyMacros :: [Macro] -> String -> String
applyMacros [] = id
applyMacros ms = iterateToFixedPoint ((2 * length ms) + 1) (applyMacrosOnce ms)

-- end of API

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
  skipCS
  name <- inbraces
  guard (take 1 name == "\\")
  let name' = drop 1 name
  numargs <- numArgs
  optarg <- if numargs > 0
               then optArg
               else return Nothing
  let numargs' = case optarg of
                   Just _  -> numargs - 1
                   Nothing -> numargs
  skipCS
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
apply args ('#':d:xs) | isDigit d = args !! (read [d] - 1) ++
  apply args xs
apply args ('\\':'#':xs) = '\\':'#' : apply args xs
apply args (x:xs) = x : apply args xs
apply _ "" = ""

endofline :: Parser ()
endofline = (newline >> return ()) <|> eof

skipCS :: Parser ()
skipCS = skipMany (spaces >> comment) >> spaces 

skipComment :: Parser ()
skipComment = skipMany comment

comment :: Parser ()
comment = char '%' >> manyTill anyChar endofline >> return ()

numArgs :: Parser Int
numArgs = option 0 $ do
  skipCS
  char '['
  n <- digit
  char ']'
  return $ read [n]

optArg :: Parser (Maybe String)
optArg = option Nothing $ (liftM Just $ skipCS >> inBrackets)

escaped :: String -> Parser String
escaped xs = try $ char '\\' >> oneOf xs >>= \x -> return ['\\',x]

inBrackets :: Parser String
inBrackets = try $ do
  char '['
  res <- manyTill (skipComment >> (escaped "[]" <|> count 1 anyChar))
          (try $ skipComment >> char ']')
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

