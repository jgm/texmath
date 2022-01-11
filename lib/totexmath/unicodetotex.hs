#!/usr/bin/env cabal
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{- cabal:
    build-depends: base, text, xml, texmath, containers, parsec
-}

import Text.Parsec hiding (optional, (<|>))
import Control.Applicative hiding (many)
import Data.List
import Text.TeXMath.Types
import Data.Maybe
import Data.Char
import Debug.Trace
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec String ()


c9001 = Record '\9001' [("base", "\\langle"), ("unicode", "\\langle")] Open "Left angle bracket"
c9002 = Record '\9002' [("base", "\\rangle"), ("unicode", "\\rangle")] Close "Right angle bracket"

c8220 = Record '\8220' [("base", "``")] Pun "Opening curly quote"
c8221 = Record '\8221' [("base", "\"")] Pun "Closing curly quote"

-- Insert updates to mapping here

updates :: [M.Map Char Record -> M.Map Char Record]
updates = 
  [ M.adjust (addCommand ("base", "-")) '-'
  , M.insert '\9001' c9001
  , M.insert '\9002' c9002
  , M.adjust (addCommand ("base", "\\blacksquare")) '\9632'
  , M.adjust (addCommand ("base", "\\square")) '\9633'
  , M.adjust (addCommand ("base", "\\hat{}")) '\710'
  , M.insert '\8220' c8220
  , M.insert '\8221' c8221
  , M.adjust (addCommand ("base", "\\hat{}")) '\94' 
  , M.adjust (addCommand ("base", "\\,")) '\8201'
  , M.adjust (addCommand ("base", "\\:")) '\8287' ]


-- DO NOT ALTER

addCommand :: (Text, Text) -> Record -> Record
addCommand newcmd@(pkg,_) r@(filter ((/= pkg) . fst ) . commands -> cs) =
  r {commands = newcmd : cs}


recordsMap :: [Record] -> M.Map Char Record
recordsMap records = M.fromList (map f records)
  where
        f r = (uchar r, r)

getSymbolType :: String -> TeXSymbolType
getSymbolType s =
  case s of
    "mathpunct"   -> Pun
    "mathord"     -> Ord
    "mathbin"     -> Bin
    "mathopen"    -> Open
    "mathclose"   -> Close
    "mathaccent"  -> Accent
    "mathfence"   -> Fence
    "mathover"    -> TOver
    "mathunder"   -> TUnder
    "mathbotaccent" -> BotAccent
    "mathop"      -> Op
    "mathrel"     -> Rel
    "mathalpha"   -> Alpha
    "mathradical" -> Rad
    _             -> Ord  -- default to Ordinary

main :: IO ()
main = do
  f <- readFile "unimathsymbols.txt"
  let applyUpdates = foldr (.) id updates
  let r = applyUpdates . recordsMap $ (either (error .show) id (parse document "" f))
  let header = "records :: [Record]\nrecords =\n  [ "
  let footer = "]"
  writeFile "UnicodeToLatex.hs" (header ++ concat (intersperse "\n  , " (map show (map snd $ M.toAscList r))) ++ footer)

document :: Parser [Record]
document = do
  skipMany comment
  manyTill row eof

comment :: Parser String
comment = char '#' *> manyTill anyChar (char '\n')

row :: Parser Record
row = do
  hex <- field
  _ <- string "^^" <|> field
  defcmd <- T.pack <$> field
  unicmd <- T.pack <$> field
  _uniclass <- field
  texclass <- field
  reqs <- filter (\z -> head z /= '-') . words <$> field
  let reqs' = if null reqs then ["base"] else map T.pack reqs
  (alts, comment) <- parseComment
  let cmds = filter (\(_,x) -> not (T.null x)) $
               zip reqs' (repeat defcmd) ++ alts ++
               [("unicode-math", unicmd)]
  return (Record (readHex hex) cmds (getSymbolType texclass) comment)

readHex :: String -> Char
readHex = fst . head . readLitChar . ("\\x" ++)

field :: Parser String
field = manyTill anyChar (char '^')

parseComment :: Parsec String () ([(Text, Text)], Text)
parseComment  = (,) <$> (catMaybes <$> sepBy command (char ',')) <* optional (many $ char ' ') <*> (T.pack <$> manyTill anyChar (char '\n'))

command :: Parsec String () (Maybe (Text, Text))
command = do
  first <- lookAhead anyChar
  case first of
    '='-> Just <$> cmd
    '#'-> Just <$> cmd
    'x'-> Nothing <$ skip
    't'-> Nothing <$ skip
    _ -> Nothing <$ return ()

cmd :: Parsec String () (Text, Text)
cmd = do
  anyChar
  optional spaces
  alt <- many1 (noneOf " ,\t")
  optional spaces
  package <- option "" (between (char '(') (char ')')
                (many1 (satisfy (/= ')'))))
  optional (many $ char ' ')
  let package' = if null package then "base" else package
  return (T.pack package', T.pack alt)

skip :: Parsec String () ()
skip = try $ do
  lookAhead (notFollowedBy (many (noneOf ",")) *> newline)
  skipMany (satisfy (/= ','))
