{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-} -- TODO text: remove
module Text.TeXMath.TeX (TeX(..),
                         renderTeX,
                         isControlSeq,
                         escapeLaTeX)
where
import Data.Char (isLetter, isAlphaNum, isAscii)
import qualified Data.Text as T

-- | An intermediate representation of TeX math, to be used in rendering.
data TeX = ControlSeq T.Text
         | Token Char
         | Literal T.Text
         | Grouped [TeX]
         | Space
         deriving (Show, Eq)

-- | Render a 'TeX' to a string, appending to the front of the given string.
renderTeX :: TeX -> T.Text -> T.Text
renderTeX (Token c) cs     = T.cons c cs
renderTeX (Literal s) cs
  | endsWith (not . isLetter) s = s <> cs
  | startsWith isLetter cs      = s <> T.cons ' ' cs
  | otherwise                   = s <> cs
renderTeX (ControlSeq s) cs
  | s == "\\ "               = s <> cs
  | startsWith (\c -> isAlphaNum c || not (isAscii c)) cs
                             = s <> T.cons ' ' cs
  | otherwise                = s <> cs
renderTeX (Grouped [Grouped xs]) cs  = renderTeX (Grouped xs) cs
renderTeX (Grouped xs) cs     =
  "{" <> foldr renderTeX "" (trimSpaces xs) <> "}" <> cs
renderTeX Space ""             = "" -- no need to end with space
-- TODO text: refactor
renderTeX Space (T.unpack -> ('^':cs)) = T.pack $ '^' : cs  -- no space before ^
renderTeX Space (T.unpack -> ('_':cs)) = T.pack $ '_' : cs  -- no space before _
renderTeX Space (T.unpack -> (' ':cs)) = T.pack $ ' ' : cs  -- no doubled up spaces
renderTeX Space cs
  | "\\limits" `T.isPrefixOf` cs = cs      -- no space before \limits
  | otherwise                  = T.cons ' ' cs

trimSpaces :: [TeX] -> [TeX]
trimSpaces = reverse . go . reverse . go
  where go = dropWhile (== Space)

-- TODO text: refactor
startsWith :: (Char -> Bool) -> T.Text -> Bool
startsWith p (T.unpack -> (c:_)) = p c
startsWith _ _    = False

endsWith :: (Char -> Bool) -> T.Text -> Bool
endsWith p = startsWith p . T.reverse -- TODO text: refactor

-- TODO text: refactor
isControlSeq :: T.Text -> Bool
isControlSeq (T.unpack -> ['\\',c]) = c /= ' '
isControlSeq (T.unpack -> ('\\':xs)) = all isLetter xs
isControlSeq _ = False

escapeLaTeX :: Char -> TeX
escapeLaTeX c =
  case c of
       '~'   -> ControlSeq "\\textasciitilde"
       '^'   -> Literal "\\textasciicircum"
       '\\'  -> ControlSeq "\\textbackslash"
       '\x200B' -> Literal "\\!"
       '\x200A' -> Literal "\\,"
       '\x2006' -> Literal "\\,"
       '\xA0'   -> Literal "~"
       '\x2005' -> Literal "\\:"
       '\x2004' -> Literal "\\;"
       '\x2001' -> ControlSeq "\\quad"
       '\x2003' -> ControlSeq "\\quad"
       '\x2032' -> Literal "'"
       '\x2033' -> Literal "''"
       '\x2034' -> Literal "'''"
       _ | T.any (== c) "#$%&_{} " -> Literal ("\\" <> T.singleton c)
         | otherwise -> Token c
