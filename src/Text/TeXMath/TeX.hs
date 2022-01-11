{-# LANGUAGE OverloadedStrings #-}

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
renderTeX Space cs
  | cs == ""                   = ""
  | any (`T.isPrefixOf` cs) ps = cs
  | otherwise                  = T.cons ' ' cs
  where
    -- No space before ^, _, or \limits, and no doubled up spaces
    ps = [ "^", "_", " ", "\\limits" ]

trimSpaces :: [TeX] -> [TeX]
trimSpaces = reverse . go . reverse . go
  where go = dropWhile (== Space)

startsWith :: (Char -> Bool) -> T.Text -> Bool
startsWith p t = case T.uncons t of
  Just (c, _) -> p c
  Nothing     -> False

endsWith :: (Char -> Bool) -> T.Text -> Bool
endsWith p t = case T.unsnoc t of
  Just (_, c) -> p c
  Nothing     -> False

isControlSeq :: T.Text -> Bool
isControlSeq t = case T.uncons t of
  Just ('\\', xs) -> T.length xs == 1 && xs /= " "
                     || T.all isLetter xs
  _               -> False

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
