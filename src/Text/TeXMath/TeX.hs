{-# LANGUAGE OverloadedStrings #-}
module Text.TeXMath.TeX (TeX(..),
                         renderTeX,
                         isControlSeq,
                         escapeLaTeX)
where
import Data.Char (isLetter, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid

-- | An intermediate representation of TeX math, to be used in rendering.
data TeX = ControlSeq Text
         | Token Char
         | Literal Text
         | Grouped [TeX]
         | Space
         deriving (Show, Eq)

-- | Render a 'TeX' to a string, appending to the front of the given string.
renderTeX :: TeX -> Text -> Text
renderTeX (Token c) cs     = Text.cons c cs
renderTeX (Literal s) cs
  | startsWith (not . isLetter)
               (Text.reverse s) = s <> cs
  | startsWith isLetter cs = s <> " " <> cs
  | otherwise              = s <> cs
renderTeX (ControlSeq s) cs
  | s == "\\ "               = s <> cs
  | startsWith isAlphaNum cs = s <> " " <> cs
  | otherwise                = s <> cs
renderTeX (Grouped [Grouped xs]) cs  = renderTeX (Grouped xs) cs
renderTeX (Grouped xs) cs     =
  "{" <> foldr renderTeX "" (trimSpaces xs) <> "}" <> cs
renderTeX Space t
  = case Text.uncons t of
         Nothing       -> ""        -- no need to end with space
         Just ('^',cs) -> "^" <> cs -- no space before ^
         Just ('_',cs) -> "_" <> cs -- no space before _
         Just (' ',cs) -> " " <> cs -- no doubled up spaces
         _ | "\\limits" `Text.isPrefixOf` t -> t -- no space before \limits
           | otherwise -> " " <> t

trimSpaces :: [TeX] -> [TeX]
trimSpaces = reverse . go . reverse . go
  where go = dropWhile (== Space)

startsWith :: (Char -> Bool) -> Text -> Bool
startsWith p t = case Text.uncons t of
                        Just (c, _) -> p c
                        Nothing     -> False

isControlSeq :: Text -> Bool
isControlSeq t =
  case Text.uncons t of
       Just ('\\', cs) -> (Text.length cs == 1 && cs /= " " && cs /= "\n")
                          || (Text.length cs >= 1 && Text.all isLetter cs)
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
       _ | c `elem` ("#$%&_{} " :: [Char])
                -> Literal ("\\" <> Text.singleton c)
         | otherwise -> Token c
