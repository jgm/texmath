module Text.TeXMath.TeX (TeX(..), renderTeX, isControlSeq, escapeLaTeX)
where
import Data.Char (isLetter, isAlphaNum)

-- | An intermediate representation of TeX math, to be used in rendering.
data TeX = ControlSeq String
         | Token Char
         | Literal String
         | Grouped [TeX]
         | Space
         deriving Show

-- | Render a 'TeX' to a string, appending to the front of the given string.
renderTeX :: TeX -> String -> String
renderTeX (Token c) cs     = c:cs
renderTeX (Literal s) cs
  | startsWith isLetter cs = s ++ (' ':cs)
  | otherwise              = s ++ cs
renderTeX (ControlSeq s) cs
  | startsWith isAlphaNum cs = s ++ (' ':cs)
  | otherwise                = s ++ cs
renderTeX (Grouped [Grouped xs]) cs  = renderTeX (Grouped xs) cs
renderTeX (Grouped xs) cs     = '{' : foldr renderTeX "" xs ++ "}" ++ cs
renderTeX Space cs
  | startsWith (==' ') cs     = cs
  | otherwise                 = ' ':cs

startsWith :: (Char -> Bool) -> String -> Bool
startsWith p (c:_) = p c
startsWith _ []    = False

isControlSeq :: String -> Bool
isControlSeq ['\\',c] = c /= ' '
isControlSeq ('\\':xs) = all isLetter xs
isControlSeq _ = False

escapeLaTeX :: Bool -> Char -> TeX
escapeLaTeX mathmode c
  | c `elem` "#$%&_{} " = Literal ("\\" ++ [c])
  | c == '~' = if mathmode
                  then ControlSeq "\\sim"
                  else ControlSeq "\\textasciitilde"
  | c == '^' = if mathmode
                  then Literal "\\char`\\^"
                  else ControlSeq "\\textasciicircum"
  | c == '\\' = if mathmode
                   then ControlSeq "\\setminus"
                   else ControlSeq "\\textbackslash"
  | otherwise = Token c
