module Text.TeXMath ( texMathToMathML, DisplayType(..) )
where
import Text.TeXMath.Parser
import Text.TeXMath.MathMLWriter
import Text.XML.Light
import Text.ParserCombinators.Parsec (parse)

texMathToMathML :: DisplayType -> String -> Either String Element
texMathToMathML dt inp = inp `seq`
  case parse formula "TeX math input" inp of
       Left err        -> Left (show err)
       Right v         -> Right $ toMathML dt v
