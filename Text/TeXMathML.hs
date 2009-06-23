module Text.TeXMathML ( texMathToMathML, DisplayType(..) )
where
import Text.TeXMathML.Parser
import Text.TeXMathML.Writer
import Text.XML.Light
import Text.ParserCombinators.Parsec (parse)

texMathToMathML :: DisplayType -> String -> Element
texMathToMathML dt inp = inp `seq`
  case parse formula "TeX math input" inp of
       Left err        -> error (show err)
       Right v         -> toMathML dt v 
