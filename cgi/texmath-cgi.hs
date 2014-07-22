module Main where
import Network.CGI
import Text.XML.Light
import Text.TeXMath
import Text.TeXMath.Readers.TeXMath.Macros
import Data.Maybe (fromMaybe)
import Control.Monad
import Text.JSON
import Codec.Binary.UTF8.String (decodeString, encodeString)
import Control.Applicative

texMathToMathML :: DisplayType -> String -> Either String Element
texMathToMathML dt s = writeMathML dt <$> readTeXMath s

cgiMain :: CGI CGIResult
cgiMain = do
  setHeader "Content-type" "text/xhtml; charset=UTF-8"
  latexFormula <- liftM (decodeString . fromMaybe "") $ getInput "latexFormula"
  rawMacros <- liftM (decodeString . fromMaybe "") $ getInput "macros"
  let (ms, _) = parseMacroDefinitions rawMacros
  output . encodeStrict $
    case texMathToMathML DisplayBlock (applyMacros (reverse ms) latexFormula) of
         Left e  -> toJSObject [("success", JSBool False)
                               , ("error", JSString $ toJSString e)]
         Right v -> toJSObject [("success", JSBool True)
                               , ("mathml", JSString $ toJSString $ encodeString $
                                     ppElement v)]

main :: IO ()
main = runCGI $ handleErrors cgiMain

