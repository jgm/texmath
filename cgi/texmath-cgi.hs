module Main where
import Network.CGI
import Text.XML.Light
import Text.TeXMath
import Data.Maybe (fromMaybe)
import Control.Monad
import Text.JSON
import Codec.Binary.UTF8.String (decodeString)

cgiMain :: CGI CGIResult
cgiMain = do
  latexFormula <- liftM (decodeString . fromMaybe "") $ getInput "latexFormula"
  setHeader "Content-type" "text/xhtml; charset=UTF-8"
  output . encodeStrict $
    case texMathToMathML DisplayBlock latexFormula of
       Left e     -> toJSObject [("success", JSBool False), ("error", JSString $ toJSString e)]
       Right v    -> toJSObject [("success", JSBool True), ("mathml", JSString $ toJSString $ ppElement v)]

main :: IO ()
main = runCGI $ handleErrors cgiMain

