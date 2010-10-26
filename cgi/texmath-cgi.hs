module Main where
import Network.CGI
import Text.XML.Light
import Text.TeXMath
import Text.TeXMath.Macros
import Data.Maybe (fromMaybe)
import Control.Monad
import Text.JSON
import Codec.Binary.UTF8.String (decodeString)
import Text.ParserCombinators.Parsec (Parser, parse, many, try, eof)

pMacros :: Parser [Macro]
pMacros = do
  macros <- many (try $ pSkipSpaceComments >> pMacroDefinition)
  pSkipSpaceComments >> eof
  return macros

cgiMain :: CGI CGIResult
cgiMain = do
  setHeader "Content-type" "text/xhtml; charset=UTF-8"
  latexFormula <- liftM (decodeString . fromMaybe "") $ getInput "latexFormula"
  rawMacros <- liftM (decodeString . fromMaybe "") $ getInput "macros"
  output . encodeStrict $
    case parse pMacros "input" rawMacros of
         Left err  -> toJSObject [("success", JSBool False)
                                 ,("error", JSString $ toJSString (show err))]
         Right ms  -> case texMathToMathML DisplayBlock (applyMacros ms latexFormula) of
                       Left e  -> toJSObject [("success", JSBool False)
                                             , ("error", JSString $ toJSString e)]
                       Right v -> toJSObject [("success", JSBool True)
                                             , ("mathml", JSString $ toJSString $
                                                          ppElement v)]

main :: IO ()
main = runCGI $ handleErrors cgiMain

