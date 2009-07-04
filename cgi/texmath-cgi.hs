module Main where
import Network.CGI
import Text.XML.Light
import Text.TeXMath
import Data.Maybe (fromJust)
import Control.Monad

cgiMain :: CGI CGIResult
cgiMain = do
  latexFormula <- liftM fromJust $ getInput "latexFormula"
  setHeader "Content-type" "text/xhtml; charset=UTF-8"
  output . ppElement $
    case texMathToMathML DisplayBlock latexFormula of
       Left e     -> unode "p" e
       Right v    -> v 

main :: IO ()
main = runCGI $ handleErrors cgiMain

