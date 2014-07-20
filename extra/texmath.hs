{-# LANGUAGE CPP #-}
module Main where

import Text.TeXMath
import Text.XML.Light
import Text.TeXMath.Types
import System.IO
import System.Environment
import Control.Applicative
import System.Console.GetOpt

inHtml :: Element -> Element
inHtml e =
  add_attr (Attr (unqual "xmlns") "http://www.w3.org/1999/xhtml") $
  unode "html"
    [ unode "head" $
        add_attr (Attr (unqual "content") "application/xhtml+xml; charset=UTF-8") $
        add_attr (Attr (unqual "http-equiv") "Content-Type") $
        unode "meta" ()
    , unode "body" e ]

getUTF8Contents :: IO String
getUTF8Contents =
       hGetContents stdin

type Reader = String -> Either String [Exp]
data Writer = XMLWriter (DisplayType -> [Exp] -> Element)
            | StringWriter (DisplayType -> [Exp] -> String)

readers :: [(String, Reader)]
readers = [
    ("tex", readLaTeX)
  , ("mml", readMathML)]

writers :: [(String, Writer)]
writers = [
    ("native", StringWriter (\_ es -> show es) )
  , ("tex", StringWriter toLaTeX)
  , ("omml",  XMLWriter toOMML)
  , ("xhtml",   XMLWriter (\dt e -> inHtml (toMathML dt e)))
  , ("mml",   XMLWriter toMathML)]

data Options = Options {
    optDisplay :: DisplayType 
  , optIn :: String 
  , optOut :: String }

def :: Options
def = Options DisplayBlock "latex" "mml"

options :: [OptDescr (Options -> Options)]
options = 
  [ Option [] ["inline"] 
      (NoArg (\opts -> opts {optDisplay = DisplayInline}))
      "Choose display type"
  , Option "f" [] 
      (ReqArg (\s opts -> opts {optIn = s}) "FORMAT")
      "Input format"
  , Option "t" [] 
      (ReqArg (\s opts -> opts {optOut = s}) "FORMAT")
      "Output format"
  ]

output :: DisplayType -> Writer -> [Exp] -> String
output dt (XMLWriter w) es = output dt (StringWriter (\dt' -> ppTopElement . w dt' )) es
output dt (StringWriter w) es = w dt es

main :: IO ()
main = do
  (actions, _, _) <- getOpt RequireOrder options <$> getArgs
  let opts = (foldl (.) id actions) def
  let reader = case lookup (optIn opts) readers of
                  Just r -> r
                  Nothing -> error "Unrecognised reader"
  let writer = case lookup (optOut opts) writers of
                  Just w -> w
                  Nothing -> error "Unrecognised writer"
  inp <- getUTF8Contents
  let formula = reader inp
  case (formula) of
        Left err         -> hPutStrLn stderr err
        Right v          -> putStr (output (optDisplay opts) writer v)
