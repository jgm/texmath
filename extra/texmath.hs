{-# LANGUAGE CPP #-}
module Main where

import Text.TeXMath
import Text.XML.Light
import System.IO
import System.Environment
import Control.Applicative
import System.Console.GetOpt
import Text.Pandoc.Definition
import Data.List (intersperse)
import System.Exit
import Data.Maybe

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
            | PandocWriter (DisplayType -> [Exp] -> [Inline])

readers :: [(String, Reader)]
readers = [
    ("tex", readTeXMath)
  , ("mml", readMathML)]

writers :: [(String, Writer)]
writers = [
    ("native", StringWriter (\_ es -> show es) )
  , ("tex", StringWriter writeTeXMathIn)
  , ("omml",  XMLWriter writeOMML)
  , ("xhtml",   XMLWriter (\dt e -> inHtml (writeMathML dt e)))
  , ("mml",   XMLWriter writeMathML)
  , ("pandoc", PandocWriter writePandoc')] 

writePandoc' :: (DisplayType -> [Exp] -> [Inline])
writePandoc' dt e = 
  fromMaybe (error "Unable to convert to inlines") (writePandoc dt e)

data Options = Options {
    optDisplay :: DisplayType 
  , optIn :: String 
  , optOut :: String }

def :: Options
def = Options DisplayBlock "latex" "mml"

options :: [OptDescr (Options -> IO Options)]
options = 
  [ Option [] ["inline"] 
      (NoArg (\opts -> return opts {optDisplay = DisplayInline}))
      "Use the inline display style"
  , Option "f" [] 
      (ReqArg (\s opts -> return opts {optIn = s}) "FORMAT")
      ("Input format: " ++ (concat $ intersperse ", " (map fst readers)))
  , Option "t" [] 
      (ReqArg (\s opts -> return opts {optOut = s}) "FORMAT")
      ("Output format: " ++ (concat $ intersperse ", " (map fst writers)))
   , Option "v" ["version"]
      (NoArg (\_ -> do
                      hPutStrLn stderr "Version 0.7"
                      exitWith ExitSuccess))
      "Print version"
                                                                
    , Option "h" ["help"]
        (NoArg (\_ -> do
                        prg <- getProgName
                        hPutStrLn stderr (usageInfo prg options)
                        exitWith ExitSuccess))
      "Show help"
  ]

output :: DisplayType -> Writer -> [Exp] -> String
output dt (XMLWriter w) es = output dt (StringWriter (\dt' -> ppTopElement . w dt' )) es
output dt (StringWriter w) es = w dt es
output dt (PandocWriter w) es = show (w dt es)

main :: IO ()
main = do
  (actions, _, _) <- getOpt RequireOrder options <$> getArgs
  opts <- foldl (>>=) (return def) actions
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
