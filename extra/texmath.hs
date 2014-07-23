{-# LANGUAGE CPP #-}
module Main where

import Text.TeXMath
import Control.Monad (when)
import Text.XML.Light
import System.IO
import System.Environment
import Control.Applicative
import System.Console.GetOpt
import Data.List (intersperse)
import System.Exit
import Data.Maybe
import Text.Pandoc.Definition
import Network.URI (unEscapeString)
import Data.List.Split (splitOn)
import Data.Aeson (encode, (.=), object)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

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
            | PandocWriter (DisplayType -> [Exp] -> Maybe [Inline])

readers :: [(String, Reader)]
readers = [
    ("tex", readTeXMath)
  , ("mathml", readMathML)]

writers :: [(String, Writer)]
writers = [
    ("native", StringWriter (\_ es -> show es) )
  , ("tex", StringWriter writeTeXMathIn)
  , ("omml",  XMLWriter writeOMML)
  , ("xhtml",   XMLWriter (\dt e -> inHtml (writeMathML dt e)))
  , ("mathml",   XMLWriter writeMathML)
  , ("pandoc", PandocWriter writePandoc)]

data Options = Options {
    optDisplay :: DisplayType
  , optIn :: String
  , optOut :: String }

def :: Options
def = Options DisplayBlock "tex" "mathml"

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option [] ["inline"]
      (NoArg (\opts -> return opts {optDisplay = DisplayInline}))
      "Use the inline display style"
  , Option "f" ["from"]
      (ReqArg (\s opts -> return opts {optIn = s}) "FORMAT")
      ("Input format: " ++ (concat $ intersperse ", " (map fst readers)))
  , Option "t" ["to"]
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
output dt (XMLWriter w) es    = output dt (StringWriter (\dt' -> ppElement . w dt' )) es
output dt (StringWriter w) es = w dt es
output dt (PandocWriter w) es = show (fromMaybe fallback (w dt es))
  where fallback = [Math mt (writeTeXMath es)]
        mt = case dt of
                  DisplayBlock  -> DisplayMath
                  DisplayInline -> InlineMath

err :: Bool -> Int -> String -> IO a
err cgi code msg = do
  if cgi
     then B.putStr $ encode $ object [T.pack "error" .= msg]
     else hPutStrLn stderr msg
  exitWith $ ExitFailure code

ensureFinalNewline :: String -> String
ensureFinalNewline "" = ""
ensureFinalNewline xs = if last xs == '\n' then xs else xs ++ "\n"

urlUnencode :: String -> String
urlUnencode = unEscapeString . plusToSpace
  where plusToSpace ('+':xs) = "%20" ++ plusToSpace xs
        plusToSpace (x:xs)   = x : plusToSpace xs
        plusToSpace []       = []

main :: IO ()
main = do
  args' <- getArgs
  mbquery <- lookup "QUERY_STRING" <$> getEnvironment
  -- if QUERY_STRING is set and we have no arguments, treat as CGI
  -- and get arguments from QUERY_STRING
  (cgi, inp, args) <-
      case (args', mbquery) of
           ([], Just q)  -> do
             let topairs xs = case break (=='=') xs of
                                   (ys,('=':zs)) -> (urlUnencode ys, urlUnencode zs)
                                   (ys,_)        -> (urlUnencode ys,"")
             let pairs = map topairs $ splitOn "&" q
             let inp'  = fromMaybe "" $ lookup "input" pairs
             let args'' = maybe [] (\x -> ["--from", x]) (lookup "from" pairs) ++
                          maybe [] (\x -> ["--to", x]) (lookup "to" pairs) ++
                          ["--inline" | lookup "inline" pairs /= Nothing]
             return $ (True, inp', args'')
           _             -> do
             inp' <- getUTF8Contents
             return (False, inp', args')
  when cgi $ putStr "Content-type: text/json; charset=UTF-8\n\n"
  let (actions, _, _) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return def) actions
  reader <- case lookup (optIn opts) readers of
                  Just r -> return r
                  Nothing -> err cgi 3 "Unrecognised reader"
  writer <- case lookup (optOut opts) writers of
                  Just w -> return w
                  Nothing -> err cgi 5 "Unrecognised writer"
  case reader inp of
        Left msg -> err cgi 1 msg
        Right v
          | cgi  -> B.putStr $ encode $ object
                       [ T.pack "success" .=
                         ensureFinalNewline (output (optDisplay opts) writer v) ]
          | otherwise -> putStr $ ensureFinalNewline
                                $ output (optDisplay opts) writer v
  exitWith ExitSuccess
