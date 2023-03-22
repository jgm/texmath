{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.TeXMath
import Data.Char (isSpace)
import Text.XML.Light
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.Maybe
import Text.Pandoc.Definition
import Network.URI (unEscapeString)
import Data.Aeson (encode, (.=), object)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Version ( showVersion )
import Text.Show.Pretty (ppShow)
import Paths_texmath (version)

tshow :: Show a => a -> T.Text
tshow = T.pack . ppShow

inHtml :: Element -> Element
inHtml e =
  add_attr (Attr (unqual "xmlns") "http://www.w3.org/1999/xhtml") $
  unode "html"
    [ unode "head" $
        add_attr (Attr (unqual "content") "application/xhtml+xml; charset=UTF-8") $
        add_attr (Attr (unqual "http-equiv") "Content-Type") $
        unode "meta" ()
    , unode "body" e ]

type Reader = T.Text -> Either T.Text [Exp]
data Writer = XMLWriter (DisplayType -> [Exp] -> Element)
            | StringWriter (DisplayType -> [Exp] -> T.Text)
            | PandocWriter (DisplayType -> [Exp] -> Maybe [Inline])

readers :: [(T.Text, Reader)]
readers = [
    ("tex", readTeX)
  , ("mathml", readMathML)
  , ("omml", readOMML)
  , ("native", readNative)]

readNative :: T.Text -> Either T.Text [Exp]
readNative s =
  case reads (T.unpack s) of
       ((exps, ws):_) | all isSpace ws -> Right exps
       ((_, (_:_)):_) -> Left "Could not read whole input as native Exp"
       _ -> Left "Could not read input as native Exp"

writers :: [(T.Text, Writer)]
writers = [
    ("native", StringWriter (\_ es -> tshow es) )
  , ("tex", StringWriter (\_ -> writeTeX))
  , ("eqn", StringWriter writeEqn)
  , ("typst", StringWriter writeTypst)
  , ("omml",  XMLWriter writeOMML)
  , ("xhtml",   XMLWriter (\dt e -> inHtml (writeMathML dt e)))
  , ("mathml",   XMLWriter writeMathML)
  , ("pandoc", PandocWriter writePandoc)]

data Options = Options {
    optDisplay :: DisplayType
  , optIn :: T.Text
  , optOut :: T.Text }

def :: Options
def = Options DisplayBlock "tex" "mathml"

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option [] ["inline"]
      (NoArg (\opts -> return opts {optDisplay = DisplayInline}))
      "Use the inline display style"
  , Option "f" ["from"]
      (ReqArg (\s opts -> return opts {optIn = T.pack s}) "FORMAT")
      ("Input format: " <> T.unpack (T.intercalate ", " (map fst readers)))
  , Option "t" ["to"]
      (ReqArg (\s opts -> return opts {optOut = T.pack s}) "FORMAT")
      ("Output format: " <> T.unpack (T.intercalate ", " (map fst writers)))
   , Option "v" ["version"]
      (NoArg (\_ -> do
                      hPutStrLn stderr $ "Version " <> showVersion version
                      exitWith ExitSuccess))
      "Print version"

    , Option "h" ["help"]
        (NoArg (\_ -> do
                        prg <- getProgName
                        hPutStrLn stderr (usageInfo (prg <> " [OPTIONS] [FILE*]") options)
                        exitWith ExitSuccess))
      "Show help"
  ]

output :: DisplayType -> Writer -> [Exp] -> T.Text
output dt (XMLWriter w) es    = output dt (StringWriter (\dt' -> T.pack . ppElement . w dt' )) es
output dt (StringWriter w) es = w dt es
output dt (PandocWriter w) es = tshow (fromMaybe fallback (w dt es))
  where fallback = [Math mt $ writeTeX es]
        mt = case dt of
                  DisplayBlock  -> DisplayMath
                  DisplayInline -> InlineMath

err :: Bool -> Int -> T.Text -> IO a
err cgi code msg = do
  if cgi
     then B.putStr $ encode $ object ["error" .= msg]
     else T.hPutStrLn stderr msg
  exitWith $ ExitFailure code

ensureFinalNewline :: T.Text -> T.Text
ensureFinalNewline xs = case T.unsnoc xs of
  Nothing        -> xs
  Just (_, '\n') -> xs
  _              -> xs <> "\n"

urlUnencode :: T.Text -> T.Text
urlUnencode = T.pack . unEscapeString . plusToSpace . T.unpack
  where plusToSpace ('+':xs) = "%20" <> plusToSpace xs
        plusToSpace (x:xs)   = x : plusToSpace xs
        plusToSpace []       = []

main :: IO ()
main = do
  progname <- getProgName
  let cgi = progname == "texmath-cgi"
  if cgi
     then runCGI
     else runCommandLine

runCommandLine :: IO ()
runCommandLine = do
  args <- getArgs
  let (actions, files, _) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return def) actions
  inp <- case files of
              [] -> T.getContents
              _  -> T.concat <$> mapM T.readFile files
  reader <- case lookup (optIn opts) readers of
                  Just r -> return r
                  Nothing -> err False 3 "Unrecognised reader"
  writer <- case lookup (optOut opts) writers of
                  Just w -> return w
                  Nothing -> err False 5 "Unrecognised writer"
  case reader inp of
        Left msg -> err False 1 msg
        Right v  -> T.putStr $ ensureFinalNewline
                             $ output (optDisplay opts) writer v
  exitWith ExitSuccess

runCGI :: IO ()
runCGI = do
  query <- T.getContents
  let topairs xs = case T.break (=='=') xs of
                     (ys, zs) -> case T.uncons zs of
                       Just ('=', zs') -> (urlUnencode ys, urlUnencode zs')
                       _               -> (urlUnencode ys, "")
  let pairs = map topairs $ T.split (== '&') query
  inp <- case lookup "input" pairs of
                 Just x  -> return x
                 Nothing -> err True 3 "Query missing 'input'"
  reader <- case lookup "from" pairs of
                 Just x  -> case lookup x readers of
                                 Just y  -> return y
                                 Nothing -> err True 5 "Unsupported value of 'from'"
                 Nothing -> err True 3 "Query missing 'from'"
  writer <- case lookup "to" pairs of
                 Just x  -> case lookup x writers of
                                 Just y  -> return y
                                 Nothing -> err True 7 "Unsupported value of 'to'"
                 Nothing -> err True 3 "Query missing 'to'"
  let inline = isJust $ lookup "inline" pairs
  putStr "Content-type: text/json; charset=UTF-8\n\n"
  case reader inp of
        Left msg -> err True 1 msg
        Right v  -> B.putStr $ encode $ object
                       [ "success" .=
                       ensureFinalNewline (output
                        (if inline
                            then DisplayInline
                            else DisplayBlock) writer v) ]
  exitWith ExitSuccess
