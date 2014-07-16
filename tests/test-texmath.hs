import System.Directory
import System.FilePath
import Text.XML.Light
import System.IO
import Text.TeXMath
import Text.TeXMath.Macros
import System.Exit
import Control.Applicative
import GHC.IO.Encoding (setLocaleEncoding)

data Status = Pass
            | Fail FilePath FilePath String
            | Error FilePath FilePath String
            deriving (Eq, Show)

main :: IO ()
main = do
  setLocaleEncoding utf8
  setCurrentDirectory "tests"
  texs <- filter (\x -> takeExtension x == ".tex") <$> getDirectoryContents "."
  statuses <- concat <$> mapM runTeXTests texs
  let passes = length $ filter (== Pass) statuses
  let failures = length statuses - passes
  putStrLn $ show passes ++ " tests passed, " ++ show failures ++ " failed."
  if all (== Pass) statuses
     then do
       exitWith ExitSuccess
     else do
       mapM_ printStatus statuses
       exitWith $ ExitFailure failures

printStatus :: Status -> IO ()
printStatus Pass = return ()
printStatus (Fail inp out actual) = do
  putStrLn $ "FAILED:  " ++ inp ++ " ==> " ++ out
  putStrLn "------ input ---------"
  readFile inp >>= putStr
  putStrLn "------ expected ------"
  readFile out >>= putStr
  putStrLn "------ actual --------"
  putStr actual
  putStrLn "----------------------"
printStatus (Error inp out msg) =
  putStrLn $ "ERROR:  " ++ inp ++ " ==> " ++ out ++ "\n" ++ msg

runTeXTests :: FilePath -> IO [Status]
runTeXTests path = do
  xhtmlStatus <- runTest
    (fmap (ppTopElement . inHtml) . texMathToMathML DisplayBlock . parseMacros)
    path (replaceExtension path "xhtml")
  ommlStatus <- runTest
    (fmap ppTopElement . texMathToOMML DisplayBlock . parseMacros)
    path (replaceExtension path "omml")
  return [xhtmlStatus, ommlStatus]

parseMacros :: String -> String
parseMacros inp =
  let (ms, rest) = parseMacroDefinitions inp
  in  applyMacros ms rest

inHtml :: Element -> Element
inHtml x =
  add_attr (Attr (unqual "xmlns") "http://www.w3.org/1999/xhtml") $
  unode "html"
    [ unode "head" $
        add_attr (Attr (unqual "content") "application/xhtml+xml; charset=UTF-8")
        $ add_attr (Attr (unqual "http-equiv") "Content-Type")
        $ unode "meta" ()
    , unode "body" x ]

runTest :: (String -> Either String String)
        -> FilePath
        -> FilePath
        -> IO Status
runTest fn input output = do
  inp_t <- readFile input
  out_t <- readFile output
  case fn inp_t of
       Left msg    -> return $ Error input output msg
       Right r     ->
         if r == out_t
            then return Pass
            else return $ Fail input output r
