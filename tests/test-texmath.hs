import System.Directory
import System.FilePath
import Text.XML.Light
import System.IO
import Text.TeXMath
import Text.TeXMath.Types
import System.Exit
import Control.Applicative
import GHC.IO.Encoding (setLocaleEncoding)
import Data.Maybe

data Status = Pass
            | Fail FilePath FilePath String
            | Error FilePath FilePath String
            deriving (Eq, Show)

type Ext = String

readers :: [(Ext, String -> Either String [Exp])]
readers = [(".tex", readTeXMath), (".mml", readMathML)]

writers :: [(Ext, [Exp] -> String)]
writers = [(".mml", ppTopElement . writeMathML DisplayBlock), (".tex", writeTeXMath DisplayBlock), (".omml", ppTopElement . writeOMML DisplayBlock)]

main :: IO ()
main = do
  setLocaleEncoding utf8
  setCurrentDirectory "tests"
  readerTests <- concat <$> mapM (uncurry runReader) readers
  writerTests <- concat <$> mapM (uncurry runWriter) writers
  let statuses = readerTests ++ writerTests
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

runReader :: String -> (String -> Either String [Exp]) -> IO [Status]
runReader ext f = do
  input <- filter (\x -> takeExtension x == ext) <$> getDirectoryContents "./src"
  let input' = map ("src" </>) input
  mapM (\inp -> runReaderTest (\x -> show <$> f x) inp (rename inp)) input'
  where
    rename x = replaceExtension (replaceDirectory x ("./readers" </> (tail ext))) "native"

runWriter ::  String -> ([Exp] -> String) -> IO [Status]
runWriter ext f = do
  let exts = map fst readers
  input <- filter (\x -> takeExtension x `elem` exts) <$> getDirectoryContents "./src"
  let input' = map ("src" </>) input
  mapM (\inp -> runWriterTest f inp (rename inp)) input'
  where
    rename x = replaceExtension (replaceDirectory x "./writers") (tail ext)

runWriterTest :: ([Exp] -> String) -> FilePath -> FilePath -> IO Status
runWriterTest f input output = do
  expr <- (\fn -> (fromJust (lookup (takeExtension input) readers)) fn)
              <$> readFile input
  let r = f (either (const []) id expr)
  out_t <- readFile output
  if (r == out_t)
    then return $ Pass
    else return $ Fail input output r

runReaderTest :: (String -> Either String String)
        -> FilePath
        -> FilePath
        -> IO Status
runReaderTest fn input output = do
  inp_t <- readFile input
  out_t <- readFile output
  case fn inp_t of
       Left msg    -> return $ Error input output msg
       Right r     ->
         if r == out_t
            then return Pass
            else return $ Fail input output r
