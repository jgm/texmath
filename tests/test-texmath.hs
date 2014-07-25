import System.Directory
import System.FilePath
import Text.XML.Light
import System.IO
import System.IO.Temp (withTempDirectory)
import System.Process
import Text.TeXMath
import System.Exit
import Control.Applicative
import GHC.IO.Encoding (setLocaleEncoding)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (intercalate)
import Data.List.Split (splitWhen)

-- strict version of readFile
readFile' :: FilePath -> IO String
readFile' f = T.unpack <$> T.readFile f

data Status = Pass FilePath FilePath
            | Fail FilePath FilePath
            | Error FilePath FilePath
            deriving (Eq, Show)

type Ext = String

readers :: [(Ext, String -> Either String [Exp])]
readers = [ (".tex", readTeXMath)
          , (".mml", readMathML)
          ]

writers :: [(Ext, [Exp] -> String)]
writers = [ (".mml", ppTopElement . writeMathML DisplayBlock)
          , (".tex", writeTeXMath)
          , (".omml", ppTopElement . writeOMML DisplayBlock)
          ]

main :: IO ()
main = do
  setLocaleEncoding utf8
  setCurrentDirectory "tests"
  readerTests <- concat <$> mapM (uncurry runReader) readers
  writerTests <- concat <$> mapM (uncurry runWriter) writers
  let statuses = readerTests ++ writerTests
  let passes = length $ filter isPass statuses
  let failures = length statuses - passes
  putStrLn $ show passes ++ " tests passed, " ++ show failures ++ " failed."
  if all isPass statuses
     then exitWith ExitSuccess
     else exitWith $ ExitFailure failures

printPass :: FilePath -> FilePath -> IO ()
printPass _inp _out = return () -- putStrLn $ "PASSED:  " ++ inp ++ " ==> " ++ out

printFail :: FilePath -> FilePath -> String -> IO ()
printFail inp out actual =  withTempDirectory "." (inp ++ ".") $ \tmpDir -> do
  -- break native files at commas for easier diffing
  let breakAtCommas = if takeExtension out == ".native"
                         then intercalate ",\n" . splitWhen (==',')
                         else id
  putStrLn $ "FAILED:  " ++ inp ++ " ==> " ++ out
  readFile' out >>=
    writeFile (tmpDir </> "expected") . ensureFinalNewline . breakAtCommas
  writeFile (tmpDir </> "actual") $ ensureFinalNewline $ breakAtCommas actual
  hFlush stdout
  _ <- runProcess "diff" ["-u", tmpDir </> "expected", tmpDir </> "actual"]
           Nothing Nothing Nothing Nothing Nothing >>= waitForProcess
  putStrLn ""

printError :: FilePath -> FilePath -> String -> IO ()
printError inp out msg =
  putStrLn $ "ERROR:  " ++ inp ++ " ==> " ++ out ++ "\n" ++ msg

ensureFinalNewline :: String -> String
ensureFinalNewline "" = ""
ensureFinalNewline xs = if last xs == '\n' then xs else xs ++ "\n"

isPass :: Status -> Bool
isPass (Pass _ _) = True
isPass _ = False

runReader :: String -> (String -> Either String [Exp]) -> IO [Status]
runReader ext f = do
  input <- filter (\x -> takeExtension x == ext) <$> getDirectoryContents "./src"
  let input' = map ("src" </>) input
  mapM (\inp -> runReaderTest (\x -> show <$> f x) inp (rename inp)) input'
  where
    rename x = replaceExtension (replaceDirectory x ("./readers" </> (tail ext))) "native"

runWriter ::  String -> ([Exp] -> String) -> IO [Status]
runWriter ext f = do
  mmls <- map ("./readers/mml/"++) <$> getDirectoryContents "./readers/mml"
  texs <- map ("./readers/tex/"++) <$> getDirectoryContents "./readers/tex"
  let sources = mmls ++ texs
  let inputs = [x | x <- sources, takeExtension x == ".native"]
  mapM (\inp -> runWriterTest f inp ("./writers/" ++ takeBaseName inp ++ ext))
         inputs

runWriterTest :: ([Exp] -> String) -> FilePath -> FilePath -> IO Status
runWriterTest f input output = do
  rawnative <- readFile' input
  native <- case reads rawnative of
                 ((x,_):_) -> return x
                 []        -> error $ "Could not read " ++ input
  let result = ensureFinalNewline $ f native
  --writeFile output result -- uncomment to regen rests (use with care!)
  out_t <- ensureFinalNewline <$> readFile' output
  if result == out_t
     then printPass input output >> return (Pass input output)
     else printFail input output result >> return (Fail input output)

runReaderTest :: (String -> Either String String)
        -> FilePath
        -> FilePath
        -> IO Status
runReaderTest fn input output = do
  inp_t <- readFile' input
  out_t <- ensureFinalNewline <$> readFile' output
  case ensureFinalNewline <$> (fn inp_t) of
       Left msg       -> printError input output msg >>
                         return (Error input output)
       Right r
         | r == out_t -> printPass input output >>
                         return (Pass input output)
         | otherwise  -> printFail input output r >>
                         return (Fail input output)
