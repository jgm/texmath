{-# LANGUAGE OverloadedStrings #-}
import System.Directory
import System.FilePath
import Data.Monoid
import Text.XML.Light
import System.IO
import System.IO.Temp (withTempDirectory)
import System.Process
import Text.TeXMath
import System.Exit
import Control.Applicative
import Control.Monad
import GHC.IO.Encoding (setLocaleEncoding)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.List (intersperse)
import Data.List.Split (splitWhen)
import System.Environment (getArgs)

-- strict version of readFile
readFile' :: FilePath -> IO Text
readFile' f = TIO.readFile f

data Status = Pass FilePath FilePath
            | Fail FilePath FilePath
            | Error FilePath FilePath
            deriving (Eq, Show)

type Ext = String

readers :: [(Ext, Text -> Either String [Exp])]
readers = [ (".tex", readTeX)
          , (".mml", readMathML)
          , (".omml", readOMML)
          ]

writers :: [(Ext, [Exp] -> Text)]
writers = [ (".mml", Text.pack . ppTopElement . writeMathML DisplayBlock)
          , (".tex", writeTeX)
          , (".omml", Text.pack . ppTopElement . writeOMML DisplayBlock)
          ]

-- when called with --round-trip, JUST do round trip tests.
-- otherwise omit them.
main :: IO ()
main = do
  args <- getArgs
  let roundTrip = "--round-trip" `elem` args
  let regen = "--regenerate-tests" `elem` args
  setLocaleEncoding utf8
  setCurrentDirectory "tests"
  statuses <- if roundTrip
                 then do
                   texs <- runRoundTrip "tex" writeTeX readTeX
                   ommls <- runRoundTrip "omml"
                               (Text.pack . ppTopElement .
                                writeOMML DisplayBlock) readOMML
                   mathmls <- runRoundTrip "mmml"
                                (Text.pack . ppTopElement .
                                 writeMathML DisplayBlock)
                                readMathML
                   return $ texs ++ ommls ++ mathmls
                 else (++) <$> (concat <$> mapM (uncurry (runReader regen)) readers)
                           <*> (concat <$> mapM (uncurry (runWriter regen)) writers)
  let passes = length $ filter isPass statuses
  let failures = length statuses - passes
  putStrLn $ show passes ++ " tests passed, " ++ show failures ++ " failed."
  if all isPass statuses
     then exitWith ExitSuccess
     else exitWith $ ExitFailure failures

printPass :: FilePath -> FilePath -> IO ()
printPass _inp _out = return () -- putStrLn $ "PASSED:  " ++ inp ++ " ==> " ++ out

-- Second parameter is Left format (for round-trip) or Right output file.
printFail :: FilePath -> Either String FilePath -> Text -> IO ()
printFail inp out actual =
  withTempDirectory "." ((either (const inp) id out) ++ ".") $ \tmpDir -> do
    -- break native files at commas for easier diffing
    let breakCommas = Text.concat . intersperse ",\n" . Text.splitOn ","
    let breakAtCommas = case out of
                             Left _ -> breakCommas
                             Right f | takeExtension f == ".native" ->
                                       breakCommas
                             _ -> id
    putStrLn $ "FAILED:  " ++ inp ++ " ==> " ++
              either (\x -> "round trip via " ++ x) id out
    readFile' (either (const inp) id out) >>=
      TIO.writeFile (tmpDir </> "expected") . ensureFinalNewline .
      breakAtCommas
    TIO.writeFile (tmpDir </> "actual") $ ensureFinalNewline $
      breakAtCommas actual
    hFlush stdout
    _ <- runProcess "diff" ["-u", tmpDir </> "expected", tmpDir </> "actual"]
             Nothing Nothing Nothing Nothing Nothing >>= waitForProcess
    TIO.putStrLn ""

printError :: FilePath -> FilePath -> String -> IO ()
printError inp out msg =
  putStrLn $ "ERROR:  " ++ inp ++ " ==> " ++ out ++ "\n" ++ msg

ensureFinalNewline :: Text -> Text
ensureFinalNewline xs
  | Text.null xs = Text.empty
  | Text.last xs == '\n' = xs
  | otherwise = xs <> "\n"

isPass :: Status -> Bool
isPass (Pass _ _) = True
isPass _ = False

runReader :: Bool -> String -> (Text -> Either String [Exp]) -> IO [Status]
runReader regen ext f = do
  input <- filter (\x -> takeExtension x == ext) <$> getDirectoryContents "./src"
  let input' = map ("src" </>) input
  mapM (\inp ->
        runReaderTest regen (\x -> Text.pack . show <$> f x)
          inp (rename inp)) input'
  where
    rename x = replaceExtension (replaceDirectory x ("./readers" </> (tail ext))) "native"

runWriter ::  Bool -> String -> ([Exp] -> Text) -> IO [Status]
runWriter regen ext f = do
  mmls <- map ("./readers/mml/"++) <$> getDirectoryContents "./readers/mml"
  texs <- map ("./readers/tex/"++) <$> getDirectoryContents "./readers/tex"
  let sources = mmls ++ texs
  let inputs = [x | x <- sources, takeExtension x == ".native"]
  mapM (\inp ->
         runWriterTest regen f inp ("./writers/" ++ takeBaseName inp ++ ext))
       inputs

runRoundTrip :: String
             -> ([Exp] -> Text)
             -> (Text -> Either String [Exp])
             -> IO [Status]
runRoundTrip fmt writer reader = do
  inps <- filter (\x -> takeExtension x == ".native") <$>
             map (("./readers/" ++ fmt ++ "/") ++) <$>
                 getDirectoryContents ("./readers/" ++ fmt)
  mapM (runRoundTripTest fmt writer reader) inps

runWriterTest :: Bool -> ([Exp] -> Text) -> FilePath -> FilePath -> IO Status
runWriterTest regen f input output = do
  rawnative <- readFile' input
  native <- case reads (Text.unpack rawnative) of
                 ((x,_):_) -> return x
                 []        -> error $ "Could not read " ++ input
  let result = ensureFinalNewline $ f native
  when regen $ TIO.writeFile output result
  out_t <- ensureFinalNewline <$> readFile' output
  if result == out_t
     then printPass input output >> return (Pass input output)
     else printFail input (Right output) result >> return (Fail input output)

runReaderTest :: Bool
        -> (Text -> Either String Text)
        -> FilePath
        -> FilePath
        -> IO Status
runReaderTest regen fn input output = do
  inp_t <- readFile' input
  let result = ensureFinalNewline <$> fn inp_t
  let errfile = dropExtension output ++ ".error"
  errorExpected <- doesFileExist errfile
  if errorExpected
     then
       case result of
         Left _ -> do
             printPass input errfile
             return (Pass input errfile)
         Right _ -> do
             printError input errfile "error expected but not raised"
             return (Error input errfile)
     else do
       when regen $
         TIO.writeFile output (either (const "") id result)
       out_t <- ensureFinalNewline <$> readFile' output
       case result of
            Left msg -> do
                printError input output msg
                return (Error input output)
            Right r
              | r == out_t -> do
                  printPass input output
                  return (Pass input output)
              | otherwise  -> do
                  printFail input (Right output) r
                  return (Fail input output)

runRoundTripTest :: String
                 -> ([Exp] -> Text)
                 -> (Text -> Either String [Exp])
                 -> FilePath
                 -> IO Status
runRoundTripTest fmt writer reader input = do
  rawnative <- readFile' input
  native <- case reads (Text.unpack rawnative) of
                 ((x,_):_) -> return x
                 []        -> error $ "Could not read " ++ input
  let rendered = ensureFinalNewline $ writer native
  case reader rendered of
       Left msg        -> printError input input msg >>
                          return (Error input input)
       Right r
         | r == native -> printPass input input >>
                          return (Pass input input)
         | otherwise   -> printFail input (Left fmt) (Text.pack $ show r) >>
                          return (Fail input input)
