{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import System.FilePath
import Text.XML.Light
import System.IO
import System.IO.Temp (withTempDirectory)
import System.Process
import Text.TeXMath
import System.Exit
import Control.Monad
import Data.Semigroup ((<>))
import GHC.IO.Encoding (setLocaleEncoding)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

data Status = Pass FilePath FilePath
            | Fail FilePath FilePath
            | Error FilePath FilePath
            deriving (Eq, Show)

type Ext = String

readers :: [(Ext, T.Text -> Either T.Text [Exp])]
readers = [ (".tex", readTeX)
          , (".mml", readMathML)
          , (".omml", readOMML)
          ]

writers :: [(Ext, [Exp] -> T.Text)]
writers = [ (".mml", T.pack . ppTopElement . writeMathML DisplayBlock)
          , (".tex", writeTeX)
          , (".omml", T.pack . ppTopElement . writeOMML DisplayBlock)
          , (".eqn", writeEqn DisplayBlock)
          ]

-- when called with --round-trip, JUST do round trip tests.
-- otherwise omit them.
main :: IO ()
main = do
  args <- getArgs
  let roundTrip = "--round-trip" `elem` args
  let regen = "--regenerate-tests" `elem` args || "--accept" `elem` args
  setLocaleEncoding utf8
  setCurrentDirectory "tests"
  statuses <- if roundTrip
                 then do
                   texs <- runRoundTrip "tex" writeTeX readTeX
                   ommls <- runRoundTrip "omml"
                               (T.pack . ppTopElement .  writeOMML DisplayBlock) readOMML
                   mathmls <- runRoundTrip "mml"
                                (T.pack . ppTopElement . writeMathML DisplayBlock)
                                readMathML
                   return $ texs <> ommls <> mathmls
                 else (<>) <$> (concat <$> mapM (uncurry (runReader regen)) readers)
                           <*> (concat <$> mapM (uncurry (runWriter regen)) writers)
  let passes = length $ filter isPass statuses
  let failures = length statuses - passes
  T.putStrLn $ tshow passes <> " tests passed, " <> tshow failures <> " failed."
  if all isPass statuses
     then exitWith ExitSuccess
     else exitWith $ ExitFailure failures

printPass :: FilePath -> FilePath -> IO ()
printPass _inp _out = return () -- putStrLn $ "PASSED:  " ++ inp ++ " ==> " ++ out

-- Second parameter is Left format (for round-trip) or Right output file.
printFail :: FilePath -> Either T.Text FilePath -> T.Text -> IO ()
printFail inp out actual =
  withTempDirectory "." ((either (const inp) id out) ++ ".") $ \tmpDir -> do
    -- break native files at commas for easier diffing
    let breakAtCommas = case out of
                             Left _ -> T.intercalate ",\n" . T.split (==',')
                             Right f | takeExtension f == ".native" ->
                                       T.intercalate ",\n" . T.split (==',')
                             _ -> id
    T.putStrLn $ "FAILED:  " <> T.pack inp <> " ==> " <>
      either (\x -> "round trip via " <> x) T.pack out
    T.readFile (either (const inp) id out) >>=
      T.writeFile (tmpDir </> "expected") . ensureFinalNewline . breakAtCommas
    T.writeFile (tmpDir </> "actual") $ ensureFinalNewline $ breakAtCommas actual
    hFlush stdout
    _ <- runProcess "diff" ["-u", tmpDir </> "expected", tmpDir </> "actual"]
             Nothing Nothing Nothing Nothing Nothing >>= waitForProcess
    putStrLn ""

printError :: FilePath -> FilePath -> T.Text -> IO ()
printError inp out msg =
  T.putStrLn $ "ERROR:  " <> T.pack inp <> " ==> " <> T.pack out <> "\n" <> msg

ensureFinalNewline :: T.Text -> T.Text
ensureFinalNewline xs = case T.unsnoc xs of
  Nothing        -> xs
  Just (_, '\n') -> xs
  _              -> xs <> "\n"

isPass :: Status -> Bool
isPass (Pass _ _) = True
isPass _ = False

runReader :: Bool -> Ext -> (T.Text -> Either T.Text [Exp]) -> IO [Status]
runReader regen ext f = do
  input <- filter (\x -> takeExtension x == ext) <$> getDirectoryContents "./src"
  let input' = map ("src" </>) input
  mapM (\inp ->
        runReaderTest regen (\x -> tshow <$> f x) inp (rename inp)) input'
  where
    rename x = replaceExtension (replaceDirectory x ("./readers" </> (tail ext))) "native"

runWriter :: Bool -> Ext -> ([Exp] -> T.Text) -> IO [Status]
runWriter regen ext f = do
  mmls <- map ("./readers/mml/"++) <$> getDirectoryContents "./readers/mml"
  texs <- map ("./readers/tex/"++) <$> getDirectoryContents "./readers/tex"
  let sources = mmls ++ texs
  let inputs = [x | x <- sources, takeExtension x == ".native"]
  mapM (\inp ->
         runWriterTest regen f inp ("./writers/" ++ takeBaseName inp ++ ext))
       inputs

runRoundTrip :: String
             -> ([Exp] -> T.Text)
             -> (T.Text -> Either T.Text [Exp])
             -> IO [Status]
runRoundTrip fmt writer reader = do
  inps <- filter (\x -> takeExtension x == ".native") <$>
             map (("./readers/" <> fmt <> "/") <>) <$>
                 getDirectoryContents ("./readers/" <> fmt)
  mapM (runRoundTripTest fmt writer reader) inps

runWriterTest :: Bool -> ([Exp] -> T.Text) -> FilePath -> FilePath -> IO Status
runWriterTest regen f input output = do
  rawnative <- T.readFile input
  native <- case reads $ T.unpack rawnative of
                 ((x,_):_) -> return x
                 []        -> error $ "Could not read " ++ input
  let result = ensureFinalNewline $ f native
  when regen $ T.writeFile output result
  out_t <- ensureFinalNewline <$> T.readFile output
  if result == out_t
     then printPass input output >> return (Pass input output)
     else printFail input (Right output) result >> return (Fail input output)

runReaderTest :: Bool
        -> (T.Text -> Either T.Text T.Text)
        -> FilePath
        -> FilePath
        -> IO Status
runReaderTest regen fn input output = do
  inp_t <- T.readFile input
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
         T.writeFile output (either (const "") id result)
       out_t <- ensureFinalNewline <$> T.readFile output
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
                 -> ([Exp] -> T.Text)
                 -> (T.Text -> Either T.Text [Exp])
                 -> FilePath
                 -> IO Status
runRoundTripTest fmt writer reader input = do
  rawnative <- T.readFile input
  native <- case reads $ T.unpack rawnative of
                 ((x,_):_) -> return x
                 []        -> error $ "Could not read " ++ input
  let rendered = ensureFinalNewline $ writer native
  case reader rendered of
       Left msg        -> printError input input msg >>
                          return (Error input input)
       Right r
         | r == native -> printPass input input >>
                          return (Pass input input)
         | otherwise   -> printFail input (Left $ T.pack fmt) (tshow r) >>
                          return (Fail input input)
