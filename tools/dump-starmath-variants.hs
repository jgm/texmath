{-# LANGUAGE OverloadedStrings #-}

-- visual review tooling - see README.md in this folder
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Text.TeXMath (DisplayType(..), Exp, readTeX, writeMathML, writeStarMath)
import Text.XML.Light.Output (ppTopElement)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputDir, outputDir] -> dumpDirectory inputDir outputDir
    _ -> error "usage: dump-starmath-variants.hs INPUT_DIR OUTPUT_DIR"

dumpDirectory :: FilePath -> FilePath -> IO ()
dumpDirectory inputDir outputDir = do
  createDirectoryIfMissing True outputDir
  files <- sort <$> listDirectory inputDir
  mapM_ (dumpOne inputDir outputDir) $
    filter (\x -> takeExtension x == ".test") files

dumpOne :: FilePath -> FilePath -> FilePath -> IO ()
dumpOne inputDir outputDir file = do
  let src = inputDir </> file
      base = takeBaseName file
  (inFormat, inText, _, _) <- parseGoldenTest src
  exps <- readInput src inFormat inText
  TIO.writeFile (outputDir </> (base <> ".display.starmath")) $
    writeStarMath DisplayBlock exps <> "\n"
  TIO.writeFile (outputDir </> (base <> ".inline.starmath")) $
    writeStarMath DisplayInline exps <> "\n"
  TIO.writeFile (outputDir </> (base <> ".display.mathml")) $
    T.pack (ppTopElement $ writeMathML DisplayBlock exps) <> "\n"
  TIO.writeFile (outputDir </> (base <> ".inline.mathml")) $
    T.pack (ppTopElement $ writeMathML DisplayInline exps) <> "\n"

parseGoldenTest :: FilePath -> IO (Text, Text, Text, Text)
parseGoldenTest fp = do
  txt <- TIO.readFile fp
  let ls = T.lines txt
  case break (T.isPrefixOf "<<<") ls of
    (_, inputSpec:rest) ->
      case break (T.isPrefixOf ">>>") rest of
        (inlines, outputSpec:outlines) ->
          pure ( T.strip $ T.drop 3 inputSpec
               , T.stripEnd $ T.unlines inlines
               , T.strip $ T.drop 3 outputSpec
               , T.stripEnd $ T.unlines outlines
               )
        _ -> error $ fp <> ": missing >>> block"
    _ -> error $ fp <> ": missing <<< block"

readInput :: FilePath -> Text -> Text -> IO [Exp]
readInput fp inFormat inText =
  case inFormat of
    "native" -> pure $ readNative inText
    "tex" ->
      case readTeX inText of
        Right exps -> pure exps
        Left err -> error $ fp <> ": could not read TeX: " <> T.unpack err
    _ -> error $ fp <> ": expected <<< native or <<< tex"

readNative :: Text -> [Exp]
readNative t =
  case reads (T.unpack t) of
    []      -> error "Could not read native AST"
    (x,_):_ -> x
