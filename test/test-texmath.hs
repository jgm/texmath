{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import System.FilePath
import Text.XML.Light
import Text.Show.Pretty (ppShow)
import Text.TeXMath
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as B
import Test.Tasty
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.Options
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Encoding as TE (decodeUtf8)
import Data.List (sort)
import Data.Typeable (Typeable)
import Data.Proxy

newtype RoundTrip = RoundTrip Bool
  deriving (Eq, Ord, Typeable)

instance IsOption RoundTrip where
  defaultValue = RoundTrip False
  parseValue = fmap RoundTrip . safeRead
  optionName = return "roundtrip"
  optionHelp = return "Do round-trip tests instead of regular"
  optionCLParser = flagCLParser Nothing (RoundTrip True)

main :: IO ()
main = do
  let getFiles dir = map (dir </>) . sort <$> listDirectory dir
  texReaderTests <- getFiles "test/reader/tex"
  mmlReaderTests <- getFiles "test/reader/mml"
  ommlReaderTests <- getFiles "test/reader/omml"
  texWriterTests <- getFiles "test/writer/tex"
  mmlWriterTests <- getFiles "test/writer/mml"
  ommlWriterTests <- getFiles "test/writer/omml"
  eqnWriterTests <- getFiles "test/writer/eqn"
  typstWriterTests <- getFiles "test/writer/typst"
  regressionTests <- getFiles "test/regression"
  roundtripTests <- getFiles "test/roundtrip"
  let ings = includingOptions [Option (Proxy :: Proxy RoundTrip)] :
                defaultIngredients
  defaultMainWithIngredients ings $ askOption $ \(RoundTrip roundTrip) ->
    testGroup "Tests" $
      if roundTrip
         then
           [ testGroup "roundtrip"
             [ testGroup "tex" $ map (toRoundtripTest "tex") roundtripTests
             , testGroup "mml" $ map (toRoundtripTest "mml") roundtripTests
             , testGroup "mml" $ map (toRoundtripTest "omml") roundtripTests
             ]
           ]
         else
           [ testGroup "reader"
             [ testGroup "tex" $ map toGoldenTest texReaderTests
             , testGroup "mml" $ map toGoldenTest mmlReaderTests
             , testGroup "omml" $ map toGoldenTest ommlReaderTests
             ],
             testGroup "writer"
             [ testGroup "tex" $ map toGoldenTest texWriterTests
             , testGroup "mml" $ map toGoldenTest mmlWriterTests
             , testGroup "omml" $ map toGoldenTest ommlWriterTests
             , testGroup "eqn" $ map toGoldenTest eqnWriterTests
             , testGroup "typst" $ map toGoldenTest typstWriterTests
             ],
             testGroup "regression" $ map toGoldenTest regressionTests
           ]

toRoundtripTest :: T.Text -> FilePath -> TestTree
toRoundtripTest format fp =
  goldenVsStringDiff (takeBaseName fp) diff fp getTested
 where
  diff ref new = ["diff", "-u", ref, new]

  getTested = do
    golden <- read <$> readFile fp
    reader <- maybe (error $ "Unknown input format " <> T.unpack format)
               return $ lookup format readers
    writer <- maybe (error $ "Unknown output format " <> T.unpack format)
               return $ lookup format writers
    case reader (writer golden) of
      Left err -> return $ encodeUtf8 $ TL.fromStrict err
      Right result -> return $ encodeUtf8 $ TL.fromStrict
                             $ ensureFinalNewline $ T.pack $ ppShow result

toGoldenTest :: FilePath -> TestTree
toGoldenTest fp =
  goldenVsStringDiff (takeBaseName fp) diff fp getTested
 where
  diff ref new = ["diff", "-u", ref, new]

  constructGoldenTest ((inFormat, inText), (outFormat, outText)) =
    return $ encodeUtf8 $ TL.fromStrict $
      "<<< " <> inFormat <> "\n" <> inText <> ">>> " <> outFormat <>
      "\n" <> ensureFinalNewline outText

  getTested = do
    ((inFormat, inText), (outFormat, _)) <- readGoldenTest fp
    result <- convert inFormat outFormat inText
    constructGoldenTest ((inFormat, inText), (outFormat, result))

  convert inFormat outFormat inText = do
    reader <- maybe (error $ "Unknown input format " <> T.unpack inFormat)
               return $ lookup inFormat readers
    writer <- maybe (error $ "Unknown output format " <> T.unpack outFormat)
               return $ lookup outFormat writers
    case writer <$> reader inText of
      Left err -> return err
      Right result -> return result

  readGoldenTest fp' = do
    lns <- B.lines <$> B.readFile fp'
    case break ("<<<" `B.isPrefixOf`) lns of
      (_, inputSpec:rest) ->
        case break (">>>" `B.isPrefixOf`) rest of
          (inlines, outputSpec:outlines) -> return
              ((T.strip $ T.drop 3 $ T.pack $ B.unpack inputSpec,
                 TE.decodeUtf8 $ B.unlines inlines),
               (T.strip $ T.drop 3 $ T.pack $ B.unpack outputSpec,
                 TE.decodeUtf8 $ B.unlines outlines))
          _ -> error $ fp' <> " contains no >>> output spec"
      _ -> error $ fp' <> " contains no <<< input spec"

ensureFinalNewline :: T.Text -> T.Text
ensureFinalNewline xs = case T.unsnoc xs of
  Nothing        -> xs
  Just (_, '\n') -> xs
  _              -> xs <> "\n"


readers :: [(T.Text, T.Text -> Either T.Text [Exp])]
readers = [ ("tex", readTeX)
          , ("mml", readMathML)
          , ("omml", readOMML)
          , ("native", readEither)
          ]

readEither :: T.Text -> Either T.Text [Exp]
readEither t = case reads (T.unpack t) of
                  [] -> error "Could not read native value"
                  ((x,_):_) -> return x

writers :: [(T.Text, [Exp] -> T.Text)]
writers = [ ("mml", T.pack . ppTopElement . writeMathML DisplayBlock)
          , ("tex", writeTeX)
          , ("omml", T.pack . ppTopElement . writeOMML DisplayBlock)
          , ("eqn", writeEqn DisplayBlock)
          , ("typst", writeTypst DisplayBlock)
          , ("native", T.pack . ppShow)
          , ("pandoc", maybe "" (T.pack . ppShow) . writePandoc DisplayBlock)
          ]
