-- creates the code for toUnicode from UnicodeData.txt
import Data.List.Split
import Control.Applicative
import Data.Maybe
import Data.Ord
import Data.List
import Data.Char (chr)

-- copied from Text.TeXMath.Types
data TextType = TextNormal
              | TextBold
              | TextItalic
              | TextMonospace
              | TextSansSerif
              | TextDoubleStruck
              | TextScript
              | TextFraktur
              | TextBoldItalic
              | TextSansSerifBold
              | TextSansSerifBoldItalic
              | TextBoldScript
              | TextBoldFraktur
              | TextSansSerifItalic
              deriving (Show, Ord, Read, Eq)

main :: IO ()
main = do
  rawEntries <- lines <$> readFile "UnicodeData.txt"
  -- get all the MATHEMATICAL entries to the head of the table, so we prefer them
  let sortedRawEntries = sortBy (comparing (\x -> if "MATHEMATICAL" `isInfixOf` x then 0 else 1)) rawEntries
  let entries = mapMaybe (\s -> toEntry s >>= getMathStyle) $ sortedRawEntries
  putStrLn "unicodeTable :: [((TextType, Char), Char)]"
  putStr   "unicodeTable = [ "
  putStrLn $ intercalate
         "\n               , " (map show entries) ++ "]"

toEntry :: String -> Maybe (Char, String, Char)
toEntry s = case splitWhen (==';') s of
                  (x:y:_:_:_:('<':'f':'o':'n':'t':'>':' ':z):_) ->
                       Just (readHexChar x, y, readHexChar z)
                  _ -> Nothing
  where readHexChar z = chr $ read $ '0':'x':z

getMathStyle :: (Char, String, Char) -> Maybe ((TextType, Char), Char)
getMathStyle (n, s, m) = go styles s
  where go [] s = Nothing
        go ((x,y):rest) s = if x `isPrefixOf` s
                            then Just ((y, m), n)
                            else go rest s

styleStrings :: [String]
styleStrings = map fst styles

styles :: [(String, TextType)]
styles  = [("MATHEMATICAL BOLD", TextBold)
          ,("MATHEMATICAL ITALIC", TextItalic)
          ,("MATHEMATICAL BOLD ITALIC", TextBoldItalic)
          ,("MATHEMATICAL SCRIPT", TextScript)
          ,("MATHEMATICAL BOLD SCRIPT", TextBoldScript)
          ,("MATHEMATICAL FRAKTUR", TextFraktur)
          ,("MATHEMATICAL DOUBLE-STRUCK", TextDoubleStruck)
          ,("MATHEMATICAL BOLD FRAKTUR", TextBoldFraktur)
          ,("MATHEMATICAL SANS-SERIF", TextSansSerif)
          ,("MATHEMATICAL SANS-SERIF BOLD", TextSansSerifBold)
          ,("MATHEMATICAL SANS-SERIF ITALIC", TextSansSerifItalic)
          ,("MATHEMATICAL SANS-SERIF BOLD ITALIC", TextSansSerifBoldItalic)
          ,("MATHEMATICAL MONOSPACE", TextMonospace)
          ,("BLACK-LETTER", TextFraktur)
          ,("SCRIPT", TextScript)
          ,("DOUBLE-STRUCK", TextDoubleStruck)]

