-- creates the code for toUnicode from UnicodeData.txt
import Data.List.Split
import Control.Applicative
import Data.Maybe
import Data.List

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
  entries <- map toEntry . lines <$>
              readFile "UnicodeData.txt"

  print $ take 5 $ mapMaybe getMathStyle entries

toEntry :: String -> (Int, String)
toEntry s = case splitWhen (==';') s of
                  (x:y:_) -> (read ('0':'x':x), y)
                  _ -> error $ "Bad entry: " ++ s

getMathStyle :: (Int, String) -> Maybe (Int, TextType, String)
getMathStyle (n, s) = go styles s
  where go [] s = Nothing
        go ((x,y):rest) s = if x `isPrefixOf` s
                            then Just (n, y, drop (length x + 1) s)
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
          ,("MATHEMATICAL MONOSPACE", TextMonospace)]

