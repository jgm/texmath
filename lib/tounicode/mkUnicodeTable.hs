#!/usr/bin/env cabal
{- cabal:
    build-depends: base, split
-}

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
  let entries = mapMaybe (\s -> toEntry s >>= getMathStyle) rawEntries
  writeFile "UnicodeTable.hs" $
    "unicodeTable :: [((TextType, Char), Char)]\n"
    <> "unicodeTable = [ "
    <> intercalate "\n               , " (map showEntry entries) ++
         "\n               ]"

showEntry :: ((TextType, Char), Char) -> String
showEntry ((tt,c),d) = show ((tt,c),d) ++ "  -- " ++ [c] ++ " -> " ++ [d]

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

-- note: it's important to have longer strings below their
-- substrings in this list, which is searched top to bottom!
styles :: [(String, TextType)]
styles  = [ ("MATHEMATICAL SANS-SERIF BOLD ITALIC", TextSansSerifBoldItalic)
          ,("MATHEMATICAL SANS-SERIF BOLD", TextSansSerifBold)
          ,("MATHEMATICAL SANS-SERIF ITALIC", TextSansSerifItalic)
          ,("MATHEMATICAL SANS-SERIF", TextSansSerif)
          ,("MATHEMATICAL BOLD ITALIC", TextBoldItalic)
          ,("MATHEMATICAL BOLD SCRIPT", TextBoldScript)
          ,("MATHEMATICAL BOLD FRAKTUR", TextBoldFraktur)
          ,("MATHEMATICAL BOLD", TextBold)
          ,("MATHEMATICAL ITALIC", TextItalic)
          ,("MATHEMATICAL SCRIPT", TextScript)
          ,("MATHEMATICAL FRAKTUR", TextFraktur)
          ,("MATHEMATICAL DOUBLE-STRUCK", TextDoubleStruck)
          ,("MATHEMATICAL MONOSPACE", TextMonospace)
          ,("BLACK-LETTER", TextFraktur)
          ,("SCRIPT", TextScript)
          ,("DOUBLE-STRUCK", TextDoubleStruck)]

