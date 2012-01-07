{-# LANGUAGE CPP #-}
module Main where

import Text.TeXMath
import Text.TeXMath.Parser (parseFormula)
import Text.XML.Light
import Text.TeXMath.Macros
import System.IO
import System.Environment

inHtml :: Either String Element -> Either String Element
inHtml (Left x) = Left x
inHtml (Right x) = Right $
  add_attr (Attr (unqual "xmlns") "http://www.w3.org/1999/xhtml") $
  unode "html"
    [ unode "head" $
        add_attr (Attr (unqual "content") "application/xhtml+xml; charset=UTF-8") $
        add_attr (Attr (unqual "http-equiv") "Content-Type") $
        unode "meta" ()
    , unode "body" x ]

getUTF8Contents :: IO String
getUTF8Contents =
#if MIN_VERSION_base(4,2,0)
       hSetEncoding stdin utf8 >> hGetContents stdin
#else
       hGetContents stdin
#endif

main :: IO ()
main = do
  args <- getArgs
  let dt = if "--inline" `elem` args
              then DisplayInline
              else DisplayBlock
  let converter = if "--omml" `elem` args
                     then fmap ppTopElement . texMathToOMML dt
                     else if "--native" `elem` args
                          then fmap show . parseFormula
                          else fmap ppTopElement . inHtml . texMathToMathML dt
  inp <- getUTF8Contents
  let (ms, rest) = parseMacroDefinitions inp
  case (converter $! applyMacros ms rest) of
        Left err         -> hPutStrLn stderr err
        Right v          -> putStr v
