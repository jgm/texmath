#!/usr/bin/env cabal
{-# LANGUAGE OverloadedStrings #-}
{- cabal:
    build-depends: base, texmath, text, xml, containers
-}

import Text.XML.Light
import Control.Applicative
import Data.Maybe
import Debug.Trace
import Numeric
import Text.TeXMath.Types
import Data.List (intersperse)
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)

updates :: [M.Map (Text, FormType) Operator -> M.Map (Text, FormType) Operator]
updates = 
  [ M.insert ("\65079", FInfix) c65079
  , M.insert ("\65080", FInfix) c65080
  ]

c65079 =  Operator {oper = "\65079", description = "PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET" , form = FInfix, priority = 880, lspace = 0, rspace = 0, properties = ["stretchy", "accent"]}
c65080 = Operator {oper = "\65080", description = "PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET", form = FInfix, priority = 880, lspace = 0, rspace = 0, properties = ["stretchy", "accent"]}

applyChanges = foldr (.) id updates

mkMap :: [Operator] -> M.Map (Text, FormType) Operator
mkMap operators = M.fromList (map (\o -> ((oper o, form o), o)) operators)

findAttrQ s = findAttr (QName s Nothing Nothing)

toChar :: Element -> Maybe String
toChar s = toHex <$> findAttrQ "unicode"  s

toHex :: String -> String
toHex s = foldr (:) [] ss 
  where
    s' = drop 1 s
    ss = map (chr . fst . head . readHex) (splitMany s' '-')

splitMany :: String -> Char -> [String]
splitMany [] _  = []
splitMany s sep =  case bs of
  [] -> [fs]
  _ -> fs : (splitMany (tail bs) sep)
  where
    (fs, bs) = span (/= sep) s


f :: Element -> Maybe Operator
f e = Operator <$> (T.pack <$> c) <*> (T.pack <$> d) <*> f <*> p <*> ls <*> rs <*> ps
  where
    c = toChar e
    d = findAttrQ "description" e 
    f = mapForm <$> findAttrQ "form" e
    p = read <$> findAttrQ "priority" e
    ls = read <$> findAttrQ "lspace" e
    rs = read <$> findAttrQ "rspace" e
    ps = return $ fromMaybe [] (T.words . T.pack <$> findAttrQ "properties" e)

mapForm "prefix" = FPrefix
mapForm "postfix" = FPostfix
mapForm "infix" = FInfix
mapForm _ = FInfix

main :: IO ()
main = do 
  Just dict <- parseXMLDoc <$> readFile "dictionary.xml"
  let b =  map snd . M.toList . applyChanges . mkMap <$> mapM f (elChildren dict)
  writeFile "mmldict.hs" (
    "operators :: [Operator]\n" ++
    "operators =\n" ++
    "  [ " ++ concat (intersperse "\n  , " (map show $ fromJust b)) ++
    "]")
