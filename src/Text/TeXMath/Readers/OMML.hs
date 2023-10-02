{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Copyright (C) 2014 Jesse Rosenthal <jrosenthal@jhu.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
-}

{- |
 Module : Text.TeXMath.Readers.OMML
 Copyright : Copyright (C) 2014 Jesse Rosenthal
 License : GNU GPL, version 2 or above

 Maintainer : Jesse Rosenthal <jrosenthal@jhu.edu>
 Stability : alpha
 Portability : portable

Types and functions for conversion of OMML into TeXMath 'Exp's.
-}

module Text.TeXMath.Readers.OMML (readOMML) where

import Text.XML.Light
import Data.Maybe (isJust, mapMaybe, fromMaybe, maybeToList)
import Data.List (intercalate)
import Data.Char (isDigit, readLitChar)
import qualified Data.Text as T
import Text.TeXMath.Types
import Text.TeXMath.Shared (fixTree, getSpaceWidth, getOperator)
import Text.TeXMath.Unicode.ToTeX (getSymbolType)
import Text.TeXMath.Unicode.Fonts (getUnicode, textToFont)
import Data.List.Split (splitWhen)

readOMML :: T.Text -> Either T.Text [Exp]
readOMML s | Just e <- parseXMLDoc s =
  case elemToOMML e of
    Just exs -> Right $ map fixTree $ unGroup exs
    Nothing   -> Left "xml file was not an <m:oMathPara> or <m:oMath> element."
readOMML _ = Left "Couldn't parse OMML file"

unGroup :: [Exp] -> [Exp]
unGroup [EGrouped exps] = exps
unGroup exps = exps

elemToOMML :: Element -> Maybe [Exp]
elemToOMML element  | isElem "m" "oMathPara" element = do
  let expList = mapMaybe elemToOMML (elChildren element)
  return $ map (\l -> if length l == 1 then (head l) else EGrouped l) expList
elemToOMML element  | isElem "m" "oMath" element =
  Just $ concat $ mapMaybe elemToExps $ unwrapWTags $ elChildren element
elemToOMML _ = Nothing

-- oMath can contain w:hyperlink, w:sdt, etc. I can't find a complete
-- documentation of this, so we just unwrap any w:tag immediately
-- beneath oMath. Note that this shouldn't affect the "w" tags in
-- elemToOMathRunElem(s) because, those are underneath an "m:r" tag.
unwrapWTags :: [Element] -> [Element]
unwrapWTags elements = concatMap unwrapChild elements
  where unwrapChild element = case qPrefix $ elName element of
                                Just "w" -> elChildren element
                                _        -> [element]

-- Kept as String because of Text.XML.Light
isElem :: String -> String -> Element -> Bool
isElem prefix name element =
  let qp = fromMaybe "" (qPrefix (elName element))
  in
   qName (elName element) == name &&
   qp == prefix

-- Kept as String because of Text.XML.Light
hasElemName :: String -> String -> QName -> Bool
hasElemName prefix name qn =
  let qp = fromMaybe "" (qPrefix qn)
  in
   qName qn == name &&
   qp       == prefix

data OMathRunElem = TextRun T.Text
                  | LnBrk
                  | Tab
                    deriving Show

data OMathRunTextStyle = NoStyle
                       | Normal
                       | Styled { oMathScript :: Maybe OMathTextScript
                                , oMathStyle  :: Maybe OMathTextStyle }
                       deriving Show

data OMathTextScript = ORoman
                     | OScript
                     | OFraktur
                     | ODoubleStruck
                     | OSansSerif
                     | OMonospace
                     deriving (Show, Eq)

data OMathTextStyle = OPlain
                    | OBold
                    | OItalic
                    | OBoldItalic
                    deriving (Show, Eq)

elemToBase :: Element -> Maybe Exp
elemToBase element | isElem "m" "e" element = do
  bs <- elemToBases element
  return $ case bs of
    (e : []) -> e
    exps     -> EGrouped exps
elemToBase _ = Nothing

elemToBases :: Element -> Maybe [Exp]
elemToBases element | isElem "m" "e" element =
  return $ concat $ mapMaybe elemToExps' (elChildren element)
elemToBases _ = Nothing


breakOnAmpersand :: [Exp] -> [[Exp]]
breakOnAmpersand = splitWhen isAmpersand
 where
  isAmpersand (ESymbol _ "&") = True
  isAmpersand _ = False

elemToOMathRunTextStyle :: Element -> OMathRunTextStyle
elemToOMathRunTextStyle element
  | Just mrPr <- filterChildName (hasElemName"m" "rPr") element
  , Just _    <- filterChildName (hasElemName"m" "nor") mrPr =
    Normal
  | Just mrPr <- filterChildName (hasElemName"m" "rPr") element =
    let scr =
          case
            filterChildName (hasElemName"m" "scr") mrPr >>=
            findAttrBy (hasElemName"m" "val")
          of
            Just "roman"         -> Just ORoman
            Just "script"        -> Just OScript
            Just "fraktur"       -> Just OFraktur
            Just "double-struck" -> Just ODoubleStruck
            Just "sans-serif"    -> Just OSansSerif
            Just "monospace"     -> Just OMonospace
            _                    -> Nothing

        sty =
          case
            filterChildName (hasElemName"m" "sty") mrPr >>=
            findAttrBy (hasElemName"m" "val")
          of
            Just "p"             -> Just OPlain
            Just "b"             -> Just OBold
            Just "i"             -> Just OItalic
            Just "bi"            -> Just OBoldItalic
            _                    -> Nothing
    in
     Styled { oMathScript = scr, oMathStyle = sty }
  | otherwise = NoStyle

elemToOMathRunElem :: Element -> Maybe OMathRunElem
elemToOMathRunElem element
  | isElem "w" "t" element
    || isElem "m" "t" element
    || isElem "w" "delText" element = Just $ TextRun $ T.pack $ strContent element
  | isElem "w" "br" element = Just LnBrk
  | isElem "w" "tab" element = Just Tab
  | isElem "w" "sym" element = Just $ TextRun $ getSymChar element
  | otherwise = Nothing

elemToOMathRunElems :: Element -> Maybe [OMathRunElem]
elemToOMathRunElems element
  | isElem "w" "r" element
    || isElem "m" "r" element =
      Just $ mapMaybe (elemToOMathRunElem) (elChildren element)
elemToOMathRunElems _ = Nothing

----- And now the TeXMath Creation

oMathRunElemToText :: OMathRunElem -> T.Text
oMathRunElemToText (TextRun s) = s
oMathRunElemToText (LnBrk) = "\n"
oMathRunElemToText (Tab) = "\t"

oMathRunElemsToText :: [OMathRunElem] -> T.Text
oMathRunElemsToText = T.concat . map oMathRunElemToText

oMathRunTextStyleToTextType :: OMathRunTextStyle -> Maybe TextType
oMathRunTextStyleToTextType (Normal) = Just $ TextNormal
oMathRunTextStyleToTextType (NoStyle) = Nothing
oMathRunTextStyleToTextType (Styled scr sty)
  | Just OBold <- sty
  , Just OSansSerif <- scr =
    Just $ TextSansSerifBold
  | Just OBoldItalic <- sty
  , Just OSansSerif <- scr =
    Just $ TextSansSerifBoldItalic
  | Just OBold <- sty
  , Just OScript <- scr =
    Just $ TextBoldScript
  | Just OBold <- sty
  , Just OFraktur <- scr =
    Just $ TextBoldFraktur
  | Just OItalic <- sty
  , Just OSansSerif <- scr =
    Just $ TextSansSerifItalic
  | Just OBold <- sty =
    Just $ TextBold
  | Just OItalic <- sty =
    Just $ TextItalic
  | Just OMonospace <- scr =
    Just $ TextMonospace
  | Just OSansSerif <- scr =
    Just $ TextSansSerif
  | Just ODoubleStruck <- scr =
    Just $ TextDoubleStruck
  | Just OScript <- scr =
    Just $ TextScript
  | Just OFraktur <- scr =
    Just $ TextFraktur
  | Just OBoldItalic <- sty =
    Just $ TextBoldItalic
  | otherwise = Nothing

elemToExps :: Element -> Maybe [Exp]
elemToExps element = unGroup <$> elemToExps' element

elemToExps' :: Element -> Maybe [Exp]
elemToExps' element | isElem "m" "acc" element = do
  let chr = filterChildName (hasElemName "m" "accPr") element >>=
            filterChildName (hasElemName "m" "chr") >>=
            findAttrBy (hasElemName "m" "val") >>=
            Just . head
      chr' = case chr of
        Just c -> T.singleton c
        Nothing -> "\x302"       -- default to wide hat.
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  return $ [EOver False baseExp (ESymbol Accent chr')]
elemToExps' element | isElem "m" "bar" element = do
  let pos = filterChildName (hasElemName "m" "barPr") element >>=
            filterChildName (hasElemName "m" "pos") >>=
            findAttrBy (hasElemName "m" "val")
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  -- According to OMML Specification, the default value of pos (whether it exists or not) is "bot"
  -- see https://github.com/jgm/texmath/issues/187
  case pos of
    Just "top" -> Just [EOver False baseExp (ESymbol TOver "\773")]
    _          -> Just [EUnder False baseExp (ESymbol TUnder "\818")]
elemToExps' element | isElem "m" "box" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  return [baseExp]
elemToExps' element | isElem "m" "borderBox" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  return [EBoxed baseExp]
elemToExps' element | isElem "m" "d" element =
  let baseExps  = mapMaybe
                  elemToBases
                  (elChildren element)
      inDelimExps = map (map Right) baseExps
      dPr = filterChildName (hasElemName "m" "dPr") element
      begChr = dPr >>=
               filterChildName (hasElemName "m" "begChr") >>=
               findAttrBy (hasElemName "m" "val") >>=
               (\c -> if null c then (Just ' ') else (Just $ head c))
      sepChr = dPr >>=
               filterChildName (hasElemName "m" "sepChr") >>=
               findAttrBy (hasElemName "m" "val") >>=
               (\c -> if null c then (Just ' ') else (Just $ head c))
      endChr = dPr >>=
               filterChildName (hasElemName "m" "endChr") >>=
               findAttrBy (hasElemName "m" "val") >>=
               (\c -> if null c then (Just ' ') else (Just $ head c))
      beg = maybe "(" T.singleton begChr
      end = maybe ")" T.singleton endChr
      sep = maybe "|" T.singleton sepChr
      exps = intercalate [Left sep] inDelimExps
  in
   Just [EDelimited beg end exps]
elemToExps' element | isElem "m" "eqArr" element =
  let expLst = mapMaybe elemToBases (elChildren element)
      expLst' = map breakOnAmpersand expLst
      cols = maximum (map length expLst')
      colspecs = take cols $ cycle [AlignRight , AlignLeft]
  in
   return [EArray colspecs expLst']
elemToExps' element | isElem "m" "f" element = do
  num <- filterChildName (hasElemName "m" "num") element
  den <- filterChildName (hasElemName "m" "den") element
  let barType = filterChildName (hasElemName "m" "fPr") element >>=
            filterChildName (hasElemName "m" "type") >>=
            findAttrBy (hasElemName "m" "val")
  let numExp = EGrouped $ concat $ mapMaybe (elemToExps) (elChildren num)
      denExp = EGrouped $ concat $ mapMaybe (elemToExps) (elChildren den)
  case barType of
    Just "noBar" -> Just [EFraction NoLineFrac numExp denExp]
    _            -> Just [EFraction NormalFrac numExp denExp]
elemToExps' element | isElem "m" "func" element = do
  fName <- filterChildName (hasElemName "m" "fName") element
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
          elemToBase
  let fnameExp = case mconcat $ mapMaybe (elemToExps') (elChildren fName) of
                   [x] -> x
                   xs  -> EGrouped xs
  return [fnameExp, baseExp]
elemToExps' element | isElem "m" "groupChr" element = do
  let gPr = filterChildName (hasElemName "m" "groupChrPr") element
      chr = gPr >>=
            filterChildName (hasElemName "m" "chr") >>=
            findAttrBy (hasElemName "m" "val")
      pos = gPr >>=
            filterChildName (hasElemName "m" "pos") >>=
            findAttrBy (hasElemName "m" "val")
      justif = gPr >>=
               filterChildName (hasElemName "m" "vertJC") >>=
               findAttrBy (hasElemName "m" "val")
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  case pos of
    Just "top" ->
      let chr' = case chr of
            Just (c:_) -> T.singleton c
            _           -> "\65079"   -- default to overbrace
      in
       return $
         case justif of
           Just "top" -> [EUnder False (ESymbol TOver chr') baseExp]
           _ -> [EOver False baseExp (ESymbol TOver chr')]
    _ -> -- bot is default
      let chr' = case chr of
            Just (c:_) -> T.singleton c
            _           -> "\65080"   -- default to underbrace
      in
       return $
         case justif of
           Just "top" -> [EUnder False baseExp (ESymbol TUnder chr')]
           _ -> [EOver False (ESymbol TUnder chr') baseExp]
elemToExps' element | isElem "m" "limLow" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element
          >>= elemToBase
  limExp <- filterChildName (hasElemName "m" "lim") element
            >>= (\e -> Just $ concat $ mapMaybe (elemToExps) (elChildren e))
            >>= (return . EGrouped)
  return [EUnder True baseExp limExp]
elemToExps' element | isElem "m" "limUpp" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element
          >>= elemToBase
  limExp <- filterChildName (hasElemName "m" "lim") element
            >>= (\e -> Just $ concat $ mapMaybe (elemToExps) (elChildren e))
            >>= (return . EGrouped)
  return [EOver True baseExp limExp]
elemToExps' element | isElem "m" "m" element =
  let rows = filterChildrenName (hasElemName "m" "mr") element
      rowExps = map
                (\mr -> mapMaybe
                        elemToBases
                        (elChildren mr))
                rows
  in
   return [EArray [AlignCenter] rowExps]
elemToExps' element | isElem "m" "nary" element = do
  let naryPr = filterChildName (hasElemName "m" "naryPr") element
      naryChr = naryPr >>=
                filterChildName (hasElemName "m" "chr") >>=
                findAttrBy (hasElemName "m" "val")
      opChr = case naryChr of
        Just (c:_) -> T.singleton c
        _          -> "\8747"   -- default to integral
      limLoc = naryPr >>=
               filterChildName (hasElemName "m" "limLoc") >>=
               findAttrBy (hasElemName "m" "val")
  subExps <- filterChildName (hasElemName "m" "sub") element >>=
         (\e -> return $ concat $ mapMaybe (elemToExps) (elChildren e))
  supExps <- filterChildName (hasElemName "m" "sup") element >>=
         (\e -> return $ concat $ mapMaybe (elemToExps) (elChildren e))
  let baseExp = maybeToList $
        filterChildName (hasElemName "m" "e") element >>= elemToBase
  case limLoc of
    Just "undOvr" -> return $ EUnderover True
                              (ESymbol Op opChr)
                              (EGrouped subExps)
                              (EGrouped supExps)
                            : baseExp
    _             -> return $ ESubsup
                              (ESymbol Op opChr)
                              (EGrouped subExps)
                              (EGrouped supExps)
                            : baseExp

elemToExps' element | isElem "m" "phant" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  return [EPhantom baseExp]
elemToExps' element | isElem "m" "rad" element = do
  degExps <- filterChildName (hasElemName "m" "deg") element >>=
              (\e -> return $ concat $ mapMaybe (elemToExps) (elChildren e))
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  return $ case degExps of
    [] -> [ESqrt baseExp]
    ds -> [ERoot (EGrouped ds) baseExp]
elemToExps' element | isElem "m" "sPre" element = do
  subExps <- filterChildName (hasElemName "m" "sub") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps) (elChildren e))
  supExps <- filterChildName (hasElemName "m" "sup") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps) (elChildren e))
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  return [ESubsup
          (EIdentifier "")
          (EGrouped subExps)
          (EGrouped supExps)
         , baseExp]
elemToExps' element | isElem "m" "sSub" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  subExps <- filterChildName (hasElemName "m" "sub") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps) (elChildren e))
  return [ESub baseExp (EGrouped subExps)]
elemToExps' element | isElem "m" "sSubSup" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  subExps <- filterChildName (hasElemName "m" "sub") element >>=
             (\e -> return $ concat $ mapMaybe (elemToExps) (elChildren e))
  supExps <- filterChildName (hasElemName "m" "sup") element >>=
             (\e -> return $ concat $ mapMaybe (elemToExps) (elChildren e))
  return [ESubsup baseExp (EGrouped subExps) (EGrouped supExps)]
elemToExps' element | isElem "m" "sSup" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  supExps <- filterChildName (hasElemName "m" "sup") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps) (elChildren e))
  return [ESuper baseExp (EGrouped supExps)]
elemToExps' element | isElem "m" "r" element = do
  let mrPr = filterChildName (hasElemName "m" "rPr") element
      lit = mrPr >>= filterChildName (hasElemName "m" "lit")
      nor = mrPr >>= filterChildName (hasElemName "m" "nor")
      txtSty = oMathRunTextStyleToTextType $ elemToOMathRunTextStyle element
  mrElems <- elemToOMathRunElems element
  return $
    if null lit && null nor
       then case txtSty of
              Nothing ->
                interpretText $ oMathRunElemsToText mrElems
              Just textSty ->
                [EStyled textSty $ interpretText $ oMathRunElemsToText mrElems]
       else [EText (fromMaybe TextNormal txtSty) $ oMathRunElemsToText mrElems]
elemToExps' _ = Nothing

interpretChar :: Char -> Exp
interpretChar c | isDigit c = ENumber $ T.singleton c
interpretChar c = case getSymbolType c of
  Alpha           -> EIdentifier c'
  Ord | isDigit c -> ENumber c'
      | otherwise -> case getSpaceWidth c of
                           Just x  -> ESpace x
                           Nothing -> ESymbol Ord c'
  symType         -> ESymbol symType c'
  where
    c' = T.singleton c

interpretText :: T.Text -> [Exp]
interpretText s
  | Just (c, xs) <- T.uncons s
  , T.null xs = [interpretChar c]
  | T.all isDigit s         = [ENumber s]
  | isJust (getOperator (EMathOperator s))
                          = [EMathOperator s]
  | otherwise             = map interpretChar (T.unpack s)

-- The char attribute is a hex string
getSymChar :: Element -> T.Text
getSymChar element
  | Just s <- lowerFromPrivate <$> getCodepoint
  , Just font <- getFont =
  case readLitChar ("\\x" ++ s) of
     [(char, _)] -> maybe "" T.singleton $ getUnicode font char
     _ -> ""
  where
    getCodepoint = findAttrBy (hasElemName "w" "char") element
    getFont = (textToFont . T.pack) =<< findAttrBy (hasElemName "w" "font") element
    lowerFromPrivate ('F':xs) = '0':xs
    lowerFromPrivate xs = xs
getSymChar _ = ""
