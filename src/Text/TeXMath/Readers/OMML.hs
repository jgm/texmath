{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
  return $ map (\case
                   [x] -> x
                   xs -> EGrouped xs) expList
elemToOMML element  | isElem "m" "oMath" element =
  Just $ elemsToExps $ elChildren element
elemToOMML _ = Nothing

-- oMath can contain w:hyperlink, w:sdt, etc. I can't find a complete
-- documentation of this, so we just unwrap any w:tag among the children
-- of a math element, except w:r (handled as a run) and w:del (tracked
-- deletions, which are dropped). Note that this shouldn't affect the "w"
-- tags in elemToOMathRunElem(s) because, those are underneath an "m:r" tag.
unwrapWTags :: [Element] -> [Element]
unwrapWTags elements = concatMap unwrapChild elements
  where unwrapChild element
          | isElem "w" "del" element = []
          | isElem "w" "r" element = [element]
          | qPrefix (elName element) == Just "w"
            = unwrapWTags (elChildren element)
          | otherwise = [element]

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
                  | Inserted [OMathRunElem]
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
  | isElem "w" "sym" element = TextRun <$> getSymChar element
  | isElem "w" "ins" element = Just $ Inserted $ mapMaybe elemToOMathRunElem (elChildren element)
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
oMathRunElemToText (Inserted es) = mconcat $ map oMathRunElemToText es

oMathRunElemsToText :: [OMathRunElem] -> T.Text
oMathRunElemsToText = T.concat . map oMathRunElemToText

oMathRunTextStyleToTextType :: OMathRunTextStyle -> Maybe TextType
oMathRunTextStyleToTextType (Normal) = Just $ TextNormal
oMathRunTextStyleToTextType (NoStyle) = Nothing
oMathRunTextStyleToTextType (Styled scr sty) =
  case scr of
    Just OSansSerif -> Just $ case sty of
                         Just OBold       -> TextSansSerifBold
                         Just OBoldItalic -> TextSansSerifBoldItalic
                         Just OItalic     -> TextSansSerifItalic
                         _                -> TextSansSerif
    Just OScript    -> Just $ if isBold then TextBoldScript else TextScript
    Just OFraktur   -> Just $ if isBold then TextBoldFraktur else TextFraktur
    Just ODoubleStruck -> Just TextDoubleStruck
    Just OMonospace -> Just TextMonospace
    _               -> case sty of  -- roman or no script
                         -- sty=p has always been ignored here; mapping
                         -- it to TextNormal would change a lot of output
                         Just OPlain      -> Nothing
                         Just OBold       -> Just TextBold
                         Just OItalic     -> Just TextItalic
                         Just OBoldItalic -> Just TextBoldItalic
                         Nothing          -> Nothing
  where isBold = sty == Just OBold || sty == Just OBoldItalic

-- merge adjacent Exps with same style; see jgm/pandoc#10560:
mergeExps :: [Exp] -> [Exp]
mergeExps [] = []
mergeExps (EStyled tt xs : EStyled tt' xs' : rest)
  | tt == tt' = mergeExps (EStyled tt (xs <> xs') : rest)
mergeExps (EText tt x : EText tt' x' : rest)
  | tt == tt' = mergeExps (EText tt (x <> x') : rest)
mergeExps (x:xs) = x : mergeExps xs

elemsToExps :: [Element] -> [Exp]
elemsToExps = mergeExps . concat . mapMaybe elemToExps . unwrapWTags

-- | Contents of an optional child element (e.g. m:sub, m:deg), or
-- the empty list if it is absent.
optionalChildExps :: String -> Element -> [Exp]
optionalChildExps name =
  maybe [] (elemsToExps . elChildren) . filterChildName (hasElemName "m" name)

elemToExps :: Element -> Maybe [Exp]
elemToExps element = unGroup <$> elemToExps' element

elemToBase :: Element -> Maybe Exp
elemToBase element | isElem "m" "e" element = do
  bs <- elemToBases element
  return $ case bs of
    (e : []) -> e
    exps     -> EGrouped exps
elemToBase _ = Nothing

elemToBases :: Element -> Maybe [Exp]
elemToBases element | isElem "m" "e" element =
  return $ concat $ mapMaybe elemToExps' (unwrapWTags (elChildren element))
elemToBases _ = Nothing



elemToExps' :: Element -> Maybe [Exp]
elemToExps' element | isElem "m" "acc" element = do
  let chr = filterChildName (hasElemName "m" "accPr") element >>=
            filterChildName (hasElemName "m" "chr") >>=
            findAttrBy (hasElemName "m" "val")
      chr' = maybe "\x302" T.pack chr  -- default to wide hat
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
  let bbPr = filterChildName (hasElemName "m" "borderBoxPr") element
      bltr = bbPr >>= filterChildName (hasElemName "m" "strikeBLTR") >>=
                      findAttrBy (hasElemName "m" "val")
      tlbr = bbPr >>= filterChildName (hasElemName "m" "strikeTLBR") >>=
                      findAttrBy (hasElemName "m" "val")
  return $
    case (bltr, tlbr) of
      (Just "1", Just "1") -> [ECancel XSlash baseExp]
      (Just "1", _)        -> [ECancel ForwardSlash baseExp]
      (_, Just "1")        -> [ECancel BackSlash baseExp]
      _                    -> [EBoxed baseExp]

elemToExps' element | isElem "m" "d" element =
  let baseExps  = mapMaybe
                  elemToBases
                  (elChildren element)
      inDelimExps = map (map Right) baseExps
      dPr = filterChildName (hasElemName "m" "dPr") element
      -- an empty value means no delimiter
      firstChar = T.take 1 . T.pack
      getChr name = dPr >>=
               filterChildName (hasElemName "m" name) >>=
               findAttrBy (hasElemName "m" "val")
      beg = maybe "(" firstChar (getChr "begChr")
      end = maybe ")" firstChar (getChr "endChr")
      sep = maybe "|" firstChar (getChr "sepChr")
      exps = if T.null sep
                then concat inDelimExps
                else intercalate [Left sep] inDelimExps
  in
   Just [EDelimited beg end exps]
elemToExps' element | isElem "m" "eqArr" element =
  let expLst = mapMaybe elemToBases (elChildren element)
      expLst' = map breakOnAmpersand expLst
      cols = foldr max 0 (map length expLst')
      colspecs = take cols $ cycle [AlignRight , AlignLeft]
  in
   return [EArray colspecs expLst']
elemToExps' element | isElem "m" "f" element = do
  num <- filterChildName (hasElemName "m" "num") element
  den <- filterChildName (hasElemName "m" "den") element
  let barType = filterChildName (hasElemName "m" "fPr") element >>=
            filterChildName (hasElemName "m" "type") >>=
            findAttrBy (hasElemName "m" "val")
  let numExp = EGrouped $ elemsToExps (elChildren num)
      denExp = EGrouped $ elemsToExps (elChildren den)
  case barType of
    Just "noBar" -> Just [EFraction NoLineFrac numExp denExp]
    _            -> Just [EFraction NormalFrac numExp denExp]
elemToExps' element | isElem "m" "func" element = do
  fName <- filterChildName (hasElemName "m" "fName") element
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
          elemToBase
  let fnameExp = case mconcat $ mapMaybe elemToExps' (elChildren fName) of
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
            _           -> "\x23DE"   -- default to overbrace
      in
       return $
         case justif of
           Just "top" | not (isHorizBracket chr')
                      -> [EUnder False (ESymbol TOver chr') baseExp]
           _ -> [EOver False baseExp (ESymbol TOver chr')]
    _ -> -- bot is default
      let chr' = case chr of
            Just (c:_) -> T.singleton c
            _           -> "\x23DF"   -- default to underbrace
      in
       return $
         case justif of
           _ | isHorizBracket chr' -- e.g. underbrace
             -> [EUnder False baseExp (ESymbol TUnder chr')]
           Just "top" -> [EUnder False baseExp (ESymbol TUnder chr')]
           _ -> [EOver False (ESymbol TUnder chr') baseExp]
  where
    -- horizontal brackets (e.g. underbrace) decorate the base,
    -- unlike e.g. arrows, which carry text above or below them
    isHorizBracket c = c `elem` [ "\x23DC", "\x23DD", "\x23DE", "\x23DF"
                                , "\x23B4", "\x23B5"
                                , "\xFE35", "\xFE36", "\xFE37", "\xFE38" ]
elemToExps' element | isElem "m" "limLow" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element
          >>= elemToBase
  limExp <- filterChildName (hasElemName "m" "lim") element
            >>= (\e -> Just $ elemsToExps (elChildren e))
            >>= (return . EGrouped)
  return [EUnder True baseExp limExp]
elemToExps' element | isElem "m" "limUpp" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element
          >>= elemToBase
  limExp <- filterChildName (hasElemName "m" "lim") element
            >>= (\e -> Just $ elemsToExps (elChildren e))
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
  let subExps = optionalChildExps "sub" element
      supExps = optionalChildExps "sup" element
  let baseExp = maybeToList $
        filterChildName (hasElemName "m" "e") element >>= elemToBase
  let op = ESymbol Op opChr
  let undOvr = limLoc == Just "undOvr"
  return $ (case (subExps, supExps) of
              ([], []) -> op
              (_, [])
                | undOvr    -> EUnder True op (EGrouped subExps)
                | otherwise -> ESub op (EGrouped subExps)
              ([], _)
                | undOvr    -> EOver True op (EGrouped supExps)
                | otherwise -> ESuper op (EGrouped supExps)
              _
                | undOvr    -> EUnderover True op (EGrouped subExps)
                                                  (EGrouped supExps)
                | otherwise -> ESubsup op (EGrouped subExps)
                                          (EGrouped supExps))
           : baseExp

elemToExps' element | isElem "m" "phant" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  return [EPhantom baseExp]
elemToExps' element | isElem "m" "rad" element = do
  let degExps = optionalChildExps "deg" element
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  return $ case degExps of
    [] -> [ESqrt baseExp]
    ds -> [ERoot (EGrouped ds) baseExp]
elemToExps' element | isElem "m" "sPre" element = do
  let subExps = optionalChildExps "sub" element
      supExps = optionalChildExps "sup" element
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
  let subExps = optionalChildExps "sub" element
  return [ESub baseExp (EGrouped subExps)]
elemToExps' element | isElem "m" "sSubSup" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  let subExps = optionalChildExps "sub" element
      supExps = optionalChildExps "sup" element
  return [ESubsup baseExp (EGrouped subExps) (EGrouped supExps)]
elemToExps' element | isElem "m" "sSup" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  let supExps = optionalChildExps "sup" element
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
                -- keep & outside of the styled group, so that it
                -- can be recognized as an alignment point in m:eqArr
                intercalate [ESymbol Ord "&"] $
                  map (\xs -> [EStyled textSty xs | not (null xs)]) $
                  breakOnAmpersand $ interpretText $
                  oMathRunElemsToText mrElems
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
  | T.null s = []
  | Just (c, xs) <- T.uncons s
  , T.null xs = [interpretChar c]
  | T.all isDigit s         = [ENumber s]
  | isJust (getOperator (EMathOperator s))
                          = [EMathOperator s]
  | otherwise             = go s
  where
    -- keep numbers like 12.5 together
    go t = case T.uncons t of
      Nothing -> []
      Just (c, rest)
        | isDigit c ->
          let (ds, rest') = T.span isDigit t
          in case T.uncons rest' of
               Just ('.', rest'')
                 | (ds', rest''') <- T.span isDigit rest''
                 , not (T.null ds')
                 -> ENumber (ds <> "." <> ds') : go rest'''
               _ -> ENumber ds : go rest'
        | otherwise -> interpretChar c : go rest

-- The char attribute is a hex string
getSymChar :: Element -> Maybe T.Text
getSymChar element = do
  s <- lowerFromPrivate <$> getCodepoint
  font <- getFont
  case readLitChar ("\\x" ++ s) of
     [(char, _)] -> T.singleton <$> getUnicode font char
     _ -> Nothing
  where
    getCodepoint = findAttrBy (hasElemName "w" "char") element
    getFont = (textToFont . T.pack) =<< findAttrBy (hasElemName "w" "font") element
    -- Symbol fonts store codepoints in the private use area
    -- (F000-F0FF); subtract 0xF000 to get the character code.
    lowerFromPrivate (c:xs) | c == 'F' || c == 'f' = '0':xs
    lowerFromPrivate xs = xs
