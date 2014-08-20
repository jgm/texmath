{-# LANGUAGE PatternGuards #-}

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
import Data.Maybe (isJust, mapMaybe, fromMaybe)
import Data.List (intercalate)
import Data.Char (isDigit)
import Text.TeXMath.Types
import Text.TeXMath.Shared (fixTree, getSpaceWidth, getOperator)
import Text.TeXMath.Unicode.ToTeX (getSymbolType)
import Control.Applicative ((<$>))
-- As we constuct from the bottom up, this situation can occur.



readOMML :: String -> Either String [Exp]
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
  Just $ concat $ mapMaybe (elemToExps) (elChildren element)
elemToOMML _ = Nothing

isElem :: String -> String -> Element -> Bool
isElem prefix name element =
  let qp = fromMaybe "" (qPrefix (elName element))
  in
   qName (elName element) == name &&
   qp == prefix

hasElemName:: String -> String -> QName -> Bool
hasElemName prefix name qn =
  let qp = fromMaybe "" (qPrefix qn)
  in
   qName qn == name &&
   qp       == prefix

data OMathRunElem = TextRun String
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


-- TODO: The right way to do this is to use the ampersand to break the
-- text lines into multiple columns. That's tricky, though, and this
-- will get us most of the way for the time being.
filterAmpersand :: Exp -> Exp
filterAmpersand (EIdentifier s)   = EIdentifier (filter ('&' /=) s)
filterAmpersand (EText tt s)      = EText tt (filter ('&' /=) s)
filterAmpersand (EStyled tt exps) = EStyled tt (map filterAmpersand exps)
filterAmpersand (EGrouped exps)   = EGrouped (map filterAmpersand exps)
filterAmpersand e                    = e

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
    || isElem "w" "delText" element = Just $ TextRun $ strContent element
  | isElem "w" "br" element = Just LnBrk
  | isElem "w" "tab" element = Just Tab
  | otherwise = Nothing

elemToOMathRunElems :: Element -> Maybe [OMathRunElem]
elemToOMathRunElems element
  | isElem "w" "r" element
    || isElem "m" "r" element =
      Just $ mapMaybe (elemToOMathRunElem) (elChildren element)
elemToOMathRunElems _ = Nothing

----- And now the TeXMath Creation

oMathRunElemToString :: OMathRunElem -> String
oMathRunElemToString (TextRun s) = s
oMathRunElemToString (LnBrk) = ['\n']
oMathRunElemToString (Tab) = ['\t']

oMathRunElemsToString :: [OMathRunElem] -> String
oMathRunElemsToString = concatMap oMathRunElemToString

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
        Just c -> c
        Nothing -> '^'       -- default to hat.
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  return $ [EOver False baseExp (ESymbol Accent [chr'])]
elemToExps' element | isElem "m" "bar" element = do
  pos <- filterChildName (hasElemName "m" "barPr") element >>=
            filterChildName (hasElemName "m" "pos") >>=
            findAttrBy (hasElemName "m" "val")
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  case pos of
    "top" -> Just [EOver False baseExp (ESymbol Accent "\175")]
    "bot" -> Just [EUnder False baseExp (ESymbol Accent "\818")]
    _     -> Nothing
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
      beg = fromMaybe '(' begChr
      end = fromMaybe ')' endChr
      sep = fromMaybe '|' sepChr
      exps = intercalate [Left [sep]] inDelimExps
  in
   Just [EDelimited [beg] [end] exps]
elemToExps' element | isElem "m" "eqArr" element =
  let expLst = mapMaybe elemToBases (elChildren element)
      expLst' = map (\es -> [map filterAmpersand es]) expLst
  in
   return [EArray [] expLst']
elemToExps' element | isElem "m" "f" element = do
  num <- filterChildName (hasElemName "m" "num") element
  den <- filterChildName (hasElemName "m" "den") element
  let numExp = EGrouped $ concat $ mapMaybe (elemToExps) (elChildren num)
      denExp = EGrouped $ concat $ mapMaybe (elemToExps) (elChildren den)
  return $ [EFraction NormalFrac numExp denExp]
elemToExps' element | isElem "m" "func" element = do
  fName <- filterChildName (hasElemName "m" "fName") element
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
          elemToBase
  -- We need a string for the fname, but omml gives it to us as a
  -- series of oMath elems. We're going to filter out the oMathRuns,
  -- which should work for us most of the time.
  let fnameString = concatMap expToString $
                    concat $ mapMaybe (elemToExps) (elChildren fName)
  return [EMathOperator fnameString, baseExp]
elemToExps' element | isElem "m" "groupChr" element = do
  let gPr = filterChildName (hasElemName "m" "groupChrPr") element
      chr = gPr >>=
            filterChildName (hasElemName "m" "chr") >>=
            findAttrBy (hasElemName "m" "val")
      pos = gPr >>=
            filterChildName (hasElemName "m" "pos") >>=
            findAttrBy (hasElemName "m" "val")
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  case pos of
    Just "top" ->
      let chr' = case chr of
            Just (c:_) -> c
            _           -> '\65079'   -- default to overbrace
      in
       return [EOver False baseExp (ESymbol Accent [chr'])]
    Just "bot" ->
      let chr' = case chr of
            Just (c:_) -> c
            _           -> '\65080'   -- default to underbrace
      in
       return [EUnder False baseExp (ESymbol Accent [chr'])]
    _          -> Nothing
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
  return [EOver True limExp baseExp]
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
        Just (c:_) -> c
        _          -> '\8747'   -- default to integral
      limLoc = naryPr >>=
               filterChildName (hasElemName "m" "limLoc") >>=
               findAttrBy (hasElemName "m" "val")
  subExps <- filterChildName (hasElemName "m" "sub") element >>=
         (\e -> return $ concat $ mapMaybe (elemToExps) (elChildren e))
  supExps <- filterChildName (hasElemName "m" "sup") element >>=
         (\e -> return $ concat $ mapMaybe (elemToExps) (elChildren e))
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  case limLoc of
    Just "undOvr" -> return [EUnderover True
                              (ESymbol Op [opChr])
                              (EGrouped subExps)
                              (EGrouped supExps)
                            , baseExp]
    _             -> return [ESubsup
                              (ESymbol Op [opChr])
                              (EGrouped subExps)
                              (EGrouped supExps)
                            , baseExp]

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
      lit = mrPr >>=
            filterChildName (hasElemName "m" "lit") >>=
            findAttrBy (hasElemName "m" "val")
      txtSty = elemToOMathRunTextStyle element
  mrElems <- elemToOMathRunElems element
  return $ case oMathRunTextStyleToTextType txtSty of
    Nothing -> interpretString $ oMathRunElemsToString mrElems
    Just textType ->
      case lit of
        Just "on" ->
          [EText textType (oMathRunElemsToString mrElems)]
        _         ->
          [EStyled textType $ interpretString $ oMathRunElemsToString mrElems]
elemToExps' _ = Nothing

interpretChar :: Char -> Exp
interpretChar c | isDigit c = ENumber [c]
interpretChar c = case getSymbolType c of
  Alpha           -> EIdentifier [c]
  Ord | isDigit c -> ENumber [c]
      | otherwise -> case getSpaceWidth c of
                           Just x  -> ESpace x
                           Nothing -> ESymbol Ord [c]
  symType         -> ESymbol symType [c]

interpretString :: String -> [Exp]
interpretString [c]       = [interpretChar c]
interpretString s
  | all isDigit s         = [ENumber s]
  | isJust (getOperator (EMathOperator s))
                          = [EMathOperator s]
  | otherwise             =
      case map interpretChar s of
            xs | all isIdentifierOrSpace xs -> [EText TextNormal s]
               | otherwise                  -> xs
  where isIdentifierOrSpace (EIdentifier _) = True
        isIdentifierOrSpace (ESpace _)      = True
        isIdentifierOrSpace _               = False

expToString :: Exp -> String
expToString (ENumber s) = s
expToString (EIdentifier s) = s
expToString (EMathOperator s) = s
expToString (ESymbol _ s) = s
expToString (EText _ s) = s
expToString (EGrouped exps) = concatMap expToString exps
expToString (EStyled _ exps) = concatMap expToString exps
expToString _ = ""
