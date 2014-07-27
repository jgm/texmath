{-# LANGUAGE ViewPatterns #-}
{-
Copyright (C) 2014 Matthew Pickering <matthewtpickering@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}
{-
Parses MathML in conformance with the MathML3 specification.

Unimplemented features
  - menclose
  - mpadded
  - mmultiscripts (etc)
  - malignmark
  - maligngroup
  - Elementary Math

To Improve
  - Handling of menclose
  - Handling of mstyle
-}

module Text.TeXMath.Readers.MathML (readMathML) where

import Text.XML.Light hiding (onlyText)
import Text.TeXMath.Types
import Text.TeXMath.Readers.MathML.MMLDict (getOperator)
import Text.TeXMath.Readers.MathML.EntityMap (getUnicode)
import Text.TeXMath.Shared (getTextType, readLength)
import Text.TeXMath.Compat (throwError, Except, runExcept, MonadError)
import Control.Applicative ((<$>), (<|>), (<*>))
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (mconcat, First(..), getFirst)
import Data.List (transpose)
import Control.Monad (filterM, guard)
import Control.Monad.Reader (ReaderT, runReaderT, asks, local)
import Data.Generics (everywhere, mkT)

-- | Parse a MathML expression to a list of 'Exp'
readMathML :: String -> Either String [Exp]
readMathML inp = map fixTree <$> (runExcept (flip runReaderT def (i >>= parseMathML)))
  where
    i = maybeToEither "Invalid XML" (parseXMLDoc inp)

fixTree :: Exp -> Exp
fixTree = everywhere (mkT fixNesting)

data MMLState = MMLState { attrs :: [Attr]
                         , position :: Maybe FormType }

type MML = ReaderT MMLState (Except String)

parseMathML :: Element -> MML [Exp]
parseMathML e@(name -> "math") = group e
parseMathML _ = throwError "Root must be math element"

expr :: Element -> MML Exp
expr e = local (addAttrs (elAttribs e)) (expr' e)

expr' :: Element -> MML Exp
expr' e =
  case name e of
    "mi" -> ident e
    "mn" -> number e
    "mo" -> op e
    "mtext" -> text e
    "ms" -> literal e
    "mspace" -> space e
    "mrow" -> row e
    "mstyle" -> style e
    "mfrac" -> frac e
    "msqrt" -> msqrt e
    "mroot" -> kroot e
    "merror" -> return $ empty
    "mpadded" -> row e
    "mphantom" -> phantom e
    "mfenced" -> fenced e
    "menclose" -> enclosed e
    "msub" -> sub e
    "msup" -> sup e
    "msubsup" -> subsup e
    "munder" -> under e
    "mover" -> over e
    "munderover" -> underover e
    "mtable" -> table e
    "maction" -> action e
    "semantics" -> semantics e
    _ -> return $ empty


-- Tokens

empty :: Exp
empty = EGrouped []

isEmpty :: Exp -> Bool
isEmpty (EGrouped []) = True
isEmpty _ = False

ident :: Element -> MML Exp
ident e =  do
  mv <- maybe EIdentifier (EText . getTextType) <$> findAttrQ "mathvariant" e
  return $ mv (getString e)

number :: Element -> MML Exp
number e = return $ ENumber (getString e)

op :: Element -> MML Exp
op e = do
  Just inferredPosition <- (<|>) <$> (getFormType <$> findAttrQ "form" e)
                            <*> asks position
  let opString = getString e
  let dummy = Operator opString "" inferredPosition 0 0 0 ["mathoperator"]
  let opDict = fromMaybe dummy
                (getOperator opString inferredPosition)
  props <- filterM (checkAttr (properties opDict))
            ["mathoperator", "fence", "accent", "stretchy"]
  let objectPosition = form opDict
  let stretchCons = if ("stretchy" `elem` props)
                      then EStretchy else id
  let ts =  [("accent", ESymbol Accent), ("mathoperator", EMathOperator),
            ("fence", ESymbol (getPosition objectPosition))]
  let constructor =
        fromMaybe (ESymbol Op)
          (getFirst . mconcat $ map (First . flip lookup ts) props)
  return $ (stretchCons . constructor) (oper opDict)
  where
    checkAttr ps v = maybe (v `elem` ps) (=="true") <$> findAttrQ v e

text :: Element -> MML Exp
text e = do
  textStyle <- maybe TextNormal getTextType
                <$> (findAttrQ "mathvariant" e)
  return $ EText textStyle (getString e)

literal :: Element -> MML Exp
literal e = do
  lquote <- fromMaybe "\x201C" <$> findAttrQ "lquote" e
  rquote <- fromMaybe "\x201D" <$> findAttrQ "rquote" e
  EText ttype cont <- text e
  return $ EText ttype (lquote ++ cont ++ rquote)

space :: Element -> MML Exp
space e = do
  width <- fromMaybe "0.0em" <$> (findAttrQ "width" e)
  return $ ESpace (widthToNum width)

-- Layout

style :: Element -> MML Exp
style e = do
  textStyle <- maybe TextNormal getTextType <$> (findAttrQ "mathvariant" e)
  EStyled textStyle <$> group e

row :: Element -> MML Exp
row e = EGrouped <$> group e

group :: Element -> MML [Exp]
group e = do
  front <- mapM expr frontSpaces
  middle <- local resetPosition (row' body)
  end <- local resetPosition (mapM expr endSpaces)
  return $ (front ++ middle ++ end)
  where
    cs = elChildren e
    (frontSpaces, noFront)  = span spacelike cs
    (endSpaces, body) = let (as, bs) = span spacelike (reverse noFront) in
                          (reverse as, reverse bs)

row' :: [Element] -> MML [Exp]
row' [] = return []
row' [x] = do
              pos <- maybe FInfix (const FPostfix) <$> asks position
              (:[]) <$> local (setPosition pos) (expr x)
row' (x:xs) =
  do
    pos <- maybe FPrefix (const FInfix) <$> asks position
    e  <- local (setPosition pos) (expr x)
    es <- local (setPosition pos) (row' xs)
    return (e: es)


frac :: Element -> MML Exp
frac e = do
  [num, dom] <- mapM expr =<< (checkArgs 2 e)
  rawThick <- findAttrQ "linethickness" e
  let constructor = (maybe "\\frac" (\l -> "\\genfrac{}{}{" ++ l ++ "}{}"))
                 (thicknessZero =<< rawThick)
  return $ EBinary constructor num dom

msqrt :: Element -> MML Exp
msqrt e = EUnary "\\sqrt" <$> (row e)

kroot :: Element -> MML Exp
kroot e = do
  [base, index] <- mapM expr =<< (checkArgs 2 e)
  return $ EBinary "\\sqrt" index base

phantom :: Element -> MML Exp
phantom e = EUnary "\\phantom" <$> row e

fenced :: Element -> MML Exp
fenced e = do
  open  <- fromMaybe "(" <$> (findAttrQ "open" e)
  close <- fromMaybe ")" <$> (findAttrQ "close" e)
  let surrounded = not (null open || null close)
  sep  <- fromMaybe "," <$> (findAttrQ "separators" e)
  let expanded =
        case sep of
          "" -> elChildren e
          _  ->
            let seps = map (\x -> unode "mo" [x]) sep
                sepsList = seps ++ repeat (last seps) in
                fInterleave (elChildren e) (sepsList)
  case (sep, surrounded) of
    ("", True) -> EDelimited open close <$> mapM expr (elChildren e)
    (_, True)  -> expr $ sepAttr (unode "mfenced"
                    (elAttribs e, expanded))
    (_, False) -> expr $ unode "mrow"
                          ([unode "mo" open | not $ null open] ++
                           [unode "mrow" expanded] ++
                           [unode "mo" close | not $ null close])
  where
    sepAttr = add_attr (Attr (unqual "separators") "")


-- This could approximate the variants
enclosed :: Element -> MML Exp
enclosed = row

action :: Element -> MML Exp
action e = do
  selection <-  maybe 1 read <$> (findAttrQ "selction" e)  -- 1-indexing
  expr =<< maybeToEither ("Selection out of range")
            (listToMaybe $ drop (selection - 1) (elChildren e))

-- Scripts and Limits

sub :: Element -> MML Exp
sub e = do
  [base, subs] <- (checkArgs 2 e)
  ESub <$> expr base <*> postfixExpr subs

sup :: Element -> MML Exp
sup e = do
  [base, sups] <- (checkArgs 2 e)
  ESuper <$> expr base <*> postfixExpr sups

subsup :: Element -> MML Exp
subsup e = do
  [base, subs, sups] <- (checkArgs 3 e)
  ESubsup <$> expr base <*> (postfixExpr subs)
                         <*> (postfixExpr sups)

under :: Element -> MML Exp
under e = do
  [base, below] <- (checkArgs 2 e)
  EUnder <$> expr base <*> (postfixExpr below)

over :: Element -> MML Exp
over e = do
  [base, above] <- (checkArgs 2 e)
  EOver <$>  expr base <*> (postfixExpr above)

underover :: Element -> MML Exp
underover e = do
  [base, below, above] <- (checkArgs 3 e)
  EUnderover <$> expr base  <*> (postfixExpr below)
                             <*> (postfixExpr above)

-- Other

semantics :: Element -> MML Exp
semantics e = do
  guard (not $ null cs)
  first <- expr (head cs)
  if isEmpty first
    then fromMaybe empty . getFirst . mconcat <$> mapM annotation (tail cs)
    else return first
  where
    cs = elChildren e

annotation :: Element -> MML (First Exp)
annotation e = do
  encoding <- findAttrQ "encoding" e
  case encoding of
    Just "application/mathml-presentation+xml" ->
      First . Just . EGrouped <$> mapM expr (elChildren e)
    Just "MathML-Presentation" ->
      First . Just . EGrouped <$> mapM expr (elChildren e)
    _ -> return (First Nothing)

-- Table

table :: Element -> MML Exp
table e = do
  defAlign <- maybe AlignDefault toAlignment <$> (findAttrQ "columnalign" e)
  rs <- mapM (tableRow defAlign) (elChildren e)
  let (onlyAligns, exprs) = (map .map) fst &&& (map . map) snd $ rs
  let rs' = map (pad (maximum (map length rs))) exprs
  let aligns = map findAlign (transpose onlyAligns)
  return $ EArray aligns rs'
  where
    findAlign xs = if null xs then AlignDefault
                    else foldl1 combine xs
    combine x y = if x == y then x else AlignDefault

tableRow :: Alignment -> Element -> MML [(Alignment, [Exp])]
tableRow a e = do
  align <- maybe a toAlignment <$> (findAttrQ "columnalign" e)
  case name e of
    "mtr" -> mapM (tableCell align) (elChildren e)
    "mlabeledtr" -> mapM (tableCell align) (tail $ elChildren e)
    _ -> throwError $ "Invalid Element: Only expecting mtr elements " ++ err e

tableCell :: Alignment -> Element -> MML (Alignment, [Exp])
tableCell a e = do
  align <- maybe a toAlignment <$> (findAttrQ "columnalign" e)
  case name e of
    "mtd" -> (,) align <$> mapM expr (elChildren e)
    _ -> throwError $ "Invalid Element: Only expecting mtd elements " ++ err e

-- Fixup

-- See Exception for embellished operators in handbox
-- This only affects stretched

fixNesting :: Exp -> Exp
fixNesting (EOver (EStretchy e) s) = EStretchy (EOver e s)
fixNesting (EUnder (EStretchy e) s) = EStretchy (EUnder e s)
fixNesting (EUnderover (EStretchy e) s1 s2) = EStretchy (EUnderover e s1 s2)
fixNesting (ESub (EStretchy e) s) = EStretchy (ESub e s)
fixNesting (ESuper (EStretchy e) s) = EStretchy (ESuper e s)
fixNesting (ESubsup (EStretchy e) s1 s2) = EStretchy (ESubsup e s1 s2)
fixNesting e = e

-- Library Functions

maybeToEither :: (MonadError e m) => e -> Maybe a -> m a
maybeToEither = flip maybe return . throwError

--interleave up to end of shorter list
fInterleave :: [a] -> [a] -> [a]
fInterleave [] _ = []
fInterleave _ [] = []
fInterleave (x:xs) ys = x : fInterleave ys xs

-- MMLState helper functions

def :: MMLState
def = MMLState [] Nothing

addAttrs :: [Attr] -> MMLState -> MMLState
addAttrs as s = s {attrs = as ++ attrs s }

setPosition :: FormType -> MMLState -> MMLState
setPosition p s = s {position = Just p}

resetPosition :: MMLState -> MMLState
resetPosition s = s {position = Nothing}

-- Utility

getString :: Element -> String
getString e = (stripSpaces . concatMap cdData . onlyText . elContent) e

-- Finds only text data and replaces entity references with corresponding
-- characters
onlyText :: [Content] -> [CData]
onlyText [] = []
onlyText ((Text c):xs) = c : onlyText xs
onlyText (CRef s : xs)  = (CData CDataText (fromMaybe s $ getUnicode s) Nothing) : onlyText xs
onlyText (_:xs) = onlyText xs

checkArgs :: Int -> Element -> MML [Element]
checkArgs x e = do
  let cs = elChildren e
  if nargs x cs
    then return cs
    else (throwError ("Incorrect number of arguments for " ++ err e))

nargs :: Int -> [a] -> Bool
nargs n xs = length xs == n

err :: Element -> String
err e = name e ++ " line: " ++ (show $ elLine e) ++ (show e)


findAttrQ :: String -> Element -> MML (Maybe String)
findAttrQ s e = do
  inherit <- asks (lookupAttrQ s . attrs)
  return $
    findAttr (QName s Nothing Nothing) e
      <|> inherit

lookupAttrQ :: String -> [Attr] -> Maybe String
lookupAttrQ s = lookupAttr (QName s Nothing Nothing)

name :: Element -> String
name (elName -> (QName n _ _)) = n

stripSpaces :: String -> String
stripSpaces = reverse . (dropWhile isSpace) . reverse . (dropWhile isSpace)

toAlignment :: String -> Alignment
toAlignment "left" = AlignLeft
toAlignment "center" = AlignCenter
toAlignment "right" = AlignRight
toAlignment _ = AlignDefault

getPosition :: FormType -> TeXSymbolType
getPosition (FPrefix) = Open
getPosition (FPostfix) = Close
getPosition (FInfix) = Op

getFormType :: Maybe String -> Maybe FormType
getFormType (Just "infix") = (Just FInfix)
getFormType (Just "prefix") = (Just FPrefix)
getFormType (Just "postfix") = (Just FPostfix)
getFormType _ = Nothing

pad :: Int -> [[a]] -> [[a]]
pad n xs = xs ++ (replicate (n - len) [])
  where
    len = length xs

isSpace :: Char -> Bool
isSpace ' '  = True
isSpace '\t' = True
isSpace '\n' = True
isSpace _    = False

spacelikeElems, cSpacelikeElems :: [String]
spacelikeElems = ["mtext", "mspace", "maligngroup", "malignmark"]
cSpacelikeElems = ["mrow", "mstyle", "mphantom", "mpadded"]

spacelike :: Element -> Bool
spacelike e@(name -> uid) =
  uid `elem` spacelikeElems || uid `elem` cSpacelikeElems &&
    and (map spacelike (elChildren e))

thicknessZero :: String -> Maybe String
thicknessZero s =
  let l = thicknessToNum s in
  if l == 0.0 then Just "0.0em" else Nothing

widthToNum :: String -> Double
widthToNum s =
  case s of
       "veryverythinmathspace"  -> 1/18
       "verythinmathspace"      -> 2/18
       "thinmathspace"          -> 3/18
       "mediummathspace"        -> 4/18
       "thickmathspace"         -> 5/18
       "verythickmathspace"     -> 6/18
       "veryverythickmathspace" -> 7/18
       "negativeveryverythinmathspace"  -> -1/18
       "negativeverythinmathspace"      -> -2/18
       "negativethinmathspace"          -> -3/18
       "negativemediummathspace"        -> -4/18
       "negativethickmathspace"         -> -5/18
       "negativeverythickmathspace"     -> -6/18
       "negativeveryverythickmathspace" -> -7/18
       _ -> fromMaybe 0 (readLength s)

thicknessToNum :: String -> Double
thicknessToNum s =
  case s of
       "thin" -> 0.175
       "medium" -> 0.5
       "thick" -> 1
       v -> fromMaybe 0.5 (readLength v)

postfixExpr :: Element -> MML Exp
postfixExpr e = local (setPosition FPostfix) (expr e)

