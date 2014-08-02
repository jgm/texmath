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
{- |

Parses MathML in conformance with the MathML3 specification.

Unimplemented features:

  - menclose
  - mpadded
  - mmultiscripts (etc)
  - malignmark
  - maligngroup
  - Elementary Math

To Improve:

  - Handling of menclose
  - Handling of mstyle
-}

module Text.TeXMath.Readers.MathML (readMathML) where

import Text.XML.Light hiding (onlyText)
import Text.TeXMath.Types
import Text.TeXMath.Readers.MathML.MMLDict (getMathMLOperator)
import Text.TeXMath.Readers.MathML.EntityMap (getUnicode)
import Text.TeXMath.Shared (getTextType, readLength, getOperator)
import Text.TeXMath.Unicode.ToTeX (getSymbolType)
import Text.TeXMath.Compat (throwError, Except, runExcept, MonadError)
import Control.Applicative ((<$>), (<|>), (<*>))
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (mconcat, First(..), getFirst)
import Data.List (transpose)
import Control.Monad (filterM, guard)
import Control.Monad.Reader (ReaderT, runReaderT, asks, local)
import Data.Generics (everywhere, mkT)
import Data.Either (rights)

-- | Parse a MathML expression to a list of 'Exp'.
readMathML :: String -> Either String [Exp]
readMathML inp = map fixTree <$>
  (runExcept (flip runReaderT defaultState (i >>= parseMathML)))
  where
    i = maybeToEither "Invalid XML" (parseXMLDoc inp)

fixTree :: Exp -> Exp
fixTree = everywhere (mkT removeNesting) . everywhere (mkT removeEmpty)

data MMLState = MMLState { attrs :: [Attr]
                         , position :: Maybe FormType
                         , inAccent :: Bool
                         , curStyle :: TextType }

type MML = ReaderT MMLState (Except String)

data SupOrSub = Sub | Sup deriving (Show, Eq)

data IR a = Stretchy TeXSymbolType String
          | Trailing (Exp -> Exp -> Exp) Exp
          | E a

instance Show a => Show (IR a) where
  show (Stretchy t s) = "Stretchy " ++ show t ++ " " ++ show s
  show (Trailing _ s) = "Trailing " ++ show s
  show (E s) = "E " ++ show s

parseMathML :: Element -> MML [Exp]
parseMathML e@(name -> "math") = do
  e' <- row e
  return $
    case e' of
      EGrouped es -> es
      _ -> [e']
parseMathML _ = throwError "Root must be math element"

expr :: Element -> MML [IR Exp]
expr e = local (addAttrs (elAttribs e)) (expr' e)

expr' :: Element -> MML [IR Exp]
expr' e =
  case name e of
    "mi" -> mkE <$> ident e
    "mn" -> mkE <$> number e
    "mo" -> (:[]) <$> op e
    "mtext" -> mkE <$> text e
    "ms" -> mkE <$> literal e
    "mspace" -> mkE <$> space e
    "mrow" -> mkE <$> row e
    "mstyle" -> mkE <$> style e
    "mfrac" -> mkE <$> frac e
    "msqrt" -> mkE <$> msqrt e
    "mroot" -> mkE <$> kroot e
    "merror" -> return (mkE empty)
    "mpadded" -> mkE <$> row e
    "mphantom" -> mkE <$> phantom e
    "mfenced" -> mkE <$> fenced e
    "menclose" -> mkE <$> enclosed e
    "msub" ->  sub e
    "msup" ->  sup e
    "msubsup" -> mkE <$> subsup e
    "munder" -> mkE <$> under e
    "mover" -> mkE <$> over e
    "munderover" -> mkE <$> underover e
    "mtable" -> mkE <$> table e
    "maction" -> mkE <$> action e
    "semantics" -> mkE <$> semantics e
    _ -> return $ mkE empty
  where
    mkE :: Exp -> [IR Exp]
    mkE = (:[]) . E


-- Tokens

empty :: Exp
empty = EGrouped []

isEmpty :: Exp -> Bool
isEmpty (EGrouped []) = True
isEmpty _ = False

ident :: Element -> MML Exp
ident e =  do
  let s = getString e
  let base = case getOperator (EMathOperator s) of
                   Just _   -> EMathOperator s
                   Nothing  -> EIdentifier s
  mbVariant <- findAttrQ "mathvariant" e
  curstyle <- asks curStyle
  case mbVariant of
       Nothing  -> return base
       Just v
         | curstyle == getTextType v -> return base
         | otherwise  -> return $ EStyled (getTextType v) [base]

number :: Element -> MML Exp
number e = return $ (ENumber (getString e))

op :: Element -> MML (IR Exp)
op e = do
  Just inferredPosition <- (<|>) <$> (getFormType <$> findAttrQ "form" e)
                            <*> asks position
  let opString = getString e
  let dummy = Operator opString "" inferredPosition 0 0 0 []
  let opDict = fromMaybe dummy
                (getMathMLOperator opString inferredPosition)
  props <- filterM (checkAttr (properties opDict))
            ["fence", "accent", "stretchy"]
  let objectPosition = getPosition $ form opDict
  inScript <- asks inAccent
  if ("stretchy" `elem` props) && not inScript
    then return $ Stretchy objectPosition (oper opDict)
    else do
      let ts =  [("accent", ESymbol Accent), ("fence", ESymbol objectPosition)]
      let fallback = case opString of
                          [c]   -> ESymbol (getSymbolType c)
                          _     -> EMathOperator
      let constructor =
            fromMaybe fallback
              (getFirst . mconcat $ map (First . flip lookup ts) props)
      return $ (E . constructor) (oper opDict)
  where
    checkAttr ps v = maybe (v `elem` ps) (=="true") <$> findAttrQ v e

text :: Element -> MML Exp
text e = do
  textStyle <- maybe TextNormal getTextType
                <$> (findAttrQ "mathvariant" e)
  return $ (EText textStyle (getString e))

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
  tt <- maybe TextNormal getTextType <$> findAttrQ "mathvariant" e
  curstyle <- asks curStyle
  -- We do not want to propagate the mathvariant else
  -- we end up with nested EStyled applying the same
  -- style
  result <- local (filterMathVariant . enterStyled tt) (row e)
  return $ if curstyle == tt
              then result
              else EStyled tt [result]

row :: Element -> MML Exp
row e = mkExp <$> group e

-- 1. matchNesting strips all additional IR
-- 2. toEDelim
-- 3. toExp makes sure that no additional nesting happens
mkExp :: [IR Exp] -> Exp
mkExp = toExp . toEDelim . matchNesting

toExp :: [InEDelimited] -> Exp
toExp [] = empty
toExp xs =
  if any isStretchy xs
    then case xs of
              [x] -> either (ESymbol Ord) id x
              _ -> EDelimited "" "" xs
    else
      case xs of
        [Right x] -> x
        _ -> EGrouped (rights xs)


toEDelim :: [IR InEDelimited] -> [InEDelimited]
toEDelim [] = []
toEDelim [Stretchy t s] = [Right $ ESymbol t s]
toEDelim (xs) = map removeIR xs

-- Strips internal representation from processed list
removeIR :: IR a -> a
removeIR (E e) = e
removeIR _ = error "removeIR, should only be ever called on processed lists"

-- Convers stretch to InEDelimited element
removeStretch :: IR Exp -> IR InEDelimited
removeStretch (Stretchy _ s) = E $ Left s
removeStretch (E e) = E $ Right e
-- This is never used as the function is only called on portions outside
-- delimiters.
removeStretch (Trailing a b)  = Trailing a b

isStretchy :: InEDelimited -> Bool
isStretchy (Left _) = True
isStretchy (Right _) = False

-- If at the end of a delimiter we need to apply the script to the whole
-- expression. We only insert Trailing when reordering Stretchy
trailingSup :: String -> String -> [IR InEDelimited] -> Exp
trailingSup open close es = go es
  where
    go [] = case (open, close) of
              ("", "") -> empty
              (_, "") -> ESymbol Open open
              ("", _) -> ESymbol Close close
              (_, _)  -> EGrouped [ESymbol Open open, ESymbol Close close]
    go es'@(last -> Trailing constructor e) = (constructor (go (init es')) e)
    go es' = EDelimited open close (toEDelim es')

-- As we constuct from the bottom up, this situation can occur.
removeNesting :: Exp -> Exp
removeNesting (EDelimited o c [Right (EDelimited "" "" xs)]) = EDelimited o c xs
removeNesting (EDelimited "" "" [x]) = either (ESymbol Ord) id x
removeNesting (EGrouped [x]) = x
removeNesting x = x

removeEmpty :: [Exp] -> [Exp]
removeEmpty xs = filter (not . isEmpty) xs

-- TODO: Break this into two functions
-- Matches open and closing brackets
-- The result of this function is a list with only E elements.
matchNesting :: [IR Exp] -> [IR InEDelimited]
matchNesting ((break isFence) -> (inis, rest)) =
  let inis' = map removeStretch inis in
  case rest of
    [] -> inis'
    ((Stretchy Open opens): rs) ->
      let (body, rems) = go rs 0 []
          body' = matchNesting body in
        case rems of
          [] -> inis' ++ [E $ Right $ trailingSup opens "" body']
          (Stretchy Close closes : rs') ->
            inis' ++ (E $ Right $ trailingSup opens closes body') : matchNesting rs'
          _ -> (error "matchNesting: Logical error 1")
    ((Stretchy Close closes): rs) ->
      (E $ Right $ trailingSup "" closes (matchNesting inis)) : matchNesting rs
    _ -> error "matchNesting: Logical error 2"
  where
    isOpen (Stretchy Open _) = True
    isOpen _ = False
    isClose (Stretchy Close _) = True
    isClose _ = False
    go :: [IR a] -> Int -> [IR a] -> ([IR a], [IR a])
    go (x:xs) 0 a | isClose x = (reverse a, x:xs)
    go (x:xs) n a | isOpen x  = go xs (n + 1) (x:a)
    go (x:xs) n a | isClose x = go xs (n - 1) (x:a)
    go (x:xs) n a = go xs n (x:a)
    go [] _ a = (reverse a, [])

isFence :: IR a -> Bool
isFence (Stretchy Open _) = True
isFence (Stretchy Close _) = True
isFence _ = False

group :: Element -> MML [IR Exp]
group e = do
  front <- concat <$> mapM expr frontSpaces
  middle <- local resetPosition (row' body)
  end <- concat <$> local resetPosition (mapM expr endSpaces)
  return $ (front ++ middle ++ end)
  where
    cs = elChildren e
    (frontSpaces, noFront)  = span spacelike cs
    (endSpaces, body) = let (as, bs) = span spacelike (reverse noFront) in
                          (reverse as, reverse bs)

row' :: [Element] -> MML [IR Exp]
row' [] = return []
row' [x] = do
              pos <- maybe FInfix (const FPostfix) <$> asks position
              local (setPosition pos) (expr x)
row' (x:xs) =
  do
    pos <- maybe FPrefix (const FInfix) <$> asks position
    e  <- local (setPosition pos) (expr x)
    es <- local (setPosition pos) (row' xs)
    return (e ++ es)

-- Indicates the closure of scope
safeExpr :: Element -> MML Exp
safeExpr e = mkExp <$> expr e

frac :: Element -> MML Exp
frac e = do 
  [num, dom] <- mapM safeExpr =<< (checkArgs 2 e)
  rawThick <- findAttrQ "linethickness" e
  let constructor = (maybe "\\frac" (\l -> "\\genfrac{}{}{" ++ l ++ "}{}"))
                 (thicknessZero =<< rawThick)
  return $ EBinary constructor num dom

msqrt :: Element -> MML Exp
msqrt e = EUnary "\\sqrt" <$> (row e)

kroot :: Element -> MML Exp
kroot e = do
  [base, index] <- mapM safeExpr =<< (checkArgs 2 e)
  return $ EBinary "\\sqrt" index base

phantom :: Element -> MML Exp
phantom e = EUnary "\\phantom" <$> row e

fenced :: Element -> MML Exp
fenced e = do
  open  <- fromMaybe "(" <$> (findAttrQ "open" e)
  close <- fromMaybe ")" <$> (findAttrQ "close" e)
  sep  <- fromMaybe "," <$> (findAttrQ "separators" e)
  let expanded =
        case sep of
          "" -> elChildren e
          _  ->
            let seps = map (\x -> unode "mo" [x]) sep
                sepsList = seps ++ repeat (last seps) in
                fInterleave (elChildren e) (sepsList)
  safeExpr $ unode "mrow"
              ([unode "mo" open | not $ null open] ++
               [unode "mrow" expanded] ++
               [unode "mo" close | not $ null close])

-- This could approximate the variants
enclosed :: Element -> MML Exp
enclosed = row

action :: Element -> MML Exp
action e = do
  selection <-  maybe 1 read <$> (findAttrQ "selection" e)  -- 1-indexing
  safeExpr =<< maybeToEither ("Selection out of range")
            (listToMaybe $ drop (selection - 1) (elChildren e))

-- Scripts and Limits

sub :: Element -> MML [IR Exp]
sub e =  do
  [base, subs] <- (checkArgs 2 e)
  reorderScripts base subs ESub

-- Handles case with strethy elements in the base of sub/sup
reorderScripts :: Element -> Element -> (Exp -> Exp -> Exp) -> MML [IR Exp]
reorderScripts e subs c = do
  baseExpr <- expr e
  subExpr <- postfixExpr subs
  return $
    case baseExpr of
      [Stretchy Open s] -> [Stretchy Open s, E $ c empty subExpr]  -- Open
      [Stretchy Close s] -> [Trailing c subExpr, Stretchy Close s] -- Close
      [Stretchy o s] -> [Stretchy o s, E $ ESub empty subExpr] -- Middle
      _ -> [E $ c (mkExp baseExpr) subExpr] -- No stretch

sup :: Element -> MML [IR Exp]
sup e = do
  [base, sups] <- (checkArgs 2 e)
  reorderScripts base sups ESuper

subsup :: Element -> MML Exp
subsup e = do
  [base, subs, sups] <- (checkArgs 3 e)
  ESubsup <$> safeExpr base <*> (postfixExpr subs)
                         <*> (postfixExpr sups)

under :: Element -> MML Exp
under e = do
  [base, below] <- (checkArgs 2 e)
  EUnder False <$> safeExpr base <*> postfixExpr below

over :: Element -> MML Exp
over e = do
  [base, above] <- (checkArgs 2 e)
  EOver False <$> safeExpr base <*> postfixExpr above

underover :: Element -> MML Exp
underover e = do
  [base, below, above] <- (checkArgs 3 e)
  EUnderover False <$> safeExpr base  <*> (postfixExpr below)
                                      <*> (postfixExpr above)

-- Other

semantics :: Element -> MML Exp
semantics e = do
  guard (not $ null cs)
  first <- safeExpr (head cs)
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
      First . Just <$> row e
    Just "MathML-Presentation" ->
      First . Just <$> row e
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
    "mtd" -> (,) align . (:[]) <$> row e 
    _ -> throwError $ "Invalid Element: Only expecting mtd elements " ++ err e

-- Fixup

-- Library Functions

maybeToEither :: (MonadError e m) => e -> Maybe a -> m a
maybeToEither = flip maybe return . throwError

--interleave up to end of shorter list
fInterleave :: [a] -> [a] -> [a]
fInterleave [] _ = []
fInterleave _ [] = []
fInterleave (x:xs) ys = x : fInterleave ys xs

-- MMLState helper functions

defaultState :: MMLState
defaultState = MMLState [] Nothing False TextNormal

addAttrs :: [Attr] -> MMLState -> MMLState
addAttrs as s = s {attrs = (map renameAttr as) ++ attrs s }

renameAttr :: Attr -> Attr
renameAttr v@(qName . attrKey -> "accentunder") =
  Attr (unqual "accent") (attrVal v)
renameAttr a = a 

filterMathVariant :: MMLState -> MMLState
filterMathVariant s@(attrs -> as) =
  s{attrs = filter ((/= unqual "mathvariant") . attrKey) as}

setPosition :: FormType -> MMLState -> MMLState
setPosition p s = s {position = Just p}

resetPosition :: MMLState -> MMLState
resetPosition s = s {position = Nothing}

enterAccent :: MMLState -> MMLState
enterAccent s = s{ inAccent = True }

enterStyled :: TextType -> MMLState -> MMLState
enterStyled tt s = s{ curStyle = tt }

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
postfixExpr e = local (setPosition FPostfix . enterAccent) (safeExpr e)

