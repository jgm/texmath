{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns, GADTs #-}
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

module Text.TeXMath.Writers.TeX (writeTeX, writeTeXWith, addLaTeXEnvironment ) where

import Text.TeXMath.Types
import Text.TeXMath.Unicode.ToTeX (getTeXMath)
import Text.TeXMath.Unicode.ToUnicode (fromUnicode)
import qualified Text.TeXMath.Shared as S
import Data.Maybe (fromMaybe)
import Data.Generics (everywhere, mkT)
import Control.Applicative ((<$>), (<|>), Applicative)
import qualified Data.Map as M
import Control.Monad (when, unless)
import Control.Monad.Reader (MonadReader, runReader, Reader, asks, local)
import Control.Monad.Writer( MonadWriter, WriterT,
                             execWriterT, tell, censor)
import Text.TeXMath.TeX

-- import Debug.Trace
-- tr' x = trace (show x) x

-- | Transforms an expression tree to equivalent LaTeX with the default
-- packages (amsmath and amssymb)
writeTeX :: [Exp] -> String
writeTeX = writeTeXWith defaultEnv

-- | Adds the correct LaTeX environment around a TeXMath fragment
addLaTeXEnvironment :: DisplayType -> String -> String
addLaTeXEnvironment dt math =
    case dt of
      DisplayInline -> "\\(" ++ math ++ "\\)"
      DisplayBlock  -> "\\[" ++ math ++ "\\]"

-- |  Transforms an expression tree to equivalent LaTeX with the specified
-- packages
writeTeXWith :: Env -> [Exp] -> String
writeTeXWith env e = drop 1 . init . flip renderTeX "" . Grouped $
                            runExpr env $
                              mapM_ writeExp (fixTree e)

runExpr :: Env -> Math () -> [TeX]
runExpr e m = flip runReader (MathState e False) $ execWriterT (runTeXMath m)

square :: [String]
square = ["\\sqrt", "\\surd"]

data MathState = MathState{ mathEnv :: Env
                          , mathConvertible :: Bool
                          } deriving Show

setConvertible :: MathState -> MathState
setConvertible s = s{ mathConvertible = True }

newtype Math a = Math {runTeXMath :: WriterT [TeX] (Reader MathState) a}
                  deriving (Functor, Applicative, Monad, MonadReader MathState
                           , MonadWriter [TeX])

getTeXMathM :: String -> Math [TeX]
getTeXMathM s = getTeXMath s <$> asks mathEnv

tellGroup :: Math () -> Math ()
tellGroup = censor ((:[]) . Grouped)

writeExp :: Exp -> Math ()
writeExp (ENumber s) = tell =<< getTeXMathM s
writeExp (EGrouped es) = tellGroup (mapM_ writeExp es)
writeExp (EDelimited open close [Right (EArray aligns rows)]) = do
  env <- asks mathEnv
  case ("amsmath" `elem` env, open, close) of
       (True, "{", "") | aligns == [AlignDefault, AlignDefault] ->
         table "cases" [] rows
       (True, "(", ")") | all (== AlignCenter) aligns ->
         table "pmatrix" [] rows
       (True, "[", "]") | all (== AlignCenter) aligns ->
         table "bmatrix" [] rows
       (True, "{", "}") | all (== AlignCenter) aligns ->
         table "Bmatrix" [] rows
       (True, "\x2223", "\x2223") | all (== AlignCenter) aligns ->
         table "vmatrix" [] rows
       (True, "\x2225", "\x2225") | all (== AlignCenter) aligns ->
         table "Vmatrix" [] rows
       _ -> do
         writeDelim DLeft open
         writeExp (EArray aligns rows)
         writeDelim DRight close
writeExp (EDelimited open close es) =  do
  writeDelim DLeft open
  mapM_ (either (writeDelim DMiddle) writeExp) es
  writeDelim DRight close
writeExp (EIdentifier s) = do
  math <- getTeXMathM s
  case math of
       []      -> return ()
       [t]     -> tell [t]
       ts      -> tell [Grouped ts]
writeExp o@(EMathOperator s) = do
  math <- getTeXMathM s
  case S.getOperator o of
       Just op  -> tell [op]
       Nothing  -> do
         tell [ControlSeq "\\operatorname"]
         -- use \operatorname* if convertible
         asks mathConvertible >>= flip when (tell [Token '*'])
         tell [Grouped math]
writeExp (ESymbol Ord [c])  -- do not render "invisible operators"
  | c `elem` ['\x2061'..'\x2064'] = return () -- see 3.2.5.5 of mathml spec
writeExp (ESymbol t s) = do
  when (t == Bin || t == Rel) $ tell [Space]
  tell =<< getTeXMathM s
  when (t == Bin || t == Rel) $ tell [Space]
writeExp (ESpace width) = tell [ControlSeq $ getSpaceCommand width]
writeExp (EBinary s e1 e2) = do
  tell [ControlSeq s]
  if (s `elem` square)
    then do tell [Token '[']
            writeExp e1
            tell [Token ']']
    else tellGroup (writeExp e1)
  tellGroup (writeExp e2)
writeExp (ESub b e1) = do
  (if isFancy b then tellGroup else id) $ writeExp b
  tell [Token '_']
  tellGroup (writeExp e1)
writeExp (ESuper b e1) = do
  (if isFancy b then tellGroup else id) $ writeExp b
  tell [Token '^']
  tellGroup (writeExp e1)
writeExp (ESubsup b e1 e2) = do
  (if isFancy b then tellGroup else id) $ writeExp b
  tell [Token '_']
  tellGroup (writeExp e1)
  tell [Token '^']
  tellGroup (writeExp e2)
writeExp (EOver convertible b e1) =
  writeScript convertible b e1 '^' "\\overset"
writeExp (EUnder convertible b e1) =
  writeScript convertible b e1 '_' "\\underset"
writeExp (EUnderover convertible b e1 e2)
  | isOperator b = do
      (if isFancy b then tellGroup else id) $
        (if convertible then local setConvertible else id) $ writeExp b
      unless convertible $ tell [ControlSeq "\\limits"]
      tell [Token '_']
      tellGroup (writeExp e1)
      tell [Token '^']
      tellGroup (writeExp e2)
  | otherwise = writeExp (EUnder convertible (EOver convertible b e2) e1)
writeExp (EUnary s e) = do
    tell [ControlSeq s]
    tellGroup (writeExp e)
writeExp (EPhantom e) = do
    tell [ControlSeq "\\phantom"]
    writeExp e
writeExp (EScaled size e)
  | case e of
         (ESymbol Open _)  -> True
         (ESymbol Close _) -> True
         _ -> False = do
    case S.getScalerCommand size of
         Just s  -> tell [ControlSeq s]
         Nothing -> return ()
    writeExp e
  | otherwise = writeExp e
writeExp (EText ttype s) = do
  let txtcmd = getTextCommand ttype
  case map escapeLaTeX (fromUnicode ttype s) of
       []   -> return ()
       xs   -> tell $ txtcmd (Grouped xs)
writeExp (EStyled ttype es) = do
  txtcmd <- (flip S.getLaTeXTextCommand ttype) <$> asks mathEnv
  tell [ControlSeq txtcmd]
  tellGroup (mapM_ writeExp $ everywhere (mkT (fromUnicode ttype)) es)
writeExp (EArray [AlignRight, AlignLeft] rows) = do
  env <- asks mathEnv
  if "amsmath" `elem` env
     then table "aligned" [] rows
     else table "array" [AlignRight, AlignLeft] rows
writeExp (EArray aligns rows) = do
  env <- asks mathEnv
  if "amsmath" `elem` env && all (== AlignCenter) aligns
     then table "matrix" [] rows
     else table "array" aligns rows

table :: String -> [Alignment] -> [ArrayLine] -> Math ()
table name aligns rows = do
  tell [ControlSeq "\\begin", Grouped [Literal name]]
  unless (null aligns) $
     tell [Grouped [Literal columnAligns]]
  tell [Token '\n']
  mapM_ row rows
  tell [ControlSeq "\\end", Grouped [Literal name]]
  where
    columnAligns = map alignmentToLetter aligns
    alignmentToLetter AlignLeft = 'l'
    alignmentToLetter AlignCenter = 'c'
    alignmentToLetter AlignRight = 'r'
    alignmentToLetter AlignDefault = 'l'

row :: ArrayLine -> Math ()
row []     = tell [Space, Literal "\\\\", Token '\n']
row [c]    = cell c >> row []
row (c:cs) = cell c >> tell [Space, Token '&', Space] >> row cs

cell :: [Exp] -> Math ()
cell = mapM_ writeExp

data FenceType = DLeft | DMiddle | DRight

type Delim = String

writeDelim :: FenceType -> Delim -> Math ()
writeDelim fence delim = do
    tex <- getTeXMathM delim
    valid <- elem tex <$> delimiters
    nullLim <- getTeXMathM "."
    let delimCmd = if valid then tex else nullLim
    tell $ case fence of
             DLeft -> [ControlSeq "\\left"] ++ delimCmd ++ [Space] ++ if valid then [] else tex
             DMiddle -> case valid of
                              True -> [Space] ++ [ControlSeq "\\middle"] ++ tex ++ [Space]
                              False -> tex
             DRight -> [Space, ControlSeq "\\right"] ++ delimCmd ++ if valid then [] else tex

writeScript :: Bool -> Exp -> Exp -> Char -> String -> Math ()
writeScript convertible b e1 sep cmd
  | isOperator b = do
     (if isFancy b then tellGroup else id) $
       (if convertible then local setConvertible else id) $ writeExp b
     unless convertible $ tell [ControlSeq "\\limits"]
     tell [Token sep]
     tellGroup (writeExp e1)
  | otherwise = do
      tell [ControlSeq cmd]
      tellGroup (writeExp e1)
      tellGroup (writeExp b)

-- Utility

-- | Maps a length in em to the nearest bigger LaTeX space command
getSpaceCommand :: Double -> String
getSpaceCommand width = snd $ fromMaybe (M.findMax spaceMap) (lookupGE width spaceMap)
  where
    spaceMap = M.fromList (map (\(k, ESpace s) -> (s, k)) spaceCommands)

lookupGE :: Ord k =>  k -> M.Map k v -> Maybe (k, v)
lookupGE k m = let (_, v, g) = M.splitLookup k m in
                    (fmap ((,) k) (v <|> (fst <$> M.minView g)))

spaceCommands :: [(String, Exp)]
spaceCommands =
           [ ("\\!", ESpace (-0.167))
           , (""   , ESpace 0.0)
           , ("\\,", ESpace 0.167)
           , ("\\>", ESpace 0.222)
           , ("\\:", ESpace 0.222)
           , ("\\;", ESpace 0.278)
           , ("~", ESpace 0.333)
           , ("\\quad", ESpace 1.0)
           , ("\\qquad", ESpace 2.0)]

getTextCommand :: TextType -> TeX -> [TeX]
getTextCommand tt x =
  case tt of
        TextNormal     -> [ControlSeq "\\text", x]
        TextItalic     -> [ControlSeq "\\textit", x]
        TextBold       -> [ControlSeq "\\textbf", x]
        TextMonospace  -> [ControlSeq "\\texttt", x]
        TextBoldItalic -> [ControlSeq "\\textit",
                             Grouped [ControlSeq "\\textbf", x]]
        TextSansSerif  -> [ControlSeq "\\textsf", x]
        TextSansSerifBold -> [ControlSeq "\\textbf",
                               Grouped [ControlSeq "\\textsf", x]]
        TextSansSerifItalic -> [ControlSeq "\\textit",
                                Grouped [ControlSeq "\\textsf", x]]
        TextSansSerifBoldItalic -> [ControlSeq "\\textbf",
                                    Grouped [ControlSeq "\\textit",
                                      Grouped [ControlSeq "\\textsf", x]]]
        _  -> [ControlSeq "\\text", x]

-- Commands which can be used with \left and \right
delimiters :: Math [[TeX]]
delimiters = do
    env <- asks mathEnv
    let commands' = [ ".", "(", ")", "[", "]", "|", "\x2016", "{", "}"
                    , "\x2309", "\x2308", "\x2329", "\x232A"
                    , "\x230B", "\x230A", "\x231C", "\x231D"]
    return $ filter (not . null) (map (flip getTeXMath env) commands')

isFancy :: Exp -> Bool
isFancy (ESub _ _) = True
isFancy (ESuper _ _) = True
isFancy (ESubsup _ _ _) = True
isFancy (EOver _ _ _) = True
isFancy (EUnder _ _ _) = True
isFancy (EUnderover _ _ _ _) = True
isFancy (EUnary _ _) = True
isFancy _ = False


isOperator :: Exp -> Bool
isOperator (EMathOperator _) = True
isOperator (ESymbol Op _)    = True
isOperator _                 = False

-- Fix up

reorderDiacritical' :: Position -> Exp -> Exp -> Exp
reorderDiacritical' p b e@(ESymbol Accent a) =
  case S.getDiacriticalCommand p a of
    Just accentCmd -> EUnary accentCmd b
    Nothing -> case p of
                    Over  -> EOver False b e
                    Under -> EUnder False b e
reorderDiacritical' _ _ _ = error "Must be called with Accent"

reorderDiacritical :: Exp -> Exp
reorderDiacritical (EOver _ b e@(ESymbol Accent _)) =
  reorderDiacritical' Over b e
reorderDiacritical (EUnder _ b e@(ESymbol Accent _)) =
  reorderDiacritical' Under b e
reorderDiacritical (EUnderover _ b e@(ESymbol Accent _) e1) =
  reorderDiacritical' Under (EOver False b e1) e
reorderDiacritical (EUnderover _ b e1 e@(ESymbol Accent _)) =
  reorderDiacritical' Over (EUnder False b e1) e
reorderDiacritical x = x

fixTree :: [Exp] -> [Exp]
fixTree (EGrouped -> es) =
    let removeGroup (EGrouped e) = e
        removeGroup e = [e] in
    removeGroup $ everywhere (mkT reorderDiacritical) es

