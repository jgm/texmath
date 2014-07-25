{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Text.TeXMath.Writers.TeXMath (writeTeXMath, writeTeXMathWith, addLaTeXEnvironment ) where

import Text.TeXMath.Types
import Text.TeXMath.Unicode.ToTeXMath (getTeXMath)
import qualified Text.TeXMath.Shared as S
import Data.Maybe (fromMaybe)
import Data.Generics (everywhere, mkT)
import Control.Applicative ((<$>), (<|>), Applicative)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, runReader, Reader, asks)
import Control.Monad.Writer(MonadWriter, WriterT, execWriterT, tell, censor)
import Text.TeXMath.TeX

-- import Debug.Trace
-- tr' x = trace (show x) x

-- | Transforms an expression tree to equivalent LaTeX with the default
-- packages (amsmath and amssymb)
writeTeXMath :: [Exp] -> String
writeTeXMath = writeTeXMathWith defaultEnv

-- | Adds the correct LaTeX environment around a TeXMath fragment
addLaTeXEnvironment :: DisplayType -> String -> String
addLaTeXEnvironment dt math =
    case dt of
      DisplayInline -> "\\(" ++ math ++ "\\)"
      DisplayBlock  -> "\\[" ++ math ++ "\\]"

-- |  Transforms an expression tree to equivalent LaTeX with the specified
-- packages
writeTeXMathWith :: Env -> [Exp] -> String
writeTeXMathWith env e = drop 1 . init . flip renderTeX "" . Grouped $
                            runExpr env $
                              mapM_ (writeExp . (fixTree env)) e

runExpr :: Env -> Math () -> [TeX]
runExpr e m = flip runReader e $ execWriterT (runTeXMath m)

square :: [String]
square = ["\\sqrt", "\\surd"]

newtype Math a = Math {runTeXMath :: WriterT [TeX] (Reader Env) a}
                  deriving (Functor, Applicative, Monad, MonadReader Env
                           , MonadWriter [TeX])

getTeXMathM :: String -> Math [TeX]
getTeXMathM s = asks (getTeXMath s)

tellGroup :: Math () -> Math ()
tellGroup = censor ((:[]) . Grouped)

writeExp :: Exp -> Math ()
writeExp (ENumber s) = tell =<< getTeXMathM s
writeExp (EGrouped es) = tellGroup (mapM_ writeExp es)
writeExp (EDelimited open close es) =  do
  tell [ControlSeq "\\left" ]
  tell =<< getTeXMathM open
  tell [Space]
  mapM_ writeExp es
  tell [Space, ControlSeq "\\right"]
  tell =<< getTeXMathM close
writeExp (EIdentifier s) = do
  math <- getTeXMathM s
  case math of
       [Token c]       -> tell [Token c] -- don't brace single token identifiers
       [Literal [c]]   -> tell [Literal [c]]
       [ControlSeq cs] -> tell [ControlSeq cs]
       cs              -> tell [ControlSeq "\\operatorname", Grouped cs]
writeExp o@(EMathOperator s) = do
  math <- getTeXMathM s
  case getOperator o of
       Just op   -> tell [op]
       Nothing  -> case math of
                        []  -> return ()
                        xs@(ControlSeq _:_) -> tell xs
                        xs -> tell [ControlSeq "\\operatorname", Grouped xs]
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
writeExp (EDown b e1) = do
  (if isFancy b then tellGroup else id) $ writeExp b
  tell [ControlSeq "\\limits", Token '_']
  tellGroup (writeExp e1)
writeExp (EUp b e1) = do
  (if isFancy b then tellGroup else id) $ writeExp b
  tell [ControlSeq "\\limits", Token '^']
  tellGroup (writeExp e1)
writeExp (EDownup b e1 e2) = do
  (if isFancy b then tellGroup else id) $ writeExp b
  tell [ControlSeq "\\limits", Token '_']
  tellGroup (writeExp e1)
  tell [Token '^']
  tellGroup (writeExp e2)
writeExp (EOver b e1) =
  case b of
    (EMathOperator _) -> writeExp (EUp b e1)
    _ -> do
          tell [ControlSeq "\\overset"]
          tellGroup (writeExp e1)
          tellGroup (writeExp b)
writeExp (EUnder b e1) =
  case b of
    (EMathOperator _) -> writeExp (EDown b e1)
    _ -> do
          tell [ControlSeq "\\underset"]
          tellGroup (writeExp e1)
          tellGroup (writeExp b)
writeExp (EUnderover b e1 e2) =
  case b of
    (EMathOperator _) -> writeExp (EDownup b e1 e2)
    _ -> writeExp (EUnder (EOver b e2) e1)
writeExp (EUnary s e) = do
    tell [ControlSeq s]
    tellGroup (writeExp e)
writeExp (EScaled size e) = do
  case S.getScalerCommand size of
       Just s  -> tell [ControlSeq s]
       Nothing -> return ()
  tellGroup (writeExp e)
writeExp (EStretchy (ESymbol Open e)) = do
  math <- getTeXMathM e
  case math of
       [] -> return ()
       e' -> tell [ControlSeq "\\left"] >> tell e' >> tell [Space]
writeExp (EStretchy (ESymbol Close e)) = do
  math <- getTeXMathM e
  case math of
       [] -> return ()
       e' -> tell [Space, ControlSeq "\\right"] >> tell e'
writeExp (EStretchy e) = writeExp e
writeExp (EText ttype s) = do
  txtcmd <- asks (flip S.getLaTeXTextCommand ttype)
  tell [ControlSeq txtcmd, Grouped (map escapeLaTeX s)]
writeExp (EArray aligns rows) = table aligns rows

table :: [Alignment] -> [ArrayLine] -> Math ()
table as rows = do
  tell [ControlSeq "\\begin", Grouped [Literal "array"],
         Grouped [Literal columnAligns], Token '\n']
  mapM_ row rows
  tell [ControlSeq "\\end", Grouped [Literal "array"]]
  where
    columnAligns = map alignmentToLetter as
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


-- Utility

-- | Maps a length in em to the nearest bigger LaTeX space command
getSpaceCommand :: String -> String
getSpaceCommand width = snd $ fromMaybe (M.findMax spaceMap) (lookupGE (readSpace width) spaceMap)
  where
    spaceMap = M.fromList (map (\(k, ESpace s) -> (readSpace s, k)) spaceCommands)
    readSpace :: String -> Float
    readSpace s = maybe 0 fst $ listToMaybe $ reads s

lookupGE :: Ord k =>  k -> M.Map k v -> Maybe (k, v)
lookupGE k m = let (_, v, g) = M.splitLookup k m in
                    (fmap ((,) k) (v <|> (fst <$> M.minView g)))

spaceCommands :: [(String, Exp)]
spaceCommands =
           [ ("\\!", ESpace "-0.167em")
           , (""   , ESpace "0.0em")
           , ("\\,", ESpace "0.167em")
           , ("\\>", ESpace "0.222em")
           , ("\\:", ESpace "0.222em")
           , ("\\;", ESpace "0.278em")
           , ("~", ESpace "0.333em")
           , ("\\quad", ESpace "1.0em")
           , ("\\qquad", ESpace "2.0em")]

-- Fix up

removeAccentStretch :: Exp -> Exp
removeAccentStretch (EStretchy e@(ESymbol Accent _)) = e
removeAccentStretch x = x

reorderDiacritical' :: Position -> Exp -> Exp -> Exp
reorderDiacritical' p b e@(ESymbol Accent a) =
  case S.getDiacriticalCommand p a of
    Just accentCmd -> EUnary accentCmd b
    Nothing -> case p of
                    Over  -> EOver b e
                    Under -> EUnder b e
reorderDiacritical' _ _ _ = error "Must be called with Accent"

reorderDiacritical :: Exp -> Exp
reorderDiacritical (EOver b e@(ESymbol Accent _)) =
  reorderDiacritical' Over b e
reorderDiacritical (EUnder b e@(ESymbol Accent _)) =
  reorderDiacritical' Under b e
reorderDiacritical (EUnderover b e@(ESymbol Accent _) e1) =
  reorderDiacritical' Under (EOver b e1) e
reorderDiacritical (EUnderover b e1 e@(ESymbol Accent _)) =
  reorderDiacritical' Over (EUnder b e1) e
reorderDiacritical x = x

matchStretch' :: Env -> [Exp] -> Int
matchStretch'  _ [] = 0
matchStretch' e ((EStretchy (ESymbol Open s)): xs) =
  (if null (getTeXMath s e) then 0 else 1) + matchStretch' e xs
matchStretch' e ((EStretchy (ESymbol Close s)): xs) =
  (if null (getTeXMath s e) then 0 else (-1)) + matchStretch' e xs
matchStretch' e (_:xs) = matchStretch' e xs

-- Ensure that the lefts match the rights.
matchStretch :: Env -> [Exp] -> [Exp]
matchStretch e es
  | n < 0 = (replicate (0 - n) $ EStretchy (ESymbol Open ".")) ++ es
  | n > 0 = es ++ (replicate n $ EStretchy (ESymbol Close "."))
  | otherwise = es
  where
    n = matchStretch' e es

ms :: Env -> Exp -> Exp
ms e (EGrouped xs) = EGrouped (matchStretch e xs)
ms e (EDelimited o c xs) = EDelimited o c (matchStretch e xs)
ms e (EArray as rs) = EArray as (map (map (matchStretch e)) rs)
ms _ x = x

fixTree :: Env -> Exp -> Exp
fixTree e = everywhere
            ( mkT (ms e)
            . mkT reorderDiacritical
            . mkT removeAccentStretch )

-- Operator Table

getOperator :: Exp -> Maybe TeX
getOperator op = fmap ControlSeq $ lookup op operators

operators :: [(Exp, String)]
operators =
           [ (EMathOperator "arccos", "\\arccos")
           , (EMathOperator "arcsin", "\\arcsin")
           , (EMathOperator "arctan", "\\arctan")
           , (EMathOperator "arg", "\\arg")
           , (EMathOperator "cos", "\\cos")
           , (EMathOperator "cosh", "\\cosh")
           , (EMathOperator "cot", "\\cot")
           , (EMathOperator "coth", "\\coth")
           , (EMathOperator "csc", "\\csc")
           , (EMathOperator "deg", "\\deg")
           , (EMathOperator "det", "\\det")
           , (EMathOperator "dim", "\\dim")
           , (EMathOperator "exp", "\\exp")
           , (EMathOperator "gcd", "\\gcd")
           , (EMathOperator "hom", "\\hom")
           , (EMathOperator "inf", "\\inf")
           , (EMathOperator "ker", "\\ker")
           , (EMathOperator "lg", "\\lg")
           , (EMathOperator "lim", "\\lim")
           , (EMathOperator "liminf", "\\liminf")
           , (EMathOperator "limsup", "\\limsup")
           , (EMathOperator "ln", "\\ln")
           , (EMathOperator "log", "\\log")
           , (EMathOperator "max", "\\max")
           , (EMathOperator "min", "\\min")
           , (EMathOperator "Pr", "\\Pr")
           , (EMathOperator "sec", "\\sec")
           , (EMathOperator "sin", "\\sin")
           , (EMathOperator "sinh", "\\sinh")
           , (EMathOperator "sup", "\\sup")
           , (EMathOperator "tan", "\\tan")
           , (EMathOperator "tanh", "\\tanh") ]

isFancy :: Exp -> Bool
isFancy (ESub _ _) = True
isFancy (ESuper _ _) = True
isFancy (ESubsup _ _ _) = True
isFancy (EOver _ _) = True
isFancy (EUnder _ _) = True
isFancy (EUnderover _ _ _) = True
isFancy (EUp _ _) = True
isFancy (EDown _ _) = True
isFancy (EDownup _ _ _) = True
isFancy (EUnary _ _) = True
isFancy _ = False

