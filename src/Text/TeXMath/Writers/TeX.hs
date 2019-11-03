{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns, GADTs, OverloadedStrings #-}
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
-- import Text.TeXMath.Unicode.ToTeX (getTeXMath) TODO text: restore
import qualified Text.TeXMath.Unicode.ToTeX as TT
import Text.TeXMath.Unicode.ToUnicode (fromUnicode)
import qualified Text.TeXMath.Shared as S
import qualified Data.Text as T
import Data.Generics (everywhere, mkT)
import Control.Applicative ((<$>), Applicative)
import Control.Monad (when, unless, foldM_)
import Control.Monad.Reader (MonadReader, runReader, Reader, asks, local)
import Control.Monad.Writer( MonadWriter, WriterT,
                             execWriterT, tell, censor)
import Text.TeXMath.TeX

-- import Debug.Trace
-- tr' x = trace (show x) x

-- TODO text: remove
getTeXMath :: String -> Env -> [TeX]
getTeXMath = TT.getTeXMath . T.pack
--

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
writeTeXWith env es = drop 1 . init . T.unpack . flip renderTeX "" . Grouped $
                            runExpr env $
                              mapM_ writeExp (removeOuterGroup es)

runExpr :: Env -> Math () -> [TeX]
runExpr e m = flip runReader (MathState e False) $ execWriterT (runTeXMath m)

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

tellGenFrac :: String -> String -> Math ()
tellGenFrac open close =
  tell [ ControlSeq "\\genfrac"
       , Grouped [Literal $ T.pack open]
       , Grouped [Literal $ T.pack close]
       , Grouped [Literal "0pt"]
       , Grouped [] ]

writeBinom :: String -> Exp -> Exp -> Math ()
writeBinom cmd x y = do
  env <- asks mathEnv
  if "amsmath" `elem` env
     then do
       case cmd of
           "\\choose" -> tell [ControlSeq "\\binom"]
           "\\brack"  -> tellGenFrac "[" "]"
           "\\brace"  -> tellGenFrac "\\{" "\\}"
           "\\bangle" -> tellGenFrac "\\langle" "\\rangle"
           _          -> error "writeBinom: unknown cmd"
       tellGroup $ writeExp x
       tellGroup $ writeExp y
     else tellGroup $ do
       writeExp x
       tell [ControlSeq $ T.pack cmd]
       writeExp y

writeExp :: Exp -> Math ()
writeExp (ENumber s) = tell =<< getTeXMathM (T.unpack s)
writeExp (EGrouped es) = tellGroup (mapM_ writeExp es)
writeExp (EDelimited "(" ")" [Right (EFraction NoLineFrac x y)]) =
  writeBinom "\\choose" x y
writeExp (EDelimited "[" "]" [Right (EFraction NoLineFrac x y)]) = do
  writeBinom "\\brack" x y
writeExp (EDelimited "{" "}" [Right (EFraction NoLineFrac x y)]) = do
  writeBinom "\\brace" x y
writeExp (EDelimited "\x27E8" "\x27E9" [Right (EFraction NoLineFrac x y)]) = do
  writeBinom "\\bangle" x y
writeExp (EDelimited open close [Right (EFraction NoLineFrac x y)]) = do
  writeExp (EDelimited open close [Right (EArray [AlignCenter]
                   [[[x]],[[y]]])])
writeExp (EDelimited open close [Right (EArray aligns rows)]) = do
  env <- asks mathEnv
  case ("amsmath" `elem` env, open, close) of
       (True, "{", "") | aligns == [AlignLeft, AlignLeft] ->
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
         writeDelim DLeft (T.unpack open)
         writeExp (EArray aligns rows)
         writeDelim DRight (T.unpack close)
writeExp (EDelimited open close es) =  do
  writeDelim DLeft (T.unpack open)
  mapM_ (either (writeDelim DMiddle . T.unpack) writeExp) es
  writeDelim DRight (T.unpack close)
writeExp (EIdentifier (T.unpack -> s)) = do
  math <- getTeXMathM s
  case math of
       []      -> return ()
       [t]     -> tell [t]
       ts      -> tell [Grouped ts]
writeExp o@(EMathOperator (T.unpack -> s)) = do
  math <- getTeXMathM s
  case S.getOperator o of
       Just op  -> tell [op]
       Nothing  -> do
         tell [ControlSeq "\\operatorname"]
         -- use \operatorname* if convertible
         asks mathConvertible >>= flip when (tell [Token '*'])
         tell [Grouped math]
writeExp (ESymbol Ord (T.unpack -> [c]))  -- do not render "invisible operators"
  | c `elem` ['\x2061'..'\x2064'] = return () -- see 3.2.5.5 of mathml spec
writeExp (ESymbol t (T.unpack -> s)) = do
  s' <- getTeXMathM s
  when (t == Bin || t == Rel) $ tell [Space]
  if length s > 1 && (t == Bin || t == Rel || t == Op)
     then tell [ControlSeq ("\\math" <> T.toLower (T.pack $ show t)),
                 Grouped [ControlSeq "\\text", Grouped s']]
     else tell s'
  when (t == Bin || t == Rel) $ tell [Space]
writeExp (ESpace width) = do
  env <- asks mathEnv
  tell [ControlSeq $ T.pack $ getSpaceCommand ("amsmath" `elem` env) width]
writeExp (EFraction fractype e1 e2) = do
  let cmd = case fractype of
                 NormalFrac  -> "\\frac"
                 DisplayFrac -> "\\dfrac"
                 InlineFrac  -> "\\tfrac"
                 NoLineFrac  -> "\\binom"  -- shouldn't happen because
                         -- a binom will be in a delimited
  tell [ControlSeq cmd]
  tellGroup (writeExp e1)
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
  writeScript Over convertible b e1
writeExp (EUnder convertible b e1) =
  writeScript Under convertible b e1
writeExp (EUnderover convertible b e1@(ESymbol Accent _) e2) =
 writeExp (EUnder convertible (EOver False b e2) e1)
writeExp (EUnderover convertible b e1 e2@(ESymbol Accent _)) =
 writeExp (EOver convertible (EUnder False b e1) e2)
writeExp (EUnderover convertible b e1 e2)
  | isOperator b = do
      (if isFancy b then tellGroup else id) $
        (if convertible then local setConvertible else id) $ writeExp b
      unless convertible $ tell [ControlSeq "\\limits"]
      tell [Token '_']
      tellGroup (checkSubstack e1)
      tell [Token '^']
      tellGroup (checkSubstack e2)
  | otherwise = writeExp (EUnder convertible (EOver convertible b e2) e1)
writeExp (ESqrt e) = do
    tell [ControlSeq "\\sqrt"]
    tellGroup (writeExp e)
writeExp (ERoot i e) = do
    tell [ControlSeq "\\sqrt"]
    tell [Token '[']
    writeExp i
    tell [Token ']']
    tellGroup (writeExp e)
writeExp (EPhantom e) = do
    tell [ControlSeq "\\phantom"]
    tellGroup (writeExp e)
writeExp (EBoxed e) = do
    env <- asks mathEnv
    if "amsmath" `elem` env
      then do
        tell [ControlSeq "\\boxed"]
        tellGroup (writeExp e)
      else writeExp e
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
writeExp (EText ttype (T.unpack -> s)) = do
  let txtcmd = getTextCommand ttype
  case map escapeLaTeX s of
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
  tell [ControlSeq "\\begin", Grouped [Literal $ T.pack name]]
  unless (null aligns) $
     tell [Grouped [Literal columnAligns]]
  tell [Token '\n']
  mapM_ row rows
  tell [ControlSeq "\\end", Grouped [Literal $ T.pack name]]
  where
    columnAligns = T.pack $ map alignmentToLetter aligns
    alignmentToLetter AlignLeft = 'l'
    alignmentToLetter AlignCenter = 'c'
    alignmentToLetter AlignRight = 'r'

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

writeScript :: Position -> Bool -> Exp -> Exp -> Math ()
writeScript pos convertible b e1 = do
  let diacmd = case e1 of
                    ESymbol stype a
                      | stype `elem` [Accent, TOver, TUnder]
                      -> S.getDiacriticalCommand pos a
                    _ -> Nothing
  case diacmd of
       Just cmd -> do
            tell [ControlSeq cmd]
            tellGroup (writeExp b)
       Nothing
         | isOperator b -> do
            (if isFancy b then tellGroup else id) $
              (if convertible then local setConvertible else id) $ writeExp b
            unless convertible $ tell [ControlSeq "\\limits"]
            tell [Token $ case pos of { Over -> '^'; Under -> '_' }]
            tellGroup (checkSubstack e1)
         | otherwise -> do
             case pos of
                  Over   -> tell [ControlSeq "\\overset"]
                  Under  -> tell [ControlSeq "\\underset"]
             tellGroup (writeExp e1)
             tellGroup (writeExp b)

-- Replace an array with a substack if appropriate.
checkSubstack :: Exp -> Math ()
checkSubstack e@(EArray [AlignCenter] rows) = do
  env <- asks mathEnv
  if "amsmath" `elem` env
     then do
       tell [ControlSeq "\\substack"]
       tellGroup $ foldM_ (\first r -> do
          if first
             then return ()
             else tell [Space, Literal "\\\\", Space]
          mapM_ (mapM_ writeExp . removeOuterGroup) r
          return False) True rows
     else writeExp e
checkSubstack e = writeExp e

-- Utility

-- | Maps a length in em to the nearest LaTeX space command
getSpaceCommand :: Bool -> Rational -> String
getSpaceCommand amsmath width =
  case floor (width * 18) :: Int of
          -3       -> "\\!"
          0        -> ""
          3        -> "\\,"
          4        -> "\\ "  -- could also use \: or \>
          5        -> "\\;"
          18       -> "\\quad"
          36       -> "\\qquad"
          n        -> if amsmath
                         then "\\mspace{" ++ show n ++ "mu}"
                         else "{\\mskip " ++ show n ++ "mu}"

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
isFancy (ESqrt _) = True
isFancy (ERoot _ _) = True
isFancy (EPhantom _) = True
isFancy _ = False


isOperator :: Exp -> Bool
isOperator (EMathOperator _) = True
isOperator (ESymbol Op _)    = True
isOperator _                 = False

removeOuterGroup :: [Exp] -> [Exp]
removeOuterGroup [EGrouped es] = es
removeOuterGroup es = es
