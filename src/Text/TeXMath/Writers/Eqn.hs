{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns, GADTs, OverloadedStrings #-}
{-
Copyright (C) 2016-2023 John MacFarlane <jgm@berkeley.edu>

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

module Text.TeXMath.Writers.Eqn (writeEqn) where

import Data.List (transpose)
import Data.Char (isAscii, ord)
import qualified Data.Text as T
import Text.Printf (printf)
import Text.TeXMath.Types
import qualified Text.TeXMath.Shared as S
import Data.Generics (everywhere, mkT)
import Data.Ratio ((%))
import Data.Text (Text)

-- import Debug.Trace
-- tr' x = trace (show x) x

-- | Transforms an expression tree to equivalent Eqn
writeEqn :: DisplayType -> [Exp] -> T.Text
writeEqn dt exprs =
  T.unwords $ map writeExp $ everywhere (mkT $ S.handleDownup dt) exprs

-- like writeExp but inserts {} if contents contain a space
writeExp' :: Exp -> T.Text
writeExp' e@(EGrouped _) = writeExp e
writeExp' e = if T.any (== ' ') s
                 then asgroup s
                 else s
               where s = writeExp e

writeExps :: [Exp] -> T.Text
writeExps = T.intercalate " " . map writeExp

asgroup :: Text -> Text
asgroup "" = "{\"\"}"  -- see #198
asgroup t = "{" <> t <> "}"

writeExp :: Exp -> T.Text
writeExp (ENumber s) = s
writeExp (EGrouped es) = asgroup $ writeExps es
writeExp (EDelimited open close es) =
  "left " <> mbQuote open <> " " <> T.intercalate " " (map fromDelimited es) <>
  " right " <> mbQuote close
  where fromDelimited (Left e)  = "\"" <> e <> "\""
        fromDelimited (Right e) = writeExp e
        mbQuote "" = "\"\""
        mbQuote s  = s
writeExp (EMathOperator s) =
  if s `elem` ["sin", "cos", "tan", "sinh", "cosh",
               "tanh", "arc", "max", "min", "lim",
               "log", "ln", "exp"]
     then s
     else "\"" <> s <> "\""
writeExp (ESymbol Ord (T.unpack -> [c]))  -- do not render "invisible operators"
  | c `elem` ['\x2061'..'\x2064'] = "" -- see 3.2.5.5 of mathml spec
writeExp (EIdentifier s) = writeExp (ESymbol Ord s)
writeExp (ESymbol t s) =
  case s of
    "{"     -> "\\[lC]"
    "}"     -> "\\[rC]"
    "\8722" -> "-"  -- minus sign, see #200
    "\8943" -> "cdots" -- centered ellipses, see #200
    "\8805" -> ">="
    "\8804" -> "<="
    "\8801" -> "=="
    "\8800" -> "!="
    "\177"  -> "+-"
    "\8594" -> "->"
    "\8592" -> "<-"
    "\8810" -> "<<"
    "\8811" -> ">>"
    "\8734" -> "inf"
    "\8706" -> "partial"
    "\189"  -> "half"
    "\8242" -> "prime"
    "\8776" -> "approx"
    "\183"  -> "cdot"
    "\215"  -> "times"
    "\8711" -> "grad"
    "\8230" -> "..."
    "\8721" -> "sum"
    "\8747" -> "int"
    "\8719" -> "prod"
    "\8898" -> "union"
    "\8899" -> "inter"
    "\945" -> "alpha"
    "\946" -> "beta"
    "\967" -> "chi"
    "\948" -> "delta"
    "\916" -> "DELTA"
    "\1013" -> "epsilon"
    "\951" -> "eta"
    "\947" -> "gamma"
    "\915" -> "GAMMA"
    "\953" -> "iota"
    "\954" -> "kappa"
    "\955" -> "lambda"
    "\923" -> "LAMBDA"
    "\956" -> "mu"
    "\957" -> "nu"
    "\969" -> "omega"
    "\937" -> "OMEGA"
    "\981" -> "phi"
    "\966" -> "varphi"
    "\934" -> "PHI"
    "\960" -> "pi"
    "\928" -> "PI"
    "\968" -> "psi"
    "\936" -> "PSI"
    "\961" -> "rho"
    "\963" -> "sigma"
    "\931" -> "SIGMA"
    "\964" -> "tau"
    "\952" -> "theta"
    "\920" -> "THETA"
    "\965" -> "upsilon"
    "\933" -> "UPSILON"
    "\958" -> "xi"
    "\926" -> "XI"
    "\950" -> "zeta"
    _      -> let s' = if T.all isAscii s
                          then s
                          else "\\[" <> T.unwords (map toUchar $ T.unpack s) <> "]"
                  toUchar c = T.pack $ printf "u%04X" (ord c)
              in  if T.length s > 1 && (t == Rel || t == Bin || t == Op)
                     then "roman{\"" <>
                          (if t == Rel || t == Bin
                              then " "
                              else "") <>
                          s' <>
                          (if t == Rel || t == Bin || t == Op
                              then " "
                              else "") <>
                          "\"}"
                     else s'

writeExp (ESpace d) =
  case d of
      _ | d > 0 && d < (2 % 9) -> "^"
        | d >= (2 % 9) && d < (3 % 9) -> "~"
        | d < 0     -> "back " <> tshow (floor (-1 * d * 100) :: Int)
        | otherwise -> "fwd " <> tshow (floor (d * 100) :: Int)
writeExp (EFraction fractype e1 e2) = writeExp' e1 <> op <> writeExp' e2
  where op = if fractype == NoLineFrac
                then " / "
                else " over "
writeExp (ESub b e1) = writeExp' b <> " sub " <> writeExp' e1
writeExp (ESuper b e1) = writeExp' b <> " sup " <> writeExp' e1
writeExp (ESubsup b e1 e2) =
  writeExp' b <> " sub " <> writeExp' e1 <> " sup " <> writeExp' e2
writeExp (EOver _convertible b e1) =
  writeExp' b <> " to " <> writeExp' e1
writeExp (EUnder _convertible b e1) =
  writeExp' b <> " from " <> writeExp' e1
writeExp (EUnderover convertible b e1@(ESymbol Accent _) e2) =
  writeExp (EUnder convertible (EOver False b e2) e1)
writeExp (EUnderover convertible b e1 e2@(ESymbol Accent _)) =
  writeExp (EOver convertible (EUnder False b e1) e2)
writeExp (EUnderover _convertible b e1 e2) =
  writeExp' b <> " from " <> writeExp' e1 <> " to " <> writeExp' e2
writeExp (ESqrt e) = "sqrt " <> writeExp' e
writeExp (ERoot i e) = "\"\" sup " <> writeExp' i <> " sqrt " <> writeExp' e
writeExp (EPhantom e) = "hphantom " <> writeExp' e
writeExp (EBoxed e) = writeExp e -- TODO: any way to do this?
writeExp (EScaled _size e) = writeExp e -- TODO: any way?
writeExp (EText ttype s) =
  let quoted = "\"" <> s <> "\""
  in case ttype of
       TextNormal -> "roman " <> quoted
       TextItalic -> quoted
       TextBold   -> "bold " <> quoted
       TextBoldItalic -> "bold italic " <> quoted
       _   -> quoted
writeExp (EStyled ttype es) =
  let contents = asgroup $ writeExps es
  in case ttype of
       TextNormal -> "roman " <> contents
       TextItalic -> "italic " <> contents
       TextBold   -> "bold " <> contents
       TextBoldItalic -> "bold italic " <> contents
       _   -> contents
writeExp (EArray aligns rows) =
  "matrix{\n" <> T.concat cols <> "}"
  where cols = zipWith tocol aligns (transpose rows)
        tocol al cs =
          (case al of
               AlignLeft -> "lcol"
               AlignCenter -> "ccol"
               AlignRight -> "rcol") <>
            "{ " <> T.intercalate " above " (map tocell cs) <> " }\n"
        tocell [e] = writeExp' e
        tocell es  = writeExp (EGrouped es)

tshow :: Show a => a -> T.Text
tshow = T.pack . show
