{-
Copyright (C) 2014 Gong-Yi Liao <gongyi.liao.uconn@gmail.com>

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

{- | Function for replacing latex commands in the 'unicode-math' TeX
   | package with corresponding unicode characters 
-}

{-# LANGUAGE PackageImports #-}

module Text.TeXMath.UniMath(ltxCmdsToUnis)
where 

import Data.Char 
import qualified Data.Map as M
import "regex-compat-tdfa" Text.Regex 

-- symbol names: latin letters 
ltnltrs = [ [x] | x <- ['A'..'Z'] ++ ['a'..'z']  ]

-- symbol names:  number names 
nums = [ "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]

-- symbol names: greek letter names
grkabn_0 = [ "alpha", "beta", "gamma", "delta",
           "epislon", "zeta", "eta", "theta",
           "iota", "kappa", "lambda", "mu",
           "nu", "xi", "omicron", "pi",
           "rho" ]
grkabn_1 = [ "sigma", "tau", "upsilon",
             "phi", "chi", "psi", "omega" ]

-- symbol names: upper case greek letter names 
grkabnC = [ (++) [toUpper (head x)] (tail x)  | x <- grkabn_0 ] ++ ["varTheta"] ++ [ (++) [toUpper (head x)] (tail x)  | x <- grkabn_1 ] ++ ["Nabla"]

-- symbol names: lower case greek letter names 
grkabnc = grkabn_0 ++ ["varsigma"] ++ grkabn_1 ++ ["partial",  "varepsilon", "vartheta", "varkappa", "varphi", "varrho", "varpi"]

-- Convert Number to Unicode
toUChars :: [Int] -> [String]
toUChars urange = [ [toEnum x] :: String | x <- urange ]

-- Mathematical Bold
mbf_ltn = toUChars [ 119808 .. 119859 ]
mbf_grk = toUChars ([ 120488 .. 120513 ] ++ [ 120778 ] ++ [ 120514 .. 120545 ] ++ [ 120779 ])
mbf_num = toUChars [ 120782 .. 120791 ] 
mbf_ltr_nms = [ (++) "mbf" x | x <- ltnltrs ++ grkabnC ++ ["Digamma"] ++ grkabnc ++ ["digamma"] ++ nums ] 
mbf_glyphs = mbf_ltn ++ mbf_grk ++ mbf_num 
mbf_sym_map = zip mbf_ltr_nms mbf_glyphs 

-- Mathematical Italic 
mit_ltn = toUChars ( [ 119860 .. 119892 ] ++ [8462] ++ [ 119894 .. 119911] ++ [ 120484, 120485 ])
mit_grk = toUChars [ 120546 .. 120603 ]
mit_ltr_nms = [ (++) "mit" x | x <- ltnltrs ] ++ ["imath", "jmath"] ++ [ (++) "\\mbit" x | x <- grkabnC ++ grkabnc ] 
mit_glyphs = mit_ltn ++ mit_grk
mit_sym_map = zip mit_ltr_nms mit_glyphs 

-- Mathematical Bold Italic
mbfit_ltn = toUChars [ 119912 .. 119963 ]
mbfit_grk = toUChars [ 120604 .. 120661 ]
mbfit_ltr_nms = [ (++) "mbfit" x | x <- ltnltrs ++ grkabnC ++ grkabnc ] 
mbfit_glyphs = mbfit_ltn ++ mbfit_grk
mbfit_sym_map = zip mbfit_ltr_nms mbfit_glyphs

-- Basic Mathematical typefaces
mbasic_sym_map = mbf_sym_map ++ mit_sym_map ++ mbfit_sym_map 

-- Mathematical Script 
mscr_ltn_C = toUChars ([ 119964, 8492 ] ++ [ 119966 .. 119967 ] ++ [ 8496, 8497, 119970, 8459, 8464, 119973, 119974, 8466, 8499 ] ++ [ 119977 .. 119980 ] ++ [ 8475 ] ++ [ 119982 .. 119989 ])
mscr_ltn_c = toUChars ([ 119990 .. 119995 ] ++ [ 8458 ] ++ [ 119997 .. 120003 ] ++ [ 8500 ] ++ [ 120005 .. 120015 ])
mscr_ltr_nms = [ (++) "mscr" x | x <- ltnltrs ] 
mscr_glyphs = mscr_ltn_C ++ mscr_ltn_c
mscr_sym_map = zip mscr_ltr_nms mscr_glyphs 

-- Mathematical Script Bold
mbfscr_ltn = toUChars [ 120016 .. 120067 ]
mbfscr_ltr_nms = [ (++) "mbfscr" x | x <- ltnltrs ] 
mbfscr_sym_map = zip mbfscr_ltr_nms mbfscr_ltn 

-- Mathematical Fraktur 
mfrak_ltn = toUChars ( [ 120068, 120069, 8493 ] ++ [ 120071 .. 120074 ] ++ [ 8460, 8465 ] ++ [ 120077 .. 120084 ] ++ [ 8476 ] ++ [ 120086 .. 120092 ] ++ [ 8488 ] ++ [ 120094 .. 120119 ] )
mfrak_ltr_nms = [ (++) "\\mfrak" x | x <- ltnltrs ] 
mfrak_sym_map = zip mfrak_ltr_nms mfrak_ltn  

-- Blackboard bold 
bbb_ltn = toUChars ( [ 120120 .. 120126 ] ++ [ 8461 ]  ++ [ 120128 .. 120132 ] ++ [ 8469, 120134, 8473, 8474, 8477 ] ++ [ 120138 .. 120144 ] ++ [ 8484 ] ++ [ 120146 .. 120171 ])
bbb_num  = toUChars [ 120792  .. 120801 ]
bbb_ltr_nms = [ (++) "Bbb" x | x <- ltnltrs ++ nums ] 
bbb_glyphs = bbb_ltn ++ bbb_num 
bbb_sym_map = zip bbb_ltr_nms bbb_glyphs 

-- Mathematical Scipt Fraktur

mbffrak_ltn = toUChars [ 120172 .. 120223 ]
mbffrak_ltr_nms = [ (++) "mbffrak" x | x <- ltnltrs ] 
mbffrak_sym_map = zip mbffrak_ltr_nms mbffrak_ltn 

-- Fraktur and Blackbold Symbols
mscr_mfrak_bbb_sym_map = mscr_sym_map ++ mbfscr_sym_map ++ mfrak_sym_map  ++ bbb_sym_map ++ mbfrak_sym_map

-- Mathematical Sans Serif
msans_ltn = toUChars [ 120224 .. 120275 ]
msans_num = toUChars [ 120802 .. 120811 ]
msans_ltr_nms = [ (++) "msans" x | x <- ltnltrs ++ nums ] 
msans_glyphs = msans_ltn  ++ msans_num 
msans_sym_map = zip msans_ltr_nms msans_glyphs 

-- Mathematical Sans Serif Bold
mbfsans_ltn = toUChars [ 120276 .. 120327 ]
mbfsans_grk = toUChars [ 120662 .. 120719 ]
mbfsans_num = toUChars [ 120812 .. 120821 ]
mbfsans_ltr_nms = [ (++) "mbfsans" x | x <- ltnltrs ++ grkabnC ++ grkabnc ++ nums ] 
mbfsans_glyphs = mbfsans_ltn ++ mbfsans_grk ++ mbfsans_num
mbfsans_sym_map = zip mbfsans_ltr_nms mbfsans_glyphs 

-- Mathematical Sans Serif Italic
mitsans_ltn = toUChars [ 120328 .. 120379 ]
mitsans_ltr_nms = [ (++) "mitsans" x | x <- ltnltrs ] 
mitsans_sym_map = zip mitsans_ltr_nms mitsans_ltn 

-- Mathematical Sans Serif Bold Italic
mbfitsans_ltn = toUChars [ 120380 .. 120431 ]
mbfitsans_grk = toUChars [ 120720 .. 120777 ]
mbfitsans_ltr_nms = [ (++) "mbfitsans" x | x <- ltnltrs ++ grkabnC ++ grkabnc ] 
mbfitsans_glyphs = mbfitsans_ltn ++ mbfitsans_grk 
mbfitsans_sym_map = zip mbfitsans_ltr_nms mbfitsans_glyphs 

-- Mathematical Monospace
mtt_ltn = toUChars [ 120432 .. 120483 ]
mtt_num = toUChars [ 120822 .. 120831 ]
mtt_ltr_nms = [ (++) "mtt" x | x <- ltnltrs ++ nums ] 
mtt_glyphs = mtt_ltn ++ mtt_num 
mtt_sym_map = zip mtt_ltr_nms mtt_glyphs 

-- Mathematical Sans Serif and monospace symbols
msans_mtt_sym_map = msans_sym_map ++ mbfsans_sym_map ++ mitsans_sym_map ++ mbfitsans_sym_map ++ mtt_sym_map 

-- Overall mapping
math_abnm_map = mbasic_sym_map ++ mscr_mfrak_bbb_sym_map ++ msans_mtt_sym_map
masym_map :: M.Map String String
masym_map = M.fromList math_abnm_map


-- Regex replace: regex pattern string 
mkPtnStr :: String -> String
mkPtnStr c = "(\\\\" ++ c ++ ")([^[:alnum:]]|\\b)"

-- Regex replace: produce the regex object 
mkRgxStr :: String -> Regex
mkRgxStr c = (mkRegex (mkPtnStr c))

-- Regex replace: replace with corresponding unicode glyph 
mapLtx :: String -> String
mapLtx c = (maybe c id (M.lookup c masym_map)) ++ "\\2"

-- Regex replace: regex subsitute 
ltxCmdToUni :: String -> String -> String
ltxCmdToUni s c = subRegex (mkRgxStr c) s (mapLtx c)

-- Regex replace: replace all the latex commands into corresponding unicode glyphs 
ltxCmdsToUnis :: String -> String
ltxCmdsToUnis s = foldl ltxCmdToUni s [ fst x | x <- math_abnm_map ]

