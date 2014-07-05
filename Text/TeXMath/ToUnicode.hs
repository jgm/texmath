{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>

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

{- | Function for replacing strings of characters with their respective mathvariant
-}

module Text.TeXMath.ToUnicode (fromUnicode, toUnicode)
where

import Text.TeXMath.Types
import qualified Data.Map as M
import Control.Applicative ((<$>), (<|>))
import Data.Maybe (fromMaybe)

-- | Replace all characters in the string A-Z, a-z with their corresponding mathvariant unicode character.
-- | MathML has a mathvariant attribute which is unimplemented in Firefox
-- | (see https://bugzilla.mozilla.org/show_bug.cgi?id=114365)
-- | Therefore, we may want to translate mathscr, etc to unicode symbols directly.
toUnicode :: TextType -> String -> String
toUnicode TextScript s       = map (mapChar mathscr) s
toUnicode TextBoldScript s   = map (mapChar mathbfscr) s
toUnicode TextFraktur s      = map (mapChar mathfrak) s
toUnicode TextBoldFraktur s  = map (mapChar mathbffrak) s
toUnicode TextDoubleStruck s = map (mapChar mathbb) s
toUnicode _ s = s

mapChar :: [(Char, Char)] -> Char -> Char
mapChar m c = fromMaybe c (M.lookup c charMap)
  where
    charMap = M.fromList m
-- | The inverse of toUnicode, returns the corresponding 
-- | A-Za-z character and 'TextType' of a unicode character. 
fromUnicode :: Char -> Maybe (TextType, Char) 
fromUnicode c = 
  getTTChar c mathscr TextScript <|>
  getTTChar c mathbfscr TextBoldScript <|>
  getTTChar c mathfrak TextFraktur <|>
  getTTChar c mathbffrak  TextBoldFraktur <|>
  getTTChar c mathbb TextDoubleStruck  

getTTChar :: Char -> [(Char, Char)] -> TextType -> Maybe (TextType, Char)
getTTChar c m ttype = (,) ttype <$> M.lookup c charMap
  where
    charMap = M.fromList $ reverseKeys m

reverseKeys :: [(a, b)] -> [(b, a)]
reverseKeys = map (\(k,v) -> (v, k)) 

-- This list is from http://www.w3.org/TR/MathML2/script.html
mathscr :: [(Char, Char)]
mathscr =  [
             ('A', '\x1D49C')
           , ('B', '\x0212C')
           , ('C', '\x1D49E')
           , ('D', '\x1D49F')
           , ('E', '\x02130')
           , ('F', '\x02131')
           , ('G', '\x1D4A2')
           , ('H', '\x0210B')
           , ('I', '\x02110')
           , ('J', '\x1D4A5')
           , ('K', '\x1D4A6')
           , ('L', '\x02112')
           , ('M', '\x02133')
           , ('N', '\x1D4A9')
           , ('O', '\x1D4AA')
           , ('P', '\x1D4AB')
           , ('Q', '\x1D4AC')
           , ('R', '\x0211B')
           , ('S', '\x1D4AE')
           , ('T', '\x1D4AF')
           , ('U', '\x1D4B0')
           , ('V', '\x1D4B1')
           , ('W', '\x1D4B2')
           , ('X', '\x1D4B3')
           , ('Y', '\x1D4B4')
           , ('Z', '\x1D4B5')
           , ('a', '\x1D4B6')
           , ('b', '\x1D4B7')
           , ('c', '\x1D4B8')
           , ('d', '\x1D4B9')
           , ('e', '\x0212F')
           , ('f', '\x1D4BB')
           , ('g', '\x0210A')
           , ('h', '\x1D4BD')
           , ('i', '\x1D4BE')
           , ('j', '\x1D4BF')
           , ('k', '\x1D4C0')
           , ('l', '\x1D4C1')
           , ('m', '\x1D4C2')
           , ('n', '\x1D4C3')
           , ('o', '\x02134')
           , ('p', '\x1D4C5')
           , ('q', '\x1D4C6')
           , ('r', '\x1D4C7')
           , ('s', '\x1D4C8')
           , ('t', '\x1D4C9')
           , ('u', '\x1D4CA')
           , ('v', '\x1D4CB')
           , ('w', '\x1D4CC')
           , ('x', '\x1D4CD')
           , ('y', '\x1D4CE')
           , ('z', '\x1D4CF')
           ]

-- Bold variant of mathscr, taken from unicode.
mathbfscr :: [(Char, Char)]
mathbfscr =  [
             ('A', '\x1D4D0')
           , ('B', '\x1D4D1')
           , ('C', '\x1D4D2')
           , ('D', '\x1D4D3')
           , ('E', '\x1D4D4')
           , ('F', '\x1D4D5')
           , ('G', '\x1D4D6')
           , ('H', '\x1D4D7')
           , ('I', '\x1D4D8')
           , ('J', '\x1D4D9')
           , ('K', '\x1D4DA')
           , ('L', '\x1D4DB')
           , ('M', '\x1D4DC')
           , ('N', '\x1D4DD')
           , ('O', '\x1D4DE')
           , ('P', '\x1D4DF')
           , ('Q', '\x1D4E0')
           , ('R', '\x1D4E1')
           , ('S', '\x1D4E2')
           , ('T', '\x1D4E3')
           , ('U', '\x1D4E4')
           , ('V', '\x1D4E5')
           , ('W', '\x1D4E6')
           , ('X', '\x1D4E7')
           , ('Y', '\x1D4E8')
           , ('Z', '\x1D4E9')
           , ('a', '\x1D4EA')
           , ('b', '\x1D4EB')
           , ('c', '\x1D4EC')
           , ('d', '\x1D4ED')
           , ('e', '\x1D4EE')
           , ('f', '\x1D4EF')
           , ('g', '\x1D4F0')
           , ('h', '\x1D4F1')
           , ('i', '\x1D4F2')
           , ('j', '\x1D4F3')
           , ('k', '\x1D4F4')
           , ('l', '\x1D4F5')
           , ('m', '\x1D4F6')
           , ('n', '\x1D4F7')
           , ('o', '\x1D4F8')
           , ('p', '\x1D4F9')
           , ('q', '\x1D4FA')
           , ('r', '\x1D4FB')
           , ('s', '\x1D4FC')
           , ('t', '\x1D4FD')
           , ('u', '\x1D4FE')
           , ('v', '\x1D4FF')
           , ('w', '\x1D500')
           , ('x', '\x1D501')
           , ('y', '\x1D502')
           , ('z', '\x1D503')
           ]

-- Similar to mathscr above, we translate manually.
-- This list is from http://www.w3.org/TR/MathML2/double-struck.html
mathbb :: [(Char, Char)]
mathbb =   [
             ('A', '\x1D538')
           , ('B', '\x1D539')
           , ('C', '\x02102')
           , ('D', '\x1D53B')
           , ('E', '\x1D53C')
           , ('F', '\x1D53D')
           , ('G', '\x1D53E')
           , ('H', '\x0210D')
           , ('I', '\x1D540')
           , ('J', '\x1D541')
           , ('K', '\x1D542')
           , ('L', '\x1D543')
           , ('M', '\x1D544')
           , ('N', '\x02115')
           , ('O', '\x1D546')
           , ('P', '\x02119')
           , ('Q', '\x0211A')
           , ('R', '\x0211D')
           , ('S', '\x1D54A')
           , ('T', '\x1D54B')
           , ('U', '\x1D54C')
           , ('V', '\x1D54D')
           , ('W', '\x1D54E')
           , ('X', '\x1D54F')
           , ('Y', '\x1D550')
           , ('Z', '\x02124')
           , ('a', '\x1D552')
           , ('b', '\x1D553')
           , ('c', '\x1D554')
           , ('d', '\x1D555')
           , ('e', '\x1D556')
           , ('f', '\x1D557')
           , ('g', '\x1D558')
           , ('h', '\x1D559')
           , ('i', '\x1D55A')
           , ('j', '\x1D55B')
           , ('k', '\x1D55C')
           , ('l', '\x1D55D')
           , ('m', '\x1D55E')
           , ('n', '\x1D55F')
           , ('o', '\x1D560')
           , ('p', '\x1D561')
           , ('q', '\x1D562')
           , ('r', '\x1D563')
           , ('s', '\x1D564')
           , ('t', '\x1D565')
           , ('u', '\x1D566')
           , ('v', '\x1D567')
           , ('w', '\x1D568')
           , ('x', '\x1D569')
           , ('y', '\x1D56A')
           , ('z', '\x1D56B')
           , ('0', '\x1D7D8')
           , ('1', '\x1D7D9')
           , ('2', '\x1D7DA')
           , ('3', '\x1D7DB')
           , ('4', '\x1D7DC')
           , ('5', '\x1D7DD')
           , ('6', '\x1D7DE')
           , ('7', '\x1D7DF')
           , ('8', '\x1D7E0')
           , ('9', '\x1D7E1')
           ]

-- Fraktur fonts, taken from unicode.
mathfrak :: [(Char, Char)]
mathfrak = [
          ('A','\x1D504')
        , ('B','\x1D505')
        , ('C','\x0212D')
        , ('D','\x1D507')
        , ('E','\x1D508')
        , ('F','\x1D509')
        , ('G','\x1D50A')
        , ('H','\x0210C')
        , ('I','\x02111')
        , ('J','\x1D50D')
        , ('K','\x1D50E')
        , ('L','\x1D50F')
        , ('M','\x1D510')
        , ('N','\x1D511')
        , ('O','\x1D512')
        , ('P','\x1D513')
        , ('Q','\x1D514')
        , ('R','\x0211C')
        , ('S','\x1D516')
        , ('T','\x1D517')
        , ('U','\x1D518')
        , ('V','\x1D519')
        , ('W','\x1D51A')
        , ('X','\x1D51B')
        , ('Y','\x1D51C')
        , ('Z','\x02128')
        , ('a','\x1D51E')
        , ('b','\x1D51F')
        , ('c','\x1D520')
        , ('d','\x1D521')
        , ('e','\x1D522')
        , ('f','\x1D523')
        , ('g','\x1D524')
        , ('h','\x1D525')
        , ('i','\x1D526')
        , ('j','\x1D527')
        , ('k','\x1D528')
        , ('l','\x1D529')
        , ('m','\x1D52A')
        , ('n','\x1D52B')
        , ('o','\x1D52C')
        , ('p','\x1D52D')
        , ('q','\x1D52E')
        , ('r','\x1D52F')
        , ('s','\x1D530')
        , ('t','\x1D531')
        , ('u','\x1D532')
        , ('v','\x1D533')
        , ('w','\x1D534')
        , ('x','\x1D535')
        , ('y','\x1D536')
        , ('z','\x1D537')
        ]

-- Bold fraktur fonts, taken from unicode.
mathbffrak :: [(Char, Char)]
mathbffrak = [
          ('A','\x1D56C')
        , ('B','\x1D56D')
        , ('C','\x1D56E')
        , ('D','\x1D57F')
        , ('E','\x1D570')
        , ('F','\x1D571')
        , ('G','\x1D572')
        , ('H','\x1D573')
        , ('I','\x1D574')
        , ('J','\x1D575')
        , ('K','\x1D576')
        , ('L','\x1D577')
        , ('M','\x1D578')
        , ('N','\x1D579')
        , ('O','\x1D57A')
        , ('P','\x1D57B')
        , ('Q','\x1D57C')
        , ('R','\x1D57D')
        , ('S','\x1D57E')
        , ('T','\x1D57F')
        , ('U','\x1D580')
        , ('V','\x1D581')
        , ('W','\x1D582')
        , ('X','\x1D583')
        , ('Y','\x1D584')
        , ('Z','\x1D585')
        , ('a','\x1D586')
        , ('b','\x1D587')
        , ('c','\x1D588')
        , ('d','\x1D589')
        , ('e','\x1D58A')
        , ('f','\x1D58B')
        , ('g','\x1D58C')
        , ('h','\x1D58D')
        , ('i','\x1D58E')
        , ('j','\x1D58F')
        , ('k','\x1D590')
        , ('l','\x1D591')
        , ('m','\x1D592')
        , ('n','\x1D593')
        , ('o','\x1D594')
        , ('p','\x1D595')
        , ('q','\x1D596')
        , ('r','\x1D597')
        , ('s','\x1D598')
        , ('t','\x1D599')
        , ('u','\x1D59A')
        , ('v','\x1D59B')
        , ('w','\x1D59C')
        , ('x','\x1D59D')
        , ('y','\x1D59E')
        , ('z','\x1D59F')
        ]

