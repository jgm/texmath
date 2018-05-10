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

This module exposes functions which attempt to approximate unicode characters as ASCII.

Information taken from the <https://pypi.python.org/pypi/Unidecode Unidecode python package> which is based upon
the <http://search.cpan.org/~sburke/Text-Unidecode-1.01/lib/Text/Unidecode.pm Text::Unidecode Perl Module> by Sean M. Burke.

-}
module Text.TeXMath.Unicode.ToASCII (getASCII) where

import qualified Data.IntMap as M
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe
import Foreign.Storable

-- | Approximates a single unicode character as an ASCII string
--  (each character is between 0x00 and 0x7F).
getASCII :: Char -> String
getASCII u = fromMaybe "" (M.lookup (ord u) table)

table :: M.IntMap String
table = M.fromList $ zip realKeyKey realValVal

foreign import ccall unsafe "&keylookup" keyKey :: Ptr CInt
foreign import ccall unsafe "&toASCIILut" valVal :: Ptr (Ptr CChar)

{-# NOINLINE realKeyKey#-}
realKeyKey :: [Int]
realKeyKey = unsafePerformIO $ mapM (\off ->  do  daInt <- peekElemOff keyKey off ; return $ fromIntegral daInt ) [ 0 .. 8922]

{-# NOINLINE realValVal #-}
realValVal :: [String]
realValVal = unsafePerformIO $ mapM (\off -> do daPtrStr <- peekElemOff valVal off ; peekCString daPtrStr ) [0 .. 8922]
