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

This module is derived from the list of unicode to LaTeX mappings
compiled by Günter Milde.

An unmodified original copy of this work can be obtained from <http://milde.users.sourceforge.net/LUCR/Math/ here>

-}

{-

All request for support should be sent to the
current maintainer of this module and NOT the aforementioned original author.

The work was originally licensed under the LaTeX Project Public License.

Changes to the work can be seen via the git commit history to this module.

Whilst distributed under the GPL in conformance with clause 10a, all
deriviations of this work must also conform with clause 6 of the the
LaTeX Project Public License.

-}

module Text.TeXMath.Unicode.ToTeX ( getTeXMath
                                      , getSymbolType
                                      , records
                                      ) where

import qualified Data.Map as M
import Text.TeXMath.TeX
import Text.TeXMath.Types
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Control.Applicative hiding (optional)
import Text.TeXMath.Unicode.ToASCII (getASCII)
import Text.TeXMath.Unicode.ToUnicode (fromUnicodeChar)
import qualified Text.TeXMath.Shared as S
import qualified Control.Applicative as CA
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe
import Foreign.Storable
import qualified Data.Foldable as F

-- | Converts a string of unicode characters into a strong of equivalent
-- TeXMath commands. An environment is a list of strings specifying which
-- additional packages are available.
getTeXMath :: String -> Env -> [TeX]
getTeXMath s e = concatMap (charToString e) s

-- Categories which require braces
commandTypes :: [TeXSymbolType]
commandTypes = [Accent, Rad, TOver, TUnder]

-- Guaranteed to return latex safe string
charToString :: Env -> Char -> [TeX]
charToString e c =
  fromMaybe fallback
    (charToLaTeXString e c <|> textConvert e c)
  where
    fallback = concatMap asciiToLaTeX $ getASCII c
    asciiToLaTeX ac = fromMaybe [escapeLaTeX ac] (charToLaTeXString e ac)

-- Takes a single character and attempts to convert it to a latex string
charToLaTeXString :: Env -> Char -> Maybe [TeX]
charToLaTeXString environment c = do
  v <- M.lookup c recordsMap
  -- Required packages for the command
  let toLit [x] = [Token x]
      toLit []   = []
      toLit cs   = [Literal cs]
  let cmds = commands v
  raw <- lookup "base" cmds
          <|> listToMaybe (mapMaybe (flip lookup cmds) environment)
  let latexCommand = if isControlSeq raw
                        then [ControlSeq raw]
                        else toLit raw
  return $ if category v `elem` commandTypes
              then latexCommand ++ [Grouped []]
              else latexCommand

-- Convert special unicode characters not in the standard mapping
textConvert :: Env -> Char -> Maybe [TeX]
textConvert env c = do
  (ttype, v) <- fromUnicodeChar c
  return [ControlSeq (S.getLaTeXTextCommand env ttype), Grouped [Token v]]


records :: [Record]
records = map snd $  M.toAscList recordsMap

-- | Returns TeX symbol type corresponding to a unicode character.
getSymbolType :: Char -> TeXSymbolType
getSymbolType c = fromMaybe Ord (category <$> M.lookup c recordsMap)


foreign import ccall unsafe "&toTexCharIntKey" c_toTexCharIntKey :: Ptr CInt
foreign import ccall unsafe "&char2TexCategory" c_char2TexCategory:: Ptr (Ptr CChar)
foreign import ccall unsafe "&char2TexCommand" c_char2TexCommand :: Ptr (Ptr CChar)
foreign import ccall unsafe "&char2PkgNam" c_char2PkgNam :: Ptr (Ptr CChar)
foreign import ccall unsafe "&char2TexComment" c_char2TexComment :: Ptr (Ptr CChar)

ingestAction :: Storable a => (a -> IO b)-> Ptr a -> Int -> IO b
ingestAction f ptr ix = do pval <- peekElemOff ptr ix;  f  pval

{-# NOINLINE recordsMap #-}
recordsMap :: M.Map Char Record
recordsMap = F.foldl' (\mp rec -> M.insertWith mergeRecords (uchar rec) rec  mp) M.empty  $ reverse normalizedRecords
  {-  NOTE, the REVERSE is needed to preserve the implicit ordering preferences
  between the common character records  -}
  where

      {- NOTE, these are all defined in a where clause to prevent them from being top
        level CAF things that never get gc'd
      -}

      mergeRecords :: Record -> Record ->Record
      mergeRecords (Record c1 ls1  cat1 comm1) (Record c2 ls2  cat2 comm2)
          | c1==c2 && cat1==cat2  && comm1 == comm2= Record c1 (ls1 ++ ls2) cat1 comm1
          | c1 /= c2 = error $ "mixed up records for characters "++ show c1 ++ " "  ++ show c2
              ++ " please file a bug report, this is a bug in TexMath"
          | otherwise =
             error $ "there is data corruption in the records for character, please report the bug! " ++ show c1

      normalizedRecords :: [Record]
      normalizedRecords = getZipList $ (\uchr cmdL cmdR cat cmmnt-> Record uchr [(cmdL,cmdR)] cat cmmnt )
                            CA.<$> ZipList recordChars CA.<*> ZipList recordPkgName
                              CA.<*> ZipList recordCommand CA.<*> ZipList recordTexCategory CA.<*> ZipList recordComment

      {-# NOINLINE recordChars#-}
      recordChars :: [Char]
      recordChars = unsafePerformIO $
         mapM  (ingestAction (return.toEnum.fromIntegral) c_toTexCharIntKey) [0 .. 6093]

      {-# NOINLINE recordTexCategory #-}
      recordTexCategory :: [TeXSymbolType]
      recordTexCategory = unsafePerformIO $ mapM (ingestAction (fmap read.peekCString) c_char2TexCategory) [0.. 6093]

      {-# NOINLINE  recordComment #-}
      recordComment :: [String]
      recordComment = unsafePerformIO $ mapM (ingestAction (peekCString) c_char2TexComment) [0 ..  6093]

      {-# NOINLINE recordCommand#-}
      recordCommand :: [String]
      recordCommand = unsafePerformIO $ mapM (ingestAction (peekCString) c_char2TexCommand) [0 ..  6093]

      {-# NOINLINE recordPkgName #-}
      recordPkgName :: [String ]
      recordPkgName = unsafePerformIO $ mapM (ingestAction peekCString c_char2PkgNam) [0.. 6093]


--records :: [Record]
--records =

