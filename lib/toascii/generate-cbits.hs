-- Generates cbits/keyToASCII.c and cbits/valToASCII.c
-- from Data.txt.
import Control.Monad
import System.IO

main :: IO ()
main = do
  let topair s = case break (=='\t') s of
                      (x,'\t':y) -> (x,y)
                      _ -> error "no tab (topair)"
  keyvals <- (map topair . lines) <$> readFile "Data.txt"
  keytoascii <- openFile "../../cbits/keyToASCII.c" WriteMode
  valtoascii <- openFile "../../cbits/valToASCII.c" WriteMode
  let donotmodify = "/* DO NOT MODIFY MANUALLY */\n" ++
       "/* Modify lib/toascii/Data.txt and make -C lib/toascii */"
  hPutStrLn valtoascii donotmodify
  hPutStrLn valtoascii $
    "char *toASCIILut[8923] = {" ++ (snd $ head keyvals)
  hPutStrLn keytoascii donotmodify
  hPutStrLn keytoascii $
    "int keylookup[8923] = {" ++ (fst $ head keyvals)
  forM_ (tail keyvals) $ \(k, v) -> do
    hPutStrLn valtoascii (',':v)
    hPutStrLn keytoascii (',':k)
  hPutStrLn valtoascii "};"
  hPutStrLn keytoascii "};"
  hClose keytoascii
  hClose valtoascii

