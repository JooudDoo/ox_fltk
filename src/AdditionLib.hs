module AdditionLib where

import System.IO
import System.IO.Strict as NLazy


writeIntoFile :: String -> String -> IO ()
writeIntoFile fileName what = do
  file <- openFile fileName WriteMode
  hSetEncoding file utf8
  hPutStr file what
  hClose file

readAllFromFile :: String -> IO (String)
readAllFromFile fileName = NLazy.readFile fileName
    