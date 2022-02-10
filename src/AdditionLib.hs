module AdditionLib where

import System.IO
import Control.Monad
import System.IO.Strict as NLazy

--Все дополнительные небольшие функции лежат тут

mergeList :: [[a]] -> [a]
mergeList = foldl1 (++)

writeIntoFile :: String -> String -> IO ()
writeIntoFile fileName what = do
  file <- openFile fileName WriteMode
  hSetEncoding file utf8
  hPutStr file what
  hClose file

readAllFromFile :: String -> IO String
readAllFromFile = NLazy.readFile