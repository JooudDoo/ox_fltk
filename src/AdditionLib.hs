{-# LANGUAGE OverloadedStrings #-}
module AdditionLib where

import System.IO
import Control.Monad
import System.IO.Strict as NLazy
import Data.Text (Text)

--Все дополнительные небольшие функции лежат тут

data GameState = Win | Draw | Game deriving (Eq, Show)

data Player = Cross | Zero | NaP deriving (Eq,Show)
pl :: Text -> Player
pl "X" = Cross
pl "O" = Zero
pl _ = NaP
plT :: Player -> String
plT Cross = "X"
plT Zero = "O"
plT _ = ""
rPl :: Player -> Player
rPl Zero = Cross
rPl Cross = Zero
rPl _ = NaP

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