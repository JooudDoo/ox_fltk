{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module AdditionLib where

import System.IO
import Control.Monad
import System.IO.Strict as NLazy
import Data.Text (Text, pack)
import qualified Data.ByteString as B
import Graphics.UI.FLTK.LowLevel.PNGImage
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Control.Exception
import Data.Monoid

debugging :: Bool
debugging = False

--Все дополнительные небольшие функции лежат тут


data GameState = Win | Draw | Game deriving (Eq, Show)
gState :: GameState -> GameState -> GameState
gState Win _= Win
gState Draw Game = Draw
gState _ Win = Win
gState Game Draw = Draw
gState _ _ = Game


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


changeInList :: [a] -> a -> Int -> [a]
changeInList lst elem ind = take ind lst ++ [elem] ++ drop (ind+1) lst


readAssetsImages :: [String] -> IO [Ref PNGImage]
readAssetsImages pathsRaw = do
  let paths = map ("assets/images/" ++) pathsRaw
  mapM
      (\p -> do
          bytes <- B.readFile p `catch`
                    (\(_ :: SomeException) -> ioError
                          (userError ("Image does not exist at path: " ++ p)))
          iE <- pngImageNewWithData (pack "") bytes
          case iE of
            Left _ -> ioError (userError ("Unable to read image data into a PNGImage:\n" ++ p))
            Right i -> return i)
      paths
