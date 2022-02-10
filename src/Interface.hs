{-# LANGUAGE OverloadedStrings #-}
module Interface where

import AdditionLib
import InterfaceLib

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Control.Monad
import Data.IORef
import Data.Text (pack, Text)

--Файл с отрисовкой игрового интерфейса

data WindowConfig = 
    WC 
    {
        width :: Int,
        height :: Int
    }
data CellsConfig =
    CC
    {
        cellSize :: Int,
        cntInRow :: Int
    }

gameCellFunc :: [Ref Button] -> Int -> Ref Button -> IO ()
gameCellFunc btnLst inRow b' = do
  player <- readAllFromFile "temp"
  newOXButtonState b'
  checkWin (pl (pack player)) btnLst inRow


createGameCells :: WindowConfig -> CellsConfig -> IO [Ref Button]
createGameCells wndConf cllsConf = do
 lstButtonsIO <- newIORef ([] :: [Ref Button])
 forM_ [0..inRow*inRow-1] $ \i -> do
    button <- newButton (i `mod` inRow*buttonSize + padX) (i `div` inRow*buttonSize + padY) buttonSize buttonSize (Just "")
    setLabelsize button (FontSize 15)
    modifyIORef lstButtonsIO (++ [button])
    return ()
 lstButtons <- readIORef lstButtonsIO
 forM_ [0..inRow*inRow-1] $ \i -> do
    setCallback (lstButtons !! i) (gameCellFunc lstButtons inRow)
 return lstButtons
 where
   padX = (windowWidth - buttonSize * inRow) `div` 2
   padY = (windowHeight - buttonSize * inRow) `div` 2
   inRow = cntInRow cllsConf
   buttonSize = cellSize cllsConf
   windowWidth = width wndConf
   windowHeight = height wndConf


runSimpleXO :: WindowConfig -> CellsConfig-> IO ()
runSimpleXO wndConf cllsConf = do
  writeIntoFile "temp" "X"

  window <- doubleWindowNew
            (Size (Width $ width wndConf) (Height $ height wndConf))
            Nothing
            Nothing
  begin window

  gameCells <- createGameCells wndConf cllsConf

  showWidget window

  FL.run
  FL.flush