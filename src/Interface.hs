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

windowWidth :: Int
windowWidth = 500
windowHeight :: Int
windowHeight = 500
buttonSize :: Int
buttonSize = 50

gameCell :: [Ref Button] -> Int -> Ref Button -> IO ()
gameCell btnLst inRow b' = do
  player <- readAllFromFile "temp"
  newOXButtonState b'
  checkWin (pl (pack player)) btnLst inRow

createGameCells :: Int -> IO [Ref Button]
createGameCells size = do
 lstButtonsIO <- newIORef ([] :: [Ref Button])
 forM_ [0..size*size-1] $ \i -> do
    button <- newButton (i `mod` size*buttonSize + padX) (i `div` size*buttonSize + padY) buttonSize buttonSize (Just "")
    setLabelsize button (FontSize 15)
    modifyIORef lstButtonsIO (++ [button])
    return ()
 lstButtons <- readIORef lstButtonsIO
 forM_ [0..size*size-1] $ \i -> do
    setCallback (lstButtons !! i) (gameCell lstButtons size)
 return lstButtons
 where
   padX = (windowWidth - buttonSize * size) `div` 2
   padY = (windowHeight - buttonSize * size) `div` 2

runInterface :: Int -> IO ()
runInterface countOfCells = do
  writeIntoFile "temp" "X"

  window <- doubleWindowNew
            (Size (Width windowWidth) (Height windowHeight))
            Nothing
            Nothing
  begin window

  gameCells <- createGameCells countOfCells

  showWidget window

  FL.run
  FL.flush