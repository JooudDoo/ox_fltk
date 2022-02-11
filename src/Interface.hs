{-# LANGUAGE OverloadedStrings #-}
module Interface where

import AdditionLib
import InterfaceLib
import Control.Concurrent
import Logic

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Control.Monad
import Data.IORef
import Data.Text (pack, Text, unpack)

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
data GameConfig =
    GC
    {
      cells :: CellsConfig,
      wind :: WindowConfig
    }

gameCellPVE :: [Ref Button] -> Int -> Ref Button -> IO ()
gameCellPVE btnLst inRow b' = do
  state <- getLabel b'
  when (state == "") $ do
    playerRead <- readAllFromFile "temp"
    let humanPlayer = pl (pack playerRead)
    newOXButtonState b' False
    gameState <- checkWin humanPlayer btnLst inRow
    case gameState of
      Win -> winWidget btnLst humanPlayer
      Draw -> drawWidget btnLst
      Game -> do
        let botPlayer = rPl  humanPlayer
        field <- readCells btnLst inRow
        botTurn <- callForBotRandom (refactorList field inRow) botPlayer
        setLabel (refactorList btnLst inRow !! fst botTurn !! snd botTurn) (pack $ plT botPlayer)
        gameState <- checkWin botPlayer btnLst inRow
        case gameState of
          Win -> winWidget btnLst botPlayer
          Draw -> drawWidget btnLst
          Game -> return ()

gameCellPVP :: [Ref Button] -> Int -> Ref Button -> IO ()
gameCellPVP btnLst inRow b' = do
  state <- getLabel b'
  when (state == "") $ do
    playerRead <- readAllFromFile "temp"
    let currentPlayer = pl (pack playerRead)
    newOXButtonState b' True
    gameState <- checkWin currentPlayer btnLst inRow
    case gameState of
      Win -> winWidget btnLst currentPlayer
      Draw -> drawWidget btnLst
      Game -> return()


createGameCells :: WindowConfig -> CellsConfig -> ([Ref Button] -> Int -> Ref Button -> IO ()) -> IO [Ref Button]
createGameCells wndConf cllsConf func = do
 lstButtonsIO <- newIORef ([] :: [Ref Button])
 forM_ [0..inRow*inRow-1] $ \i -> do
    button <- newButton (i `mod` inRow*buttonSize + padX) (i `div` inRow*buttonSize + padY) buttonSize buttonSize (Just "")
    setLabelsize button (FontSize 15)
    modifyIORef lstButtonsIO (++ [button])
    return ()
 lstButtons <- readIORef lstButtonsIO
 forM_ [0..inRow*inRow-1] $ \i ->
    setCallback (lstButtons !! i) (func lstButtons inRow)
 return lstButtons
 where
   padX = (windowWidth - buttonSize * inRow) `div` 2
   padY = (windowHeight - buttonSize * inRow) `div` 2
   inRow = cntInRow cllsConf
   buttonSize = cellSize cllsConf
   windowWidth = width wndConf
   windowHeight = height wndConf


runSimpleXOPVE :: WindowConfig -> CellsConfig -> Ref Window -> Ref Button -> IO ()
runSimpleXOPVE wndConf cllsConf mainMenu _ = do
  destroy mainMenu
  writeIntoFile "temp" "X"
  window <- doubleWindowNew
            (Size (Width $ width wndConf) (Height $ height wndConf))
            Nothing
            (Just "Simple XO PVE")
  begin window
  gameCells <- createGameCells wndConf cllsConf gameCellPVE
  showWidget window


runSimpleXOPVP :: WindowConfig -> CellsConfig -> Ref Window -> Ref Button -> IO ()
runSimpleXOPVP wndConf cllsConf mainMenu _ = do
  destroy mainMenu
  writeIntoFile "temp" "X"
  window <- doubleWindowNew
            (Size (Width $ width wndConf) (Height $ height wndConf))
            Nothing
            (Just "Simple XO PVP")
  begin window
  gameCells <- createGameCells wndConf cllsConf gameCellPVP
  showWidget window
  

mainMenu :: WindowConfig -> GameConfig -> IO ()
mainMenu windowC gameConfig = do
  window <- windowNew
          (Size (Width $ width windowC) (Height $ height windowC))
          Nothing
          (Just "Main Menu")
  begin window

  simplePVPMode <- newButton (width windowC `div` 4) (height windowC `div` 2) 200 50 (Just "Simple XO PVP")
  setLabelsize simplePVPMode (FontSize 20)
  setCallback simplePVPMode (runSimpleXOPVP (wind gameConfig) (cells gameConfig) window)

  simplePVEMode <- newButton (width windowC `div` 4) (height windowC `div` 2 - 50) 200 50 (Just "Simple XO PVE")
  setLabelsize simplePVEMode (FontSize 20)
  setCallback simplePVEMode (runSimpleXOPVE (wind gameConfig) (cells gameConfig) window)

  showWidget window
  FL.run
  FL.flush
  return ()