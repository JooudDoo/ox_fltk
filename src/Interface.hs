{-# LANGUAGE OverloadedStrings #-}
module Interface where

import AdditionLib
import InterfaceLib
import Logic

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.Text (pack, Text, unpack)

--Файл с отрисовкой игрового интерфейса

exitButton :: MainGUI -> [Ref Button] -> IO ()
exitButton gui btns = do
  let size = 20
  b' <- newButton 0 (height (wind $ gameCnf gui)-size) size size (Just "<-")
  setLabelsize b' (FontSize 12)
  setCallback b' (exitButtonFunc gui btns)


exitButtonFunc :: MainGUI -> [Ref Button] -> Ref Button -> IO ()
exitButtonFunc gui btns b' = do
  forM_ [0..length btns-1] $ \i -> do
    hide (btns!!i)
  destroy b'
  mainMenu gui
  

gameCellPVE :: [Ref Button] -> Int -> Ref Button -> IO ()
gameCellPVE btnLst inRow b' = do
  state <- getLabel b'
  when (state == "") $ do
    let humanPlayer = Cross
    newOXButtonState b' Cross
    gameState <- checkWin humanPlayer btnLst inRow
    case gameState of
      Win -> winWidget btnLst humanPlayer
      Draw -> drawWidget btnLst
      Game -> do
        let botPlayer = rPl  humanPlayer
        field <- readCells btnLst inRow
        botTurn <- callForBotRandom (refactorList field inRow) botPlayer
        let botCell = refactorList btnLst inRow !! fst botTurn !! snd botTurn
        setLabel botCell (pack $ plT botPlayer)
        switchColorPlayer botPlayer botCell
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
    newOXButtonState b' NaP
    gameState <- checkWin currentPlayer btnLst inRow
    case gameState of
      Win -> winWidget btnLst currentPlayer
      Draw -> drawWidget btnLst
      Game -> return()


runSimpleXOPVE :: MainGUI -> IO ()
runSimpleXOPVE gui = do
  setLabel (mainWindow gui) "Simple XO PVE"
  begin $ mainWindow gui
  gameCells <- createGameCells (wind $ gameCnf gui) (cells $ gameCnf gui) gameCellPVE
  exitButton gui gameCells
  showWidget $ mainWindow gui


runSimpleXOPVP :: MainGUI -> IO ()
runSimpleXOPVP gui = do
  writeIntoFile "temp" "X"
  setLabel (mainWindow gui) "Simple XO PVP"
  begin $ mainWindow gui
  gameCells <- createGameCells (wind $ gameCnf gui) (cells $ gameCnf gui) gameCellPVP
  exitButton gui gameCells
  showWidget $ mainWindow gui


startGameMode :: MainGUI -> (MainGUI -> IO ()) -> Ref Button -> IO ()
startGameMode mainWindow func _ = do
  let buttons = btns mainWindow
  forM_ [0..length buttons - 1] $ \i -> do
    hide (buttons !! i) 
  func mainWindow


mainMenu :: MainGUI -> IO ()
mainMenu gui = do
  setLabel (mainWindow gui) "Main Menu"
  let buttons = btns gui
  begin $ mainWindow gui
  forM_ [0..length buttons - 1] $ \i -> do
    showWidget (buttons !! i)
  showWidget $ mainWindow gui
  return ()


createMainMenu :: WindowConfig -> GameConfig -> IO ()
createMainMenu windowC gameConfig = do
  window <- windowNew
          (Size (Width $ width windowC) (Height $ height windowC))
          Nothing
          (Just "Main Menu")
  
  simplePVPMode <- newButton (width windowC `div` 4) (height windowC `div` 2) 200 50 (Just "Simple XO PVP")
  setLabelsize simplePVPMode (FontSize 20)
  simplePVEMode <- newButton (width windowC `div` 4) (height windowC `div` 2 - 50) 200 50 (Just "Simple XO PVE")
  setLabelsize simplePVEMode (FontSize 20)

  decCntCells <- newButton (width windowC `div` 4 + 200) (height windowC `div` 2 + 20) 20 20 (Just "-")
  cntCells <- newLabel (width windowC `div` 4 + 220) (height windowC `div` 2 + 20) 40 40 (Just "")
  addCntCells <- newButton (width windowC `div` 4 + 260) (height windowC `div` 2 + 20) 20 20 (Just "+")
  setLabel cntCells ("3")
  setLabelsize decCntCells (FontSize 10)
  setLabelsize cntCells (FontSize 10)
  setLabelsize addCntCells (FontSize 10)


  let mainWindow = MG
                    {
                      gameCnf = gameConfig,
                      mainWindow = window,
                      btns = [simplePVPMode, simplePVEMode]
                    }
  
  setCallback simplePVPMode (startGameMode mainWindow runSimpleXOPVP)
  setCallback simplePVEMode (startGameMode mainWindow runSimpleXOPVE)

  mainMenu mainWindow

  FL.run
  FL.flush