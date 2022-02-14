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
  b' <- newButton 0 (height (windCnf gui)-size) size size (Just "<-")
  setLabelsize b' (FontSize 12)
  setCallback b' (exitButtonFunc gui btns)


exitButtonFunc :: MainGUI -> [Ref Button] -> Ref Button -> IO ()
exitButtonFunc gui btns b' = do
  forM_ [0..length btns-1] $ \i -> do
    hide (btns!!i)
  hide b'
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
  gameCells <- createGameCells (windCnf gui) (cllsCnf gui) gameCellPVE
  exitButton gui gameCells
  showWidget $ mainWindow gui


runSimpleXOPVP :: MainGUI -> IO ()
runSimpleXOPVP gui = do
  writeIntoFile "temp" "X"
  setLabel (mainWindow gui) "Simple XO PVP"
  begin $ mainWindow gui
  gameCells <- createGameCells (windCnf gui) (cllsCnf gui) gameCellPVP
  exitButton gui gameCells
  showWidget $ mainWindow gui


runHardXOPVP :: MainGUI -> IO ()
runHardXOPVP gui = do
  setLabel (mainWindow gui) "Hard XO PVP"
  begin $ mainWindow gui

  exitButton gui []
  showWidget $ mainWindow gui


startGameMode :: MainGUI -> (MainGUI -> IO ()) -> Ref Button -> IO ()
startGameMode gui func _ = do
  hide $ packs gui
  func gui


mainMenu :: MainGUI -> IO ()
mainMenu gui = do
  setLabel (mainWindow gui) "Main Menu"
  begin $ mainWindow gui
  showWidget $ packs gui
  showWidget $ mainWindow gui
  return ()


decCells :: IORef Int -> Ref Box -> Ref Button -> IO ()
decCells cnt label _ = do
  cnt' <- readIORef cnt
  when (cnt' > 3) $ do
    writeIORef cnt (cnt'-1)
    setLabel label (pack $ show $ cnt'-1)
    return ()


addCells :: IORef Int -> Ref Box -> Ref Button -> IO ()
addCells cnt label _ = do
  cnt' <- readIORef cnt
  when (cnt' < 8) $ do
    modifyIORef cnt (+1)
    setLabel label (pack $ show $ cnt'+1)
    return ()


createMainMenu :: WindowConfig -> IO ()
createMainMenu windowC = do
  window <- windowNew
          (Size (Width $ width windowC) (Height $ height windowC))
          Nothing
          (Just "Main Menu")
  cellsCount <- newIORef 3

  mainframe <- groupNew (toRectangle (0,0,width windowC, height windowC)) Nothing

  begin mainframe
  simplePVPMode <- newButton 0 (height windowC `div` 4) 200 50 (Just "Simple XO PVP")
  simplePVEMode <- newButton 0 (height windowC `div` 4 + 50) 200 50 (Just "Simple XO PVE")
  hardPVPMode <- newButton 0 (height windowC `div` 4 + 100) 200 50 (Just "Hard XO PVP")
  decCntCells <- newButton 0 (height windowC `div` 4 + 150) 20 20 (Just "-")
  addCntCells <- newButton 20 (height windowC `div` 4 + 170) 20 20 (Just "+")
  cntCells <- newLabel 0 (height windowC `div` 4 + 190) 20 20 (Just "3")
  end mainframe

  setLabelsize hardPVPMode (FontSize 20)
  setLabelsize simplePVEMode (FontSize 20)
  setLabelsize simplePVPMode (FontSize 20)
  
  setLabelsize decCntCells (FontSize 10)
  setLabelsize addCntCells (FontSize 10)
  setLabelsize cntCells (FontSize 10)

  let mainWindow = MG
                    {

                      windCnf = windowC,
                      cllsCnf = CC
                        {
                          cellSize = 50,
                          cntInRow = cellsCount
                        },
                      mainWindow = window,
                      packs = mainframe
                    }

  setCallback simplePVPMode (startGameMode mainWindow runSimpleXOPVP)
  setCallback simplePVEMode (startGameMode mainWindow runSimpleXOPVE)
  setCallback hardPVPMode (startGameMode mainWindow runHardXOPVP)

  setCallback decCntCells (decCells cellsCount cntCells)
  setCallback addCntCells (addCells cellsCount cntCells)

  mainMenu mainWindow
  FL.run
  FL.flush