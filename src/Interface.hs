{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Interface where

import AdditionLib
import InterfaceLib
import Logic

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Control.Monad
import Data.IORef
import Data.Text (pack, Text, unpack)

--Файл с отрисовкой игрового интерфейса

images :: [String]
images = ["simplePVP.png",
          "simplePVE.png",
          "hardPVP.png",
          "hardPVE.png",
          "mainMenuBack.png"]


exitButton :: MainGUI -> Ref Group -> IO ()
exitButton gui frame = do
  let size = 20
  b' <- newButton 0 0 size size (Just "<-")
  setLabelsize b' (FontSize 12)
  setCallback b' (exitButtonFunc gui frame)
  where
    exitButtonFunc :: MainGUI -> Ref Group -> Ref Button -> IO ()
    exitButtonFunc gui frame b' = do
      hide frame --Оптимизировать удаление детей
      mainMenu gui


--Исправить костыль с лишним кодом | Смена порядка игроков (Работает криво (Больше не работает)) | Сделать смену порядка возможной
simpleCellPVE :: MainGUI -> SimpleField -> IORef Player -> Ref Button -> IO ()
simpleCellPVE gui fieldIO pla b' = do
  state <- getLabel b'
  when (state == "") $ do
    let btnLst = fieldBtns fieldIO
    let inRow = rowCnt fieldIO
    currentPlayer <- readIORef pla
    block <- readIORef $ cellToWin $ cllsCnf gui
    setLabel (labelInfo fieldIO) (pack $ plT (rPl currentPlayer) ++ " bot move") --Переделать это окно на красиво богато
    newButtonState b' currentPlayer
    checkWinRAWSimple currentPlayer block inRow btnLst >>=
      \case
        Win -> endGameScreen gui (Just fieldIO) Nothing currentPlayer Win
        Draw -> endGameScreen gui (Just fieldIO) Nothing NaP Draw
        Game -> do
          let botPlayer = rPl currentPlayer
          setLabel (labelInfo fieldIO) (pack $ plT (rPl botPlayer) ++ " human move") --Переделать это окно на красиво богато
          (btx, bty) <- readCells btnLst inRow >>= \f -> callForBot f botPlayer
          newButtonState (btnLst !! (btx * inRow + bty)) botPlayer
          checkWinRAWSimple botPlayer block inRow btnLst >>=
            \case
              Win -> endGameScreen gui (Just fieldIO) Nothing botPlayer Win
              Draw -> endGameScreen gui (Just fieldIO) Nothing NaP Draw
              Game -> return ()


simpleCellPVP :: MainGUI -> SimpleField -> IORef Player -> Ref Button -> IO ()
simpleCellPVP gui fieldIO pla b' = do
  state <- getLabel b'
  when (state == "") $ do
    currentPlayer <- readIORef pla
    block <- readIORef $ cellToWin $ cllsCnf gui
    setLabel (labelInfo fieldIO) (pack $ plT (rPl currentPlayer) ++ " move") --Переделать это окно на красиво богато
    let btnLst = fieldBtns fieldIO
    newButtonState b' currentPlayer
    writeIORef pla (rPl currentPlayer)
    checkWinRAWSimple currentPlayer block (rowCnt fieldIO) btnLst  >>=
      \case
        Win -> endGameScreen gui (Just fieldIO) Nothing currentPlayer Win
        Draw -> endGameScreen gui (Just fieldIO) Nothing NaP Draw
        Game -> return ()


hardCellPVE :: MainGUI -> IORef [HardField] -> ButtonData -> IORef Player -> Ref Button -> IO ()
hardCellPVE gui allFieldIO btnData pl b' = do
  stateB <- getLabel b'
  when(stateB == "") $ do
    currentPlayer <- readIORef pl
    let currentField = fieldN btnData
    newButtonState b' currentPlayer
    allField <- readIORef allFieldIO
    currentSmallFieldState <- checkWinRAWSimple currentPlayer 3 3 (field $ allField !! currentField)
    switchHardFieldsState allField allFieldIO btnData currentSmallFieldState
    updateHardFieldData allFieldIO allField currentPlayer currentField currentSmallFieldState >>=
      \case
        Win -> endGameScreen gui Nothing (Just allFieldIO) currentPlayer Win
        Draw -> endGameScreen gui Nothing (Just allFieldIO) NaP Draw
        Game -> do --Добавить проверку доступности поля 
         let botPlayer = rPl currentPlayer
         allField <- readIORef allFieldIO
         (fieldB, xB, yB)  <- callForHardBotRandom (refactorHardField allField) botPlayer
         newButtonState (field (allField !! fieldB) !! (xB * 3 + yB)) botPlayer
         currentSmallFieldState <- checkWinRAWSimple botPlayer 3 3 (field $ allField !! fieldB)
         switchHardFieldsState allField allFieldIO (BD{fieldN = fieldB, btnN =xB * 3 + yB}) currentSmallFieldState
         updateHardFieldData allFieldIO allField botPlayer fieldB currentSmallFieldState >>=
          \case
            Win -> endGameScreen gui Nothing (Just allFieldIO) botPlayer Win
            Draw -> endGameScreen gui Nothing (Just allFieldIO) NaP Draw
            Game -> return ()


hardCellPVP :: MainGUI -> IORef [HardField] -> ButtonData -> IORef Player -> Ref Button -> IO ()
hardCellPVP gui allFieldIO btnData pl b' = do
  stateB <- getLabel b'
  when(stateB == "") $ do
    currentPlayer <- readIORef pl
    let currentField = fieldN btnData
    newButtonState b' currentPlayer
    writeIORef pl (rPl currentPlayer)
    allField <- readIORef allFieldIO
    currentSmallFieldState <- checkWinRAWSimple currentPlayer 3 3 (field $ allField !! currentField)

    switchHardFieldsState allField allFieldIO btnData currentSmallFieldState

    updateHardFieldData allFieldIO allField currentPlayer currentField currentSmallFieldState >>=
      \case
        Win -> endGameScreen gui Nothing (Just allFieldIO) currentPlayer Win
        Draw -> endGameScreen gui Nothing (Just allFieldIO) NaP Draw
        Game -> return ()


runHardMode :: MainGUI -> Text -> (MainGUI -> IORef [HardField] -> ButtonData -> IORef Player -> Ref Button -> IO ()) -> IO ()
runHardMode gui windowName gameMode = do
  hide $ packs gui
  setLabel (mainWindow gui) windowName
  begin $ mainWindow gui

  mainframe <- groupNew (toRectangle (0,0,width $ windCnf gui, height $ windCnf gui)) Nothing
  begin mainframe
  _ <- createHardCells gui gameMode
  exitButton gui mainframe

  end mainframe
  end $ mainWindow gui


runSimpleMode :: MainGUI -> Text -> (MainGUI -> SimpleField -> IORef Player -> Ref Button -> IO ()) -> IO ()
runSimpleMode gui windowName gameMode = do
  hide $ packs gui
  setLabel (mainWindow gui) windowName
  begin $ mainWindow gui

  mainframe <- groupNew (toRectangle (0,0,width $ windCnf gui, height $ windCnf gui)) Nothing
  begin mainframe
  infoLabel  <- newLabel (width (windCnf gui) `div` 4) 10 (width (windCnf gui) `div` 5) 70 (Just "Hello human") --Переделать это окно на красиво богато
  _ <- createGameCells mainframe gui infoLabel gameMode
  exitButton gui mainframe

  end mainframe
  end $ mainWindow gui


createMainMenu :: WindowConfig -> Int -> IO ()
createMainMenu windowC cellCountIN = do
  imgs <- readAssetsImages images
  cellsToWin <- newIORef (cellToWinCoef !! cellCountIN)
  cellsCount <- newIORef cellCountIN
  --MAINMENUCONTS не трогать
  let bigButtonWidth = 300
  let bigButtonHeight = 100
  --MAINMENUCONTS

  window <- windowNew
          (Size (Width $ width windowC) (Height $ height windowC))
          Nothing
          (Just "Main Menu")
  when (fullscreen windowC) $ do makeFullscreen window
  begin window
  backLayout    <- newLabel 0 0 (width windowC) (height windowC) Nothing
  mainframe <- groupNew (toRectangle (0,0,width windowC, height windowC)) Nothing

  let mainWindow = MG
                  {
                    windCnf = windowC,
                    cllsCnf = CC
                      {
                        cellSize = 50,
                        cntInRow = cellsCount,
                        cellToWin = cellsToWin
                      },
                    mainWindow = window,
                    packs = mainframe
                  }

  begin mainframe
  settingButton <- newButton (width windowC-20) 0 20 20 Nothing
  simplePVPMode <- newButton ((width windowC - bigButtonWidth) `div` 2) (height windowC `div` 6) bigButtonWidth bigButtonHeight Nothing
  simplePVEMode <- newButton ((width windowC - bigButtonWidth) `div` 2) (height windowC `div` 6 + bigButtonHeight + 5) bigButtonWidth bigButtonHeight Nothing
  hardPVPMode   <- newButton ((width windowC - bigButtonWidth) `div` 2) (height windowC `div` 6 + 2 * bigButtonHeight + 10) bigButtonWidth bigButtonHeight Nothing
  hardPVEMode   <- newButton ((width windowC - bigButtonWidth) `div` 2) (height windowC `div` 6 + 3 * bigButtonHeight + 15) bigButtonWidth bigButtonHeight Nothing
  end mainframe
  end window

  setImage simplePVPMode (Just (head imgs))
  setImage simplePVEMode (Just (imgs !! 1))
  setImage hardPVPMode   (Just (imgs !! 2))
  setImage hardPVEMode   (Just (imgs !! 3))
  setImage backLayout    (Just (imgs !! 4)) --Доработать задний фон

  rgbColorWithRgb backGroundColor >>= setColor window

  setCallback simplePVPMode (\_ -> runSimpleMode mainWindow "Simple PvP" simpleCellPVP)
  setCallback simplePVEMode (\_ -> runSimpleMode mainWindow "Simple PvE" simpleCellPVE)
  setCallback hardPVPMode   (\_ -> runHardMode   mainWindow "Hard PvP"   hardCellPVP)
  setCallback hardPVEMode   (\_ -> runHardMode   mainWindow "Hard PvE"   hardCellPVE)
  setCallback settingButton (\_ -> settingsScreen mainWindow cellsToWin cellsCount createMainMenu)

  mainMenu mainWindow
  FL.run
  FL.flush