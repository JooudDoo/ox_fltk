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
      hide frame
      mainMenu gui


--Исправить костыль с лишним кодом | Смена порядка игроков (Работает криво) | Сделать смену порядка возможной
gameCellPVE :: MainGUI -> SimpleField -> IORef Player -> Ref Button -> IO ()
gameCellPVE gui fieldIO pla b' = do
  state <- getLabel b'
  when (state == "") $ do
    let btnLst = fieldBtns fieldIO
    let inRow = rowCnt fieldIO
    currentPlayer <- readIORef pla
    block <- readIORef $ cellToWin $ cllsCnf gui
    setLabel (labelInfo fieldIO) (pack $ plT (rPl currentPlayer) ++ " bot move") --Переделать это окно на красиво богато
    newButtonState b' pla
    checkWinSimple currentPlayer block inRow btnLst >>=
      \case
        Win -> endGameScreen gui (Just fieldIO) Nothing currentPlayer Win
        Draw -> endGameScreen gui (Just fieldIO) Nothing NaP Draw
        Game -> do
          botPlayer <- readIORef pla
          setLabel (labelInfo fieldIO) (pack $ plT (rPl botPlayer) ++ " human move") --Переделать это окно на красиво богато
          (btx, bty) <- readCells btnLst inRow >>= \f -> callForBotRandom f botPlayer
          newButtonState (btnLst !! (btx * inRow + bty)) pla
          checkWinSimple botPlayer block inRow btnLst >>=
            \case
              Win -> endGameScreen gui (Just fieldIO) Nothing botPlayer Win
              Draw -> endGameScreen gui (Just fieldIO) Nothing NaP Draw
              Game -> return ()


gameCellPVP :: MainGUI -> SimpleField -> IORef Player -> Ref Button -> IO ()
gameCellPVP gui fieldIO pla b' = do
  state <- getLabel b'
  when (state == "") $ do
    currentPlayer <- readIORef pla
    block <- readIORef $ cellToWin $ cllsCnf gui
    setLabel (labelInfo fieldIO) (pack $ plT (rPl currentPlayer) ++ " move") --Переделать это окно на красиво богато
    let btnLst = fieldBtns fieldIO
    newButtonState b' pla
    checkWinSimple currentPlayer block (rowCnt fieldIO) btnLst  >>=
      \case
        Win -> endGameScreen gui (Just fieldIO) Nothing currentPlayer Win
        Draw -> endGameScreen gui (Just fieldIO) Nothing NaP Draw
        Game -> return ()


hardCellPVP :: MainGUI -> IORef [HardField] -> ButtonData -> IORef Player -> Ref Button -> IO ()
hardCellPVP gui allFieldIO btnData pl b' = do
  stateB <- getLabel b'
  currentPlayer <- readIORef pl
  when(stateB == "") $ do
    let currentField = fieldN btnData
    let currentBtn = btnN btnData
    newButtonState b' pl

    allField <- readIORef allFieldIO
    smallField <- checkWinSimple currentPlayer 3 3 (field $ allField !! currentField)

    if (state (allField !! currentBtn) /= Game) || (smallField /= Game && currentBtn == currentField)
      then
        mapM_ (activateField . field) allField
      else do
        mapM_ (deactivateField .field. fst) (filter (\(_,s) -> s /= currentBtn) (zip allField [0..]))
        activateField (field $ allField !! currentBtn)

    when (smallField /= Game && state (allField !! currentField) == Game) $ do
          writeIORef allFieldIO (changeInList allField (HF {field = field $ allField !! currentField, state = smallField, player = currentPlayer}) currentField)
          changeButtonBlockColor (field $ allField !! currentField) (checkTypeOfGame smallField currentPlayer)
          gameState <- checkWinHard currentPlayer allFieldIO
          case gameState of
            Win -> endGameScreen gui Nothing (Just allFieldIO) currentPlayer Win
            Draw -> endGameScreen gui Nothing (Just allFieldIO) NaP Draw
            Game -> return ()
   where
     checkTypeOfGame x y
      | x == Draw = NaP
      | otherwise = y


runHardXOPVP :: MainGUI -> IO ()
runHardXOPVP gui = do
  setLabel (mainWindow gui) "Hard XO PVP"
  begin $ mainWindow gui

  mainframe <- groupNew (toRectangle (0,0,width $ windCnf gui, height $ windCnf gui)) Nothing
  begin mainframe
  _ <- createHardCells gui hardCellPVP
  exitButton gui mainframe

  end mainframe
  end $ mainWindow gui


--Обьединить функции запуска простых режимов в один
runSimpleXOPVE :: MainGUI -> IO ()
runSimpleXOPVE gui = do
  setLabel (mainWindow gui) "Simple XO PVE"
  begin $ mainWindow gui

  mainframe <- groupNew (toRectangle (0,0,width $ windCnf gui, height $ windCnf gui)) Nothing
  begin mainframe
  infoLabel  <- newLabel (width (windCnf gui) `div` 4) 10 (width (windCnf gui) `div` 5) 70 (Just "Hello human") --Переделать это окно на красиво богато
  _ <- createGameCells mainframe gui infoLabel gameCellPVE
  exitButton gui mainframe

  end mainframe
  end $ mainWindow gui


runSimpleXOPVP :: MainGUI -> IO ()
runSimpleXOPVP gui = do
  setLabel (mainWindow gui) "Simple XO PVP"
  begin $ mainWindow gui

  mainframe <- groupNew (toRectangle (0,0,width $ windCnf gui, height $ windCnf gui)) Nothing
  begin mainframe
  infoLabel  <- newLabel (width (windCnf gui) `div` 4) 10 (width (windCnf gui) `div` 5) 70 (Just "Hello human") --Переделать это окно на красиво богато
  _ <- createGameCells mainframe gui infoLabel gameCellPVP
  exitButton gui mainframe

  end mainframe
  end $ mainWindow gui


startGameMode :: MainGUI -> (MainGUI -> IO ()) -> Ref a -> IO ()
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


createMainMenu :: WindowConfig -> IO ()
createMainMenu windowC = do
  imgs <- readAssetsImages images
  cellsToWin <- newIORef 3
  cellsCount <- newIORef 3
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
  simplePVPMode <- newButton ((width windowC - bigButtonWidth) `div` 2) (height windowC `div` 4) bigButtonWidth bigButtonHeight Nothing
  simplePVEMode <- newButton ((width windowC - bigButtonWidth) `div` 2) (height windowC `div` 4 + bigButtonHeight + 5) bigButtonWidth bigButtonHeight Nothing
  hardPVPMode   <- newButton ((width windowC - bigButtonWidth) `div` 2) (height windowC `div` 4 + 2 * bigButtonHeight + 10) bigButtonWidth bigButtonHeight Nothing
  end mainframe
  end window

  setImage simplePVPMode (Just (head imgs))
  setImage simplePVEMode (Just (imgs !! 1))
  setImage hardPVPMode   (Just (imgs !! 2))
  setImage backLayout    (Just (imgs !! 3)) --Доработать задний фон

  rgbColorWithRgb backGroundColor >>= setColor window

  setCallback simplePVPMode (startGameMode mainWindow runSimpleXOPVP)
  setCallback simplePVEMode (startGameMode mainWindow runSimpleXOPVE)
  setCallback hardPVPMode (startGameMode mainWindow runHardXOPVP)
  setCallback settingButton (\_ -> settingsScreen mainWindow cellsToWin cellsCount createMainMenu)

  mainMenu mainWindow
  FL.run
  FL.flush