{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module InterfaceLib where

import AdditionLib

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Data.Maybe
import Data.IORef
import System.IO.Unsafe
import Control.Monad
import qualified Data.ByteString as B
import Data.Text (pack, Text)

--Дополнительные функции для интерефейса тут

data WindowConfig =
    WC
    {
        width :: Int,
        height :: Int,
        fullscreen :: Bool
    }
data CellsConfig =
    CC
    {
        cellSize :: Int,
        cntInRow :: IORef Int,
        cellToWin :: IORef Int
    }
data MainGUI =
    MG
    {
      cllsCnf :: CellsConfig,
      windCnf :: WindowConfig,
      mainWindow :: Ref Window,
      packs :: Ref Group
    }
data HardField =
    HF
    {
      field ::[Ref Button],
      state :: GameState,
      player :: Player
    }
data HardPlayers =
    HP
    {
      fieldP ::[[Player]],
      stateP :: GameState,
      playerP :: Player
    }
data SimpleField =
    SF
    {
      fieldBtns :: [Ref Button],
      labelInfo :: Ref Box,
      rowCnt :: Int,
      group :: Ref Group
    }

type FieldNumber = Int
type ButtonNumber = Int
data ButtonData =
    BD
    {
      fieldN :: FieldNumber,
      btnN :: ButtonNumber
    }
data SettingsData =
    SD
    {
      cellsToWin :: IORef Int,
      cellsInField :: IORef Int
    }



defaultColor :: RGB
defaultColor = (0,0,0)
crossColor :: RGB
crossColor = (155,17,30)
zeroColor :: RGB
zeroColor = (14,19,236)
crossColor' :: RGB
crossColor' = (214,120,130)
zeroColor' :: RGB
zeroColor' = (98,101,224)
backGroundColor :: RGB
backGroundColor = (47, 110, 147)


cellToWinCoef :: [Int]
cellToWinCoef = [0,0,0,3,0,4,0,4,0,5]


plToColorFont :: Player -> RGB
plToColorFont Cross = crossColor
plToColorFont Zero = zeroColor
plToColorFont _ = defaultColor
plToColorBack :: Player -> RGB
plToColorBack Cross = crossColor'
plToColorBack Zero = zeroColor'
plToColorBack _ = defaultColor


newButton :: Int -> Int -> Int -> Int -> Maybe Text -> IO (Ref Button)
newButton xPos yPos xSize ySize = buttonNew
            (Rectangle (Position (X xPos) (Y yPos)) (Size (Width xSize) (Height ySize)))


newLabel :: Int -> Int -> Int -> Int -> Maybe Text -> IO (Ref Box)
newLabel xPos yPos xSize ySize = boxNew
            (Rectangle (Position (X xPos) (Y yPos)) (Size (Width xSize) (Height ySize)))


newButtonState :: Ref Button -> Player -> IO ()
newButtonState b' pl = do
  let currentPlayer = pl
  --currentPlayer <- readIORef pl
  setLabel b' (pack $ plT currentPlayer)
  switchColorPlayer currentPlayer b'
  --writeIORef pl (rPl currentPlayer)


switchColorPlayer :: Player -> Ref Button -> IO ()
switchColorPlayer player widget =
  rgbColorWithRgb (plToColorFont player) >>= setLabelcolor widget


changeButtonBlockColor :: [Ref Button] -> Player -> IO ()
changeButtonBlockColor btns pl = mapM_ helper btns
  where
    helper :: Ref Button -> IO ()
    helper b' =
      rgbColorWithRgb (plToColorBack pl) >>= setColor b' >>
      rgbColorWithRgb (plToColorBack pl) >>= setDownColor b' >>
      hide b' >>
      showWidget b'


settingsScreen :: MainGUI -> IORef Int -> IORef Int -> (WindowConfig -> Int -> IO()) -> IO ()
settingsScreen gui cellsToWin cellsCount createWin = do
     let winWidth  = width (windCnf gui) `div` 2
     let winHeight = height (windCnf gui) `div` 4 *3
     minCells <- newIORef 3
     maxCells <- newIORef 8
     cellTOWin <-readIORef cellsToWin
     cellCount <- readIORef cellsCount
     let textcntCells = "Count of cells in row: "
     let textcntToWinCells = "\nCount of cells in row to win: "

     win <- overlayWindowNew (Size (Width winWidth) (Height winHeight))
                              Nothing
                              Nothing
                              winFunc
     setModal win
     clearBorder win
     showWidget win
     begin win

     applyButton   <- newButton (winWidth-40) (winHeight-20) 40 20 (Just "Apply")
     destroyButton <- newButton 0 (winHeight-20) 40 20 (Just "EXIT")
     exitButton    <- newButton (winWidth-20) 0 20 20 (Just "X")

     cntCells      <- newLabel  80 40 100 30 (Just $ pack $ textcntCells ++ show cellCount ++ textcntToWinCells ++ show cellTOWin)
     addCntCells   <- newButton 260 30 20 20 (Just "+")
     decCntCells   <- newButton 260 50 20 20 (Just "-")

     fullscreenButton <- roundButtonNew (toRectangle (30,90,100,20)) (Just "Fullscreen")
     setValue fullscreenButton (fullscreen $ windCnf gui)
     setCallback applyButton (applySettings Nothing Nothing fullscreenButton win)
     when interfaceDebugging $
      unless (fullscreen $ windCnf gui) $ do
          winWidthSlider <- horSliderNew (toRectangle (130, 90, 100, 30)) (Just "Width")
          bounds winWidthSlider 600 1600
          setValue winWidthSlider (fromIntegral $ width (windCnf gui))
          winheightSlider <- horSliderNew (toRectangle (130, 140, 100, 30)) (Just "Height")
          bounds winheightSlider 500 1000
          setValue winheightSlider (fromIntegral $ height (windCnf gui))
          setCallback applyButton (applySettings (Just winWidthSlider) (Just winheightSlider) fullscreenButton win)

     setLabelsize decCntCells (FontSize 10)
     setLabelsize addCntCells (FontSize 10)
     setLabelsize cntCells (FontSize 16)

     setCallback decCntCells (decCells (textcntCells : [textcntToWinCells]) minCells cellsCount cntCells)
     setCallback addCntCells (addCells (textcntCells : [textcntToWinCells]) maxCells cellsCount cntCells)
     setCallback destroyButton (destroyApp win)

     setCallback exitButton (closeWin win)
     end win
     return ()

     where
      winFunc ::Ref OverlayWindow -> IO ()
      winFunc = hide
      closeWin :: Ref OverlayWindow -> Ref Button -> IO ()
      closeWin win b' = destroy win
      applySettings :: Maybe (Ref HorSlider) -> Maybe (Ref HorSlider) -> Ref RoundButton -> Ref OverlayWindow -> Ref Button -> IO ()
      applySettings sliderX sliderY fullB win _ = do
        destroy win
        destroy (mainWindow gui)
        cellsWinCnt <-readIORef (cntInRow $ cllsCnf gui)
        fullS <- getValue fullB
        if fullS then do
          x1 <- FL.x
          y1 <- FL.y
          newX <- FL.w
          newY <- FL.h
          createWin (WC {width=newX+x1, height=newY+y1, fullscreen = fullS}) cellsWinCnt
        else
          if isNothing sliderX
            then
              createWin (WC {width=800, height=600, fullscreen = False}) cellsWinCnt
            else do
              x <- getValue (fromJust sliderX)
              y <- getValue (fromJust sliderY)
              createWin (WC {width=floor x, height=floor y, fullscreen = False})  cellsWinCnt
        return ()
      destroyApp :: Ref OverlayWindow -> Ref Button -> IO ()
      destroyApp winX _ = do
        destroy winX
        destroy (mainWindow gui)
      decCells :: [String] -> IORef Int -> IORef Int -> Ref Box -> Ref Button -> IO ()
      decCells txt min cnt label _ = do
        cnt' <- readIORef cnt
        minC <- readIORef min
        when (cnt' > minC) $ do
          writeIORef cnt (cnt'-2)
          writeIORef cellsToWin (cellToWinCoef !! (cnt'-2))
          setLabel label (pack $ head txt ++ show (cnt'-2) ++ txt !! 1++ show (cellToWinCoef !! (cnt'-2)))
          updateWid label
      addCells :: [String] -> IORef Int -> IORef Int -> Ref Box -> Ref Button -> IO ()
      addCells txt max cnt label _ = do
        cnt' <- readIORef cnt
        maxC <- readIORef max
        when (cnt' < maxC) $ do
          modifyIORef cnt (+2)
          writeIORef cellsToWin (cellToWinCoef !! (cnt'+2))
          setLabel label (pack $ head txt ++ show (cnt'+2) ++ txt !! 1 ++ show (cellToWinCoef !! (cnt'+2)))
          updateWid label
      updateWid :: Ref Box -> IO ()
      updateWid widg = hide widg >> showWidget widg


switchHardFieldsState :: [HardField] -> ButtonData -> GameState -> IO ()
switchHardFieldsState allField btnD gstate
    | state (allField !! currentBtn) /= Game || gstate /= Game && currentBtn == currentField =
                  mapM_ (activateField . field) allField
    | otherwise = do
                  mapM_ (deactivateField .field. fst) (filter (\(_,s) -> s /= currentBtn) (zip allField [0..]))
                  activateField (field $ allField !! currentBtn)
    where
      currentField = fieldN btnD
      currentBtn = btnN btnD


updateHardFieldData :: IORef [HardField] -> [HardField] -> Player -> FieldNumber -> GameState -> IO GameState
updateHardFieldData allFieldIO allField currentPlayer currentField currentSmallFieldState
  | currentSmallFieldState /= Game && state (allField !! currentField) == Game = do
      writeIORef allFieldIO (changeInList allField (HF {field = field $ allField !! currentField, state = currentSmallFieldState, player = currentPlayer}) currentField)
      changeButtonBlockColor (field $ allField !! currentField) (checkTypeOfGame currentSmallFieldState currentPlayer)
      checkWinRAWHard currentPlayer allFieldIO
  | otherwise = return Game
    where
     checkTypeOfGame x y
      | x == Draw = NaP
      | otherwise = y


deactivateField :: [Ref Button] -> IO ()
deactivateField = mapM_ deactivate


activateField :: [Ref Button] -> IO ()
activateField = mapM_ activate


readCells :: [Ref Button] -> Int -> IO [[Player]]
readCells cellList inRow = do
  fieldIO <- newIORef ([] :: [Player])
  mapM_ (getLabel >=> (\d -> modifyIORef fieldIO (++ [pl d]))) cellList
  readIORef fieldIO >>= \s -> return (refactorList s inRow)


mainMenu :: MainGUI -> IO ()
mainMenu gui = do
  setLabel (mainWindow gui) "Main Menu"
  begin $ mainWindow gui
  showWidget $ packs gui
  showWidget $ mainWindow gui
  return ()


--TODO 
--Отдельным блоком ✓
--Возможность контролировать весь интерфейс ✓ 
--Красиво и нарядно (30%)
--Обьединить блоки и сделать их универсальными ✓
endGameScreen :: MainGUI -> Maybe SimpleField -> Maybe (IORef [HardField]) -> Player -> GameState -> IO ()
endGameScreen gui simplField hrdField player gstate = do
  when interfaceDebugging $ print ("State: " ++ gSt gstate ++ " " ++ plT player)
  overlayScreen gui player gstate
  where
      overlayScreen :: MainGUI -> Player -> GameState -> IO ()
      overlayScreen gui player gstate = do
        win <- overlayWindowNew (Size (Width $ width (windCnf gui) `div` 2) (Height $ height (windCnf gui) `div` 4))
                                  Nothing
                                  Nothing
                                  winFunc
        setModal win
        clearBorder win
        begin win
        let playerCur | player == Cross = "Human"
                      | player == Zero = "Bot?"
                      | otherwise  = "?????"
        let text   | gstate == Draw = "DRAW"
                   | otherwise = "Winner is " ++ playerCur ++ " (" ++plT player ++ ")"
        infoLabel  <- newLabel ((width (windCnf gui) `div` 2 - width (windCnf gui) `div` 3) `div` 2)
                                10
                                (width (windCnf gui) `div` 3)
                                (height (windCnf gui) `div` 10)
                                (Just $ pack text) --Переделать это окно на красиво богато
        let btnCountDown = 2
        let buttonSizeDown = (width (windCnf gui) `div` 2) `div` btnCountDown -10
        let padButtonsDown = ((width (windCnf gui) `div` 2) - buttonSizeDown*btnCountDown) `div` 2
        exitButton <- newButton
                      padButtonsDown
                      (height (windCnf gui) `div` 4 - height (windCnf gui) `div` 16)
                      buttonSizeDown
                      (height (windCnf gui) `div` 16)
                      (Just "Main menu")

        pushBtn <- newButton
                    (padButtonsDown + buttonSizeDown + 5)
                    (height (windCnf gui) `div` 4 - height (windCnf gui) `div` 16)
                    buttonSizeDown
                    (height (windCnf gui) `div` 16)
                    (Just "Again")
        
        hideButton  <- boxCustom
                       (Rectangle (Position (X 0) (Y 0)) (Size (Width 20) (Height 20)))
                       (Just "O")
                       Nothing
                       (Just (defaultCustomWidgetFuncs {
                         handleCustom = Just $ windownHandel win
                       }))

        setCallback exitButton (\_ -> exitButtonFunc gui win)
        setCallback pushBtn    (\_ -> btnFunc win)
        --setCallback hideButton (hideButtonFnc win)
        showWidget win
        end win
      exitButtonFunc :: MainGUI -> Ref OverlayWindow -> IO ()
      exitButtonFunc gui win = do
        btnFunc win
        hide (group $ fromJust simplField) --Оптимизировать удаление детей
        mainMenu gui
      windownHandel :: Ref OverlayWindow -> Ref Box -> Event -> IO (Either UnknownEvent ())
      windownHandel win x e =
        case e of
          Release -> do
            resize win (toRectangle (0,0 ,width (windCnf gui) `div` 2, height (windCnf gui) `div` 4))
            return (Right ())
          Push -> do
            resize win (toRectangle (0,0,0,0))
            return (Right ())
          _ -> return (Left UnknownEvent)
      hideButtonFnc :: Ref OverlayWindow -> Ref Button  -> IO ()
      hideButtonFnc win b' =
        hide win
      btnFunc :: Ref OverlayWindow -> IO ()
      btnFunc win = do
        when (isJust simplField) (cleanAllCells (fieldBtns (fromJust simplField)))
        forM_ hrdField cleanHardField
        destroy win
      winFunc :: Ref OverlayWindow -> IO ()
      winFunc win = destroy win


cleanAllCells :: [Ref Button] -> IO ()
cleanAllCells = mapM_ helper
  where
    helper s = setLabel s "" >>= \m -> switchColorPlayer NaP s


cleanHardField :: IORef [HardField] -> IO ()
cleanHardField fieldIO = do
  fields <- readIORef fieldIO
  writeIORef fieldIO ([] :: [HardField])
  forM_ [0..8] $ \i -> do
    mapM_ helper (field (fields !! i))
    modifyIORef fieldIO (++[HF{field = field (fields !! i), state = Game, player = NaP}])
  where
    helper :: Ref Button -> IO ()
    helper s = setLabel s "" >>
     (setColor s backgroundColor >>
     (setDownColor s backgroundColor >>
     activate s))


refactorHardField :: [HardField] -> [HardPlayers]
refactorHardField = map (\s -> HP {fieldP = unsafePerformIO (readCells (field s) 3), stateP = state s,playerP = player s})

checkDrawSimple :: [[Player]] -> GameState
checkDrawSimple field
  | NaP `notElem` mergeList field= Draw
  | otherwise = Game


checkDrawHard :: [GameState] -> GameState
checkDrawHard field
  | not (any (/= Game) field) = Draw
  | otherwise  = Game


checkWinRAWSimple :: Player -> Int -> Int -> [Ref Button] -> IO GameState
checkWinRAWSimple player block row btnLst =
  readCells btnLst row >>= \s -> return $ checkWinSimple player block row s


checkWinRAWHard :: Player -> IORef [HardField] -> IO GameState
checkWinRAWHard playerCur fieldIO = do
   field <- readIORef fieldIO
   fieldBigIO <- newIORef ([] :: [Player])
   forM_ [0..length field -1] $ \i ->
     if state (field !! i) == Win
       then
         modifyIORef fieldBigIO (++[player (field !! i)])
        else
         modifyIORef fieldBigIO (++[NaP])
   fieldNIO <-readIORef fieldBigIO
   return $ checkWinHard playerCur (refactorHardField field)


checkWinSimple :: Player -> Int -> Int -> [[Player]] ->  GameState
checkWinSimple player block row field = do
   let playerIsWin = checkWinPlCustom field row block player
   gState playerIsWin (checkDrawSimple field)


checkWinHard :: Player -> [HardPlayers] -> GameState
checkWinHard playerCur fieldDATA = do
   let fieldDATAPlayers = refactorList (map playerP fieldDATA) 3
   let fieldDATAStates = map  stateP fieldDATA
   let playerIsWin = checkWinPlCustom fieldDATAPlayers 3 3 playerCur
   gState Game (checkDrawHard fieldDATAStates)


checkWinPlCustom :: (Eq a) => [[a]] -> Int -> Int -> a -> GameState
checkWinPlCustom pole inRowIn block player
          | checkSquares 0 = Win
          | checkDiags 0 = Win
          | otherwise = Game
  where
    checkSquares cnt
        | cnt == inRowIn = False
        | checkR' 1 0 0 cnt 0 0 = True
        | checkR' 0 cnt 1 0 0 0 = True
        | otherwise = checkSquares (cnt+1)
    checkDiags offset
        | offset == inRowIn-block+1 = False
        | checkD' offset 0 0 = True
        | checkD'' offset 0 0 = True
        | checkD''' offset 0 0 = True
        | checkD'''' offset 0 0 = True
        | otherwise = checkDiags (offset+1)
    checkD' :: Int -> Int -> Int -> Bool
    checkD' offset y cnt
            | cnt == block = True
            | y > inRowIn-offset-1 = False
            | pole !! y !! x1 == player = checkD' offset (y+1) (cnt+1)
            | otherwise = checkD' offset (y+1) 0
      where
        x1 = y+ offset
    checkD'' :: Int -> Int -> Int -> Bool
    checkD'' offset y cnt
            | cnt == block = True
            | y > inRowIn-offset-1 = False
            | pole !! x1 !! y == player = checkD'' offset (y+1) (cnt+1)
            | otherwise = checkD'' offset (y+1) 0
      where
        x1 = y+ offset
    checkD''' :: Int -> Int -> Int -> Bool
    checkD''' offset y cnt
            | cnt == block = True
            | y > inRowIn-offset-1 = False
            | pole !! y !! x2 == player = checkD''' offset (y+1) (cnt+1)
            | otherwise = checkD''' offset (y+1) 0
      where
        x2 = inRowIn-1-y-offset
    checkD'''' :: Int -> Int -> Int -> Bool
    checkD'''' offset y cnt
            | cnt == block = True
            | y > inRowIn-offset-1 = False
            | pole !! (y+offset) !! (x2+offset) == player = checkD'''' offset (y+1) (cnt+1)
            | otherwise = checkD'''' offset (y+1) 0
      where
        x2 = inRowIn-1-y-offset
    checkR' :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
    checkR' xC offX yC offY cnt toWin
            | toWin == block = True
            | cnt == inRowIn = False
            | pole !! x !! y == player = checkR' xC offX yC offY (cnt+1) (toWin+1)
            | otherwise = checkR' xC offX yC offY (cnt+1) 0
      where
          x = cnt * xC + offX
          y = cnt * yC + offY


createGameCells ::Ref Group -> MainGUI -> Ref Box -> (MainGUI -> SimpleField -> IORef Player -> Ref Button -> IO ()) -> IO [Ref Button]
createGameCells frame gui lb func = do
 lstButtonsIO <- newIORef ([] :: [Ref Button])
 inRow <- readIORef $ cntInRow $ cllsCnf gui
 player <- newIORef Cross
 let padX = (windowWidth - buttonSize inRow * inRow) `div` 2
 let padY = (windowHeight - buttonSize inRow * inRow) `div` 2
 forM_ [0..inRow*inRow-1] $ \i -> do
    button <- newButton (i `mod` inRow*buttonSize inRow + padX) (i `div` inRow*buttonSize inRow + padY) (buttonSize inRow) (buttonSize inRow) (Just "")
    setLabelsize button (FontSize (fromIntegral $ buttonSize inRow`div`2))
    modifyIORef lstButtonsIO (++ [button])
 lstButtons <- readIORef lstButtonsIO
 mapM_ (\s -> setCallback s (func gui (SF {fieldBtns = lstButtons,labelInfo = lb, group = frame, rowCnt = inRow}) player)) lstButtons
 return lstButtons
 where
   buttonSize r = (windowHeight -200) `div` 3 *2 `div` r
   windowWidth = width $ windCnf gui
   windowHeight = height $ windCnf gui


createHardCells :: MainGUI -> (MainGUI -> IORef [HardField] -> ButtonData -> IORef Player -> Ref Button -> IO ()) -> IO ()
createHardCells gui func = do
  playerTurn <- newIORef (Cross :: Player)
  fieldX <- newIORef ([] :: [[Ref Button]])
  forM_ [1..9] $ \i -> do
    cache <- createHardCellsField gui i
    modifyIORef fieldX (++[cache])
  fl <- readIORef fieldX
  field <- newIORef (writeHardField fl [] :: [HardField])
  forM_ [0..8] $ \i ->
    updateHardCellsFunc field (fl !! i) func i playerTurn gui


writeHardField :: [[Ref Button]] -> [HardField] -> [HardField]
writeHardField btns res
  | not $ null btns = writeHardField (tail btns) (res ++ [HF {field = head btns, state = Game, player = NaP}])
  | otherwise = res


createHardCellsField :: MainGUI -> FieldNumber -> IO [Ref Button]
createHardCellsField gui field = do
  lstButtonsIO <- newIORef ([] :: [Ref Button])
  forM_ [0..2] $ \i ->
    forM_ [0..2] $ \d -> do
      b' <- newButton (padX d) (padY i) buttonSize buttonSize (Just "")
      setLabelsize b' (FontSize (fromIntegral $ buttonSize `div`2))
      modifyIORef lstButtonsIO (++[b'])
  readIORef lstButtonsIO
  where
    buttonSize = (heightW -151) `div` 3 *2 `div` 9
    widthW = width $ windCnf gui
    heightW = height $ windCnf gui
    padX i = winPadX + (field-1)`mod`3 * 3 * buttonSize + buttonSize * i + 10 * ((field-1) `mod` 3)
    padY i= winPadY + (field-1)`div`3 * 3 * buttonSize + buttonSize * i + 10 * ((field-1)`div`3)
    winPadX = (widthW  - buttonSize * 9) `div` 2
    winPadY = (heightW - buttonSize * 9) `div` 2


updateHardCellsFunc :: IORef [HardField] -> [Ref Button] -> (MainGUI -> IORef [HardField] -> ButtonData -> IORef Player -> Ref Button -> IO ()) -> FieldNumber -> IORef Player -> MainGUI -> IO ()
updateHardCellsFunc field btns func num pl gui =
  forM_ [0..8] $ \i ->
    setCallback (btns !! i) (func gui field (BD {fieldN = num, btnN = i}) pl)