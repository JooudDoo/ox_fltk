{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module InterfaceLib where

import AdditionLib

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Data.Maybe
import Data.IORef
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


newButtonState :: Ref Button -> IORef Player -> IO ()
newButtonState b' pl = do
  currentPlayer <- readIORef pl
  setLabel b' (pack $ plT currentPlayer)
  switchColorPlayer currentPlayer b'
  writeIORef pl (rPl currentPlayer)


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


settingsScreen :: MainGUI -> IORef Int -> IORef Int -> (WindowConfig -> IO()) -> IO ()
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

     cntCells    <- newLabel  80 40 100 30 (Just $ pack $ textcntCells ++ show cellCount ++ textcntToWinCells ++ show cellTOWin)
     addCntCells <- newButton 260 30 20 20 (Just "+")
     decCntCells <- newButton 260 50 20 20 (Just "-")
     fullscreenButton <- roundButtonNew (toRectangle (80,90,100,20)) (Just "Fullscreen")
     setValue fullscreenButton (fullscreen $ windCnf gui)
     applyButton <- newButton (winWidth-40) (winHeight-20) 40 20 (Just "Apply")
     destroyButton <- newButton 0 (winHeight-20) 40 20 (Just "EXIT")
--Сделать режим полного окна
     setCallback applyButton (applySettings Nothing Nothing fullscreenButton win)
     unless (fullscreen $ windCnf gui) $ do
        winWidthSlider <- horSliderNew (toRectangle (80, 120, 100, 30)) (Just "Width")
        bounds winWidthSlider 600 1600
        setValue winWidthSlider (fromIntegral $ width (windCnf gui))
        winheightSlider <- horSliderNew (toRectangle (80, 180, 100, 30)) (Just "Height")
        bounds winheightSlider 500 1000
        setValue winheightSlider (fromIntegral $ height (windCnf gui))
        setCallback applyButton (applySettings (Just winWidthSlider) (Just winheightSlider) fullscreenButton win)
      
    
     setLabelsize decCntCells (FontSize 10)
     setLabelsize addCntCells (FontSize 10)
     setLabelsize cntCells (FontSize 16)

     setCallback decCntCells (decCells (textcntCells : [textcntToWinCells]) minCells cellsCount cntCells)
     setCallback addCntCells (addCells (textcntCells : [textcntToWinCells]) maxCells cellsCount cntCells)
     setCallback destroyButton (destroyApp win)

     exitButton <- newButton (winWidth-20) 0 20 20 (Just "X")
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
        fullS <- getValue fullB
        if fullS then do
          newX <- FL.w
          newY <- FL.h
          createWin (WC {width=newX, height=newY, fullscreen = fullS})
        else
          if (isNothing sliderX)
            then
              createWin (WC {width=1200, height=600, fullscreen = False})
            else do
              x <- getValue (fromJust sliderX)
              y <- getValue (fromJust sliderY)
              createWin (WC {width=floor x, height=floor y, fullscreen = False})
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


deactivateField :: [Ref Button] -> IO ()
deactivateField = mapM_ deactivate


activateField :: [Ref Button] -> IO ()
activateField = mapM_ activate


readCells :: [Ref Button] -> Int -> IO [[Player]]
readCells cellList inRow = do
  fieldIO <- newIORef ([] :: [Player])
  mapM_ (getLabel >=> (\d -> modifyIORef fieldIO (++ [pl d]))) cellList
  readIORef fieldIO >>= \s -> return (refactorList s inRow)

checkDraw :: [[Player]] -> GameState
checkDraw field
  | cntNaPs == 0 = Draw
  | otherwise = Game
  where
    cntNaPs = length $ filter (==NaP) $ mergeList field


--TODO 
--Отдельным блоком ✓
--Возможность контролировать весь интерфейс ✓ 
--Красиво и нарядно (30%)
--Обьединить блоки и сделать их универсальными ✓
endGameScreen :: MainGUI -> Maybe SimpleField -> Maybe (IORef [HardField]) -> Player -> GameState -> IO ()
endGameScreen gui simplField hrdField player gstate = do
  when debugging $ print ("State: " ++ gSt gstate ++ plT player)
  when (isJust simplField) (cleanAllCells (fieldBtns (fromJust simplField)))
  forM_ hrdField cleanHardField
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
        let text   | gstate == Draw = "DRAW"
                   | otherwise = "Winner is " ++ plT player
        infoLabel  <- newLabel ((width (windCnf gui) `div` 2 - width (windCnf gui) `div` 3) `div` 2)
                                10
                                (width (windCnf gui) `div` 3)
                                (height (windCnf gui) `div` 10)
                                (Just $ pack text) --Переделать это окно на красиво богато

        pushBtn <- newButton
                    ((width (windCnf gui) `div` 2 - width (windCnf gui) `div` 3) `div` 2)
                    (height (windCnf gui) `div` 4 - height (windCnf gui) `div` 16)
                    (width (windCnf gui) `div` 3)
                    (height (windCnf gui) `div` 16)
                    (Just "Повтор?")

        setCallback pushBtn (btnFunc win)
        showWidget win
        end win
      btnFunc :: Ref OverlayWindow -> Ref Button -> IO ()
      btnFunc win b' = destroy win
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


checkWinSimple :: Player -> Int -> Int -> [Ref Button] -> IO GameState
checkWinSimple player block row btnLst = do
   field <- readCells btnLst row
   playerIsWin <- checkWinPlCustom field row block player
   when debugging $ print field
   return $ gState playerIsWin (checkDraw field)


checkWinHard :: Player -> IORef [HardField] -> IO GameState
checkWinHard playerCur fieldIO = do
   field <- readIORef fieldIO
   fieldBigIO <- newIORef ([] :: [Player])
   forM_ [0..length field -1] $ \i ->
     if state (field !! i) == Win
       then
         modifyIORef fieldBigIO (++[player (field !! i)])
        else
         modifyIORef fieldBigIO (++[NaP])
   fieldBig <- readIORef fieldBigIO >>= \s -> return $ refactorList s 3
   playerIsWin <- checkWinPlCustom fieldBig 3 3 playerCur
   when debugging $ print $ "BIG" ++ show fieldBig
   return $ gState playerIsWin (checkDraw fieldBig)


--Больше тестов возможны ошибки
checkWinPlCustom :: (Eq a) => [[a]] -> Int -> Int -> a -> IO GameState
checkWinPlCustom pole inRowIn block player = do
  print block
  iSwin <- newIORef False
  forM_[0..inRowIn-1] (check 1 0 0 >=> (\ s -> modifyIORef iSwin (|| s)))
  forM_[0..inRowIn-1] $ \i -> check 0 i 1 0 >>= (\s -> modifyIORef iSwin (|| s))
  checkDiags >>= \s -> modifyIORef iSwin (|| s)
  iSWinFin <- readIORef iSwin
  if iSWinFin
    then
      return Win
    else
      return Game
  where
    check :: Int -> Int -> Int -> Int -> IO Bool
    check xC offX yC offY = do
      win <- newIORef False
      cnt <- newIORef 0
      forM_[0..inRowIn-1] $ \i -> do
        let x = i * xC + offX
        let y = i * yC + offY
        if pole !! x !! y == player
          then
            modifyIORef cnt (+1)
          else
            writeIORef cnt 0
        cntSym <- readIORef cnt
        when (cntSym == block) $ do
            writeIORef win True
            return ()
      readIORef win
    checkDiags :: IO Bool
    checkDiags = do
        win <- newIORef False
        cnt1 <- newIORef 0
        cnt2 <- newIORef 0
        cnt3 <- newIORef 0
        cnt4 <- newIORef 0
        forM_[0..inRowIn-block] $ \offset ->
          forM_[0..inRowIn-offset-1] $ \y -> do
          let x1 = y + offset
          let x2 = inRowIn-1-y-offset
          if pole !! y !! x1 == player
            then
              modifyIORef cnt1 (+1)
            else
              writeIORef cnt1 0
          if pole !! x1 !! y == player
            then
              modifyIORef cnt3 (+1)
            else
              writeIORef cnt3 0

          if pole !! y !! x2 == player
            then
              modifyIORef cnt2 (+1)
            else
              writeIORef cnt2 0

          if pole !! (y+offset) !! (x2+offset) == player
            then
              modifyIORef cnt4 (+1)
            else
              writeIORef cnt4 0
          cntSym1 <- readIORef cnt1
          cntSym2 <- readIORef cnt2
          cntSym3 <- readIORef cnt3
          cntSym4 <- readIORef cnt4

          when (cntSym1 == block || cntSym2 == block ||
                cntSym3 == block || cntSym4 == block) $ do
              writeIORef win True
              return ()
        readIORef win


createGameCells ::Ref Group -> MainGUI -> Ref Box -> (MainGUI -> SimpleField -> IORef Player -> Ref Button -> IO ()) -> IO [Ref Button]
createGameCells frame gui lb func = do
 lstButtonsIO <- newIORef ([] :: [Ref Button])
 inRow <- readIORef $ cntInRow $ cllsCnf gui
 player <- newIORef Cross
 let padX = (windowWidth - buttonSize * inRow) `div` 2
 let padY = (windowHeight - buttonSize * inRow) `div` 2
 forM_ [0..inRow*inRow-1] $ \i -> do
    button <- newButton (i `mod` inRow*buttonSize + padX) (i `div` inRow*buttonSize + padY) buttonSize buttonSize (Just "")
    setLabelsize button (FontSize (fromIntegral $ buttonSize`div`2))
    modifyIORef lstButtonsIO (++ [button])
 lstButtons <- readIORef lstButtonsIO
 mapM_ (\s -> setCallback s (func gui (SF {fieldBtns = lstButtons,labelInfo = lb, group = frame, rowCnt = inRow}) player)) lstButtons
 return lstButtons
 where
   buttonSize = cellSize $cllsCnf gui
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
      b' <- newButton (padX d) (padY i) (cellSize $ cllsCnf gui) (cellSize $ cllsCnf gui) (Just "")
      setLabelsize b' (FontSize (fromIntegral $ cellSize (cllsCnf gui) `div`2))
      modifyIORef lstButtonsIO (++[b'])
  readIORef lstButtonsIO
  where
    widthW = width $ windCnf gui
    heightW = height $ windCnf gui
    padX i = winPadX + (field-1)`mod`3 * 3 * cellSize (cllsCnf gui) + cellSize (cllsCnf gui) * i + 10 * ((field-1) `mod` 3)
    padY i= winPadY + (field-1)`div`3 * 3 * cellSize (cllsCnf gui) + cellSize (cllsCnf gui) * i + 10 * ((field-1)`div`3)
    winPadX = (widthW  - cellSize (cllsCnf gui) * 9) `div` 2
    winPadY = (heightW - cellSize (cllsCnf gui) * 9) `div` 2


updateHardCellsFunc :: IORef [HardField] -> [Ref Button] -> (MainGUI -> IORef [HardField] -> ButtonData -> IORef Player -> Ref Button -> IO ()) -> FieldNumber -> IORef Player -> MainGUI -> IO ()
updateHardCellsFunc field btns func num pl gui =
  forM_ [0..8] $ \i ->
    setCallback (btns !! i) (func gui field (BD {fieldN = num, btnN = i}) pl)