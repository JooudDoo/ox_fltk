{-# LANGUAGE OverloadedStrings #-}
module InterfaceLib where

import AdditionLib

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Control.Monad
import Data.IORef
import Data.Text (pack, Text)

--Дополнительные функции для интерефейса тут
--Сделать проверку на меньшие поля в большом поле

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
        cntInRow :: IORef Int
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


deactivateField :: [Ref Button] -> IO ()
deactivateField = mapM_ deactivate


activateField :: [Ref Button] -> IO ()
activateField = mapM_ activate


readCells :: [Ref Button] -> Int -> IO [Player]
readCells cellList inRow = do
  fieldIO <- newIORef ([] :: [Player])
  mapM_ (getLabel >=> (\d -> modifyIORef fieldIO (++ [pl d]))) cellList
  readIORef fieldIO


checkWinSimple :: Player -> [Ref Button] -> Int -> IO GameState
checkWinSimple player btnLst row = do
   field <- readCells btnLst row
   playerIsWin <- checkWinPl (refactorList field row) row player
   when debuging $ print $ refactorList field row
   return $ gState playerIsWin (checkDraw field)


checkWinHard :: Player -> IORef [HardField] -> IO GameState
checkWinHard playerCur fieldIO = do
   field <- readIORef fieldIO
   fieldBig <- newIORef ([] :: [Player])
   forM_ [0..length field -1] $ \i ->
     if state (field !! i) == Win
       then
         modifyIORef fieldBig (++[player (field !! i)])
        else
         modifyIORef fieldBig (++[NaP])
   fieldW <- readIORef fieldBig
   playerIsWin <- checkWinPl (refactorList fieldW 3) 3 playerCur
   when debuging $ print $ "BIG" ++ show(refactorList fieldW 3)
   return $ gState playerIsWin (checkDraw fieldW)


checkDraw :: [Player] -> GameState
checkDraw field
  | cntNaPs == 0 = Draw
  | otherwise = Game
  where
    cntNaPs = length $ filter (==NaP) field


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


refactorList :: [a] -> Int -> [[a]]
refactorList lst inRow = recur lst inRow 0 [] []
    where
        recur :: [a] -> Int -> Int -> [a] -> [[a]] -> [[a]]
        recur lst inRow cur buff res
                    | null lst = res ++ [buff]
                    | cur /= inRow = recur (tail lst) inRow (cur+1) (buff ++ [head lst]) res
                    | otherwise = recur lst inRow 0 [] (res ++ [buff])


--TODO 
--Отдельным блоком
--Возможность контролировать весь интерфейс
--Красиво и нарядно
winWidget :: [Ref Button] -> Player -> IO ()
winWidget field player = do
  print ("Winner is " ++ plT player)
  cleanAllCells field


--TODO 
--Отдельным блоком
--Возможность контролировать весь интерфейс
--Красиво и нарядно
drawWidget :: [Ref Button] -> IO ()
drawWidget field = do
  print "DRAW"
  cleanAllCells field


checkWinPl :: Eq a => [[a]] -> Int -> a -> IO GameState
checkWinPl pole inRow player = do
  toRight <- newIORef True
  toLeft <- newIORef True
  win <- newIORef False

  forM_ [0..inRow-1] $ \row -> do
    cols <- newIORef True
    rows <- newIORef True

    modifyIORef toRight (&& pole !! row !! row==player)
    modifyIORef toLeft (&& pole !! row !! (inRow-row-1)==player)

    forM_ [0..inRow-1] $ \col -> do
      modifyIORef cols (&& (pole !! row !! col == player))
      modifyIORef rows (&& (pole !! col !! row == player))

    val1 <- readIORef cols
    val2 <- readIORef rows
    when (val1 || val2) $ writeIORef win True

  left <- readIORef toLeft
  right <- readIORef toRight
  winColsRows <- readIORef win
  if left || right || winColsRows
    then return Win
    else return Game


createGameCells :: WindowConfig -> CellsConfig -> Ref Group -> (SimpleField -> IORef Player -> Ref Button -> IO ()) -> IO [Ref Button]
createGameCells wndConf cllsConf frame func = do
 lstButtonsIO <- newIORef ([] :: [Ref Button])
 inRow <- readIORef $ cntInRow cllsConf
 player <- newIORef Cross
 let padX = (windowWidth - buttonSize * inRow) `div` 2
 let padY = (windowHeight - buttonSize * inRow) `div` 2
 forM_ [0..inRow*inRow-1] $ \i -> do
    button <- newButton (i `mod` inRow*buttonSize + padX) (i `div` inRow*buttonSize + padY) buttonSize buttonSize (Just "")
    setLabelsize button (FontSize (fromIntegral $ buttonSize`div`2))
    modifyIORef lstButtonsIO (++ [button])
 lstButtons <- readIORef lstButtonsIO
 mapM_ (\s -> setCallback s (func (SF {fieldBtns = lstButtons, group = frame, rowCnt = inRow}) player)) lstButtons
 return lstButtons
 where
   buttonSize = cellSize cllsConf
   windowWidth = width wndConf
   windowHeight = height wndConf


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