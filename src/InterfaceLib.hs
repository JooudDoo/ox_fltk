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
    field1 :: [Ref Button],
    field2 :: [Ref Button],
    field3 :: [Ref Button],
    field4 :: [Ref Button],
    field5 :: [Ref Button],
    field6 :: [Ref Button],
    field7 :: [Ref Button],
    field8 :: [Ref Button],
    field9 :: [Ref Button]
  }
type FieldNumber = Int

defaultColor :: RGB
defaultColor = (0,0,0)
crossColor :: RGB
crossColor = (155,17,30)
zeroColor :: RGB
zeroColor = (14,19,236)
backGroundColor :: RGB
backGroundColor = (47, 110, 147)


plToColor :: Player -> RGB
plToColor Cross = crossColor
plToColor Zero = zeroColor
plToColor _ = defaultColor


newButton :: Int -> Int -> Int -> Int -> Maybe Text -> IO (Ref Button)
newButton xPos yPos xSize ySize = buttonNew
            (Rectangle (Position (X xPos) (Y yPos)) (Size (Width xSize) (Height ySize)))


newLabel :: Int -> Int -> Int -> Int -> Maybe Text -> IO (Ref Box)
newLabel xPos yPos xSize ySize = boxNew
            (Rectangle (Position (X xPos) (Y yPos)) (Size (Width xSize) (Height ySize)))

newOXButtonState :: Ref Button -> Player -> IO ()
newOXButtonState b' pl' = do
  state <- getLabel b'
  when (state == "") $
    if pl' == NaP
      then do
          stateNew <- readAllFromFile "temp"
          setLabel b' (pack stateNew)
          switchColorPlayer (pl $ pack stateNew) b'
          if stateNew == "X"
            then writeIntoFile "temp" "O"
            else writeIntoFile "temp" "X"
      else do
        setLabel b' $ pack $ plT pl'
        switchColorPlayer pl' b'


switchColorPlayer :: Player -> Ref Button -> IO ()
switchColorPlayer player widget =
  rgbColorWithRgb (plToColor player) >>= setLabelcolor widget


readCells :: [Ref Button] -> Int -> IO [Player]
readCells cellList inRow = do
  fieldIO <- newIORef ([] :: [Player])
  forM_ [0..inRow*inRow-1] $ \i -> do
      state <- getLabel (cellList !! i)
      modifyIORef fieldIO (++ [pl state])
  readIORef fieldIO


checkWin :: Player -> [Ref Button] -> Int -> IO GameState
checkWin player btnLst row = do
   field <- readCells btnLst row
   playerIsWin <- checkWinPl (refactorList field row) row player
   return $ gState playerIsWin (checkDraw field)


checkDraw :: [Player] -> GameState
checkDraw field
  | cntNaPs == 0 = Draw
  | otherwise = Game
  where
    cntNaPs = length $ filter (==NaP) field


cleanAllCells :: [Ref Button] -> IO ()
cleanAllCells btns =
   forM_ [0..length btns-1] $ \i -> do
    setLabel (btns!!i) ""
    switchColorPlayer NaP (btns!!i)


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


checkWinPl :: [[Player]] -> Int -> Player -> IO GameState
checkWinPl pole inRow player = do
  toRight <- newIORef True
  toLeft <- newIORef True
  win <- newIORef False

  when debuging $ print pole

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


createGameCells :: WindowConfig -> CellsConfig -> ([Ref Button] -> Int -> Ref Button -> IO ()) -> IO [Ref Button]
createGameCells wndConf cllsConf func = do
 lstButtonsIO <- newIORef ([] :: [Ref Button])
 inRow <- readIORef $ cntInRow cllsConf
 let padX = (windowWidth - buttonSize * inRow) `div` 2
 let padY = (windowHeight - buttonSize * inRow) `div` 2
 forM_ [0..inRow*inRow-1] $ \i -> do
    button <- newButton (i `mod` inRow*buttonSize + padX) (i `div` inRow*buttonSize + padY) buttonSize buttonSize (Just "")
    setLabelsize button (FontSize (fromIntegral $ buttonSize`div`2))
    modifyIORef lstButtonsIO (++ [button])
 lstButtons <- readIORef lstButtonsIO
 forM_ [0..inRow*inRow-1] $ \i ->
    setCallback (lstButtons !! i) (func lstButtons inRow)
 return lstButtons
 where
   buttonSize = cellSize cllsConf
   windowWidth = width wndConf
   windowHeight = height wndConf


createHardCells :: MainGUI -> f x -> IO ()
createHardCells gui func = do
  fieldX <- newIORef ([] :: [Ref Button])
  cache <- createHardCellsField gui 1 
  modifyIORef fieldX (++cache)
  cache <- createHardCellsField gui 2
  modifyIORef fieldX (++cache)
  cache <- createHardCellsField gui 3 
  modifyIORef fieldX (++cache)
  cache <- createHardCellsField gui 4 
  modifyIORef fieldX (++cache)
  cache <- createHardCellsField gui 5 
  modifyIORef fieldX (++cache)
  cache <- createHardCellsField gui 6 
  modifyIORef fieldX (++cache)
  cache <- createHardCellsField gui 7 
  modifyIORef fieldX (++cache)
  cache <- createHardCellsField gui 8 
  modifyIORef fieldX (++cache)
  cache <- createHardCellsField gui 9 
  modifyIORef fieldX (++cache)
  let field = HF
        {
          field1 = fl1,
          field2 = fl2,
          field3 = fl3,
          field4 = fl4,
          field5 = fl5,
          field6 = fl6,
          field7 = fl7,
          field8 = fl8,
          field9 = fl9
        }
  updateHardCellsFunc (field1 field) func

  return ()

createHardCellsField :: MainGUI -> FieldNumber -> IO [Ref Button]
createHardCellsField gui field = do
  lstButtonsIO <- newIORef ([] :: [Ref Button])
  forM_ [0..2] $ \i ->
    forM_ [0..2] $ \d -> do
    b' <- newButton (padX d) (padY i) (cellSize $ cllsCnf gui) (cellSize $ cllsCnf gui) (Just "")
    --Костыль
    setCallback b' kostil
    modifyIORef lstButtonsIO (++[b'])
    print winPadX
  readIORef lstButtonsIO
  where
    widthW = width $ windCnf gui
    heightW = height $ windCnf gui
    padX i = winPadX + ((field-1)`mod`3) * 3 * cellSize (cllsCnf gui) + cellSize (cllsCnf gui) * i + 10 * ((field-1) `mod` 3)
    padY i= winPadY + ((field-1)`div`3) * 3 * cellSize (cllsCnf gui) + cellSize (cllsCnf gui) * i + 10 * ((field-1)`div`3)
    winPadX = (widthW  - cellSize (cllsCnf gui) * 9) `div` 2
    winPadY = (heightW - cellSize (cllsCnf gui) * 9) `div` 2


updateHardCellsFunc :: HardField -> f x -> IO ()
updateHardCellsFunc field func =
  undefined


kostil :: Ref Button -> IO ()
kostil b' = do
  state <- getLabel b'
  if (state == "X")
    then setLabel b' "O"
    else setLabel b' "X"