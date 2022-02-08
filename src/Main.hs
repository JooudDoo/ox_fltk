{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Control.Monad
import Data.IORef

windowWidth :: Int
windowWidth = 500
windowHeight :: Int
windowHeight = 500
buttonSize :: Int
buttonSize = 50

swtichButton :: Ref Button -> IO ()
swtichButton b' = do
  state <- getLabel b'
  if state == "" || state == "O"
    then setLabel b' "X"
    else setLabel b' "O"

checkButtons :: [Ref Button] -> Int -> IO ()
checkButtons btns size = do
  pole <- newIORef ([] :: [Int])


  forM_ [0..(length btns-1)] $ \i -> do
    state <- getLabel (btns !! i)
    case state of
      "X" -> do
        modifyIORef pole (++ [1])
      "O" -> do
        modifyIORef pole (++ [-1])
      _ -> do
        modifyIORef pole (++ [0])
--TEST FOR ONLY 3x3 (PRE-PRE-ALPHA-BETA-DEMO)
  poleNe <- readIORef pole
  let poleNew = refactList poleNe

  print(poleNew)
  xIsWin <- newIORef False
  yIsWin <- newIORef False

  checkRes <- checkWinColsRows poleNew (size) 1
  modifyIORef xIsWin  (|| checkRes)
  checkRes <- checkWinColsRows poleNew (size) (-1)
  modifyIORef yIsWin  (|| checkRes)

  xWin <- readIORef xIsWin
  yWin <- readIORef yIsWin

  when (xWin) $ print ("X IS WIN COLS")
  when (yWin) $ print ("Y IS WIN COLS")

  checkRes <- checkWinDiag poleNew (size) 1
  modifyIORef xIsWin  (|| checkRes)

  checkRes <- checkWinDiag poleNew (size) (-1)
  modifyIORef yIsWin  (|| checkRes)

  xWin <- readIORef xIsWin
  yWin <- readIORef yIsWin

  when (xWin) $ print ("X IS WIN")
  when (yWin) $ print ("Y IS WIN")

  FL.repeatTimeout 1.5 (checkButtons btns size)
  return ()

refactList :: [Int] -> [[Int]]
refactList lst = [take 3 lst] ++ [take 3 $ drop 3 lst] ++ [drop 6 lst]

checkWinDiag :: [[Int]] -> Int -> Int -> IO Bool
checkWinDiag pole size sym = do
  toRight <- newIORef True
  toLeft <- newIORef True
  forM_ [0..size-1] $ \i -> do
    modifyIORef toRight (&& (pole !! i !! i)==sym)
    modifyIORef toLeft (&& (pole !! i !! (3-i-1))==sym)
  left <- readIORef toLeft
  right <- readIORef toRight
  return (left || right)
  

checkWinColsRows :: [[Int]] -> Int -> Int -> IO Bool
checkWinColsRows pole size sym = do
  win <- newIORef False
  forM_ [0..size-1] $ \row -> do
    cols <- newIORef True
    rows <- newIORef True
    forM_ [0..size-1] $ \col -> do
      modifyIORef cols (&& ((pole !! row !! col) == sym))
      modifyIORef rows (&& ((pole !! col !! row) == sym))
    val1 <- readIORef cols
    val2 <- readIORef rows
    when (val1 || val2) $ writeIORef win True
  readIORef win


createButtons :: Int -> IO [Ref Button]
createButtons size = do
 result <- newIORef ([] :: [Ref Button])
 forM_ [0..size*size-1] $ \i -> do
    button <- buttonNew
              (Rectangle (Position (X ((i `mod` size)*buttonSize)) (Y ((i `div` size)*buttonSize))) (Size (Width buttonSize) (Height buttonSize)))
              (Just "")
    setLabelsize button (FontSize 15)
    setCallback button swtichButton
    modifyIORef result (++ [button])
    return ()
 readIORef result



main :: IO ()
main = do

  window <- doubleWindowNew
            (Size (Width windowWidth) (Height windowHeight))
            Nothing
            Nothing
  begin window

  buttons <- createButtons 3
  print buttons
  showWidget window

  print "created"
  checkButtons buttons 3
  FL.run
  print "run"
  FL.flush
  print "flush"


replMain :: IO ()
replMain = FL.replRun
