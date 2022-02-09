{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Control.Monad
import Data.IORef
import System.IO
import Data.Text (pack)

windowWidth :: Int
windowWidth = 500
windowHeight :: Int
windowHeight = 500
buttonSize :: Int
buttonSize = 50


swtichButton :: Ref Button -> IO ()
swtichButton b' = do
  state <- getLabel b'
  when (state == "") $ do
    file <- openFile "temp" ReadMode
    hSetEncoding file utf8
    stateNew <- hGetContents file
    setLabel b' $ pack stateNew
    hClose file

    file <- openFile "temp" WriteMode
    hSetEncoding file utf8
    if stateNew == "X"
      then hPutStr file "O"
      else hPutStr file "X"
    hClose file

winButton :: Ref Button -> IO ()
winButton b' = do
  setLabel b' "Da"
  hide b'

checkButtons :: [Ref Button] -> Int -> Ref Button  -> IO ()
checkButtons btns size winB = do
  pole <- newIORef ([] :: [[Int]])
  cntOfZeros <- newIORef 0
  onePacket <- newIORef ([] :: [Int])

  forM_ [0..(length btns-1)] $ \i -> do
    state <- getLabel (btns !! i)
    when (i `mod` size == 0 && i /= 0) $ do
      cache <- readIORef onePacket
      modifyIORef pole (++[cache])
      writeIORef onePacket []
    case state of
      "X" ->
        modifyIORef onePacket (++ [1])
      "O" ->
        modifyIORef onePacket (++ [-1])
      _ -> do
        modifyIORef onePacket (++ [0])
        modifyIORef cntOfZeros (+1) 
    return ()
  cntOfZeros <- readIORef cntOfZeros
  when (cntOfZeros == 0) $ do
    setLabel winB "Draw"
    showWidget winB
    forM_ [0..(length btns-1)] $ \i -> do
      setLabel (btns !! i)  ""
    FL.repeatTimeout 0.2 (checkButtons btns size winB)
    return ()

  cache <- readIORef onePacket
  modifyIORef pole (++[cache])

  poleNew <- readIORef pole

  print poleNew
  xIsWin <- newIORef False
  yIsWin <- newIORef False

  checkRes <- checkWin poleNew size 1
  writeIORef xIsWin checkRes
  checkRes <- checkWin poleNew size (-1)
  writeIORef yIsWin checkRes

  xWin <- readIORef xIsWin
  yWin <- readIORef yIsWin


  when (xWin || yWin) $ do
    
    if xWin 
      then setLabel winB "The crosses won"
      else setLabel winB "The noughts won"

    showWidget winB
    forM_ [0..(length btns-1)] $ \i -> do
      setLabel (btns !! i)  ""


  FL.repeatTimeout 0.2 (checkButtons btns size winB)
  return ()



checkWin :: [[Int]] -> Int -> Int -> IO Bool
checkWin pole size sym = do
  toRight <- newIORef True
  toLeft <- newIORef True
  win <- newIORef False

  forM_ [0..size-1] $ \row -> do
    cols <- newIORef True
    rows <- newIORef True

    modifyIORef toRight (&& pole !! row !! row==sym)
    modifyIORef toLeft (&& pole !! row !! (size-row-1)==sym)

    forM_ [0..size-1] $ \col -> do
      modifyIORef cols (&& (pole !! row !! col == sym))
      modifyIORef rows (&& (pole !! col !! row == sym))
    val1 <- readIORef cols
    val2 <- readIORef rows
    when (val1 || val2) $ writeIORef win True

  left <- readIORef toLeft
  right <- readIORef toRight
  winColsRows <- readIORef win
  return (left || right || winColsRows)


createButtons :: Int -> IO [Ref Button]
createButtons size = do

 let padX = (windowWidth - buttonSize * size) `div` 2
 let padY = (windowHeight - buttonSize * size) `div` 2
 result <- newIORef ([] :: [Ref Button])
 forM_ [0..size*size-1] $ \i -> do
    button <- buttonNew
              (Rectangle (Position (X (i `mod` size*buttonSize + padX)) (Y (i `div` size*buttonSize + padY))) (Size (Width buttonSize) (Height buttonSize)))
              (Just "")
    setLabelsize button (FontSize 15)
    setCallback button swtichButton
    modifyIORef result (++ [button])
    return ()
 readIORef result



main :: IO ()
main = do

  file <- openFile "temp" WriteMode
  hSetEncoding file utf8
  hPutStr file "X"
  hClose file



  let countOfButtonsInRow = 4

  window <- doubleWindowNew
            (Size (Width windowWidth) (Height windowHeight))
            Nothing
            Nothing
  begin window

  textInLabel <- boxCustom  
              (Rectangle (Position (X (windowWidth`div`4 + 20)) (Y 0)) (Size (Width 150) (Height 60)))
              (Just "OX Game")
              Nothing 
              Nothing 
              

  buttons <- createButtons countOfButtonsInRow

  winB <- buttonNew
              (Rectangle (Position (X (windowWidth`div` 4)) (Y (windowHeight`div` 2))) (Size (Width $ windowWidth`div` 2) (Height 50)))
              (Just "")
  setLabelsize winB (FontSize 15)
  setCallback winB winButton
  hide winB

  showWidget window

  print "created"
  checkButtons buttons countOfButtonsInRow winB
  FL.run
  print "run"
  FL.flush
  print "flush"


replMain :: IO ()
replMain = FL.replRun
