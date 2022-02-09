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
  pole <- newIORef ([] :: [[Int]])
  onePacket <- newIORef ([] :: [Int])

  forM_ [0..(length btns-1)] $ \i -> do
    state <- getLabel (btns !! i)
    when ((i `mod` size == 0) && i /= 0) $ do
      cache <- readIORef onePacket
      modifyIORef pole (++[cache])
      writeIORef onePacket []
    case state of
      "X" -> do
        modifyIORef onePacket (++ [1])
      "O" -> do
        modifyIORef onePacket (++ [-1])
      _ -> do
        modifyIORef onePacket (++ [0])
    return ()
  cache <- readIORef onePacket
  modifyIORef pole (++[cache])

--TEST FOR ONLY 3x3 (PRE-PRE-ALPHA-BETA-DEMO)
  poleNew <- readIORef pole

  print(poleNew)
  xIsWin <- newIORef False
  yIsWin <- newIORef False

  checkRes <- checkWin poleNew size 1
  writeIORef xIsWin checkRes
  checkRes <- checkWin poleNew size (-1)
  writeIORef yIsWin checkRes

  xWin <- readIORef xIsWin
  yWin <- readIORef yIsWin

  when (xWin) $ print ("X IS WIN")
  when (yWin) $ print ("Y IS WIN")

  FL.repeatTimeout 1.5 (checkButtons btns size)
  return ()

refactList :: [Int] -> [[Int]]
refactList lst = [take 4 lst] ++ [take 4 $ drop 4 lst] ++ [take 4 $ drop 8 lst] ++ [drop 12 lst]

checkWin :: [[Int]] -> Int -> Int -> IO Bool
checkWin pole size sym = do
  toRight <- newIORef True
  toLeft <- newIORef True
  win <- newIORef False

  forM_ [0..size-1] $ \row -> do
    cols <- newIORef True
    rows <- newIORef True

    modifyIORef toRight (&& (pole !! row !! row)==sym)
    modifyIORef toLeft (&& (pole !! row !! (size-row-1))==sym)

    forM_ [0..size-1] $ \col -> do
      modifyIORef cols (&& ((pole !! row !! col) == sym))
      modifyIORef rows (&& ((pole !! col !! row) == sym))
    val1 <- readIORef cols
    val2 <- readIORef rows
    when (val1 || val2) $ writeIORef win True
  
  left <- readIORef toLeft
  right <- readIORef toRight
  winColsRows <- readIORef win
  return (left || right || winColsRows)


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

  buttons <- createButtons 4
  print buttons
  showWidget window

  print "created"
  checkButtons buttons 4
  FL.run
  print "run"
  FL.flush
  print "flush"


replMain :: IO ()
replMain = FL.replRun
