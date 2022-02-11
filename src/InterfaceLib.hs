{-# LANGUAGE OverloadedStrings #-}
module InterfaceLib where

import AdditionLib
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Control.Monad
import Data.IORef

import Data.Text (pack, Text)

--Дополнительные функции для интерефейса тут

newButton :: Int -> Int -> Int -> Int -> Maybe Text -> IO (Ref Button)
newButton xPos yPos xSize ySize = buttonNew
            (Rectangle (Position (X xPos) (Y yPos)) (Size (Width xSize) (Height ySize)))

newOXButtonState :: Ref Button -> IO ()
newOXButtonState b' = do
  state <- getLabel b'
  when (state == "") $ do
    stateNew <- readAllFromFile "temp"
    setLabel b' $ pack stateNew

    {--if stateNew == "X"
      then writeIntoFile "temp" "O"
      else writeIntoFile "temp" "X" --}

readCells :: [Ref Button] -> Int -> IO [Player]
readCells cellList inRow = do
  fieldIO <- newIORef ([] :: [Player])
  forM_ [0..inRow*inRow-1] $ \i -> do
      state <- getLabel (cellList !! i)
      modifyIORef fieldIO (++ [pl state])
  readIORef fieldIO

checkWin :: Player -> [Ref Button] -> Int -> IO Bool
checkWin player btnLst row = do
   field <- readCells btnLst row
   checkWinPl (refactorList field row) row player


cleanAllCells :: [Ref Button] -> IO ()
cleanAllCells btns =
   forM_ [0..length btns-1] $ \i ->
    setLabel (btns!!i) ""


refactorList :: [a] -> Int -> [[a]]
refactorList lst inRow = recur lst inRow 0 [] []
    where
        recur :: [a] -> Int -> Int -> [a] -> [[a]] -> [[a]]
        recur lst inRow cur buff res
                    | null lst = res ++ [buff]
                    | cur /= inRow = recur (tail lst) inRow (cur+1) (buff ++ [head lst]) res
                    | otherwise = recur lst inRow 0 [] (res ++ [buff])



checkWinPl :: [[Player]] -> Int -> Player -> IO Bool
checkWinPl pole inRow player = do
  toRight <- newIORef True
  toLeft <- newIORef True
  win <- newIORef False
  print pole

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
  return (left || right || winColsRows)
