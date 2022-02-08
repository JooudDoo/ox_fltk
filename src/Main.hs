{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Control.Monad

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

checkButtons :: [Ref Button] -> IO ()
checkButtons blst = do
  state <- getLabel (head blst)
  if state == "X"
    then setLabel (head blst) "HA"
    else setLabel (head blst) state
  FL.repeatTimeout 0.12 (checkButtons blst)
  return ()


createButtons :: Int -> IO [Ref Button]
createButtons size = do
 let result = [] :: [Ref Button]
 let currentRow = 1
 let currentInRow = 1
 forM_ [0..size*size-1] $ \i -> do
    button <- buttonNew
              (Rectangle (Position (X ((i `mod` size)*buttonSize)) (Y ((i `div` size)*buttonSize))) (Size (Width buttonSize) (Height buttonSize)))
              (Just "")
    setLabelsize button (FontSize 15)
    setCallback button swtichButton
    let result = result ++ [button]
    return ()
 return result

main :: IO ()
main = do
  window <- doubleWindowNew
            (Size (Width windowWidth) (Height windowHeight))
            Nothing
            Nothing
  begin window

  buttons <- createButtons 3
  showWidget window

  FL.run
  FL.flush

replMain :: IO ()
replMain = FL.replRun
