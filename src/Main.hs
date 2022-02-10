module Main where

import Interface

cellsConfiguration :: CellsConfig
cellsConfiguration = 
    CC {
        cellSize = 50,
        cntInRow = 3
    } 
windowConfiguration :: WindowConfig 
windowConfiguration =
    WC {
        width = 500,
        height = 500
    }

main :: IO ()
main = do
    runInterface windowConfiguration cellsConfiguration