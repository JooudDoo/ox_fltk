module Main where

import Interface
import InterfaceLib

cellsConfiguration :: CellsConfig
cellsConfiguration = 
    CC {
        cellSize = 75,
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
    createMainMenu windowConfiguration (GC cellsConfiguration windowConfiguration)