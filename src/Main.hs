module Main where

import Interface
import InterfaceLib

windowConfiguration :: WindowConfig 
windowConfiguration =
    WC {
        width = 600,
        height = 600
    }

main :: IO ()
main = do
    createMainMenu windowConfiguration
