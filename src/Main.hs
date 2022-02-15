module Main where

import Interface
import InterfaceLib

windowConfiguration :: WindowConfig 
windowConfiguration =
    WC {
        width = 1000,
        height = 600
    }

main :: IO ()
main = do
    createMainMenu windowConfiguration
