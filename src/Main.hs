module Main where

import Interface
import InterfaceLib

windowConfiguration :: WindowConfig 
windowConfiguration =
    WC {
        width = 1200,
        height = 600
    }

main :: IO ()
main = do
    createMainMenu windowConfiguration
