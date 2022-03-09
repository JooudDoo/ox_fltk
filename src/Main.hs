module Main where

import Interface ( createMainMenu )
import InterfaceLib ( WindowConfig(..) )

windowConfiguration :: WindowConfig 
windowConfiguration =
    WC {
        width = 800,
        height = 600,
        fullscreen = False
    }

main :: IO ()
main = createMainMenu windowConfiguration 3
