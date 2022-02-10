module Main where

import Interface

countOfCells :: Int 
countOfCells = 3

main :: IO ()
main = do
    runInterface countOfCells
