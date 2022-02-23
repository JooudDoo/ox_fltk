module Logic where

import Data.IORef
import Control.Monad
import AdditionLib
import InterfaceLib
import System.Random
--Тут хранится разум бота

{-callForBot :: [[Player]] -> Player -> IO (Int, Int)
callForBot field player = do
    let listOfKey = dropWhile (\(r,c) ->  field !!( abs r `mod` length (head field) ) !! ( abs c `mod` length (head field) ) /= NaP) 
        [(i, j) | i <- [0..length (head field) - 1], j <- [0..length (head field) - 1]] 
-}

    

callForBotRandom :: [[Player]] -> Player -> IO (Int, Int)
callForBotRandom field player = do
    seedRow <- newStdGen
    let rows = randoms seedRow :: [Int]
    --let rows = [3..6]
    seedColl <- newStdGen
    let colls = randoms seedColl :: [Int]
    --let colls = [0..6]
    --let listik = [(i, j) | i <- [0..6], j <- [0..6]] 
    let turn = head $ dropWhile (\(r,c) -> takeCell field r c /= NaP) (zip rows colls)
    --when debuging $ print (abs (fst turn) `mod` length (head field), abs (snd turn) `mod` length (head field))
    return (abs (fst turn) `mod` length (head field), abs (snd turn) `mod` length (head field))
    where
        takeCell field r c = field !!( abs r `mod` length (head field) ) !! ( abs c `mod` length (head field) )