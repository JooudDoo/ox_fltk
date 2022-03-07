module Logic where

import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef
import Control.Monad
import AdditionLib
import InterfaceLib
import System.Random
--Тут хранится разум бота


callForBot :: [[Player]] -> Player -> IO (Int, Int)
callForBot field player = do
    let inRowIn = length (head field)
    let block = cellToWinCoef !! inRowIn
    let listOfFree = [(i, j) | i <- [0..inRowIn - 1], j <- [0..inRowIn - 1], takeCell field i j == NaP]
    let listOfWin = [i | i <- listOfFree, checkWinPlCustom (changeInField field player i) inRowIn block player == Win]
    let listOfLose = [i | i <- listOfFree, checkWinPlCustom (changeInField field (rPl player) i) inRowIn block (rPl player) == Win]
    lastChanse <- callForBotRandom field player
    if not (null listOfWin)
        then return $ head listOfWin
        else
            if not (null listOfLose)
                then return $ head listOfLose
                else return lastChanse


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


callForHardBotRandom :: [HardPlayers] -> Player -> IO (FieldNumber, Int, Int)
callForHardBotRandom field player = undefined
