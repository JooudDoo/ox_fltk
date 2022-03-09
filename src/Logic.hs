module Logic where

import Data.List
import Data.IORef
import Control.Monad
import AdditionLib
import InterfaceLib
import System.Random
--Тут хранится разум бота


listOfFree :: [[Player]] -> Int -> [(Int, Int)]
listOfFree field rows = [(i, j) | i <- [0..rows - 1], j <- [0..rows - 1], takeCell field i j == NaP]

checkWinPole :: [[Player]] -> Player -> (Int, Int) -> Int -> Int -> Bool
checkWinPole field player i rows block = checkWinPlCustom (changeInField field player i) rows block player == Win

listOfWin :: [[Player]] -> Player -> Int -> Int -> [(Int, Int)]
listOfWin field player rows block = [i | i <- listOfFree field rows, checkWinPole field player i rows block]

countOfWins :: [[Player]] -> Player -> Int -> Int -> Int
countOfWins field player rows block     | not $ null $ listOfWin field player rows block        = length $ listOfWin field player rows block
                                        | not $ null $ listOfWin field (rPl player) rows block  = negate (length $ listOfWin field (rPl player) rows block)
                                        | otherwise  = 0

callForBot :: [[Player]] -> Player -> IO (Int, Int)
callForBot field player = do
    let rows = length field
    let block = cellToWinCoef !! rows
    let win1 = listOfWin field player rows block
    let lose1 = listOfWin field (rPl player) rows block
    let goodPole = sortBy (\a b -> if countOfWins (changeInField field player a) player rows block > countOfWins (changeInField field player b) player rows block then LT else GT) (listOfFree field rows)
    lastChanse <- callForBotRandom field player
    if not (null win1)
        then return $ head win1
        else if not (null lose1)
                then return $ head lose1
                else if countOfWins (changeInField field player (head goodPole)) player rows block > 1
                    then return $ head goodPole-- lastChanse
                    else if countOfWins (changeInField field player (last goodPole)) player rows block < -1
                        then return $ last goodPole
                        else return lastChanse




callForBotRandom :: [[Player]] -> Player -> IO (Int, Int)
callForBotRandom field player = do
    let inRowIn = length field
    seedRow <- newStdGen
    let rows = randoms seedRow :: [Int]
    seedColl <- newStdGen
    let colls = randoms seedColl :: [Int]
    let turn = head $ dropWhile (\(r,c) -> takeCell field r c /= NaP) (zip rows colls)
    return (abs (fst turn) `mod` inRowIn, abs (snd turn) `mod` inRowIn)


callForHardBotRandom :: [HardPlayers] -> Player -> IO (FieldNumber, Int, Int)
callForHardBotRandom field player = undefined
