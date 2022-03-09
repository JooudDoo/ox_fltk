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

--countOfWins :: [[Player]] -> Player -> Int -> Int -> Int
--countOfWins field player rows block     | not $ null $ listOfWin field player rows block        = length $ listOfWin field player rows block
--                                        | not $ null $ listOfWin field (rPl player) rows block  = negate (length $ listOfWin field (rPl player) rows block)
--                                        | otherwise  = 0
countOfWins :: [[Player]] -> Player -> Int -> Int -> (Int, Int) -> Int
countOfWins field player rows block i   = length $ listOfWin (changeInField field player i) player rows block

countOfLose :: [[Player]] -> Player -> Int -> Int -> (Int, Int) -> Int
countOfLose field player rows block i   = length $ listOfWin (changeInField field (rPl player) i) (rPl player) rows block

callForBot :: [[Player]] -> Player -> IO (Int, Int)
callForBot field player = do
    let rows = length field
    let block = cellToWinCoef !! rows
    let win1 = listOfWin field player rows block
    let lose1 = listOfWin field (rPl player) rows block
    let goodPole = sortBy (\a b -> if countOfWins field player rows block a > countOfWins field player rows block b then LT else GT) (listOfFree field rows)
    let badPole = sortBy (\a b -> if countOfLose field player rows block a > countOfLose field player rows block b then LT else GT) (listOfFree field rows)
    --let test = [(i, countOfWins (changeInField field player i) player rows block) | i <- goodPole]
    --when debugging $ print test
    lastChanse <- callForBotRandom field player
    if not (null win1)
        then return $ head win1
        else if not (null lose1)
                then return $ head lose1
                else if countOfWins field player rows block (head goodPole) > 1
                    then return $ head goodPole-- lastChanse
                    else if countOfLose field player rows block (head badPole) > 1
                        then return $ head badPole
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
