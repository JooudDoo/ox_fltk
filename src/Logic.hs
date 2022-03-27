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

countOfWins :: [[Player]] -> Player -> Int -> Int -> (Int, Int) -> Int
countOfWins field player rows block i   = length $ listOfWin (changeInField field player i) player rows block

countOfLose :: [[Player]] -> Player -> Int -> Int -> (Int, Int) -> Int
countOfLose field player rows block i   = length $ listOfWin (changeInField field (rPl player) i) (rPl player) rows block

countHard :: [HardPlayers] -> (Int, Int) -> Player -> Int
countHard field (i, j) player   | stateP (field !! (i * 3 + j)) == Draw     = 1000
                                | stateP (field !! (i * 3 + j)) == Win      = 100
                                | otherwise                                 = length $ listOfWin (fieldP (field !! (i * 3 + j))) (rPl player) 3 3

chanseToWin :: [HardPlayers] -> Int -> Player -> Bool
chanseToWin field idPole player = not (null $ listOfWin (fieldP (field !! idPole)) player 3 3)
--

callForBot :: [[Player]] -> Player -> IO (Int, Int)
callForBot field player = do
    let rows = length field
    let block = cellToWinCoef !! rows
    let win1 = listOfWin field player rows block
    let lose1 = listOfWin field (rPl player) rows block
    lastChanse <- callForBotRandom field player
    if not (null win1)
        then return $ head win1
        else if not (null lose1)
                then return $ head lose1
                else do
                    let goodPole = sortBy (\a b -> if countOfWins field player rows block a > countOfWins field player rows block b then LT else GT) (listOfFree field rows)
                    if countOfWins field player rows block (head goodPole) > 1
                    then return $ head goodPole
                    else do
                        let badPole = sortBy (\a b -> if countOfLose field player rows block a > countOfLose field player rows block b then LT else GT) (listOfFree field rows)
                        if countOfLose field player rows block (head badPole) > 1
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

callForHelp :: [HardPlayers] -> FieldNumber -> Player -> IO (Int, Int)
callForHelp field poleId player = do
    let pole = fieldP (field !! poleId)
    let win1 = listOfWin pole player 3 3
    let lose1 = listOfWin pole (rPl player) 3 3
    if not (null win1)
        then return (head win1)
        else if not (null lose1)
            then return (head lose1)
            else do
                let prioritet = sortBy (\a b -> if countOfWins pole player 3 3 a > countOfWins pole player 3 3 b then LT else GT) (listOfFree pole 3)
                let var = sortBy (\a b -> if countHard field a player < countHard field b player then LT else GT) (reverse prioritet)
                let ser = takeWhile (\a -> countHard field a player == countHard field (head var) player && countOfWins pole player 3 3 a == countOfWins pole player 3 3 (head var)) var
                ind <- randomRIO(0, length ser - 1)
                return (ser !! ind)

callForHardBotRandom :: [HardPlayers] -> Player -> IO (FieldNumber, Int, Int)
callForHardBotRandom field player = do
    let pole = [i | i <- zip [0..] field, isActiveFieldP (snd i)]
    if length pole == 1
        then do
            res <- callForHelp field (fst $ head pole) player
            return (fst $ head pole, fst res, snd res)
        else do
            let listFree = [i | i <- pole, stateP (field !! fst i) == Game]
            let idNext = sortBy (\a b -> if chanseToWin field (fst a) player then LT else GT) listFree
            if chanseToWin field (fst $ head idNext) player
                then do
                    res <- callForHelp field (fst $ head idNext) player
                    return (fst $ head idNext, fst res, snd res)
                else do
                    let idLose = sortBy (\a b -> if chanseToWin field (fst a) (rPl player) then LT else GT) listFree
                    if chanseToWin field (fst $ head idLose) (rPl player)
                        then do
                            res <- callForHelp field (fst $ head idLose) player
                            return (fst $ head idLose, fst res, snd res)
                        else do
                            ind <- randomRIO(0, length listFree - 1)
                            res <- callForHelp field ind player
                            return (ind, fst res, snd res)

