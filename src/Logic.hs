module Logic where

import Data.IORef
import Control.Monad
import AdditionLib
import InterfaceLib
import System.Random
--Тут хранится разум бота

callForBotRandom :: [[Player]] -> Player -> IO (Int, Int)
callForBotRandom field player = do
    seedRow <- newStdGen
    let rows = randoms seedRow :: [Int]
    seedColl <- newStdGen
    let colls = randoms seedColl :: [Int]
    let turn = head $ dropWhile (\(r,c) -> takeCell field r c /= NaP) (zip rows colls)
    --when debuging $ print (abs (fst turn) `mod` length (head field), abs (snd turn) `mod` length (head field))
    return (abs (fst turn) `mod` length (head field), abs (snd turn) `mod` length (head field))
    where
        takeCell field r c = field !!( abs r `mod` length (head field) ) !! ( abs c `mod` length (head field) )
