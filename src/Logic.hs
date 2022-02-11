module Logic where

import Data.IORef
import Control.Monad
import AdditionLib
--Тут хранится разум бота

callForBotRandom :: [[Player]] -> Player -> IO (Int, Int)
callForBotRandom field player = do
    result <- newIORef (0,0)
    forM_ [0..inRow-1] $ \i -> do
        forM_ [0..inRow-1] $ \k -> do
            when (field !! i !! k == NaP) $ do
                writeIORef result (i,k)
    readIORef result
    where
        inRow = length $ head field