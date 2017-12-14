-- Day5
module MazeOfTwistyTrampolines where

import qualified Data.Vector as V
import System.IO
import Debug.Trace

data Maze = Maze { mIndex :: Int, mMaze :: V.Vector Int} deriving (Eq, Show)

stepsUntilEnd :: Maze -> Int
stepsUntilEnd (Maze index vector) = go 0 index vector
  where
   go n i v 
    | i >= V.length v = n
    | i < 0 = go n 0 v
    | otherwise = go (n + 1) (i + (v V.! i)) (v V.// [(i, (v V.! i) + 1)])


-- super slow
strangeStepsUntilEnd :: Maze -> Int
strangeStepsUntilEnd (Maze index vector) = go 0 index vector
  where
   go n i v 
    | i >= V.length v = n
    | i < 0 = go n 0 v
    | (v V.!? i) >= (Just 3) = trace (traceMsg n i v) $
        go (n + 1) (i + (v V.! i)) (v V.// [(i, (v V.! i) - 1)])
    | otherwise = trace (traceMsg n i v) $ 
        go (n + 1) (i + (v V.! i)) (v V.// [(i, (v V.! i) + 1)])

traceMsg :: Int -> Int -> V.Vector Int -> String
traceMsg n i v = 
  "Step: " ++ show n ++ ", Index:" ++ show i ++ ", Value: " ++ (show $ v V.!? i)

readInput :: IO Maze
readInput = do
  inputFile <- openFile "src/Day5Input.txt" ReadMode
  contents <- lines <$> hGetContents inputFile
  return $ Maze 0 (V.fromList $ read <$> contents)

