-- Day3 
module SpiralMemory where

import Data.List

type Coordinate = (Integer, Integer)
data Direction = North | South | East | West deriving (Eq, Show)

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft South = East
turnLeft East = North
turnLeft West = South

proceed :: Direction -> Coordinate -> Coordinate
proceed North (x,y) = (x, y + 1)
proceed South (x,y) = (x, y - 1) 
proceed East  (x,y) = (x + 1, y) 
proceed West  (x,y) = (x - 1, y)

steps :: Integer -> Coordinate
steps end = go 1 1 0 East (0, 0)
  where
    go step path n d (x, y)
     | step == end = (x, y)
     | path == n = go step (up path (x,y)) 0 (turnLeft d) (x, y) 
     | otherwise = go (step + 1) path (n + 1) d (proceed d (x, y))
    up p (x, y) = if x == y then p + 1 else p

manhattanDistance :: Coordinate -> Coordinate -> Integer
manhattanDistance (a, b) (c, d) = abs (a - c) + abs (b - d)

isAdjacent :: Coordinate -> Coordinate -> Bool
isAdjacent (a,b) (c,d) = (a - c) ^ 2 + (b - d) ^ 2 < 4


adjPrevSquares :: Integer -> [Integer]
adjPrevSquares n = [ m | m <- [1..(n -1)], isAdjacent (steps n) (steps m) ]

memoryVal :: Integer -> Integer
memoryVal 0 = 0  
memoryVal 1 = 1
memoryVal n = foldl (+) 0 (memoryVal <$> adjPrevSquares n)

memoryVal' :: Int -> Integer
memoryVal' = (map memV [1..] !!)
  where
    memV 0 = 0
    memV 1 = 1
    memV n = foldl (+) 0 (memoryVal' <$> (fromIntegral <$> adjPrevSquares n))

result :: Maybe Integer
result = find (\x -> x > 265149) (memoryVal <$> [1..])


