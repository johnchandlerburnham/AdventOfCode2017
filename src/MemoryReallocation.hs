module MemoryReallocation where

import Data.List
import Data.Maybe
import System.IO

data Memory = Memory [Int] deriving (Eq, Show)
data History = History [Memory] deriving (Eq, Show)

readInitial :: IO Memory
readInitial = do
  inputFile <- openFile "src/Day6Input1.txt" ReadMode
  contents <- hGetContents inputFile
  return $ Memory (read <$> words contents)

greatestElem :: [Int] -> (Int, Int)
greatestElem m = go m 0 (0,0)
  where
    go     [] i n = n
    go (x:xs) i (n, v) = 
      if x > v then go xs (i + 1) (i, x) else go xs (i + 1) (n, v)

dist :: Int -> Int -> Int -> [Int]
dist i v l = rotateRight (l - i - 1) $ go (mod v l) (replicate l (div v l))
  where
    go 0 xs = xs
    go n (x:xs) = (x + 1) : (go (n - 1) xs)
    rotateRight n xs = (drop n xs) ++ (take n xs)

section :: Int -> [Int] -> [Int]
section n xs = take n xs ++ [0] ++ drop (n + 1) xs

next :: Memory -> Memory
next (Memory mem) = Memory $ zipWith (+) (section gn mem) ds
  where
    (gn, gv) = greatestElem mem 
    ds = dist gn gv (length mem)

debugger :: Memory -> Int
debugger mem = go 0 mem []
  where
    go n m hist = if (elem m hist) then n else go (n + 1) (next m) (m:hist) 

solution1 = debugger <$> readInitial

test = Memory [0,2,7,0]

debugger' :: Memory -> Int
debugger' mem = go 0 mem []
  where
    go n m hist = let index = elemIndex m hist in
      if (isJust index)
      then (fromJust index) + 1
      else go (n + 1) (next m) (m:hist) 

solution2 = debugger' <$> readInitial
