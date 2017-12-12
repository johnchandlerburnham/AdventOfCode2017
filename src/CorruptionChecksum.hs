-- Day2
module CorruptionChecksum where

import System.IO

type Spreadsheet = [[Integer]]

rowDiff :: [Integer] -> Integer
rowDiff (x:xs) = go x x xs
  where
    go min max [] = max - min
    go min max (x:xs)
      | x > max = go min x xs
      | x < min =  go x max xs
      | otherwise = go min max xs

checkSum :: Spreadsheet -> Integer
checkSum s = foldr (+) 0 $ map rowDiff s

rowDiv :: [Integer] -> [Integer]
rowDiv list = [ (div x y) | x <- list, y <- list, x > y, x `mod` y == 0]

checkDiv :: Spreadsheet -> Integer
checkDiv s = foldr (+) 0 $ (head . rowDiv) <$> s

readSheet :: IO Spreadsheet
readSheet = do
  inputFile <- openFile "src/Day2Input1.txt" ReadMode
  contents <- hGetContents inputFile
  return ((fmap read) <$> words <$> lines contents)


