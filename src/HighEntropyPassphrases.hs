-- Day4

module HighEntropyPassphrases where

import Data.List
import System.IO

unique :: [String] -> Bool
unique s = (length $ nub s) == length s

noAnagram :: [String] -> Bool
noAnagram s = unique $ sort <$> s

readPhrases :: ([String] -> Bool) -> IO Integer
readPhrases check = do
  inputFile <- openFile "src/Day4Input.txt" ReadMode
  contents <- (fmap words) <$> lines <$> hGetContents inputFile
  return $ fromIntegral $ length $ filter ((==) True) (check <$> contents)
