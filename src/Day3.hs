module Day3
    ( day3
    ) where

import Paths_aoc_v2021
import Text.Read (readMaybe)
import Data.Char (digitToInt)

splitBit :: String -> [Int]
splitBit = map digitToInt

joinBits :: [[Int]] -> [Int]
joinBits (bits1:bits2:rest) = joinBits (joined:rest)
    where
        joined = zipWith (+) bits1 bits2
joinBits (bitsX:[]) = bitsX

testBit :: Int -> Int -> Int
testBit testVal bit
    | bit > testVal = 1
    | otherwise = 0

calculateGamma :: [Int] -> Int -> [Int]
calculateGamma joinedBits testVal = map (testBit testVal) joinedBits

day3 :: IO()
day3 = do
    filePath <- getDataFileName "data/day3.txt"
    bitsFile <- readFile filePath
    let bits = lines bitsFile
    let splitBits = map splitBit bits 
    let joinedBits = joinBits splitBits
    putStrLn "Day 2"
    putStr "Part 1: "
    print(calculateGamma joinedBits ((length splitBits) `div` 2))
    putStr "Part 2: "
        
