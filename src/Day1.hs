module Day1
    ( day1
    ) where

import Paths_aoc_v2021
import Text.Read (read)

depth_increase :: [Int] -> Int
depth_increase (one:two:rest)
    | two > one = 1 + depth_increase (two:rest)
    | otherwise = depth_increase (two:rest)
depth_increase (last:[]) = 0

triplets :: [Int] -> [Int]
triplets (one:two:three:rest) = (one + two + three : triplets (two:three:rest))
triplets (_:_:[]) = []

day1 :: IO()
day1 = do
    filePath <- getDataFileName "data/day1.txt"
    numbersFile <- readFile filePath
    let numbersFileSplit = lines numbersFile
    let numbers = map read numbersFileSplit :: [Int]
    putStrLn "Day 1"
    putStr "Part 1: "
    print (depth_increase numbers)
    let three_measure = triplets numbers
    putStr "Part 2: "
    print (depth_increase three_measure)
    putStrLn ""
