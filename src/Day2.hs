module Day2
    ( day2
    ) where

import Paths_aoc_v2021
import Text.Read (readMaybe)

data Vector = Vector {
    direction :: String,
    size      :: Int
} deriving (Show)

parseVector :: String -> Vector
parseVector line = Vector direction size
    where
        pieces = words line
        direction = head pieces 
        size = parseSize (last pieces) 

parseSize :: String -> Int
parseSize str = case (readMaybe str :: Maybe Int) of Nothing -> 0
                                                     Just x -> x

calculate :: [Vector] -> Int -> Int -> Int
calculate ((Vector direction size):rest) horizontal depth
    | direction == "forward" = calculate rest (horizontal + size) depth
    | direction == "down"    = calculate rest horizontal (depth + size)
    | direction == "up"      = calculate rest horizontal (depth - size)
    | otherwise              = calculate rest horizontal depth
calculate [] horizontal depth = horizontal * depth

calculate' :: [Vector] -> Int -> Int -> Int -> Int
calculate' ((Vector direction size):rest) horizontal depth aim
    | direction == "forward" = calculate' rest (horizontal + size) (depth + aim * size) aim
    | direction == "down"    = calculate' rest horizontal depth (aim + size)
    | direction == "up"      = calculate' rest horizontal depth (aim - size)
    | otherwise              = calculate' rest horizontal depth aim
calculate' [] horizontal depth _ = horizontal * depth

day2:: IO()
day2 = do
    filePath <- getDataFileName "data/day2.txt"
    instructionsFile <- readFile filePath
    let instructions = lines instructionsFile
    let parsedVectors = map parseVector instructions
    putStrLn "Day 2"
    putStr "Part 1: "
    print (calculate parsedVectors 0 0)
    putStr "Part 2: "
    print (calculate' parsedVectors 0 0 0)
        
