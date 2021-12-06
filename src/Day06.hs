module Day06 (solution, example) where

import Util (Solution, parseFile)

import Text.Parsec
import Text.Parsec.String
import Data.List
import qualified Data.Map as Map

inputParser :: Parser [Int]
inputParser = map read <$> sepBy (many1 digit) (char ',') <* newline

runDay :: Map.Map Int Int -> Map.Map Int Int
runDay input = foldl insertFish Map.empty $ Map.toList input
    where insertFish newMap (timer, count) =
            if timer == 0
               then Map.insert 8 count (Map.insertWith (+) 6 count newMap)
               else Map.insertWith (+) (timer - 1) count newMap

solveIt :: [Int] -> Int -> Int
solveIt input days = sum $ map snd $ Map.toList allFish
    where mapFish = Map.fromList [(head ks, length ks) | ks <- group $ sort input]
          allFish = foldl (\m _ -> runDay m) mapFish [0..days-1]

example :: Solution
example = do
    input <- parseFile inputParser "src/inputs/day06_example.txt"
    let solution1 = solveIt input 80
    let solution2 = solveIt input 256
    return (show solution1, show solution2)

solution :: Solution
solution = do
    input <- parseFile inputParser "src/inputs/day06.txt"
    let solution1 = solveIt input 80
    let solution2 = solveIt input 256
    return (show solution1, show solution2)
