module Day11 (solution, example) where

import Util (Solution, parseIt)

import qualified Data.Map as Map
import qualified Data.Bifunctor

type Octopuses = Map.Map (Int, Int) Int

parseInput :: String -> Octopuses
parseInput input = toMap $ map (map (\c -> read [c] :: Int)) $ lines input
    where toMap lst = Map.fromList $ map (\(x, y) -> ((x, y), lst !! x !! y)) (allPoints lst)

allPoints :: [[Int]] -> [(Int, Int)]
allPoints input = [ (x, y) | x <- [0..length input - 1], y <- [0..length (input !! 1) - 1]]

surroundingPoints :: [(Int, Int)]
surroundingPoints = [ (x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], not (x == 0 && y == 0) ]

runStep :: Octopuses -> Octopuses
runStep octopuses = newOctopuses
    where newOctopuses = Map.map (\v -> if v > 9 then 0 else v) addedOctopuses
          addedOctopuses = foldl incrementNeighbors baseOctopuses $ Map.keys $ Map.filter (> 9) baseOctopuses
          incrementNeighbors o (x, y) = foldl incrementOctopus o $ map (Data.Bifunctor.bimap (x +) (y +)) surroundingPoints
          incrementOctopus o (x, y)
            | Map.notMember (x, y) o = o
            | o Map.! (x, y) == 9 = incrementNeighbors (addOne (x, y) o) (x, y)
            | otherwise = addOne (x, y) o
          addOne (x, y) = Map.insertWith (+) (x, y) 1
          baseOctopuses = Map.map (+ 1) octopuses

sumFlashes :: Int -> Octopuses -> Int -> Int
sumFlashes 0 _ flashes = flashes
sumFlashes step octopuses flashes = sumFlashes (step - 1) newOctopuses (flashes + newFlashes)
    where newFlashes = length $ Map.filter (== 0) newOctopuses
          newOctopuses = runStep octopuses

findAllFlashing :: Int -> Octopuses -> Int
findAllFlashing step octopuses
  | all (== 0) octopuses = step
  | otherwise = findAllFlashing (step + 1) (runStep octopuses)

part1 :: String -> Int
part1 input = sumFlashes 100 (parseInput input) 0

part2 :: String -> Int
part2 input = findAllFlashing 0 (parseInput input)

example :: Solution
example = do
    input <- readFile "src/inputs/day11_example.txt"
    let solution1 = part1 input
    let solution2 = part2 input
    return (show solution1, show solution2)

solution :: Solution
solution = do
    input <- readFile "src/inputs/day11.txt"
    let solution1 = part1 input
    let solution2 = part2 input
    return (show solution1, show solution2)
