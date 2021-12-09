module Day09 (solution, example) where

import Data.List
import qualified Data.Map as Map

import Util (Solution, parseFile)

isLowPoint :: Int -> Int -> [[Int]] -> Bool
isLowPoint y x m = checkUp && checkDown && checkLeft && checkRight
    where checkUp    = y == 0                   || currentVal < (m !! (y - 1) !! x)
          checkDown  = y == length m - 1        || currentVal < (m !! (y + 1) !! x)
          checkLeft  = x == 0                   || currentVal < (m !! y !! (x - 1))
          checkRight = x == length (m !! 1) - 1 || currentVal < (m !! y !! (x + 1))
          currentVal = m !! y !! x

allPoints :: [[Int]] -> [(Int, Int)]
allPoints input = [ (y, x) | y <- [0..length input - 1], x <- [0..length (input !! 1) - 1]]

lowPoints :: [[Int]] -> [(Int, Int)]
lowPoints input = filter (\(y, x) -> isLowPoint y x input) (allPoints input)

floodFill :: Int -> Int -> Int -> Map.Map (Int, Int) Int -> (Int, Map.Map (Int, Int) Int)
floodFill y x c m
  | Map.notMember (y, x) m = (c, m)
  | m Map.! (y, x) == 9 = (c, m)
  | otherwise = flood (Map.insert (y, x) 9 m)
  where flood newMap = foldl (\(cn, mn) (yn, xn) -> floodFill yn xn cn mn) (c + 1, newMap) [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]

part1 :: [[Int]] -> Int
part1 input = sum . map riskLevel $ lowPoints input
    where riskLevel (y, x) = 1 + input !! y !! x

part2 :: [[Int]] -> Int
part2 input = product $ take 3 $ reverse (sort basins)
    where basins = map (fst . uncurry countBasin) (lowPoints input)
          countBasin y x = floodFill y x 0 (pointsMap input)
          pointsMap input = Map.fromList $ map (\(y, x) -> ((y, x), input !! y !! x)) (allPoints input)

parseInput :: String -> [[Int]]
parseInput input = map (map (\y -> read [y])) (lines input)

example :: Solution
example = do
    input <- readFile "src/inputs/day09_example.txt"
    let contents = parseInput input
    let solution1 = part1 contents
    let solution2 = part2 contents
    return (show solution1, show solution2)

solution :: Solution
solution = do
    input <- readFile "src/inputs/day09.txt"
    let contents = parseInput input
    let solution1 = part1 contents
    let solution2 = part2 contents
    return (show solution1, show solution2)
