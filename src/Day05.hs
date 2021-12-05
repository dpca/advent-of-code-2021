module Day05 (solution, example) where

import Util (Solution, parseFile)

import Data.List
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map

type Coordinate = (Int, Int)
type Grid = Map.Map Coordinate Integer

lineParser :: Parser (Coordinate, Coordinate)
lineParser = do
    x1 <- read <$> many1 digit
    char ','
    y1 <- read <$> many1 digit
    string " -> "
    x2 <- read <$> many1 digit
    char ','
    y2 <- read <$> many1 digit
    return ((x1, y1), (x2, y2))

parseInput :: Parser [(Coordinate, Coordinate)]
parseInput = endBy lineParser newline

makeGrid :: Grid -> [(Coordinate, Coordinate)] -> Grid
makeGrid grid [] = grid
makeGrid grid (((x1, y1), (x2, y2)):lst)
  | x1 == x2 = makeGrid (foldl (\g y -> Map.insertWith (+) (x1, y) 1 g) grid [sy1..sy2]) lst
  | y1 == y2 = makeGrid (foldl (\g x -> Map.insertWith (+) (x, y1) 1 g) grid [sx1..sx2]) lst
  | abs(sy2 - sy1) == abs(sx2 - sx1) = makeGrid (foldl (\g n -> Map.insertWith (+) (sx1 + n, (if sy1 > sy2 then (-) else (+)) sy1 n) 1 g) grid [0..abs(sy2 - sy1)]) lst
  | otherwise = makeGrid grid lst
    where [(sx1, sy1), (sx2, sy2)] = sort [(x1, y1), (x2, y2)]

part1 :: [(Coordinate, Coordinate)] -> Int
part1 input = length $ Map.filter (> 1) grid
    where grid = makeGrid Map.empty $ filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) input

part2 :: [(Coordinate, Coordinate)] -> Int
part2 input = length $ Map.filter (> 1) grid
    where grid = makeGrid Map.empty input

example :: Solution
example = do
    input <- parseFile parseInput "src/inputs/day05_example.txt"
    let solution1 = part1 input
    let solution2 = part2 input
    return (show solution1, show solution2)

solution :: Solution
solution = do
    input <- parseFile parseInput "src/inputs/day05.txt"
    let solution1 = part1 input
    let solution2 = part2 input
    return (show solution1, show solution2)
