module Day07 (solution, example) where

import Util (Solution, parseFile)

import Text.Parsec
import Text.Parsec.String

inputParser :: Parser [Int]
inputParser = map read <$> sepBy (many1 digit) (char ',') <* newline

minFuel :: (Int -> Int) -> [Int] -> Int
minFuel fuel input = minimum $ map fuel [(minimum input)..(maximum input)]

part1 :: [Int] -> Int
part1 input = minFuel fuel input
    where fuel pos = sum $ map (\x -> abs (x - pos)) input

part2 :: [Int] -> Int
part2 input = minFuel fuel input
    where fuel pos = sum $ map (\x -> sum [0..(abs (x - pos))]) input

example :: Solution
example = do
    let input = [16,1,2,0,4,2,7,1,2,14]
    let solution1 = part1 input
    let solution2 = part2 input
    return (show solution1, show solution2)

solution :: Solution
solution = do
    input <- parseFile inputParser "src/inputs/day07.txt"
    let solution1 = part1 input
    let solution2 = part2 input
    return (show solution1, show solution2)
