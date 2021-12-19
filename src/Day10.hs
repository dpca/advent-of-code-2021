module Day10 (solution, example) where

import Util (Solution, parseIt)

import Data.List

data Line line = ValidLine | CorruptLine line | IncompleteLine line

parseLine :: String -> String -> Line String
parseLine [] [] = ValidLine
parseLine [] buffer = IncompleteLine buffer
parseLine ('(':xs) ys = parseLine xs ('(':ys)
parseLine (')':xs) ('(':ys) = parseLine xs ys
parseLine ('[':xs) ys = parseLine xs ('[':ys)
parseLine (']':xs) ('[':ys) = parseLine xs ys
parseLine ('{':xs) ys = parseLine xs ('{':ys)
parseLine ('}':xs) ('{':ys) = parseLine xs ys
parseLine ('<':xs) ys = parseLine xs ('<':ys)
parseLine ('>':xs) ('<':ys) = parseLine xs ys
parseLine line _ = CorruptLine line

findCorruptLines :: String -> [String]
findCorruptLines input = foldr (isCorrupt . (`parseLine` [])) [] (lines input)
    where isCorrupt (CorruptLine line) acc = line : acc
          isCorrupt _ acc = acc

findIncompleteLines :: String -> [String]
findIncompleteLines input = foldr (isIncomplete . (`parseLine` [])) [] (lines input)
    where isIncomplete (IncompleteLine line) acc = line : acc
          isIncomplete _ acc = acc

findError :: String -> Int
findError (')':_) = 3
findError (']':_) = 57
findError ('}':_) = 1197
findError ('>':_) = 25137
findError _ = error "Bad input"

findAutocompleteScore :: String -> Int -> Int
findAutocompleteScore [] score = score
findAutocompleteScore ('(':xs) score = findAutocompleteScore xs ((5 * score) + 1)
findAutocompleteScore ('[':xs) score = findAutocompleteScore xs ((5 * score) + 2)
findAutocompleteScore ('{':xs) score = findAutocompleteScore xs ((5 * score) + 3)
findAutocompleteScore ('<':xs) score = findAutocompleteScore xs ((5 * score) + 4)
findAutocompleteScore _ _ = error "Bad input"

part1 :: String -> Int
part1 input = sum $ map findError (findCorruptLines input)

part2 :: String -> Int
part2 input = scores !! (length scores `div` 2)
    where scores = sort $ map (`findAutocompleteScore` 0) (findIncompleteLines input)

example :: Solution
example = do
    input <- readFile "src/inputs/day10_example.txt"
    let solution1 = part1 input
    let solution2 = part2 input
    return (show solution1, show solution2)

solution :: Solution
solution = do
    input <- readFile "src/inputs/day10.txt"
    let solution1 = part1 input
    let solution2 = part2 input
    return (show solution1, show solution2)
