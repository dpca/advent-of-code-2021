module Day04 (solution, example) where

import Data.List
import Text.Parsec
import Text.Parsec.String
import Data.List.Split (splitOn, chunksOf)

import Util (Solution, parseIt)

type Board = [[(Int, Bool)]]

flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (flip (foldr z)) n xs) (:) []

parseCalledNumbers :: String -> [Int]
parseCalledNumbers input = map read $ splitOn "," input

parseBoardLine :: Parser [(Int, Bool)]
parseBoardLine = do
    optional spaces
    nums <- many1 digit `sepBy` spaces
    return $ map (\x -> (read x, False)) nums

parseBoards :: [String] -> [Board]
parseBoards input = map (map (parseIt parseBoardLine) . tail) boardChunks
    where boardChunks = chunksOf 6 input

boardHasWon :: Board -> Bool
boardHasWon board = any (all snd) board || any (all snd) (transpose board)

playBingo :: [Int] -> [Board] -> (Int, Board)
playBingo (x:xs) boards =
    case find boardHasWon markedBoards of
      Just board -> (x, board)
      Nothing -> playBingo xs markedBoards
    where markedBoards = map (map (map (\(num, seen) -> (num, seen || num == x)))) boards
          wonBoard = find boardHasWon markedBoards
playBingo _ boards = error "Nobody has won"

playBingoUntilLastBoard :: [Int] -> [Board] -> (Int, Board)
playBingoUntilLastBoard nums [board] = playBingo nums [board]
playBingoUntilLastBoard (x:xs) boards = playBingoUntilLastBoard xs filteredBoards
    where markedBoards = map (map (map (\(num, seen) -> (num, seen || num == x)))) boards
          filteredBoards = filter (not. boardHasWon) markedBoards
playBingoUntilLastBoard _ boards = error "Nobody has won"

sumOfUnmarked :: Board -> Int
sumOfUnmarked board = sum $ map fst $ filter (\(num, seen) -> not seen) $ flatten board

part1 :: String -> Int
part1 input = winningNum * sumOfUnmarked winningBoard
    where (x:xs) = lines input
          calledNumbers = parseCalledNumbers x
          boards = parseBoards xs
          (winningNum, winningBoard) = playBingo calledNumbers boards

part2 :: String -> Int
part2 input = winningNum * sumOfUnmarked winningBoard
    where (x:xs) = lines input
          calledNumbers = parseCalledNumbers x
          boards = parseBoards xs
          (winningNum, winningBoard) = playBingoUntilLastBoard calledNumbers boards

example :: Solution
example = do
    contents <- readFile "src/inputs/day04_example.txt"
    let solution1 = part1 contents
    let solution2 = part2 contents
    return (show solution1, show solution2)

solution :: Solution
solution = do
    contents <- readFile "src/inputs/day04.txt"
    let solution1 = part1 contents
    let solution2 = part2 contents
    return (show solution1, show solution2)
