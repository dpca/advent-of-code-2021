module Day02 (solution) where

import Data.List
import Text.Parsec
import Text.Parsec.String
import Control.Monad

import Util (Solution, parseIt)

type Command = (String, Int)
type Position = (Int, Int)
type Aim = Int

parseCommand :: Parser Command
parseCommand = do
    direction <- many1 alphaNum
    space
    num <- many1 digit
    return (direction, read num)

part1 :: Position -> [Command] -> Position
part1 (x, y) ((direction, num):lst)
  | direction == "forward" = part1 (x + num, y) lst
  | direction == "down"    = part1 (x, y + num) lst
  | direction == "up"      = part1 (x, y - num) lst
  | otherwise = error "Bad input"
part1 (x, y) _ = (x, y)

part2 :: Position -> Aim -> [Command] -> Position
part2 (x, y) aim ((direction, num):lst)
  | direction == "forward" = part2 (x + num, y + (aim * num)) aim lst
  | direction == "down"    = part2 (x, y) (aim + num) lst
  | direction == "up"      = part2 (x, y) (aim - num) lst
  | otherwise = error "Bad input"
part2 (x, y) _ _ = (x, y)

solution :: Solution
solution = do
    contents <- readFile "src/inputs/day02.txt"
    let commands = map (parseIt parseCommand) $ lines contents
    let (p1x, p1y) = part1 (0, 0) commands
    let (p2x, p2y) = part2 (0, 0) 0 commands
    return (show (p1x * p1y), show (p2x * p2y))
