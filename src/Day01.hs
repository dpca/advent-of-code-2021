module Day01 (solution) where

import Data.List

import Util (Solution)

part1 :: [Int] -> Int -> Int
part1 (x:y:ys) num
  | y > x = part1 (y:ys) (num + 1)
  | otherwise = part1 (y:ys) num
part1 _ num = num

part2 :: [Int] -> Int -> Int -> Int
part2 (x:y:z:zs) prev num
  | window > prev = part2 (y:z:zs) window (num + 1)
  | otherwise = part2 (y:z:zs) window num
  where window = x + y + z
part2 _ _ num = num

solution :: Solution
solution = do
    contents <- readFile "src/inputs/day01.txt"
    let lst = map read (words contents) :: [Int]
    let answer1 = show $ part1 lst 0
    let answer2 = show $ part2 lst 10000 0
    return (answer1, answer2)
