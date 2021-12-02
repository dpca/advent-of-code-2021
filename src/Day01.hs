module Day01 (solution) where

import Util (Solution)

part1 :: [Int] -> Int -> Int
part1 (x:y:ys) count
  | y > x = part1 (y:ys) (count + 1)
  | otherwise = part1 (y:ys) count
part1 _ count = count

part2 :: [Int] -> Int -> Int
part2 (w:x:y:z:zs) count
  | windowB > windowA = part2 (x:y:z:zs) count + 1
  | otherwise         = part2 (x:y:z:zs) count
  where windowA = w + x + y
        windowB = x + y + z
part2 _ count = count

solution :: Solution
solution = do
    contents <- readFile "src/inputs/day01.txt"
    let lst = map read (words contents) :: [Int]
    let answer1 = show $ part1 lst 0
    let answer2 = show $ part2 lst 0
    return (answer1, answer2)
