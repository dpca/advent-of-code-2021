module Day08 (solution, example) where

import Util (Solution, parseFile)

import Data.List
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String

-- display goes
--
-- 0:      1:      2:      3:      4:
--  aaaa    ....    aaaa    aaaa    ....
-- b    c  .    c  .    c  .    c  b    c
-- b    c  .    c  .    c  .    c  b    c
--  ....    ....    dddd    dddd    dddd
-- e    f  .    f  e    .  .    f  .    f
-- e    f  .    f  e    .  .    f  .    f
--  gggg    ....    gggg    gggg    ....
--
-- 5:      6:      7:      8:      9:
--  aaaa    aaaa    aaaa    aaaa    aaaa
-- b    .  b    .  .    c  b    c  b    c
-- b    .  b    .  .    c  b    c  b    c
--  dddd    dddd    ....    dddd    dddd
-- .    f  e    f  .    f  e    f  .    f
-- .    f  e    f  .    f  e    f  .    f
--  gggg    gggg    ....    gggg    gggg
--
-- with length 2 = 1
-- with length 3 = 7
-- with length 4 = 4
-- with length 5 = 2, 3, 5
-- with length 6 = 0, 6, 9
-- with length 7 = 8
--
-- 3 = only length 5 w/ 1 in it
-- 6 = only length 6 w/out 1 in it
-- 9 = only length 6 w/ 4 in it
-- 5 = length 5 missing 1 from 6

possibleInputs :: String
possibleInputs = "abcdefg"

lineParser :: Parser ([String], [String])
lineParser = do
    leftSide <- endBy (many1 (oneOf possibleInputs)) space
    string "| "
    rightSide <- sepBy (many1 (oneOf possibleInputs)) (char ' ')
    return (leftSide, rightSide)

inputParser :: Parser [([String], [String])]
inputParser = endBy lineParser newline

isUnique :: String -> Bool
isUnique entry = length entry `elem` [2, 3, 4, 7]

part1 :: [([String], [String])] -> Int
part1 input = sum $ map (\(_, outputs) -> length $ filter isUnique outputs) input

createMap :: [String] -> Map.Map Int String
createMap input = run Map.empty $ map sort input
    where run m entries
            | length m == 10 = m
            | notFound 1 = findMatch 1 (\x -> length x == 2)
            | notFound 7 = findMatch 7 (\x -> length x == 3)
            | notFound 4 = findMatch 4 (\x -> length x == 4)
            | notFound 8 = findMatch 8 (\x -> length x == 7)
            | notFound 3 = findMatch 3 (\x -> length x == 5 && isContained 1 x)
            | notFound 6 = findMatch 6 (\x -> length x == 6 && not (isContained 1 x))
            | notFound 9 = findMatch 9 (\x -> length x == 6 && isContained 4 x)
            | notFound 5 = findMatch 5 (\x -> length x == 5 && length (mapAt 6 \\ x) == 1)
            | notFound 2 = findMatch 2 (\x -> length x == 5 && x /= mapAt 3 && x /= mapAt 5)
            | notFound 0 = findMatch 0 (\x -> length x == 6 && x /= mapAt 6 && x /= mapAt 9)
            | otherwise = error "Couldn't populate map"
            where notFound n = Map.notMember n m
                  findMatch num fn = run (insertIfExists num fn) entries
                  isContained idx str = all (`elem` str) (m Map.! idx)
                  mapAt n = m Map.! n
                  insertIfExists len fn =
                      case find fn entries of
                        Just entry -> Map.insert len entry m
                        Nothing -> m

part2 :: [([String], [String])] -> Int
part2 input = sum outputValues
    where outputValues = map calculateOutput input
          calculateOutput (input, output) = outputToNum $ mapLookup output (invertMap $ createMap input)
          invertMap m = Map.fromList [(v, k) | (k, v) <- Map.toList m]
          mapLookup output m = map (\x -> m Map.! sort x) output
          outputToNum out = sum $ zipWith (\x e -> x * (10 ^ e)) out (reverse [0..3])

example :: Solution
example = do
    input <- parseFile inputParser "src/inputs/day08_example.txt"
    let solution1 = part1 input
    let solution2 = part2 input
    return (show solution1, show solution2)

solution :: Solution
solution = do
    input <- parseFile inputParser "src/inputs/day08.txt"
    let solution1 = part1 input
    let solution2 = part2 input
    return (show solution1, show solution2)
