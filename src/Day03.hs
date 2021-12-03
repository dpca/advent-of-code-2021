module Day03 (solution) where

import Data.List

import Util (Solution, parseIt)

type Binary = String
type Gamma = Binary
type Epsilon = Binary

binaryToDecimal :: Binary -> Int
binaryToDecimal lst = sum $ map (2^) $ elemIndices '1' $ reverse lst

getGamma :: [Binary] -> Gamma
getGamma input = intercalate "" $ map mostCommon $ transpose input
    where mostCommon lst = [snd (maximum [ (length ks, head ks) | ks <- group (sort lst) ])]

getEpsilon :: Gamma -> Epsilon
getEpsilon = map (\x -> if x == '0' then '1' else '0')

part1 :: [Binary] -> Int
part1 input = binaryToDecimal gamma * binaryToDecimal epsilon
    where gamma = getGamma input
          epsilon = getEpsilon gamma

getOxygen :: [Binary] -> Binary
getOxygen = _getOxygen 0
    where _getOxygen _ [x] = x
          _getOxygen iter lst = _getOxygen (iter + 1) $ filterMostCommonAtPosition iter lst
              where filterMostCommonAtPosition iter lst = filter (\x -> x !! iter == gamma !! iter) lst
                    gamma = getGamma lst

getCo2 :: [Binary] -> Binary
getCo2 = _getCo2 0
    where _getCo2 _ [x] = x
          _getCo2 iter lst = _getCo2 (iter + 1) $ filterLeastCommonAtPosition iter lst
              where filterLeastCommonAtPosition iter lst = filter (\x -> x !! iter == epsilon !! iter) lst
                    gamma = getGamma lst
                    epsilon = getEpsilon gamma


part2 :: [Binary] -> Int
part2 input = binaryToDecimal oxygenRating * binaryToDecimal co2ScrubberRating
    where oxygenRating = getOxygen input
          co2ScrubberRating = getCo2 input

solution :: Solution
solution = do
    contents <- readFile "src/inputs/day03.txt"
    let inputs = lines contents
    let solution1 = part1 inputs
    let solution2 = part2 inputs
    return (show solution1, show solution2)
