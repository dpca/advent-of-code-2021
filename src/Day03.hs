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

getBinaryFromMatcher :: ([Binary] -> Binary) -> [Binary] -> Binary
getBinaryFromMatcher = getBin 0
    where getBin _ _ [x] = x
          getBin pos matcher lst = getBin (pos + 1) matcher $ filterWithMatcher (matcher lst) pos lst
          filterWithMatcher matcher position = filter (\x -> x !! position == matcher !! position)

getOxygen :: [Binary] -> Binary
getOxygen = getBinaryFromMatcher getGamma

getCo2 :: [Binary] -> Binary
getCo2 = getBinaryFromMatcher (getEpsilon . getGamma)

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
