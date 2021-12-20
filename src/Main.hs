import System.Environment (getArgs)

import Util (Solution)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11

solution :: Int -> Solution
solution 1 = Day01.solution
solution 2 = Day02.solution
solution 3 = Day03.solution
solution 4 = Day04.solution
solution 5 = Day05.solution
solution 6 = Day06.solution
solution 7 = Day07.solution
solution 8 = Day08.solution
solution 9 = Day09.solution
solution 10 = Day10.solution
solution 11 = Day11.solution
solution _ = return ("Not implemented", "Not implemented")

main :: IO ()
main = do
    args <- getArgs
    run args
        where
            usage = "Usage: stack run advent 1"
            run [] = print usage
            run [x] = do
                let day = read x :: Int
                (part1, part2) <- solution day
                print $ "Part1: " ++ part1
                print $ "Part2: " ++ part2
            run _ = print usage
