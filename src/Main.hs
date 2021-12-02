import System.Environment (getArgs)

import Util (Solution)
import qualified Day01
import qualified Day02

solution :: Int -> Solution
solution 1 = Day01.solution
solution 2 = Day02.solution
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
