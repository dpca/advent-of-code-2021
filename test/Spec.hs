import Test.Hspec

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08

main :: IO ()
main = hspec $ do
    describe "Day 1" $ do
        it "correctly solves the puzzle" $ do
            (part1, part2) <- Day01.solution
            part1 `shouldBe` "1477"
            part2 `shouldBe` "1523"

    describe "Day 2" $ do
        it "correctly solves the puzzle" $ do
            (part1, part2) <- Day02.solution
            part1 `shouldBe` "2027977"
            part2 `shouldBe` "1903644897"

    describe "Day 3" $ do
        it "correctly solves the puzzle" $ do
            (part1, part2) <- Day03.solution
            part1 `shouldBe` "749376"
            part2 `shouldBe` "2372923"

    describe "Day 4" $ do
        it "correctly solves the example" $ do
            (part1, part2) <- Day04.example
            part1 `shouldBe` "4512"
            part2 `shouldBe` "1924"

        it "correctly solves the puzzle" $ do
            (part1, part2) <- Day04.solution
            part1 `shouldBe` "16716"
            part2 `shouldBe` "4880"

    describe "Day 5" $ do
        it "correctly solves the example" $ do
            (part1, part2) <- Day05.example
            part1 `shouldBe` "5"
            part2 `shouldBe` "12"

        it "correctly solves the puzzle" $ do
            (part1, part2) <- Day05.solution
            part1 `shouldBe` "4745"
            part2 `shouldBe` "18442"

    describe "Day 6" $ do
        it "correctly solves the example" $ do
            (part1, part2) <- Day06.example
            part1 `shouldBe` "5934"
            part2 `shouldBe` "26984457539"

        it "correctly solves the puzzle" $ do
            (part1, part2) <- Day06.solution
            part1 `shouldBe` "358214"
            part2 `shouldBe` "1622533344325"

    describe "Day 7" $ do
        it "correctly solves the example" $ do
            (part1, part2) <- Day07.example
            part1 `shouldBe` "37"
            part2 `shouldBe` "168"

        it "correctly solves the puzzle" $ do
            (part1, part2) <- Day07.solution
            part1 `shouldBe` "353800"
            part2 `shouldBe` "98119739"

    describe "Day 8" $ do
        it "correctly solves the example" $ do
            (part1, part2) <- Day08.example
            part1 `shouldBe` "26"
            part2 `shouldBe` "61229"

        it "correctly solves the puzzle" $ do
            (part1, part2) <- Day07.solution
            part1 `shouldBe` "255"
            part2 `shouldBe` "982158"
