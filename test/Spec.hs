import Test.Hspec

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05

main :: IO ()
main = hspec $ do
    describe "Solutions" $ do
        it "Day 1 returns the correct answers" $ do
            (part1, part2) <- Day01.solution
            part1 `shouldBe` "1477"
            part2 `shouldBe` "1523"

        it "Day 2 returns the correct answers" $ do
            (part1, part2) <- Day02.solution
            part1 `shouldBe` "2027977"
            part2 `shouldBe` "1903644897"

        it "Day 3 returns the correct answers" $ do
            (part1, part2) <- Day03.solution
            part1 `shouldBe` "749376"
            part2 `shouldBe` "2372923"

    describe "Day 4" $ do
        it "correctly solves the example" $ do
            (part1, part2) <- Day04.example
            part1 `shouldBe` "4512"
            part2 `shouldBe` "1924"

        it "correctly solves the full puzzle" $ do
            (part1, part2) <- Day04.solution
            part1 `shouldBe` "16716"
            part2 `shouldBe` "4880"

    describe "Day 5" $ do
        it "correctly solves the example" $ do
            (part1, part2) <- Day05.example
            part1 `shouldBe` "5"
            part2 `shouldBe` "12"

        it "correctly solves the full puzzle" $ do
            (part1, part2) <- Day05.solution
            part1 `shouldBe` "4745"
            part2 `shouldBe` "18442"
