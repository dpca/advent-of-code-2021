import Test.Hspec

import qualified Day01
import qualified Day02
import qualified Day03

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
