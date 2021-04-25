module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Random
import System.IO.Unsafe

import Game
import Types

main :: IO ()
main = hspec $ describe "Game of dice" $ do
    describe "Excercise 2" $
      it "Army decrease" $
        property $ checkDecrease (10, 10) $ unsafePerformBattle $ battle (Battlefield 10 10)

    describe "Excercise 3" $
      it "Army destroyed" $ 
        property $ checkDestroyed $ unsafePerformBattle $ invade (Battlefield 10 10)

    describe "Excercise 4" $
      it "1 probability with no defenders" $
        unsafePerformBattle (successProb (Battlefield 10 0)) `shouldBe` 1

    describe "Excercise 4" $
      it "0 probability with no attackers" $
        unsafePerformBattle (successProb (Battlefield 0 10)) `shouldBe` 0

unsafePerformBattle :: Rand StdGen a -> a
unsafePerformBattle = unsafePerformIO . evalRandIO

checkDecrease :: (Int, Int) -> Battlefield -> Bool
checkDecrease (attackers, defenders) battlefield = battlefield.attackers < attackers || battlefield.defenders < defenders

checkDestroyed :: Battlefield -> Bool
checkDestroyed battlefield = battlefield.attackers == 1 || battlefield.defenders == 0
