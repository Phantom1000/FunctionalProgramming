module Game where

import Control.Monad.Random
import Data.Dice
import Data.List
import Types

sortedThrows :: Int -> Rand StdGen [Dice]
sortedThrows n = sortBy (flip compare) <$> replicateM n getRandom

decideBattle :: Battlefield -> (Dice, Dice) -> Battlefield
decideBattle battlefield (attack, defense) =
    if attack > defense
    then battlefield {defenders = pred battlefield.defenders}
    else battlefield {attackers = pred battlefield.attackers}

battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield = do
    (attackersThrows, defendersThrows) <- (,) <$> sortedThrows attackers <*> sortedThrows defenders
    pure $ foldl decideBattle battlefield $ zip attackersThrows defendersThrows
  where 
    attackers = if battlefield.attackers > 3 
                then 3 
                else min (battlefield.attackers - 1) 3
    defenders = if battlefield.defenders > 2 
                then 2 
                else battlefield.defenders

invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield =
    if battlefield.attackers <= 1 || battlefield.defenders <= 0 
    then pure battlefield
    else invade =<< battle battlefield

successProb :: Battlefield -> Rand StdGen Double
successProb battlefield = do
    battles <- replicateM 1000 $ invade battlefield
    let filteredBattles = filter (\x -> x.defenders == 0) battles
    pure $ fromIntegral (length filteredBattles) / 1000.0