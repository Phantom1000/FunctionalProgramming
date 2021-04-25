module Data.Dice where

import Control.Monad.Random
import Data.Bifunctor

data Dice
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    deriving (Eq, Ord, Show)

diceToInt :: Integral a => Dice -> a
diceToInt = \case
    One -> 1
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6

instance Num Dice where
    (+) x y = fromInteger $ diceToInt x + diceToInt y
    (*) x y = fromInteger $ diceToInt x * diceToInt y
    negate = id
    abs = fromInteger . abs . diceToInt
    signum = fromInteger . signum . diceToInt
    fromInteger = \case
        1 -> One
        2 -> Two
        3 -> Three
        4 -> Four
        5 -> Five
        _ -> Six

instance Random Dice where
    randomR (n, m) = first fromInteger . randomR (max 1 (diceToInt n), min 6 (diceToInt m))
    random = first fromInteger . randomR (1, 6)