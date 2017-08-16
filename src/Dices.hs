module Dices where

import System.Random

data Throw = One | Two | Three

data Dice = Single Int
          | Double Int Int
          | Triple Int Int Int
          deriving (Show)

sumd :: Dice -> Int
sumd (Single x) = x
sumd (Double x y) = x + y
sumd (Triple x y z) = x + y + z

cast :: Throw -> Int -> IO Dice
cast One mx = do
    r <- randomRIO (1, mx)
    return (Single r)

cast Two mx = do
    (Single r1) <- cast One mx
    (Single r2) <- cast One mx
    return (Double r1 r2)

cast Three mx = do
    (Double r1 r2) <- cast Two mx
    (Single r3)    <- cast One mx
    return (Triple r1 r2 r3)

series :: Throw -> Int -> Int -> IO [Dice]
series _ _ 0 = return []
series c mx n = do
    d <- cast c mx
    ds <- series c mx (n-1)
    return (d : ds)
