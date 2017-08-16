module Main where

import Dices

main :: IO ()
main = series Three 20 10 >>= \s -> mapM_ print $ zip s $ map sumd s
