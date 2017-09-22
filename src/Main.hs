module Main where

import Control.Monad.State
import RPParty
import Game

initialGame :: IO Game 
initialGame = do
  p1 <- genPartyFromTemplate "Adventurers" ["Uhus", "Mops"] [fighterTemlate, fighterTemlate]
  p2 <- genPartyFromTemplate "Rogues"      ["Orc", "Ogr"]  [fighterTemlate, fighterTemlate]
  return Game { party1 = p1, party2 = p2}

outMaybe :: (a -> IO ()) -> Maybe a -> IO ()
outMaybe f (Just a) = f a
outMaybe _ Nothing  = return ()

main :: IO ()
main = do
  g <- initialGame
  gfin <- execStateT playGame g
  putStr "Winner: "
  outMaybe (putStrLn . title) (winner gfin)
