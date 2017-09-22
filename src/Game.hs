module Game where

import Control.Monad.State
import RPParty

data Game = Game {
  party1 :: Party,
  party2 :: Party
}

type GameState = StateT Game IO Bool

endgame :: Game -> Bool
endgame g = any (allDead . (\p -> p g)) [party1, party2]

winner :: Game -> Maybe Party
winner g
  | endgame g = if allDead (party1 g) then return (party2 g) else return (party1 g)
  | otherwise = Nothing

firstHalf :: Game -> IO Game
firstHalf g = do
  pn2 <- halfRound (party1 g) (party2 g)
  return g { party2 = pn2 }

secondHalf :: Game -> IO Game
secondHalf g = do
  pn1 <- halfRound (party2 g) (party1 g)
  return g { party1 = pn1 }

playRound :: GameState
playRound = do
  g  <- get
  gn1 <- lift $ firstHalf g
  if endgame gn1 then do
    put gn1
    return (endgame gn1)
  else do
    gn2 <- lift $ secondHalf gn1
    put gn2
    return (endgame gn2)

playGame :: GameState
playGame = do
    g <- get
    if endgame g then 
      return (endgame g)
    else do
      gn <- lift $ execStateT playRound g
      put gn
      playGame
