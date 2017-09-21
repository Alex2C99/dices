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

fullRound :: Game -> IO Game
fullRound g = do
  pn2 <- halfRound (party1 g) (party2 g)
  pn1 <- halfRound pn2 (party1 g)
  return Game { party1 = pn1, party2 = pn2 }

playRound :: GameState
playRound = do
  g  <- get
  gn <- lift $ fullRound g
  put gn
  return (endgame gn)

