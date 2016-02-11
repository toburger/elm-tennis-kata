module Actions (..) where

import Models exposing (Player)


type Action
  = PlayerScores Player
  | Restart
