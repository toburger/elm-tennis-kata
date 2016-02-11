module Update (update) where

import Models exposing (..)
import Actions exposing (..)


advancePoint : Point -> Maybe Point
advancePoint playerPoint =
  case playerPoint of
    Love ->
      Just Fifteen

    Fifteen ->
      Just Thirty

    Thirty ->
      Nothing


advancePointsData : Player -> PointsData -> Score
advancePointsData player ({ playerOnePoint, playerTwoPoint } as score) =
  if player == PlayerOne then
    case advancePoint playerOnePoint of
      Just points ->
        Points { score | playerOnePoint = points }

      Nothing ->
        Forty
          { player = player
          , otherPlayerPoint = playerTwoPoint
          }
  else
    case advancePoint playerTwoPoint of
      Just points ->
        Points { score | playerTwoPoint = points }

      Nothing ->
        Forty
          { player = player
          , otherPlayerPoint = playerOnePoint
          }


advanceForty : Player -> FortyData -> Model
advanceForty player fortyData =
  if fortyData.player == player then
    Game player
  else
    case advancePoint fortyData.otherPlayerPoint of
      Just point ->
        Forty { fortyData | otherPlayerPoint = point }

      Nothing ->
        Deuce


advanceDeuce : Player -> Model
advanceDeuce player =
  Advantage player


advanceAdvantage : Player -> Player -> Model
advanceAdvantage player player' =
  if player' == player then
    Game player
  else
    Deuce


playerScores : Player -> Model -> Model
playerScores player model =
  case model of
    Points pointsData ->
      advancePointsData player pointsData

    Forty fortyData ->
      advanceForty player fortyData

    Deuce ->
      advanceDeuce player

    Advantage player' ->
      advanceAdvantage player player'

    Game player ->
      model


update : Action -> Model -> Model
update action model =
  case action of
    PlayerScores player ->
      playerScores player model

    Restart ->
      initialModel
