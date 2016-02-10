module Main (..) where

import Html exposing (div, text, button)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import StartApp.Simple


type Player
  = PlayerOne
  | PlayerTwo


type Point
  = Love
  | Fifteen
  | Thirty


type alias PointsData =
  { playerOnePoint : Point
  , playerTwoPoint : Point
  }


type alias FortyData =
  { player : Player
  , otherPlayerPoint : Point
  }


type Score
  = Points PointsData
  | Forty FortyData
  | Deuce
  | Advantage Player
  | Game Player


type alias Model =
  Score


initialModel : Model
initialModel =
  Points
    { playerOnePoint = Love
    , playerTwoPoint = Love
    }


type Action
  = PlayerScores Player
  | Restart


toInt : Point -> Int
toInt point =
  case point of
    Love ->
      0

    Fifteen ->
      15

    Thirty ->
      30


comparePoint : Point -> Point -> Order
comparePoint point1 point2 =
  compare (toInt point1) (toInt point2)


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


isGameOver : Model -> Bool
isGameOver model =
  case model of
    Game _ ->
      True

    _ ->
      False


type Leader
  = Draw
  | Lead Player
  | Win Player


whichPlayerLeads : Model -> Leader
whichPlayerLeads model =
  case model of
    Points { playerOnePoint, playerTwoPoint } ->
      case comparePoint playerOnePoint playerTwoPoint of
        EQ ->
          Draw

        LT ->
          Lead PlayerTwo

        GT ->
          Lead PlayerOne

    Forty { player } ->
      Lead player

    Deuce ->
      Draw

    Advantage player ->
      Lead player

    Game player ->
      Win player


type GameScore
  = Number ( Int, Int )
  | Leader Player
  | Winner Player


gameScore : Model -> GameScore
gameScore model =
  case model of
    Points { playerOnePoint, playerTwoPoint } ->
      Number ( toInt playerOnePoint, toInt playerTwoPoint )

    Forty { player, otherPlayerPoint } ->
      if player == PlayerOne then
        Number ( 40, toInt otherPlayerPoint )
      else
        Number ( toInt otherPlayerPoint, 40 )

    Deuce ->
      Number ( 40, 40 )

    Advantage player ->
      Leader player

    Game player ->
      Winner player


viewPlayerScore : Model -> Html.Html
viewPlayerScore model =
  let
    msg =
      case whichPlayerLeads model of
        Draw ->
          "It's a draw"

        Lead player ->
          toString player ++ " leads"

        Win player ->
          toString player ++ " wins!"
    score =
      toString (gameScore model)
  in
    text (score ++ ": " ++ msg)


view : Signal.Address Action -> Model -> Html.Html
view address model =
  div
    []
    [ button
        [ onClick address (PlayerScores PlayerOne)
        , disabled (isGameOver model)
        ]
        [ text "player one scores" ]
    , button
        [ onClick address (PlayerScores PlayerTwo)
        , disabled (isGameOver model)
        ]
        [ text "player two scores" ]
    , div
        []
        [ viewPlayerScore model ]
    , button
        [ onClick address Restart ]
        [ text "Restart" ]
    ]


main : Signal Html.Html
main =
  StartApp.Simple.start
    { model = initialModel
    , update = update
    , view = view
    }
