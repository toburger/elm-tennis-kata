module View (view) where

import Html exposing (div, text, button)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Models exposing (..)
import Actions exposing (..)


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
