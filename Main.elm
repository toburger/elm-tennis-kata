module Main (..) where

import Html
import StartApp.Simple
import Models exposing (initialModel)
import Update exposing (update)
import View exposing (view)


main : Signal Html.Html
main =
  StartApp.Simple.start
    { model = initialModel
    , update = update
    , view = view
    }
