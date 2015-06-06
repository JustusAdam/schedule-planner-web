module Main where

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Signal exposing (Signal, Address)
import InputFields exposing (updateMailbox, actions)
import OutputFields
import Task exposing (Task)
import Http
import Maybe
import Types exposing (Model, emptyModel)



-- MAIN


initialModel : Model
initialModel = Maybe.withDefault emptyModel getStorage


dataTask : Signal (Task Http.Error ())
dataTask = Signal.map2 InputFields.doUpdate updateMailbox.signal model


model : Signal Model
model =
  Signal.foldp InputFields.update initialModel actions.signal


main : Signal Html
main =
  Signal.map2
    (\a b -> div [ class "row" ] [ a, b ])
    (InputFields.htmlSignal model)
    OutputFields.htmlSignal


port updatePort : Signal (Task Http.Error ())
port updatePort = dataTask


port getStorage : Maybe Model

port setStorage : Signal Model
port setStorage = model
