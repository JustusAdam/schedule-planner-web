module Main where

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Signal exposing (Signal, Address)
import InputFields
import OutputFields
import Task exposing (Task)
import Http



-- MAIN


main : Signal Html
main =
  Signal.map2
    (\a b -> div [ class "row" ] [ a, b ])
    InputFields.htmlSignal
    OutputFields.htmlSignal


port updatePort : Signal (Task Http.Error ())
port updatePort = InputFields.dataTask
