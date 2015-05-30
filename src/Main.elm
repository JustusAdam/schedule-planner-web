module Main where

import Html exposing (Html)
import Signal exposing (Signal, Address)
import InputFields
import Task exposing (Task)
import Http



-- MAIN


main : Signal Html
main =
  InputFields.htmlSignal


port updatePort : Signal (Task Http.Error ())
port updatePort = InputFields.dataTask
