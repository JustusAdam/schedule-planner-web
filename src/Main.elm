module Main where

import Html exposing (Html)
import Signal exposing (Signal, Address)
import InputFields


-- CONSTANTS


recevier = "http://localhost:7097"

-- MAIN


main : Signal Html
main =
  InputFields.htmlSignal
