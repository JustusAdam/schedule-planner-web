module Foundation where


import Html exposing (..)
import Html.Attributes exposing (..)



tooltip : String -> List Attribute
tooltip m =
  [ attribute "data-hastooltip" ""
  , attribute "title" m
  , attribute "aria-haspopup" "true" 
  ]
