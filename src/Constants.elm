module Constants where

import Html exposing (span, text)
import Html.Attributes exposing (style)

receiver = "http://justusad.octans.uberspace.de:63013"
-- receiver = "http://localhost:8989"


minSlot = 1
maxSlot = 9
minDay = 0
maxDay = 6


colorTransparent = "rgba(0,0,0,0)"
textTransparent = [("color", colorTransparent)]
styleTransparent = style textTransparent

nbsp = span [ styleTransparent ] [ text "-" ]
