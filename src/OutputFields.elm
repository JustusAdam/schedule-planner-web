module OutputFields (htmlSignal, actions, Action(..)) where

import Html exposing (..)
import Html.Attributes exposing (class)
import Types exposing (Lesson)
import Util exposing (..)
import List.Extra exposing (zip)


type alias Model = { lessons : List Lesson }


type Action
  = NoOp
  | Update (List Lesson)


emptyModel : Model
emptyModel = { lessons = [] }


update : Action -> Model -> Model
update action model =
  case action of
    NoOp     -> model
    Update l -> { model | lessons <- l }


view : Model -> Html
view model =
  let
    grouped = groupBy (\l1 l2 -> l1.slot == l2.slot ) model.lessons
    sorted = List.sortBy (\(x::xs) -> x.day) <| List.map (List.sortBy (\l -> l.day)) grouped
    col elem ind =
      if elem.day == ind
        then elem.subject.name
        else "-"

    row l =
      List.reverse [0..10] |>
      List.foldr
        (\index l ->
          let
            (n, y) = l
          in
            case y of
              [] -> ("&nbsp;"::n, y)
              (x::xs) ->
                if x.day == index
                  then (x.subject.name::n, xs)
                  else ("&nbsp;"::n, y))
        ([], l) |>
      fst
  in
    div
      []
      (List.map ((div [ class "row" ] << List.map (\a -> div [ class "column small-2" ] [ text a ]) ) << row) sorted)

-- SIGNALS

htmlSignal : Signal Html
htmlSignal = Signal.map view model


model : Signal Model
model = Signal.foldp update emptyModel actions.signal


actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp
