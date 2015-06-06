module OutputFields (htmlSignal, actions, Action(..)) where

import Html exposing (..)
import Html.Attributes exposing (class)
import Types exposing (Lesson)
import Util exposing (..)
import List.Extra exposing (zip)
import Text exposing (fromString)
import Graphics.Element exposing (leftAligned)


type alias Model = { lessons : List (Int, List Lesson) }


type Action
  = NoOp
  | Update (List (Int, List Lesson))


emptyModel : Model
emptyModel = { lessons = [] }


update : Action -> Model -> Model
update action model =
  case action of
    NoOp     -> model
    Update l -> { model | lessons <- l }


view : Model -> Html
view model =
  div
    []
    (List.map (scheduleView << snd) model.lessons)


scheduleView : List Lesson -> Html
scheduleView lessons =
  let
    cellField a = div [ class "column small-2 text-center" ] [ a ]
    grouped = groupBy (\l1 l2 -> l1.slot == l2.slot ) lessons
    sorted = List.sortBy (\(x::xs) -> x.day) <| List.map (List.sortBy (\l -> l.day)) grouped
    col elem ind =
      if elem.day == ind
        then elem.subject.name
        else "-"

    row l =
      List.reverse [0..5] |>
      List.foldr
        (\index l ->
          let
            (n, y) = l
            space = ""
          in
            case y of
              [] -> (space::n, y)
              (x::xs) ->
                if x.day == index
                  then (x.subject.name::n, xs)
                  else (space ::n, y))
        ([], l) |>
      fst
  in
    table
      [ class "column small-12" ]
      (hr [] (List.map (th [ class "column small-2 text-center" ] << (flip (::)[]) << text) (List.take 6 days)) ::
      List.map ((tr [] << List.map (cellField << text) ) << row) sorted)

-- SIGNALS

htmlSignal : Signal Html
htmlSignal = Signal.map view model


model : Signal Model
model = Signal.foldp update emptyModel actions.signal


actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp
