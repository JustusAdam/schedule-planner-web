module OutputFields (htmlSignal, actions, Action(..)) where

import Html exposing (..)
import Html.Attributes exposing (class)
import Types exposing (Lesson)
import Util exposing (..)
import List.Extra exposing (zip)
import Constants exposing (..)


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
    cellField a = td [ class "lesson-field text-center" ] [ a ]
    grouped = groupBy (\l1 l2 -> l1.slot == l2.slot ) lessons -- group all lessons by their slots
    sorted = List.sortBy (\(x::xs) -> x.day) <| List.map (List.sortBy (\l -> l.day)) grouped
    fillList num l = -- fills the lesson list with empty cells for days/slots without actual lessons
      if num > maxSlot
        then []
        else
          case l of
            [] -> []:: fillList (num + 1) l
            (x::xs) ->
              case x of
                [] -> fillList num xs
                (y::ys) ->
                  if y.slot == num
                    then x::fillList (num + 1) xs
                    else []:: fillList (num + 1) l
    filledList = fillList 1 sorted
    col elem ind =
      if elem.day == ind
        then elem.subject.name
        else "-"

    row l =
      [minDay..maxDay] |>
      List.foldr
        (\index l ->
          let
            (n, y) = l
            space = nbsp
          in
            case y of
              [] -> (space :: n, y)
              (x::xs) ->
                if x.day == index
                  then (text x.subject.name :: n, xs)
                  else (space :: n, y))
        ([], l) |>
      fst
  in
    table
      [ class "column small-12" ]
      (tr [] (List.map (th [ class "lesson-field text-center" ] << (flip (::)[]) << text) days) ::
      List.map ((tr [] << List.map cellField) << row) filledList)

-- SIGNALS

htmlSignal : Signal Html
htmlSignal = Signal.map view model


model : Signal Model
model = Signal.foldp update emptyModel actions.signal


actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp
