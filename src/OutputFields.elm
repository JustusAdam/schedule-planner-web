module OutputFields (htmlSignal, actions, Action(..)) where

import Html exposing (..)
import Types exposing (Lesson)


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
  div
    []
    (List.map (text << .name << .subject ) model.lessons )

-- SIGNALS

htmlSignal : Signal Html
htmlSignal = Signal.map view model


model : Signal Model
model = Signal.foldp update emptyModel actions.signal


actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp
