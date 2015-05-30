module Com (requestUpdate) where

import Types exposing (Lesson)
import Html exposing (..)
import Task exposing (Task)
import OutputFields


type RequestUpdate = NoOp | RequestUpdate (List Lesson)



updateBox : Signal.Mailbox
updateBox = Signal.mailbox NoOp


requestUpdate : List Lesson -> Message
requestUpdate data' = Signal.message updateBox.address (RequestUpdate data')


update : List Lesson -> Task x ()
update data' = Signal.send (Signal.message OutputFields.actions.address (OutputFields.Update data'))


getData : Task Error (List Lesson)
getData =
