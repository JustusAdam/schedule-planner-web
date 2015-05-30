module InputFields (htmlSignal) where


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Signal exposing (Signal, Address)
import String
import Http
import Types exposing (Model, Lesson, Subject, emptyModel, emptyLesson, emptySubject)
import Util exposing (..)

days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]

type Action
  = NoOp
  | AddLesson
  | AddSubject
  | Submit
  | DeleteLesson Int
  | DeleteSubject Int
  | UpdateSubjectName String
  | UpdateSubject Subject
  | UpdateSlot Int
  | UpdateDay Int


update : Action -> Model -> Model
update action model =
  let
    sf = model.subjectField
    lf = model.lessonField
  in
    case action of
      NoOp                  -> model
      AddLesson             ->
        { model |
          lessons     <- model.lessons ++ [model.lessonField],
          nlid        <- model.nlid + 1,
          lessonField <- { lf | lid <- model.nlid }
        }
      DeleteLesson i        -> { model | lessons  <- List.filter (\t -> t.lid /= i) model.lessons }
      AddSubject            ->
        { model |
          subjects      <- model.subjects ++ [model.subjectField],
          nsid          <- model.nsid + 1,
          subjectField  <- { sid = model.nsid, name = "" }
        }
      DeleteSubject i       -> { model | subjects <- List.filter (\t -> t.sid /= i) model.subjects }
      Submit                -> model
      UpdateSubjectName str -> { model | subjectField <- { sf | name <- str } }
      UpdateSubject s       -> { model | lessonField <- { lf | subject <- s } }
      UpdateDay i           -> { model | lessonField <- { lf | day <- i} }
      UpdateSlot i          -> { model | lessonField <- { lf | slot <- i} }


-- VERIFIERS


lessonIsValid : Lesson -> Bool
lessonIsValid l = l.lid >= 0 && l.subject.sid >= 0


subjectIsValid : Subject -> Bool
subjectIsValid s = s.sid >= 0 && s.name /= ""


formReady : Model -> Bool
formReady m =
  not (List.isEmpty m.subjects || List.isEmpty m.lessons)


-- VIEW


view : Address Action -> Model -> Html
view address model =
  let
    enabled = formReady model
    buttonAction = if enabled then [onClick address Submit] else []
  in
    div
      [ class "row" ]
      [ div
        [ id "subjects-area", class "small-6 columns" ]
        [ subjectDisplay address model ]
      , div
        [ id "lessons-area", class "small-6 columns" ]
        [ lessonDisplay address model ]
      , a
        ([ classList [("button", True), ("success", True), ("disabled", not enabled)] ] ++ buttonAction)
        [ text ">>=" ]
      ]


subjectDisplay : Address Action -> Model -> Html
subjectDisplay a m =
  div
    []
    [ nav [] [ ul [ class "side-nav" ] (List.map (\s -> singleSubjectDisplay a s (m.lessonField.subject.sid == s.sid) ) m.subjects) ]
    , subjectFields a m
    ]


singleSubjectDisplay : Address Action -> Subject -> Bool -> Html
singleSubjectDisplay address s active =
  li
    [ classList [ ("active", active) ] ]
    [ a
      [ class "subject"
      , onClick address (UpdateSubject s)
      ]
      [ text s.name ]
    ]


subjectFields : Address Action -> Model -> Html
subjectFields address model =
  let
    enabled = subjectIsValid model.subjectField
    buttonAction = if enabled then [onClick address (AddSubject)] else []
  in
    div
      [ id "subject-inputs", class "row collapse" ]
      [ div
        [ class "small-10 column" ]
        [ input
          [ id "subject-name"
          , placeholder "Enter a Subject"
          , type' "text"
          , name "subject-name"
          , on "input" targetValue (Signal.message address << UpdateSubjectName)
          , onEnter address AddSubject
          ]
          [ text model.subjectField.name ]
        ]
      , div
        [ class "small-2 columns" ]
        [ a
          ([ id "new-subject"
          , classList [("button", True), ("postfix", True), ("disabled", not enabled)]
          ] ++ buttonAction )
          [ text "+" ]
        ]
      ]


lessonDisplay : Address Action -> Model -> Html
lessonDisplay a m =
  div
    [ class "row" ]
    (if List.isEmpty m.subjects
      then [ p [] [ text "Enter some Subjects" ] ]
      else [ div [ class "columns" ] (List.map (singleLessonDisplay a) m.lessons)
           , lessonFields a m
           ])

singleLessonDisplay : Address Action -> Lesson -> Html
singleLessonDisplay address l =
  div
    [ class "lesson row" ]
    [ div [ class "lesson-day small-3 columns" ] [ text (Maybe.withDefault "invalid" (days !! l.day))]
    , div [ class "lesson-slot small-3 columns" ] [ text (toString l.slot) ]
    , div [ class "lesson-subject small-3 columns" ] [ text l.subject.name ]
    , div [ class "columns small-3" ] [ a [ class "button alert postfix", onClick address (DeleteLesson l.lid)] [text "x"] ]
    ]


lessonFields : Address Action -> Model -> Html
lessonFields a m =
  let
    enabled        = lessonIsValid m.lessonField
    buttonAction   = if enabled then [onClick a (AddLesson)] else []
    updateSlot     = Signal.message a << withDefault NoOp UpdateSlot << String.toInt
    updateDay      = Signal.message a << withDefault NoOp UpdateDay << String.toInt
    slotToOption n = let v = toString n in option [value v] [text v]
    dayToOption n  = option [value (toString n)] [text (Maybe.withDefault "invalid" (days !! n))]
  in
    div
      [ class "row" ]
      [ div
        [ class "small-4 columns" ]
        [ select
            [ on "input" targetValue updateSlot ]
            (List.map slotToOption [0..9])
        ]
      , div
        [class "small-4 columns"]
        [ select
            [ on "input" targetValue updateDay]
            (List.map dayToOption [0..6])
        ]
      , div
        [ class "columns small-4" ]
        [ button
            ([ classList [("postfix", True), ("disabled", not enabled)] ] ++ buttonAction)
            [ text "+" ]
        ]
      ]


subToOption : Subject -> Html
subToOption s = option [ value (toString s.sid) ] [ text s.name ]


-- SIGNALS


htmlSignal : Signal Html
htmlSignal = Signal.map (view actions.address) model


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

model : Signal Model
model =
  Signal.foldp update initialModel actions.signal

initialModel : Model
initialModel = emptyModel
