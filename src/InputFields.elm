module InputFields (htmlSignal, dataTask) where


import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Signal exposing (Signal, Address)
import String
import Http
import Types exposing (Rule, Model, Lesson, Subject, Target(..), emptyModel, emptyLesson, emptySubject)
import Util exposing (..)
import Html.Lazy exposing (lazy2)
import OutputFields
import Task exposing (andThen, Task)


days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]


type Action
  = NoOp
  | AddLesson
  | AddSubject
  | DeleteLesson Int
  | DeleteSubject Int
  | UpdateSubjectName String
  | UpdateSubject Subject
  | UpdateSlot Int
  | UpdateDay Int
  | AddRule
  | UpdateRuleSlot Int
  | UpdateRuleDay Int
  | UpdateRuleTarget String
  | DeleteRule Int
  | UpdateSeverity Int


update : Action -> Model -> Model
update action model =
  let
    sf = model.subjectField
    lf = model.lessonField

    updateRuleSlot value target =
      case target of
        Cell d _ -> Cell d value
        Slot _   -> Slot value
        _        -> target

    updateRuleDay value target =
      case target of
        Cell _ s -> Cell value s
        Day _    -> Day value
        _        -> target

  in
    case action of
      NoOp                  -> model
      AddLesson             ->
        { model |
          lessons     <- model.lessonField :: model.lessons,
          nlid        <- model.nlid + 1,
          lessonField <- { lf | lid <- model.nlid }
        }
      DeleteLesson i        ->
        { model | lessons  <- List.filter (\t -> t.lid /= i) model.lessons }
      AddSubject            ->
        { model |
          subjects      <- model.subjectField :: model.subjects,
          nsid          <- model.nsid + 1,
          subjectField  <- { sid = model.nsid, name = "" }
        }
      DeleteSubject i       ->
        { model | subjects <- List.filter (\t -> t.sid /= i) model.subjects }
      AddRule               ->
        Maybe.withDefault model <| Maybe.map
        (\a ->
          let
            newRule = { rid = model.nrid, target = a, severity = model.currentSeverity }
          in
            { model |
              rules <- newRule :: model.rules,
              nrid <- model.nrid + 1
            }) model.target
      DeleteRule i          -> { model | rules <- List.filter (\r -> r.rid /= i) model.rules }
      UpdateSubjectName str -> { model | subjectField <- { sf | name <- str } }
      UpdateSubject s       -> { model | lessonField <- { lf | subject <- s } }
      UpdateDay i           -> { model | lessonField <- { lf | day <- i} }
      UpdateSlot i          -> { model | lessonField <- { lf | slot <- i} }
      UpdateRuleDay v       -> { model | target <- Maybe.map (updateRuleDay v) model.target }
      UpdateRuleSlot v      -> { model | target <- Maybe.map (updateRuleSlot v) model.target }
      UpdateRuleTarget t    ->
        case t of
          "Cell"  -> { model | target <- Just (Cell 0 0) }
          "Day"   -> { model | target <- Just (Day 0) }
          "Slot"  -> { model | target <- Just (Slot 0) }
          "None"  -> { model | target <- Nothing  }
          _       -> model
      UpdateSeverity v      -> { model | currentSeverity <- v }


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
    box = updateMailbox
    buttonAction = if enabled then (::) (onClick box.address RequestUpdate) else Util.id
  in
    div
      [ class "row" ]
      [ section
        [ Attr.id "subjects-area", class "small-6 columns" ]
        [ lazy2 subjectDisplay address model ]
      , section
        [ Attr.id "lessons-area", class "small-6 columns" ]
        [ lazy2 lessonDisplay address model ]
      , section
        [ Attr.id "rule-area", class "small-6 columns" ]
        [ lazy2 ruleFields address model ]
      , a
        (buttonAction [ classList
           [("button success large", True), ("disabled", not enabled)] ])
        [ text ">>=" ]
      ]


subjectDisplay : Address Action -> Model -> Html
subjectDisplay a m =
  div
    []
    [ nav
      []
      [ ul
        [ class "side-nav" ]
        (List.map
          (\s -> singleSubjectDisplay a s (m.lessonField.subject.sid == s.sid) )
          m.subjects)
      ]
    , subjectFields a m
    ]


singleSubjectDisplay : Address Action -> Subject -> Bool -> Html
singleSubjectDisplay address s active =
  li
    [ classList [ ("active", active) ] ]
    [ div
        [ class "row collapse" ]
        [ div
            [ class "subject column small-10" ]
            [ a
                [ onClick address (UpdateSubject s) ]
                [ text s.name ]
            ]
        , div
            [ class "remove-subject small-2 column" ]
            [ a
                [ class "button alert postfix"
                , onClick address (DeleteSubject s.sid)
                ]
                [ text "x" ]
            ]
        ]
    ]


subjectFields : Address Action -> Model -> Html
subjectFields address model =
  let
    enabled = subjectIsValid model.subjectField
    buttonAction = if enabled then (::) (onClick address (AddSubject)) else Util.id
  in
    div
      []
      [ div
        [ class "row" ]
        [ div
          [ class "columns asmall-12" ]
          [ label
            [ for "subject-name" ]
            [ text "Add subjects" ]
          ]
        ]
      , div
        [ Attr.id "subject-inputs", class "row collapse" ]
        [ div
          [ class "small-10 column" ]
          [ input
            [ Attr.id "subject-name"
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
            (buttonAction [ Attr.id "new-subject"
            , classList
              [ ("button", True), ("postfix", True), ("disabled", not enabled) ]
            ])
            [ text "+" ]
          ]
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
    [ div
      [ class "lesson-day small-3 columns" ]
      [ text (Maybe.withDefault "invalid" (days !! l.day)) ]
    , div [ class "lesson-slot small-3 columns" ] [ text (toString l.slot) ]
    , div [ class "lesson-subject small-3 columns" ] [ text l.subject.name ]
    , div
      [ class "lesson-delete columns small-3" ]
      [ a
        [ class "button alert postfix", onClick address (DeleteLesson l.lid)]
        [ text "x" ]
      ]
    ]


lessonFields : Address Action -> Model -> Html
lessonFields address model =
  let
    enabled        = lessonIsValid model.lessonField

    buttonAction   = if enabled
                       then [ onClick address (AddLesson) ]
                       else []

    updateSlot     = Signal.message address << withDefault NoOp UpdateSlot << String.toInt
    updateDay      = Signal.message address << withDefault NoOp UpdateDay << String.toInt

    slotToOption n = let
                       v = toString n
                     in
                       option [ value v ] [ text v ]

    dayToOption n  = option
                       [ value (toString n) ]
                       [ text (Maybe.withDefault "invalid" (days !! n)) ]
  in
    div
      []
      [ div
        [ class "row" ]
        [ div
          [ class "small-4 columns" ]
          [ label
            [ for "update-slot" ]
            [ text "Enter Slot" ]
          , select
            [ Attr.id "update-slot", on "input" targetValue updateSlot ]
            (List.map slotToOption [0..9])
          ]
        , div
          [ class "small-4 columns" ]
          [ label
            [ for "update-day" ]
            [ text "Enter Day" ]
          , select
            [ Attr.id "update-day", on "input" targetValue updateDay ]
            (List.map dayToOption [0..6])
          ]
        ]
      , div
        [ class "row" ]
        [ div
          [ class "columns small-12" ]
          [ a
            ([ classList [ ("button expand", True), ("disabled", not enabled) ] ]
              ++ buttonAction)
            [ text "+" ]
          ]
        ]
      ]


subToOption : Subject -> Html
subToOption s = option [ value (toString s.sid) ] [ text s.name ]


ruleFields : Address Action -> Model -> Html
ruleFields address model =
  div [] (List.map (ruleField address) model.rules ++ [ruleInput address model])


ruleField : Address Action -> Rule -> Html
ruleField address {target, severity, rid}  =
  div
    [ class "row" ]
    [ div
      [ class "rule columns small-9" ]
      [ text ("Rule for " ++ targetToString target ++ " with a weight of " ++ toString severity) ]
    , div
      [ class "delete-rule columns small-3" ]
      [ a
        [ class "alert button postfix", onClick address (DeleteRule rid) ]
        [ text "x" ]
      ]
    ]


ruleInput : Address Action -> Model -> Html
ruleInput address model =
  let
    toOption i = option [ value (toString i) ] [ text <| Maybe.withDefault "invalid" (days !! i) ]
    updateTarget = Signal.message address << UpdateRuleTarget
    updateDay = Signal.message address << withDefault NoOp UpdateRuleDay << String.toInt
    updateSlot = Signal.message address << withDefault NoOp UpdateRuleSlot << String.toInt
    updateSeverity = Signal.message address << withDefault NoOp UpdateSeverity << String.toInt
    uDayIn = div
              [ class "columns small-6" ]
              [ label
                [ for "update-rule-day" ]
                [ text "Target Day" ]
              , select
                [ Attr.id "update-rule-day", on "input" targetValue updateDay ]
                (List.map toOption [0..List.length days])
              ]
    uSlotIn = div
                [ class "small-6 columns" ]
                [ label
                  [ for "update-rule-slot" ]
                  [ text "Target Slot" ]
                , select
                  [ Attr.id "update-rule-slot", on "input" targetValue updateSlot ]
                  (List.map (\a -> option [] [ text <| toString a ]) [0..10])
                ]

    enabled = isJust model.target

    buttonAction l =
      if enabled
        then onClick address AddRule :: l
        else l

  in
    div
      []
      [ div
        [ class "row" ]
        ([ div
          [ class "columns small-6" ]
          [ label
            [ for "update-target" ]
            [ text "Select Target" ]
          , select
            [ Attr.id "update-target",  on "input" targetValue updateTarget ]
            (List.map (\a -> option [ value a ] [ text a ]) [ "None", "Cell", "Day", "Slot" ])
          ]
        ] ++
          (case model.target of
            Nothing -> []
            Just targ ->
              case targ of
                Cell _ _ -> [uDayIn, uSlotIn]
                Day _ -> [uDayIn]
                Slot _ -> [uSlotIn])
          ++  [ div
                [ class "columns small-6" ]
                [ label
                  [ for "update-severity" ]
                  [ text "Select Rule impact" ]
                , select
                  [ Attr.id "update-severity", on "input" targetValue updateSeverity ]
                  (List.map ((\a -> option [ value a ] [text a]) << toString) [0..10])
                ]
              ]
            )
        , div
          [ class "row" ]
          [ div
            [ class "column small-12" ]
            [ a
              (buttonAction [ classList [("button success expand", True), ("disabled", not enabled)] ])
              [ text "+" ]
            ]
          ]
      ]


targetToString : Types.Target -> String
targetToString target =
  case target of
    Types.Cell a b -> "Cell on " ++ (Maybe.withDefault "invalid" <| days !! a) ++ " in slot " ++ toString b
    Types.Day d    -> "Day " ++ (Maybe.withDefault "invalid" <| days !! d)
    Types.Slot s   -> "Slot " ++ toString s


-- SIGNALS

recevier = "http://localhost:7097"


type RequestAction = Waiting | RequestUpdate


updateMailbox : Signal.Mailbox RequestAction
updateMailbox = Signal.mailbox Waiting


dummyTask : Task Http.Error (List Lesson)
dummyTask =
  Task.succeed
    [ { subject = { name = "wirjvn", sid = 0}, lid = 0, day = 0, slot = 0 }
    , { subject = { name = "wirjvn", sid = 0}, lid = 0, day = 0, slot = 2 }
    , { subject = { name = "wirjvn", sid = 0}, lid = 0, day = 0, slot = 3 }
    ]


doUpdate : RequestAction -> Model -> Task Http.Error ()
doUpdate action model =
  let
    act = OutputFields.actions
  in
    case action of
      Waiting       -> Task.succeed ()
      RequestUpdate ->
        Signal.send updateMailbox.address Waiting
        `andThen` (const (getData (Types.encode_datafile model)))
        -- `andThen` (\_ -> dummyTask)
        `andThen` (Signal.send act.address << OutputFields.Update)


dataTask : Signal (Task Http.Error ())
dataTask = Signal.map2 doUpdate updateMailbox.signal model


getData : Encode.Value -> Task Http.Error (List Lesson)
getData =
  Http.post
    (Decode.list Types.decode_lesson) recevier
  << Http.string
  << Encode.encode 0


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
