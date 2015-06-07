module InputFields
  ( doUpdate
  , updateMailbox
  , actions
  , update
  , htmlSignal
  ) where


import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Signal exposing (Signal, Address)
import String
import Http
import Types exposing (Rule, Model, Lesson, Subject, Target, emptyModel, emptyLesson, emptySubject, decode_schedule)
import Util exposing (..)
import Html.Lazy exposing (lazy2)
import OutputFields
import Task exposing (andThen, Task)
import Foundation exposing (tooltip)


recevier = "justusad.octans.uberspace.de:63013"


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
  | DeleteAllLessons
  | DeleteAllRules
  | DeleteAllSubjects
  | DeleteAll


type RequestAction = Waiting | RequestUpdate


update : Action -> Model -> Model
update action model =
  let
    sf = model.subjectField
    lf = model.lessonField

    updateRuleSlot value target = { target | slot <- value }

    updateRuleDay value target = { target | day <- value }

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
        { model |
          subjects <- List.filter (\t -> t.sid /= i) model.subjects,
          lessons  <- List.filter (\t -> t.subject.sid /= i) model.lessons
        }
      AddRule               ->
        if targetIsValid model.target
          then
            let
              newRule = { rid = model.nrid, target = model.target, severity = model.currentSeverity }
            in
              { model |
                rules <- newRule :: model.rules,
                nrid <- model.nrid + 1
              }
          else model
      DeleteRule i          -> { model | rules <- List.filter (\r -> r.rid /= i) model.rules }
      UpdateSubjectName str -> { model | subjectField <- { sf | name <- str } }
      UpdateSubject s       -> { model | lessonField <- { lf | subject <- s } }
      UpdateDay i           -> { model | lessonField <- { lf | day <- i} }
      UpdateSlot i          -> { model | lessonField <- { lf | slot <- i} }
      UpdateRuleDay v       -> { model | target <- updateRuleDay v model.target }
      UpdateRuleSlot v      -> { model | target <- updateRuleSlot v model.target }
      UpdateRuleTarget t    ->
        let
          oldTarget = model.target
        in
          { model | target <- { oldTarget | scope <- t } }
      UpdateSeverity v      -> { model | currentSeverity <- v }
      DeleteAllLessons      -> { model | lessons <- [] }
      DeleteAllRules        -> { model | rules <- [] }
      DeleteAllSubjects     ->
        { model |
          subjects <- [],
          lessons <- []
        }
      DeleteAll             -> emptyModel


-- VERIFIERS


lessonIsValid : Lesson -> Bool
lessonIsValid l = l.lid >= 0 && l.subject.sid >= 0


subjectIsValid : Subject -> Bool
subjectIsValid s = s.sid >= 0 && s.name /= ""


formReady : Model -> Bool
formReady m =
  not (List.isEmpty m.subjects || List.isEmpty m.lessons)


validTargetValues = [ "cell", "day", "slot" ]


targetIsValid : Target -> Bool
targetIsValid = flip List.member validTargetValues << .scope


-- VIEW


view : Address Action -> Model -> Html
view address model =
  let
    enabled = formReady model
    box = updateMailbox
    buttonAction =
      if enabled
        then (::) (onClick box.address RequestUpdate)
        else (++) (tooltip "Why don't you define some lessons?")
  in
    div
      [ class "row" ]
      [ div [ class "row" ]
        [ section
          [ Attr.id "subjects-area", class "small-6 columns" ]
          [ lazy2 subjectDisplay address model ]
        , section
          [ Attr.id "lessons-area", class "small-6 columns" ]
          [ lazy2 lessonDisplay address model ]
        ]
      , div [ class "row" ]
        [ div
          [ class "column small-12" ]
          [ section
            [ Attr.id "rule-area", class "small-6 columns" ]
            [ lazy2 ruleFields address model ]
          , div
            [ class "column small-6" ]
            [ a
              (buttonAction [ classList
                 [("button success expand", True), ("disabled", not enabled), ("has-tip", not enabled)] ])
              [ text ">>=" ]
            ]
          ]
        ]
      ]


subjectDisplay : Address Action -> Model -> Html
subjectDisplay address m =
  let
    deleteAllSubjectsButton =
      if List.isEmpty m.subjects
        then identity
        else (::) (a [ class "button alert expand", onClick address DeleteAllSubjects ] [ text "delete all" ])
  in
    div
      []
      [ nav
        []
        (deleteAllSubjectsButton [ ul
          [ class "side-nav" ]
          (List.map
            (\s -> singleSubjectDisplay address s (m.lessonField.subject.sid == s.sid) )
            m.subjects)
        ])
      , subjectFields address m
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
                ([ class "button alert postfix has-tip"
                , onClick address (DeleteSubject s.sid)
                ] ++ tooltip "You want to delete me?")
                [ text "x" ]
            ]
        ]
    ]


subjectFields : Address Action -> Model -> Html
subjectFields address model =
  let
    enabled      = subjectIsValid model.subjectField
    buttonAction =
      if enabled
        then (::) (onClick address AddSubject)
        else (++) (tooltip "You have to enter a name for the subject, before you can add it.")
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
            (buttonAction
              [ Attr.id "new-subject"
              , classList
                [ ("button postfix", True), ("disabled", not enabled), ("has-tip", not enabled) ]
              ])
            [ text "+" ]
          ]
        ]
      ]


lessonDisplay : Address Action -> Model -> Html
lessonDisplay address m =
  let
    deleteAllLessonsButton =
      if List.isEmpty m.lessons
        then identity
        else (::) (a [ class "button alert expand", onClick address DeleteAllLessons ] [ text "delete all lessons" ])
  in
  div
    [ class "row" ]
    (if List.isEmpty m.subjects
      then [ h3 [] [ text "Enter some subjects to get started" ] ]
      else
        [ div
          [ class "columns" ]
          (deleteAllLessonsButton (List.map (singleLessonDisplay address) m.lessons))
        , lessonFields address m
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
        ([ class "has-tip button alert postfix", onClick address (DeleteLesson l.lid) ] ++ tooltip "Do you want to delete me?")
        [ text "x" ]
      ]
    ]


lessonFields : Address Action -> Model -> Html
lessonFields address model =
  let
    enabled        = lessonIsValid model.lessonField

    buttonAction   =
      if enabled
        then (::) (onClick address (AddLesson))
        else (++) (tooltip "You should choose a subject first. It's really easy, just click on it.")

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
            (buttonAction [ classList [ ("button expand", True), ("disabled", not enabled), ("has-tip", not enabled) ] ])
            [ text "+" ]
          ]
        ]
      ]


subToOption : Subject -> Html
subToOption s = option [ value (toString s.sid) ] [ text s.name ]


ruleFields : Address Action -> Model -> Html
ruleFields address model =
  let
    deleteAllRulesButton =
      if List.isEmpty model.rules
        then identity
        else (::) (a [ class "button alert expand", onClick address DeleteAllRules ] [ text "delete all rules" ])
  in
    div [] (deleteAllRulesButton (List.map (ruleField address) model.rules ++ [ruleInput address model]))


ruleField : Address Action -> Rule -> Html
ruleField address { target, severity, rid }  =
  div
    [ class "row" ]
    [ div
      [ class "rule columns small-9" ]
      [ p [] [text ("Rule for " ++ targetToString target ++ " with a weight of " ++ toString severity) ] ]
    , div
      [ class "delete-rule columns small-2" ]
      [ a
        ([ class "has-tip alert button postfix", onClick address (DeleteRule rid) ] ++ tooltip "You want to delete me?")
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

    enabled = List.member model.target.scope ["scope", "cell", "day"]

    buttonAction =
      if enabled
        then (::) (onClick address AddRule)
        else (++) (tooltip "You have to specify a valid target before you can press me.")

    scopeToOption a =
      option
        [ selected (a == model.target.scope), value <| String.toLower a ]
        [ text a ]
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
            (List.map scopeToOption [ "None", "Cell", "Day", "Slot" ])
          ]
        ] ++
          (case model.target.scope of
            "cell" -> [uDayIn, uSlotIn]
            "day"  -> [uDayIn]
            "slot" -> [uSlotIn]
            _      -> [])
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
              (buttonAction [ classList [("button success expand", True), ("disabled", not enabled), ("has-tip", not enabled)] ])
              [ text "+" ]
            ]
          ]
        ]


targetToString : Types.Target -> String
targetToString { scope, day, slot } =
  case scope of
    "cell" -> "Cell on " ++ (Maybe.withDefault "invalid" <| days !! day) ++ " in slot " ++ toString slot
    "day"  -> "Day " ++ (Maybe.withDefault "invalid" <| days !! day)
    "slot" -> "Slot " ++ toString slot
    _      -> "invalid"


-- SIGNALS


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


getData : Encode.Value -> Task Http.Error (List (Int, List Lesson))
getData =
  Http.post
    decode_schedule recevier
  << Http.string
  << Encode.encode 0


htmlSignal : Signal Model -> Signal Html
htmlSignal = Signal.map (view actions.address)


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp
