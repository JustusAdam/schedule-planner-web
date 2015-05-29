module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Signal exposing (Signal, Address)
import String
import Window

-- MODEL

type alias Model =
  { lessons           : List Lesson
  , subjects          : List Subject
  , lessonField       : Lesson
  , subjectField      : Subject
  , nsid              : Int
  , nlid              : Int
  }


type alias Lesson =
  { lid     : Int
  , subject : Subject
  , day     : Int
  , slot    : Int
  }


type alias Subject =
  { sid  : Int
  , name : String
  }


-- UPDATE

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


days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]

infixl 9 !!
(!!) : List a -> Int -> Maybe a
xs !! n  =
  if | n < 0     -> Nothing
     | otherwise -> case (xs,n) of
         ([],_)    -> Nothing
         (x::xs,0) -> Just x
         (_::xs,n) -> xs !! (n-1)


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



emptyModel : Model
emptyModel =
  { lessons           = []
  , subjects          = []
  , lessonField       = emptyLesson
  , subjectField      = emptySubject
  , nsid              = 1
  , nlid              = 1
  }


emptySubject : Subject
emptySubject =
  { sid = 0
  , name = ""
  }


emptyLesson : Lesson
emptyLesson =
  { lid = 0
  , day = 0
  , slot = 0
  , subject = { sid = -1, name = ""}
  }


withDefault : b -> (a -> b) -> Result err a -> b
withDefault v f res =
  case res of
    Err _ -> v
    Ok n  -> f n


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


-- VIEW

view : Address Action -> Model -> Html
view address model =
  div
    [ class "row" ]
    [ div
      [ id "subjects-area", class "small-6 columns" ]
      [ subjectDisplay address model ]
    , div
      [ id "lessons-area", class "small-6 columns" ]
      [ lessonDisplay address model ]
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


subjectIsValid : Subject -> Bool
subjectIsValid s = s.sid >= 0 && s.name /= ""


subjectFields : Address Action -> Model -> Html
subjectFields address model =
  let
    enabled = subjectIsValid model.subjectField
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
          ] ++
            (if enabled
              then [onClick address (AddSubject)]
              else []))
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


lessonIsValid : Lesson -> Bool
lessonIsValid l = l.lid >= 0 && l.subject.sid >= 0


lessonFields : Address Action -> Model -> Html
lessonFields a m =
  let
    enabled = lessonIsValid m.lessonField
  in
    div
      [ class "row" ]
      [ div
        [ class "small-4 columns" ]
        [ select
            [ on "input" targetValue (Signal.message a << withDefault NoOp UpdateSlot << String.toInt) ]
            (List.map ((\n -> option [value n] [text n] ) << toString) [0..9])
        ]
      , div
        [class "small-4 columns"]
        [ select
            [ on "input" targetValue (Signal.message a << withDefault NoOp UpdateDay << String.toInt)]
            (List.map (\n -> option [value (toString n)] [text (Maybe.withDefault "invalid" (days !! n))] ) [0..6])
        ]
      , div
        [ class "columns small-4" ]
        [ button
            ([ classList [("postfix", True), ("disabled", not enabled)] ] ++
            (if enabled
              then [onClick a (AddLesson)]
              else []))
            [ text "+" ]
        ]
      ]


subToOption : Subject -> Html
subToOption s = option [ value (toString s.sid) ] [ text s.name ]


main : Signal Html
main =
  Signal.map (view actions.address) model

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

model : Signal Model
model =
  Signal.foldp update initialModel actions.signal


initialModel : Model
initialModel = emptyModel
