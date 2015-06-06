module Types
  ( Lesson
  , Model
  , Subject
  , Rule
  , Target
  , emptyModel
  , emptyLesson
  , emptySubject
  , encode_lesson
  , encode_datafile
  , decode_lesson
  , decode_schedule
  ) where

import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode


type alias Model =
  { lessons           : List Lesson
  , subjects          : List Subject
  , lessonField       : Lesson
  , subjectField      : Subject
  , nsid              : Int
  , nlid              : Int
  , nrid              : Int
  , rules             : List Rule
  , target            : Target
  , currentSeverity   : Int
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


type alias Rule =
  { target   : Target
  , severity : Int
  , rid      : Int
  }


type alias Target =
  { scope : String
  , day   : Int
  , slot  : Int
  }


-- INITIALIZERS


emptyModel : Model
emptyModel =
  { lessons           = []
  , subjects          = []
  , lessonField       = emptyLesson
  , subjectField      = emptySubject
  , nsid              = 1
  , nlid              = 1
  , nrid              = 0
  , rules             = []
  , target            = { scope = "", day = 0, slot = 0 }
  , currentSeverity   = 0
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
  , subject = { sid = -1, name = "" }
  }


-- DE- ENCODING


decode_lesson : Decode.Decoder Lesson
decode_lesson =
  Decode.object3
    (\sub day' slot' ->
      { lid = 0, subject = { name = sub, sid = 0 }, day = day', slot = slot' })
    ("subject" :=  Decode.string)
    ("day" := Decode.int)
    ("slot" := Decode.int)


encode_lesson : Lesson -> Encode.Value
encode_lesson l =
  Encode.object
    [ ("subject", Encode.string l.subject.name)
    , ("day", Encode.int l.day)
    , ("slot", Encode.int l.slot)
    ]


encode_target : Target -> List (String, Encode.Value)
encode_target { scope, day, slot } =
  [ ("scope",  Encode.string scope)
  , ("day", Encode.int day)
  , ("slot", Encode.int slot)
  ]


encode_rule : Rule -> Encode.Value
encode_rule { target, severity } =
  Encode.object
    (("severity", Encode.int severity)::encode_target target)


encode_datafile : Model -> Encode.Value
encode_datafile m =
  Encode.object
    [ ("lessons", Encode.list (List.map encode_lesson m.lessons)),
      ("rules", Encode.list (List.map encode_rule m.rules))
    ]

decode_schedule : Decode.Decoder (List (Int, List Lesson))
decode_schedule =
  Decode.list
    (Decode.object2 (,)
      ("weight" := Decode.int)
      ("lessons" := Decode.list decode_lesson))
