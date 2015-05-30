module Types
  ( Lesson
  , Model
  , Subject
  , emptyModel
  , emptyLesson
  , emptySubject
  , encode_lesson
  , encode_datafile
  , decode_lesson
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


-- INITIALIZERS


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


-- DE- ENCODING


decode_lesson : Decode.Decoder Lesson
decode_lesson =
  Decode.object3
    (\sub day' slot' -> { lid = 0, subject = { name = sub, sid = 0}, day = day', slot = slot' })
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


encode_datafile : Model -> Encode.Value
encode_datafile m =
  Encode.object
    [ ("lessons", Encode.list (List.map encode_lesson m.lessons))
    ]
