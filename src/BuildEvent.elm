module BuildEvent where

import Json.Decode as Json exposing ((:=))

type BuildEvent
  = BuildStatus BuildStatus
  | Log Origin String

type alias BuildEventEnvelope =
  { event : String
  , version : String
  , value : Json.Value
  }

type alias Origin =
  { stepName : String
  , stepType : StepType
  , source : String
  , location : Location
  }

type StepType
  = StepTypeTask
  | StepTypeGet
  | StepTypePut

type alias Location =
  { id : Int
  , parentID : Int
  , parallelGroup : Int
  , serialGroup : Int
  , hook : String
  }

type BuildStatus
  = BuildStatusStarted
  | BuildStatusSucceeded
  | BuildStatusFailed
  | BuildStatusErrored

decode : Json.Decoder BuildEvent
decode = Json.customDecoder decodeEnvelope decodeEvent

decodeEnvelope : Json.Decoder BuildEventEnvelope
decodeEnvelope =
  Json.object3 BuildEventEnvelope
    ("event" := Json.string)
    ("version" := Json.string)
    ("data" := Json.value)

decodeEvent : BuildEventEnvelope -> Result String BuildEvent
decodeEvent e =
  case e.event of
    "status" ->
      Json.decodeValue (Json.object1 BuildStatus decodeStatus) e.value

    "log" ->
      Json.decodeValue (Json.object2 Log ("origin" := decodeOrigin) ("payload" := Json.string)) e.value

    unknown ->
      Err ("unknown event type: " ++ unknown)

decodeStatus : Json.Decoder BuildStatus
decodeStatus =
  Json.customDecoder ("status" := Json.string) <| \status ->
    case status of
      "started" -> Ok BuildStatusStarted
      "succeeded" -> Ok BuildStatusSucceeded
      unknown -> Err ("unknown build status: " ++ unknown)

decodeOrigin : Json.Decoder Origin
decodeOrigin =
  Json.object4 Origin
    ("name" := Json.string)
    decodeStepType
    ("source" := Json.string)
    ("location" := decodeLocation)

decodeLocation : Json.Decoder Location
decodeLocation =
  Json.object5 Location
    ("id" := Json.int)
    ("parent_id" := Json.int)
    ("parallel_group" := Json.int)
    ("serial_group" := Json.int)
    ("hook" := Json.string)

decodeStepType : Json.Decoder StepType
decodeStepType =
  Json.customDecoder ("type" := Json.string) <| \t ->
    case t of
      "task" -> Ok StepTypeTask
      "get" -> Ok StepTypeGet
      "put" -> Ok StepTypePut
      unknown -> Err ("unknown step type: " ++ unknown)
