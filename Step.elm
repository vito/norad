module Step where

type alias Step =
  { id : Int
  , name : String
  , hooks : StepHooks
  }

type alias Hooks =
  { onSuccess : Maybe Step
  , onFailure : Maybe Step
  , ensure : Maybe Step
  }
