module Step where

import Html exposing (Html)

import BuildEvent
import Log

type alias Model =
  { name : String
  , stepType : BuildEvent.StepType
  , location : BuildEvent.Location
  , log : Log.Model
  }

type Action
  = AppendLog String

init : String -> BuildEvent.StepType -> BuildEvent.Location -> Model
init name stepType location =
  { name = name
  , stepType = stepType
  , location = location
  , log = Log.init
  }

update : Action -> Model -> Model
update action model =
  case action of
    AppendLog log ->
      { model | log <- Log.update log model.log }

view : Model -> Html
view model =
  Html.div []
    [ Html.text (toString model.location)
    , Log.view model.log
    ]
