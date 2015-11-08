module Norad where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Time


-- MODEL

type alias Model =
  { pipelines : List Pipeline
  , connectionError : Bool
  }

type alias Pipeline =
  { name : String
  , paused : Bool
  }

init : (Model, Effects Action)
init = (Model [] False, fetchPipelines)


-- UPDATE

type Action
  = PipelinesLoaded (Maybe (List Pipeline))
  | Refresh

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    PipelinesLoaded Nothing ->
      ( { model | connectionError <- True }
      , Effects.none
      )

    PipelinesLoaded (Just pipelines) ->
      ( { model | connectionError <- False, pipelines <- pipelines }
      , Effects.none
      )

    Refresh ->
      (model, fetchPipelines)


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ ul [] (List.map (\p -> li [] [viewPipeline p]) model.pipelines)
    , if model.connectionError
         then text "connection failed"
         else text "connection ok"
    ]

viewPipeline : Pipeline -> Html
viewPipeline pipeline =
  text (pipeline.name ++ " (" ++ (if pipeline.paused then "paused" else "active") ++ ")")


-- EFFECTS

fetchPipelines : Effects Action
fetchPipelines =
  Http.get decodePipelines "http://127.0.0.1:8080/api/v1/pipelines"
    |> Task.toMaybe
    |> Task.map PipelinesLoaded
    |> Effects.task


decodePipelines : Json.Decoder (List Pipeline)
decodePipelines =
  Json.list (Json.object2 Pipeline ("name" := Json.string) ("paused" := Json.bool))
