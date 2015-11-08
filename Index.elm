module Index where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing ((:=))
import RouteHash
import Task
import Time

import Routes

-- MODEL

type alias Model =
  { pipelines : List Pipeline
  , lastUpdated : Time.Time
  , connectionError : Bool
  }

type alias Pipeline =
  { name : String
  , paused : Bool
  }

init : (Model, Effects Action)
init = (Model [] 0 False, fetchPipelines)


-- UPDATE

type Action
  = PipelinesLoaded (Maybe (List Pipeline))
  | Refresh Time.Time

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    PipelinesLoaded Nothing ->
      ( { model | connectionError <- True }
      , Effects.tick Refresh
      )

    PipelinesLoaded (Just pipelines) ->
      ( { model | connectionError <- False, pipelines <- pipelines }
      , Effects.tick Refresh
      )

    Refresh time ->
      if (time - model.lastUpdated) > (5 * Time.second)
         then ({ model | lastUpdated <- time }, fetchPipelines)
         else (model, Effects.tick Refresh)


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ ul [] (List.map (\p -> li [] [viewPipeline address p]) model.pipelines)
    , if model.connectionError
         then text "connection failed"
         else text "connection ok"
    ]

viewPipeline : Signal.Address Action -> Pipeline -> Html
viewPipeline address pipeline =
  a [href (Routes.path (Routes.Pipeline pipeline.name))]
    [text (pipeline.name ++ " (" ++ (if pipeline.paused then "paused" else "active") ++ ")")]



-- EFFECTS

fetchPipelines : Effects Action
fetchPipelines =
  Http.get decodePipelines "http://127.0.0.1:8080/api/v1/pipelines"
    |> Task.toMaybe
    |> Task.map PipelinesLoaded
    |> Effects.task


decodePipelines : Json.Decoder (List Pipeline)
decodePipelines =
  Json.list <|
    Json.object2 Pipeline
      ("name" := Json.string)
      ("paused" := Json.bool)
