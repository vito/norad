module Index where

import Effects
import Html
import Html.Attributes
import Http
import Json.Decode as Json exposing ((:=))
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

init : (Model, Effects.Effects Action)
init = (Model [] 0 False, fetchPipelines)


-- UPDATE

type Action
  = PipelinesLoaded (Maybe (List Pipeline))
  | Refresh Time.Time

update : Action -> Model -> (Model, Effects.Effects Action)
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

view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div []
    [ Html.ul [] (List.map (\p -> Html.li [] [viewPipeline address p]) model.pipelines)
    , if model.connectionError
         then Html.text "connection failed"
         else Html.text "connection ok"
    ]

viewPipeline : Signal.Address Action -> Pipeline -> Html.Html
viewPipeline address pipeline =
  Html.a
    [Html.Attributes.href (Routes.path (Routes.Pipeline pipeline.name))]
    [Html.text (pipeline.name ++ " (" ++ (if pipeline.paused then "paused" else "active") ++ ")")]



-- EFFECTS

fetchPipelines : Effects.Effects Action
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
