module Pipeline where

import Debug
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (href)
import Http
import Json.Decode as Json exposing ((:=))
import RouteHash
import Task
import Time

import Routes


-- MODEL

type alias Model =
  { pipeline : String
  , jobs : List Job
  , lastUpdated : Time.Time
  , connectionError : Bool
  }

type alias Job =
  { name : String
  , paused : Bool
  }

init : String -> (Model, Effects Action)
init pipeline = (Model pipeline [] 0 False, fetchJobs pipeline)


-- UPDATE

type Action
  = JobsLoaded (Maybe (List Job))
  | Refresh Time.Time

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    JobsLoaded Nothing ->
      ( { model | connectionError <- True }
      , Effects.tick Refresh
      )

    JobsLoaded (Just jobs) ->
      ( { model | connectionError <- False, jobs <- jobs }
      , Effects.tick Refresh
      )

    Refresh time ->
      if (time - model.lastUpdated) > (5 * Time.second)
         then ({ model | lastUpdated <- time }, fetchJobs model.pipeline)
         else (model, Effects.tick Refresh)


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ h1 [] [text model.pipeline]
    , ul [] (List.map (\p -> li [] [viewJob model.pipeline p]) model.jobs)
    , if model.connectionError
         then text "connection failed"
         else text "connection ok"
    ]

viewJob : String -> Job -> Html
viewJob pipeline job =
  a [href (Routes.path (Routes.Job pipeline job.name))]
    [text (job.name ++ " (" ++ (if job.paused then "paused" else "active") ++ ")")]


-- EFFECTS

fetchJobs : String -> Effects Action
fetchJobs pipeline =
  Http.get decodeJobs ("http://127.0.0.1:8080/api/v1/pipelines/" ++ pipeline ++ "/jobs")
    |> Task.toMaybe
    |> Task.map JobsLoaded
    |> Effects.task


decodeJobs : Json.Decoder (List Job)
decodeJobs =
  Json.list <|
    Json.object2 Job
      ("name" := Json.string)
      (Json.map (Maybe.withDefault False) (Json.maybe ("paused" := Json.bool)))
