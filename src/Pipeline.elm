module Pipeline where

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
  { pipeline : String
  , jobs : List Job
  , lastUpdated : Time.Time
  , connectionError : Bool
  }

type alias Job =
  { name : String
  , paused : Bool
  }

init : String -> (Model, Effects.Effects Action)
init pipeline = (Model pipeline [] 0 False, fetchJobs pipeline)


-- UPDATE

type Action
  = JobsLoaded (Maybe (List Job))
  | Refresh Time.Time

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    JobsLoaded Nothing ->
      ({ model | connectionError <- True }, Effects.none)

    JobsLoaded (Just jobs) ->
      ({ model | connectionError <- False, jobs <- jobs }, Effects.none)

    Refresh time ->
      if (time - model.lastUpdated) > (5 * Time.second)
         then ({ model | lastUpdated <- time }, fetchJobs model.pipeline)
         else (model, Effects.none)


-- VIEW

view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div []
    [ Html.h1 [] [Html.text model.pipeline]
    , Html.ul [] (List.map (\p -> Html.li [] [viewJob model.pipeline p]) model.jobs)
    , if model.connectionError
         then Html.text "connection failed"
         else Html.text "connection ok"
    ]

viewJob : String -> Job -> Html.Html
viewJob pipeline job =
  Html.a
    [Html.Attributes.href (Routes.path (Routes.Job pipeline job.name))]
    [Html.text (job.name ++ " (" ++ (if job.paused then "paused" else "active") ++ ")")]


-- EFFECTS

fetchJobs : String -> Effects.Effects Action
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
