module Job where

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
  , job : String
  , builds : List Build
  , connectionError : Bool
  }

type alias Build =
  { id : Int
  , name : String
  , status : String
  }

init : String -> String -> (Model, Effects Action)
init pipeline job = (Model pipeline job [] False, fetchBuilds pipeline job)


-- UPDATE

type Action = BuildsLoaded (Maybe (List Build))

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    BuildsLoaded Nothing ->
      ( { model | connectionError <- True }
      , Effects.none
      )

    BuildsLoaded (Just builds) ->
      ( { model | connectionError <- False, builds <- builds }
      , Effects.none
      )


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ h1 [] [text (model.pipeline ++ "/" ++ model.job)]
    , ul [] (List.map (\p -> li [] [viewBuild p]) model.builds)
    , if model.connectionError
         then text "connection failed"
         else text "connection ok"
    ]

viewBuild : Build -> Html
viewBuild build =
  a [href (Routes.path (Routes.Build (toString build.id)))]
    [text (build.name ++ " (" ++ build.status ++ ")")]


-- EFFECTS

fetchBuilds : String -> String -> Effects Action
fetchBuilds pipeline job =
  Http.get decodeBuilds ("http://127.0.0.1:8080/api/v1/pipelines/" ++ pipeline ++ "/jobs/" ++ job ++ "/builds")
    |> Task.toMaybe
    |> Task.map BuildsLoaded
    |> Effects.task


decodeBuilds : Json.Decoder (List Build)
decodeBuilds =
  Json.list <|
    Json.object3 Build
      ("id" := Json.int)
      ("name" := Json.string)
      ("status" := Json.string)
