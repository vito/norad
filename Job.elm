module Job where

import Effects
import Html
import Html.Attributes
import Http
import Json.Decode as Json exposing ((:=))
import Task

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

init : String -> String -> (Model, Effects.Effects Action)
init pipeline job = (Model pipeline job [] False, fetchBuilds pipeline job)


-- UPDATE

type Action = BuildsLoaded (Maybe (List Build))

update : Action -> Model -> (Model, Effects.Effects Action)
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

view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div []
    [ Html.h1 [] [Html.text (model.pipeline ++ "/" ++ model.job)]
    , Html.ul [] (List.map (\p -> Html.li [] [viewBuild p]) model.builds)
    , if model.connectionError
         then Html.text "connection failed"
         else Html.text "connection ok"
    ]

viewBuild : Build -> Html.Html
viewBuild build =
  Html.a
    [Html.Attributes.href (Routes.path (Routes.Build (toString build.id)))]
    [Html.text (build.name ++ " (" ++ build.status ++ ")")]


-- EFFECTS

fetchBuilds : String -> String -> Effects.Effects Action
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
