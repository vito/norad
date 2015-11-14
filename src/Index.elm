module Index where

import Effects exposing (Effects)
import Html
import Html.Attributes
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Time

import Pipeline
import Routes

-- MODEL

type alias Model =
  { pipelines : List Pipeline
  , currentPipeline : Maybe Pipeline.Model
  , connectionError : Bool
  }

type alias Pipeline =
  { name : String
  , paused : Bool
  }

init : Maybe String -> (Model, Effects Action)
init pipeline =
  let model = Model [] Nothing False
  in
     case pipeline of
       Nothing ->
         (model, fetchPipelines)

       Just name ->
         let (loadedModel, modelEffects) = switchToPipeline name model
         in (loadedModel, Effects.batch [fetchPipelines, modelEffects])

-- UPDATE

type Action
  = PipelinesLoaded (Maybe (List Pipeline))
  | Refresh
  | PipelineAction String Pipeline.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    PipelinesLoaded Nothing ->
      ( { model | connectionError <- True }
      , Effects.none
      )

    PipelinesLoaded (Just []) ->
      ( { model | connectionError <- False }
      , Effects.none
      )

    PipelinesLoaded (Just (main :: rest)) ->
      let withPipelines =
            { model | connectionError <- False
                    , pipelines <- (main :: rest) }
      in
        case withPipelines.currentPipeline of
          Just _ -> (withPipelines, Effects.none)
          Nothing -> switchToPipeline main.name withPipelines

    Refresh ->
      case model.currentPipeline of
        Nothing ->
          (model, fetchPipelines)

        Just current ->
          let (updated, effects) = Pipeline.update Pipeline.Refresh current
          in
            ( { model | currentPipeline <- Just updated }
            , Effects.batch
                [ fetchPipelines
                , Effects.map (PipelineAction current.pipeline) effects
                ]
            )

    PipelineAction name a ->
      case model.currentPipeline of
        Just current ->
          if current.pipeline == name
            then
              let (updated, effects) = Pipeline.update a current
              in
                 ( { model | currentPipeline <- Just updated }
                 , Effects.map (PipelineAction name) effects
                 )

            -- navigated away
            else (model, Effects.none)

        Nothing ->
          -- should be impossible
          (model, Effects.none)

switchToPipeline : String -> Model -> (Model, Effects Action)
switchToPipeline name model =
  let (mainPipeline, mainEffects) = Pipeline.init name
  in
    ( { model | currentPipeline <- Just mainPipeline }
    , Effects.map (PipelineAction name) mainEffects
    )

-- VIEW

view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div []
    [ Html.ul [] (List.map (\p -> Html.li [] [viewPipeline address p]) model.pipelines)
    , if model.connectionError
         then Html.text "connection failed"
         else Html.text "connection ok"
    , case model.currentPipeline of
        Nothing ->
          Html.text "no pipeline selected"
        Just pipeline ->
          Pipeline.view (Signal.forwardTo address (PipelineAction pipeline.pipeline)) pipeline
    ]

viewPipeline : Signal.Address Action -> Pipeline -> Html.Html
viewPipeline address pipeline =
  Html.a
    [Html.Attributes.href (Routes.path (Routes.Pipeline pipeline.name))]
    [Html.text (pipeline.name ++ " (" ++ (if pipeline.paused then "paused" else "active") ++ ")")]



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
