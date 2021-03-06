module Build where

import Dict
import Debug
import Effects
import Html
import Json.Decode as Json exposing ((:=))
import Task

import EventSource

import BuildEvent
import Log
import Step

-- MODEL

type alias Model =
  { actions : Signal.Address Action
  , build : String
  , eventSource : Maybe EventSource.EventSource
  , eventsLoaded : Bool
  , buildStatus : Maybe BuildEvent.BuildStatus
  , steps : Dict.Dict Int Step.Model
  , connectionError : Bool
  }

type alias Build =
  { name : String
  , status : String
  }

init : Signal.Address Action -> String -> (Model, Effects.Effects Action)
init actions build =
  let
    model =
      { actions = actions
      , build = build
      , eventSource = Nothing
      , eventsLoaded = False
      , buildStatus = Nothing
      , steps = Dict.empty
      , connectionError = False
      }
  in
    (model, subscribeToEvents build actions)


-- UPDATE

type Action
  = Listening EventSource.EventSource
  | Opened
  | Errored
  | Event (Result String BuildEvent.BuildEvent)
  | EndOfEvents
  | Closed

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Listening es ->
      ({ model | eventSource <- Just es }, Effects.none)

    Opened ->
      (model, Effects.none)

    Errored ->
      (model, Effects.none)

    Event (Ok (BuildEvent.BuildStatus s)) ->
      ({ model | buildStatus <- Just s }, Effects.none)

    Event (Ok (BuildEvent.Log origin output)) ->
      let
        updatedStep = appendLog origin output
      in
        ( { model | steps <- Dict.update origin.location.id updatedStep model.steps }
        , Effects.none
        )

    Event (Ok _) ->
      (model, Effects.none)

    Event (Err e) ->
      (model, Debug.log e Effects.none)

    EndOfEvents ->
      case model.eventSource of
        Just es ->
          ({ model | eventsLoaded <- True }, closeEvents es)

        Nothing ->
          (model, Effects.none)

    Closed ->
      ({ model | eventSource <- Nothing }, Effects.none)


appendLog : BuildEvent.Origin -> String -> Maybe Step.Model -> Maybe Step.Model
appendLog origin log ms =
  let
    step =
      case ms of
        Nothing ->
          Step.init origin.stepName origin.stepType origin.location
        Just x ->
          x
  in
    Just (Step.update (Step.AppendLog log) step)

-- VIEW

view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div []
    [ Html.h1 []
        [ Html.text ("build #" ++ model.build)
        , Html.text " "
        , Html.text ("status " ++ Maybe.withDefault "unknown" (Maybe.map toString model.buildStatus))
        ]
    , if model.eventsLoaded
        then Html.ul [] (List.map (\e -> Html.li [] [Step.view e]) (Dict.values model.steps))
        else Html.div [] [Html.text "loading..."]
    , if model.connectionError
        then Html.text "connection failed"
        else Html.text "connection ok"
    ]


-- EFFECTS

subscribeToEvents : String -> Signal.Address Action -> Effects.Effects Action
subscribeToEvents build actions =
  let
    settings =
      EventSource.Settings
        (Just <| Signal.forwardTo actions (always Opened))
        (Just <| Signal.forwardTo actions (always Errored))

    connect =
      EventSource.connect ("http://127.0.0.1:8080/api/v1/builds/" ++ build ++ "/events") settings

    eventsSub =
      EventSource.on "event" <|
        Signal.forwardTo actions (Event << parseEvent)

    endSub =
      EventSource.on "end" <|
        Signal.forwardTo actions (always EndOfEvents)
  in
    connect `Task.andThen` eventsSub `Task.andThen` endSub
      |> Task.map Listening
      |> Effects.task

closeEvents : EventSource.EventSource -> Effects.Effects Action
closeEvents eventSource =
  EventSource.close eventSource
    |> Task.map (always Closed)
    |> Effects.task

parseEvent : EventSource.Event -> Result String BuildEvent.BuildEvent
parseEvent e = Json.decodeString BuildEvent.decode e.data
