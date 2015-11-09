module Build where

import Debug
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing ((:=))
import RouteHash
import Task
import Time

import EventSource

-- MODEL

type alias Model =
  { actions : Signal.Address Action
  , build : String
  , eventSource : Maybe EventSource.EventSource
  , events : List EventSource.Event
  , connectionError : Bool
  }

type alias Build =
  { name : String
  , status : String
  }

init : Signal.Address Action -> String -> (Model, Effects Action)
init actions build = (Model actions build Nothing [] False, subscribeToEvents build actions)


-- UPDATE

type Action
  = Connected EventSource.EventSource
  | Event EventSource.Event
  | EndOfEvents
  | Closed

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Connected es ->
      ({ model | eventSource <- Just es }, Effects.none)

    Event e ->
      ({ model | events <- model.events ++ [e] }, Effects.none)

    EndOfEvents ->
      case model.eventSource of
        Just es ->
          (model, closeEvents es)

        Nothing ->
          (model, Effects.none)

    Closed ->
      ({ model | eventSource <- Nothing }, Effects.none)


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ h1 [] [text ("build #" ++ model.build)]
    , ul [] (List.map (\e -> li [] [text e.data]) model.events)
    , if model.connectionError
         then text "connection failed"
         else text "connection ok"
    ]


-- EFFECTS

subscribeToEvents : String -> Signal.Address Action -> Effects Action
subscribeToEvents build actions =
  let
      connect = EventSource.connect ("http://127.0.0.1:8080/api/v1/builds/" ++ build ++ "/events")
      eventsSub = EventSource.on "event" (Signal.forwardTo actions Event)
      endSub = EventSource.on "end" (Signal.forwardTo actions (always EndOfEvents))
  in
    connect `Task.andThen` eventsSub `Task.andThen` endSub
      |> Task.map Connected
      |> Effects.task

closeEvents : EventSource.EventSource -> Effects Action
closeEvents eventSource =
  EventSource.close eventSource
    |> Task.map (always Closed)
    |> Effects.task
