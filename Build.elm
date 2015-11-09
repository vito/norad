module Build where

import Effects
import Html
import Task

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

init : Signal.Address Action -> String -> (Model, Effects.Effects Action)
init actions build = (Model actions build Nothing [] False, subscribeToEvents build actions)


-- UPDATE

type Action
  = Listening EventSource.EventSource
  | Opened
  | Errored
  | Event EventSource.Event
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

view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div []
    [ Html.h1 [] [Html.text ("build #" ++ model.build)]
    , Html.ul [] (List.map (\e -> Html.li [] [Html.text e.data]) model.events)
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
          (Just (Signal.forwardTo actions (always Opened)))
          (Just (Signal.forwardTo actions (always Errored)))
      connect = EventSource.connect ("http://127.0.0.1:8080/api/v1/builds/" ++ build ++ "/events") settings
      eventsSub = EventSource.on "event" (Signal.forwardTo actions Event)
      endSub = EventSource.on "end" (Signal.forwardTo actions (always EndOfEvents))
  in
    connect `Task.andThen` eventsSub `Task.andThen` endSub
      |> Task.map Listening
      |> Effects.task

closeEvents : EventSource.EventSource -> Effects.Effects Action
closeEvents eventSource =
  EventSource.close eventSource
    |> Task.map (always Closed)
    |> Effects.task
