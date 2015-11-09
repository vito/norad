module Norad where

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

import Index
import Pipeline
import Job
import Build
import Routes

-- MODEL

type alias Model =
  { currentPage : Page
  , events : Signal.Address Action
  }

type Page
  = IndexPage Index.Model
  | PipelinePage Pipeline.Model
  | JobPage Job.Model
  | BuildPage Build.Model

init : Signal.Address Action -> (Model, Effects Action)
init events =
  let
      (indexModel, indexEffects) = Index.init
      model =
        { currentPage = IndexPage indexModel
        , events = events
        }
  in
     (model, Effects.map IndexAction indexEffects)


-- UPDATE

type Action
  = Event
  | GoTo Routes.Page
  | IndexAction Index.Action
  | PipelineAction Pipeline.Action
  | JobAction Job.Action
  | BuildAction Build.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    GoTo (Routes.Index) ->
      updatePage model Index.init IndexPage IndexAction

    GoTo (Routes.Pipeline pipeline) ->
      updatePage model (Pipeline.init pipeline) PipelinePage PipelineAction

    GoTo (Routes.Job pipeline job) ->
      updatePage model (Job.init pipeline job) JobPage JobAction

    GoTo (Routes.Build build) ->
      updatePage model (Build.init (Signal.forwardTo model.events BuildAction) build) BuildPage BuildAction

    GoTo _ ->
      Debug.log "not found" (model, Effects.none)

    IndexAction a ->
      case model.currentPage of
        IndexPage m ->
          updatePage model (Index.update a m) IndexPage IndexAction

        _ ->
          -- navigated away; ignore action which may have come from async effect
          (model, Effects.none)

    PipelineAction a ->
      case model.currentPage of
        PipelinePage m ->
          updatePage model (Pipeline.update a m) PipelinePage PipelineAction

        _ ->
          -- navigated away; ignore action which may have come from async effect
          (model, Effects.none)

    JobAction a ->
      case model.currentPage of
        JobPage m ->
          updatePage model (Job.update a m) JobPage JobAction

        _ ->
          -- navigated away; ignore action which may have come from async effect
          (model, Effects.none)

    BuildAction a ->
      case model.currentPage of
        BuildPage m ->
          updatePage model (Build.update a m) BuildPage BuildAction

        _ ->
          -- navigated away; ignore action which may have come from async effect
          (model, Effects.none)

updatePage : Model -> (pm, Effects pa) -> (pm -> Page) -> (pa -> Action) -> (Model, Effects Action)
updatePage m (pm, pe) p a = ({ m | currentPage <- p pm }, Effects.map a pe)


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  case model.currentPage of
    IndexPage m ->
      Index.view (Signal.forwardTo address IndexAction) m

    PipelinePage m ->
      Pipeline.view (Signal.forwardTo address PipelineAction) m

    JobPage m ->
      Job.view (Signal.forwardTo address JobAction) m

    BuildPage m ->
      Build.view (Signal.forwardTo address BuildAction) m
