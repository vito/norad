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
import Routes

-- MODEL

type alias Model =
  { currentPage : Page
  }

type Page
  = IndexPage Index.Model
  | PipelinePage Pipeline.Model

init : (Model, Effects Action)
init =
  let
      (indexModel, indexEffects) = Index.init
      model =
        { currentPage = IndexPage indexModel
        }
  in
     (model, Effects.map IndexAction indexEffects)


-- UPDATE

type Action
  = GoTo Routes.Page
  | IndexAction Index.Action
  | PipelineAction Pipeline.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    GoTo (Routes.Index) ->
      let
          (m, e) = Index.init
          newModel = { model | currentPage <- IndexPage m }
      in
        (newModel, Effects.map IndexAction e)

    GoTo (Routes.Pipeline pipeline) ->
      let
          (m, e) = Pipeline.init pipeline
          newModel = { model | currentPage <- (PipelinePage m) }
      in
        (newModel, Effects.map PipelineAction e)

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
