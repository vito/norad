module Norad where

import Debug
import Effects
import Html

import Index
import Pipeline
import Job
import Build
import Routes

-- MODEL

type alias Model =
  { currentPage : Page
  , pageDrivenActions : Signal.Address Action
  }

type Page
  = IndexPage Index.Model
  | PipelinePage Pipeline.Model
  | JobPage Job.Model
  | BuildPage Build.Model

init : Signal.Address Action -> (Model, Effects.Effects Action)
init pageDrivenActions =
  let
      (indexModel, indexEffects) = Index.init Nothing
      model =
        { currentPage = IndexPage indexModel
        , pageDrivenActions = pageDrivenActions
        }
  in
     (model, Effects.map IndexAction indexEffects)


-- UPDATE

type Action
  = Noop
  | GoTo Routes.Page
  | IndexAction Index.Action
  | PipelineAction Pipeline.Action
  | JobAction Job.Action
  | BuildAction Build.Action
  | Refresh

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Noop ->
      (model, Effects.none)

    GoTo (Routes.Index) ->
      updatePage model (Index.init Nothing) IndexPage IndexAction

    GoTo (Routes.Pipeline pipeline) ->
      case model.currentPage of
        IndexPage indexModel ->
          updatePage model (Index.update (Index.SwitchPipeline pipeline) indexModel) IndexPage IndexAction

        _ ->
          updatePage model (Index.init (Just pipeline)) IndexPage IndexAction

    GoTo (Routes.Job pipeline job) ->
      updatePage model (Job.init pipeline job) JobPage JobAction

    GoTo (Routes.Build build) ->
      let
        buildActions = Signal.forwardTo model.pageDrivenActions BuildAction
      in
        updatePage model (Build.init buildActions build) BuildPage BuildAction

    GoTo _ ->
      Debug.log "not found" (model, Effects.none)

    Refresh ->
      case model.currentPage of
        IndexPage m ->
          updatePage model (Index.update Index.Refresh m) IndexPage IndexAction

        _ ->
          (model, Effects.none)


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

updatePage : Model -> (pm, Effects.Effects pa) -> (pm -> Page) -> (pa -> Action) -> (Model, Effects.Effects Action)
updatePage m (pm, pe) p a = ({ m | currentPage <- p pm }, Effects.map a pe)


-- VIEW

view : Signal.Address Action -> Model -> Html.Html
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
