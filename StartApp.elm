module StartApp ( start, Config, App ) where

import Html exposing (Html)
import Task
import Effects exposing (Effects, Never)


type alias Config model action =
    { init : (model, Effects action)
    , update : action -> model -> (model, Effects action)
    , view : Signal.Address action -> model -> Html
    , inputs : List (Signal.Signal action)
    }


type alias App model action =
    { html : Signal Html
    , tasks : Signal (Task.Task Never ())
    , model : Signal model
    , address : Signal.Address action
    }


start : Config model action -> App model action
start config =
    let
        singleton action = [ action ]

        -- messages : Signal.Mailbox (List action)
        messages =
            Signal.mailbox []

        -- address : Signal.Address action
        address =
            Signal.forwardTo messages.address singleton

        -- updateStep : action -> (model, Effects action) -> (model, Effects action)
        updateStep action (oldModel, accumulatedEffects) =
            let
                (newModel, additionalEffects) = config.update action oldModel
            in
                (newModel, Effects.batch [accumulatedEffects, additionalEffects])

        -- update : List action -> (model, Effects action) -> (model, Effects action)
        update actions (model, _) =
            List.foldl updateStep (model, Effects.none) actions

        -- inputs : Signal (List action)
        inputs =
            Signal.mergeMany (messages.signal :: List.map (Signal.map singleton) config.inputs)

        -- effectsAndModel : Signal (model, Effects action)
        effectsAndModel =
            Signal.foldp update config.init inputs

        model =
            Signal.map fst effectsAndModel
    in
        { html = Signal.map (config.view address) model
        , tasks = Signal.map (Effects.toTask messages.address << snd) effectsAndModel
        , model = model
        , address = address
        }
