import Effects
import Html
import RouteHash
import StartApp
import Task
import Time

import Norad

app : StartApp.App Norad.Model Norad.Action
app =
  StartApp.start
    { init = Norad.init
    , update = Norad.update
    , view = Norad.view
    , inputs = []
    }

main : Signal Html.Html
main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks

port routeTasks : Signal (Task.Task () ())
port routeTasks =
  RouteHash.start
    { prefix = RouteHash.defaultPrefix
    , address = app.address
    , models = app.model
    , delta2update = Norad.delta2update
    , location2action = Norad.location2action
    }
