import Effects
import Html
import StartApp
import Task
import Time

import Norad

app : StartApp.App Norad.Model
app =
  StartApp.start
    { init = Norad.init
    , update = Norad.update
    , view = Norad.view
    , inputs = [refreshEvery (5 * Time.second)]
    }

main : Signal Html.Html
main = app.html

refreshEvery : Time.Time -> Signal Norad.Action
refreshEvery interval = Signal.map (always Norad.Refresh) (Time.every interval)

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks
