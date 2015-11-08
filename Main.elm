import Effects
import StartApp
import Task

import Norad

app =
  StartApp.start
    { init = Norad.init
    , update = Norad.update
    , view = Norad.view
    , inputs = []
    }

main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks
