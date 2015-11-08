import Effects
import History
import Html
import RouteHash
import StartApp
import Task
import Time

import Norad
import Routes

app : StartApp.App Norad.Model
app =
  StartApp.start
    { init = Norad.init
    , update = Norad.update
    , view = Norad.view
    , inputs = []
    , inits = [Signal.map Norad.GoTo pageNavigations]
    }

pageNavigations : Signal Routes.Page
pageNavigations = Signal.map Routes.mainRoute History.hash

main : Signal Html.Html
main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks
