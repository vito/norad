import Effects
import History
import Html
import RouteHash
import StartApp
import Task
import Time

import Norad
import Routes

events : Signal.Mailbox Norad.Action
events = Signal.mailbox Norad.Event

app : StartApp.App Norad.Model
app =
  StartApp.start
    { init = Norad.init events.address
    , update = Norad.update
    , view = Norad.view
    , inputs = [events.signal]
    , inits = [Signal.map Norad.GoTo pageNavigations]
    }

pageNavigations : Signal Routes.Page
pageNavigations = Signal.map Routes.mainRoute History.hash

main : Signal Html.Html
main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks
