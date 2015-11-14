import Effects
import History
import Html
import StartApp
import Task
import Time

import Norad
import Routes

app : StartApp.App Norad.Model
app =
  let
    pageDrivenActions =
      Signal.mailbox Norad.Noop

    autoUpdate =
      Signal.map (always Norad.Refresh) <|
        Time.every (5 * Time.second)
  in
    StartApp.start
      { init = Norad.init pageDrivenActions.address
      , update = Norad.update
      , view = Norad.view
      , inputs = [pageDrivenActions.signal, autoUpdate]
      , inits = [Signal.map Norad.GoTo pageNavigations]
      }

pageNavigations : Signal Routes.Page
pageNavigations =
  Signal.map Routes.mainRoute History.hash

main : Signal Html.Html
main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
