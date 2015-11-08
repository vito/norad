module Routes where

import Router exposing (..)

type Page
  = NotFound String
  | Index
  | Pipeline String

mainRoute : Route Page
mainRoute = match
  [ "#!/index" :-> always Index
  , "#!/pipelines/" :-> Pipeline
  ] NotFound

path : Page -> String
path path =
  case path of
    Index -> "#!/"
    Pipeline name -> "#!/pipelines/" ++ name
