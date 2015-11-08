module Routes where

import Router exposing (..)
import String

type Page
  = NotFound String
  | Index
  | Pipeline String
  | Job String String

mainRoute : Route Page
mainRoute = match
  [ "" :-> always Index
  , "#!/index" :-> always Index
  , "#!/pipelines/" :-> pipelineRoute
  ] NotFound

pipelineRoute : Route Page
pipelineRoute route =
  case String.split "/" route of
    [name] ->
      Pipeline name

    [name, "jobs", job] ->
      Job name job

path : Page -> String
path path =
  case path of
    Index -> "#!/"
    Pipeline name -> "#!/pipelines/" ++ name
    Job pipeline name -> "#!/pipelines/" ++ pipeline ++ "/jobs/" ++ name
