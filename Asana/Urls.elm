module Asana.Urls exposing (..)

import Asana.Model as Asana

type alias Url = String

project : Asana.ProjectId -> Url
project projectId =
    "https://app.asana.com/0/" ++ projectId ++ "/list"
