module App exposing (..)

import Components.Asana.Model as Asana
import Components.Asana.TypeAhead as TypeAhead
import Csv
import FileReader exposing (FileInfo)
import OAuth

type ApiStatus a
    = Loaded a
    | Loading

type alias Model =
    { csvData : Maybe Csv.Csv
    -- Asana
    , oauth : OAuth.Model
    , workspaces : ApiStatus (List Asana.Resource)
    , selectedWorkspace : Maybe Asana.Workspace
    , projectTypeahead : Maybe (TypeAhead.TypeAhead Asana.Project)
    }

type Msg
    = NewFiles (List FileInfo)
    | MoreData String
    | ApiMe (Asana.ApiResult Asana.User)
    | WorkspaceSelection Asana.Workspace
    | ProjectTypeaheadMsg (Typeahead.Msg Asana.Project)
    | ProjectTypeaheadInput String

