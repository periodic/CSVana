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
    , projectTypeahead : TypeAhead.TypeAhead Asana.Project
    , projectTypeaheadString : String
    }

type Msg
    = NewFiles (List FileInfo)
    | MoreData String
    | ApiMe (Asana.ApiResult Asana.User)
    | ProjectTypeaheadMsg (Typeahead.Msg Asana.Project)
    | ProjectTypeaheadInput String

