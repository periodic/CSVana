module App exposing (..)

import Asana exposing (Resource, ApiResult)
import Csv
import FileReader exposing (FileInfo)
import OAuth

type ApiStatus a
    = Loaded a
    | Loading

type alias Model =
    { csvData : Maybe Csv.Csv
    , oauth : OAuth.Model
    , workspaces : ApiStatus (List Resource)
    }

type Msg
    = NewFiles (List FileInfo)
    | MoreData String
    | ApiMsg Asana.ApiResult

