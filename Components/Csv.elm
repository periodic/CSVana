module Components.Csv exposing (Props, Model, Msg, init, update, view, subscriptions, getRecords, getHeaders, getNumFields)

import Csv
import FileReader
import Html exposing (..)
import Html.Attributes exposing (..)

type alias Props =
    {}

type alias Model =
    { csvData : Maybe Csv.Csv
    , hasHeader : Bool
    }


type Msg
    = NewFiles (List FileReader.FileInfo)
    | MoreData String
    | HasHeader Bool

init _ =
    let
        model =
            { csvData = Nothing
            , hasHeader = True
            }
    in
        (model, Cmd.none)

update _ msg model =
    case msg of
        MoreData chunk ->
            ({model | csvData = Just (Csv.parse chunk) }, Debug.log "Got CsvData" Cmd.none)
        NewFiles files ->
            case List.head files of
                Just file ->
                    (model, FileReader.readFile file)
                Nothing ->
                    (model, Cmd.none)
        HasHeader hasHeader ->
            ({ model | hasHeader = hasHeader }, Cmd.none)

subscriptions _ _ = FileReader.fileChunk MoreData

view _ model =
    div [ class "Csv" ] 
        ([ div [] [ input [ type' "file", FileReader.onFileInput (NewFiles) ] [] ]
         ] ++
         List.filterMap identity [ Maybe.map (.headers >> renderHeaders) model.csvData ])

getRecords : Model -> Maybe (List (List String))
getRecords { csvData } =
    Maybe.map (.records) csvData

getHeaders : Model -> Maybe (List String)
getHeaders { csvData } =
    Maybe.map (.headers) csvData

getNumFields : Model -> Maybe Int
getNumFields =
    .csvData >> Maybe.map (.headers >> List.length)

--------------------------------------------------------------------------------
-- Private

renderHeaders : List String -> Html Msg
renderHeaders headers =
    div [ class "CsvHeaders" ]
        (List.map renderHeader headers)

renderHeader : String -> Html Msg
renderHeader header =
    div [ class "CsvHeaders-header" ]
        [ text header ]
