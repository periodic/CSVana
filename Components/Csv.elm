module Components.Csv exposing (Props, Msg, Component, Spec, spec, getRecords, getHeaders, getNumFields)

import Html exposing (..)
import Html.Attributes exposing (..)

import Base
import Csv
import FileReader.FileReader as FileReader

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

type alias Spec =
    Base.Spec Model Msg
type alias Component =
    Base.Component Model Msg

spec : Props -> Spec
spec props =
    { init = init props
    , update = update props
    , subscriptions = subscriptions props
    , view = view props
    }

getRecords : Component -> Maybe (List (List String))
getRecords =
    Maybe.map (.records) << .csvData << Base.stateC

getHeaders : Component -> Maybe (List String)
getHeaders  =
    Maybe.map (.headers) << .csvData << Base.stateC

getNumFields : Component -> Maybe Int
getNumFields =
    getHeaders >> Maybe.map List.length

--------------------------------------------------------------------------------
-- Private

init : Props -> (Model, Cmd Msg)
init _ =
    let
        model =
            { csvData = Nothing
            , hasHeader = True
            }
    in
        (model, Cmd.none)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
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

subscriptions : Props -> Model -> Sub Msg
subscriptions _ _ = FileReader.fileChunk MoreData

view : Props -> Model -> Html Msg
view _ model =
    div [ class "Csv" ] 
        [ h3 [] [ text "Upload a CSV file:"]
        , div [] [ input [ type' "file", FileReader.onFileInput (NewFiles) ] [] ]
        ]

