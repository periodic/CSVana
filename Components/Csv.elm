module Components.Csv exposing (Props, Msg, Component, Spec, spec, records, headers, numFields)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Base
import Csv
import FileReader.FileReader as FileReader

type alias Props =
    {}

type Msg
    = NewFiles (List FileReader.FileInfo)
    | MoreData String
    | HasHeaderRow Bool

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

records : Model -> Maybe (List (List String))
records { csvData, hasHeaderRow }=
    case csvData of
        Just csv ->
            if hasHeaderRow
            then Just csv.records
            else 
                if List.isEmpty csv.records && List.isEmpty csv.headers
                then Nothing
                else Just (csv.headers :: csv.records)
        Nothing ->
            Nothing

headers : Model -> Maybe (List String)
headers =
    .csvData >> Maybe.map .headers

numFields : Model -> Maybe Int
numFields =
    headers >> Maybe.map List.length

--------------------------------------------------------------------------------
-- Private

type alias Model =
    { csvData : Maybe Csv.Csv
    , hasHeaderRow : Bool
    }

init : Props -> (Model, Cmd Msg)
init _ =
    let
        model =
            { csvData = Nothing
            , hasHeaderRow = True
            }
    in
        (model, Cmd.none)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update _ msg model =
    case msg of
        MoreData chunk ->
            ({model | csvData = Just (Csv.parse chunk) }, Cmd.none)
        NewFiles files ->
            case List.head files of
                Just file ->
                    (model, FileReader.readFile file)
                Nothing ->
                    (model, Cmd.none)
        HasHeaderRow hasHeaderRow ->
            ({ model | hasHeaderRow = hasHeaderRow }, Cmd.none)

subscriptions : Props -> Model -> Sub Msg
subscriptions _ _ = FileReader.fileChunk MoreData

view : Props -> Model -> Html Msg
view _ model =
    div [ class "Csv" ] 
        [ div [ class "Csv-fileInput" ]
            [ input [ type' "file", FileReader.onFileInput (NewFiles) ] [] ]
        , div [ class "Csv-headerInput" ]
            [ input [ type' "checkbox", onClick (HasHeaderRow <| not model.hasHeaderRow), checked model.hasHeaderRow ] []
            , text "Header Row"
            ]
        ]

