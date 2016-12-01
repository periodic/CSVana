module Components.Csv exposing (Props, Msg, Instance, create, numFields)

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

type alias Data =
    Maybe (List String, List (List String))

type alias Instance =
    Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = init props
        , update = update props
        , subscriptions = subscriptions props
        , view = view props
        , get = get props
        }

numFields : Data -> Maybe Int
numFields =
    Maybe.map (fst >> List.length)

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

get : Props -> Model -> Data
get _ model =
    Maybe.map2 (,) (headers model) (records model)

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

