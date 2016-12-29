module Components.Csv.Form exposing (Props, Msg, Data, Instance, create)

import Csv
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Base
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

--------------------------------------------------------------------------------
-- Private

type Upload
    = Empty
    | Error (List String)
    | Success Csv.Csv

type alias Model =
    { csvData : Upload
    , hasHeaderRow : Bool
    }

init : Props -> (Model, Cmd Msg)
init _ =
    let
        model =
            { csvData = Empty
            , hasHeaderRow = True
            }
    in
        (model, Cmd.none)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update _ msg model =
    case msg of
        MoreData chunk ->
            ({model | csvData = parseData chunk }, Cmd.none)
        NewFiles files ->
            case List.head files of
                Just file ->
                    (model, FileReader.readFile file)
                Nothing ->
                    (model, Cmd.none)
        HasHeaderRow hasHeaderRow ->
            ({ model | hasHeaderRow = hasHeaderRow }, Cmd.none)

parseData : String -> Upload
parseData chunk =
    case Csv.parse chunk of
        Ok csvData ->
            Success csvData
        Err errors ->
            Error errors

subscriptions : Props -> Model -> Sub Msg
subscriptions _ _ = FileReader.fileChunk MoreData

view : Props -> Model -> Html Msg
view _ model =
    div [ class "Csv" ] 
        [ div [ class "Csv-fileInput" ]
            [ input [ type_ "file", FileReader.onFileInput (NewFiles) ] [] ]
        , div [ class "Csv-headerInput" ]
            [ input [ type_ "checkbox", onClick (HasHeaderRow <| not model.hasHeaderRow), checked model.hasHeaderRow ] []
            , text "Header Row"
            ]
        , viewStatus model.csvData
        ]

viewStatus : Upload -> Html Msg
viewStatus upload =
    case upload of
        Success csvData ->
            List.map text csvData.headers
                |> List.map (\e -> span [] [e])
                |> div [ class "Csv-headers" ]
        Error errors ->
            List.map text errors
                |> List.map (\e -> p [] [e])
                |> div [ class "Csv-errors" ]
        _ ->
            text ""

get : Props -> Model -> Data
get _ model =
    Maybe.map2 (,) (headers model) (records model)

records : Model -> Maybe (List (List String))
records { csvData, hasHeaderRow }=
    case csvData of
        Success csv ->
            if hasHeaderRow
            then Just csv.records
            else 
                if List.isEmpty csv.records && List.isEmpty csv.headers
                then Nothing
                else Just (csv.headers :: csv.records)
        _ ->
            Nothing

headers : Model -> Maybe (List String)
headers { csvData } =
    case csvData of
        Success csv ->
            Just csv.headers
        _ ->
            Nothing
