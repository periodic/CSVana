module Main exposing (main)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Navigation

import Components.Csv as Csv
import Components.Asana as Asana

type alias Model =
    { asana : Asana.Model
    , csv : Csv.Model
    , baseUrl : String
    }

type Msg
    = AsanaMsg Asana.Msg
    | CsvMsg Csv.Msg

init : Navigation.Location -> (Model, Cmd Msg)
init location =
    let
        (asana, asanaCmd) =
            Asana.init { baseUrl = location.origin }
        (csv, csvCmd) =
            Csv.init {}
        model =
            { asana = asana
            , csv = csv
            , baseUrl = location.origin
            }
        cmd = Cmd.batch
            [ Cmd.map AsanaMsg asanaCmd
            , Cmd.map CsvMsg csvCmd
            ]
    in
        (model, cmd)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
        case msg of
            AsanaMsg msg' ->
                let
                    (asana', asanaCmd) =
                         -- TODO: Bind the props in init.
                         Asana.update { baseUrl = model.baseUrl } msg' model.asana
                in
                    ({ model | asana = asana' }, Cmd.map AsanaMsg asanaCmd)
            CsvMsg msg' ->
                let
                    (csv', csvCmd) =
                        Csv.update {} msg' model.csv
                in
                    ({ model | csv = csv' }, Cmd.map CsvMsg csvCmd)


subscriptions :  Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map AsanaMsg <| Asana.subscriptions { baseUrl = model.baseUrl } model.asana
        , Sub.map CsvMsg <| Csv.subscriptions {} model.csv
        ]

urlUpdate : Navigation.Location -> Model -> (Model, Cmd Msg)
urlUpdate location model =
    (model, Cmd.none)

view model =
    div [ class "Main" ]
        [ div [ class "Main-asana" ]
            [ Html.App.map AsanaMsg <| Asana.view { baseUrl = model.baseUrl } model.asana ]
        , div [ class "Main-csv" ]
            [ Html.App.map CsvMsg <| Csv.view {} model.csv ]
        ]

main =
    Navigation.program
        (Navigation.makeParser identity)
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        , view = view
        }
