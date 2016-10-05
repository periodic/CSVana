import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App

import FileReader exposing (..)

type alias Model =
    { data : String
    }

type Msg
    = NewFiles (List FileInfo)
    | MoreData String

init : (Model, Cmd Msg)
init =
    ({ data = "" }, Cmd.none)



view : Model -> Html Msg
view {data} =
    div [] 
        [ div [] [ input [ type' "file", onFileInput (NewFiles) ] [] ]
        , div [] [ text data ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MoreData chunk ->
            ({ model | data = model.data ++ chunk }, Cmd.none)
        NewFiles files ->
            case List.head files of
                Just file ->
                    (model, readFile file)
                Nothing ->
                    (model, Cmd.none)

subscriptions :  Model -> Sub Msg
subscriptions model = fileChunk MoreData

main =
    App.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
