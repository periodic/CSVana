module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import App exposing (..)
import FileReader exposing (onFileInput)
import Maybe
import Components.Asana.View as Asana

view : Model -> Html Msg
view {csvData, workspaces} =
    div []
        [ div [ class "Csv" ] 
            ([ div [] [ input [ type' "file", onFileInput (NewFiles) ] [] ]
             ] ++
             List.filterMap identity [ Maybe.map (.headers >> renderHeaders) csvData ])
        , div [ class "Asana" ]
            [ Asana.apiResource Asana.renderWorkspaces workspaces
            , maybeRenderProjectSelect 
            ]

        ]

renderHeaders : List String -> Html Msg
renderHeaders headers =
    div [ class "CsvHeaders" ]
        (List.map renderHeader headers)

renderHeader : String -> Html Msg
renderHeader header =
    div [ class "CsvHeaders-header" ]
        [ text header ]
