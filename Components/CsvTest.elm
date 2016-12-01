module Components.CsvTest exposing (main)

import Html exposing (..)
import Html.App exposing (..)

import CommonViews
import Components.Csv as Csv

view subView model =
    div []
        [ subView model
        , div []
            [ text "Headers:"
            , text <| toString <| Csv.headers model
            ]
        , div []
            [ text "Records:"
            , text <| toString <| Csv.records model
            ]
        ]


main : Program Never
main =
    let
        spec = Csv.spec {}
    in
        program { spec | view = CommonViews.debugView (view spec.view) }
