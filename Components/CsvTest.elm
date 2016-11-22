module Components.CsvTest exposing (main)

import Components.Csv as Csv
import Html exposing (..)
import Html.App exposing (..)
import Html.Attributes exposing (..)

props : Csv.Props
props =
    {}

view : Csv.Model -> Html Csv.Msg
view model =
    div []
        [ div [ class "Debug" ]
            [ text <| toString model ]
        , Csv.view props model
        ]

main : Program Never
main =
    program
        { init = Csv.init props
        , update = Csv.update props
        , subscriptions = Csv.subscriptions props
        , view = view
        }
