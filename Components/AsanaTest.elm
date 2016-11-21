module AsanaTest exposing (main)

import Components.Asana as Asana
import Components.Asana.View as View
import Html exposing (..)
import Html.App exposing (..)
import Html.Attributes exposing (..)

view : Asana.Model -> Html Asana.Msg
view model =
    div []
        [ div [ class "Debug" ]
            [ text <| toString model ]
        , View.view model
        ]

main : Program Never
main =
    program
        { init = Asana.init "https://localhost:8000"
        , update = Asana.update
        , subscriptions = Asana.subscriptions
        , view = view
        }
