module AsanaTest exposing (main)

import Components.Asana as Asana
import Html exposing (..)
import Html.App exposing (..)
import Html.Attributes exposing (..)

props : Asana.Props
props =
    { baseUrl =  "https://localhost:8000"
    }

view : Asana.Model -> Html Asana.Msg
view model =
    div []
        [ div [ class "Debug" ]
            [ text <| toString model ]
        , Asana.view props model
        , div [] [ text <| toString <| Asana.getSelectedProject model ]
        ]

main : Program Never
main =
    program
        { init = Asana.init props
        , update = Asana.update props
        , subscriptions = Asana.subscriptions props
        , view = view
        }
