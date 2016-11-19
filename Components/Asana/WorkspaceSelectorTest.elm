module WorkspaceSelectorTest exposing (main)

import Components.Asana.Model as Asana
import Components.Asana.WorkspaceSelector as WorkspaceSelector
import Html exposing (..)
import Html.App exposing (..)

type alias Model = WorkspaceSelector.Model
type alias Msg = WorkspaceSelector.Msg

workspaces : List Asana.WorkspaceResource
workspaces =
    [ { id = "1", name = "First" }
    , { id = "2", name = "Second" }
    , { id = "foo", name = "Foo-th" }
    ]

view : Model -> Html Msg
view model =
    div []
        [ div [] [ text <| toString model ]
        , div [] [ WorkspaceSelector.view model ]
        ]

main : Program Never
main =
    program
        { init = WorkspaceSelector.init workspaces
        , update = WorkspaceSelector.update
        , subscriptions = always Sub.none
        , view = view
        }
