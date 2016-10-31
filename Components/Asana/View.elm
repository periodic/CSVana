module Components.Asana.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Components.Asana.Model exposing (Workspace, toInt)
import App exposing (..)

apiResource : (a -> Html Msg) -> ApiStatus a -> Html Msg
apiResource subRenderer status =
    case status of
        Loading ->
            renderLoadingIndicator
        Loaded a ->
            subRenderer a

renderLoadingIndicator : Html Msg
renderLoadingIndicator =
    text "Loading..."

renderWorkspaces : List Workspace -> Html Msg
renderWorkspaces workspaces =
    select
        [ class "Workspaces" ]
        (List.map workspaceOption workspaces)

workspaceOption : Workspace -> Html Msg
workspaceOption {id, name} =
    option [ value <| toString <| toInt id ] [ text name ]
