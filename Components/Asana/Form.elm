module Components.Asana.Form exposing (Model, Msg, init, update, view)

import Components.Asana.Model as Asana
import Components.Asana.WorkspaceSelector as WorkspaceSelector
import Html exposing (Html)
import Html.App

type alias Model =
    { currentUser : Asana.User
    , workspaceSelector : WorkspaceSelector.Model
    }

type Msg
    = WorkspaceSelectorMsg WorkspaceSelector.Msg

init : Asana.User -> (Model, Cmd Msg)
init user =
    let
        (wss, wsscmd) = WorkspaceSelector.init (Maybe.withDefault [] user.workspaces)
        model =
            { currentUser = user
            , workspaceSelector = wss
            }
        cmd = Cmd.map WorkspaceSelectorMsg wsscmd
    in
        (model, cmd)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        WorkspaceSelectorMsg msg' ->
            let
                (wss', wsscmd) = WorkspaceSelector.update msg' model.workspaceSelector
                cmd = Cmd.map WorkspaceSelectorMsg wsscmd
            in
                ({model | workspaceSelector = wss'}, cmd)

view : Model -> Html Msg
view model =
    Html.App.map WorkspaceSelectorMsg
        <| WorkspaceSelector.view model.workspaceSelector


