module Components.Form exposing (Props, Model, Msg, component, getSelectedProject)

import Html exposing (Html, div, text, input, label, h3)
import Html.Attributes exposing (class)
import Html.App

import Asana.Api as Api
import Asana.Model as Asana
import Base exposing (..)
import Components.Typeahead as Typeahead
import Components.WorkspaceSelector as WorkspaceSelector

type alias Props =
    { token : Api.Token
    , user : Asana.User
    }

type alias Model =
    { workspaceSelector : WorkspaceSelector.Model
    -- Only valid if a workspace is selected.  Consider disabling instead?
    , projectTypeahead : Maybe (Typeahead.Model Asana.ProjectResource)
    }

type Msg
    = WorkspaceSelectorMsg WorkspaceSelector.Msg
    | ProjectTypeaheadMsg (Typeahead.Msg Asana.ProjectResource)

component : Props -> Spec Model Msg
component props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = always Sub.none
    }

getSelectedProject : Component Model Msg -> Maybe Asana.ProjectResource
getSelectedProject =
    Base.stateC >> .projectTypeahead >> (\typeahead -> typeahead `Maybe.andThen` Typeahead.getSelection)

--------------------------------------------------------------------------------
-- Private

init : Props -> (Model, Cmd Msg)
init {token, user} =
    let
        (wss, wsscmd) = WorkspaceSelector.init (Maybe.withDefault [] user.workspaces)
        model =
            { workspaceSelector = wss
            , projectTypeahead = Nothing
            }
        cmd = Cmd.batch [Cmd.map WorkspaceSelectorMsg wsscmd]
    in
        (model, cmd)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    case msg of
        WorkspaceSelectorMsg msg' ->
            updateWorkspace props msg' model
        ProjectTypeaheadMsg msg' ->
            updateProject props msg' model

updateProject : Props -> (Typeahead.Msg Asana.ProjectResource) -> Model -> (Model, Cmd Msg)
updateProject {token} msg model =
    case model.projectTypeahead of
        Just typeahead ->
            let
                (typeahead', typeaheadCmd) = Typeahead.update msg typeahead
                cmd = Cmd.map ProjectTypeaheadMsg typeaheadCmd
                model' = { model | projectTypeahead = Just typeahead' }
            in
                (model', cmd)
        Nothing ->
            (model, Cmd.none)

updateWorkspace : Props -> WorkspaceSelector.Msg -> Model -> (Model, Cmd Msg)
updateWorkspace { token } msg model =
    let
        (wss', wsscmd) = WorkspaceSelector.update msg model.workspaceSelector
        (typeahead, typeaheadCmd) =
            case WorkspaceSelector.getValue wss' of
                Just workspaceId ->
                    -- TODO: Only create a new typeahead if the workspace changes.
                    mapFst Just <| Typeahead.init token workspaceId Api.projectTypeahead
                Nothing ->
                    (Nothing, Cmd.none)
        cmd = Cmd.batch [Cmd.map WorkspaceSelectorMsg wsscmd, Cmd.map ProjectTypeaheadMsg typeaheadCmd]
    in
        ({ model | workspaceSelector = wss', projectTypeahead = typeahead }, cmd)

view : Props -> Model -> Html Msg
view _ model =
    let
        workspaces =
            Html.App.map WorkspaceSelectorMsg <| WorkspaceSelector.view model.workspaceSelector
        projects =
            case model.projectTypeahead of
                Just typeahead ->
                    Html.App.map ProjectTypeaheadMsg <| Typeahead.view typeahead
                Nothing ->
                    div [] [ input [] [] ]
    in
        div [ class "AsanaForm" ]
            [ h3 [] [ text "Select an Asana project:" ]
            , div [ class "AsanaForm-workspaces" ]
                [ label [ class "AsanaForm-workspacesLabel" ] [ text "Workspace: " ]
                , workspaces
                ]
            , div [ class "AsanaForm-projects"]
                [ label [ class "AsanaForm-projectsLabel" ] [ text "Projects: " ]
                , projects
                ]
            ]
