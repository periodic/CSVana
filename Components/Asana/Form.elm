module Components.Asana.Form exposing (Props, Model, Msg, component, getSelectedProject)

import Html exposing (Html, div, text, input)
import Html.Attributes exposing (class)
import Html.App

import Base exposing (..)
import Components.Asana.Model as Asana
import Components.Asana.Api as Api
import Components.Asana.WorkspaceSelector as WorkspaceSelector
import Components.Asana.Typeahead as Typeahead

type alias Props =
    { token : Api.Token
    , user : Asana.User
    }

type alias Model =
    { workspaceSelector : WorkspaceSelector.Model
    -- Only valid if a workspace is selected.  Consider disabling instead?
    , projectTypeahead : Maybe (Typeahead.Model Asana.Project)
    }

type Msg
    = WorkspaceSelectorMsg WorkspaceSelector.Msg
    | ProjectTypeaheadMsg (Typeahead.Msg Asana.Project)

component : Props -> Component Model Msg
component props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = always Sub.none
    }

init : Props -> (Model, Cmd Msg)
init {token, user} =
    let
        (wss, wsscmd) = WorkspaceSelector.init (Maybe.withDefault [] user.workspaces)
        model =
            { workspaceSelector = wss
            , projectTypeahead = Nothing
            }
        cmd = Cmd.map WorkspaceSelectorMsg wsscmd
    in
        (model, cmd)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update {token} msg model =
    case msg of
        WorkspaceSelectorMsg msg' ->
            let
                (wss', wsscmd) = WorkspaceSelector.update msg' model.workspaceSelector
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
        ProjectTypeaheadMsg msg' ->
            case model.projectTypeahead of
                Just typeahead ->
                    let
                        (typeahead', typeaheadCmd) = Typeahead.update msg' typeahead
                        cmd = Cmd.map ProjectTypeaheadMsg typeaheadCmd
                    in
                        ({ model | projectTypeahead = Just typeahead' }, cmd)
                Nothing ->
                    (model, Cmd.none)

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
            [ div [ class "AsanaForm-workspaces" ]
                [ text "Workspace: "
                , workspaces
                ]
            , div [ class "AsanaForm-projects"]
                [ text "Projects: "
                , projects
                ]
            ]

getSelectedProject : Model -> Maybe Asana.Project
getSelectedProject { projectTypeahead }=
    case projectTypeahead of
        Just typeahead ->
            Typeahead.getSelection typeahead
        Nothing ->
            Nothing

