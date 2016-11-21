module Components.Asana.Form exposing (Props, Model, Msg, component, getSelectedProject)

import Html exposing (Html, div, text, input)
import Html.Attributes exposing (class)
import Html.App

import Base exposing (..)
import Components.Asana.Model as Asana
import Components.Asana.Api as Api
import Components.Asana.WorkspaceSelector as WorkspaceSelector
import Components.Asana.Typeahead as Typeahead
import Components.Asana.FieldOptions as FieldOptions
import Components.Asana.ProjectLoader as ProjectLoader

type alias Props =
    { token : Api.Token
    , user : Asana.User
    }

type alias Model =
    { workspaceSelector : WorkspaceSelector.Model
    -- Only valid if a workspace is selected.  Consider disabling instead?
    , projectTypeahead : Maybe (Typeahead.Model Asana.ProjectResource)
    , fieldOptions : ProjectLoader.Model FieldOptions.Model FieldOptions.Msg
    , fieldOptionsComponent : Component (ProjectLoader.Model FieldOptions.Model FieldOptions.Msg) (ProjectLoader.Msg FieldOptions.Msg)
    }

type Msg
    = WorkspaceSelectorMsg WorkspaceSelector.Msg
    | ProjectTypeaheadMsg (Typeahead.Msg Asana.ProjectResource)
    | FieldOptionsMsg (ProjectLoader.Msg FieldOptions.Msg)

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
        fieldOptionsComponent =
            ProjectLoader.component
                { token = token
                , childComponent = \project ->
                    let
                        customFields = List.map .customField project.customFieldSettings
                    in
                        FieldOptions.component
                            { customFields = customFields
                            , maxValues = 20 -- TODO
                            }
                }
        (fieldOptions, fieldOptionsCmd) = fieldOptionsComponent.init
        model =
            { workspaceSelector = wss
            , projectTypeahead = Nothing
            , fieldOptions = fieldOptions
            , fieldOptionsComponent = fieldOptionsComponent
            }
        cmd = Cmd.batch [Cmd.map WorkspaceSelectorMsg wsscmd, Cmd.map FieldOptionsMsg fieldOptionsCmd]
    in
        (model, cmd)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    case msg of
        WorkspaceSelectorMsg msg' ->
            updateWorkspace props msg' model
        ProjectTypeaheadMsg msg' ->
            updateProject props msg' model
        FieldOptionsMsg msg' ->
            updateFieldOptions props msg' model

updateProject : Props -> (Typeahead.Msg Asana.ProjectResource) -> Model -> (Model, Cmd Msg)
updateProject {token} msg model =
    case model.projectTypeahead of
        Just typeahead ->
            let
                (typeahead', typeaheadCmd) = Typeahead.update msg typeahead
                cmd = Cmd.map ProjectTypeaheadMsg typeaheadCmd
                model' = { model | projectTypeahead = Just typeahead' }
            in
                case Typeahead.getSelection typeahead' of
                    Just project ->
                        let
                            (options, optionsCmd) = ProjectLoader.load project.id token model.fieldOptions
                        in
                            ({ model' | fieldOptions = options }, Cmd.map FieldOptionsMsg optionsCmd)
                    Nothing ->
                        (model', cmd)
        Nothing ->
            (model, Cmd.none)

updateFieldOptions : Props -> ProjectLoader.Msg FieldOptions.Msg -> Model -> (Model, Cmd Msg)
updateFieldOptions {token} msg model =
    let
        (fieldOptions', fieldOptionsCmd) = model.fieldOptionsComponent.update msg model.fieldOptions
    in
        ({ model | fieldOptions = fieldOptions' }, Cmd.map FieldOptionsMsg fieldOptionsCmd)

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
        fieldOptions =
            Html.App.map FieldOptionsMsg <| model.fieldOptionsComponent.view model.fieldOptions 
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
            , div [ class "AsanaForm-fields" ]
                [ fieldOptions ]
            ]

getSelectedProject : Model -> Maybe Asana.ProjectResource
getSelectedProject { projectTypeahead }=
    case projectTypeahead of
        Just typeahead ->
            Typeahead.getSelection typeahead
        Nothing ->
            Nothing

