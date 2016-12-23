module Components.Form exposing (Props, Data, Msg, Instance, create)

import Html exposing (Html, div, text, input, label, h3)
import Html.Attributes exposing (class, disabled, type_)

import Asana.Api as Api
import Asana.Model as Asana
import Base
import Components.Typeahead as Typeahead
import Components.WorkspaceSelector as WorkspaceSelector

type alias Props =
    { token : Api.Token
    , user : Asana.User
    }

type Msg
    = WorkspaceSelectorMsg WorkspaceSelector.Msg
    | ProjectTypeaheadMsg (Typeahead.Msg Asana.ProjectResource)

type alias Data = Maybe (Asana.WorkspaceId, Asana.ProjectResource)
type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = init props
        , update = update props
        , view = view props
        , subscriptions = always Sub.none
        , get = get
        }

--------------------------------------------------------------------------------
-- Private

type alias Model =
    { workspaceSelector : WorkspaceSelector.Instance
    -- Only valid if a workspace is selected.  Consider disabling instead?
    , projectTypeahead : Maybe (Typeahead.Instance Asana.ProjectResource)
    }


get : Model -> Maybe (Asana.WorkspaceId, Asana.ProjectResource)
get { workspaceSelector, projectTypeahead } =
    let
        workspace = Base.get workspaceSelector
    in
        Base.get workspaceSelector
            |> Maybe.andThen (\workspace -> projectTypeahead |> Maybe.andThen Base.get |> Maybe.map ((,) workspace))

init : Props -> (Model, Cmd Msg)
init {token, user} =
    let
        (wss, wsscmd) = WorkspaceSelector.create
            { workspaces = Maybe.withDefault [] user.workspaces
            }
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
        WorkspaceSelectorMsg msg_ ->
            updateWorkspace props msg_ model
        ProjectTypeaheadMsg msg_ ->
            updateProject props msg_ model

updateProject : Props -> (Typeahead.Msg Asana.ProjectResource) -> Model -> (Model, Cmd Msg)
updateProject {token} msg model =
    case model.projectTypeahead of
        Just typeahead ->
            let
                (typeahead_, cmd) = Base.updateWith ProjectTypeaheadMsg msg typeahead
            in
                ({ model | projectTypeahead = Just typeahead_ }, cmd)
        Nothing ->
            (model, Cmd.none)

updateWorkspace : Props -> WorkspaceSelector.Msg -> Model -> (Model, Cmd Msg)
updateWorkspace { token } msg model =
    let
        (wss_, wsscmd) = Base.updateWith WorkspaceSelectorMsg msg model.workspaceSelector
        (typeahead, typeaheadCmd) =
            case Base.get wss_ of
                Just workspaceId ->
                    -- TODO: Only create a new typeahead if the workspace changes.
                    Base.mapFst Just
                    <| Base.mapCmd ProjectTypeaheadMsg
                    <| Typeahead.create
                        { fetcher = \fragment -> Api.projectTypeahead workspaceId fragment token }
                Nothing ->
                    (Nothing, Cmd.none)
        cmd = Cmd.batch [wsscmd, typeaheadCmd]
    in
        ({ model | workspaceSelector = wss_, projectTypeahead = typeahead }, cmd)

view : Props -> Model -> Html Msg
view _ model =
    let
        workspaces =
            Base.viewWith WorkspaceSelectorMsg model.workspaceSelector
        projects =
            case model.projectTypeahead of
                Just typeahead ->
                    Base.viewWith ProjectTypeaheadMsg typeahead
                Nothing ->
                    div [] [ input [ class "AsanaForm-projectInput--disabled", type_ "text", disabled True ] [] ]
    in
        div [ class "AsanaForm" ]
            [ div [ class "AsanaForm-workspaces" ]
                [ label [ class "AsanaForm-workspacesLabel" ] [ text "Workspace: " ]
                , workspaces
                ]
            , div [ class "AsanaForm-projects"]
                [ label [ class "AsanaForm-projectsLabel" ] [ text "Projects: " ]
                , projects
                ]
            ]
