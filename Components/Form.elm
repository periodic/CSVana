module Components.Form exposing (Props, Msg, Data, Instance, create)

import Html exposing (Html, div, h2, text, p)
import Html.Attributes exposing (class, classList)

import Asana.Api as Api
import Asana.Model as Asana
import Base
import CommonViews
import Components.ApiResource as ApiResource
import Components.Csv.Form as CsvForm
import Components.Csv.Summary as CsvSummary
import Components.MatcherSection as MatcherSection
import Components.FormSection as FormSection
import Components.Project.Form as ProjectForm
import Components.Project.Summary as ProjectSummary
import Components.Uploader.Form as UploaderForm
import Util

type alias Props =
    { token : Api.Token
    }

type Msg
    = CsvMsg (FormSection.Msg CsvForm.Msg CsvSummary.Msg)
    | ProjectMsg (ApiResource.Msg Asana.User (FormSection.Msg ProjectForm.Msg ProjectSummary.Msg))
    | FieldMatcherMsg MatcherSection.Msg
    | UploaderMsg UploaderForm.Msg

type alias Data = ()
type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = init props
        , update = update props
        , view = view props
        , subscriptions = subscriptions props
        , get = always ()
        }

-------------------------------------------------------------------------------
-- Private

type alias CsvData = (List String, List (List String))

type alias Csv = FormSection.Instance CsvData CsvForm.Msg CsvSummary.Msg
type alias Form = ApiResource.Instance Asana.User ProjectForm.Data (FormSection.Msg ProjectForm.Msg ProjectSummary.Msg)

type alias Model =
    { csv :  Csv
    , project : Form
    , matcher : Maybe MatcherSection.Instance
    , uploader : Maybe UploaderForm.Instance 
    }

createProjectForm : Props -> (Form, Cmd Msg)
createProjectForm { token } =
    Util.mapCmd ProjectMsg <| ApiResource.create
        { child = \user ->
            FormSection.create
                { incompleteChild = \maybeProject -> 
                    ProjectForm.create
                        { token = token
                        , user = user
                        }
                , completeChild = \project ->
                    ProjectSummary.create { project = project }
                , value = Nothing
                }
        , fetch = Api.me token
        , loadingView = CommonViews.loadingIndicator
        , errorView = CommonViews.errorView
        }

createCsvForm : Props -> (Csv, Cmd Msg)
createCsvForm _ =
    Util.mapCmd CsvMsg <| FormSection.create
        { incompleteChild = \maybeCsv -> 
            -- TODO: Better reconfiguration.
            CsvForm.create {}
        , completeChild = \csvData ->
            CsvSummary.create { csvData = csvData }
        , value = Nothing
        }

createMatcher : MatcherSection.Props -> (MatcherSection.Instance, Cmd Msg)
createMatcher =
    MatcherSection.create >> Util.mapCmd FieldMatcherMsg

createUploader : UploaderForm.Props -> (UploaderForm.Instance, Cmd Msg)
createUploader =
    UploaderForm.create >> Util.mapCmd UploaderMsg

init : Props -> (Model, Cmd Msg)
init props =
    let
        (project, projectCmd) = createProjectForm props
        (csv, csvCmd) = createCsvForm props
        cmd = Cmd.batch [projectCmd, csvCmd ]
    in
        ({ csv = csv
        , project = project
        , matcher = Nothing
        , uploader = Nothing
        }, cmd)

subscriptions : Props -> Model -> Sub Msg
subscriptions _ { csv, project, matcher } =
    let
        csvSubs = Base.subscriptionsWith CsvMsg csv
        projectSubs = Base.subscriptionsWith ProjectMsg project
        matcherSubs =
            Maybe.map (Base.subscriptionsWith FieldMatcherMsg) matcher
                |> Maybe.withDefault Sub.none
    in
        Sub.batch [csvSubs, projectSubs, matcherSubs]

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model0 =
    let
        (model1, cmd1) = processMsg props msg model0
        (model2, cmd2) = updateState props model1
    in
        (model2, Cmd.batch [ cmd1, cmd2 ])

processMsg : Props -> Msg -> Model -> (Model, Cmd Msg)
processMsg props msg model =
    case msg of
        CsvMsg msg_ ->
            Base.updateWith CsvMsg msg_ model.csv
                |> Util.mapComponent (\csv -> { model | csv = csv })
        ProjectMsg msg_ ->
            Base.updateWith ProjectMsg msg_ model.project
                |> Util.mapComponent (\project -> { model | project = project })
        FieldMatcherMsg msg_ ->
            case model.matcher of
                Just matcher ->
                    Base.updateWith FieldMatcherMsg msg_ matcher
                        |> Util.mapComponent (\matcher -> { model | matcher = Just matcher })
                Nothing ->
                    (model, Cmd.none)
        UploaderMsg msg_ ->
            case model.uploader of
                Just uploader ->
                    Base.updateWith UploaderMsg msg_ uploader
                        |> Util.mapComponent (\uploader -> { model | uploader = Just uploader })
                Nothing ->
                    (model, Cmd.none)

updateState : Props -> Model -> (Model, Cmd Msg)
updateState props model =
    let
        csv = Base.get model.csv
        project = Base.get model.project |> Maybe.andThen identity
        targets = model.matcher |> Maybe.andThen Base.get
    in
        case (csv, project, model.matcher, targets, model.uploader) of
            -- If CSV and project are done, and we don't have a matcher created, create one.
            (Just (headers, records), Just (workspaceId, project), Nothing, _, _) ->
                createMatcher
                    { apiContext =
                        { token = props.token
                        , workspaceId = workspaceId
                        }
                    , projectId = project.id
                    , headers = headers
                    , records = records
                    }
                    |> Util.mapComponent (\matcher -> { model | matcher = Just matcher })
            -- If CSV and project and targets are done, if we don't have an uploader, create one.
            (Just (headers, records), Just (workspaceId, project), _, Just targets, Nothing) ->
                createUploader
                    { apiContext =
                        { workspaceId = workspaceId
                        , token = props.token
                        }
                    , projectId = project.id
                    , records = records
                    , fieldTargets = targets
                    }
                    |> Util.mapComponent (\uploader -> { model | uploader = Just uploader })
            _ ->
                (model, Cmd.none)

csvSelected : Model -> Bool
csvSelected { csv } =
    Util.isJust <| Base.get csv

projectAndCsvSelected : Model -> Bool
projectAndCsvSelected { csv, project } =
    (Util.isJust <| Base.get csv) && (Util.isJust <| Base.get project)

view : Props -> Model -> Html Msg
view props model =
    div [ class "Form" ]
        [ viewCsv model
        , viewProject model
        , viewMatcher model
        , viewUploader model
        ]

viewCsv : Model -> Html Msg
viewCsv model =
    div [ class "Form-section Form-csvSection" ]
        [ h2 [] [ text "1. Choose a CSV file" ]
        , Base.viewWith CsvMsg model.csv
        ]

viewProject : Model -> Html Msg
viewProject model =
    div [ classList
            [ ("Form-section Form-projectSection", True)
            , ("Form-section--disabled", not <| csvSelected model)
            ]
        ]
        [ h2 [] [ text "2. Choose a Project in Asana" ]
        , if csvSelected model
            then Base.viewWith ProjectMsg model.project
            else text ""
        ]

viewMatcher : Model -> Html Msg
viewMatcher model =
    div [ classList
            [ ("Form-section Form-fieldsSection", True)
            , ("Form-section--disabled", not <| projectAndCsvSelected model)
            ]
        ]
        [ h2 [] [ text "3. Map columns to fields" ]
        , case model.matcher of
            Just matcher ->
                Base.viewWith FieldMatcherMsg matcher
            Nothing ->
                text ""
        ]

viewUploader : Model -> Html Msg
viewUploader model =
    div [ classList
            [ ("Form-section Form-uplaodSection", True)
            , ("Form-section--disabled", not <| projectAndCsvSelected model)
            ]
        ]
        [ h2 [] [ text "4. Import Records as Tasks" ]
        , case model.uploader of
            Just uploader ->
                div [ class "Form-uploader" ]
                    [ Base.viewWith UploaderMsg uploader ]
            Nothing ->
                div [ class "Form-uploader--disabled" ] []
        ]

