module Components.Asana exposing (Props, Msg, Model, spec)

import Html exposing (Html, div, h3, text)
import Html.App
import Html.Attributes exposing (class)

import Asana.Api as Api
import Asana.Model as Asana
import Base
import CommonViews
import Components.ApiResource as ApiResource
import Components.ApiParallelResource as ApiParallelResource
import Components.Csv as Csv
import Components.FieldMatcher as FieldMatcher
import Components.Form as Form

type alias Props =
    { token : Api.Token
    }

type Msg
    = FormMsg (ApiResource.Msg Asana.User Form.Msg)
    | CsvMsg Csv.Msg
    | FieldMatcherMsg (ApiResource.Msg Asana.Project (ApiParallelResource.Msg Asana.CustomFieldInfo FieldMatcher.Msg))

type alias Model =
    { form : ApiResource.Component Asana.User Form.Model Form.Msg
    , csv : Csv.Component
    , fieldMatcher : Maybe
        (ApiResource.Component
            Asana.Project
            (ApiParallelResource.Model Asana.CustomFieldInfo FieldMatcher.Model FieldMatcher.Msg)
            (ApiParallelResource.Msg Asana.CustomFieldInfo FieldMatcher.Msg))
    }

spec : Props -> Base.Spec Model Msg
spec props =
    { init = init props
    , update = update props
    , view = view props
    , subscriptions = subscriptions props
    }

--------------------------------------------------------------------------------
-- Private

init : Props -> (Model, Cmd Msg)
init {token} =
    let
        (form, formCmd) = Base.mapCmd FormMsg <| Base.initC <| ApiResource.component
            { childSpec = \user ->
                Form.component
                    { token = token
                    , user = user
                    }
            , fetch = Api.me token
            , unloadedView = CommonViews.loadingIndicator
            , loadingView = CommonViews.loadingIndicator
            , errorView = CommonViews.errorView
            }
        (csv, csvCmd) = Base.mapCmd CsvMsg <| Base.initC <| Csv.spec {}
        cmd = Cmd.batch [formCmd, csvCmd ]
    in
        ({ form = form
        , csv = csv
        , fieldMatcher = Nothing
        }, cmd)

subscriptions : Props -> Model -> Sub Msg
subscriptions _ { form, csv } =
    let
        formSubs = Sub.map FormMsg <| Base.subscriptionsC form
        csvSubs = Sub.map CsvMsg <| Base.subscriptionsC csv
    in
        Sub.batch [formSubs, csvSubs]

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update =
    processMessage


processMessage : Props -> Msg -> Model -> (Model, Cmd Msg)
processMessage props msg model =
    case msg of
        FormMsg msg' ->
            let
                (form', formCmd) = Base.updateC msg' model.form
                model' = { model | form = form' }
                cmd = Cmd.map FormMsg formCmd
                project = getSelectedProject model
                project' = getSelectedProject model'
            in
                if project /= project'
                    then updateMatcher props (model', cmd)
                    else (model', cmd)
        CsvMsg msg' ->
            let
                (csv', csvCmd) = Base.updateC msg' model.csv
                model' = { model | csv = csv' }
                cmd = Cmd.map CsvMsg csvCmd
                headers = Csv.getHeaders model.csv
                headers' = Csv.getHeaders csv'
            in
                if headers /= headers'
                    then updateMatcher props (model', cmd)
                    else (model', cmd)
        FieldMatcherMsg msg' ->
            case model.fieldMatcher of
                Just matcher ->
                    let
                        (matcher', matcherCmd) = Base.updateC msg' matcher
                        cmd = Cmd.map FieldMatcherMsg matcherCmd
                    in
                        ({ model | fieldMatcher = Just matcher' }, cmd)
                Nothing ->
                    (model, Cmd.none)

updateMatcher : Props -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateMatcher {token} (model, cmd) =
    case ( getSelectedProject model, Csv.getHeaders model.csv, Csv.getRecords model.csv) of
        (Just project, Just headers, Just records) ->
            let
                (matcher, matcherCmd) = Base.mapCmd FieldMatcherMsg <| Base.initC <|
                    ApiResource.component
                        { childSpec = \project ->
                            let
                                customFieldIds = List.map (.customField >> .id) project.customFieldSettings
                                numFields = List.length headers
                            in
                                ApiParallelResource.component 
                                    { childSpec = \customFieldInfos ->
                                        FieldMatcher.component
                                            { token = token
                                            , projectId = project.id
                                            , csvHeaders = headers
                                            , csvRecords = records
                                            , customFields = customFieldInfos
                                            }
                                    , fetches = List.map (flip Api.customField token) customFieldIds
                                    , unloadedView = CommonViews.loadingIndicator
                                    , loadingView = CommonViews.loadingIndicator
                                    , errorView = CommonViews.errorView
                                    }
                        , fetch = Api.project project.id token
                        , unloadedView = CommonViews.loadingIndicator
                        , loadingView = CommonViews.loadingIndicator
                        , errorView = CommonViews.errorView
                        }
            in
                ({ model | fieldMatcher = Just matcher }, matcherCmd)
        _ ->
            ({ model | fieldMatcher = Nothing }, Cmd.none)


getSelectedProject : Model -> Maybe Asana.ProjectResource
getSelectedProject model =
        ApiResource.getChild model.form
            `Maybe.andThen` Form.getSelectedProject

view : Props -> Model -> Html Msg
view props model =
    div [ class "Asana" ]
        [ viewInputs props model
        , viewMatcher props model
        ]

viewInputs : Props -> Model -> Html Msg
viewInputs props model =
    div [ class "Asana-inputs" ]
        [ div [ class "Asana-form" ]
            [ h3 [] [ text "Select an Asana project:" ]
            , Html.App.map FormMsg <| Base.viewC model.form
            ]
        , div [ class "Asana-csv" ]
            [ h3 [] [ text "Upload a CSV file:"]
            , Html.App.map CsvMsg <| Base.viewC model.csv
            ]
        ]

viewMatcher : Props -> Model -> Html Msg
viewMatcher props { fieldMatcher } =
    case fieldMatcher of
        Just matcher ->
            div [ class "Asana-matcher" ]
                [ Html.App.map FieldMatcherMsg <| Base.viewC matcher ]
        Nothing ->
            div [ class "Asana-matcher--disabled" ] []
