module Components.Asana exposing (Props, Msg, Data, Instance, create)

import Html exposing (Html, div, h3, text, p)
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

--------------------------------------------------------------------------------
-- Private

type alias Model =
    { form : ApiResource.Instance Asana.User Form.Data Form.Msg
    , csv : Csv.Instance
    , fieldMatcher : Maybe
        (ApiResource.Instance
            Asana.Project
            (ApiParallelResource.Data FieldMatcher.Data)
            (ApiParallelResource.Msg Asana.CustomFieldInfo FieldMatcher.Msg))
    }


init : Props -> (Model, Cmd Msg)
init {token} =
    let
        (form, formCmd) = Base.mapCmd FormMsg <| ApiResource.create
            { child = \user ->
                Form.create
                    { token = token
                    , user = user
                    }
            , fetch = Api.me token
            , loadingView = CommonViews.loadingIndicator
            , errorView = CommonViews.errorView
            }
        (csv, csvCmd) = Base.mapCmd CsvMsg <| Csv.create {}
        cmd = Cmd.batch [formCmd, csvCmd ]
    in
        ({ form = form
        , csv = csv
        , fieldMatcher = Nothing
        }, cmd)

subscriptions : Props -> Model -> Sub Msg
subscriptions _ { form, csv } =
    let
        formSubs = Sub.map FormMsg <| Base.subscriptions form
        csvSubs = Sub.map CsvMsg <| Base.subscriptions csv
    in
        Sub.batch [formSubs, csvSubs]

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update =
    processMessage


processMessage : Props -> Msg -> Model -> (Model, Cmd Msg)
processMessage props msg model =
    case msg of
        FormMsg msg_ ->
            let
                (form_, formCmd) = Base.update msg_ model.form
                model_ = { model | form = form_ }
                cmd = Cmd.map FormMsg formCmd
                project = Base.get model.form
                project_ = Base.get model_.form
            in
                if project /= project_
                    then updateMatcher props (model_, cmd)
                    else (model_, cmd)
        CsvMsg msg_ ->
            let
                (csv_, csvCmd) = Base.updateWith CsvMsg msg_ model.csv
                model_ = { model | csv = csv_ }
                (headers, _) = Base.get model.csv |> Maybe.withDefault ([], [])
                (headers_, _) = Base.get csv_ |> Maybe.withDefault ([], [])
            in
                if headers /= headers_
                    then updateMatcher props (model_, csvCmd)
                    else (model_, csvCmd)
        FieldMatcherMsg msg_ ->
            case model.fieldMatcher of
                Just matcher ->
                    let
                        (matcher_, matcherCmd) = Base.update msg_ matcher
                        cmd = Cmd.map FieldMatcherMsg matcherCmd
                    in
                        ({ model | fieldMatcher = Just matcher_ }, cmd)
                Nothing ->
                    (model, Cmd.none)

updateMatcher : Props -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateMatcher { token } (model, cmd) =
    case (Base.get model.form |> Maybe.andThen identity, Base.get model.csv) of
        (Just (workspaceId, project), Just (headers, records)) ->
            let
                (matcher, matcherCmd) = Base.mapCmd FieldMatcherMsg
                    <| ApiResource.create
                        { child = \project ->
                            let
                                customFieldIds = List.map (.customField >> .id) project.customFieldSettings
                                numFields = List.length headers
                            in
                                ApiParallelResource.create 
                                    { child = \customFieldInfos ->
                                        FieldMatcher.create
                                            { projectId = project.id
                                            , csvHeaders = headers
                                            , csvRecords = records
                                            , customFields = customFieldInfos
                                            , apiContext =
                                                { token = token
                                                , workspaceId = workspaceId
                                                }
                                            }
                                    , fetches = List.map (flip Api.customField token) customFieldIds
                                    , loadingView = CommonViews.loadingIndicator
                                    , errorView = CommonViews.errorView
                                    }
                        , fetch = Api.project project.id token
                        , loadingView = CommonViews.loadingIndicator
                        , errorView = CommonViews.errorView
                        }
            in
                ({ model | fieldMatcher = Just matcher }, matcherCmd)
        _ ->
            ({ model | fieldMatcher = Nothing }, Cmd.none)

view : Props -> Model -> Html Msg
view props model =
    div [ class "Asana" ]
        [ viewInputs props model
        , viewMatcher props model
        ]

viewInputs : Props -> Model -> Html Msg
viewInputs props model =
    div [ class "Asana-inputs Grid" ]
        [ div [ class "Asana-csv Cell -5of12" ]
            [ h3 [] [ text "CSV"]
            , p [ class "Asana-infoText" ] [ text "Upload a CSV file:" ]
            , Base.viewWith CsvMsg model.csv
            ]
        , div [ class "Asana-arrow Cell -2of12" ]
            [ text "→" ]
        , div [ class "Asana-form Cell -5of12" ]
            [ h3 [] [ text "Asana" ]
            , p [ class "Asana-infoText" ] [ text "Select an Asana project:" ]
            , Base.viewWith FormMsg model.form
            ]
        ]

viewMatcher : Props -> Model -> Html Msg
viewMatcher props { fieldMatcher } =
    case fieldMatcher of
        Just matcher ->
            div [ class "Asana-matcher" ]
                [ Base.viewWith FieldMatcherMsg matcher ]
        Nothing ->
            div [ class "Asana-matcher--disabled" ] []
