module Components.Asana exposing (Props, Msg, Data, Instance, create)

import Html exposing (Html, div, h3, text, p)
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
            , unloadedView = CommonViews.loadingIndicator
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
        FormMsg msg' ->
            let
                (form', formCmd) = Base.update msg' model.form
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
                (csv', csvCmd) = Base.updateWith CsvMsg msg' model.csv
                model' = { model | csv = csv' }
                (headers, _) = Base.get model.csv |> Maybe.withDefault ([], [])
                (headers', _) = Base.get csv' |> Maybe.withDefault ([], [])
            in
                if headers /= headers'
                    then updateMatcher props (model', csvCmd)
                    else (model', csvCmd)
        FieldMatcherMsg msg' ->
            case model.fieldMatcher of
                Just matcher ->
                    let
                        (matcher', matcherCmd) = Base.update msg' matcher
                        cmd = Cmd.map FieldMatcherMsg matcherCmd
                    in
                        ({ model | fieldMatcher = Just matcher' }, cmd)
                Nothing ->
                    (model, Cmd.none)

updateMatcher : Props -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateMatcher {token} (model, cmd) =
    case (getSelectedProject model, Base.get model.csv) of
        (Just project, Just (headers, records)) ->
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
getSelectedProject =
    .form >> Base.get >> flip Maybe.andThen identity

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
            , Html.App.map CsvMsg <| Base.view model.csv
            ]
        , div [ class "Asana-arrow Cell -2of12" ]
            [ text "→" ]
        , div [ class "Asana-form Cell -5of12" ]
            [ h3 [] [ text "Asana" ]
            , p [ class "Asana-infoText" ] [ text "Select an Asana project:" ]
            , Html.App.map FormMsg <| Base.view model.form
            ]
        ]

viewMatcher : Props -> Model -> Html Msg
viewMatcher props { fieldMatcher } =
    case fieldMatcher of
        Just matcher ->
            div [ class "Asana-matcher" ]
                [ Html.App.map FieldMatcherMsg <| Base.view matcher ]
        Nothing ->
            div [ class "Asana-matcher--disabled" ] []
